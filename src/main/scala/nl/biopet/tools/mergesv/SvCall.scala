package nl.biopet.tools.mergesv

import java.util

import htsjdk.samtools.reference.IndexedFastaSequenceFile
import htsjdk.variant.variantcontext.{
  Allele,
  VariantContext,
  VariantContextBuilder
}

import scala.collection.JavaConversions._
import scala.util.matching.Regex

case class SvCall(contig1: String,
                  pos1: Int,
                  contig2: String,
                  pos2: Int,
                  svType: String,
                  posCi: Interval,
                  endCi: Interval,
                  callers: List[String],
                  orientation1: Boolean = true,
                  orientation2: Boolean = true) { //TODO: Genotypes

  def overlapWith(other: SvCall): Boolean = {
    this.contig1 == other.contig1 && this.contig2 == other.contig2 &&
    this.posCi.overlapWith(other.posCi) && this.endCi.overlapWith(other.endCi)
  }

  def toVariantContext(
      referenceReader: IndexedFastaSequenceFile): VariantContext = {

    val referenceAllele = Allele.create(
      referenceReader.getSubsequenceAt(contig1, pos1, pos1).getBaseString,
      true)

    def commonBuilder =
      new VariantContextBuilder()
        .chr(contig1)
        .start(pos1)
        .attribute("SVTYPE", svType)
        .attribute("CIPOS",
                   Array(posCi.relativeStart(pos1), posCi.relativeEnd(pos1)))
        .attribute("CIEND",
                   Array(endCi.relativeStart(pos2), endCi.relativeEnd(pos2)))

    svType match {
      case "BND" =>
        val altAllele = (orientation1, orientation2) match {
          case (true, true) =>
            Allele.create(s"${referenceAllele.getBaseString}[$contig2:$pos2[")
          case (true, false) =>
            Allele.create(s"${referenceAllele.getBaseString}]$contig2:$pos2]")
          case (false, true) =>
            Allele.create(s"[$contig2:$pos2[${referenceAllele.getBaseString}")
          case (false, false) =>
            Allele.create(s"]$contig2:$pos2]${referenceAllele.getBaseString}")
        }
        commonBuilder
          .alleles(referenceAllele :: altAllele :: Nil)
          .make()
      case _ =>
        val altAllele = Allele.create(s"<$svType>")
        commonBuilder
          .stop(pos2)
          .alleles(referenceAllele :: altAllele :: Nil)
          .make()
    }
  }
}

object SvCall {

  val bndRegexFF: Regex = """.*\[(.*):(\d+)\[""".r
  val bndRegexFR: Regex = """.*\](.*):(\d+)\]""".r
  val bndRegexRF: Regex = """\[(.*):(\d+)\[.*""".r
  val bndRegexRR: Regex = """\](.*):(\d+)\].*""".r

  def from(variant: VariantContext, caller: String, defaultCi: Int): SvCall = {
    val contig1 = variant.getContig
    val pos1 = variant.getStart
    val svType: String = variant.getAttribute("SVTYPE") match {
      case s: String => s
      case _ =>
        throw new IllegalStateException("Variant does not have tag 'SVTYPE'")
    }

    def getCi(pos1: Int, pos2: Int): (Interval, Interval) =
      (variant.getAttribute("CIPOS"),
       variant.getAttribute("CIEND"),
       variant.getAttribute("CILEN")) match {
        case (pos: util.ArrayList[String], end: util.ArrayList[String], _) =>
          val p = pos.map(_.toInt)
          val e = end.map(_.toInt)
          (Interval(pos1 + p(0), pos1 + p(1)),
           Interval(pos2 + e(0), pos2 + e(1)))
        case (_, _, len: util.ArrayList[String]) =>
          val l = len.map(_.toInt)
          (Interval(pos1 + l(0), pos1 + l(1)),
           Interval(pos2 + l(0), pos2 + l(1)))
        case _ =>
          (Interval(pos1 - defaultCi, pos1 + defaultCi),
           Interval(pos2 - defaultCi, pos2 + defaultCi))
      }

    svType match {
      case "BND" =>
        require(variant.getAlternateAlleles.size() == 1,
                "Multiple alleles are not supported")
        val (contig2, pos2, ori1, ori2) =
          variant.getAlternateAlleles.head.getDisplayString match {
            case bndRegexFF(c, p) => (c, p.toInt, true, true)
            case bndRegexFR(c, p) => (c, p.toInt, true, false)
            case bndRegexRF(c, p) => (c, p.toInt, false, true)
            case bndRegexRR(c, p) => (c, p.toInt, false, false)
            case _ =>
              throw new IllegalStateException(
                s"Contig and position not found in ${variant.getAlternateAlleles.head.getDisplayString}")
          }
        val (ciPos, ciEnd) = getCi(pos1, pos2)
        SvCall(contig1,
               pos1,
               contig2,
               pos2,
               svType,
               ciPos,
               ciEnd,
               caller :: Nil,
               ori1,
               ori2)
      case "DEL" | "INS" | "INV" | "DUP" | "CNV" =>
        val end = Option(variant.getAttribute("END"))
          .map(_.toString.toInt)
          .getOrElse(pos1) //TODO
        val (ciPos, ciEnd) = getCi(pos1, end)
        SvCall(contig1,
               pos1,
               contig1,
               end,
               svType,
               ciPos,
               ciEnd,
               caller :: Nil)
      case _ =>
        throw new IllegalStateException(s"Svtype '$svType' does not exist")
    }
  }
}
