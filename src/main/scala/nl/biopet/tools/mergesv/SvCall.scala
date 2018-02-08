package nl.biopet.tools.mergesv

import htsjdk.variant.variantcontext.VariantContext

import scala.collection.JavaConversions._

case class SvCall(contig1: String,
                  pos1: Int,
                  contig2: String,
                  pos2: Int,
                  svType: String,
                  ciPosMin: Int,
                  ciPosPlus: Int,
                  ciEndMin: Int,
                  ciEndPlus: Int,
                  orientation1: Boolean = true,
                  orientation2: Boolean = true) { //TODO: Genotypes

}

object SvCall {

  val bndRegex = """.*[\[\]](.*):(\d+)[\[\]].*""".r

  def from(variant: VariantContext, defaultCi: Int): SvCall = {
    val contig1 = variant.getContig
    val pos1 = variant.getStart
    val svType: String = variant.getAttribute("SVTYPE") match {
      case s: String => s
      case _ => throw new IllegalStateException("Variant does not have tag 'SVTYPE'")
    }
    svType match {
      case "BND" =>
        require(variant.getAlternateAlleles.size() == 1, "Multiple alleles are not supported")
        val (contig2, pos2) = variant.getAlternateAlleles.head.getDisplayString match {
          case bndRegex(c, p) => (c, p.toInt)
          case _ => throw new IllegalStateException(s"Contig and position not found in ${variant.getAlternateAlleles.head.getDisplayString}")
        }
        //TODO: Orientation
        SvCall(contig1, pos1, contig2, pos2, svType, 0, 0, 0, 0)
      case "DEL" | "INS" | "INV" | "DUP" | "CNV" =>
        val end = Option(variant.getAttribute("END")).map(_.toString.toInt)
        SvCall(contig1, pos1, contig1, 0, svType, 0, 0, 0, 0)
      case _ => throw new IllegalStateException(s"Svtype '$svType' does not exist")
    }
  }
}