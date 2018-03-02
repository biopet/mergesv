/*
 * Copyright (c) 2018 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.mergesv

import java.io.File

import nl.biopet.utils.tool.{AbstractOptParser, ToolCommand}
import nl.biopet.utils.io

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {
  opt[(String, File)]('i', "inputFile")
    .unbounded()
    .action {
      case ((key, file), c) => c.addFile(key, file)
    }
    .valueName("<caller>=<file>")
    .text("Input vcf files to merge into a single file")
  opt[File]("inputFileList")
    .unbounded()
    .action {
      case (x, c) =>
        io.getLinesFromFile(x).foldLeft(c) {
          case (a, line) =>
            val values = line.split("\t")
            require(values.size == 2)
            a.addFile(values(0), new File(values(1)))
        }
    }
    .valueName("<tsv file>")
    .text("file with input files, first column is the caller, second column is the path to the file, separated by a tab")
  opt[File]('o', "outputFile")
    .required()
    .action((x, c) => c.copy(outputFile = x))
    .text("Output vcf file")
  opt[File]('R', "referenceFasta")
    .required()
    .action((x, c) => c.copy(referenceFasta = x))
    .text("Reference fasta file")
  opt[Int]("windowsSize")
    .action((x, c) => c.copy(windowsSize = x))
    .text(s"Size of sliding window, default is ${Args().windowsSize} basepair")
  opt[Int]("defaultCi")
    .action((x, c) => c.copy(defaultCi = x))
    .text(
      s"Interval when caller does not give one, default is ${Args().defaultCi} basepair")
  opt[Unit]("keepNonVariant")
    .action((_, c) => c.copy(keepNonVariant = true))
    .text(
      s"By default calls without a genotype with a alternative allele will be filtered. " +
        s"This option keeps them in the merging.")
  opt[(String, String)]("callerField")
    .action {
      case ((k, v), c) =>
        val newValue = c.callerFields.getOrElse(k, Set()) + v
        val newMap = c.callerFields + (k -> newValue)
        c.copy(callerFields = newMap)
    }
    .unbounded()
    .valueName("<caller>=<key>")
    .text("Format fields to copy to new vcf file.")
}
