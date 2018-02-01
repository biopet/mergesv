/*
 * Copyright (c) 2017 Sequencing Analysis Support Core - Leiden University Medical Center
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

import nl.biopet.utils.tool.ToolCommand

object MergeSv extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    //TODO: Execute code

    logger.info("Done")
  }
  // TODO: Remove loremIpsum
  val loremIpsum: String =
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                              |Aliquam bibendum tellus sed lectus tristique egestas.
                              |Aenean malesuada lacus sed mollis hendrerit. Aliquam ac mollis sapien.
                              |Donec vel suscipit dui. Aenean pretium nibh in pulvinar consequat.
                              |Duis feugiat mattis erat, sed varius lectus eleifend vel.
                              |Etiam feugiat neque a dolor ornare pulvinar.
                              |
                              |Aenean id nibh mi.Fusce vel dapibus dui, quis dapibus felis.
                              |Aenean ipsum purus, bibendum a odio non, mattis efficitur dui.
                              |In fermentum est faucibus, bibendum urna sollicitudin, tempor erat.
                              |Vivamus aliquet nulla enim, non pharetra dui pulvinar id.
                              |Aliquam erat volutpat. Morbi tincidunt iaculis viverra.
                              |Suspendisse eget metus at lorem varius feugiat. Aliquam erat volutpat.
                              |Aliquam consequat nibh ut feugiat condimentum.
                              |Pellentesque aliquam cursus ex, ac consequat est viverra vitae.
                              |Donec purus orci, efficitur vel sem a, sodales aliquam tellus.
                              |Maecenas at leo posuere, tempus risus in, sodales ligula.
                              |Nam mattis enim a ligula iaculis vulputate. Nam fringilla.
                              """.stripMargin

  def descriptionText: String = loremIpsum.substring(0, 250)

  def manualText: String =
    s"""
      |${loremIpsum.substring(0, 250)} Example:
      |${example("-i", "<input_file>")}
    """.stripMargin

  def exampleText: String = loremIpsum.substring(0, 250)
}
