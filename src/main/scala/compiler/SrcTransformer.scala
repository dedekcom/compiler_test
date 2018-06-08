package compiler

import scala.io.BufferedSource

object SrcTransformer {

  def apply(src: BufferedSource): List[String] = {
    src.getLines
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
  }

}
