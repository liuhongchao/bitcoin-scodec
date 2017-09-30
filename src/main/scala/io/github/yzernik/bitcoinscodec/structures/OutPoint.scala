package io.github.yzernik.bitcoinscodec.structures

import scodec.Codec
import scodec.codecs._
import io.github.yzernik.bitcoinscodec.util.Util._

case class OutPoint(
  hash: Hash,
  index: Long)

object OutPoint {

  implicit val codec: Codec[OutPoint] = {
    ("hash" | Codec[Hash]) ::
      ("index" | customerizedUint32)
  }.as[OutPoint]

}
