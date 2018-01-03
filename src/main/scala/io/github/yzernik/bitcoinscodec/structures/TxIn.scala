package io.github.yzernik.bitcoinscodec.structures

import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs._
import io.github.yzernik.bitcoinscodec.util.Util._

case class TxIn(
  previous_output: OutPoint,
  sig_script: ByteVector,
  witness_script: List[ByteVector],
  sequence: Long)

object TxIn {

  implicit val scriptCodec = {
    val countCodec = VarInt.varIntCodec.xmap(_.toInt, (i: Int) => i.toLong)
    variableSizeBytes(countCodec, bytes)
  }

  implicit val codec: Codec[TxIn] = {
    ("previous_output" | Codec[OutPoint]) ::
      ("sig_script" | scriptCodec) ::
      ("witness_script" | listOfN(provide(0), Codec[ByteVector])) ::
      ("sequence" | customerizedUint32)
  }.as[TxIn]

}
