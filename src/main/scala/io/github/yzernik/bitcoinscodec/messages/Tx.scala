package io.github.yzernik.bitcoinscodec.messages

import io.github.yzernik.bitcoinscodec.structures._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

case class Tx(
  version: Long,
  marker: Option[Int], // FIXME: do not expose marker and flags
  flags: Option[Int],
  tx_in: List[TxIn],
  tx_out: List[TxOut],
  witness_scripts: List[ByteVector],
  lock_time: Long
) extends Message {
  type E = Tx
  def companion = Tx
}

object Tx extends MessageCompanion[Tx] {

  implicit val witnessScriptCodec: Codec[ByteVector] = {
    val countCodec = VarInt.varIntCodec.xmap(_.toInt, (i: Int) => i.toLong)
    variableSizeBytes(countCodec, bytes)
  }

  def baseCodec(version: Int) = {
    ("version" | uint32L) ::
      ("marker" | conditional(false, uint8)) ::
      ("flags" | conditional(false, uint8)) ::
      ("tx_in" | VarList.varList(Codec[TxIn])) ::
      ("tx_out" | VarList.varList(Codec[TxOut])) ::
      ("witness_scripts" | listOfN(provide(0), Codec[ByteVector])) ::
      ("lock_time" | uint32L)
  }.as[Tx]

  def witnessCodec(version: Int) = {
    ("version" | uint32L) ::
      ("marker" | conditional(true, uint8)) ::
      ("flags" | conditional(true, uint8)) ::
      VarList.varList(Codec[TxIn]).flatPrepend { txIns =>
        ("tx_out" | VarList.varList(Codec[TxOut])) ::
          ("witness_scripts" | listOfN(provide(txIns.length), Codec[ByteVector])) ::
          ("lock_time" | uint32L)
      }
  }.as[Tx]

  def codec(version: Int): Codec[Tx] = {
    val txWithWitnessCodec: Codec[Tx] = witnessCodec(version)
    val txWithoutWitnessCodec = baseCodec(version)

    Codec(
      (transaction: Tx) => {
        txWithWitnessCodec.encode(transaction).orElse(txWithoutWitnessCodec.encode(transaction))
      },
      (bitVector: BitVector) => {
        txWithWitnessCodec.decode(bitVector).orElse(txWithoutWitnessCodec.decode(bitVector))
      }
    )
  }.as[Tx]

  def command = "tx"
}
