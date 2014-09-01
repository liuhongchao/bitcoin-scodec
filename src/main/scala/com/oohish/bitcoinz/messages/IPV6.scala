package com.oohish.bitcoinz.messages

import scodec.bits.ByteVector
import scodec.Codec
import scodec.codecs

case class IPV6(value: ByteVector) extends IP {
  override def toString = value.toIterable.grouped(2)
    .map { dig =>
      digitString(dig)
    }.mkString(":")

  def digitString(digit: Iterable[Byte]): String = {
    digit.map { b =>
      (0xff & b.toInt).toHexString
    }.mkString("")
  }
}

object IPV6 {
  implicit val codec: Codec[IPV6] = codecs.bytes(16).xmap[IPV6](v => IPV6(v), _.value)
}