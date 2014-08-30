package com.oohish.bitcoinz.messages

import scodec.Codec
import scodec.codecs._

case class Port(value: Int) {
  require(value >= Port.MinValue && value <= Port.MaxValue)
}

object Port {
  val MinValue = 0
  val MaxValue = 65535

  implicit val codec: Codec[Port] = uint16.xmap(Port.apply, _.value)
}