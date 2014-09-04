package com.oohish.bitcoinscodec.messages

import scodec.Codec
import scodec.codecs._
import com.oohish.bitcoinscodec.structures.UInt64

case class Pong(value: UInt64)

object Pong {

  /** Creates a Pong. */
  def apply(value: BigInt): Pong =
    Pong(UInt64(UInt64.bigIntToLong(value)))

  implicit val codec: Codec[Pong] =
    Codec[UInt64].xmap(Pong.apply, _.value)
}