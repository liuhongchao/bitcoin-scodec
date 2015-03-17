package com.github.yzernik.bitcoinscodec.structures

import scala.language.implicitConversions

import scalaz.{ \/, \/-, -\/, Monad, Monoid }

import scodec.Codec
import scodec.codecs._
import scodec.bits.BitVector

object VarList {

  implicit def varList[A](codec: Codec[A]): Codec[List[A]] = {
    val countCodec = VarInt.varIntCodec.xmap(_.toInt, (i: Int) => i.toLong)
    listOfN(countCodec, codec)
  }
}