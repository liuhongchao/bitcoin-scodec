package com.oohish.bitcoinscodec.structures

import scalaz.\/

import com.oohish.bitcoinscodec.CodecSuite
import scodec.bits._
import scodec.codecs._
import scala.math.BigInt.long2bigInt

class IPV4Spec extends CodecSuite {

  import IPV4._

  "IPV4 codec" should {
    "roundtrip" in {
      roundtrip(IPV4("10.0.0.1"))
      roundtrip(IPV4("10.0.0.255"))
    }

    "print" in {
      IPV4("10.0.0.1").toString shouldBe "10.0.0.1"
    }

    "encode" in {
      codec.encode(IPV4("10.0.0.1")) shouldBe
        \/.right(hex"0A 00 00 01".toBitVector)
    }

    "decode" in {
      codec.decode(hex"0A 00 00 01".toBitVector) shouldBe
        \/.right(BitVector.empty, IPV4("10.0.0.1"))
    }
  }
}
