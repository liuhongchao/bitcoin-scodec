package io.github.yzernik.bitcoinscodec.messages

import java.nio.file.{Files, Paths}

import io.github.yzernik.bitcoinscodec.CodecSuite
import scodec._
import scodec.bits._

class HeadersSpec extends CodecSuite {

  "Headers codec" should {
    "roundtrip" in {
      //roundtrip(headers)
    }

    "decode" in {
      val path = Paths.get(getClass.getResource("/headerspayload.data").toURI)
      val bytes = BitVector(Files.readAllBytes(path))

      val Attempt.Successful(DecodeResult(actual, rest)) = Headers.codec(1) decode bytes
      rest shouldBe BitVector.empty
    }

  }
}
