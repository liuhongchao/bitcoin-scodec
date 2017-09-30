package io.github.yzernik.bitcoinscodec.util

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.security.MessageDigest

import scodec.bits.{ByteOrdering, ByteVector}
import io.github.yzernik.bitcoinscodec.structures.Hash
import scodec.Codec

object Util {

  def checksum(data: ByteVector): Long = {
    val hash = hashBytes(data.toArray)
    val padding: Array[Byte] = Array.fill(4)(0)
    val byteBuffer = ByteBuffer.wrap(hash.slice(0, 4) ++ padding)
      .order(ByteOrder.LITTLE_ENDIAN)
    byteBuffer.getLong()
  }

  def hash(bytes: Array[Byte]): CustomizedHash = {
    val hash = hashBytes(bytes)
    CustomizedHash(ByteVector(hash).reverse)
  }

  def hashBytes(bytes: Array[Byte]): Array[Byte] = {
    val messageDigest = MessageDigest.getInstance("SHA-256")
    val hash1 = messageDigest.digest(bytes)
    val hash2 = messageDigest.digest(hash1)
    hash2
  }


  val customerizedUint32: Codec[Long] = new CustomizedLongCodec(32, false, ByteOrdering.BigEndian)
}