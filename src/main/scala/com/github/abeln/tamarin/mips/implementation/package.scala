package com.github.abeln.tamarin.mips

import com.github.abeln.tamarin.mips

package object implementation {
  private[mips] def asUnsigned(word: mips.Word) = word.asInstanceOf[implementation.Word].bits.toLong & 0xffffffffL
  private[mips] def asSigned(word: mips.Word) = word.asInstanceOf[implementation.Word].bits.toLong
  private[mips] def encodeUnsigned(number: Long): mips.Word = {
    require(0L <= number)
    require(number <= 0xffffffffL)
    Word(number.toInt)
  }
  private[mips] def encodeSigned(number: Long): mips.Word = {
    require(-twoTo(31) <= number)
    require(number < twoTo(31))
    Word(number.toInt)
  }
  private[mips] def encodeUnsigned64(number: Long): Seq[Boolean] = {
    encodeUnsigned(number >>> 32) ++ encodeUnsigned(number & 0xffffffffL)
  }
  private[mips] def encodeSigned64(number: Long): Seq[Boolean] = encodeUnsigned64(number)
  private[mips] def asUnsigned(bits: Seq[Boolean]): Long =
    bits.foldLeft(0L){case (acc, bit) => (acc<<1)+(if(bit) 1L else 0L)}
  private[mips] def asSigned(bits: Seq[Boolean]): Long = {
    require(bits.size > 0)
    val tail = implementation.asUnsigned(bits.tail)
    if(bits.head) tail - twoTo(bits.length-1) else tail
  }
  private[mips] def incrementAddress(word: mips.Word): mips.Word = {
    encodeUnsigned((asUnsigned(word) + 4) & 0xffffffff)
  }
}
