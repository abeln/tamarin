package com.github.abeln.tamarin.mips.implementation

import com.github.abeln.tamarin.mips
import mips.{Bits, twoTo}

/** The implementation of a representation of a  32-bit sequence of bits. */
case class Word(private[implementation] val bits: Int) extends mips.Word {
  override def toString: String = toSeq.map {
    case false => '0'
    case true => '1'
  }.mkString

  implicit def toSeq = {
    var bits1 = bits
    var ret: List[Boolean] = Nil
    for(i <- 1 to 32) {
      ret = ((bits1%2) != 0) :: ret
      bits1 = bits1 >> 1
    }
    ret.toSeq
  }

  private[implementation] def asUnsigned: Long = if(bits<0) bits.toLong + twoTo(32) else bits.toLong
  private[implementation] def asSigned: Long = bits.toLong
}

object Word {
/** Create a word from a string.
  * Precondition: bits.length == 32
  * Precondition: forall i. 0 <= i <= 31 ==> bits(i) == '0' || bits(i) == '1'
  */
  def apply(bits: String): Word = Word(Bits(bits))

  /** Create a word from a sequence of bits.
    * Precondition: bits.length == 32.
    */
  def apply(bits: Seq[Boolean]): Word = {
    require(bits.length == 32)
    Word(bits.foldLeft(0){case (acc, bit) => (acc<<1)+(if(bit) 1 else 0)})
  }
  private[implementation] lazy val zero = mips.Word.zero.asInstanceOf[Word]
}