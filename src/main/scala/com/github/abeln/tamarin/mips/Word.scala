package com.github.abeln.tamarin.mips

/** A representation of a 32-bit sequence of bits. */
trait Word {
  /** Returns the bits as a Scala Seq.
    */
  def toSeq: Seq[Boolean]

  /** Splits the bits into multiple `Seq`s of bits of a list of lengths `is`.
    * Example: `Word("01" * 16).splitAt(List(2,1)) == List(Bits("01"), Bits("0"), Bits("1" + "01"*14))`
    */
  def splitAt(is: List[Int]): List[Seq[Boolean]] = toSeq.splitAt(is)

}

object Word {
  /** Create a word from a string.
    * Precondition: bits.length == 32
    * Precondition: forall i. 0 <= i <= 31 ==> bits(i) == '0' || bits(i) == '1'
    */
  def apply(bits: String): Word = implementation.Word(bits)

  /** Create a word from a sequence of bits.
    * Precondition: bits.length == 32.
    */
  def apply(bits: Seq[Boolean]): Word = implementation.Word(bits)

  /** The word of 32 zero bits. Scala note: "0" * 32 is shorthand for "00000000000000000000000000000000".
    */
  val zero = Word("0" * 32)

  /** Allows the `Word` to be used as if it were a `Seq[Boolean]`. */
  implicit def wordToSeq(word: Word) = word.toSeq
}
