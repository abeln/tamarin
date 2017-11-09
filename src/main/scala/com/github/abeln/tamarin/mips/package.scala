package com.github.abeln.tamarin

package object mips {
  /** Given `power`, computes 2^`power`^. */
  def twoTo(power: Int): Long = (1L<<power)

  /** Given a `number`, finds a `result` congruent to it modulo 2^32^ and within the
    * range `rangeStart` to `rangeStart`+2^32^-1 (inclusive).
    */
  private def mod2to32Range(number: Long, rangeStart: Long): Long = {
    var remainder = number % twoTo(32)
    while(remainder < rangeStart) remainder += twoTo(32)
    while(remainder >= rangeStart + twoTo(32)) remainder -= twoTo(32)
    remainder
  } ensuring(result =>
    (result - number) % twoTo(32) == 0
      && result >= rangeStart
      && result < rangeStart + twoTo(32)
  )

  /** Given a `number`, finds a `result` congruent to it modulo 2^32^ and within the
    * range 0 to 2^32^-1 (inclusive).
    */
  def mod2to32(number: Long): Long = mod2to32Range(number, 0)

  /** Given a `number`, finds a `result` congruent to it modulo 2^32^ and within the
    * range -2^31^ to 2^31^-1 (inclusive).
    */
  def mod2to32signed(number: Long): Long = mod2to32Range(number, -twoTo(31))

  /** Enables a `Seq` to be split into multiple `Seq`s at a list of offsets. */
  implicit class SeqImprovements[A](seq: Seq[A]) {
    /** Splits a `Seq` into multiple `Seq`s of a list of lengths `is`.
      * Example: `Seq(1,2,3,4,5).splitAt(List(2,1)) == List(Seq(1,2), Seq(3), Seq(4,5))`
      */
    def splitAt(is: List[Int]): List[Seq[A]] = is match {
      case ihead :: itail =>
        val (head, tail) = seq.splitAt(ihead)
        head :: tail.splitAt(itail)
      case Nil => List(seq)
    }
  }
}
