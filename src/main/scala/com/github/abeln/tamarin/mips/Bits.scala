package com.github.abeln.tamarin.mips

/** An object containing utility methods for writing and pattern matching sequences of bits
  * using strings.
  */
object Bits {
  /** Constructs a bit sequence from a string.
    * Example: `Bits("01") == Seq(false, true)`
    */
  def apply(bits: String): Seq[Boolean] = bits.map{
    case '0' => false
    case '1' => true
    case c => sys.error(s"illegal bit: $c")
  }

  /** Enables pattern matching of bit sequences using strings.
    * Example: {{{
    * (Seq(false, true) match {
    *   case Bits("01") => 42
    * }) == 42
    * }}}
    */
  def unapply(bits: Seq[Boolean]): Option[String] = Some(bits.map{if(_) '1' else '0'}.mkString)
}
