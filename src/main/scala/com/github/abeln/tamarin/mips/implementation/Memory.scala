package com.github.abeln.tamarin.mips.implementation

import com.github.abeln.tamarin.mips

/** An implementation of a memory (array of words) with functional update. */

case class Memory(private[implementation] val map: Map[Int, Vector[mips.Word]]) {
  private val pageSizeBits = 8

  private def pageOffset(addr: Int) = {
    require((addr & 3) == 0)
    require(addr >= 0)
    require(addr < asUnsigned(mips.CPU.maxAddr))

    val index = addr >> 2
    (index >> pageSizeBits, index & ((1 << pageSizeBits) - 1))
  }

  /** Read the word at memory location `address`. */
  def apply(address: Int) = {
    val (page, offset) = pageOffset(address)
    map.get(page).map(_(offset)).getOrElse(Word.zero)
  }

  /** Update the word at memory location `address` with new word `word`. */
  def updated(address: Int, word: mips.Word): Memory = {
    def newPage = Vector.fill(1<<pageSizeBits)(Word.zero)
    val (page, offset) = pageOffset(address)
    Memory(map + (page -> map.getOrElse(page, newPage).updated(offset, word)))
  }
}

object Memory {
  /** Create a new empty memory all of whose words are zero. */
  def apply(): Memory = Memory(Map())
}
