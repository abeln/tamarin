package com.github.abeln.tamarin.mips.assembler

/** Here, we give symbolic names to registers to keep track of what we will use each register for. Feel free
  * to add your own register names here for your own uses of registers.
  */
object Reg {
  /** Holds the old program counter value after a JALR instruction has executed. */
  val savedPC = Reg(31)
  /** Address of the top item in the stack. */
  val stackPointer = Reg(30)
  /** Address of the frame of the currently executing procedure. */
  val framePointer = Reg(29)
  /** Address of the next free word of memory on the heap. */
  val heapPointer = Reg(28)
  /** Address just after the end of the current semi-space of the heap. */
  val semiSpaceTop = Reg(27)

  /** The parameters given as input to the program. */
  val input1 = Reg(1)
  val input2 = Reg(2)
  /** Contains the
    *  of the most recently evaluated expression. */
  val result = Reg(3)
  /** Scratch register to be used for storing temporary results during evaluation of an expression. */
  val scratch = Reg(4)
  /** In a procedure prologue, temporarily holds the address of the Chunk holding the arguments to the
    * procedure while the frame for the procedure is being created.
    */
  val savedParamPtr = Reg(5)
  /** Holds the address of the most recently allocated Chunk of memory. */
  val allocated = Reg(6)
  /** An extra scratch register available to be used in the `copyChunk` method. */
  val copyChunkScratch = Reg(7)
  /** Used to hold the address of the procedure to be called in the implementation of a call to a procedure. */
  val targetPC = Reg(8)
  /** An extra scratch register available to be used by the garbage collector. */
  val scratchPtrForGC = Reg(9)

  /** The special register that always has the value zero. */
  val zero = Reg(0)
}

case class Reg(number: Int) {
  require(number >= 0)
  require(number <= 31)
}
