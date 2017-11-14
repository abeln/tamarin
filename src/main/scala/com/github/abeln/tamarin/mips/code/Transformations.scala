package com.github.abeln.tamarin.mips.code

import ProgramRepresentation._
import com.github.abeln.tamarin.mips.Word
import com.github.abeln.tamarin.mips.assembler.Assembler

import scala.collection.mutable

/** Implementations of various transformations on the `Code` objects defined in `ProgramRepresentation.scala`.
  * In general, the transformations successively eliminate various types of `Code` objects by translating them
  * into sequences of simpler `Code`s, until only `Code`s directly representing machine instructions are left.
  */

object Transformations {
  private val WordSize = 4 /* 1 word = 4 bytes */
  private def error(msg: String): Nothing = {
    throw new RuntimeException(msg)
  }

  /* ############################################################### */
  /* ## Assignment 2 ############################################### */
  /* ############################################################### */

  /* Before doing Assignment 2, read the code in the first half of `ProgramRepresentation.scala`
   * (up to the `Block` case class) to get an idea of how we will represent programs using the various subclasses
   * of the `Code` class.
   *
   * Hint: You can navigate to the `Code` class by Ctrl-clicking on any use of the word `Code`.
   */

  /* Complete the implementation of the following method by replacing the `???`. */

  /** Given a sequence of `Code`s that may be one of `CodeWord`, `Define`, `Use`, or `BeqBne`,
    * resolve all of the labels, and output the corresponding MIPS machine-language program
    * as a sequence of `Word`s.
    *
    * Refer to `ProgramRepresentation.scala` for documentation of the meanings of these `Code`s.
    *
    * If a label is defined multiple times or if a label is used but not defined, call
    * `error()` with an appropriate error message.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Seq
    *
    * The high-level structure of this method is given for you, but you should learn about
    * methods such as `foreach` and `flatMap` because you may want to use them yourself in
    * later assignments.
    */
  def eliminateLabels(code: Seq[Code]): Seq[Word] = {
    val labelToValue = mutable.Map[Label, Int]()

    /* First pass: fill in the `labelToValue` map with an address for each label. */
    def setLabels(): Unit = {
      var location = 0
      code.foreach {
        case Define(label) =>
          if (labelToValue contains label) {
            error(s"Label $label defined twice")
          } else {
            labelToValue(label) = location
          }
        case _ => location += WordSize
      }
    }

    /* Second pass: replace each `Code` with an equivalent sequence of `Word`s. */
    def translate: Seq[Word] = {
      var location = 0

      // looks up a given label, failing if label isn't present
      def lookup(label: Label): Long = {
        if (labelToValue contains label) {
          labelToValue(label)
        } else {
          error(s"Tried to lookup label $label, which doesn't exist")
        }
      }

      // offset between a label and the current location, in words
      def offset(label: Label): Long = {
        (lookup(label) - location) / 4
      }

      code.flatMap {
        (code) =>
          val res = code match {
            case Define(label) => Seq.empty[Word]
            case CodeWord(word) => Seq(word)
            case Use(label) =>
              Seq(Word(Assembler.encodeUnsigned(lookup(label))))
            case BeqBne(bits, label) =>
              Seq(Word(bits ++ Assembler.encodeSigned(offset(label) - 1, 16)))
            case _ => error(s"Encountered unsupported code $code.")
          }
          if (!code.isInstanceOf[Define]) {
            location += WordSize
          }
          res
      }
    }

    setLabels()
    translate
  }

  /** Links two sequences of code together to form a single program. */
  def link(codes1: Seq[Code], codes2: Seq[Code]): Seq[Code] = codes1 ++ codes2

  /** Remove all `Comment`s from a sequence of `Code`s.
    *
    * Assumes that the input sequence does not contain any `Code`
    * types that are defined after `Block` in `ProgramRepresentation.scala`.
    */
  def eliminateComments(codes: Seq[Code]): Seq[Code] = {
    codes filterNot(_.isInstanceOf[Comment])
  }

  /** Entry point to convert from assembly with labels and comments into machine code. */
  def toMachineCode(codes: Seq[Code]): Seq[Word] = {
    val noComments = eliminateComments(codes)
    eliminateLabels(noComments)
  }
}

