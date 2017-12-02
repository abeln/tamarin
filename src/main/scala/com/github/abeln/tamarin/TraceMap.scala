package com.github.abeln.tamarin

import com.github.abeln.tamarin.SymInstr.{Instr, Trace}

/**
  * Maps traces to other traces
  */
abstract class TraceMap extends (Trace => Trace) {
  def apply(trace: Trace): Trace
}

object TraceMap {
  implicit def instrToTrace(i: Instr): Trace = Seq(i)
}
