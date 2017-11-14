package com.github.abeln.tamarin
import com.github.abeln.tamarin.SymInstr._
import com.github.abeln.tamarin.TraceMap.instrToTrace

/**
  * Desugaring of "complicated" MIPS instructions.
  */
object Desugar extends TraceMap {
  override protected def transform: PartialFunction[SymInstr.Instr, Trace] = {
    case Jalr(concretePC) => assign(savedPC, Lit(concretePC))
    case Mult(s, t) => trace(
      Mult64(TMP, s, t),
      Low32(LO, TMP),
      High32(HI, TMP)
    )
    case MultU(s, t) => trace(
      MultU64(TMP, s, t),
      Low32(LO, TMP),
      High32(HI, TMP)
    )
    case Div(s, t) => trace(
      Quot(LO, s, t),
      Rem(HI, s, t)
    )
    case DivU(s, t) => trace(
      QuotU(LO, s, t),
      RemU(HI, s, t)
    )
    case Mflo(d) => assign(d, LO)
    case Mfhi(d) => assign(d, HI)
  }
}
