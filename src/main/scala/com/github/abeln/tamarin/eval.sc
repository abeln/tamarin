import com.github.abeln.tamarin.SSAConv
import com.github.abeln.tamarin.SymInstr._

val r1 = Reg(1)
val r2 = Reg(2)
val r3 = Reg(3)
val r4 = Reg(4)

val tr: Trace = Seq(
  Add(r1, r2, r3),
  Beq(r1, r2, 10),
  Add(r1, r1, r2),
  Sub(r1, r2, r2),
  Slt(r1, r2, r3),
  Bne(r1, r2, 20)
)

val ssa = SSAConv(tr)