import com.github.abeln.tamarin.{Desugar, Query, SSAConv}
import com.github.abeln.tamarin.SymInstr._

Query.solve(trace(
  Add(Reg(3), Reg(1), Reg(2)),
  EqCond(Reg(3), Lit(42))
))