import miniscala.Ast._

object VarEnv {
  sealed abstract class VarEnv {
    def extend(x: Id, v: Int): VarEnv = ConsVarEnv(x, v, this)
    def lookup(x: Id): Int
  }

  case class ConsVarEnv(x: Id, v: Int, next: VarEnv) extends VarEnv {
    override def lookup(y: Id): Int = {
      if (x == y) v
      else next.lookup(y)
    }
  }

  case object NilVarEnv extends VarEnv {
    override def lookup(x: Id): Int = throw new RuntimeException("key not found")
  }

  def makeEmpty(): VarEnv = NilVarEnv

}
