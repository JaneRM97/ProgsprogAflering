package miniscala

import miniscala.Ast._
import miniscala.Interpreter._

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(e: AstNode): String = e match {
    case IntLit(c) => s"$c"
    case BoolLit(c) => s"${c}f"
    case FloatLit(c) => s"$c"
    case StringLit(c) => s"""$c"""
    case VarExp(x) => x

    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = unparse(leftexp)
      val rightval = unparse(rightexp)
      op match {
        case PlusBinOp() => s"($leftval+$rightval)"
        case MinusBinOp() => s"($leftval-$rightval)"
        case MultBinOp() => s"($leftval*$rightval)"
        case DivBinOp() => s"($leftval/$rightval)"
        case ModuloBinOp() => s"($leftval%$rightval)"
        case MaxBinOp() => s"($leftval max $rightval)"
        case EqualBinOp() => s"($leftval == $rightval)"
        case LessThanBinOp() => s"($leftval < $rightval)"
        case LessThanOrEqualBinOp() => s"($leftval <= $rightval)"
        case AndBinOp() => s"($leftval & $rightval)"
        case OrBinOp() => s"($leftval | $rightval)"

      }
    case UnOpExp(op, exp) =>
      val expval = unparse(exp)
      op match {
        case NegUnOp() => s"(-$expval)"

        case NotUnOp() => s"(!$expval)"
      }

    case IfThenElseExp(condexp, thenexp, elseexp) => s"if (${unparse(condexp)}) ${unparse(thenexp)} else ${unparse(elseexp)}"

    case BlockExp(vals, defs, exp) =>
      var blockstring: String = s""
      for (d <- vals) {
        blockstring = s"${blockstring}val ${d.x}=${unparse(d.exp)};"

      }
      s"{$blockstring${unparse(exp)}}"

    case TupleExp(exps) =>
      var e = ""
      for(exp <- exps) {
        e += unparse(exp) + ", "
      }
      s"($e)"

    case MatchExp(exp, cases) => exp + cases.map(unparse).mkString("(", ";", ")")

    case MatchCase(pattern, exp) => s"$exp match { case ${pattern.mkString("(", ";", ")")}"

    case ValDecl(x, None, exp) => s"val $x  = ${unparse(exp)}"
    case ValDecl(x, Some(t), exp) => s"val $x: ${unparse(t)} = ${unparse(exp)}"

    case IntType() => "Int"
    case BoolType() => "Boolean"
    case FloatType() => "Float"
    case StringType() => "String"
    case TupleType(types) => s"(${types.map(unparse).mkString(",")})"

    case CallExp(funexp, args) => unparse(funexp) + args.map(unparse).mkString("(", ",", ")")
    case LambdaExp(params, body) => s"${params.map(unparse).mkString("(", ",", ")")} => ${unparse(body)}"
    case FunParam(x, opttype) => x
  }
}
