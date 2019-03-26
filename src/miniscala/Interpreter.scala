package miniscala

import miniscala.Ast._
import miniscala.Interpreter.FloatVal
import miniscala.Unparser.unparse

import scala.io.StdIn

/**
  * Interpreter for MiniScala.
  */
object Interpreter {

  sealed abstract class Val

  case class IntVal(v: Int) extends Val

  case class BoolVal(v: Boolean) extends Val

  case class FloatVal(v: Float) extends Val

  case class StringVal(v: String) extends Val

  case class TupleVal(vs: List[Val]) extends Val

  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, env: Env, defs: List[DefDecl]) extends Val

  type Env = Map[Id, Val]

  def eval(e: Exp, env: Env): Val = e match {
    case IntLit(c) => IntVal(c)
    case BoolLit(c) => BoolVal(c)
    case FloatLit(c) => FloatVal(c)
    case StringLit(c) => StringVal(c)
    case VarExp(x) =>
      env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) => trace(s"BinOpExp")
      val leftval = eval(leftexp, env)
      val rightval = eval(rightexp, env)
      op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 + v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 + v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 + v2)
            case (StringVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case (StringVal(v1), IntVal(v2)) => StringVal(v1 + v2)
            case (StringVal(v1), FloatVal(v2)) => StringVal(v1 + v2)
            case (IntVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case (FloatVal(v1), StringVal(v2)) => StringVal(v1 + v2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() => trace(s"$leftval-$rightval")
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 - v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 - v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 - v2)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() => trace(s"$leftval*$rightval")
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 * v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 * v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 * v2)
            case _ => throw new InterpreterError(s"Type mismatch at '*', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case DivBinOp() => trace(s"$leftval/$rightval")
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 / v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 / v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 / v2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() => trace(s"$leftval modulo $rightval")
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Modulo by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(v1 % v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(v1 % v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(v1 % v2)
            case _ => throw new InterpreterError(s"Type mismatch at '%', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => IntVal(if (v1 > v2) v1 else v2)
            case (FloatVal(v1), FloatVal(v2)) => FloatVal(if (v1 > v2) v1 else v2)
            case (IntVal(v1), FloatVal(v2)) => FloatVal(if (v1 > v2) v1 else v2)
            case (FloatVal(v1), IntVal(v2)) => FloatVal(if (v1 > v2) v1 else v2)
            case _ => throw new InterpreterError(s"Type mismatch at 'max', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 == v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 == v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 == v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 == v2)
            case (StringVal(v1), StringVal(v2)) => BoolVal(v1 == v2)
            case (TupleVal(vs), TupleVal(xs)) => BoolVal(vs == xs)
            case _ => throw new InterpreterError(s"Type mismatch at '==', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 < v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 < v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 < v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 < v2)
            case _ => throw new InterpreterError(s"Type mismatch at '<', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case LessThanOrEqualBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => BoolVal(v1 <= v2)
            case (FloatVal(v1), FloatVal(v2)) => BoolVal(v1 <= v2)
            case (IntVal(v1), FloatVal(v2)) => BoolVal(v1 <= v2)
            case (FloatVal(v1), IntVal(v2)) => BoolVal(v1 <= v2)
            case _ => throw new InterpreterError(s"Type mismatch at '<=', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 & v2)
            case _ => throw new InterpreterError(s"Type mismatch at '&', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case OrBinOp() =>
          (leftval, rightval) match {
            case (BoolVal(v1), BoolVal(v2)) => BoolVal(v1 | v2)
            case _ => throw new InterpreterError(s"Type mismatch at '|', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
      }
    case UnOpExp(op, exp) => trace(s"UnOpExp")
      val expval = eval(exp, env)
      op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => IntVal(-v)
            case FloatVal(v) => FloatVal(-v)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() =>
          expval match {
            case BoolVal(v) => BoolVal(!v)
            case _ => throw new InterpreterError(s"Type mismatch at '!', unexpected value ${valueToString(expval)}", op)
          }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      eval(condexp, env) match {
        case BoolVal(v) => if (v) eval(thenexp, env) else eval(elseexp, env)
        case _ => throw new InterpreterError(s"Type mismatch at 'IfThenElseExp', unexpected value ${valueToString(eval(condexp, env))}", condexp)
      }

    case BlockExp(vals, defs, exp) => trace(s"{Decl ; Exp}")
      var env1 = env
      for (d <- vals) {
        val v = eval(d.exp, env1)
        checkValueType(v, d.opttype, e)
        env1 = env1 + (d.x -> v)
      }
      for (d <- defs) {
        env1 = env1 + (d.fun -> ClosureVal(d.params, d.optrestype, d.body, env1, defs))
      }
      eval(exp, env1)

    case TupleExp(exps) =>
      var vals = List[Val]()
      for (exp <- exps)
        vals = eval(exp, env) :: vals
      TupleVal(vals.reverse)

    case MatchExp(exp, cases) =>
      val expval = eval(exp, env)
      expval match {
        case TupleVal(vs) => {
          for (c <- cases)
            if (vs.length == c.pattern.length) {
              val xs = c.pattern.zip(vs)
              return eval(c.exp, env ++ xs)
            }
        }
          throw new InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
      //Nu med ClosureVal, hvor defs er tilføjet for at tillade mutual recursion
    case CallExp(funexp, args) =>
      val (params, optrestype, body, closureEnv, defs) = eval(funexp, env) match {
        case ClosureVal(params, optrestype, body, env, defs) => (params, optrestype, body, env, defs)
        case _ => throw new InterpreterError("Error", e)
      }
      var env1 = closureEnv
      if (args.length == params.length) {
        for ((p, arg) <- params.zip(args)) {
          val argEval = eval(arg, env)
          //Dynamisk typetjek af parameterrypen
          checkValueType(argEval, p.opttype, e)
          env1 = env1 + (p.x -> argEval)
        }
      }
      else throw new InterpreterError("Wrong number of arguments", e)
      for (d <- defs) {
        env1 = env1 + (d.fun -> ClosureVal(d.params, d.optrestype, d.body, closureEnv, defs))
      }
      //Dynamisk typetjek af returtypen
      val res = eval(body, env1)
      checkValueType(res, optrestype, e)
      res

    case LambdaExp(params, body) =>
      //lambda har ikke typeaanot., så vi skriver None i stedet for optrestype.
      //Tilsidst en tom liste af defdecl som tillader mutual recursion. Lambda tager ikke defs?
      ClosureVal(params, None, body, env, List[DefDecl]())
  }

  /**
    * Checks whether value `v` has type `ot` (if present), generates runtime type error otherwise.
    */
  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t) =>
      (v, t) match {
        case (IntVal(_), IntType()) |
             (BoolVal(_), BoolType()) |
             (FloatVal(_), FloatType()) |
             (IntVal(_), FloatType()) |
             (StringVal(_), StringType()) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case (ClosureVal(cparams, optcrestype, _, _, _), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, p.opttype, n)
          checkTypesEqual(restype, optcrestype, n)
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new InterpreterError(s"Type mismatch: type ${unparse(t1)} does not match expected type ${unparse(t2)}", n)
    case None => // do nothing
  }

  /**
    * Converts a value to its string representation (for error messages).
    */
  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(v => valueToString(v)).mkString("(", ",", ")")
    case ClosureVal(params, _, exp, _, _) => // the resulting string ignores the result type annotation and the declaration environment
      s"<(${params.map(p => unparse(p)).mkString(",")}), ${unparse(exp)}>"
  }

  def simplify(e: Exp): Exp = e match {
    case IntLit(_) | VarExp(_) => e
    case BinOpExp(leftexp, op, rightexp) =>
      val left = simplify(leftexp)
      val right = simplify(rightexp)
      (left, op, right) match {
        case (IntLit(0), PlusBinOp(), _) => right
        case (_, PlusBinOp(), IntLit(0)) => left

        case (l, MinusBinOp(), r) if l == r => IntLit(0)
        case (l, MinusBinOp(), IntLit(0)) => l

        case (l, MultBinOp(), IntLit(1)) => l
        case (IntLit(1), MultBinOp(), r) => r
        case (_, MultBinOp(), IntLit(0)) => IntLit(0)
        case (IntLit(0), MultBinOp(), _) => IntLit(0)

        case (IntLit(0), DivBinOp(), _) => IntLit(0)
        case (l, DivBinOp(), r) if l == r => IntLit(1)

        case (l, ModuloBinOp(), r) if l == r => IntLit(0)

        case (l, MaxBinOp(), r) if l == r => l

        case _ => BinOpExp(left, op, right)
      }
    case UnOpExp(op, exp) => UnOpExp(op, simplify(exp))
    /* case BlockExp(vals, exp) =>
  var venv1 =
  for (d <- vals)
     BlockExp(vals, simplify(exp, venv1)

  /* case BlockExp(vals, exp) => trace(s"{Decl ; Exp}")
  var venv1 = venv
  for (d <- vals)
    venv1 = venv1 + (d.x -> eval(d.exp, venv1))
  eval(exp, venv1) */*/
  }

  /**
    * Builds an initial environment, with a value for each free variable in the program.
    */
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> IntVal(StdIn.readInt()))
    }
    env
  }

  /**
    * Prints message if option -trace is used.
    */
  def trace(msg: String): Unit =
    if (Options.trace)
      println(msg)

  /**
    * Exception thrown in case of MiniScala runtime errors.
    */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)
}