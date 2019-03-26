package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker.{TypeError, checkTypesEqual}
import miniscala.Unparser.unparse

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type TypeEnv = Map[Id, Type]

  def typeCheck(e: Exp, tenv: TypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case VarExp(x) =>
      tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e))
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv)
      val righttype = typeCheck(rightexp, tenv)
      op match {
        case PlusBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case (StringType(), StringType()) => StringType()
            case (StringType(), IntType()) => StringType()
            case (StringType(), FloatType()) => StringType()
            case (IntType(), StringType()) => StringType()
            case (FloatType(), StringType()) => StringType()
            case _ => throw new TypeError(s"Type mismatch at '+', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case MinusBinOp() | MultBinOp() | DivBinOp() | ModuloBinOp() | MaxBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case _ => throw new TypeError(s"Type mismatch at '-,*,/,%,max', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
            case EqualBinOp() =>
              if (lefttype == righttype) BoolType()
              else throw new TypeError(s"Type mismatch at '==', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)

            case LessThanBinOp() | LessThanOrEqualBinOp() =>
              (lefttype, righttype) match {
                case (IntType(), IntType()) => BoolType()
                case (FloatType(), FloatType()) => BoolType()
                case (IntType(), FloatType()) => BoolType()
                case (FloatType(), IntType()) => BoolType()
                case (_,_) => throw new TypeError(s"Type mismatch at '<,<=', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
              }

            case AndBinOp() | OrBinOp() =>
              (lefttype, righttype) match {
                case (BoolType(), BoolType()) => BoolType()
                case _ => throw new TypeError(s"Type mismatch at '&,|', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
              }
          }
    case UnOpExp(op, exp) =>
      val exptype = typeCheck(exp, tenv)
      op match {
        case NegUnOp() =>
          exptype match {
            case IntType() => IntType()
            case FloatType() => FloatType()
            case _ => throw new TypeError(s"Type mismatch at '-', unexpected types ${unparse(exptype)}", op)
          }
        case NotUnOp() =>
          exptype match {
            case BoolType() => BoolType()
            case _ => throw new TypeError(s"Type mismatch at '!', unexpected types ${unparse(exptype)}", op)

          }
      }

    case IfThenElseExp(condexp, thenexp, elseexp) =>
      typeCheck(condexp, tenv) match {
        case BoolType() =>
          val a = typeCheck(thenexp, tenv)
          val b = typeCheck(elseexp, tenv)
          if(a == b) a else throw new TypeError(s"Type mismatch at '!', unexpected types ${unparse(typeCheck(condexp, tenv))}", condexp)
        case _ => throw new TypeError(s"Type mismatch at '!', unexpected types ${unparse(typeCheck(condexp, tenv))}", condexp)
      }

    case BlockExp(vals, defs, exp) =>
      var tenv1 = tenv
      for (d <- vals) {
        val typeOf = typeCheck(d.exp, tenv1)
        checkTypesEqual(typeOf, d.opttype, d)
        tenv1 = tenv1 + (d.x -> d.opttype.getOrElse(typeOf))
      }
      for (d <- defs) {
        tenv1 = tenv1 + (d.fun -> getFunType(d))
      }
      for(d <- defs) {
        var dvtenv = tenv1
        for (FunParam(x, Some(t)) <- d.params) // must be 'Some' since we called getFunType earlier?
          dvtenv = dvtenv + (x -> t)
        val typeCheckedBody = typeCheck(d.body, dvtenv)
        checkTypesEqual(typeCheckedBody, d.optrestype, d)
      }
      typeCheck(exp, tenv1)

    case TupleExp(exps) =>
      var vals = List[Type]()
      for(exp <- exps)
        vals = typeCheck(exp, tenv) :: vals
      TupleType(vals.reverse)

    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, tenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              val xs = c.pattern.zip(ts)
              return typeCheck(c.exp, tenv ++ xs)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(funexp, args) =>
      val (paramtypes, restype) = typeCheck(funexp, tenv) match {
        case FunType(paramtypes, restype) => (paramtypes, restype)
        case _ => throw new TypeError(s"Found no types", e)
      }
      val argsTypeList = args.map(typeCheck(_, tenv))
      if(paramtypes.length == argsTypeList.length)
        argsTypeList.zip(paramtypes).foreach {
          case (a, param) => checkTypesEqual(a, Some(param), e)}
      else throw new TypeError(s"Type mismatch", e)
      restype

    case LambdaExp(params, body) =>
      var ptenv = tenv
        for (FunParam(x, t) <- params) {
          val paramtype = t.getOrElse(throw new TypeError(s"", e))
          ptenv = ptenv + (x -> paramtype)
        }
        val typeCheckedBody = typeCheck(body, ptenv)
        FunType(params.map(p => p.opttype.getOrElse(throw new TypeError(s"", e))), typeCheckedBody)
  }

  /**
    * Returns the function type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): FunType =
    FunType(d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw new TypeError(s"Type annotation missing at function result ${d.fun}", d)))

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new TypeError(s"Type mismatch: expected type ${unparse(t2)}, found type ${unparse(t1)}", n)
    case None => // do nothing
  }

  /**
    * Builds an initial type environment, with a type for each free variable in the program.
    */
  def makeInitialTypeEnv(program: Exp): TypeEnv = {
    var tenv: TypeEnv = Map()
    for (x <- Vars.freeVars(program))
      tenv = tenv + (x -> IntType())
    tenv
  }

  /**
    * Exception thrown in case of MiniScala type errors.
    */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
