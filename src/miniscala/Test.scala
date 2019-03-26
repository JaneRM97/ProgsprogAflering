package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test {

  def main(args: Array[String]): Unit = {
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
    test("{ val start = 1; { def adder(a: Int): Int = start + a; { val start = 2; adder(10) } } }", IntVal(11), IntType())
    testFail("{val start = 1;{ def adder(a: Int): Int = start + a;{ val start = 2; adder(10.0f)}}}")
    test("{ def fib(n: Int): Int = if (n <= 1) n else fib(n - 1) + fib(n - 2); fib(10) }", IntVal(55), IntType())
    test("{ def isEven(n: Int): Boolean = if (n == 0) true else isOdd(n - 1); def isOdd(n: Int): Boolean = if (n == 0) false else isEven(n - 1); isEven(17) }", BoolVal(false), BoolType())

    testVal("{ def f(x) = x; f(2)}", IntVal(2))
    testTypeFail("{ def f(x) = x; f(2)}")

    testVal("{val x = 1; val g = {val x = 2; def f(a) = a+x; f}; {val x = 3; g(4) } }", IntVal(6))
    testVal("{val x = 1;val f = (a: Int) => a + x; f(2)}", IntVal(3))
    testVal("{ val x = 1;  { def q(a) = x + a;{ val x = 2; q(3)} } }", IntVal(4))
    testVal("{ val x = 1;  { def q() = {val x = 2;x };{ val t = q(); t+ x} } }", IntVal(3))
    //testVal("{ val x = 1; val z = { val x = 21; def f(a) = { val x = a + x; (x, a) }; (n) => f(n + 1) }; z(x) }", TupleVal())
    testType("{ def foo(x: Int => Int): Int = x(x(4)); foo((x: Int) => x % 2) }", IntType())
    //testType("{ def isEven(x) = if (x == 0) true else isOdd(x-1); def isOdd(x) = if (x == 0) false else isEven(x-1); isEven(2) }", BoolType())
    //testType("{val x = (f: Int => Int) => (x: Int) => f(f(x)); def g(a: Int) = a + 1; x(g)(2)}", IntVal(3), IntType()))

    // <-- add more test cases here
  }

  def test(prg: String, rval: Val, rtype: Type): Unit = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String): Unit = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, env: Env = Map[Id, Val]()): Unit = {
    assert(eval(parse(prg), env) == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()): Unit = {
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String): Unit = {
    try {
      eval(parse(prg), Map[Id, Val]())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String): Unit = {
    try {
      typeCheck(parse(prg), Map[Id, Type]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}