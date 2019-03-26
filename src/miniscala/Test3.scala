//package miniscala
//
//import miniscala.Ast._
//import miniscala.TypeChecker._
//import miniscala.parser.Parser.parse
//
//object Test3 {
//
//  def main(args: Array[String]): Unit = {
//    test("1", IntType())
//    test("true", BoolType())
//    test("2.0f", FloatType())
//    test("\"foo\"", StringType())
//    test("5 - 2", IntType())
//    test("5.0f - 1.9f", FloatType())
//    test("5 - 2.3f", FloatType())
//    test("5.1f - 2.0f", FloatType())
//    testFail("\"foo\" - \"bar\"")
//    test("5 * 2", IntType())
//    test("5.0f * 2.0f", FloatType())
//    test("5 * 2.1f", FloatType())
//    test("5.1f * 2", FloatType())
//    testFail("\"foo\" * \"bar\"")
//    test("10 / 0", IntType())
//    test("10 / 0.0f", FloatType())
//    test("10 / 5", IntType())
//    test("10.0f / 2.0f", FloatType())
//    test("10 / 2.0f", FloatType())
//    test("10.2f / 2", FloatType())
//    testFail("\"foo\" / \"bar\"")
//    test("10 % 0", IntType())
//    test("10 % 0.0f", FloatType())
//    test("10 % 3", IntType())
//    test("10.0f % 2.0f", FloatType())
//    test("10 % 3.0f", FloatType())
//    test("10.5f % 10", FloatType())
//    testFail("\"foo\" % \"bar\"")
//    test("1 == 1", BoolType())
//    test("\"foo\" == \"bar\"", BoolType())
//    test("2 < 5", BoolType())
//    test("2.0f < 2.1f", BoolType())
//    test("2 < 2.1f", BoolType())
//    test("1.9f < 2", BoolType())
//    testFail("\"foo\" < \"bar\"")
//    test("2 <= 5", BoolType())
//    test("2.0f <= 2.0f", BoolType())
//    test("2 <= 1.9f", BoolType())
//    test("2.0f <= 2", BoolType())
//    testFail("\"foo\" <= \"bar\"")
//    test("2 max 5", IntType())
//    test("2.0f max 2.0f", FloatType())
//    test("2 max 1.9f", FloatType())
//    test("2.0f max 2", FloatType())
//    testFail("\"foo\" max \"bar\"")
//    test("true & true", BoolType())
//    test("false & true", BoolType())
//    testFail("2 & 5")
//    test("true | false", BoolType())
//    test("false | false", BoolType())
//    testFail("2 | \"foo\"")
//    test("!true", BoolType())
//    test("!false", BoolType())
//    test("if (true) 1 else 2", IntType())
//    test("if (false) true else false", BoolType())
//    testFail("if (true) \"foo\" else 2")
//    testFail("if (true) 2.0f else 2")
//    test("(1, 2) match { case (a,b) => a }", IntType())
//    test("(1, 2) match { case (a,b) => b }", IntType())
//    test("(1, 2, 3, 4) match { case (a,b,c,d) => d }", IntType())
//    test("{ val x = 4; x }", IntType())
//    test("(1,2)", TupleType(List(IntType(), IntType())))
//    test("(1, true, \"hello\")", TupleType(List(IntType(), BoolType(), StringType())))
//
//    // exercise 34
//    val env2 = Map("x" -> IntType(), "y" -> TupleType(List(IntType(), IntType())))
//    test("{ val z = 1; if (z == x) 42 else 17 }", IntType(), env2)
//    test("y match { case (a, b) => x <= (a + b)}", BoolType(), env2)
//    test("{ val x = (17, x == 2); y match { case (a, b) => { val z = (a + b, true); if (x == z) a - b else a + b } } }"
//      , IntType(), env2)
//
//    // exercise 35
//    test("if (true) 1 else 0", IntType())
//    testFail("{ val x: Int = 42; x + { val z = x == 2; if (z) true else false } }")
//    test("(1, 2) match { case (a, b) => (a <= b, a + b) }", TupleType(List(BoolType(), IntType())))
//
//    println("All tests succeeded")
//  }
//
//  def test(prg: String, out: Type, env: VarTypeEnv = Map[Var, Type]()): Unit = {
//    assert(typeCheck(parse(prg), env) == out)
//  }
//
//  def testFail(prg: String): Unit = {
//    try {
//      typeCheck(parse(prg), Map[Var, Type]())
//      assert(false)
//    } catch {
//      case _: TypeError => assert(true)
//      case _: Throwable => assert(false)
//    }
//  }
//}
