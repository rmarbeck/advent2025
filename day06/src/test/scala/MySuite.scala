// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day06 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "5595593539811")
    assertEquals(score2, "10153315705125")

  test("Day06 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "4277556")
    assertEquals(score2, "3263827")
