// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day05 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "701")
    assertEquals(score2, "352340558684863")

  test("Day05 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "3")
    assertEquals(score2, "14")
