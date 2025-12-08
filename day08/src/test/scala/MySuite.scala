// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day08 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "46398")
    assertEquals(score2, "8141888143")

  test("Day08 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "40")
    assertEquals(score2, "176904")
