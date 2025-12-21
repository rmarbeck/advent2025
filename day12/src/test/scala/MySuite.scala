// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day12 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "414")
    assertEquals(score2, "it s a win")

  test("Day12 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "3")
    assertEquals(score2, "it s a win")