// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day03 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "17113")
    assertEquals(score2, "169709990062889")

  test("Day03 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "357")
    assertEquals(score2, "3121910778619")
