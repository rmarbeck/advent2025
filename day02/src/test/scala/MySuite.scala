// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day02 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "5398419778")
    assertEquals(score2, "15704845910")

  test("Day02 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "1227775554")
    assertEquals(score2, "4174379265")
