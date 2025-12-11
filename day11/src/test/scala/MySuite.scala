// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day11 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "63170507621624566")
    assertEquals(score2, "438314708837664")

  test("Day11 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "8")
    assertEquals(score2, "2")
    
