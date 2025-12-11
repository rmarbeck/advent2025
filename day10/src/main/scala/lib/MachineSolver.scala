package lib

import com.google.ortools.Loader
import com.google.ortools.linearsolver.{MPSolver, MPVariable}

object MachineSolver:

  // À appeler une fois au démarrage de ton appli (ou lazy dans un objet)
  private val _ = Loader.loadNativeLibraries()

  def solveMachine(
                    target: Array[Int],
                    buttons: Array[Array[Int]],
                    maxPressesUpperBound: Long = 1_000_000L
                  ): Option[Array[Long]] =
    require(buttons.nonEmpty)
    val dim = target.length
    require(buttons.forall(_.length == dim))

    val nButtons = buttons.length

    val solver = MPSolver.createSolver("CBC_MIXED_INTEGER_PROGRAMMING")
    if solver == null then throw new IllegalStateException("Could not create solver")

    // x_j = nombre de pressions sur le bouton j
    val xVars: Array[MPVariable] =
      Array.tabulate(nButtons) { j =>
        solver.makeIntVar(0.0, maxPressesUpperBound.toDouble, s"x_$j")
      }

    // Contraintes linéaires : sum_j buttons(j)(i) * x_j = target(i)
    for i <- 0 until dim do
      val ct = solver.makeConstraint(target(i), target(i), s"eq_$i")
      for j <- 0 until nButtons do
        val coeff = buttons(j)(i)
        if coeff != 0 then ct.setCoefficient(xVars(j), coeff)

    // Objectif : minimiser la somme des pressions
    val objective = solver.objective()
    for j <- 0 until nButtons do
      objective.setCoefficient(xVars(j), 1.0)
    objective.setMinimization()

    val status = solver.solve()

    if status == MPSolver.ResultStatus.OPTIMAL || status == MPSolver.ResultStatus.FEASIBLE then
      Some(xVars.map(v => Math.round(v.solutionValue())))
    else
      None
