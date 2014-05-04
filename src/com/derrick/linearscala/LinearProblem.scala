package com.derrick.linearscala

import com.derrick.linearscala.variable._
import scala.collection.mutable.ArrayBuffer

import scpsolver.constraints.LinearBiggerThanEqualsConstraint;
import scpsolver.constraints.LinearSmallerThanEqualsConstraint;
import scpsolver.constraints.LinearEqualsConstraint
import scpsolver.lpsolver.LinearProgramSolver;
import scpsolver.lpsolver.SolverFactory;
import scpsolver.problems.LinearProgram;

/**
 * Note this class breaks some idioms in Scala to allow flexibility
 * in defining the problem.
 */
class LinearProblem {

  private val isMaximis = true;
  private val _constraints = scala.collection.mutable.MutableList[Constraint]()
  private var _objective = new Expression()

  // To keep track of variables
  val _hashSetOfVariables = new UniqueList[String]();

  /**
   * Add the objective function
   * @param expression
   * @return
   */
  def +=(expression: Expression): Any = {
    _objective = expression
    trackVariables(expression.getVariableIndentifiers)
  }

  /**
   * Add the constraints
   * @param constraint
   * @return
   */
  def +=(constraint: Constraint): Any = {
    _constraints += constraint
    trackVariables(constraint.getVariableIndentifiers)
  }

  /**
   * Solve the problem
   * @return
   */
  def solve(): Any = {

    println("objective")
    println(this._objective);
    println
    println("constraints:")
    val lp: LinearProgram = new LinearProgram(this.getObjectiveAsArray);
    lp.setMinProblem(false);
    for (constraint <- this._constraints) {
      println(constraint)
      addConstraint(lp, constraint)
    }
    
    val solver: LinearProgramSolver = SolverFactory.newDefault();
    val sol = solver.solve(lp);
    
    println
    println("solution: ")

    var i = 0;
    for (v <- sol) {
      println(this.identifierAtIndex(i)+"="+v)
      i+=1
    }

  }

  private def noVariables: Int = this._hashSetOfVariables.size()
  private def trackVariables(variables: Iterable[String]) = { variables foreach { v: String => _hashSetOfVariables.add(v) } }
  private def indexOfVariable(identifier: String): Int = this._hashSetOfVariables.indexOf(identifier)
  private def identifierAtIndex(i: Int): String = this._hashSetOfVariables.get(i)

  private def expressionToArray(expression: Expression): Array[Double] = {
    val array = ArrayBuffer[Double]()

    for (v <- 1 to this.noVariables) {
      array += 0
    }

    for (v <- expression.symbolicVariables) {
      array(indexOfVariable(v._1)) = -v._2.constant
    }

    return array.toArray
  }

  private def getObjectiveAsArray = {
    this.expressionToArray(this._objective)
  }

  private def addConstraint(lp: LinearProgram, constraint: Constraint) = {
    val constraintType = constraint.constraint
    val constraintArray = this.expressionToArray(constraint.getStandardized)
    val constraintValue = constraint.getStandardized.numericValue

    constraintType match {
      case ConstraintType.LE => lp.addConstraint(new LinearSmallerThanEqualsConstraint(constraintArray, constraintValue, ""))
      case ConstraintType.E => lp.addConstraint(new LinearEqualsConstraint(constraintArray, constraintValue, ""))
      case ConstraintType.GE => lp.addConstraint(new LinearBiggerThanEqualsConstraint(constraintArray, constraintValue, ""))
    }
  }

}

class UniqueList[U] extends java.util.ArrayList[U] {
  override def add(element: U): Boolean = {
    if (this.contains(element)) {
      return false
    } else {
      super.add(element)
      return true
    }
  }
}
