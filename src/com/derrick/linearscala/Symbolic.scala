package com.derrick.linearscala.variable

import com.derrick.linearscala.variable.DoubleVariableImplicits._
//package com.derrick.linearscala {



  object ConstraintType extends ConstraintType;

  /**
   * @author Derrick
   *
   * Types of constraints in the LP
   *
   */
  class ConstraintType extends Enumeration {
    val LE = Value("<=")
    val E = Value("==")
    val GE = Value(">=")
  }

  /**
   * @author Derrick
   *
   * Base class for all things symbolic
   *
   */
  //  trait Symbolic {
  //    def isSymbol(): Boolean = true
  //  }

  /**
   * @author Derrick
   *
   * Constraints to be used with LP
   *
   */
  class Constraint(val left: Expression, val constraint: ConstraintType.Value, val right: Expression) { //} extends Symbolic {
    def getVariableIndentifiers = for (v <- (left.symbolicVariables ++ right.symbolicVariables)) yield v._1
    def getStandardized(): Expression = {
      this.right - this.left
    }
    override def toString() = this.left.toString + " " + this.constraint + " " + right.toString
  }

  /**
   * This expression encapsulates a symbolic expression
   *
   * @param variable
   */
  class Expression { //extends Symbolic {

    private val _symbolicVariables = scala.collection.mutable.Map[String, Variable]()
    private var _isDouble: Boolean = false;
    
    
    def symbolicVariables = _symbolicVariables

    private var _numericValue: Double = 0.0;
    def numericValue = _numericValue

    def this(x: Double) = { this(); _numericValue = x }
    
    def this(x: Double, isDouble: Boolean) {
      this()
      _numericValue = x 
      _isDouble = isDouble
    }

    def constraint(expression: Expression, constraint: ConstraintType.Value) = new Constraint(this, constraint, expression)

    def <=(expression: Any): Constraint = expression match {
      case expression: Double => this <= (new Expression(expression))
      case expression: Expression => constraint(expression, ConstraintType.LE)
      case expression: Variable => this <= (new Expression(expression))
    }

    // cannot use x:Any for the == operator as this is defined as final in the scala library
    def ==(expression: Double): Constraint = this == (new Expression(expression))
    def ==(expression: Expression): Constraint = constraint(expression, ConstraintType.E)
    def ==(expression: Variable): Constraint = this == (new Expression(expression))

    def >=(expression: Any): Constraint = expression match {
      case expression: Double => this >= (new Expression(expression))
      case expression: Expression => constraint(expression, ConstraintType.GE)
      case expression: Variable => this >= (new Expression(expression))
      case _ => println(expression); return null
    }
    
//    def *(expression: Any): Expression = expression match {
//      case expression: Double => {
//        this._numericValue = 3.0
//      }
//    }

    def getVariableIndentifiers = for (v <- this.symbolicVariables) yield v._1

    def this(variable: Variable) = {
      this()
      this._symbolicVariables(variable.symbolicReference) = variable
    }

    def operationMD(that: Double, f: ((Double, Double) => Double)) = {
      this._numericValue = f(this._numericValue, that)
      for (tuple <- this.symbolicVariables) {
        val key = tuple._1
        val variable = tuple._2
        this.symbolicVariables(key) = new Variable(key, f(variable.constant, that))
      }
    }

    def +(that: Double): Expression = {
      this._numericValue += that
      return this
    }

    def -(that: Double): Expression = {
      this._numericValue -= that
      return this
    }

    override def toString: String = {

      var string = ""
      if (Math.abs(this.numericValue) >= 0.000001) {
        string += this.numericValue + " "
      }

      this.symbolicVariables.foreach(tuple =>
        {
          string += " " + tuple._2.constant + "*" + tuple._1
        })
      return string
    }

    private def operationAS(that: Expression, function: ((Double, Double) => Double)): Expression = {

      // add the numeric values
      this._numericValue = function(this.numericValue, that.numericValue)

      // function performed on the numeric value
      that.symbolicVariables foreach ((tuple) => {
        var k1 = tuple._1
        var variableThat = tuple._2
        if (this.symbolicVariables.isDefinedAt(k1)) {
          this.symbolicVariables(k1) = new Variable(k1, function(this.symbolicVariables(k1).constant, variableThat.constant))
        } else {
          this.symbolicVariables(k1) = new Variable(k1, function(0.0, variableThat.constant))
        }
      })
      return this
    }

    def +(that: Expression): Expression = this.operationAS(that, (x: Double, y: Double) => x + y)
    def -(that: Expression): Expression = this.operationAS(that, (x: Double, y: Double) => x - y)
    def *(that: Double) = this.operationMD(that, (x: Double, y: Double) => x * y)
    def /(that: Double) = this.operationMD(that, (x: Double, y: Double) => x / y)

    def +(that: Variable): Expression = this + that.toExpression
    def -(that: Variable): Expression = this - that.toExpression

  }

  /**
   * @author Derrick
   *
   * Class to encapsulate a symbolic variable
   *
   */
  class Variable(val symbolicReference: String, private val _constant: Double = 0.0) { //} extends Symbolic {

    def this() = this("x", 0.0)

    def toExpression = new Expression(this)

    /** Operators which the class has **/

    def +(that: Any): Expression = that match {
      case that: Expression => that + this.toExpression
      case that: Variable => that.toExpression + this.toExpression
      case that: Double => this.toExpression + that
      case _ => throw new Error("Oh no")
    }

    def -(that: Any): Expression = that match {
      case that: Expression => this.toExpression - that
      case that: Variable => this.toExpression - that.toExpression
      case that: Double => this.toExpression - that
      case _ => throw new Error("Oh no")
    }

    def *(x: Double): Variable = {
      return new Variable(this.symbolicReference, this.constant * x)
    }

    def *:(x: Double): Variable = {
      return new Variable(this.symbolicReference, this.constant * x)
    }

    def /(x: Double): Variable = {
      return new Variable(this.symbolicReference, this.constant / x)
    }

    def <=(x: Any): Constraint = x match {
      case x: Double => this.toExpression <= new Expression(x)
      case x: Variable => this.toExpression <= new Expression(x)
      case x: Expression => this.toExpression <= x
    }

    def >=(x: Any): Constraint = x match {
      case x: Double => this.toExpression >= new Expression(x)
      case x: Variable => this.toExpression >= new Expression(x)
      case x: Expression => this.toExpression >= x
    }

    override def toString = this.constant + "*" + this.symbolicReference

    def constant = _constant

  }
  
  class Numeric (val value:Double) {
    
  }
  


object DoubleVariableImplicits {
    implicit def Double2Variable(value: Double) = new Variable()
  }

  
  
  
  
  
  
  
  
  
  
  
