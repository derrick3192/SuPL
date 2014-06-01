package com.derrick.linearscala.symoblic {

  import collection.immutable.HashMap
  import com.sun.xml.internal.bind.v2.runtime.IllegalAnnotationsException
  import com.derrick.linearscala.symoblic.SymbolicVarImplicits._

  object ConstraintType extends ConstraintType;

  class ConstraintType extends Enumeration {
    val LE = Value("<=")
    val E = Value("==")
    val GE = Value(">=")
  }

  object SymbolicVar {
    def apply(x: Double) = new SymbolicVar(x)
    def apply(v: Map[String, Double]) = new SymbolicVar(v)
    def apply(v: String) = new SymbolicVar(v)
  }

  case class SymbolicVar(val _variables: Map[String, Double], val _numeric: Double, val _isNumeric: Boolean = false) {

    def variables: Map[String, Double] = _variables;
    def numeric: Double = _numeric;
    def isNumeric: Boolean = _isNumeric;

    def apply(v: Map[String, Double]) = new SymbolicVar(v)
    def apply(x: Double) = new SymbolicVar(x)

    def this(x: Double) = this(Map(), x, true)
    def this(v: Map[String, Double]) = this(v, 0.0)
    def this(v: String) = this(Map(v -> 1.0), 0.0, false)

    // Operator overloading with a SymbolicVar

    def applyFunction(that: SymbolicVar, f: (Double, Double) => Double): SymbolicVar = {
      val variables = SymbolicVar.this.variables ++ that.variables.map { case (k, v) => k -> f(SymbolicVar.this.variables.getOrElse(k, 0.0), v) }
      val numeric = f(this.numeric, that.numeric)
      return new SymbolicVar(variables, numeric)
    }

    def +(that: SymbolicVar): SymbolicVar = applyFunction(that, (x: Double, y: Double) => x + y)
    def -(that: SymbolicVar): SymbolicVar = applyFunction(that, (x: Double, y: Double) => x - y)

    def applyMD(that: SymbolicVar, this1: (Double) => SymbolicVar, that1: (Double) => SymbolicVar): SymbolicVar = {
      if (that.isNumeric || that.variables.size == 0) {
        return this1(that.numeric)
      } else if (this.isNumeric || this.variables.size == 0) {
        return that1(this.numeric)
      } else {
        throw new IllegalArgumentException("One of the symbolics is not a numeric")
      }
    }

    def *(that: SymbolicVar): SymbolicVar = { applyMD(that, this.*, that.*) }

    def /(that: SymbolicVar): SymbolicVar = { applyMD(that, this./, that./) }

    // Operator overloading with a Double

    def *(that: Double): SymbolicVar = return new SymbolicVar(transformVariable((x: Double) => x * that), SymbolicVar.this.numeric * that)

    def /(that: Double): SymbolicVar = return new SymbolicVar(transformVariable((x: Double) => x / that), SymbolicVar.this.numeric / that)

    def +(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric + that)

    def -(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric - that)

    def variablesToString: String = ((for (m <- SymbolicVar.this.variables) yield " " + m._2 + "*" + m._1).toString())

    override def toString: String = return SymbolicVar.this.numeric + "+" + SymbolicVar.this.variablesToString

    private def transformVariable(func: (Double) => Double): Map[String, Double] = _variables.map(k => (k._1, func(k._2)))

    // Constraint constructs

    //def <=(Double:that) = 

  }

  object SymbolicVarImplicits {
    implicit def Double2Symbolic(x: Double): SymbolicVar = new SymbolicVar(x)
  }

  object Main {

    def main(args: Array[String]) {
      //      val x = SymbolicVar(2.0)
      //      val y = SymbolicVar(30.2)
      //
      //      val z: SymbolicVar = x * 3.00 + 2.9
      //      println(z)
      //
      //      val zz = new SymbolicVar(Map("x" -> 3.0))
      //      val zy = new SymbolicVar(Map("x" -> 32.00, "y" -> 23.0))
      //
      //      println(zz + zy * 3.0)

      val xy = SymbolicVar(Map("x" -> 3.0d, "y" -> 2.0d))
      val yx = SymbolicVar(Map("x" -> -3.0d, "y" -> -2.0d))

      println(xy + yx)

      val x = SymbolicVar("x")
      val x2 = SymbolicVar("x") * 2.0
      println(2.0 * x - 2.0 - 2 * x + 2.0)

      println(10 * x)
      println(10 * x / 10 * 20 - 10 - 20 * x + 10) //incorrect
      println(-10 - 20 * x + 10)
    }
  }

}