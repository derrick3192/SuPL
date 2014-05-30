package com.derrick.linearscala.symoblic {

  import collection.immutable.HashMap
  import com.sun.xml.internal.bind.v2.runtime.IllegalAnnotationsException
  import com.derrick.linearscala.symoblic._

  object SymbolicVar {
    def apply(x: Double) = new SymbolicVar(x)
    def apply(v: Map[String, Double]) = new SymbolicVar(v)
  }

  class SymbolicVar(val _variables: Map[String, Double], val _numeric: Double) {

    def variables = _variables;
    def numeric = _numeric;

    def apply(v: Map[String, Double]) = new SymbolicVar(v)
    def apply(x: Double) = new SymbolicVar(x)

    def this(x: Double) = this(Map(), x)
    def this(v: Map[String, Double]) = this(v, 0.0)

    def +(that: SymbolicVar): SymbolicVar =
      {
        val variables = SymbolicVar.this.variables ++ that.variables.map { case (k, v) => k -> (v + SymbolicVar.this.variables.getOrElse(k, 0.0)) }
        val numberic = SymbolicVar.this._numeric + that._numeric
        return new SymbolicVar(variables, numberic)
      }

    def -(that: SymbolicVar): SymbolicVar =
      {
        val variables = SymbolicVar.this.variables ++ that.variables.map { case (k, v) => k -> (v - SymbolicVar.this.variables.getOrElse(k, 0.0)) }
        val numberic = SymbolicVar.this._numeric + that._numeric
        return new SymbolicVar(variables, numberic)
      }

    def *(that: SymbolicVar): SymbolicVar = {
      if (SymbolicVar.this.variables.size != 0) {
        return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric * that.numeric)
      } else if (that.variables.size != 0) {
        return new SymbolicVar(that.variables, SymbolicVar.this.numeric * that.numeric)
      } else {
        throw new IllegalArgumentException("One of the symbolics is not a numeric")
      }
    }

    def /(that: SymbolicVar): SymbolicVar = {
      if (SymbolicVar.this.variables.size != 0) {
        return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric / that.numeric)
      } else if (that.variables.size != 0) {
        return new SymbolicVar(that.variables, SymbolicVar.this.numeric / that.numeric)
      } else {
        throw new IllegalArgumentException("One of the symbolics is not a numeric")
      }
    }

    def *(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric * that)

    def +(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric + that)

    def -(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric - that)

    def /(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric / that)

    def variablesToString: String = ((for (m <- SymbolicVar.this.variables) yield " " + m._2 + "*" + m._1).toString())

    override def toString: String = return SymbolicVar.this.numeric + "+" + SymbolicVar.this.variablesToString

  }

  object SymbolicImplicits {
    implicit def Double2Symbolic(x: Double) = new SymbolicVar(x)
  }

  object Main {
    def main(args: Array[String]) {
    	val x = SymbolicVar(2.0)
    	val y = SymbolicVar(30.2)
    	
    	val z:SymbolicVar = x * 3.00 + 2.9
    	
    	println(z)
    }
  }

}