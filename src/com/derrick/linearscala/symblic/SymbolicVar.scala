package com.derrick.linearscala.symoblic {

  import collection.immutable.HashMap
  import com.sun.xml.internal.bind.v2.runtime.IllegalAnnotationsException
  import com.derrick.linearscala.symoblic.SymbolicVarImplicits._
  
  

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
    def this(v: String) = this(Map(v->1.0), 0.0, false)

    
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
      if (that.isNumeric || that.variables.size == 0) {
        return this * that.numeric
      } else if (this.isNumeric || this.variables.size == 0) {
        return that * this.numeric
      } else {
        throw new IllegalArgumentException("One of the symbolics is not a numeric")
      }
    }

    def /(that: SymbolicVar): SymbolicVar = {
      if (that.isNumeric || that.variables.size == 0) {
        return this / that.numeric
      } else if (this.isNumeric || this.variables.size == 0) {
        return that / this.numeric
      } else {
        throw new IllegalArgumentException("One of the symbolics is not a numeric")
      }
    }

    def *(that: Double): SymbolicVar = return new SymbolicVar(transformVariable((x:Double) => x * that), SymbolicVar.this.numeric * that)
    
    def /(that: Double): SymbolicVar = return new SymbolicVar(transformVariable((x:Double) => x / that), SymbolicVar.this.numeric / that)

    def +(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric + that)

    def -(that: Double): SymbolicVar = return new SymbolicVar(SymbolicVar.this.variables, SymbolicVar.this.numeric - that)



    def variablesToString: String = ((for (m <- SymbolicVar.this.variables) yield " " + m._2 + "*" + m._1).toString())

    override def toString: String = return SymbolicVar.this.numeric + "+" + SymbolicVar.this.variablesToString

    private def transformVariable(func: (Double) => Double): Map[String,Double] = {
     return _variables.map( k=>(k._1,func(k._2)))
    }
    

  }

  object SymbolicVarImplicits {
    implicit def Double2Symbolic(x: Double):SymbolicVar = new SymbolicVar(x)
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
      
      val x = SymbolicVar("x")
      val x2 = SymbolicVar("x") * 2.0
      println(2.0*x-2.0-2*x+2.0)
    }
  }

  class Main {
    def main(args: Array[String]) {
      Main.main(args)
    }
  }

}