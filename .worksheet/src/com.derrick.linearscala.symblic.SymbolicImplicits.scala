package com.derrick.linearscala.symblic

import com.derrick.linearscala.symoblic.SymbolicImplicits
import com.derrick.linearscala.symoblic.Symbolic

  object SymbolicImplicits {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(239); 
    implicit def Double2Symbolic(x: Double) = new Symbolic(x);System.out.println("""Double2Symbolic: (x: Double)com.derrick.linearscala.symoblic.Symbolic""")}
  }
object teststuff {
  println("Welcome to the Scala worksheet")
  
  val a = List(("x",3),("y",2))
  val b = Map("x"->3.1, "y"->3.9)
  b.get("x").get + 3.0
  
  val x = Symbolic(Map("x" -> 22.0))
  val y = Symbolic(3.9)
  
  val z = x + 3.0
  val z2 = 3.0 * x
  
  
  println(z)
}
