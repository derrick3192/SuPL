package com.derrick.linearscala.symblic

import com.derrick.linearscala.symoblic._


object teststuff {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(147); 
  println("Welcome to the Scala worksheet");$skip(35); 
  
  val a = List(("x",3),("y",2));System.out.println("""a  : List[(String, Int)] = """ + $show(a ));$skip(34); 
  val b = Map("x"->3.1, "y"->3.9);System.out.println("""b  : scala.collection.immutable.Map[String,Double] = """ + $show(b ));$skip(23); val res$0 = 
  b.get("x").get + 3.0;System.out.println("""res0: Double = """ + $show(res$0));$skip(43); 
  
  val x = SymbolicVar(Map("x" -> 22.0));System.out.println("""x  : com.derrick.linearscala.symoblic.SymbolicVar = """ + $show(x ));$skip(27); 
  val y = SymbolicVar(3.9);System.out.println("""y  : com.derrick.linearscala.symoblic.SymbolicVar = """ + $show(y ));$skip(22); 
  
  val z = x + 3.0;System.out.println("""z  : com.derrick.linearscala.symoblic.SymbolicVar = """ + $show(z ));$skip(19); 
  val z2 = 3.0 + x;System.out.println("""z2  : <error> = """ + $show(z2 ));$skip(19); 
  
  
  println(z)}
}
