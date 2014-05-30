
import com.derrick.linearscala.variable.Variable
import scpsolver.constraints.LinearBiggerThanEqualsConstraint;
import scpsolver.constraints.LinearSmallerThanEqualsConstraint;
import scpsolver.constraints.LinearConstraint;
import scpsolver.constraints._;
import scpsolver.lpsolver.LinearProgramSolver;
import scpsolver.lpsolver.SolverFactory;
import scpsolver.problems.LinearProgram;
import scpsolver.constraints.LinearEqualsConstraint;
import com.derrick.linearscala._

import com.derrick.linearscala.variable.DoubleVariableImplicits._

object Main {

  def main(args: Array[String]) {

    /*
	 * 	Min 5 x + 10 y 
		
		Subject to
		
		3 x + 1 y >= 8.3
		4y >= 4
		2x <= 2
	 */

    val x = new Variable("x", 1.0)
    val y = new Variable("y", 1.0)
    val lp2 = new LinearProblem()
    
    lp2 += x * 5.0 + y * 10.0
    lp2 += x * 3.0 + y * 1.0 >= 8.3
    lp2 += y * 4.0 >= 4.0			// this is probably a better idea
    lp2 += x * 2.0 <= 2.0

    lp2.solve
    
    
    
    
    
     println(30.2+x);
    
    
    
    
    
    
    
    
    /** OR before the old annoying way in java which is terrible**/

    val lp: LinearProgram = new LinearProgram(Array(5.0, 10.0));
    lp.addConstraint(new LinearBiggerThanEqualsConstraint(Array(3.0, 1.0), 8.30, "c1"));
    lp.addConstraint(new LinearBiggerThanEqualsConstraint(Array(0.0, 4.0), 4.0, "c2"));
    lp.addConstraint(new LinearSmallerThanEqualsConstraint(Array(2.0, 0.0), 2.0, "c3"));
    lp.setMinProblem(true);
    val solver = SolverFactory.newDefault();
    val sol = solver.solve(lp);

    // printing out the values
    for (v <- sol) {
      System.out.println(v);
    }

  }
}