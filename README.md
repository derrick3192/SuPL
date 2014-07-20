SuPL
====

Linear Programming/Optimisation for scala inspired by PuLP


SuPL is inspired by pulp https://code.google.com/p/pulp-or/

I always loved pulp and felt that scala with it's kool syntax would be great for implementing an LP interface so made
this one. It's pretty basic, but usually that's all you want anyway, here is a basic example...


    // LP Variables
    val x = SymbolicVar("x")
    val y = new SymbolicVar("y")
    
    // LP Problem
    val lp2 = new LinearProblem()

    // Constraints
    lp2 += x * 5.0 + y * 10.0			
    lp2 += x * 2.0 + y * 1.0 >= 8.3	- x
    lp2 += 4.0 <= y * 4.0 
    lp2 += x * 2.0 <= 2.0

    // Solve
    lp2.solve



I know there are more involed ones out there, but this is so easy event a monky could use it....

Thing I like about mine is your variables can be on either side of the >=, <= contraint which I think is trendy...

Pull requests welcome I'm so lonely ) :




