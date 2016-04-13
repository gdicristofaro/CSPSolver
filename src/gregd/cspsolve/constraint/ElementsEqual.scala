package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.PairConstraintArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.IntArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPConstraint

object ElementsEqual {
	val logger = new MyLogger("ElementsEqual")	
}

class ElementsEqual extends ModelConstraint {
  def getName = "Elements Equal"
    
  def getDescription = "Identifies the number of elements in a particular variable range"
   

  val varArg = new ListConstraintArg(
      "Range of variables to check", 
      "In this constraint, these are the variables to check for the minimum number of elements", 
      1, None, new VarArg("","",true))
  
  val elArgs = new ListConstraintArg(
      "Elements to count", 
      "Pairing of elements to how many of each should occur in given circumstance", 1, None, 
      new PairConstraintArg("", "", true,
          new ElementArg("","",true), 
          new IntArg("","", true)
      )
  )
    
  def getArgs = Array[ConstraintArg[_]](varArg, elArgs)
  
  def newInstance = new ElementsEqual
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = {
	    val vars = ConstraintArg.extractList(varArg).toArray
	    
	    ConstraintArg.extractList(elArgs).:\[List[CSPConstraint]](Nil){
	      (pair, lst) => 
	        val pr = ConstraintArg.extractPair(pair)
	        new CSPElementEqual(vars, pr._1, pr._2)::lst
	    }.toArray
  }
}