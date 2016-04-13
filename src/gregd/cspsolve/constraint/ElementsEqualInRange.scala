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

object ElementsEqualInRange {
	val logger = new MyLogger("ElementsEqualInRange")	
}

class ElementsEqualInRange extends ModelConstraint {
  def getName = "Elements Equal In Range"
    
  def getDescription = "Identifies the number of elements in a particular variable range given ranges"
   

  val varArg = new ListConstraintArg(
      "Range of variables to check", 
      "In this constraint, these are the variables to check for the minimum number of elements", 
      1, None,
      	new ListConstraintArg("", "", 
      	1, None, new VarArg("","",true)))
  
  val elArgs = new ListConstraintArg(
      "Elements to count", 
      "Pairing of elements to how many of each should occur in given circumstance", 1, None, 
      new PairConstraintArg("", "", true,
          new ElementArg("","",true), 
          new IntArg("","", true)
      )
  )
    
  def getArgs = Array[ConstraintArg[_]](varArg, elArgs)
  
  def newInstance = new ElementsEqualInRange
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = {
	    val vars = ConstraintArg.extractList(varArg)
	    
	    ConstraintArg.extractList(elArgs).:\[List[CSPConstraint]](Nil){
	      (pair, lst) => 
	        val pr = ConstraintArg.extractPair(pair)
	        vars.:\[List[CSPConstraint]](Nil){
	          (vs, lst) =>  
	            val curVars = ConstraintArg.extractOptionList(vs).toArray
	            val pr = ConstraintArg.extractPair(pair)
	            new CSPElementEqual(curVars, pr._1, pr._2)::lst
	        }   
	    }.toArray
  }
}