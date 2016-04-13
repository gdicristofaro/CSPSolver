package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.IntArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPConstraint

object ElementMax {
	val logger = new MyLogger("ElementMax")	
}

class ElementMax extends ModelConstraint {
    
  def getName = "Element Max"
    
  def getDescription = "Identifies the maximimum number of a certain element in a particular variable range"
    
  val varArg = new ListConstraintArg(
      "Range of variables to check", 
      "In this constraint, these are the variables to check for the maximum number of elements", 
      1, None, new VarArg("","",true)) 
  
  val elArg = new ElementArg("Element","The element to check for",true)
  val countArg = new IntArg("Count", "The maximum number of element", true)
    
  def getArgs = Array[ConstraintArg[_]](varArg, elArg, countArg)
  
  def newInstance = new ElementMax
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = 
    Array[CSPConstraint](new CSPElementMax( 
        ConstraintArg.extractList(varArg).toArray,
        ConstraintArg.extractElement(elArg),
        ConstraintArg.extractElement(countArg)))
}