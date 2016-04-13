package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.IntArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPConstraint

object DifferentElementInRange {
  val logger = new MyLogger("DifferentElementInRange")
}

class DifferentElementInRange extends ModelConstraint {
    
  def getName = "Different Element Count"
    
  def getDescription = "Identifies the maximimum number of different elements in a particular variable range"
    
  val varArg = new ListConstraintArg(
      "Range of variables to check", 
      "These are the variables to check for the maximum number of different elements", 
      1, None, new VarArg("","",true)) 

    val elArg = new ListConstraintArg(
      "Elements to include", 
      "These are the elements to include", 
      0, None, new ElementArg("","",true)) 

  val countArg = new IntArg("Count", "The maximum number of different elements", true)
    
  def getArgs = Array[ConstraintArg[_]](varArg, elArg, countArg)
  
  def newInstance = new DifferentElementInRange
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = 
    Array[CSPConstraint](new CSPDifferentElementInRange( 
        ConstraintArg.extractList(varArg),
        ConstraintArg.extractList(elArg),
        ConstraintArg.extractElement(countArg)))
}