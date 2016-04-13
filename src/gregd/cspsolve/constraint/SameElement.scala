package gregd.cspsolve.constraint

import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.IntArg
import gregd.cspsolve.constraintarg.PairConstraintArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.model.ModelConstraint

/**
 * maximizes the potential to have the same element for pairs of variables
 */
class SameElement extends ModelConstraint {
  def getName = "Same Element"
  def getDescription = "Each pair in list must contain the same element if item in the list"
  
  val varArgs = new ListConstraintArg(
      "Range of variables to check", 
      "In this constraint, these are the variables to check for spacing", 
      1, None, new PairConstraintArg("", "", true, new VarArg("","",true), new VarArg("","",true)))
  
  val elArgs = new ListConstraintArg(
      "Elements to check", 
      "Elements that should have this spacing", 1, None, 
      new ElementArg("","",true)
  )
  
  def getArgs = Array[ConstraintArg[_]](elArgs, varArgs)
  
  def newInstance = new SameElement
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = {
    val varPairs = ConstraintArg.extractList(varArgs)
    val els = ConstraintArg.extractList(elArgs)
    
    varPairs.:\[List[CSPConstraint]](Nil){
      (vPair, lst) =>
        val vs = ConstraintArg.extractPair(vPair)
        new CSPPairBoth(vs._1, vs._2, els)::lst
    }.toArray[CSPConstraint]
  }
  
}
