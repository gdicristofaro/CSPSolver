package gregd.cspsolve.constraint

import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPConstraint
import scala.collection.mutable.HashMap
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.IntArg
import gregd.cspsolve.utils.Utils
import gregd.cspsolve.constraintarg.PairConstraintArg
import gregd.cspsolve.constraintarg.BoolArg

object ElementEqual {
	val logger = new MyLogger("ElementEqual")	
}

class ElementEqual extends ModelConstraint {
    
  def getName = "Element Equal"
    
  def getDescription = "Identifies the number of a certain element in a particular variable range"
    
  val varArg = new ListConstraintArg(
      "Range of variables to check", 
      "In this constraint, these are the variables to check for the minimum number of elements", 
      1, None, new VarArg("","",true)) 
  
  val elArg = new ElementArg("Element","The element to check for",true)
  val countArg = new IntArg("Count", "The number of element", true)
    
  def getArgs = Array[ConstraintArg[_]](varArg, elArg, countArg)
  
  def newInstance = new ElementEqual
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = 
    Array[CSPConstraint](new CSPElementEqual( 
        ConstraintArg.extractList(varArg).toArray,
        ConstraintArg.extractElement(elArg),
        ConstraintArg.extractElement(countArg)
    ))
}