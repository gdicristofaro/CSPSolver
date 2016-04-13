package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPConstraint
import scala.collection.mutable.HashMap

object CSPElementMax {
  val logger = new MyLogger("CSPElementMax") 
}

/**
 * specifies the max number of elements of a certain type in varRange
 * @param varRange			the range of variables to check
 * @param el				the element to check for
 * @param count				the count to compare
 */
class CSPElementMax(varRange : Array[CSPVar], el : CSPElement, count : Int) extends CSPConstraint {
	val vars = new scala.collection.immutable.HashSet[CSPVar]() ++ varRange
  
    def getSatisfaction(varsEls : HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) : Double = {
	  
	  val elCount = vars.:\(0){
	    (curVar, curCount) => 
	    val domain = varsEls.get(curVar).getOrElse{
	      CSPElementMax.logger.error("mapping of variables does not contain CSPVar " + curVar)
	      throw new IllegalArgumentException("map does not contain necessary variable")
	    }
	    
	     if (domain.size == 1 && domain.contains(el)) curCount + 1
	     else curCount
	  }
      if (elCount > count) 0.0
      else 1.0
    }

	
    def getEffectedVars() : scala.collection.immutable.HashSet[CSPVar] = vars  
}