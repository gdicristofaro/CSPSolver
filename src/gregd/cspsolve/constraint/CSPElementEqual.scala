package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.solve.CSPElement
import scala.collection.mutable.HashMap

object CSPElementEqual {
  val logger = new MyLogger("CSPElementEqual") 
}

/**
 * specifies the number of elements of a certain type in varRange
 * @param varRange			the range of variables to check
 * @param el				the element to check for
 * @param count				the count to compare
 */
class CSPElementEqual(varRange : Array[CSPVar], el : CSPElement, count : Int) extends CSPConstraint {
	val vars = new scala.collection.immutable.HashSet[CSPVar]() ++ varRange
  
    def getSatisfaction(varsEls : HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) : Double = {
	  
	  val elCount = vars.:\((0, 0)){
	    (curVar, curCount) => 
	      val (ofOneCount, multCount) = curCount
	    val domain = varsEls.get(curVar).getOrElse{
	      CSPElementEqual.logger.error("mapping of variables does not contain CSPVar " + curVar)
	      throw new IllegalArgumentException("map does not contain necessary variable")
	    }
	    
	     (domain.contains(el), domain.size == 1) match {
	       case (true, true) => (ofOneCount + 1, multCount + 1)
	       case (true, false) => (ofOneCount, multCount + 1)
	       case _ => curCount
	     }
	  }
	  
	  val (oneElement, multElements) = elCount
	  
      if (oneElement > count || multElements < count) 0.0
      else 1.0
    }

	
    def getEffectedVars() : scala.collection.immutable.HashSet[CSPVar] = vars  
}