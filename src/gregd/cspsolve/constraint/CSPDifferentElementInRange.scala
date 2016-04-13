package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.solve.CSPVar
import scala.collection.mutable.HashMap

object CSPDifferentElementInRange {
  val logger = new MyLogger("CSPDifferentElementInRange") 
}


class CSPDifferentElementInRange(varRange : List[CSPVar], el : List[CSPElement], count : Int) extends CSPConstraint {
	val vars = new scala.collection.immutable.HashSet[CSPVar]() ++ varRange
	
	val checkEls = new scala.collection.immutable.HashSet[CSPElement]() ++ el
  
	private def overList(allVars : HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]], 
	    vars : List[CSPVar], checkEls :scala.collection.immutable.HashSet[CSPElement], els : scala.collection.immutable.HashSet[CSPElement], countLeft : Int) : Double = {
	  vars match {
	    case Nil => 1.0
	    case v :: tl =>
	      val curDomain = allVars.get(v).getOrElse{
		      CSPElementMax.logger.error("mapping of variables does not contain CSPVar " + v)
		      throw new IllegalArgumentException("map does not contain necessary variable")
		    }
	      
	      //CSPDifferentElementInRange.logger.debug("curDomain is " + curDomain + " size is " + curDomain.size + " contained )
	      
	      
	      if (curDomain.size == 1 && checkEls.contains(curDomain.head) && !els.contains(curDomain.head)) {	        
	        if (countLeft <= 0) 0.0
	        else overList(allVars, tl, checkEls, els ++ curDomain, countLeft - 1)
	      }
	      else overList(allVars, tl, checkEls, els, countLeft)
	  }
	}
	
    def getSatisfaction(varsEls : HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) : Double = {
		overList(varsEls, varRange, checkEls, new scala.collection.immutable.HashSet[CSPElement], count)
    }

	
    def getEffectedVars() : scala.collection.immutable.HashSet[CSPVar] = vars  
}