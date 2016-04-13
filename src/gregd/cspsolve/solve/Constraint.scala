package gregd.cspsolve.solve

import scala.collection.mutable.HashMap

trait CSPConstraint {
	/**
	 * determines how much the constraint has been satisfied given the current state of CSPVars
	 * @param vars		the CSP var state
	 * @return			number between 0 - 1 inclusive (1 is max satisfaction; 0 is no satisfaction)
	 */
	def getSatisfaction(varsEls : HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) : Double
	
	/**
	 * returns vars in which this constraint is affected
	 * @return 			a list with all effected variables
	 */
	def getEffectedVars() : scala.collection.immutable.HashSet[CSPVar]
	
	
	
	var absolute : Boolean = true;
	
	/**
	 * returns true if constraint cannot be violated
	 * @return 	true if absolute rule
	 */
	def isAbsolute : Boolean = absolute
	
	/**
	 * sets whether or not this rule can be violated
	 * @param abs		true if an absolute rule
	 */
	def setAbsolute(abs : Boolean) = {
	  absolute = abs
	}
	
	
	
	var priority : Double = 1;
	/**
	 * returns a number 0 -1 inclusive of priority; 1 representing max priority (unbreakable priority)
	 * default is 1
	 * @return the priority
	 */
	def getPriority : Double = priority
	
	/**
	 * sets the priority of this constraint
	 * @param pri		the new priority (0 - 1 inclusive)
	 */
	@throws(classOf[IllegalArgumentException])
	def setPriority(pri : Double) = {
	  if (pri > 1 | pri < 0) throw new IllegalArgumentException("priority is outside acceptable bounds: must be between 0 and 1 inclusive.")
	  priority = pri
	}
	
	
	
	var weight : Double = 1;
	/**
	 * returns a number 0 -1 inclusive of weight; 1 representing max weight
	 * default is 1
	 * @return the weight
	 */
	def getWeight : Double = weight
	
	/**
	 * sets the priority of this constraint
	 * @param pri		the new priority (0 - 1 inclusive)
	 */
	@throws(classOf[IllegalArgumentException])
	def setWeight(w : Double) = {
	  if (w > 1 | w < 0) throw new IllegalArgumentException("priority is outside acceptable bounds: must be between 0 and 1 inclusive.")
	  weight = w
	}
	
	/**
	 * override equals to get equality of object
	 */
	override def equals(other : Any) = {
    try {
    	(other.asInstanceOf[CSPConstraint] eq this.asInstanceOf[CSPConstraint])
    }
    catch {
      case e : Throwable => false
    }
  }
}