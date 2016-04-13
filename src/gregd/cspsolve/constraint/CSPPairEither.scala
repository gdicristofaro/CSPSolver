package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPConstraint
import scala.collection.mutable.HashMap
import gregd.cspsolve.solve.CSPElement

object CSPPairEither {
	val logger = new MyLogger("CSPPairEither")	
}

class CSPPairEither(var1 : CSPVar, var2 : CSPVar) extends CSPConstraint {
	val vars = scala.collection.immutable.HashSet[CSPVar](var1, var2)
  
    def getSatisfaction(varsEls : HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) : Double = {
	  val var1Dom = varsEls.get(var1).getOrElse{
		  CSPPairEither.logger.error("mapping of variables does not contain CSPVar 1")
	      throw new IllegalArgumentException("map does not contain necessary variable")}
	      
	  val var2Dom = varsEls.get(var2).getOrElse{
		  CSPPairEither.logger.error("mapping of variables does not contain CSPVar 2")
	      throw new IllegalArgumentException("map does not contain necessary variable")}
	  
	  val toReturn =
	    if (var1Dom.size > 1 || var2Dom.size > 1 || var1Dom != var2Dom) 1.0
	    else 0.0
	  
	  CSPPairEither.logger.debug(
	      "var 1: " + var1 + " domain is " + var1Dom + 
	      "var 2: " + var2 + " domain is " + var2Dom +
	      " resulting score is " + toReturn + " (absolute: " + isAbsolute + ")")
	      
	  toReturn
    }

    def getEffectedVars() : scala.collection.immutable.HashSet[CSPVar] = vars
}