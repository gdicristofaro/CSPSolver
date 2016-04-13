package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.solve.CSPVar
import scala.collection.mutable.HashMap
import gregd.cspsolve.solve.CSPElement


object CSPPairBoth {
	val logger = new MyLogger("CSPPairBoth")	
}

class CSPPairBoth(var1 : CSPVar, var2 : CSPVar, els : List[CSPElement]) extends CSPConstraint {
	val vars = scala.collection.immutable.HashSet[CSPVar](var1, var2)
	
	val consideredEls = scala.collection.immutable.HashSet[CSPElement]() ++ els
  
    def getSatisfaction(varsEls : HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) : Double = {
	  val var1Dom = varsEls.get(var1).getOrElse{
		  CSPPairBoth.logger.error("mapping of variables does not contain CSPVar 1 " + var1)
	      throw new IllegalArgumentException("map does not contain necessary variable")}
	      
	  val var2Dom = varsEls.get(var2).getOrElse{
		  CSPPairBoth.logger.error("mapping of variables does not contain CSPVar 2 " + var2)
	      throw new IllegalArgumentException("map does not contain necessary variable")}
	  
	  //println("getting here...")
	  
	  //if the only elements in a domain are the elements to be considered, then do a check
	  //otherwise it could be fine
	  if ((var1Dom.intersect(consideredEls).size == var1Dom.size)
	      || (var2Dom.intersect(consideredEls).size == var2Dom.size)) {
	    
	      if (var1Dom.intersect(var2Dom).size > 0) 1.0
		  else {
		    //println("dom of " + var1 + " is + " + var1Dom + " and for " + var2 + " is " + var2Dom + " with included dom of " + consideredEls)
		    0.0
		  }
	  }
	  else 1.0 
    }

    def getEffectedVars() : scala.collection.immutable.HashSet[CSPVar] = vars
}






/*
object PairBoth {
	val logger = new MyLogger("PairBoth")	
}


class PairBoth extends ModelConstraint {
    
  def getName = "Pair Both"
    
  def getDescription = "Identified variables must share the same value"
    
    
  val arg1 = new VarArg("Variable 1","First Variable Argument",true)
  val arg2 = new VarArg("Variable 2", "Second Variable Argument", true)
    
  def getArgs = Array[ConstraintArg[_]](arg1,arg2)
  
  def newInstance = new PairBoth
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = 
    Array[CSPConstraint](new CSPPairBoth(
      arg1.getValue.getOrElse{
        PairBoth.logger.error("no value could be extracted from first argument")
        throw new IllegalArgumentException("could not extract value")
      },
      arg2.getValue.getOrElse{
        PairBoth.logger.error("no value could be extracted from second argument")
        throw new IllegalArgumentException("could not extract value")
      }))
}
*/