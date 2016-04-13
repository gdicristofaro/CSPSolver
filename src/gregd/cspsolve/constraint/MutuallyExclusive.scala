package gregd.cspsolve.constraint

import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.constraintarg.ConstraintArg
import scala.collection.mutable.HashMap
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.log.MyLogger
import scala.collection.mutable.HashSet
import gregd.cspsolve.utils.Utils
import gregd.cspsolve.constraintarg.ElementArg

object MutuallyExclusive {
	val logger = new MyLogger("MutuallyExclusive")	
}

class MutuallyExclusive extends ModelConstraint {
    
  def getName = "Mutually Exclusive"
    
  def getDescription = "Identified cells cannot contain the same value"
    
  val arg = new ListConstraintArg(
      "Mutually Exclusive Variables", 
      "In this constraint, these variables will not have the same element", 
      2, None, new VarArg("","",true)) 
    
  def getArgs = Array[ConstraintArg[_]](arg)
  
  def newInstance = new MutuallyExclusive
  
  //for each var, creates pairing with that var and each var that occurs after it in the list ( n * (n-1) pairings)
  private def getPairings(vars : List[CSPVar]) : List[CSPConstraint] = {
    vars match {
      case Nil => Nil
      case v::Nil => Nil
      case v :: tl => 
        tl.:\[List[CSPConstraint]](Nil){
          (curVar, list) => 
        (new CSPPairEither(v, curVar))::list} ++ 
        getPairings(tl)
    }
  }
  
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = {
    
    val optionList = arg.getValue.getOrElse{
    	MutuallyExclusive.logger.error("no value could be extracted from the arguments")
    	throw new IllegalArgumentException("No arg was gathered")
      }
    
    val lst = Utils.extractList(optionList)
        
    MutuallyExclusive.logger.debug("there are  " + lst.length + " vars for constraints")
    
    val cons = getPairings((new HashSet[CSPVar] ++ lst).toList)
    MutuallyExclusive.logger.debug("created " + cons.length + " PairEither constraints")
    
    cons.toArray[CSPConstraint]
  }
}