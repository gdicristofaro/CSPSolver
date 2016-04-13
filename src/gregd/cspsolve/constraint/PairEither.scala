package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPConstraint

object PairEither {
	val logger = new MyLogger("PairEither")	
}

class PairEither extends ModelConstraint {
    
  def getName = "Mutually Exclusive"
    
  def getDescription = "Identified cells cannot contain the same value"
    
    
  val arg1 = new VarArg("Variable 1","First Variable Argument",true)
  val arg2 = new VarArg("Variable 2", "Second Variable Argument", true)
    
  def getArgs = Array[ConstraintArg[_]](arg1,arg2)
  
  def newInstance = new PairEither
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = 
    Array[CSPConstraint](new CSPPairEither(
      arg1.getValue.getOrElse{
        PairEither.logger.error("no value could be extracted from first argument")
        throw new IllegalArgumentException("could not extract value")
      },
      arg2.getValue.getOrElse{
        PairEither.logger.error("no value could be extracted from second argument")
        throw new IllegalArgumentException("could not extract value")
      }))
}