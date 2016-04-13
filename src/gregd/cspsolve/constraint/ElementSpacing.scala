package gregd.cspsolve.constraint

import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.IntArg
import gregd.cspsolve.constraintarg.BoolArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPConstraint

object ElementSpacing {
  val logger = new MyLogger("ElementSpacing")
}


class ElementSpacing extends ModelConstraint {
  def getName = "Elements Spacing"
    
  def getDescription = "Identifies how much space should exist between occurrences of particular elements"
   

  val varArg = new ListConstraintArg(
      "Range of variables to check", 
      "In this constraint, these are the variables to check for spacing", 
      1, None, new VarArg("","",true))
  
  val elArgs = new ListConstraintArg(
      "Elements to check", 
      "Elements that should have this spacing", 1, None, 
      new ElementArg("","",true)
  )
  
  val countArg = new IntArg("Spacing", "How many variables to wait until the next possible occurence", true)
  
  val wrapArg = new BoolArg("Wrap Around", "Should the spacing wrap around to beginning when reaching the end", true)
  
  def getArgs = Array[ConstraintArg[_]](varArg, elArgs, countArg, wrapArg)
  
  def newInstance = new ElementSpacing
  
  def getSubList(beg : List[CSPVar], cur : List[CSPVar], num : Int, wraparound : Boolean) : Option[List[CSPVar]] = {
    (cur, num > 0, wraparound) match {
      case (_, false, _) => Some(Nil)
      case (v::tl, _, _) => 
        getSubList(beg, tl, num - 1, wraparound) match {
          case None => None
          case Some(lst) => Some(v::lst)
        }
      case (Nil, true, false) => None
      case (Nil, true, true) => getSubList(beg, beg, num, true)
    }
  }
  
  def getVarArgs(itemSpacing : Int, wraparound : Boolean, beginning : List[CSPVar], cur : List[CSPVar]) : List[List[CSPVar]] = {
    cur match {
      case hd::tl =>
        getSubList(beginning, cur, itemSpacing + 1, wraparound) match {
          case Some(lst) => (lst)::getVarArgs(itemSpacing, wraparound, beginning, tl)
          case None => getVarArgs(itemSpacing, wraparound, beginning, tl)
        }
      case Nil => Nil
    }
  }
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = {
	    val vars = ConstraintArg.extractList(varArg)
	    val els = ConstraintArg.extractList(elArgs)
	    
	    getVarArgs(ConstraintArg.extractElement(countArg), 
	        ConstraintArg.extractElement(wrapArg), vars, vars).:\[List[CSPConstraint]](Nil) {
	      (vars, lst) => 
	        val newCons = els.:\[List[CSPConstraint]](Nil)((el, elst) => new CSPElementMax(vars.toArray, el, 1)::elst)
	        lst ++  newCons
	    }.toArray
  }
}