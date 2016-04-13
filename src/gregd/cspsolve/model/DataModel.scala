package gregd.cspsolve.model

import scala.collection.mutable.HashMap
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.solve.CSPConstraint
import scala.collection.mutable.HashSet
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.Searcher
import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.utils.Constants

/**
 * manages basic preferences
 */
trait PrefsManager {
  // the minimum acceptable score for solution as percentage
  private var minScore : Double = Constants.defaultMinScore
  
  // exhaustive search or not
  private var exhaustive : Boolean = Constants.defaultExhaustiveSearch
  
  private var tieredScore : Boolean = Constants.defaultUseTieredSearch
  
  
  def setTieredScore(ts : Boolean) = tieredScore = ts
  
  def getTieredScore = tieredScore
  
  /**
   * set minScore
   * @param score		the new minScore (must be between 0 - 1)
   */
  def setMinScore(score : Double) : Unit = 
    minScore = Math.min(1.0, Math.max(0.0, score))
    	
  /**
   * returns current preference for minimum score
   * @return 		the minimum score
   */
  def getMinScore : Double = minScore
  
  /**
   * set whether or not this will be an exhaustive search
   * @param state			exhaustive or not
   */
  def setExhaustive(state : Boolean) : Unit = exhaustive = state
  
  /**
   * returns whether or not the search will be exhaustive or not
   * @return		exhaustive or not
   */
  def getExhaustive : Boolean = exhaustive
}



  

/**
 * holds all constraints and manages them
 */
trait ConstraintManager {
  
  //list of constraints
  private var constraints: HashSet[ModelConstraint] = 
    new HashSet[ModelConstraint]

  
  /**
   * finds a constraint and returns it if exists
   * @param c			the constraint
   * @return			constraint holder if found
   */
  private def findConstraint(c : ModelConstraint) : Option[ModelConstraint] = {
   //go through constraints and look for our constraint
    if (constraints.contains(c)) Some(c)
    else None
  }
  
  
  /**
   * remove constraint and all pertinent arguments
   * @param c			the constraint to remove
   */
  def removeConstraint(c : ModelConstraint) = {
    constraints.remove(c)
  } 

  /**
   * add constraint and all pertinent arguments
   * @param c			the constraint to remove
   */    
  def addConstraint(c : ModelConstraint) = {
      constraints += c
  }
  
  /**
   * get a list of all constraints
   */
  def getConstraints = constraints.toList
  
  /**
   * get CSPConstraint list from these constraint
   * @return hashset of constraints
   */
  def getCSPConstraintMapping : scala.collection.mutable.HashMap[ModelConstraint, Array[CSPConstraint]] = {
    val theReturn = constraints.:\(new scala.collection.mutable.HashMap[ModelConstraint, Array[CSPConstraint]]){
      (c, hs) =>
        val CSPcons = c.getConstraintInstance
        hs += (c -> CSPcons)
    }
    theReturn
  }
  
  
  /**
   * get args for constraint if constraint exists
   * @param c			the constraint
   * @return			the return array of constraint args if exists
   */
  def getConstraintArgs(c : ModelConstraint) : Option[Array[ConstraintArg[_]]] = {
    findConstraint(c) match {
      case Some(c) => Some(c.getArgs)
      case None => None
    }
  }
  
  /**
   * clear all constraints
   */
  def clearConstraints = constraints = new HashSet[ModelConstraint]
 
  
  
    /**
     * remove variable from constraints (probably shouldn't be accessed directly)
     * @param v					the variable
     * @param removeConstraint	if we remove from a constraint, remove constraint as well
     */
    def removeVariableFromConstraints(v : ModelVar, removeConstraint : Boolean) = {      
        // go through all constraints
        constraints.:\(new HashSet[ModelConstraint]){
        	(c, hs) => 
        	  val removeIt = c.getArgs.:\(true){
        	    (a, bool) => a.removeVariable(v) && bool && removeConstraint 
        	  }
        	  hs += c
        }
    } 
  
  
  /**
   * remove Element from data model (probably shouldn't be accessed directly)
   * @param e					the element
   * @param removeConstraint	if we remove from a constraint, remove constraint as well
   */
  def removeElementFromConstraints(e : ModelElement, removeConstraint : Boolean) = {      
        // go through all constraints
        constraints.:\(new HashSet[ModelConstraint]){
        	(c, hs) => 
        	  val removeIt = c.getArgs.:\(true){
        	    (a, bool) => a.removeElement(e) && bool && removeConstraint 
        	  }
        	  hs += c
        }
  }  
}




abstract class Results

case class Error(val errorVar : ModelVar) extends Results

case class Solution(
    val solution : scala.collection.immutable.HashMap[ModelVar, ModelElement], 
    val constraintScores : scala.collection.mutable.HashMap[ModelConstraint, Double], 
    val maxScore : Double) extends Results
    
    


object DataModel {
  var SearcherObj : Option[Searcher] = None;
  
  val logger = new MyLogger("DataModel")
  
  
  /**
   * gets results from data model
   * @param tieredScore		indicates whether score for solution should be tiered
   *		 				i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95
   * @return model results
   */
  
  def getSearcherObj = SearcherObj

  
  def getResults(varsElMap : HashMap[ModelVar, HashSet[ModelElement]], 
      constraintMapping : HashMap[ModelConstraint, Array[CSPConstraint]],
      minScore : Double, exhaustive : Boolean, tieredScore : Boolean) : Results = { 
        val varDomains = varsElMap.:\[scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]](
            new scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) {
				(varEl, hm) => 
				val (v, els) = varEl
				hm += (v -> (new scala.collection.mutable.HashSet[CSPElement] ++= els))
			}
        
        DataModel.logger.debug("data table var domains before results include " + 
            varDomains.:\(""){
        		(entry, str) =>
        		  val (v, d) = entry
        		  str + "\nvar: " + v + "   domain: " + d
        	})
                    
        SearcherObj = Some(
            new Searcher(
        		varDomains,
        		constraintMapping.keySet.:\(new scala.collection.immutable.HashSet[CSPConstraint]) {
        		  (c, hs) => hs ++ constraintMapping.get(c).get
        		},
        		minScore, exhaustive, tieredScore));
        		
       
        		
		val toReturn = SearcherObj.get.getResults;
		SearcherObj = None;
		
		toReturn match {
          case e : gregd.cspsolve.solve.Error => new Error(e.errorVar.asInstanceOf[ModelVar])
          case s : gregd.cspsolve.solve.Solution => 
            new Solution(
            	s.solution.:\(new scala.collection.immutable.HashMap[ModelVar, ModelElement]) {
            		(entry, hm) =>
            		val (v, e) = entry
              
		            try {
		              hm + (v.asInstanceOf[ModelVar] -> e.asInstanceOf[ModelElement])  
		            }
		            catch {
		              case e : Throwable => hm
		            }
            	},
            	getModelConstraintScores(s.constraintScores, constraintMapping),
            	s.maxScore)
        }
  }
  
  
  
    /**
   * gets model constraint that spawned CSP constraint
   * @param c		the csp constraint
   * @return		the model constraint if exists
   */
  private def getModelConstraint(c: CSPConstraint, 
      constraintMapping : HashMap[ModelConstraint, Array[CSPConstraint]]) : 
      Option[ModelConstraint] = {
    constraintMapping.:\[Option[ModelConstraint]](None){
      (entry, prevSol) =>
        prevSol match {
          case Some(_) => prevSol
          case None => 
            val (modelC, cspC) = entry
            if (cspC.contains(c)) Some(modelC)
            else None
        }
    }
  }
  
  
  /**
   * gets scores of model constraints given scores for csp constraints
   * @param consMap			mapping of CSP constraints to their relative scores
   * @return				mapping of Model constraints to their scores
   */
  def getModelConstraintScores(consMap : scala.collection.mutable.HashMap[CSPConstraint, Double],
      constraintMapping : HashMap[ModelConstraint, Array[CSPConstraint]]) :
	  scala.collection.mutable.HashMap[ModelConstraint, Double]= {
    consMap.:\(new scala.collection.mutable.HashMap[ModelConstraint, Double]) {
        (entry, hm) =>
          val (cspCons, score) = entry
          getModelConstraint(cspCons, constraintMapping) match {
            case None => hm
            case Some(modelcons) =>
              val prevVal = hm.get(modelcons).getOrElse(0.0)
              hm.put(modelcons, prevVal + score);
              hm
          }
      }
  }
}

/**
 * model representing the CSP problem
 */
class DataModel extends ConstraintManager with PrefsManager {
    
  //mapping of variables to elements
  private var varsElMap = new HashMap[ModelVar, HashSet[ModelElement]]
  

  /**
   * compiles a hashset of all vars
   * @return 		hash set of model vars representing all variables
   */
  private def allVars : HashSet[ModelVar] = new HashSet[ModelVar] ++ varsElMap.keySet
  
  /**
   * compiles a hashset of all elements
   * @return		hash set of model elements representing all elements
   */
  private def allEls : HashSet[ModelElement] = varsElMap./:(new HashSet[ModelElement]) {
    (hs, keyEl) =>  hs++(keyEl._2)
  }

 
    /**
     * set or add a variable with domain
     * @param v			variable
     * @param domain	new domain for variable
     */
    def setVarDomain(v : ModelVar, domain : HashSet[ModelElement]) = varsElMap.put(v, domain)
    
    
    
    /**
     * get variable domain
     * @param v			variable
     * @return			if variable is contained, return domain
     */    
    def getVarDomain(v : ModelVar) : Option[HashSet[ModelElement]] = varsElMap.get(v)
    

    /**
     * remove variable from data model
     * @param v					the variable
     * @param removeConstraint	if we remove from a constraint, remove constraint as well
     */
    def removeVariable(v : ModelVar, removeConstraint : Boolean) = {      
    	removeVariableFromConstraints(v, removeConstraint)
    	
        // go through variables
        varsElMap-=v    
  } 
	

  /**
   * gets all variables
   */
  def getVariables = allVars

  /**
   * clear all variables
   * @param removeConstraint		remove constraint if contains variable
   */
  def clearVariables(removeConstraint : Boolean) = {
    //go through all elements
    allVars./:(){ (_, v) => removeVariable(v, removeConstraint) }
  }
  
  def getAllVarsDomain = varsElMap
  
  /**
   * gets Results given data model
   */
  def getResults : Results = {
    DataModel.getResults(varsElMap, getCSPConstraintMapping,
      getMinScore, getExhaustive, getTieredScore)
  }
  
  /**
   * remove Element from data model
   * @param e					the element
   * @param removeConstraint	if we remove from a constraint, remove constraint as well
   */
  def removeElement(e : ModelElement, removeConstraint : Boolean) = {      
        // go through all constraints
    	removeElementFromConstraints(e, removeConstraint)
        
        // go through variables
        varsElMap./:(){
          (_, vel) =>
          val (v, els) = vel
          varsElMap + (v -> (els - e))
        }    
  } 
	

  /**
   *   get all elements as a list
   *   @return 		hash set of all elements
   */  
  def getElements = allEls

  
  /**
   * clear all elements
   * @param removeConstraint		remove constraint if contains element
   */
  def clearElements(removeConstraint : Boolean) = {
    //go through all elements
    allEls./:(){ (_, el) => removeElement(el, removeConstraint) }
  }
}

class NameLabel {
  private var the_name : String = ""
  def name(name : String) : Unit = the_name = name
  def name : String = the_name
}

class ModelVar extends CSPVar 

class ModelElement extends CSPElement


trait ModelConstraint extends NameLabel {
  private var priority : Double = 1.0
  private var absolute : Boolean = true
  
  def getName : String
  def getDescription : String
  
  def getArgs : Array[ConstraintArg[_]]
  
  def getPriority : Double = priority
  
  def setPriority(pri : Double) = priority = pri

  def isAbsolute : Boolean = absolute
  
  def setAbsolute(abs : Boolean) = absolute = abs
  
  
  @throws(classOf[IllegalArgumentException])
  def getConstraintInstance : Array[CSPConstraint] = {
    if (!getArgs.:\(true){
      (arg, prev) =>
        prev && (arg.isSet || (!arg.isNecessary))
    })
    throw new IllegalArgumentException("Not all args have been set")
    
    
    val cspcons = getConstraint
    val size = cspcons.length
    
    cspcons.map{
      (cspcon) => 
        if (priority < 1.0 && priority >= 0.0) cspcon.setPriority(priority)
        if (!absolute) cspcon.setAbsolute(false)
        if (size > 1) cspcon.setWeight(1.0 / size)
    };
    cspcons
  }
  
  override def equals(other : Any) = {
    try {
    	(other.asInstanceOf[CSPElement] eq this.asInstanceOf[CSPElement])
    }
    catch {
      case e : Throwable => false
    }
  }
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint : Array[CSPConstraint]
  
  def newInstance : ModelConstraint
}