package gregd.cspsolve.solve

import scala.collection.mutable.HashSet
import gregd.cspsolve.log.MyLogger
import scala.collection.mutable.DoubleLinkedList

abstract class Results
case class Error(val errorVar : CSPVar) extends Results
case class Solution(val solution : scala.collection.immutable.HashMap[CSPVar, CSPElement], val constraintScores : scala.collection.mutable.HashMap[CSPConstraint, Double], val maxScore : Double) extends Results



class Status(val depth : Int, val depthLimit : Int, val branchPercentage : Double, val state: Option[CSPVars])


object Searcher {
  
	val logger = new MyLogger("Searcher")
  
	/**
	 * extracts a Results object from CSPVars
	 */
	def extractResults(varsState : CSPVars) : Results = {
	  logger.info("extracting results...")
	  val hm = varsState.getAllDomains./:(new scala.collection.immutable.HashMap[CSPVar, CSPElement]()) {
	    (hm, varDom) => 
	      val (v, dom) = varDom
	      if (dom.size != 1) {
	        logger.error("domain of all variables from results should be 1")
	        throw new IllegalStateException("Domain should be of size 1 for each object when extracting results")
	      }
	      else
	        hm + (v -> dom.head)
	  }
	  new Solution(hm, varsState.getConstraintScores, varsState.getMaxScore)
	}
}


/**
 * the searcher class is in charge of determining answers
 * 
 * @param vars			the original variables state
 * @param minScore		the minimum score for solution as percentage
 * 						The score is weighted based on constraint weights. 
 *       				Constraints with a weight of 1 must be satisfied completely
 * @param exhaustive	do exhaustive search looking for best result
 * @param tieredScore	indicates whether score for solution should be tiered
 * 						i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95
 */
class Searcher(vars : CSPVars, minScore : Double, exhaustive : Boolean, tieredScore : Boolean) {
  
  /**
   * alternate constructor for Searcher
   * @param varsToDomains			mapping of variables to their domain
   * @param constraints				constraints
   * @param minScore				minimum acceptable score
   * @param exhaustive				do exhaustive search looking for best result
   * @param tieredScore				indicates whether score for solution should be tiered
   *		 						i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95
   */
  def this(varsToDomains : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]],
			constraints : scala.collection.immutable.HashSet[CSPConstraint],
			minScore : Double,
			exhaustive : Boolean,
			tieredScore : Boolean) = {
    		  this(new CSPVars(varsToDomains, constraints, tieredScore), minScore, exhaustive, tieredScore)
  }
	
  
			  
  //do a domain check
  vars.getAllDomains.map {
    (entry) =>
      val (v, d) = entry
      if (d.size < 1) throw new IllegalArgumentException("size of each domain must be larger than or equal to 1: var " + v + " has size of " + d.size)
  }

  Searcher.logger.info("starting with CSP variables state of " + vars)
  /**
   * extract values from searcher
   * @return		results from search
   */
  def getResults : Results = 
    //multiply maxScore by minScore to derive percentage * maximum possible
    search(vars.getAdjustedMinScore(minScore), vars.getMaxScore, exhaustive, vars)
  

    
  private abstract class StatusItem
  private case class StatusNode(var branchesCovered : Int, totalBranches : Int, 
      varsSet : Int, varsState : Option[CSPVars], var next : StatusItem) extends StatusItem
  private case class StatusEnd() extends StatusItem
  
  //number of variables present
  private val varsNum = vars.getVars.size
  
  //
  private var firstStatusNode : StatusNode = new StatusNode(0, 1, 0, None, new StatusEnd)
  private var lastItem : StatusNode  = firstStatusNode
  
  
  //TODO fix
  /**
   * returns an object representing the current status
   * @return			status object representing current status
   */
  def getStatus : Status = {
    //depth, branches covered
    def calcStatus(node : StatusNode, percentagePiece : Double, percentageTotal : Double) : (Int, Double, Option[CSPVars]) = {
      val nextPerAvail = percentagePiece / node.totalBranches
      node.next match {
        case n : StatusNode => calcStatus(n, nextPerAvail, percentageTotal + (nextPerAvail * node.branchesCovered))
        case e : StatusEnd => (node.varsSet, percentageTotal + (nextPerAvail * node.branchesCovered), node.varsState)
      }
    }

    val (varsTot, percTot, varState) = calcStatus(firstStatusNode, 1.0, 0.0)
    new Status(varsTot, varsNum, percTot, varState)
  }

  /**
   * adds a new status object to the queue
   * @param branches		number of branches to cover
   * @param varsCovered		the number of variables that are set
   * @param varsState		the current state of vars
   * @return 				the created status node
   */
  private def newStatus(branches : Int, varsCovered : Int, varsState : CSPVars) : StatusNode = {
    val newNode : StatusNode = new StatusNode(0, branches, varsCovered, Some(varsState), new StatusEnd)
	lastItem.next = newNode;
	lastItem = newNode;
	newNode
  }

  /**
   * updates the status of the given status node adding one
   * @param status					the status node to update
   * @return 						the updated status node
   */
  private def updateStatus(status : StatusNode) : 
	  StatusNode = {
    status.next = new StatusEnd;
    lastItem = status;
    status.branchesCovered = status.branchesCovered + 1;
    status
  }
  
  
  
  /**
   * go through CSPVars and returns new CSPVars when v's domain is set to e (and e only)
   * 
   * @param minScore		the minimum acceptable score
   * @param v				the variable in questions
   * @param vars			the CSPVars state
   * @return 				Some CSPVars if CSPVars is viable or None if not viable
   */
  private def arcReduce(minScore : Double, v : CSPVar, vars : CSPVars) : Option[(CSPVars, HashSet[CSPVar])] = {

    /**
     * checks the consistency of each variable with a particular constraint
     * @param minScore			the minimum total score for constraints acceptable
     * @param vars				the CSPVars state
     * @param c					the current constraint
     * @param vs				the list of variables to check for this constraint
     * @param effectedVars		the variables that have been effected in this constraint
     * @param curBestScore		the current best score for constraint					
     * @return					None if not satisfiable in current state, Some(effected variables, CSPVars)
     */
    def constraintConsistency(minScore : Double, vars : CSPVars, c : CSPConstraint, 
    		vs : List[CSPVar], effectedVars : scala.collection.immutable.HashSet[CSPVar], curBestCScore : Double) : 
    	Option[(scala.collection.immutable.HashSet[CSPVar], CSPVars)] = {
      //matching on list of CSPVars to check against constraint
      vs match {
        //if we are out, then return what we have
        case Nil => 
          //update score in CSPVars
          val newscore = curBestCScore
          vars.setConstraintScore(c, newscore)
          
          //return it
          Searcher.logger.debug("Updating constraint score: " + c + " with best score of " + newscore + 
              " effected vars include " + effectedVars.:\("")((v, s) => s + v + ", "))
          
          Some(effectedVars, vars)
        //otherwise, get testing...
        case v :: tl => 
          	//check each element in domain to see if it will work, if not, don't add to new domain and say we altered
        	val (altered, bestScore, newDomain) = vars.getVarDomain(v)./:[(Boolean, Double, HashSet[CSPElement])](
        	    (false, 0.0, new HashSet[CSPElement]())){
        		(prev, curEl) =>
        		  //if altered..., previous best score, the previously established domain
        		  val (alt, prevBest, prevDom) = prev
        		  
        		  vars.testConstraint(c, v, curEl, minScore) match {
        		    case None => (true, prevBest, prevDom)
        		    case Some(score) => (alt, math.max(score, prevBest), prevDom+=curEl)
        		  }
      		}
        	
    	    //set the new domain if new domain exists, otherwise, we have a problem and return as such
        	if (newDomain.isEmpty) {
        		Searcher.logger.debug("domain for variable " + v + " is empty")
        		None
        	}
        	else {
        		val newEffected =
        		  if (altered) {
            		Searcher.logger.debug("variable " + v + " has alterations")
        		    effectedVars+v
        		  }
        		  else {
        		    Searcher.logger.debug("variable " + v + " has no alterations")
        		    effectedVars
        		  }
        		  
        		//reset var domain
        		vars.setVarDomain(v, newDomain)
        	  	constraintConsistency(minScore, vars, c, tl, newEffected, math.max(curBestCScore, bestScore))
          }
      }
    } 
    
    
    
    //TODO mutable hashmap???
    /**
     * the process of going through all constraints to check what variable elements will still work
     * 
     * @param minScore			the minimum acceptable score for the vars state
     * @param vars				the CSP Vars state
     * @param cItems			the hashset of constraints left to check
     * @param effectedVars		the variables currently effected by this current run
     * @return					the variables state and the variables effected
     */
    def arcHelper(minScore : Double, vars : CSPVars, cItems : scala.collection.immutable.HashSet[CSPConstraint], 
        effectedVars : HashSet[CSPVar]) : Option[(CSPVars, HashSet[CSPVar])] = {

      Searcher.logger.debug("current status of vars: " +vars)
      
      cItems.headOption match {
        //if all constraints are checked, then return
        case None => 
          Searcher.logger.debug("No more checks for this run of arc reduce")
          Some((vars, effectedVars))

        //otherwise, go through constraints
        case Some(c) =>
          Searcher.logger.debug("checking constraint " + c)
          
          constraintConsistency(minScore, vars, c, c.getEffectedVars.toList, 
              new scala.collection.immutable.HashSet[CSPVar], 0.0) match {
            //if None is returned, we have a problem, so return None here as well
            case None => None
            
            //we had some return, so deal with results
            case Some((neweffected, newVars)) => 
              
              // add all constraints of effected vars to CItems
              val newcItems = neweffected.:\(cItems)((thisVar, hs) => hs ++ vars.getVarConstraints(thisVar) ) - c
              
              //recurse
              arcHelper(minScore, newVars, 
                newcItems,
                // update effected vars
                effectedVars++=neweffected)
          }
      }
    }
    
    val constraints = new scala.collection.immutable.HashSet[CSPConstraint]() ++ vars.getVarConstraints(v)
    Searcher.logger.debug("checking constraints with current size " + constraints.size)
    
    //run arcHelper and start it up
    arcHelper(minScore, vars, 
        constraints, 
        new HashSet[CSPVar]()+v)
  }

  
  /**
   * gets the best Variable option to search next (based on smallest domain)
   * @param	vars	the CSPVars state
   * @return 		a potential variable or None if finished and the number of completed variables
   */
  private def getNext(vars : CSPVars) : (Option[CSPVar], Int) = {
    //this fold contains current best choice for next var and number of vars that have a domain of 1
    vars.getVars./:[(Option[(CSPVar, Int)], Int)]((None, 0)){
        (prevResult, curVar) =>
          (prevResult, vars.getVarDomain(curVar).size) match {
            //if we have domain of 0
            case (_, 0) => 
              throw new IllegalStateException("domain of " + curVar + " is 0: an impossible state")
            //if we have a set domain item, add to num
            case ((None, num), 1) => (None, num + 1) 
            case ((Some(ans), num) , 1) => (Some(ans), num + 1)
            
            //if we have an acceptable answer and we didn't before
            case ((None, num), ln) => (Some(curVar, ln), num)
            
            //if we have to compare two choices
            case ((Some((oldVar, oldLngth)), num) , newLngth) => 
              if (newLngth < oldLngth) (Some((curVar, newLngth)), num)
              else prevResult
          }}
     
    //return results
     match {
       case (Some((opt, _)), num) => (Some(opt), num)
       case (None, num) => (None, num)
     }
  }
  
  
  /**
   * checks to see if a backjump is necessary
   * @param vars			CSPVars state
   * @param errVar			the error variable
   * @param effectedVars	states the effected variables in current state
   * @return				returns true if there is a need for backjumping
   */
  private def backJump(vars : CSPVars, errVar : CSPVar, effectedVars : HashSet[CSPVar]) : Boolean = {
	
    //compile all variables who share a constraint with the error variable
    vars.getVarConstraints(errVar)./:(new HashSet[CSPVar]()){
      (hs, cons) => hs++=cons.getEffectedVars
    }
    //get intersection with effected variables
    .intersect(effectedVars)
    //if there is no common variable, we need to backjump
    .isEmpty
    
  }


  
  /**
   * searches for an actual result given constraints and variables
   * @param minScore			the minimum acceptable score for solution
   * @param maxScore			the maximum score possible
   * @param exhaustive			perform exhaustive search
   * @param curState			the current CSPVars state
   * @return					the solutionState to return from current search
   */
  private def search(minScore : Double, maxScore : Double, exhaustive : Boolean, curState : CSPVars) : Results = {
    getNext(curState) match {
	  //we're done!...no more best options
	  case (None, _) => Searcher.extractResults(curState)
	  
	  //there is another option, so keep going
	  case (Some(nextV), numVarsCompleted) => 
	    Searcher.logger.debug("continuing search with variable "+ nextV)
	    
	    //update status and hold on to it for further updates 
	    val curStatus = newStatus(curState.getVarDomain(nextV).size, numVarsCompleted, curState)
	    
	    //recurseive function that goes over all choices
	  	def tryElements(els : List[CSPElement], curBest : Option[Solution], minScore : Double) : Results = {
	      
	      //we are out of options...what do we have
	      els match {
	      	case Nil =>
	  		  curBest match {
	  	  	    //return a solution if there is one
	  	  	    case Some(sol) => 
	  	  	      Searcher.logger.debug("No more options available at this point so returning best solution with score " + sol.maxScore)
	  	  	      sol
	  	  	    //otherwise return errorval of this
	  	  	    case None => 
	  	  	      Searcher.logger.debug("No successful solution at location.  Returning error for " + nextV)
	  	  	      new Error(nextV)
	  	  	  }
 
	  	  //still some more options to go...	  	  
	      	case curEl::remaining =>
	  	      Searcher.logger.debug("checking if for " + nextV + " element " + curEl + " works")
	  	      
	  	  	  arcReduce(minScore, nextV, new CSPVars(curState, nextV, curEl)) match {
	  	  	    //arc Reduce did not produce a usable CSPVars, so go on
	  	  	    case None => 
	  	  	      Searcher.logger.debug("No successful solution for " + nextV + " and " + curEl)
	  	  	      updateStatus(curStatus)
	  	  	      tryElements(remaining, curBest, minScore)
	  	  	    
	  	  	    // arcReduce produced something usable
	  	  	    case Some((newState, effectedVars)) =>
	  	  	      	Searcher.logger.info("Attempting option " + nextV + " with " + curEl)
	  	  	      	
	  	  	      	//recurse on search
  	  				(search(minScore, maxScore, exhaustive, newState), curBest) match {
		  	  			  // if arc reduce gives us nothing and we dont have current best: check for back jump and return error
		  	  			  case (ev1 : Error, None) => 
		  	  			    //check if we need to backjump here...
		  	  			    if (backJump(newState, ev1.errorVar, effectedVars)) {
		  	  			      Searcher.logger.debug("backjump needed for variable at " + ev1.errorVar)
		  	  			      ev1
		  	  			    }
		  	  			    //otherwise, keep going
		  	  			    else {
		  	  			      Searcher.logger.info("Continuing because using " + curEl + " at " + nextV + " did not work");
		  	  			      updateStatus(curStatus);
		  	  			      tryElements(remaining, None, minScore)
		  	  			    }
		  	  			  
		  	  			  // if arc reduce gives us something and curBest has something, check which one is better
		  	  			  case (thisSolution : Solution, Some(prevSolution)) =>
		  	  			      //if this solution is at max Score, just return it because you won't do better
		  	  			    
		  	  			    	//if score is greater than equal to maxScore and not exhaustive search
		  	  			      if (thisSolution.maxScore >= maxScore && !exhaustive) {
		  	  			        Searcher.logger.debug("returning solution");
		  	  			        thisSolution
		  	  			      }
		  	  			      //if this solution is better than previous solution, use this
		  	  			      else if (thisSolution.maxScore > prevSolution.maxScore) {
		  	  			        Searcher.logger.debug("replacing current best solution for variable at " + nextV);
		  	  			        updateStatus(curStatus);
		  	  			        tryElements(remaining, Some(thisSolution), thisSolution.maxScore)
		  	  			      }
		  	  			      //otherwise use previous solution
		  	  			      else {
		  	  			        updateStatus(curStatus)		        
		  	  			        tryElements(remaining, Some(prevSolution), minScore)
		  	  			      }
		  	  			      
		  	  			    			    
		  	  			  // then either we have a successful previous solution
		  	  			  case (ev : Error, Some(_)) => 
	  	  			        updateStatus(curStatus)
		  	  			    tryElements(remaining, curBest, minScore)
		  	  			  
		  	  			  // ...or a successful new solution: if at maxScore, just return it
		  	  			  case (thisSolution : Solution, None) => 
		  	  			    
		  	  			    //if score is greater than equal to maxScore and not exhaustive search
		  	  			    if (thisSolution.maxScore >= maxScore && !exhaustive) thisSolution
		  	  			    else {
	  	  			          updateStatus(curStatus)
		  	  			      tryElements(remaining, Some(thisSolution), thisSolution.maxScore)
		  	  			    }
		  	  			  
		  	  			  //TODO fix this maybe???
		  	  			  case _ => throw new IllegalArgumentException("unexpected match in solution")
		  	  			}
		  	  	  }
		  	  }
		  	}
		  	
	    	//recurse over choices
		  	tryElements(curState.getVarDomain(nextV).toList, 
		  	    None, minScore)
        }
	}
}