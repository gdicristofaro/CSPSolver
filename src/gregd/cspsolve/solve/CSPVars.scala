package gregd.cspsolve.solve

import gregd.cspsolve.log.MyLogger



/**
 * some util functions to initially make CSPVars
 */
object CSPVars {
	val logger = new MyLogger("CSPVars")
  
	/**
	 * determines a satisfaction map given constraints and the mapping of variables to domains
	 * @param constraints		the HashSet of constraints
	 * @param varsToDomains		a mapping of variables to domains
	 * @param tieredScore		indicates whether score for solution should be tiered
	 *			 				i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95
	 * @return					hashmap of constraint to double
	 */
	def detSatisfaction(constraints : scala.collection.immutable.HashSet[CSPConstraint], 
	    varsToDomains : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]], tieredScore : Boolean): 
	    scala.collection.mutable.HashMap[CSPConstraint, Double] =
		  constraints./:(new scala.collection.mutable.HashMap[CSPConstraint, Double]()){
		    (hm, c) => hm += (c -> determineConsScore(c, c.getSatisfaction(varsToDomains), tieredScore))
	  }
	
	/**
	 * creates a mapping of variables to the constraints that rely on those variables
	 * @param constraints		the hashset of constraints
	 * @param varsToDomains		the mapping of variables to domains
	 * @return					the mapping of variables to constraints effected
	 */
	def varsToCons(constraints : scala.collection.immutable.HashSet[CSPConstraint], 
	    varsToDomains : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]) :
	    scala.collection.immutable.HashMap[CSPVar, scala.collection.immutable.HashSet[CSPConstraint]] = {
	  //map all variables to scala.collection.mutable.HashSet of constraints 
	  	  
	  val mapping = constraints./:(new scala.collection.immutable.HashMap[CSPVar, scala.collection.immutable.HashSet[CSPConstraint]]()) {
		(hm, c) =>
		  c.getEffectedVars./:(hm) { 
		    (hm, v) => 
		      hm.get(v) match {
		        case None => hm + (v -> scala.collection.immutable.HashSet[CSPConstraint](c))
		        case Some(cs) => hm + (v -> (cs + c))
		      }
		  }
	  }
	  mapping
	}
	

	/**
	 * determines total score given all constraint scores
	 * @param constraints		the hashset of constraints
	 * @param varsToDomains		the mapping of variables to domains
	 * @param tieredScore		indicates whether score for solution should be tiered
	 *			 				i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95
	 * @return					the score
	 */
	def getTotScore (constraints : scala.collection.immutable.HashSet[CSPConstraint], 
	    varsToDomains : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]],
	    tieredScore : Boolean) : Double = {
	  if (tieredScore)
		  detSatisfaction(constraints,varsToDomains, tieredScore)./:(1.0)((curScore, entry) => math.min(entry._2 , curScore ))
	  else
	      detSatisfaction(constraints,varsToDomains, tieredScore)./:(0.0)((curScore, entry) => entry._2 + curScore )
	}
	  

	/**
	 * does a copying of var domain and set domain of v to e
	 * @param old			old variable domains
	 * @param v				the variable to set domain
	 * @param e				the element to which the domain will be
	 * @return				the copy
	 */
	def copyVarDomainsAndSet (old : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]],
			v : CSPVar, e : CSPElement) : 
		scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]] = {
	  val curDoms = (old./:(
	          new scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]]()) {
	        (hm, varDom) => 
	          val (v, dom) = varDom
	          hm += (v -> (new scala.collection.mutable.HashSet[CSPElement]() ++= dom))
	      })
	  curDoms += (v -> (new scala.collection.mutable.HashSet[CSPElement]() += e))
	}
	
	/**
	 * copy scores of constraint to double mapping to map 
	 * @param old			the old mapping
	 * @return 				the copy
	 */
	def copyScores (old : scala.collection.mutable.HashMap[CSPConstraint, Double]) : 
		scala.collection.mutable.HashMap[CSPConstraint, Double] =
	  old./:(new scala.collection.mutable.HashMap[CSPConstraint, Double]()){
		(hm, consDoub) => 
		  val (cons, doub) = consDoub
		  hm += (cons -> doub)
	  }
	
    /**
     * determines weighted prioritized score given constraint and score
     * 
     * @param c					the constraint
     * @param score				the new score
	 * @param tieredScore		indicates whether score for solution should be tiered
	 *			 				i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95
     */
    def determineConsScore(c : CSPConstraint, score : Double, tieredScore : Boolean) : Double =
      if (tieredScore) score
      else c.getPriority * c.getWeight * score
}





/**
 * class that contains information pertaining to the state of the CSPVars
 * @param constraintScores			the mapping of constraints to their current score
 * @param varsToConstraints			a mapping of variables to the constraints that utilized
 * @param varsToDomains				a mapping of variables to their domain
 * @param tieredScore				indicates whether score for solution should be tiered
 *			 						i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95 
 */
class CSPVars private (
    private val constraintScores : scala.collection.mutable.HashMap[CSPConstraint, Double],
    private val varsToConstraints : 
    	scala.collection.immutable.HashMap[CSPVar, scala.collection.immutable.HashSet[CSPConstraint]],
    private val varsToDomains : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]],
    private var totalScore : Double,
    private val tieredScore : Boolean
	) {
  
  
    /**
     * serves as initial constructor for CSPVars
     * @param varsToDomains			the mapping of variables to their domains
     * @param constraints			the constraints
     * @param tieredScore			indicates whether score for solution should be tiered
     *			 					i.e. if constraint of .9 is met but constraint of .95 if not met, score is .95
     */
	def this(varsToDomains : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]],
			constraints : scala.collection.immutable.HashSet[CSPConstraint], tieredScore : Boolean) = {
			  this(
			      //go through constraints and determine initial satisfaction
			      CSPVars.detSatisfaction(constraints, varsToDomains, tieredScore), 
			      //map constraints to each variable that constraint contains
			      CSPVars.varsToCons(constraints, varsToDomains), 
			      //the variable domain
			      varsToDomains,
			      CSPVars.getTotScore(constraints, varsToDomains, tieredScore),
			      tieredScore
			      )
	}
  
  
  
	/**
	 * constructor that just copies an old CSPVars and sets domain of v to e
	 * 
	 * @param oldVars		the old CSPVars to copy
	 * @param v				CSPVar to set
	 * @param e				CSPElement that will be the new domain in var
	 */
	def this(oldVars : CSPVars, v : CSPVar, e : CSPElement) = 
	  this(
	      //do a deeper clone of scores
	      CSPVars.copyScores(oldVars.constraintScores),
	      oldVars.varsToConstraints, 
	      //do a deeper clone of old domain
	      CSPVars.copyVarDomainsAndSet(oldVars.varsToDomains, v, e),
	      oldVars.totalScore,
	      oldVars.tieredScore
	      )

	      
	/**
	 * gives a list for all constraints that pertain to variable
	 * @param v			the CSPVar we want to get constraints for
	 * @return			list with all constraints
	 */
    def getVarConstraints(v : CSPVar) : scala.collection.immutable.HashSet[CSPConstraint] =
    	varsToConstraints.get(v).getOrElse(new scala.collection.immutable.HashSet[CSPConstraint])
    
  
	/**
	 * returns hashmap with all domains
	 * @return 			the hashmap with vars mapped to scala.collection.mutable.HashSet of current domain
	 */
	def getAllDomains : scala.collection.mutable.HashMap[CSPVar, scala.collection.mutable.HashSet[CSPElement]] = 
	  varsToDomains 
 
	  
	/**
	 * get variables 
	 * @return 		a list of all variables
	 */
    def getVars : scala.collection.Set[CSPVar] = varsToDomains.keySet
    
    
    /**
     * get variable domain for variable
     * @param v		the variable that you want the domain for
     * @return 		the list of elements in domain
     */
    def getVarDomain(v : CSPVar) : scala.collection.mutable.HashSet[CSPElement] =
      varsToDomains.get(v).getOrElse(new scala.collection.mutable.HashSet[CSPElement]()) 

      
    /**
     * set domain for variable
     * @param v		the variable whose domain is set
     * @param lst	the list of elements to update domain to
     */
    def setVarDomain(v : CSPVar, lst : scala.collection.mutable.HashSet[CSPElement]) : Unit = 
      varsToDomains += (v -> lst)

    
    /** 
     *  returns maximum possible score from this particular state of the variables
     *  @return		the score
     */
    def getMaxScore : Double = totalScore
    
    /**
     * adjusts min score from percentage to usable data by searcher
     * @param minScore			original minScore data
     * @return					adjusted minScore
     */
    def getAdjustedMinScore(minScore : Double) : Double=
      if (tieredScore) minScore
      else (minScore * totalScore)


    /**
     * set the score for a constraint
     * @param c				the constraint
     * @param cScore		the satisfaction score
     */
    def setConstraintScore(c: CSPConstraint, score : Double) : Unit = {
	  //determine new constraint score
  	  val cScore = CSPVars.determineConsScore(c, score, tieredScore)

      //update total score
  	  
      totalScore = 
        if (tieredScore) math.min(totalScore, cScore) 
        else totalScore + (cScore - constraintScores.get(c).get)
      //put in hashmap
      constraintScores += (c-> cScore)
    }

	
	
    /**
	 * tests constraint with a variable set to a specific element in domain
	 */
	def testConstraint(c : CSPConstraint, v : CSPVar, e : CSPElement, minScore : Double) : Option[Double] = {
    	val oldDomain = getVarDomain(v)
	  
	    //set domain to just the one element
	    setVarDomain(v, new scala.collection.mutable.HashSet[CSPElement] += e)
		//see what the score is
		val cScore =  c.getSatisfaction(getAllDomains)
	
		setVarDomain(v, oldDomain)
		
		//if priority 1, and score is less than one, ditch this element
		if (c.isAbsolute && cScore < 1) None
	    else if (!testConstraintScore(c, cScore, minScore)) None
	    else Some(cScore)
	}
	
    
    /**
     * tests a constraints score to check if it will keep the total above minScore
     * @param c			the constraint that we are checking the score for
     * @param cScore	the new score for this constraint
     * @param minScore	the minimum total score acceptable
     * @return			true if acceptable value 
     */
    def testConstraintScore(c : CSPConstraint, score : Double, minScore : Double) : Boolean = {
      //determine constraint score
      val cScore = CSPVars.determineConsScore(c, score, tieredScore)
      
	  //test
      if (tieredScore) (math.min(cScore, totalScore) > minScore)
      else ((getMaxScore - constraintScores.get(c).get + cScore) > minScore)
    }
    
    /**
     * returns constraint scores mapping
     * 
     * note: score is a function of satisfaction level * priority * weight 
     * 
     * @return		the mapping of constraints to their respective score
     * 
     */
    def getConstraintScores : scala.collection.mutable.HashMap[CSPConstraint, Double] = constraintScores
    
    override def toString = {
      "\ncur max score is " + totalScore + 
      "\n\nvars to domains are:\n" +
      varsToDomains.:\(""){
        (entry, str) =>
          val (v, d) = entry
          str +
          "\nvariable: " + v + " - " +
          d.:\(""){
            (e, str) =>
              str + e + "; "
          }
          
      }
      
    }
}


object CSPVar {
  private var id = 0 
  def getID :Int = {
    id = id + 1;
    id
  }
}

/**
 * a CSP variable
 */
trait CSPVar {
  private val id = CSPVar.getID
  
   override def equals(other : Any) = {
    try {
    	(other.asInstanceOf[CSPVar] eq this.asInstanceOf[CSPVar])
    }
    catch {
      case e : Throwable => false
    }
  }
  
  override def toString : String = {
    "var["+id+"]"
  }
}


object CSPElement {
  private var id = 0 
  def getID :Int = {
    id = id + 1;
    id
  }
}

/**
 * a CSP element to belong to the domain
 */
trait CSPElement {
	private val id = CSPElement.getID
  
    override def equals(other : Any) = {
    try {
    	(other.asInstanceOf[CSPElement] eq this.asInstanceOf[CSPElement])
    }
    catch {
      case e : Throwable => false
    }
  }
    
  override def toString : String = {
    "el["+id+"]"
  }
}