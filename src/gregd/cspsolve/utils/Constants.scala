package gregd.cspsolve.utils

import gregd.cspsolve.constraint.PairEither
import gregd.cspsolve.constraint.MutuallyExclusive
import gregd.cspsolve.constraint.ElementMax
import gregd.cspsolve.constraint.ElementEqual
import gregd.cspsolve.constraint.ElementMin
import gregd.cspsolve.constraint.ElementsEqual
import gregd.cspsolve.constraint.ElementsEqualInRange
import gregd.cspsolve.constraint.ElementsMax
import gregd.cspsolve.constraint.ElementSpacing
import gregd.cspsolve.constraint.DifferentElementInRange
import gregd.cspsolve.constraint.SameElement
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraint.SpecialsSchedule
import gregd.cspsolve.constraint.SpecialsScheduleByClass

object Constants {
	  val availableConstraints : List[ModelConstraint] = 
	    new SpecialsScheduleByClass :: new SpecialsSchedule::
	    new ElementSpacing::new SameElement::
	    new DifferentElementInRange::new ElementsMax::
	    new ElementsEqualInRange::new ElementsEqual::
	    new ElementEqual::new ElementMax::
	    new ElementMin::new MutuallyExclusive::
	    new PairEither::Nil
	  
	  
	  val defaultMinScore : Double = 0.0
	  val defaultExhaustiveSearch : Boolean = false
	  val defaultUseTieredSearch : Boolean = false
}