package gregd.cspsolve.constraint

import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.PairConstraintArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.constraintArgParser.ObjectConverter
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.constraintarg.IntArg


/**
 * does necessary set up for specials schedule
 */
class SpecialsScheduleByClass extends ModelConstraint {
  def getName = "Specials Schedule By Class"
  def getDescription = "Defines necessary specials classes setup by classroom teacher"

    val ClassArg = new ListConstraintArg(
      "Classes", 
      "Allclasses day 1-10", 
      10, Some(10), new VarArg("","",true))

      val SpecialsArg = new ListConstraintArg(
      "Specials Classes", 
      "Media, Art, Music1, Music2, PE1, PE2", 
      6, Some(6), new ElementArg("","",true))
  
  def getArgs = Array[ConstraintArg[_]](ClassArg, SpecialsArg)
    
  
  def newInstance = new SpecialsScheduleByClass
 
  
  
  private def getEqualLimits(classes : List[CSPElement], vars : List[CSPVar], num : Int) : Array[CSPConstraint] = {
    val ConLim = new ElementsEqual;
    val conlimArgs = ConLim.getArgs;
    ObjectConverter.setValue(vars, conlimArgs(0));
    ObjectConverter.setValue(classes.map((c) => (c, num)), conlimArgs(1));
    ConLim.getConstraint
  }
  
  private def getMaxLimits(classes : List[CSPElement], vars : List[CSPVar], num : Int) : Array[CSPConstraint] = {
    val ConLim = new ElementsMax;
    val conlimArgs = ConLim.getArgs;
    ObjectConverter.setValue(vars, conlimArgs(0));
    ObjectConverter.setValue(classes.map((c) => (c, num)), conlimArgs(1));
    ConLim.getConstraint
  }
  
  private def getSpacing(classes : List[CSPElement], vars : List[CSPVar], num : Integer, wrapAround : java.lang.Boolean) : Array[CSPConstraint] = {
    val ConLim = new ElementSpacing;
    val conlimArgs = ConLim.getArgs;
    ObjectConverter.setValue(vars, conlimArgs(0));
    ObjectConverter.setValue(classes, conlimArgs(1));
    ObjectConverter.setValue(num, conlimArgs(2));
    ObjectConverter.setValue(wrapAround, conlimArgs(3));
    ConLim.getConstraint
  }
  
  private def getDifferentElement(classes : List[CSPElement], vars : List[CSPVar], num : Integer) : Array[CSPConstraint] = {
    val ConLim = new DifferentElementInRange;
    val conlimArgs = ConLim.getArgs;
    ObjectConverter.setValue(vars, conlimArgs(0));
    ObjectConverter.setValue(classes, conlimArgs(1));
    ObjectConverter.setValue(num, conlimArgs(2));
    ConLim.getConstraint
  }
  
  @throws(classOf[IllegalArgumentException])
  def getConstraint = {
        
    var constraints : List[CSPConstraint] = Nil;
    
    val classes = ConstraintArg.extractList(ClassArg)
    val specials = ConstraintArg.extractList(SpecialsArg)
        
    val media = specials(0)
    val art = specials(1)
    val m1 = specials(2)
    val m2 = specials(3)
    val PE1 = specials(4)
    val PE2 = specials(5)
    
    //media and art one to a week; PE and Music max of 2 to week but also 3 total
    constraints ++= getEqualLimits(media::art::Nil, classes.slice(0, 5), 1)
    constraints ++= getEqualLimits(media::art::Nil, classes.slice(5, 10), 1)

    constraints ++= getMaxLimits(PE1::PE2::Nil, classes, 3)
    constraints ++= getMaxLimits(m1::m2::Nil, classes, 3)
    
    constraints ++= getMaxLimits(PE1::PE2::Nil, classes.slice(0, 5), 2)
    constraints ++= getMaxLimits(PE1::PE2::Nil, classes.slice(5, 10), 2)
    
    constraints ++= getMaxLimits(m1::m2::Nil, classes.slice(0, 5), 2)
    constraints ++= getMaxLimits(m1::m2::Nil, classes.slice(5, 10), 2)    
        
    //spacing for music and PE
    constraints ++= getSpacing(PE1::PE2::m1::m2::Nil, classes, 1, true)
    
      
    //limits per teacher in music and PE
    constraints ++= getDifferentElement(m1::m2::Nil, classes, 1)
    constraints ++= getDifferentElement(PE1::PE2::Nil, classes, 1)
    
    
    constraints.toArray[CSPConstraint]
  }
  
}