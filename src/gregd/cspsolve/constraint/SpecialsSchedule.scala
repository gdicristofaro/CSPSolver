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
class SpecialsSchedule extends ModelConstraint {
  def getName = "Specials Schedule"
  def getDescription = "Defines necessary specials classes setup"

    val MediaArg = new ListConstraintArg(
      "Media Classes", 
      "All Media classes day 1-10", 
      10, Some(10), new VarArg("","",true))
  
    val ArtArg = new ListConstraintArg(
      "Media Classes", 
      "All Media classes day 1-10", 
      10, Some(10), new VarArg("","",true))
  
    val Music1Arg = new ListConstraintArg(
      "Media Classes", 
      "All Media classes day 1-10", 
      10, Some(10), new VarArg("","",true))
  
    val Music2Arg = new ListConstraintArg(
      "Media Classes", 
      "All Media classes day 1-10", 
      10, Some(10), new VarArg("","",true))
  
    val PE1Arg = new ListConstraintArg(
      "Media Classes", 
      "All Media classes day 1-10", 
      10, Some(10), new VarArg("","",true))
  
   val PE2Arg = new ListConstraintArg(
      "Media Classes", 
      "All Media classes day 1-10", 
      10, Some(10), new VarArg("","",true)) 
  
  
  val classesArg = new ListConstraintArg(
      "All Standard Form Classes", 
      "Elements that should have this spacing", 1, None, 
      new ElementArg("","",true)
  )
  
   val CountArg = new ListConstraintArg(
      "Class Numbers for specialists in Music and PE", 
      "Music1, Music2, PE1, PE2", 
   4, Some(4), new IntArg("","",true)) 

  
  
  def getArgs = Array[ConstraintArg[_]](MediaArg, ArtArg, Music1Arg, Music2Arg, PE1Arg, PE2Arg, classesArg, CountArg)
    
  
  def newInstance = new SpecialsSchedule
 
  
  
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
    
    val media = ConstraintArg.extractList(MediaArg)
    val art = ConstraintArg.extractList(ArtArg)
    val m1 = ConstraintArg.extractList(Music1Arg)
    val m2 = ConstraintArg.extractList(Music2Arg)
    val PE1 = ConstraintArg.extractList(PE1Arg)
    val PE2 = ConstraintArg.extractList(PE2Arg)
    val classes = ConstraintArg.extractList(classesArg)
    val counts = ConstraintArg.extractList(CountArg)
    
    
    //media and art one to a week; PE and Music max of 2 to week but also 3 total
    constraints ++= getEqualLimits(classes, media.slice(0, 5), 1)
    constraints ++= getEqualLimits(classes, media.slice(5, 10), 1)
    constraints ++= getEqualLimits(classes, art.slice(0, 5), 1)
    constraints ++= getEqualLimits(classes, art.slice(5, 10), 1)
    
    constraints ++= getEqualLimits(classes, PE1 ++ PE2, 3)
    constraints ++= getEqualLimits(classes, m1 ++ m2, 3)

    constraints ++= getMaxLimits(classes, PE1.slice(0, 5) ++ PE2.slice(0, 5), 2)
    constraints ++= getMaxLimits(classes, PE1.slice(5, 10) ++ PE2.slice(5, 10), 2)
    constraints ++= getMaxLimits(classes, m1.slice(0, 5) ++ m2.slice(0, 5), 2)
    constraints ++= getMaxLimits(classes, m1.slice(5, 10) ++ m2.slice(5, 10), 2)

    //spacing for music and PE
    constraints ++= getSpacing(classes, PE1, 1, true)
    constraints ++= getSpacing(classes, PE2, 1, true)
    constraints ++= getSpacing(classes, m1, 1, true)
    constraints ++= getSpacing(classes, m2, 1, true)
    
    //one class per day
    for (x <- 0 to 9) {
      constraints ++= getEqualLimits(classes, media(x)::art(x)::m1(x)::m2(x)::PE1(x)::PE2(x)::Nil, 1)
    }
      
    //limits per teacher in music and PE
    constraints ++= getDifferentElement(classes, m1, counts(0))
    constraints ++= getDifferentElement(classes, m2, counts(1))
    constraints ++= getDifferentElement(classes, PE1, counts(2))
    constraints ++= getDifferentElement(classes, PE2, counts(3))
    
    constraints.toArray[CSPConstraint]
  }
  
}