package gregd.cspsolve.constraintarg

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.utils.Utils
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.log.MyLogger

object ConstraintArg {
  val logger = new MyLogger("ConstraintArg")

  def extractElement[A](item : ConstraintArg[A]) : A = {
	  item.getValue.getOrElse{
        logger.error("no value could be extracted from element argument")
        throw new IllegalArgumentException("could not extract value")
      }
  }
  
  def extractPair[A,B](pair : Pair[Option[A], Option[B]]) : (A, B) = {
    (pair.p1.getOrElse{
    	logger.error("no value could be extracted from the arguments")
    	throw new IllegalArgumentException("No arg was gathered")      
    },
    pair.p2.getOrElse{
    	logger.error("no value could be extracted from the arguments")
    	throw new IllegalArgumentException("No arg was gathered")      
    })
  }
  
  def extractOptionList[A](lstArg : List[Option[A]]) : List[A] = {
    lstArg.:\[List[A]]{
      Nil} {
        (arg, lst) =>
        arg match {
          case None => lst
          case Some(i) => i::lst
        }
      } 
  }
  
  def extractList[A](lstArg : ListConstraintArg[A]) : List[A] = {
      extractOptionList(lstArg.getValue.getOrElse{
    	logger.error("no value could be extracted from the arguments")
    	throw new IllegalArgumentException("No arg was gathered")
      })
  }
}

abstract class ConstraintArg[T] 
		(name : String, description : String, necessary : Boolean)
	{
	//is entirely necessary
	def isNecessary : Boolean = necessary
	
	//gets the name of the arg
	def getName : String = name
	
	//gets the description of the arg
	def getDescription : String = description
	
	def getNewInstance : ConstraintArg[T]
	
	//gets value
	def getValue : Option[T]
	
	//verifies that the argument is set
	def isSet : Boolean
		
	//returns true if a variable/element is removed
	def removeVariable(v : CSPVar) : Boolean;
	def removeElement(e : CSPElement) :  Boolean;
}

object ConstraintArgObject {
	val logger = new MyLogger("ConstraintArgObject")	
}

abstract class ConstraintArgObject[T] 
		(name : String, description :String, necessary : Boolean)
		extends ConstraintArg[T](name, description, necessary) {
  
  	def getNewInstance : ConstraintArgObject[T]; // = new ConstraintArgObject[T](name, description, necessary)
  
  	private var value : Option[T] = None
	
  	def setValue(aVal : T) = value = Some(aVal)
	  	  
	def getValue : Option[T] = value
	
	def isSet : Boolean = 
	  value match {
  	  	case Some(_) => true
  	  	case None => false
  	  }

  	def removeVariable(v : CSPVar) : Boolean = {
  	    if (value.getOrElse(false).equals(v)) {
  	      ConstraintArgObject.logger.debug("value of this constraintArgObject is equivalent to variable being removed; returning true")
  	      value = None
  	    }
  	    
  	    value match {
  	      case Some(_) => false
  	      case None => true
  	    }
  	}

  	def removeElement(e : CSPElement) : Boolean = {
  	    if (value.getOrElse(false).equals(e)) {
  	      ConstraintArgObject.logger.debug("value of this constraintArgObject is equivalent to element being removed; returning true")
  	      value = None
  	    }
  	    
  	    value match {
  	      case Some(_) => false
  	      case None => true
  	    }
  	}
}


case class IntArg (name : String, description :String, necessary : Boolean) 
		extends ConstraintArgObject[Int](name, description, necessary) {
  def getNewInstance : IntArg = new IntArg(name, description, necessary)
}
case class DoubleArg (name : String, description :String, necessary : Boolean) 
		extends ConstraintArgObject[Double](name, description, necessary) {
  def getNewInstance : DoubleArg = new DoubleArg(name, description, necessary)
}
case class BoolArg (name : String, description :String, necessary : Boolean) 
		extends ConstraintArgObject[Boolean](name, description, necessary) {
  def getNewInstance : BoolArg = new BoolArg(name, description, necessary)
}
case class StringArg (name : String, description :String, necessary : Boolean) 
		extends ConstraintArgObject[String](name, description, necessary) {
  def getNewInstance : StringArg = new StringArg(name, description, necessary)
}
case class VarArg (name : String, description :String, necessary : Boolean) 
		extends ConstraintArgObject[CSPVar](name, description, necessary) {
  def getNewInstance : VarArg = new VarArg(name, description, necessary)
}
case class ElementArg (name : String, description :String, necessary : Boolean) 
		extends ConstraintArgObject[CSPElement](name, description, necessary) {
  def getNewInstance : ElementArg = new ElementArg(name, description, necessary)
}
case class ConsArg (name : String, description :String, necessary : Boolean) 
		extends ConstraintArgObject[ModelConstraint](name, description, necessary) {
  def getNewInstance : ConsArg = new ConsArg(name, description, necessary)
}

		
object ListConstraintArg {
	val logger = new MyLogger("ListConstraintArg")	
}
		
class ListConstraintArg[T] 
		(name : String, description :String, min : Int, max : Option[Int], cArg : ConstraintArg[T])
		extends ConstraintArg[List[Option[T]]](name, description, (min > 0)) {
  
  	private var items : List[ConstraintArg[T]] = Nil
  	
	def getValue : Option[List[Option[T]]] = Some(items.map(arg => arg.getValue))
  	
  	def getSize = items.length
  	
  	def getMin = min
  	
  	def getMax = max
  	
	def getNewInstance : ListConstraintArg[T] =
	  new ListConstraintArg(name, description, min, max, cArg)
  	
  	def getListItem(index : Int) : ConstraintArg[T] =
  	  if (index >= getSize || index < 0) {
  	    ListConstraintArg.logger.error("index is " + index + " and size of this list is " + getSize)
  	    throw new ArrayIndexOutOfBoundsException
  	  }
  	  else items(index)
  
  	  
   def getAllArgs = items  
  	  
   def addListItem : Option[ConstraintArg[T]] = 
     max match {
  	    case Some(limit) => 
  	      if (getSize >= limit) {
  	        ListConstraintArg.logger.warn(
  	            "adding an element to this list causes it to be over max.  current size is: " + getSize + 
  	            " and max is " + max)
  	        None
  	      }
  	      else {
	  	    val newItem = cArg.getNewInstance
	        items :+= newItem;
	  	    ListConstraintArg.logger.debug("adding new element to list.  list size is now " + getSize)
	        Some(newItem)    
  	      }
  	    case None =>
	  	    val newItem = cArg.getNewInstance
	        items :+= newItem;
	  	    ListConstraintArg.logger.debug("adding new element to list.  list size is now " + getSize)
	        Some(newItem)
  	  }
  	
  	  	  
    def removeListItem(index : Int) : Boolean = 
  	  if (index >= getSize || index < 0) {
  	    ListConstraintArg.logger.error("index is " + index + " and size of this list is " + getSize)  	    
  	    false
  	  }
  	  else {
  	    items = Utils.removeIndex(index, items);
  	    true
  	  }
    
    
    def isSet : Boolean = {
  	   val itemSize = getSize
  	   ((itemSize < min), max) match {
  	     case (true, _) => 
  	       ListConstraintArg.logger.debug("list size is smaller than minimum amount, so not set")
  	       false
  	     case (false, None) | (false, Some(_))  => 
  	       items.:\(true){
  	       (item, prev) => 
  	         if (!prev) false
  	         else {
  	           item.getValue match {
  	           	  case None => 
  	           	    ListConstraintArg.logger.debug("list item " + item + " is not set")
  	           	    false
  	           	  case Some(_) => true
  	       	   }
  	         }
  	       }
  	   }
    }
    
    def removeVariable(v : CSPVar) : Boolean = {
         items.map((item) => item.removeVariable(v))
        
        items.:\(0){
          (item, curCnt) =>
            item.getValue match {
              case None => curCnt
              case Some(_) => curCnt + 1
            }
        } <= 0
    }

    def removeElement(e : CSPElement) : Boolean = {
        items.map((item) => item.removeElement(e))
        
        items.:\(0){
          (item, curCnt) =>
            item.getValue match {
              case None => curCnt
              case Some(_) => curCnt + 1
            }
        } <= 0
    }

}



class Pair[F,S](val p1 : F, val p2 : S)

object PairConstraintArg {
	val logger = new MyLogger("PairConstraintArg")	
}

class PairConstraintArg[F,S]
	(name : String, description : String, necessary : Boolean, 
	    first : ConstraintArg[F], second : ConstraintArg[S])
	(implicit tag: TypeTag[Pair[F,S]])
	extends ConstraintArg[Pair[Option[F],Option[S]]](name, description, necessary) {

	val thePair = new Pair(first, second)
  
	def getValue : Option[Pair[Option[F],Option[S]]] = Some(new Pair(thePair.p1.getValue, thePair.p2.getValue))
		
	def getNewInstance = new PairConstraintArg[F,S](name, description, necessary, 
	    thePair.p1.getNewInstance, thePair.p2.getNewInstance)
	
	def getFirst : ConstraintArg[F] = 
		thePair.p1
	
    def getSecond : ConstraintArg[S] = 
	  thePair.p2
    
    
    def isSet = 
        thePair.p1.isSet && thePair.p2.isSet
   
    def removeVariable(v : CSPVar) = {
	  thePair.p1.removeVariable(v);
      thePair.p2.removeVariable(v);
      (thePair.p1.getValue, thePair.p2.getValue) match {
	  	case (None, None) => 
	  	  PairConstraintArg.logger.debug("both entries are empty at this point")
	  	  true
	  	case _ => false
	  }
	}
      



    def removeElement(e : CSPElement) = {
	  thePair.p1.removeElement(e);
      thePair.p2.removeElement(e);
      (thePair.p1.getValue, thePair.p2.getValue) match {
	  	case (None, None) => 
	  	  PairConstraintArg.logger.debug("both entries are empty at this point")
	  	  true
	  	case _ => false
	  }
	}
}