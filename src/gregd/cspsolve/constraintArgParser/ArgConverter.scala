package gregd.cspsolve.constraintArgParser

import gregd.cspsolve.constraintarg.ConstraintArgObject
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.constraintarg.ConstraintArg
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.PairConstraintArg
import gregd.cspsolve.constraintarg.BoolArg
import gregd.cspsolve.constraintarg.IntArg
import gregd.cspsolve.constraintarg.DoubleArg
import gregd.cspsolve.constraintarg.StringArg
import gregd.cspsolve.constraintarg.VarArg
import gregd.cspsolve.constraintarg.ElementArg
import gregd.cspsolve.constraintarg.ConsArg
import gregd.cspsolve.model.ModelVar
import gregd.cspsolve.model.ModelElement
import scala.collection.mutable.HashMap
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.log.MyLogger



		
object ArgTypeConverter {
  val logger = new MyLogger("ArgTypeConverter")
}
		
/**
 * responsible for converting input to output and put into an argument
 * @param encoder		the encoding function
 * @param decoder		the decoding function
 */
class ArgTypeConverter[I,O](encoder : I => O, decoder : O => I ) {
  
    /**
     * sets value given input type value
     * @param value			the value that the arg should have
     * @param arg			the argument of type O
     * @return				the argument that is set
     */
	def setValue(value : I, arg : ConstraintArgObject[O]) : ConstraintArgObject[O] = {
	  try {
		  val encoded = encoder(value)
		  ArgTypeConverter.logger.debug("converting arg from " + value + " to " + encoded)
		  arg.setValue(encoded);
		  arg	    
	  }
	  catch {
	    case e : Throwable => throw new IllegalArgumentException("error setting value: " + e.getLocalizedMessage)
	  }

	}
	
	/**
	 * retrieves value from argument
	 * @param arg			the argument to obtain
	 * @return				the value of type I
	 */
	def getValue(arg : ConstraintArgObject[O]) : I = {
	  try {
		  val value = arg.getValue.get
		  val decoded = decoder(value)
		  ArgTypeConverter.logger.debug("converting arg from " + value + " to " + decoded);
		  decoded  
	  }
	  catch {
	    case e : Throwable => throw new IllegalArgumentException()
	  }
	}
}



object Parser {
  val logger = new MyLogger("Parser")
}

/**
 * Parser converts from one form to another
 * @param encoder			encoding function
 * @param decoder			decoding function
 */
class Parser[A, B](encoder : A => B, decoder : B => A) {
  /**
   * uses encoder operation to parse the value
   * @param value		parses value
   * @return			the parsed value
   */
  def parse(value : A) : B = {
	  try {
		  val encoded = encoder(value)   
		  Parser.logger.debug("converting from " + value + " to " + encoded);
		  encoded
	  }
	  catch {
	    case e : Throwable => throw new IllegalArgumentException()
	  }    
  }

  /**
   * uses decoder function to join
   * @param value		the value to join
   * @return			the joined value
   */
    def join(value : B) : A = {
	  try {
		  val decoded = decoder(value)   
		  Parser.logger.debug("converting from " + value + " to " + decoded);
		  decoded   
	  }
	  catch {
	    case e : Throwable => throw new IllegalArgumentException()
	  }    
  }
}


object ArgConverter {
  val logger = new MyLogger("ArgConverter")
}

/**
 * converts an arg of type I to relevant arguments
 * @param all relevant converters
 */
class ArgConverter[I]
	(BoolConvert : ArgTypeConverter[I, Boolean],
	 IntConvert : ArgTypeConverter[I, Int],
	 DoubleConvert : ArgTypeConverter[I, Double],
	 StringConvert : ArgTypeConverter[I, String],
	 VarConvert : ArgTypeConverter[I, CSPVar],
	 ElementConvert : ArgTypeConverter[I, CSPElement],
	 ConstraintConvert : ArgTypeConverter[I, ModelConstraint],
	 ListConvert : Parser[I, List[I]],
	 PairConvert : Parser[I, (I,I)]) {
  
  /**
   * sets List given arguments to args
   */
  private def setList (curIndex : Int, args : ListConstraintArg[_], Ilist : List[I]) : ListConstraintArg[_] = {
    Ilist match {
      case hd::tl =>
        val curArg = 
        	if (curIndex >= args.getSize) args.addListItem.getOrElse(throw new ArrayIndexOutOfBoundsException())
        	else args.getListItem(curIndex)
        setValue(hd, curArg);
        setList(curIndex + 1, args, tl)
      case Nil => args
    }    
  }
  
  /**
   * sets value given a value I
   * @param value		the value
   * @param arg			the argument to set
   */
  def setValue[T](value : I, arg : ConstraintArg[T]) : ConstraintArg[_] = {
	  arg match {
	    case (listArg : ListConstraintArg[_]) => 
	      setList(0, listArg, ListConvert.parse(value))
	    case (pairArg : PairConstraintArg[_,_]) => 
	      val (fst, scd) = PairConvert.parse(value)
	      setValue(fst, pairArg.getFirst);
	      setValue(scd, pairArg.getSecond);
	      pairArg

	    case (boolarg : BoolArg) => BoolConvert.setValue(value, boolarg)
	    case (intarg : IntArg) => IntConvert.setValue(value, intarg)
	    case (doublearg : DoubleArg) => DoubleConvert.setValue(value, doublearg)
	    case (stringarg : StringArg) => StringConvert.setValue(value, stringarg)
	    case (vararg : VarArg) => VarConvert.setValue(value, vararg)
	    case (elarg : ElementArg) => ElementConvert.setValue(value, elarg)
	    case (consarg : ConsArg) => ConstraintConvert.setValue(value, consarg)
		case _ => 
		  ArgConverter.logger.error("Constraint Arg " + arg + " does not match accepted args")
		  throw new IllegalArgumentException("ConstraintArg does not match accepted args")
	  }
	}
  
  	
  /**
   * gets value given a constraint arg
   * @param arg			the argument to get the value
   */  	
  	def getValue[T](arg : ConstraintArg[T]) : I = {
	  arg match {
	    case (listArg : ListConstraintArg[_]) => 
	      ListConvert.join(listArg.getAllArgs.map(arg => getValue(arg)))
	    case (pairArg : PairConstraintArg[_,_]) => 
	      PairConvert.join(getValue(pairArg.getFirst), getValue(pairArg.getSecond))
	    case (objArg : ConstraintArgObject[T]) => 
		  arg match {
		    case (boolarg : BoolArg) => BoolConvert.getValue(boolarg)
		    case (intarg : IntArg) => IntConvert.getValue(intarg)
		    case (doublearg : DoubleArg) => DoubleConvert.getValue(doublearg)
		    case (stringarg : StringArg) => StringConvert.getValue(stringarg)
		    case (vararg : VarArg) => VarConvert.getValue(vararg)
		    case (elarg : ElementArg) => ElementConvert.getValue(elarg)
		    case (consarg : ConsArg) => ConstraintConvert.getValue(consarg)
		    case _ => throw new IllegalArgumentException("ConstraintArg does not match accepted args")
		  }
		case _ => throw new IllegalArgumentException("ConstraintArg does not match accepted args")
	  }
	}
}


class StringConverter(
		VarConvert : ArgTypeConverter[String, CSPVar],
		ElementConvert : ArgTypeConverter[String, CSPElement],
		ConstraintConvert : ArgTypeConverter[String, ModelConstraint]
    ) extends ArgConverter[java.lang.String](
	    new ArgTypeConverter[String, Boolean](java.lang.Boolean.parseBoolean, java.lang.Boolean.toString),
	    new ArgTypeConverter[String, Int](java.lang.Integer.parseInt, java.lang.Integer.toString),
	    new ArgTypeConverter[String, Double](java.lang.Double.parseDouble, java.lang.Double.toString),
	    new ArgTypeConverter[String, String]((input => input), (output => output)),

	    VarConvert,
	    ElementConvert,
	    ConstraintConvert,

	    new Parser[String, List[String]]
	      ((string => string.split(Array(';')).:\[List[String]](Nil){(str, arr) => str.trim :: arr}), 
	      (list => list.:\(""){(curString, orig) => curString + " ; " + orig})), 
	    new Parser[String, (String,String)](
	        {string => 
	          val arr = string.split(Array(':'))
	          (arr(0).trim, arr(1).trim)}, 
	      (pair => pair._1 + " : " + pair._2)))
  
	  
object ObjectConverter extends ArgConverter[Object](
	    new ArgTypeConverter[Object, Boolean]((input => input.asInstanceOf[Boolean]), (o => o.asInstanceOf[Object])),
	    new ArgTypeConverter[Object, Int]((input => input.asInstanceOf[Int]), (o => o.asInstanceOf[Object])),
	    new ArgTypeConverter[Object, Double]((input => input.asInstanceOf[Double]), (o => o.asInstanceOf[Object])),
	    new ArgTypeConverter[Object, String]((input => input.asInstanceOf[String]), (output => output.asInstanceOf[String])),

	    new ArgTypeConverter[Object, CSPVar]((input => input.asInstanceOf[CSPVar]), (output => output.asInstanceOf[CSPVar])),
	    new ArgTypeConverter[Object, CSPElement]((input => input.asInstanceOf[CSPElement]), (output => output.asInstanceOf[CSPElement])),
	    new ArgTypeConverter[Object, ModelConstraint]((input => input.asInstanceOf[ModelConstraint]), (output => output.asInstanceOf[ModelConstraint])),
	    new Parser[Object, List[Object]]
	      ((obj => obj.asInstanceOf[List[Object]]), 
	      (obj => obj.asInstanceOf[Object])),
	    new Parser[Object, (Object,Object)](
	      (obj => obj.asInstanceOf[(Object, Object)]), 
	      (obj => obj.asInstanceOf[Object])))