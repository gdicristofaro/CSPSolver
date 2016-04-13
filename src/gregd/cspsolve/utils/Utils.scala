package gregd.cspsolve.utils

import gregd.cspsolve.constraintarg.ConstraintArg

object Utils {
  	def removeIndex[A](timesLeft : Int, lst : List[A]) : List[A] =
  	  ((timesLeft > 0), lst) match {
  	    case (true, hd::tl) => hd::(removeIndex((timesLeft - 1), tl))
  	    case (false, hd::tl) => tl
  	    case _ => throw new ArrayIndexOutOfBoundsException
  	  }
  	
  	
  /**
   * get index of item in list or none if does not exist
   * @param lst			the list
   * @param item		the item to find
   * @return 			the index if exists
   */
  def getIndex[A](lst : List[A], item : A) : Option[Int] = {
      def getIndexHelper[A](lst : List[A], curIndex : Int) : Option[Int] = {
    	lst match {
    	  case Nil => None
    	  case hd::tl => 
    	    if (hd.equals(item)) Some(curIndex)
    	    else getIndexHelper(tl, curIndex+1)
    	}
      }
      getIndexHelper(lst, 0)
  }
  
  /**
   * get sublist of list
   * @param lst				the list
   * @param indexStart		the starting index
   * @param indexEnd		the ending index
   * @return				the new list
   */
  def getSublist[A](lst : List[A], indexStart : Int, indexEnd : Int) : List[A] = {
    def subHelper[A](lst : List[A], curIndex : Int) : List[A] = {
      (lst, (curIndex < indexStart), (curIndex > indexEnd)) match {
      	//there is list, we are less than indexStart and not greater than indexEnd 
      	case (hd::tl, true, false) => subHelper(tl, (curIndex + 1))
      	//there is a list, we are in indexstart, we are not at indexEnd
      	case (hd::tl, false, false) => hd::subHelper(tl, (curIndex + 1))
      	//otherwise, just return Nil
      	case _ => Nil
      }
    }
    subHelper(lst, 0)
  }
  
  /**
   * splits a string at delimiter
   * @param origString		the string to split
   * @param delimiter		the delimiter to split
   * @param trim			remove white spaces before and after
   */
  def splitString(origString : String, delimiter : String, trim : Boolean) : Array[String] = {
    // split string by delimiter
    val stringItems = origString.split(delimiter.toCharArray)
    if (trim)
      stringItems.map((string) => string.trim)
    else
      stringItems
  }
    
 
  
    /**
     * extracts list from iterator
     * @param iter			the iterator
     * @param compList		the currently compiled List
     */
    def listFromIterator[A](iterator : java.util.Iterator[A]) : List[A] = {
		def listHelper[A](iter : java.util.Iterator[A], compList : List[A]) : List[A] = {
		  if (iter.hasNext) {
		    val nextval = iter.next
		    listHelper(iter, compList :+ nextval)
		  }
		  else
		    compList
		}
		listHelper(iterator, Nil)
    } 
    

   /**
    * extracts list from list of options
    * @param lst		the array
    * @return			the finished array
    */
   def extractList[A: Manifest](lst : List[Option[A]]) : List[A] = {
    lst.:\[List[A]]{
      Nil} {
        (arg, lst) =>
        arg match {
          case None => lst
          case Some(i) => i::lst
        }
      }
  }
   
}