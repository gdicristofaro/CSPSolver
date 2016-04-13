package gregd.cspsolve.model

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.utils.Utils
import gregd.cspsolve.solve.Searcher

object TableDataModel {
  val logger = new MyLogger("TableDataModel")
}

class TableDataModel extends ConstraintManager with PrefsManager {
  /**
   * class that contains information pertaining to a variable's table, row, and column
   */
  private class Key(val t : ModelTable, val r : ModelRow, val c : ModelColumn) {
    //necessary for finding in hash
	override def hashCode(): Int = {t.hashCode() + r.hashCode() + c.hashCode() }
    
	//necessary for anonymous hash finding stuff
	override def equals(obj:Any) = {
	    obj match {
		    case other : Key =>  ((this.t eq other.t) && (this.c eq other.c) && (this.r eq other.r))
		  	case _ => false
	    }
    }
	
	override def toString : String = {
	  "Key: " + t.name + "-" + r.name + "-" + c.name
	}
  }
  
  
  //holds all relevant data (not in table form, though...)
  private val dataModel = new DataModel
    
  //mapping of keys (table, row, column) to vars
  private var keyVarsMap = new HashMap[Key, ModelVar]();
  
  
  //holds all available columns, rows, and tables
  private var columnsList : List[ModelColumn] = Nil
  private var rowsList : List[ModelRow] = Nil
  private var tablesList : List[ModelTable] = Nil

  

  /**
   * returns all columns
   * @return 	a list of columns
   */
  def getCols = columnsList
  
  /**
   * returns all rows
   * @return 	a list of rows
   */
  def getRows = rowsList
  
  /**
   * returns all tables
   * @return 	a list of tables
   */  
  def getTables = tablesList
  
  
  /**
   * gets position of var in table given var
   * 
   * @param v		the var to be looked up
   * @return		the table, row, column position
   */
  def getVarPosition(v : ModelVar) : Option[(ModelTable, ModelRow, ModelColumn)] = {    
    keyVarsMap.:\[Option[(ModelTable, ModelRow, ModelColumn)]](None){
      (set, pos) =>
        pos match {
          case Some(_) => pos
          case None =>
            val (key, thisVar) = set
            if (thisVar == v) Some((key.t, key.r, key.c))
            else None
        }
    }
  }
  
  
  
  /**
   * sets a variable's domain at a given table, row, and column
   * @param tab			table in which to set
   * @param row			row in which to set
   * @param col			column in which to set
   * @param domain		 
   */
  def setVar(tab: ModelTable, row : ModelRow, col : ModelColumn, domain : HashSet[ModelElement]) : Unit = {
    if (!tablesList.contains(tab)) {
      TableDataModel.logger.error("table " + tab + " is not contained in table list")
      throw new IllegalArgumentException("table " + tab + " is not contained in table list")
    }
    else if (!rowsList.contains(row)) {
      TableDataModel.logger.error("row " + row + " is not contained in row list")
      throw new IllegalArgumentException("row " + row + " is not contained in row list")
    }
    else if (!columnsList.contains(col)) {
      TableDataModel.logger.error("column " + col + " is not contained in column list")
      throw new IllegalArgumentException("column " + col + " is not contained in column list")
    }
    else {
      keyVarsMap.get(new Key(tab, row, col)) match {
        case None =>
	        val newVar = new ModelVar
		    keyVarsMap.put(new Key(tab, row, col), newVar)
		    dataModel.setVarDomain(newVar, domain)  
        case Some(v) =>
          dataModel.setVarDomain(v, domain)
      }
        
    
    }
  }
  
  
  /**
   * set var using table, row, column index
   * @param tab		table index
   * @param row		row index
   * @param col		col index
   * @param domain	hashset of modelelement

   */
  def setVar(tab: Int, row : Int, col : Int, domain : HashSet[ModelElement]) : Unit = {
    if (tab < 0 || tab >= tablesList.length) throw new IllegalArgumentException("index for table is out of bounds")
    if (row < 0 || row >= rowsList.length) throw new IllegalArgumentException("index for row is out of bounds")
    if (col < 0 || col >= columnsList.length) throw new IllegalArgumentException("index for column is out of bounds")
    setVar(tablesList(tab), rowsList(row), columnsList(col), domain)
  }
  

  /**
   * get var's domain given table, row, column
   * @param t			the table
   * @param r			the row
   * @param c			the column
   * @return			the domain if exists
   */
  def getVarDomain(t : ModelTable, r : ModelRow, c : ModelColumn) : Option[HashSet[ModelElement]] = {   
    getVar(t, r, c) match {
      case None => None
      case Some(v) => dataModel.getVarDomain(v) 
    }
  }
  
  /**
   * get var given table, row, column
   * @param t			the table
   * @param r			the row
   * @param c			the column
   * @return			the var if exists
   */
  def getVar(t : ModelTable, r : ModelRow, c : ModelColumn) : Option[ModelVar] = {
    keyVarsMap.get(new Key(t, r, c))
  }

  
  /**
   * useful for filling in dimensions with variables
   * @param dim				the new dimension
   * @param defaultDomain	the new domain to fill in vars
   */
  private def fillVars(dim : ModelDimension, defaultDomain : HashSet[ModelElement]) = {
    dim match {
      // fill for each row / table for new column
      case (c : ModelColumn) =>
          TableDataModel.logger.debug("filling vars for added column: " + c)
	      rowsList./:(){ (_, row) => 
	        tablesList./:(){ (_, tab) =>
	          keyVarsMap.get(new Key(tab, row, c)).getOrElse(
	              setVar(tab, row, c, defaultDomain)
	          )
	        }
	      }
      // fill for each column / table for new row
      case (r : ModelRow) =>
        	TableDataModel.logger.debug("filling vars for added row: " + r)
            columnsList./:(){ (_, col) =>
		        tablesList./:(){ (_, tab) =>
		          keyVarsMap.get(new Key(tab, r, col)).getOrElse(
		              setVar(tab, r, col, defaultDomain)
		          )
		        }
		    }
      // fill for each row / column for new table
      case (t : ModelTable) =>
        	TableDataModel.logger.debug("filling vars for added table: " + t)
            columnsList./:(){ (_, col) =>
		      rowsList./:(){ (_, row) => 
		          keyVarsMap.get(new Key(t, row, col)).getOrElse(
		              setVar(t, row, col, defaultDomain)
		          )
		      }
		    }
    }
  }

  
  /**
   * add column to model
   * @param col				the column to add
   * @param defaultDomain	the domain to copy
   */
  def addColumn(col : ModelColumn, defaultDomain : HashSet[ModelElement]) = {
    if (columnsList.contains(col)) {
      TableDataModel.logger.error("Column " + col + " is already contained in model")
      throw new IllegalArgumentException("Column is already contained")
    }
    else {
	    columnsList = columnsList:+ col
	    fillVars(col, defaultDomain)      
    }
  }

  /**
   * add column to model
   * @param col				the column to add
   * @param index			the index of where to add
   * @param defaultDomain	the domain to copy
   */
  def addColumn(col : ModelColumn, index : Int, defaultDomain : HashSet[ModelElement]) = {
    if (columnsList.contains(col)) {
      TableDataModel.logger.error("Column " + col + " is already contained in model")
      throw new IllegalArgumentException("Column is already contained")
    }
    else if (index < 0 || index >= columnsList.length) {
      throw new IllegalArgumentException("index out of bounds: requested location at " + index + 
          " and length of columns is " + columnsList.length )
      TableDataModel.logger.error("index of " + index + " is out of bounds for columns list size of " + columnsList.length)
    }
    else {
	    columnsList = columnsList:+ col
	    fillVars(col, defaultDomain)      
    }    
  }
  

  /**
   * remove a column
   * @param col		the column to remove
   */
  def removeColumn(col : ModelColumn) = {
    if (!columnsList.contains(col)) {
      TableDataModel.logger.error("Column " + col + " is not contained in model for removal")
      throw new IllegalArgumentException("Column is not in model")
    }
    else 
      columnsList = columnsList.:\[List[ModelColumn]](Nil){
    	(curCol, lst) => if (col eq curCol) lst else curCol::lst
      }
  }
  

  /**
   * add row to model
   * @param row				the row to add
   * @param defaultDomain	the domain to copy
   */
  def addRow(row : ModelRow, defaultDomain : HashSet[ModelElement]) = {
    if (rowsList.contains(row)) {
      TableDataModel.logger.error("Row " + row + " is already contained in model")
      throw new IllegalArgumentException("Row is already contained")
    }
    else {
	    rowsList = rowsList:+ row
	    fillVars(row, defaultDomain)      
    }
  }

  /**
   * add row to model
   * @param row				the row to add
   * @param index			the index of where to add
   * @param defaultDomain	the domain to copy
   */
  def addRow(row : ModelRow, index : Int, defaultDomain : HashSet[ModelElement]) = {
    if (rowsList.contains(row)) {
      TableDataModel.logger.error("Row " + row + " is already contained in model")
      throw new IllegalArgumentException("Row is already contained")
    }
    else if (index < 0 || index >= rowsList.length) {
      TableDataModel.logger.error("index of " + index + " is out of bounds for row length of " + rowsList.length)
      throw new IllegalArgumentException("index out of bounds: requested location at " + index + 
          " and length of row is " + rowsList.length ) 
    }
    else {
	    rowsList = rowsList:+ row
	    fillVars(row, defaultDomain)      
    }    
  }
  

  /**
   * remove a row
   * @param row		the row to remove
   */  
  def removeRow(row : ModelRow) = {
    if (!rowsList.contains(row)) {
      TableDataModel.logger.error("row " + row + " is not in the model")
      throw new IllegalArgumentException("Row is not in model")
    }
    else 
      rowsList = rowsList.:\[List[ModelRow]](Nil){
    	(curRow, lst) => if (row eq curRow) lst else curRow::lst
      }
  }
  

  /**
   * add table to model
   * @param table			the table to add
   * @param defaultDomain	the domain to copy
   */
  def addTable(table : ModelTable, defaultDomain : HashSet[ModelElement]) = {
    if (tablesList.contains(table)) {
      TableDataModel.logger.error("table " + table + " is already contained in model")
      throw new IllegalArgumentException("Table is already contained")
    }
    else {
	    tablesList = tablesList:+ table
	    fillVars(table, defaultDomain)      
    }
  }

  /**
   * add table to model
   * @param table			the table to add
   * @param index			the index of where to add
   * @param defaultDomain	the domain to copy
   */
  def addTable(table : ModelTable, index : Int, defaultDomain : HashSet[ModelElement]) = {
    if (tablesList.contains(table)) {
      TableDataModel.logger.error("table " + table + " is already contained in model")
      throw new IllegalArgumentException("Table is already contained")
    }
    else if (index < 0 || index >= tablesList.length) {
      TableDataModel.logger.error("index " + index + " is out of bounds for tables list of " + tablesList)
      throw new IllegalArgumentException("index out of bounds: requested location at " + index + 
          " and length of table is " + tablesList.length )
    }
    else {
	    tablesList = tablesList:+ table
	    fillVars(table, defaultDomain)      
    }    
  }
  

  /**
   * remove a table
   * @param table		the table to remove
   */   
  def removeTable(table : ModelTable) = {
    if (!tablesList.contains(table)) {
      TableDataModel.logger.error("table " + table + " is not contained in model")
      throw new IllegalArgumentException("Table is not in model")
    }
    else 
      tablesList = tablesList.:\[List[ModelTable]](Nil){
    	(curTable, lst) => if (table eq curTable) lst else curTable::lst
      }
  }
  
  /**
   * gets Results given data model
   */
  def getResults : Results = {
    DataModel.getResults(dataModel.getAllVarsDomain, getCSPConstraintMapping,
      getMinScore, getExhaustive, getTieredScore)
  }
  
  def getSearcherObj : Option[Searcher] = DataModel.getSearcherObj
  

  
  /**
   * gets list of modelvars given columns, rows, and tables as inclusive range
   * @param t1			the table
   * @param t2			the other table
   * @param r1			the row
   * @param r2			the other row
   * @param c1 			the column
   * @param c2			the other column
   * 
   * @return			the potential list of variables
   */
  def getRange(t1 : ModelTable, t2 : ModelTable, r1 : ModelRow, r2 : ModelRow, 
      c1 : ModelColumn, c2 : ModelColumn) : Option[List[ModelVar]] = {
    (Utils.getIndex(columnsList, c1), Utils.getIndex(columnsList, c2), Utils.getIndex(rowsList, r1), Utils.getIndex(rowsList, r2), 
        Utils.getIndex(tablesList, t1), Utils.getIndex(tablesList, t2)) match {
      //need to have all indexes for this to work
      case (Some(c1index), Some(c2index), Some(r1index), Some(r2index), Some(t1index), Some(t2index)) =>
      	val colSets = new HashSet++
      		Utils.getSublist(columnsList, Math.min(c1index, c2index), Math.max(c1index, c2index))
        val rowSets = new HashSet++
      		Utils.getSublist(rowsList, Math.min(r1index, r2index), Math.max(r1index, r2index))
        val tableSets =  new HashSet++
      		Utils.getSublist(tablesList, Math.min(t1index, t2index), Math.max(t1index, t2index))
      	
      		
        Some(keyVarsMap.:\[List[ModelVar]](Nil) {
          (entry, lst) =>
            var (key, v) = entry
            if (colSets.contains(key.c) && rowSets.contains(key.r) && tableSets.contains(key.t))
              v::lst
              else lst
        })
      	/*Some(
      	    tableSets.:\[List[ModelVar]](Nil){ (tbl, lst) =>
      	    	rowSets.:\[List[ModelVar]](Nil){ (row, lst) =>
      	    		colSets.:\[List[ModelVar]](Nil){ (col, lst) =>
      	    			keyVarsMap.get(new Key(tbl, row, col)).get::lst
      	    		}
      	    	}
      	    }
      	)*/
        
      //again need to have all indexes for this to work
      case _ => None
    }
  }
}


/**
 * abstract class representing dimension
 */
abstract class ModelDimension extends NameLabel {
  //object equality over structural...
  override def equals(other : Any) = {
    try {
    	(other.asInstanceOf[AnyRef] eq this.asInstanceOf[AnyRef])
    }
    catch {
      case e : Throwable => false
    }
  }
}

/**
 * represents a table dimension
 */
case class ModelTable() extends ModelDimension {
  override def toString = {
    "Table: " + this.name
  }
}

/**
 * represents a row dimension
 */
case class ModelRow() extends ModelDimension {
    override def toString = {
    "Row: " + this.name
  }
}

/**
 * represents a column dimension
 */
case class ModelColumn() extends ModelDimension {
    override def toString = {
    "Column: " + this.name
  }
}