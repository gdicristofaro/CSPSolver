package gregd.cspsolve.excelIO

import org.apache.poi.xssf.usermodel.XSSFSheet
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import java.io.File
import java.io.FileInputStream
import org.apache.poi.ss.usermodel.Row
import org.apache.poi.ss.formula.functions.Column
import org.apache.poi.ss.usermodel.Cell
import java.io.FileNotFoundException
import java.io.IOException
import scala.collection.mutable.HashMap
import gregd.cspsolve.model.ModelElement
import com.google.common.collect.BiMap
import com.google.common.collect.HashBiMap
import gregd.cspsolve.model.TableDataModel
import scala.collection.mutable.HashSet
import gregd.cspsolve.model.ModelVar
import gregd.cspsolve.model.ModelTable
import gregd.cspsolve.model.ModelRow
import gregd.cspsolve.model.ModelColumn
import gregd.cspsolve.model.ModelConstraint
import gregd.cspsolve.constraint.PairEither
import gregd.cspsolve.constraint.MutuallyExclusive
import gregd.cspsolve.solve.CSPConstraint
import gregd.cspsolve.constraintArgParser.ArgConverter
import gregd.cspsolve.constraintArgParser.StringConverter
import gregd.cspsolve.log.MyLogger
import gregd.cspsolve.constraintArgParser.ArgTypeConverter
import gregd.cspsolve.solve.CSPVar
import gregd.cspsolve.solve.CSPElement
import gregd.cspsolve.model.Results
import gregd.cspsolve.model.Solution
import gregd.cspsolve.model.Error
import gregd.cspsolve.utils.Utils
import java.io.FileOutputStream
import gregd.cspsolve.utils.Constants
import gregd.cspsolve.constraintarg.ListConstraintArg
import gregd.cspsolve.constraintarg.VarArg


object Application {
	def main(args: Array[String]): Unit = {
	    if (args.length != 2) {
	      println("there should be two arguments: the input file and the output file")
	      return
	    }
	    
	    val input = args(0)
	    val output = args(1)
	    
			val excelImporter = new ExcelIO(
					new FileInputStream(
							new File(input)))

					val checkit = new Runnable {
				def run() {
					try {
						Thread.sleep(5000);  
					}
					catch {
					case e : InterruptedException => ()
					}

					excelImporter.getSearcherObj match {
					case None => ()
					case Some(o) =>
					println("branch percentage: " + o.getStatus.branchPercentage + ";   depth: " 
							+ o.getStatus.depth + " of " + o.getStatus.depthLimit)

					run()
					}
				}  
			}   

			val checkitThread = new Thread(checkit)

					val runit = new Runnable {
				def run() {
					val results = excelImporter.getResults
							excelImporter.exportResults(results, true).write(
									new FileOutputStream(output))
							checkitThread.interrupt()
				}
			}

			new Thread(runit).start()
			checkitThread.start() 
	}    
}

object ExcelIO {
	val logger = new MyLogger("ExcelIO")

			/**
			 * holds on to all available constraints
			 */
			def getConstraintByName(name : String) : Option[ModelConstraint] = {
					try {
						Some ((Constants.availableConstraints.filter(cons => (cons.getName == name))(0)).newInstance)
					}
					catch {
					case e : Throwable => None
					}
	}
}


class ExcelIO(xlsxDoc : FileInputStream, colOffSet : Int, rowOffSet : Int, 
		delim : String, trim : Boolean, ConstraintSheet : String, OmittedSheets : Array[String]) {




	/**
	 * alternate constructor with some defaults
	 * @param xlsxDoc			the excel document
	 */
	def this(xlsxDoc : FileInputStream) = this(xlsxDoc, 1, 1, ",", true, "ConstraintSheet", new Array[String](0))

			//the data model
			val dataModel : TableDataModel = new TableDataModel

			//a mapping of strings to elements
			val elementStringMap : BiMap[String, ModelElement] = HashBiMap.create[String,ModelElement]

					//a mapping of strings to elements
					val variableStringMap : BiMap[String, ModelVar] = HashBiMap.create[String,ModelVar]

							//column indexrow
							val colIndex : HashMap[ModelColumn, Int] = new HashMap[ModelColumn, Int]

									//row index	
									val rowIndex : HashMap[ModelRow, Int] = new HashMap[ModelRow, Int]

											//cspconstraint map
											var cspMap = new HashMap[Int, ModelConstraint]


													val stringconvert = new StringConverter(
															new ArgTypeConverter[String, CSPVar]({string => variableStringMap.get(string)}, 
																	{v => variableStringMap.inverse.get(v)}),
															new ArgTypeConverter[String, CSPElement]({string => elementStringMap.get(string)}, 
																	{e => elementStringMap.inverse.get(e)}),
															new ArgTypeConverter[String, ModelConstraint]({string => ExcelIO.getConstraintByName(string).get}, 
																	{c => c.getName}))


													try {
														val workbook = new XSSFWorkbook(xlsxDoc);

														//get worksheets as list
														val allSheets = Utils.listFromIterator(workbook.iterator)
																val worksheets = allSheets.filterNot{
															sheet => OmittedSheets.contains(sheet.getSheetName) || sheet.getSheetName == ConstraintSheet}

														val constraintSheet = allSheets.filter{(sheet) => sheet.getSheetName == ConstraintSheet}(0)

																processVars(worksheets, colOffSet, rowOffSet, delim, trim)
																cspMap = parseConstraints(constraintSheet)

																cspMap.map{
															(entry) =>
															val (_, c) = entry
															dataModel.addConstraint(c)
														}

													} catch {
													case e : Throwable => 
													ExcelIO.logger.error("There was an error parsing the excel workbook: " + e.getMessage() + 
															"\nwith stack trace:\n" + e.getStackTraceString)
													}

							def getSearcherObj = dataModel.getSearcherObj

									def getResults = dataModel.getResults



									/**
									 * process Variables and sets local variables appropriately
									 * @param worksheets		the worksheets as a list
									 * @param colOffSet			which column to start with
									 * @param rowOffSet			which row to start with
									 * @param delim				the delimiter by which to split elements
									 * @param trim				whether or not to trim the string that represents the element
									 */
									def processVars(worksheets : List[XSSFSheet], colOffSet : Int, rowOffSet : Int, delim : String, trim : Boolean) = {
											//for each sheet number
											for (sheetNum <- 0 to (worksheets.length - 1)) {
												val newTable = new ModelTable
														val tablename = worksheets(sheetNum).getSheetName
														newTable.name(tablename)
														ExcelIO.logger.debug("adding table with name " + tablename + " to data model")
														dataModel.addTable(newTable, new HashSet[ModelElement])

														//for each row number
														for (rowNum <- rowOffSet to worksheets(sheetNum).getLastRowNum) {
															val r = worksheets(sheetNum).getRow(rowNum)
																	//if not null, go through row looking for values
																	if (r != null) {
																		//does row already exist in model?
																		val curRow = 
																				if (dataModel.getRows.length > rowNum) dataModel.getRows(rowNum)
																				else {
																					ExcelIO.logger.debug("creating new row for " + rowNum)
																					val newRow = new ModelRow
																					newRow.name(r.getCell(0, Row.CREATE_NULL_AS_BLANK).getStringCellValue);
																					rowIndex += (newRow -> rowNum);
																					dataModel.addRow(newRow, new HashSet[ModelElement]);
																					newRow
																				}


																		val lastColNum = r.getLastCellNum
																				//for each column number
																				for (colNum <- colOffSet to (lastColNum - 1)) {
																					//does row already exist in model?

																					val optionCol = dataModel.getCols.:\[Option[ModelColumn]](None){
																						(col, prev) => 
																						prev match {
																						case Some(_) => prev
																						case None =>
																						if (col.name == colNum.toString) Some(col)
																						else None
																						}
																					}
																					//get column or create new one
																					val curCol = optionCol.getOrElse{
																						ExcelIO.logger.debug("creating new column for " + colNum)

																						val newCol = new ModelColumn
																						newCol.name(colNum.toString);
																						colIndex += (newCol -> colNum);
																						dataModel.addColumn(newCol, new HashSet[ModelElement]);
																						newCol
																					}

																					//get cell
																					val cellVal = r.getCell(colNum, Row.CREATE_NULL_AS_BLANK).getStringCellValue

																							variableStringMap.put(positionToString(newTable.name, rowNum.toString, curCol.name), 
																									dataModel.getVar(newTable, curRow, curCol).get)

																							val domain = elementsFromString(cellVal, delim, trim)

																							ExcelIO.logger.info("setting cell at " + tablename + " - " + rowNum + " - " + colNum + " to domain: " + domain.:\("")((element, str) => str + elementStringMap.inverse.get(element) + ", "))

																							//set domain at table, row, column
																							dataModel.setVar(newTable, curRow, curCol, 
																									new HashSet[ModelElement] ++ domain) 
																				}
																	}
														} 
											}
							}


							/**
							 *  returns elements given a string, delimiter, and whether or not to trim
							 *  if element is already contained, returns that element, otherwise creates new one
							 *  @param domain			the string representing the domain
							 *  @param delimiter		the string representing the delimiter
							 *  @param trim			whether or not to trim
							 *  @return 				the array of elements to return
							 */
							def elementsFromString(domain : String, delimiter : String, trim: Boolean) : Array[ModelElement] ={
									Utils.splitString(domain, delimiter, trim).map{
										(str) => 
										val mapCheck = elementStringMap.get(str)
										if (mapCheck != null) {
											ExcelIO.logger.debug("element of name " + str + " already contained")
											mapCheck
										}
										else {
											ExcelIO.logger.info("creating new element with name " + str)
											val newEl = new ModelElement
											elementStringMap.put(str, newEl);
											newEl
										}
									}
							}


							//format will be first col is constraint, next col is priority (or empty if 1), following cols are arguments
							//1 row offset
							// row index and list of constraints
							def parseConstraints(constraintSheet : XSSFSheet) : HashMap[Int, ModelConstraint] = {

									//for each row number
									def parseConstraint(rowNum : Int, rowLimit : Int, constraintSheet : XSSFSheet) : HashMap[Int, ModelConstraint] = {
											ExcelIO.logger.debug("working with constraint at row " + rowNum)
											if (rowNum > rowLimit) new HashMap[Int, ModelConstraint]
													else {
														val r = constraintSheet.getRow(rowNum)
																//if not null, go through row looking for values
																if (r == null) {
																	ExcelIO.logger.debug("skipping constraint in row " + rowNum + " because it is empty")
																	parseConstraint(rowNum + 1, rowLimit, constraintSheet)
																}
																else {
																	val consName = r.getCell(1, Row.CREATE_NULL_AS_BLANK).getStringCellValue
																			ExcelIO.logger.debug("constraint parsed to: " + consName + ".  finding...")
																			// get constraint object
																			ExcelIO.getConstraintByName(consName) match {
																		// if you can't get constraint, continue to parse next
																			case None => 
																			ExcelIO.logger.warn("unable to find constraint by name  " + consName)
																			parseConstraint(rowNum + 1, rowLimit, constraintSheet)
																			case Some(newCons) =>
																			//try {
																			val args = newCons.getArgs

																			ExcelIO.logger.debug("working with constraint " + newCons.getName + " with arg size " + args.length)

																			//go through args
																			for (arrNum <- 0 to args.length - 1) {
																				ExcelIO.logger.debug("working with arg " + arrNum)
																				val cell = r.getCell(arrNum + 3, Row.CREATE_NULL_AS_BLANK)
																				val argVal = 
																				if (cell.getCellType == 0) cell.getNumericCellValue.toString
																				else cell.getStringCellValue

																				ExcelIO.logger.debug("for Constraint in row " + rowNum + ", setting arg at col " + (arrNum +3) + " to " + argVal)

																				//TODO this will break very quickly if not CSPVar
																				(args(arrNum), argVal.contains('/')) match {
																				case (lstArg: ListConstraintArg[_], true) =>
																				var tokens = argVal.split('/')
																				if (trim) tokens = tokens.map((str) => str.trim)
																				val first = dataModel.getVarPosition(variableStringMap.get(tokens(0))).get
																				val second = dataModel.getVarPosition(variableStringMap.get(tokens(1))).get
																				val range = dataModel.getRange(first._1, second._1, first._2, second._2, first._3, second._3)

																				ExcelIO.logger.info("range is: " + range.:\("")((v, str) => str + v + "; "))
																				range.get.map{
																					(v) =>
																					val curItem = lstArg.addListItem.get
																					curItem.asInstanceOf[VarArg].setValue(v)
																				}
																				case _ => stringconvert.setValue(argVal, args(arrNum))
																				}
																			}


																			//set priority
																			val pri = r.getCell(2, Row.CREATE_NULL_AS_BLANK).getNumericCellValue
																					/*val pri = 
    		    	  if (priString == "") 1.0
    		    	  else java.lang.Double.parseDouble(priString)*/

																					if (pri < 1.0) {
																						newCons.setPriority(pri)
																						newCons.setAbsolute(false)
																					}

																			parseConstraint(rowNum + 1, rowLimit, constraintSheet) += (rowNum -> newCons)
																					/*}
    		    catch {
    		      case e : Throwable => 
    		        ExcelIO.logger.error("problem parsing constraint at row " 
    		            + rowNum + " with error: " + e.getMessage + "\nstacktrace: \n" + e.getStackTraceString)
    		        throw new IllegalStateException("there was a problem parsing constraint at row " + rowNum)
    		    }*/
																	}
																}
													}
									} 

									parseConstraint(1, constraintSheet.getLastRowNum, constraintSheet)
							}



							/**
							 * creates a string from position
							 * @param t		the modelTable
							 * @param r		the modelRow
							 * @param c		the modelColumn
							 * @return		the string of position
							 */
							def positionToString(t : String, r : String, c : String) : String = 
									t + "-" + r + "-" + c




									def exportResults(results : Results, resultbyChoice : Boolean) : XSSFWorkbook = {
											val workbook = new XSSFWorkbook

													ExcelIO.logger.debug("creating new workbook from results ")

													results match {
													case sol : Solution =>
													try {
														dataModel.getTables.map {
															table =>
															val worksheet = workbook.createSheet(table.name)
															ExcelIO.logger.debug("creating worksheet at " + table.name)

															val firstRow = worksheet.createRow(0);

															for (x <- 0 to dataModel.getCols.length - 1)
																firstRow.createCell(x + 1).setCellValue(dataModel.getCols(x).name)


																dataModel.getRows.map {
																row =>
																val cellRow = worksheet.createRow(rowIndex.get(row).get)
																cellRow.createCell(0).setCellValue(row.name)

																dataModel.getCols.map {
																	col =>
																	val cell = cellRow.createCell(colIndex.get(col).get)
																	val cspvar = dataModel.getVar(table, row, col).get
																	val cellValue = sol.solution.get(cspvar).get
																	val stringVal = elementStringMap.inverse.get(cellValue)

																	ExcelIO.logger.debug("creating cell at " +  table.name + " - " + row + " - " + col + " to " + stringVal)

																	cell.setCellValue(stringVal)
																}
															}
														}

														if (resultbyChoice)
															exportResultsByChoice(results, workbook)

															//take care of constraint data
															val consSat = workbook.createSheet("Constraint Satisfaction")
															val row = consSat.createRow(0)
															row.createCell(0).setCellValue("Constraint")
															row.createCell(1).setCellValue("Satisfaction")

															cspMap.map{
															(entry) =>
															val (index, constraint) = entry
															val thisRow = consSat.createRow(index)
															thisRow.createCell(0).setCellValue(index)

															thisRow.createCell(1).setCellValue(sol.constraintScores.get(constraint).get.toString)
														}
														workbook 
													}
													catch {
													case e : Throwable =>
													ExcelIO.logger.error("problem in setting results data: " +  e.getMessage() + " with stacktrace " + e.getStackTraceString)
													throw new IllegalStateException("problem in setting results data: "
															+  e.getMessage() + " with stacktrace " + e.getStackTraceString)
													}
													case err : gregd.cspsolve.model.Error => 
													val position : String = 
													try {
														val triple = dataModel.getVarPosition(err.errorVar.asInstanceOf[ModelVar]).get
																"Error in " + positionToString(triple._1.name, triple._2.name, triple._3.name)
													}
													catch {
													case e : Throwable => "problem..."
													}

													workbook.createSheet("Error").createRow(1).createCell(0).setCellValue(position);
													ExcelIO.logger.info("results dictate error for cell at " + position)
													workbook
													case _ =>
													ExcelIO.logger.error("unintended result?  result does not match solution or error")
													throw new IllegalStateException("unintended result.  result does not match solution or error")
											}

							}


							def exportResultsByChoice(results : Results, workbook : XSSFWorkbook) = {
									//val workbook = new XSSFWorkbook

									ExcelIO.logger.debug("creating new workbook from results ")

									results match {
									case sol : Solution =>
									try {
										dataModel.getTables.map {
											table =>
											val worksheet = workbook.createSheet("By Element " + table.name)
											ExcelIO.logger.debug("creating worksheet at by element")


											//TODO fix
											val choiceToRows = new HashMap[CSPElement, Array[ModelRow]];

											dataModel.getRows.map {
												(r) =>
												dataModel.getCols.map {
													(c) =>
													val cspvar = dataModel.getVar(table, r, c).get
													val cellValue = sol.solution.get(cspvar).get

													if(choiceToRows.contains(cellValue))
														(choiceToRows.get(cellValue).get)(dataModel.getCols.indexOf(c)) = r
														else {
															choiceToRows.put(cellValue, new Array[ModelRow](dataModel.getCols.size))
															(choiceToRows.get(cellValue).get)(dataModel.getCols.indexOf(c)) = r
														}
												}
											}

											choiceToRows.:\(1) {
												(entry, num) =>
												val (element, arr) = entry
												val thisRow = worksheet.createRow(num)

												//set cell values
												thisRow.createCell(0).setCellValue(
														elementStringMap.inverse.get(element))

												for (x <- 1 to arr.length)
													if (arr(x-1) != null)
														thisRow.createCell(x).setCellValue(arr(x-1).name)
														num+1
											}

										}
									}
									catch {
									case e : Throwable =>
									ExcelIO.logger.error("problem in setting results data: " +  e.getMessage() + " with stacktrace " + e.getStackTraceString)
									throw new IllegalStateException("problem in setting results data: "
											+  e.getMessage() + " with stacktrace " + e.getStackTraceString)
									}
									case err : gregd.cspsolve.model.Error => 
									val position : String = 
									try {
										val triple = dataModel.getVarPosition(err.errorVar.asInstanceOf[ModelVar]).get
												"Error in " + positionToString(triple._1.name, triple._2.name, triple._3.name)
									}
									catch {
									case e : Throwable => "problem..."
									}

									workbook.createSheet("Error").createRow(1).createCell(0).setCellValue(position);
									ExcelIO.logger.info("results dictate error for cell at " + position)
									workbook
									case _ =>
									ExcelIO.logger.error("unintended result?  result does not match solution or error")
									throw new IllegalStateException("unintended result.  result does not match solution or error")
									}

							}
}