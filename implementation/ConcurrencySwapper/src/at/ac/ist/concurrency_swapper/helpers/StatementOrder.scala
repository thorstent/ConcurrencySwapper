/*
Copyright 2013 IST Austria

This file is part of ConcurrencySwapper.

ConcurrencySwapper is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

ConcurrencySwapper is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

You should have received a copy of the GNU General Public License
  along with ConcurrencySwapper. If not, see <http://www.gnu.org/licenses/>.
  */

package at.ac.ist.concurrency_swapper.helpers

import org.sosy_lab.cpachecker.cfa.objectmodel.{CFAEdge, CFAFunctionDefinitionNode}
import collection.mutable
import at.ac.ist.concurrency_swapper.structures._
import collection.mutable.ListBuffer

object StatementOrder {
  type Statement = at.ac.ist.concurrency_swapper.structures.Statement
  type StmtConstraint = Constraint[Structure]

  private def getPartialOrder(program:Program) : PartialOrder[Int] = {
    val res = new PartialOrder[Int]
    val mb = new mutable.MapBuilder[Int,Structure,Map[Int,Structure]](Map.empty) // we need that for printing

    var bucket = 0
    def processLine(currentStruct:Structure, structures : List[Structure]):Boolean = {
      // we want to ignore not stortable functions
      if (!currentStruct.isInstanceOf[Function] || currentStruct.asInstanceOf[Function].getSortable()) {
        bucket += 1
        var toProcess = structures.reverse
        while (!toProcess.isEmpty) {
          val currentEdge = toProcess.head
          res.AddElement(currentEdge.getNumber, bucket)
          mb += ((currentEdge.getNumber, currentEdge))
          var toProcess2 = toProcess.tail
          while (!toProcess2.isEmpty) {
            val compareEdge = toProcess2.head
            res.AddElement(compareEdge.getNumber, bucket)
            mb += ((compareEdge.getNumber, compareEdge))
            if (dependency(currentEdge, compareEdge)) {
              res.Add(compareEdge.getNumber, currentEdge.getNumber)
            }
            toProcess2 = toProcess2.tail
          }
          toProcess = toProcess.tail
        }
        return true
      }
      else {
        return false
      }
    }

    program.processAllStructures(processLine)

    res.setPrinter(i => i+":" + (mb.result().get(i) match { case Some(x) => x.toString(); case None => "unknown"}))

    return res
  }

  private def dependency(currentStmt: Structure, compareStmt: Structure): Boolean = {
    val changedVariablesCompare: VariableAnalysisResult = compareStmt.getChangedVariables()
    val changedVariablesCurrent: VariableAnalysisResult = currentStmt.getChangedVariables()
    val usedVariablesCompare: VariableAnalysisResult = compareStmt.getUsedVariables()
    val usedVariablesCurrent: VariableAnalysisResult = currentStmt.getUsedVariables()
    if (((changedVariablesCurrent & changedVariablesCompare) != SomeVars(Set.empty)) || ((changedVariablesCurrent & usedVariablesCompare) != SomeVars(Set.empty)) || ((usedVariablesCurrent & changedVariablesCompare) != SomeVars(Set.empty))) {
      return true
    }
    return false
  }
}

object PrintType extends Enumeration {
  type PrintType = Value
  val Normal, Poirot= Value
}

class StatementOrder(threads1 : List[(CFAFunctionDefinitionNode, String)],otherFunctions1 : List[(CFAFunctionDefinitionNode, String)] , originalProgram1:String) {
  private val threads = threads1
  private val otherFunctions = otherFunctions1
  private val originalProgram = originalProgram1
  private var program = PostParser.postParse(threads, otherFunctions, originalProgram)
  private var order = StatementOrder.getPartialOrder(program)

  def isOrdered(stmtNo1 : Int, stmtNo2 : Int) = order.isOrdered(stmtNo1, stmtNo2)

  def this(so: StatementOrder) {
    this(so.threads, so.otherFunctions, so.originalProgram)
    this.program = so.program.myClone()
    this.order = new PartialOrder[Int](so.order)
    PostParser.secondRound(this.program)
  }

  def length = threads.length

  def threadNames() : List[String] = {
    return threads.map(_._2)
  }

  // this one doesn't change the statement order, but gives back modified clones
  def integrate(constraint: List[StatementOrder.StmtConstraint]): List[StatementOrder] = {
    def addOneConstraint(e1: Structure, e2: Structure, so:StatementOrder) : Boolean = {
      // make sure they are on the same level
      if (!Structure.isSameLevel(e1,e2))
        throw new Exception("the structures have no relation to each other")
      try so.order.Add(e2.getNumber, e1.getNumber) catch {case e:Exception => return false}
      return true
    }
    def addOneAtomicSec(f:Function, so:StatementOrder):Boolean = {
      if (!f.getSortable())
        return false // we cannot change this function, no point in placing a lock
      val start = new Statement(ExpressionHelpers.makeFunctionEdge("atomicStart", List(), f.getName()))
      val end = new Statement(ExpressionHelpers.makeFunctionEdge("atomicEnd", List(), f.getName()))
      val f2 = so.program.getFunctions()(f.getName())
      val bucket = so.order.getBucket(f.getCommands()(0).getNumber)
      // add it to the order
      so.order.AddElement(start.getNumber, bucket)
      so.order.AddElement(end.getNumber, bucket)
      // we need to seperate the order into declaration edges and normal edges
      val (decls,rest) = f.getCommands().partition(_.getUsedVariables() == Declaration)
      // and make sure we add the atomic section after declarations
      rest.foreach(s => so.order.Add(start.getNumber,s.getNumber))
      decls.foreach(s => so.order.Add(s.getNumber,start.getNumber))
      f.getCommands().foreach(s => so.order.Add(s.getNumber,end.getNumber))
      f2.addCommand(start)
      f2.addCommand(end)
      return true
    }

    def addConstraint(cl: List[StatementOrder.StmtConstraint]) : List[StatementOrder] = {
      var res = new ListBuffer[StatementOrder]
      for (c <- cl) {
        c match {
          case After(e1, e2) => {
            val so = new StatementOrder(this)
            if (addOneConstraint(e2, e1, so))
              res += so
          }
          case PlaceAtomicSectionFunction(f) => {
            val so = new StatementOrder(this)
            if (addOneAtomicSec(f,so))
              res += so
          }
        }
      }
      res.result()
    }

    return addConstraint(constraint)
  }

  def printProgram(printType:PrintType.PrintType) : (String, Map[Int,List[(CFAEdge,StatementOrder.Statement)]]) = {
    val map = new mutable.MapBuilder[Int, List[(CFAEdge,StatementOrder.Statement)], Map[Int, List[(CFAEdge,StatementOrder.Statement)]]](Map.empty)

    def addToMap(line:Int, newElement:(CFAEdge,StatementOrder.Statement)) = {
      var list:List[(CFAEdge,StatementOrder.Statement)] = List.empty
      if (map.result().contains(line))
        list = map.result()(line) ++ List(newElement)
      else
        list = List(newElement)
      map += ((line, list))
      () // return unit
    }

    program.sort(order)
    val code = if (printType == PrintType.Normal)
      program.print()
    else
      program.printForPoirot(addToMap)

    return (code, map.result())
  }

  def printOrder(folder:String) = {
    order.ExportToDOT(folder + "/order.dot")
  }
}
