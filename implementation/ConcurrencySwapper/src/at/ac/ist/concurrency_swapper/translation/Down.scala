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

package at.ac.ist.concurrency_swapper.translation

import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge
import at.ac.ist.concurrency_swapper.helpers.{VariableAnalysisResult, SomeVars, ExpressionHelpers}
import org.sosy_lab.cpachecker.cfa.ast.IASTBinaryExpression.BinaryOperator
import org.sosy_lab.cpachecker.cfa.ast.{IASTExpression, IASTBinaryExpression}
import org.sosy_lab.cpachecker.cfa.ast.IASTUnaryExpression.UnaryOperator
import at.ac.ist.concurrency_swapper.structures.Statement
import collection.mutable.ListBuffer

object Down extends IntermediateTranslation with VariableAnalysis {
  var deadlockAnalysis = false;

  def accepts(edge: CFAEdge):Boolean = {
    isSem(edge) || isLock(edge)
  }

  def isSem(edge:CFAEdge):Boolean = {
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => false
      case Some((name,_)) => name == "down"
    }
  }

  def isLock(edge:CFAEdge):Boolean = {
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => false
      case Some((name,_)) => name == "lock"
    }
  }

  def translate(edge: CFAEdge,originalStatement:Statement) : List[CFAEdge] = {
    translate(edge, false,originalStatement)
  }

  override def translateForFormula(edge: CFAEdge,originalStatement:Statement) : List[CFAEdge] = {
    translate(edge, true,originalStatement)
  }

  private def addToInitList(name:String,originalStatement:Statement) = {
    if (originalStatement != null) {
      val p = originalStatement.getProgramLevel()
      p.declarationsForPoirot += name -> 0
    }
  }

  private def translate(edge:CFAEdge, forFormula:Boolean,originalStatement:Statement):List[CFAEdge] = {
    val threads = if (originalStatement==null) Set.empty else originalStatement.getProgramLevel().getThreadNames
    val locks = if (originalStatement==null) Set.empty else originalStatement.getProgramLevel().getLockNames
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => null
      case Some((_,arg1)) =>
        ExpressionHelpers.getName(arg1) match {
          case None => null
          case Some(name) =>
            val functionName = ExpressionHelpers.getFunctionName(edge)
            val otherLocks = locks.filterNot(_==name)
            val thead_id = ExpressionHelpers.makeName("thread_id",false)
            val ourLockBusy = ExpressionHelpers.makeBinaryOperation(ExpressionHelpers.makeName(name),ExpressionHelpers.makeIntConst(0),BinaryOperator.NOT_EQUALS)
            val waitedge = new ListBuffer[CFAEdge]
            for (l<-otherLocks){
              // let's see if we hold a lock the other guy is waiting for
              val otherLock = ExpressionHelpers.makeBinaryOperation(ExpressionHelpers.makeName(l),thead_id,BinaryOperator.EQUALS)
              // check if the other thread is waiting for it
              val waitingFor = ExpressionHelpers.makeBinaryOperation(ExpressionHelpers.makeName(l+"_waiting"),ExpressionHelpers.makeName(name),BinaryOperator.EQUALS)
              addToInitList(l+"_waiting", originalStatement)
              val waitexpression = ExpressionHelpers.makeBinaryOperation(otherLock, waitingFor, BinaryOperator.LOGICAL_AND)
              val combined = ExpressionHelpers.makeBinaryOperation(waitexpression, ourLockBusy,BinaryOperator.LOGICAL_AND)
              val e = ExpressionHelpers.makeAssertEdgeT(ExpressionHelpers.makeUnaryOperation(combined, UnaryOperator.NOT),functionName)
              e.setOtherLock(l)
              waitedge += e
            }
            val edge0 = ExpressionHelpers.makeAssignmentEdge(name + "_waiting", thead_id, functionName)
            addToInitList(name + "_waiting", originalStatement)
            val edgeB = ExpressionHelpers.makeFunctionEdge("atomicStart", List(), functionName)
            val edge2 = ExpressionHelpers.makeAssumeEdge(
              ExpressionHelpers.makeBinaryOperation(ExpressionHelpers.makeName(name),ExpressionHelpers.makeIntConst(0),BinaryOperator.EQUALS), functionName)
            val edge3 = ExpressionHelpers.makeAssignmentEdge(name, thead_id, functionName)
            val edge4 = ExpressionHelpers.makeAssignmentEdge(name + "_waiting", ExpressionHelpers.makeIntConst(0), functionName)
            val edgeE = ExpressionHelpers.makeFunctionEdge("atomicEnd", List(), functionName)
            if (forFormula || !deadlockAnalysis || isSem(edge))
              List(edgeB,edge2, edge3, edgeE)
            else
              List(edge0, edgeB) ++ waitedge.result ++ List(edgeE, edgeB, edge2, edge3, edge4, edgeE)
        }
    }
  }

  def changedVariables(edge: CFAEdge):VariableAnalysisResult = {
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => null
      case Some((_,arg1)) =>
        ExpressionHelpers.getName(arg1) match {
          case None => null
          case Some(name) =>
            return SomeVars(Set(name))
        }
    }
  }

  def usedVariables(edge: CFAEdge) = SomeVars(Set.empty):VariableAnalysisResult
}
