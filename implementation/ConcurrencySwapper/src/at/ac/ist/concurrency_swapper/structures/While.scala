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

package at.ac.ist.concurrency_swapper.structures

import org.sosy_lab.cpachecker.cfa.ast.{IASTUnaryExpression, IASTExpression}
import at.ac.ist.concurrency_swapper.helpers.{VariableAnalysisResult, SomeVars, ExpressionHelpers, PartialOrder}
import org.sosy_lab.cpachecker.cfa.ast.IASTUnaryExpression.UnaryOperator
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge

class While(condition:IASTExpression, loop: List[Structure]) extends Structure{
  loop.foreach(_.setParent(this))

  var loop_ = loop

  def myClone():While = {
    val i = new While(condition, loop_.map(_.myClone()))
    i.number = number
    i
  }

  def getUsedVariables() = {
    loop_.map(_.getUsedVariables()).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l) ++
      SomeVars(ExpressionHelpers.getUsedVariables(condition))
  }

  def getChangedVariables() = {
    loop_.map(_.getChangedVariables()).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l)
  }

  private var parent:Structure = null
  def getParent() = parent
  def setParent(parent:Structure) = {this.parent = parent}

  def getLoop() = loop_
  def getCondition() = condition

  def getNegCondition():IASTUnaryExpression = ExpressionHelpers.makeUnaryOperation(condition, UnaryOperator.NOT)

  def sort(order: PartialOrder[Int]) = {
    loop_.foreach(_.sort(order))
    loop_ = order.OrderItems[Structure](loop_, _.getNumber)
  }

  def printForPoirot(writeString: (String) => Unit, writeStatement: (String,CFAEdge, Statement) => Unit) {
    val ifAssumption = new Statement(ExpressionHelpers.makeAssumeEdge(getCondition, getFunctionName))
    val elseAssumption = new Statement(ExpressionHelpers.makeAssumeEdge(getNegCondition, getFunctionName))
    ifAssumption.setParent(this)
    elseAssumption.setParent(this)
    writeString(getTabs + "while ("+getCondition.toASTString+") {\n")
    loop_.foreach(_.printForPoirot(writeString, writeStatement))
    writeString(getTabs + "}\n")
  }

  def print(writeString: (String) => Unit, writeStatement: (String, Statement) => Unit) {
    writeString("while (" + condition.toASTString + "){\n")
    loop_.foreach(_.print(writeString, writeStatement))
    writeString("}\n")
  }

  def processAllStructures(processor:(Structure,List[Structure])=>Boolean) {
    if (processor(this,loop_)){
      loop_.foreach(_.processAllStructures(processor))
    }
  }

  def getBlockSize(): Int = loop_.foldLeft(1)((i,s) => i+s.getBlockSize())
}
