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

class If(condition:IASTExpression, then: List[Structure], els: List[Structure]) extends Structure{
  then.foreach(_.setParent(this))
  els.foreach(_.setParent(this))

  var then_ = then
  var else_ = els

  def myClone():If = {
    val i = new If(condition, then_.map(_.myClone()), else_.map(_.myClone()))
    i.number = number
    i
  }

  def getUsedVariables() = {
    then_.map(_.getUsedVariables()).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l) ++
    else_.map(_.getUsedVariables()).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l) ++
      SomeVars(ExpressionHelpers.getUsedVariables(condition))
  }

  def getChangedVariables() = {
    then_.map(_.getChangedVariables()).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l) ++
    else_.map(_.getChangedVariables()).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l)
  }

  private var parent:Structure = null
  def getParent() = parent
  def setParent(parent:Structure) = {this.parent = parent}

  def getThen() = then_
  def getElse() = else_
  def getCondition() = condition

  def getNegCondition():IASTUnaryExpression = ExpressionHelpers.makeUnaryOperation(condition, UnaryOperator.NOT)

  def sort(order: PartialOrder[Int]) = {
    then_.foreach(_.sort(order))
    else_.foreach(_.sort(order))
    then_ = order.OrderItems[Structure](then_, _.getNumber)
    else_ = order.OrderItems[Structure](else_, _.getNumber)
  }

  def printForPoirot(writeString: (String) => Unit, writeStatement: (String,CFAEdge, Statement) => Unit) {
    val ifAssumption = new Statement(ExpressionHelpers.makeAssumeEdge(getCondition, getFunctionName))
    val elseAssumption = new Statement(ExpressionHelpers.makeAssumeEdge(getNegCondition, getFunctionName))
    ifAssumption.setParent(this)
    elseAssumption.setParent(this)
    writeString(getTabs + "if (poirot_nondet()) {\n")
    if (getCondition.toASTString != "nondet") writeStatement(getTabs + "\t__hv_assume (" + getCondition.toASTString + ")", null, ifAssumption)
    then_.foreach(_.printForPoirot(writeString, writeStatement))
    writeString(getTabs + "} else {\n")
    if (getCondition.toASTString != "nondet") writeStatement(getTabs + "\t__hv_assume (" + getNegCondition.toASTString() + ")", null, elseAssumption)
    else_.foreach(_.printForPoirot(writeString, writeStatement))
    writeString(getTabs + "}\n")
  }

  def print(writeString: (String) => Unit, writeStatement: (String, Statement) => Unit) {
    writeString(getTabs + "if (" + condition.toASTString + ") {\n")
    then_.foreach(_.print(writeString, writeStatement))
    if (else_ != List.empty){
      writeString(getTabs + "} else {\n")
      else_.foreach(_.print(writeString, writeStatement))
    }
    writeString(getTabs + "}\n")
  }

  def processAllStructures(processor:(Structure,List[Structure])=>Boolean) {
    if (processor(this,then_))
      if (processor(this,else_)) {
        then_.foreach(_.processAllStructures(processor))
        else_.foreach(_.processAllStructures(processor))
      }
  }

  def getBlockSize(): Int = then_.foldLeft(1)((i,s) => i+s.getBlockSize()) + else_.foldLeft(0)((i,s) => i+s.getBlockSize())
}
