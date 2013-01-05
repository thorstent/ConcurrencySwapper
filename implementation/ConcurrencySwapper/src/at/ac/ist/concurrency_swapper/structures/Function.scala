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

import org.sosy_lab.cpachecker.cfa.objectmodel.{CFAFunctionDefinitionNode, CFAEdge}
import at.ac.ist.concurrency_swapper.helpers.{Declaration, VariableAnalysisResult, SomeVars, PartialOrder}
import org.sosy_lab.cpachecker.cfa.objectmodel.c.FunctionDefinitionNode

class Function(name: String, commands: List[Structure], functionDef:CFAFunctionDefinitionNode) extends Structure{
  commands.foreach(_.setParent(this))

  private var parent:Structure = null
  private var commands_ = commands

  private var sortable = true

  def myClone():Function = {
    val fun = new Function(name, commands_.map(_.myClone()), functionDef)
    fun.sortable = this.sortable
    fun.number = number
    return fun
  }

  def setSortable(sortable:Boolean) = {this.sortable = sortable}
  def getSortable() = sortable

  private var threadName = name

  def setThreadName(name:String) = {threadName = name}
  override def getThreadName() = threadName

  def printForPoirot(writeString:String=>Unit,writeStatement:(String,CFAEdge,Statement) => Unit) {
    //writeString("int "+name+"() { int thread_id = corral_getThreadID();\n")
    writeString(functionDef.asInstanceOf[FunctionDefinitionNode].getFunctionDefinition.getRawSignature + " { int thread_id = corral_getThreadID();\n")
    commands_.foreach(_.printForPoirot(writeString, writeStatement))
    writeString("}\n\n")
  }

  def print(writeString:String=>Unit,writeStatement:(String,Statement) => Unit) {
    //writeString("void "+name+"() {\n")
    writeString(functionDef.asInstanceOf[FunctionDefinitionNode].getFunctionDefinition.getRawSignature + " {\n")
    commands_.foreach(_.print(writeString, writeStatement))
    writeString("}\n\n")
  }

  def getUsedVariables() = {
    commands_.map(_.getUsedVariables()).filterNot(_==Declaration).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l)
  }

  def getChangedVariables() = {
    commands_.map(_.getChangedVariables()).filterNot(_==Declaration).foldLeft(SomeVars(Set.empty):VariableAnalysisResult)((s,l)=>s++l)
  }

  def getParent() = parent
  def setParent(parent:Structure) { this.parent = parent }

  def getCommands() = commands_

  def sort(order: PartialOrder[Int]) = {
    if (sortable) {
      commands.foreach(_.sort(order))
      commands_ = order.OrderItems[Structure](commands_, _.getNumber)
    }
  }

  def getName() = name

  def processAllStructures(processor:(Structure,List[Structure])=>Boolean) {
    if (processor(this,commands_))
      commands_.foreach(_.processAllStructures(processor))
  }

  def addCommand(newS:Structure) {
    commands_ ::= newS
    newS.setParent(this)
  }

  def getBlockSize(): Int = commands_.foldLeft(1)((i,s) => i+s.getBlockSize())
}
