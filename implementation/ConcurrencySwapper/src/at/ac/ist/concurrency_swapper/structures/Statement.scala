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

import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge
import at.ac.ist.concurrency_swapper.helpers.PartialOrder
import at.ac.ist.concurrency_swapper.translation.{Up, Down, TranslationChain}

class Statement(edge:CFAEdge) extends Structure {

  def myClone():Statement = {
    val stmt = new Statement(edge)
    stmt.number = number
    stmt
  }

  def getUsedVariables() = TranslationChain.usedVariables(edge)

  def getChangedVariables() = TranslationChain.changedVariables(edge)

  def getFormula() = TranslationChain.getFormula(this)

  private var parent:Structure = null
  def getParent() = parent
  def setParent(parent:Structure) = {this.parent = parent}

  def getEdge = edge

  def sort(order: PartialOrder[Int]) {//do nothing
  }

  def printForPoirot(writeString: (String) => Unit, writeStatement: (String, CFAEdge, Statement) => Unit) {
    val edges = TranslationChain.translatePoirot(edge,this).map(x => (x._1,getTabs + x._2))
    for (e<-edges)
      writeStatement(e._2, e._1, this)
  }

  def print(writeString: (String) => Unit, writeStatement: (String, Statement) => Unit) {
    writeStatement(getTabs + TranslationChain.printEdge(this), this)
  }

  def processAllStructures(processor:(Structure,List[Structure])=>Boolean) {}

  def getBlockSize(): Int = 1

  override def isLock():Boolean = {
    Down.accepts(edge) || Up.accepts(edge)
  }
}
