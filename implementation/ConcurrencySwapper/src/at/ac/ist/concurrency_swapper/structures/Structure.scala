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

import at.ac.ist.concurrency_swapper.helpers.{VariableAnalysisResult, PartialOrder}
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge

object Structure {
  def findCommandParent(s1:Structure, s2:Structure):Int = findCommandParent[Structure](s1,s2,_.getParent(),_.getNumber)
  def findCommandParent[T](s1:T, s2:T, getParent:T=>T, getid:T=>Int):Int = {
    var s1i = s1
    var parents:List[Int] = List.empty
    while (getid(s1i) != 0)
    {
      parents ::= getid(s1i)
      s1i = getParent(s1i)
    }

    var s2i = s2
    while (getid(s2i) != 0)
    {
      if (parents.contains(getid(s2i)))
        return getid(s2i)
      s2i = getParent(s2i)
    }
    return 0
  }

  // this function makes sure s1 and s2 are at the same level
  def getSameLevel(s1:Structure, s2:Structure) = getSameLevel[Structure](s1,s2,_.getParent(),_.getNumber)
  def getSameLevel[T](s1:T, s2:T, getParent:T=>T, getid:T=>Int):(T,T) = {
    val parent = findCommandParent[T](s1,s2,getParent,getid)
    var s1i = s1
    var s2i = s2
    while (getid(getParent(s1i)) != parent)
    {
      s1i = getParent(s1i)
    }
    while (getid(getParent(s2i)) != parent)
    {
      s2i = getParent(s2i)
    }
    return (s1i, s2i)
  }

  def isSameLevel(s1:Structure, s2:Structure):Boolean = {
    return s1.getParent.getNumber == s2.getParent().getNumber
  }

  private var number = 0
  def getNumber() = {
    number += 1
    number
  }
}

abstract class Structure extends Cloneable {
  protected var number = Structure.getNumber()
  def getNumber = number

  def printForPoirot(writeString:String=>Unit,writeStatement:(String,CFAEdge,Statement) => Unit)
  def print(writeString:String=>Unit,writeStatement:(String,Statement) => Unit)

  def getUsedVariables():VariableAnalysisResult
  def getChangedVariables():VariableAnalysisResult

  def getParent():Structure
  def setParent(parent:Structure)

  def sort(order: PartialOrder[Int])

  def myClone():Structure

  def getBlockSize() : Int

  def isLock() : Boolean = false


  // gets the name of the function this statement is in
  def getFunctionLevel():Function = {
    var str = this
    while (true) {
      if (str.isInstanceOf[Function])
        return str.asInstanceOf[Function]
      else
        str = str.getParent()
    }
    return null // will be never reached
  }

  def getFunctionName():String = {
    getFunctionLevel().getName()
  }

  def getThreadName():String = {
    var str = this
    while (true) {
      if (str.isInstanceOf[Function])
        return str.asInstanceOf[Function].getThreadName()
      else
        str = str.getParent()
    }
    return null // will be never reached
  }

  override def toString() : String = {
    val sb = new StringBuilder
    def write(printout:String):Unit = {sb.append(printout)}
    def writeStatement(printout:String,e: Statement) = write(printout)
    print(write, writeStatement)
    return sb.toString()
  }

  // the processor returns if the structure processing should continue
  def processAllStructures(processor:(Structure,List[Structure])=>Boolean):Unit

  // this processes all structures, but only one at a time
  def processAllStructuresByOne(processor:(Structure)=>Boolean):Unit = {
    def newProcessor(str:Structure,l:List[Structure]):Boolean = {
      for (s<-l)
        if (!processor(s)) return false
      return true
    }

    if (processor(this))
      this.processAllStructures(newProcessor)
  }

  def getProgramLevel():Program = {
    var str = this
    while (true) {
      if (str.isInstanceOf[Program])
        return str.asInstanceOf[Program]
      else
        str = str.getParent()
    }
    return null // will be never reached
  }

  // gives the needed number of tabs to be put in front of this command when printing
  protected def getTabs:String = {
    var str = ""
    var st = this.getParent()
    while (!st.isInstanceOf[Program]) {
      str = "  " + str
      st = st.getParent()
    }
    return str
  }
}
