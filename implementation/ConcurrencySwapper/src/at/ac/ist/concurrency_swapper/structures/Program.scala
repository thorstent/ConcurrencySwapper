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

import at.ac.ist.concurrency_swapper.helpers.PartialOrder
import java.util.regex.Pattern
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge

class Program(functions:List[Function], originalProgram:String, threadNames:Set[String]) extends Structure {
  functions.foreach(_.setParent(this))
  private val functionMap = functions.map(f=>(f.getName(),f)).toMap
  private var lockNames:Set[String] = Set.empty

  def myClone():Program = {
    val prog = new Program(functions.map(_.myClone), originalProgram, threadNames)
    prog.number = number
    prog
  }

  var declarationsForPoirot : Map[String,Int] = Map.empty

  def getThreadNames = threadNames
  def getLockNames = lockNames

  def addLockName(name:String) = {
    lockNames += name
  }

  def printForPoirot(writeString: (String) => Unit, writeStatement: (String, CFAEdge, Statement) => Unit) = {
    functions.foreach(_.printForPoirot(writeString,writeStatement))
  }

  def print(writeString: (String) => Unit, writeStatement: (String, Statement) => Unit) = {
    functions.foreach(_.print(writeString,writeStatement))
  }

  def print():String = {
    val sb = new StringBuilder

    def write1(s:String) = {
      sb.append(s)
      ()
    }

    def writeStatement1(printout:String,e: Statement) = {
      var s = printout
      if (!s.endsWith(";")) s += ";"
      s = s + "\n"
      write1(s)
    }

    print(write1, writeStatement1)

    val program = Pattern.compile("#pragma region threads.*#pragma endregion threads", Pattern.DOTALL)
      .matcher(originalProgram).replaceAll(sb.result)
    return program
  }

  def printForPoirot(addToMap:(Int, (CFAEdge,Statement))=>Unit):String = {
    var program = originalProgram
    val sb = new StringBuilder

    var threadMatcher = Pattern.compile("#pragma region threads.*#pragma endregion threads", Pattern.DOTALL)
      .matcher(program)
    threadMatcher.find()
    val strToStrings = originalProgram.substring(0,threadMatcher.start())
    var l = strToStrings.count(_=='\n') + 3 + 2 + 1 // 3 for includes, 2 for declarations, 1 magic

    def write2(s:String) = {
      sb.append(s)
      l += s.count(_ == '\n')
    }

    def writeStatement2(printout:String,edge:CFAEdge,e: Statement) = {
      addToMap(l, (edge,e))
      var s = printout
      if (!s.endsWith(";")) s += ";"
      //s = "/*" + l.toString + "*/ " + s + "\n"
      s = s + "\n"
      write2(s)
    }

    printForPoirot(write2, writeStatement2)

    // now add the variable declarations
    val declsb = new StringBuilder
    val defsb = new StringBuilder
    for ((name,init)<-declarationsForPoirot) {
      declsb.append("int " + name + " = " + init + "; ")
      defsb.append(name + " = " + init + "; ")
    }

    val threadnames = functions.map(_.getName())

    program = "#include \"havoc.h\"\n#include \"poirot.h\"\n\n" + program

    for (t <- threadnames)
    {
      program = program.replaceAll("[\\n]\\s+"+t+"\\(\\)", "\n\t__async_call "+t+"()")
    }
    program = program.replaceFirst("main\\(\\)\\s*\\{", "poirot_main() {")
    program = program.replaceFirst("\n\t__async_call", "\n\t" + defsb.result + "\n\t__async_call")

    threadMatcher = Pattern.compile("#pragma region threads.*#pragma endregion threads", Pattern.DOTALL)
      .matcher(program)
    program = threadMatcher.replaceAll(declsb.result() + "\n\n" + sb.result())

    return program
  }

  def getUsedVariables() = throw new Exception("not supported")

  def getChangedVariables() = throw new Exception("not supported")

  def getParent() = null

  def setParent(parent: Structure) { throw new Exception("not supported")}

  def sort(order: PartialOrder[Int]) {
    functions.foreach(_.sort(order))
  }

  def processAllStructures(processor:(Structure,List[Structure]) => Boolean) {
    //processor(functions)
    functions.foreach(_.processAllStructures(processor))
  }

  def getFunctions():Map[String,Function] = functionMap

  def getBlockSize(): Int = functions.foldLeft(1)((i,s) => i+s.getBlockSize())
}
