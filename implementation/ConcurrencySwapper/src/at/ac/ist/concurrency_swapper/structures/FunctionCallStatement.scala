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

import org.sosy_lab.cpachecker.cfa.objectmodel.c.FunctionCallEdge
import at.ac.ist.concurrency_swapper.helpers.{VariableAnalysisResult, ExpressionHelpers, SomeVars}
import org.sosy_lab.cpachecker.cfa.ast.IASTFunctionCallAssignmentStatement
import at.ac.ist.concurrency_swapper.translation.VariableAnalysis

class FunctionCallStatement(edge:FunctionCallEdge) extends Statement(edge){
  override def myClone():FunctionCallStatement = {
    val stmt = new FunctionCallStatement(edge)
    stmt.number = number
    stmt
  }

  def functionCalled():Function = this.getProgramLevel().getFunctions()(edge.getSuccessor.getFunctionDefinition.getName)

  override def getUsedVariables() = functionCalled().getUsedVariables()

  override def getChangedVariables():VariableAnalysisResult = {
    if (edge.getRawAST.isInstanceOf[IASTFunctionCallAssignmentStatement]) {
      ExpressionHelpers.getName(edge.getRawAST.asInstanceOf[IASTFunctionCallAssignmentStatement].getLeftHandSide) match {
        case Some(x) => return functionCalled().getChangedVariables() ++ SomeVars(Set(x))
        case None => throw new Exception("this shouldn't happen")
      }

    }
    return functionCalled().getChangedVariables()
  }

  /*override def printForPoirot(writeString:String=>Unit,writeStatement:(String,Statement) => Unit) = {
    def newWriteStatement(s1:String,stmt:Statement) = {
      var s = s1
      if (!s.endsWith(";")) s += ";"
      s = s + "\n"
      writeString(s)
    }
    super.printForPoirot(writeString,newWriteStatement)
  }*/
}
