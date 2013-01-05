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
import at.ac.ist.concurrency_swapper.structures.Statement

object Up extends IntermediateTranslation with VariableAnalysis {
  def accepts(edge: CFAEdge):Boolean = {
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => false
      case Some((name,_)) => name == "up" || name == "unlock"
    }
  }

  def translate(edge: CFAEdge,originalStatement:Statement) : List[CFAEdge] = {
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => null
      case Some((_,arg1)) =>
        ExpressionHelpers.getName(arg1) match {
          case None => null
          case Some(name) =>
            val edge1 = ExpressionHelpers.makeAssignmentEdge(name, ExpressionHelpers.makeIntConst(0), ExpressionHelpers.getFunctionName(edge))
            List(edge1)
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

  def usedVariables(edge: CFAEdge) = SomeVars(Set.empty)
}
