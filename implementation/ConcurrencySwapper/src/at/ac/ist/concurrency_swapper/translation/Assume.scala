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

import org.sosy_lab.cpachecker.cfa.objectmodel.{CFAEdgeType, CFAEdge}
import org.sosy_lab.cpachecker.cfa.ast.{IASTIdExpression, IASTFunctionCallStatement}
import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import at.ac.ist.concurrency_swapper.helpers.{ExpressionHelpers, StateFormula, GatedCommand, FormulaHelpers}
import org.sosy_lab.cpachecker.cfa.objectmodel.c.AssumeEdge
import at.ac.ist.concurrency_swapper.structures.Statement

object Assume extends FinalTranslation {
  def translatePoirot(edge: CFAEdge,originalStatement:Statement):String = {
    printEdge(edge).replaceFirst("assume","__hv_assume")
  }

  def accepts(edge: CFAEdge):Boolean = {
    if (edge.getEdgeType eq CFAEdgeType.AssumeEdge) return true
    return ExpressionHelpers.getFunctionDef(edge) match {
      case None => false
      case Some((name,_)) => name == "assume"
    }
  }

  def getFormula(edge: CFAEdge): GatedCommand = {
    var formula:Formula = null
    if (edge.getEdgeType eq CFAEdgeType.AssumeEdge) {
      formula = FormulaHelpers.getFormula(edge.asInstanceOf[AssumeEdge].getExpression,edge.getPredecessor.getFunctionName)
    } else {
      ExpressionHelpers.getFunctionDef(edge) match {
        case None => null
        case Some((_,arg1)) =>
          formula = FormulaHelpers.getFormula(arg1, if ((edge.getPredecessor != null)) edge.getPredecessor.getFunctionName else null)
      }
    }
    new GatedCommand(FormulaHelpers.fm.makeTrue, new StateFormula(formula, 0, 0))
  }
}
