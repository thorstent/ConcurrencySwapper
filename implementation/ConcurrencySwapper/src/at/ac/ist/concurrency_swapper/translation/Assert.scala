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
import org.sosy_lab.cpachecker.cfa.ast.{IASTIdExpression, IASTFunctionCallStatement}
import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import at.ac.ist.concurrency_swapper.helpers.{ExpressionHelpers, StateFormula, GatedCommand, FormulaHelpers}
import at.ac.ist.concurrency_swapper.structures.Statement

object Assert extends FinalTranslation {
  def translatePoirot(edge: CFAEdge,originalStatement:Statement) = printEdge(edge).replaceFirst("assert","POIROT_ASSERT")

  def accepts(edge: CFAEdge):Boolean = {
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => false
      case Some((name,_)) => name == "assert"
    }
  }

  def getFormula(edge: CFAEdge): GatedCommand = {
    ExpressionHelpers.getFunctionDef(edge) match {
      case None => null
      case Some((_,arg1)) =>
        val formula = FormulaHelpers.getFormula(arg1, if ((edge.getPredecessor != null)) edge.getPredecessor.getFunctionName else null)
        new GatedCommand(formula, new StateFormula(FormulaHelpers.fm.makeTrue(), 0, 0))
    }
  }
}
