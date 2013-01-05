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
import at.ac.ist.concurrency_swapper.helpers.{Declaration, SomeVars, ExpressionHelpers, GatedCommand}
import at.ac.ist.concurrency_swapper.structures.Statement

object DefaultTranslation extends FinalTranslation with VariableAnalysis{
  def translatePoirot(edge: CFAEdge,originalStatement:Statement) = ExpressionHelpers.printEdge(edge)

  def accepts(edge: CFAEdge) = true

  def getFormula(edge: CFAEdge) = GatedCommand.ConvertEdge(edge)

  def usedVariables(edge: CFAEdge) = {
    if (edge.getEdgeType == CFAEdgeType.DeclarationEdge)
      Declaration
    else
      SomeVars(ExpressionHelpers.getUsedVariables(edge.getRawAST))
  }

  def changedVariables(edge: CFAEdge) = {
    if (edge.getEdgeType == CFAEdgeType.DeclarationEdge)
      Declaration
    else
      SomeVars(ExpressionHelpers.getChangedVariables(edge.getRawAST))
  }
}
