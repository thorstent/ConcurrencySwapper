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
import at.ac.ist.concurrency_swapper.helpers.{ExpressionHelpers, GatedCommand}
import at.ac.ist.concurrency_swapper.structures.Statement
import org.sosy_lab.cpachecker.cfa.ast.{IASTPointerTypeSpecifier, IASTExpressionAssignmentStatement, IASTIdExpression}

object PointerInit extends IntermediateTranslation {

  def accepts(edge: CFAEdge): Boolean = {
    if (edge.getRawAST.isInstanceOf[IASTExpressionAssignmentStatement]) {
      val ass = edge.getRawAST.asInstanceOf[IASTExpressionAssignmentStatement]
      if (ass.getLeftHandSide.isInstanceOf[IASTIdExpression]) {
        val id = ass.getLeftHandSide.asInstanceOf[IASTIdExpression]
        if (id.getExpressionType.isInstanceOf[IASTPointerTypeSpecifier])
          return true
      }
    }
    return false;
  }

  override def translate(edge: CFAEdge,originalStatement:Statement): List[CFAEdge] = {
    val ass = edge.getRawAST.asInstanceOf[IASTExpressionAssignmentStatement]
    val id = ass.getLeftHandSide.asInstanceOf[IASTIdExpression]
    originalStatement.getProgramLevel().declarationsForPoirot += id.getName+"_init" -> 0
    return List(ExpressionHelpers.makeAssignmentEdge(id.getName+"_init", ExpressionHelpers.makeIntConst(1),originalStatement.getFunctionName()))
  }
}
