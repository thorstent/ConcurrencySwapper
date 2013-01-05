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
import org.sosy_lab.cpachecker.cfa.objectmodel.c.StatementEdge
import org.sosy_lab.cpachecker.cfa.ast.{IASTFunctionCallStatement,IASTUnaryExpression}
import org.sosy_lab.cpachecker.cfa.ast.IASTUnaryExpression.UnaryOperator
import org.sosy_lab.cpachecker.cfa.ast.IASTBinaryExpression.BinaryOperator


object PointerDeref extends IntermediateTranslation {

  def accepts(edge: CFAEdge): Boolean = {
    if (edge.getEdgeType == CFAEdgeType.StatementEdge) {
      if (edge.asInstanceOf[StatementEdge].getStatement.isInstanceOf[IASTFunctionCallStatement]) {
        val stmt = edge.asInstanceOf[StatementEdge].getStatement.asInstanceOf[IASTFunctionCallStatement]
        if (stmt.getFunctionCallExpression.getFunctionNameExpression.isInstanceOf[IASTUnaryExpression]) {
          val unary = stmt.getFunctionCallExpression.getFunctionNameExpression.asInstanceOf[IASTUnaryExpression]
          if (unary.getOperator == UnaryOperator.STAR)
            return true
        }
      }
    }
    return false
  }

  def translate(edge: CFAEdge, originalStatement: Statement): List[CFAEdge] = {
    val stmt = edge.asInstanceOf[StatementEdge].getStatement.asInstanceOf[IASTFunctionCallStatement]
    val unary = stmt.getFunctionCallExpression.getFunctionNameExpression.asInstanceOf[IASTUnaryExpression]
    ExpressionHelpers.getName(unary.getOperand) match {
      case Some(name) =>
        return List(ExpressionHelpers.makeAssertEdge(ExpressionHelpers.makeBinaryOperation(ExpressionHelpers.makeName(name+"_init"),ExpressionHelpers.makeIntConst(1),BinaryOperator.EQUALS), originalStatement.getFunctionName()))
      case None => throw new Exception("should never happen")
    }
  }
}
