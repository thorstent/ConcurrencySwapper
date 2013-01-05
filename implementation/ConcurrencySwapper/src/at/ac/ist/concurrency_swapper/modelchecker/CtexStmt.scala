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

package at.ac.ist.concurrency_swapper.modelchecker

import at.ac.ist.concurrency_swapper.helpers.{ExpressionHelpers, StatementOrder}

class CtexStmt(stmt:List[StatementOrder.Statement],calledFrom:List[StatementOrder.Statement],thread:Int,assertionFailure:Boolean = false,addedLater:Boolean = false) {
  def getStatement = stmt
  def getCalledFrom = calledFrom
  def getThread = thread
  def getAssertionFailure = assertionFailure // if this is the place where the assertion fails

  override def toString() = {
    val sb = new StringBuilder()
    if (addedLater) sb.append("l")
    if (assertionFailure) sb.append("!")
    sb.append(getThread.toString)
    sb.append(": ")
    sb.append(getStatement.head.getFunctionName())
    for (e <- getCalledFrom)
    {
      sb.append(", ")
      sb.append(e.getFunctionName())
    }
    sb.append(": ")
    sb.append(getStatement.map(s => ExpressionHelpers.printEdge(s.getEdge)).mkString("; "))
    // sb.append("(")
    // sb.append(getStatement(0).getNumber)
    // sb.append(")")
    sb.append("\n")
    sb.result()
  }
}
