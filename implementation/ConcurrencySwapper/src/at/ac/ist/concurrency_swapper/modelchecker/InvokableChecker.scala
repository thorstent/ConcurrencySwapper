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

import at.ac.ist.concurrency_swapper.helpers.StatementOrder

trait InvokableChecker {
  // it returns if there is a bug or not, if there is it also returns the trace and the id of the bug
  // the id is used to determine if the bug is the same or if it is fixed
  def invokeChecker(so: StatementOrder) : (Boolean, List[CtexStmt], Int, Double)
}
