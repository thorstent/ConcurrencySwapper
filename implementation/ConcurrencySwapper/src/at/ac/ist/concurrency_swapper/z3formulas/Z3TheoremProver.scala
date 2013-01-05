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

package ac.at.ist.concurrency_swapper.z3formulas

import java.util.Collection
import org.sosy_lab.common.Timer
import org.sosy_lab.cpachecker.util.predicates.AbstractionManager
import org.sosy_lab.cpachecker.util.predicates.Model
import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import org.sosy_lab.cpachecker.util.predicates.interfaces.TheoremProver
import z3.scala._
import java.lang
import scala.Some

class Z3TheoremProver(z3: Z3Context) extends TheoremProver {

  private def getTerm(f: Formula): Z3AST = {
    return (f.asInstanceOf[Z3Formula]).getTerm
  }

  private def negateOption(x: Option[Boolean]) : Option[Boolean] = {
    x match {
      case None => None
      case Some(b) => Some(!b)
    }
  }

  def init {
    val tactic1 = z3.mkTactic("qe")
    val tactic2 = z3.mkTactic("smt")
    tactic = z3.mkTacticAndThen(tactic1, tactic2)
    tactic1.delete()
    tactic2.delete()
    solver = z3.mkSolverFromTactic(tactic)
  }

  def push(f: Formula) {
    solver.push
    solver.assertCnstr(getTerm(f))
  }

  def pop {
    solver.pop(1)
  }

  def isUnsat: Boolean = {
    val res = solver.check()
    res match {
      case Some(x) => return !x;
      case None => return false;
    }
  }

  def isUnsat(f: Formula): Boolean = {
    push(f)
    val res: Boolean = isUnsat
    pop
    return res
  }

  def checkSat() : Option[Boolean] = {
    val res = solver.check
    if (res == Some(true))
      modelAvailable = true
    else
      modelAvailable = false
    res
  }

  def checkSat(f: Formula): Option[Boolean] = {
    push(f)
    val res = checkSat()
    pop()
    return res
  }

  def getModel: Model = {
    return null
  }

  def getZ3Model: Z3ModelWrapper = {
    if (!modelAvailable) return null
    return new Z3ModelWrapper(null, solver.getModel())
  }

  def reset() = solver.reset()

  def allSat(f: Formula, important: Collection[Formula], mgr: AbstractionManager, timer: Timer): TheoremProver.AllSatResult = {
    return null
  }

  private var solver: Z3Solver = null
  private var tactic: Z3Tactic = null
  private var modelAvailable = false
}

