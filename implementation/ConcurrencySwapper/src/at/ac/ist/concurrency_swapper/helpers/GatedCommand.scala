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

package at.ac.ist.concurrency_swapper.helpers

import ac.at.ist.concurrency_swapper.z3formulas.Z3FormulaManager
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge
import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import org.sosy_lab.cpachecker.util.predicates.interfaces.FormulaManager
import org.sosy_lab.cpachecker.cfa.ast.{ExpressionVisitor, IASTExpression, IASTIdExpression, IASTFunctionCallStatement}
import org.sosy_lab.cpachecker.exceptions.UnrecognizedCCodeException
import org.sosy_lab.cpachecker.util.predicates.SSAMap

object GatedCommand {
  def Init(newFm: Z3FormulaManager) {
    fm = newFm
  }

  def ConvertEdge(edge: CFAEdge): GatedCommand = {
    return new GatedCommand(fm.makeTrue, StateFormula.fromEdge(edge))
  }

  def SequentialComposition(c1: GatedCommand, c2: GatedCommand): GatedCommand = {
    val newGate: Formula = fm.makeAnd(c1.getGate, WeakestPrecondition(c1.getStmt, c2.getGate))
    val f: StateFormula = StateFormula.compose(c1.getStmt, c2.getStmt)
    return new GatedCommand(newGate, f)
  }

  def WeakestPrecondition(tau: StateFormula, phi: Formula): Formula = {
    val phip : Formula = fm.simplify(phi)
    if (phip.isTrue) return fm.makeTrue
    else
    {
      if (fm.extractVariablesS(phip).isEmpty)
        return fm.makeImplies(tau.getFormula, phip)
      val max = fm.extractVariablesS(phip).map(FormulaHelpers.filterIndexInt).max
      var newphi = FormulaHelpers.shiftVariables(phip, tau.getUpperBound - max)
      val newphip = FormulaHelpers.stichFormulas(tau.getFormula, newphi)
      newphi = fm.makeImplies(tau.getFormula, newphip)
      val varsToQuantify = FormulaHelpers.getVarsOfLevel(fm.extractVariablesS(newphi), tau.getUpperBound)
      newphi = fm.makeForall(newphi, varsToQuantify)
      newphi
    }
  }

  def Simulates(c1: GatedCommand, c2: GatedCommand): (Formula, Formula) = {
    val f1: Formula = fm.makeImplies(c2.getGate, c1.getGate)
    val f2: Formula = FormulaHelpers.makeImplies(fm.makeAnd(c2.getGate, c1.getStmt.getFormula), c2.getStmt.getFormula)
    return (f1, f2)
  }

  private var fm: Z3FormulaManager = null
}

class GatedCommand {
  def this(gate: Formula, stmt: StateFormula) {
    this()
    this.gate = gate
    this.stmt = stmt
  }

  def this(stmt: StateFormula) {
    this(GatedCommand.fm.makeTrue, stmt)
  }


  def getStmt: StateFormula = {
    return stmt
  }

  def getGate: Formula = {
    return gate
  }

  override def toString: String = {
    return gate.toString + " ▷ " + stmt.toString
  }

  private var stmt: StateFormula = null
  private var gate: Formula = null
}