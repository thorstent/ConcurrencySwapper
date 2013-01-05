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

import ac.at.ist.concurrency_swapper.z3formulas.{Z3FormulaManager}
import org.sosy_lab.common.LogManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.configuration.InvalidConfigurationException
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge
import org.sosy_lab.cpachecker.util.predicates._
import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import org.sosy_lab.cpachecker.cfa.ast.{ExpressionVisitor, IASTExpression, IASTIdExpression, IASTFunctionCallStatement}
import org.sosy_lab.cpachecker.util.predicates.SSAMap.SSAMapBuilder
import org.sosy_lab.cpachecker.exceptions.UnrecognizedCCodeException

object StateFormula {
  def Init(newFm: Z3FormulaManager, cpaConfig: Configuration, logManager: LogManager) {
    fm = newFm
    try {
      efm = new ExtendedFormulaManager(fm, cpaConfig, logManager)
      cto = new CtoFormulaConverter(cpaConfig, efm, logManager)
      pfm = new PathFormulaManagerImpl(efm, cpaConfig, logManager)
    }
    catch {
      case e: InvalidConfigurationException => {
        e.printStackTrace
      }
    }
  }

  def fromEdge(edge: CFAEdge): StateFormula = {
    var pf: PathFormula = pfm.makeEmptyPathFormula
    pf = cto.makeAnd(pf, edge)
    var f: Formula = pf.getFormula
    //calculate min and shift everything up to min
    if (fm.extractVariablesS(f) != Set.empty) {
      val min = fm.extractVariablesS(f).map(FormulaHelpers.filterIndexInt).min
      f = FormulaHelpers.shiftVariables(new StateFormula(f, 0, 3), -min, Set.empty)
      //make sure assignments end with 1
      f = FormulaHelpers.moveLastAssignment(f, 1)
    }
    return new StateFormula(f, 0, 1)
  }

  def compose(f1: StateFormula, f2: StateFormula): StateFormula = {
    val shift: Int = f1.getUpperBound - f2.getLowerBound

    // move all variables in formula 2 by shift
    val newf2 = FormulaHelpers.shiftVariables(f2, shift)
    val newf2p: Formula = FormulaHelpers.stichFormulas(f1.getFormula, newf2)
    var newf = fm.makeAnd(f1.getFormula, newf2p)
    val max = FormulaHelpers.getHighestIndex(fm.extractVariablesS(newf))
    newf = FormulaHelpers.moveLastAssignment(newf, max)

    // make and exists over the intermediate state
    var vars: Set[String] = fm.extractVariablesS(newf)
    vars --= FormulaHelpers.getVarsOfLevel(vars, 0)
    vars --= FormulaHelpers.getHighestVars(vars)
    val existsFormula: Formula = fm.makeExists(newf, vars)
    return new StateFormula(existsFormula, f1.getLowerBound, FormulaHelpers.getHighestIndex(fm.extractVariablesS(existsFormula)))
  }

  private[concurrency_swapper] var fm: Z3FormulaManager = null
  private var efm: ExtendedFormulaManager = null
  private[concurrency_swapper] var cto: CtoFormulaConverter = null
  private var pfm: PathFormulaManagerImpl = null
}

class StateFormula {
  def this(formula: Formula, lowerBound: Int, upperBound: Int) {
    this()
    if (lowerBound != 0) throw new UnsupportedOperationException("Currently lowerBound has to be 0")
    this.upperBound = upperBound
    this.lowerBound = lowerBound
    this.formula = formula
  }

  def getUpperBound: Int = {
    return upperBound
  }

  def getLowerBound: Int = {
    return lowerBound
  }

  def getFormula: Formula = {
    return formula
  }

  override def toString: String = {
    return formula.toString
  }

  private var formula: Formula = null
  private var upperBound: Int = 0
  private var lowerBound: Int = 0
}