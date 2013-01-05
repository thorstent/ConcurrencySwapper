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

import ac.at.ist.concurrency_swapper.z3formulas.Z3Formula
import ac.at.ist.concurrency_swapper.z3formulas.Z3FormulaManager
import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import org.sosy_lab.cpachecker.util.predicates.interfaces.FormulaManager
import org.sosy_lab.cpachecker.cfa.ast.{ExpressionVisitor, IASTExpression}
import org.sosy_lab.cpachecker.util.predicates.SSAMap
import org.sosy_lab.cpachecker.exceptions.UnrecognizedCCodeException

object FormulaHelpers {
  def Init(newFm: Z3FormulaManager) {
    fm = newFm
  }

  def getBounds(vars: Set[String]): Map[String, (Int, Int)] = {
    var map: Map[String, (Int, Int)] = Map.empty
    for (v <- vars) {
      val vs: Array[String] = v.split("@")
      val variable: String = vs(0)
      val number: Int = Integer.parseInt(vs(1))
      if (!map.contains(variable)) {
        map += variable -> (number, number)
      }
      else {
        var (lower, upper): (Int,Int) = map.get(variable).get
        if (number < lower) lower = number
        else if (number > upper) upper = number
        map += variable -> (lower, upper)
      }
    }
    return map
  }

  def replaceVar(f: Formula, oldName: String, newName: String): Formula = {
    if (f.isInstanceOf[Z3Formula]) {
      return (fm.asInstanceOf[Z3FormulaManager]).replace(f, oldName, newName)
    }
    else {
      var formula: String = fm.dumpFormula(f)
      formula = formula.replace("\"", "")
      formula = formula.replace(oldName, newName)
      return fm.parse(formula)
    }
  }

  def makeImplies(f1: Formula, f2: Formula): Formula = {
    if (fm.isInstanceOf[Z3FormulaManager]) {
      return (fm.asInstanceOf[Z3FormulaManager]).makeImplies(f1, f2)
    }
    val nf1: Formula = fm.makeNot(f1)
    return fm.makeOr(nf1, f2)
  }

  def getVarsOfLevel(variables: Set[String], level: Int): Set[String] = {
    var res: Set[String] = Set.empty
    for (`var` <- variables) {
      if (`var`.endsWith("@" + Integer.toString(level))) res += `var`
    }
    return res
  }

  final val filterVar = (s:String) => s.split("@")(0)
  final val filterIndex = (s:String) => s.split("@")(1)
  final val filterIndexInt = (s:String) => s.split("@")(1).toInt

  def shiftVariables(f: Formula, shift: Int) : Formula = {
    val min = fm.extractVariablesS(f).map(filterIndexInt).min
    val max = fm.extractVariablesS(f).map(filterIndexInt).max
    shiftVariables(new StateFormula(f, min, max), shift)
  }

  def shiftVariables(f: StateFormula, shift: Int) : Formula = {
    shiftVariables(f, shift, Set.empty)
  }

  /// This moves variables indexes by shift amount
  /// if variables is empty it moves them all
  def shiftVariables(f: StateFormula, shift: Int, variables: Set[String]): Formula = {
    var i: Int = if (shift >=0) f.getUpperBound else f.getLowerBound
    var newf2: Formula = f.getFormula
    while (if (shift >= 0) i >= f.getLowerBound else i <= f.getUpperBound) {
      val vars: Set[String] = fm.extractVariablesS(newf2).filter(s => if (variables.isEmpty) true else variables.contains(s.split("@")(0)))
      for (variable <- FormulaHelpers.getVarsOfLevel(vars, i)) {
        val newvar: String = variable.replace("@" + Integer.toString(i), "@" + Integer.toString(i + shift))
        newf2 = FormulaHelpers.replaceVar(newf2, variable, newvar)
      }
      i -= (if (shift >= 0) 1 else -1)
    }
    newf2
  }

  def moveLastAssignment(f: Formula, lastNum: Int) : Formula = {
    var newf = f
    val vars = fm.extractAssignedVariables(f).map(filterVar)
    for (v <- vars)
    {
      val max = fm.extractAssignedVariables(f).filter(_.split("@")(0) == v).map(filterIndexInt).max
      if (max < lastNum)
      {
        newf = FormulaHelpers.replaceVar(newf, v + "@" + max, v + "@" + lastNum)
      }
    }
    newf
  }

  // the purpose of this is to move the first use of a variable in f2 to the last assignment in f1
  // we assume that f2 is only one command
  // we assume that f1 and f2 do not overlap (previous shift)
  // we return the second formula as it is adjusted
  def stichFormulas(f1: Formula, f2: Formula): Formula = {
    var f2p = f2
    val vassf1: Set[String] = fm.extractAssignedVariables(f1)
    val varsf1 = vassf1.map(filterVar)
    for (v <- fm.extractUsedVariables(f2)) {
      val vname = filterVar(v)
      // check if vname is assigned in f1
      if (varsf1.contains(vname))
      {
        // calculate maximum and rename
        val max = vassf1.filter(_.split("@")(0) == vname).map(filterIndexInt).max
        f2p = fm.replace(f2p, v, vname + "@" + max)
      }
      else
      {
        // we set it to 0
        f2p = fm.replace(f2p, v, vname + "@0")
      }
    }
    return f2p
  }


  // gets the highest overall index
  def getHighestIndex(vars: Set[String]): Int = {
    if (vars.isEmpty) return 1
    vars.map(filterIndexInt).max
  }

  // we get the highest index of these variables
  // however, if the highest is 0 then not included
  def getHighestVars(vars: Set[String]): Set[String] = {
    var res : Set[String] = Set.empty
    val varnames = vars.map(filterVar)
    for (v <- varnames)
    {
      val max = vars.filter(_.split("@")(0) == v).map(filterIndexInt).max
      res += v + "@" + max
    }
    return res
  }

  def getFormula(expr : IASTExpression, functionName:String): Formula = {
    // get private class from CPAChecker
    //val constraints: CtoFormulaConverter#Constraints = new CtoFormulaConverter#Constraints
    val ConstraintsCl = Class.forName("org.sosy_lab.cpachecker.util.predicates.CtoFormulaConverter$Constraints")
    val ConstraintsCtor = ConstraintsCl.getDeclaredConstructors()(0)
    ConstraintsCtor.setAccessible(true)
    val constraints: AnyRef = ConstraintsCtor.newInstance(StateFormula.cto).asInstanceOf[AnyRef]

    val SSAMapBuilderCl = Class.forName("org.sosy_lab.cpachecker.util.predicates.SSAMap$SSAMapBuilder")
    val SSAMapBuilderCtor = SSAMapBuilderCl.getDeclaredConstructors()(0)
    SSAMapBuilderCtor.setAccessible(true)
    val ssaBuilder = SSAMapBuilderCtor.newInstance(SSAMap.emptySSAMap()).asInstanceOf[AnyRef]
    val cl = Class.forName("org.sosy_lab.cpachecker.util.predicates.CtoFormulaConverter$StatementToFormulaVisitor");
    val ctor = cl.getDeclaredConstructors()(0)
    ctor.setAccessible(true);
    val v = ctor.newInstance(StateFormula.cto, functionName, ssaBuilder, constraints).asInstanceOf[ExpressionVisitor[Formula,UnrecognizedCCodeException]]

    // finally use that class to get our formula
    var formula = expr.accept(v)
    val min = fm.extractVariablesS(formula).map(FormulaHelpers.filterIndexInt).min
    formula = FormulaHelpers.shiftVariables(new StateFormula(formula, 0, 3), -min, Set.empty)
    return formula
  }

  private[concurrency_swapper] var fm: Z3FormulaManager = null
}