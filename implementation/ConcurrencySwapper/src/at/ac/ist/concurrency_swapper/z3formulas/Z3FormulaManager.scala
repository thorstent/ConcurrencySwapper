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

package ac.at.ist.concurrency_swapper.z3formulas;

import org.sosy_lab.cpachecker.util.predicates.interfaces.{FormulaList, Formula, FormulaManager}
import z3.scala._
import org.sosy_lab.cpachecker.util.predicates.SSAMap
import collection.JavaConversions
import scala.Some
;

class Z3FormulaManager(z3ctx : Z3Context) extends FormulaManager {
  private var z3: Z3Context = z3ctx
  private final val INDEX_SEPARATOR: String = "@"
  final def intSort : Z3Sort = z3.mkBVSort(32) //use 32 bit wide machine integers for z3
  final def boolSort : Z3Sort = z3.mkBoolSort()
  private def defaultSort : Z3Sort = intSort

  def setZ3(z3ctx:Z3Context) { z3 = z3ctx }
  def getZ3() = z3

  private def makeName(name: String, idx: Int): String = {
    return name + INDEX_SEPARATOR + idx
  }

  private def encapsulate(ast: Z3AST, assignedVariables: Set[String], usedVariables: Set[String]): Formula = {
    val newast = ast
    return new Z3Formula(newast, z3, assignedVariables, usedVariables)
  }

  private def encapsulate(ast: Z3AST): Formula = {
    return encapsulate(ast, (Set.empty:Set[String]), (Set.empty:Set[String]))
  }

  private def encapsulate(ast: Z3AST, newVar: String): Formula = {
    return encapsulate(ast, (Set.empty:Set[String]), Set.empty + newVar)
  }

  private def encapsulate(ast: Z3AST, oldFormula: Formula): Formula = {
    return encapsulate(ast, getAssignedVariables(oldFormula), getUsedVariables(oldFormula))
  }

  private def encapsulate(ast: Z3AST, oldFormula1: Formula, oldFormula2: Formula): Formula = {
    val usedVars: Set[String] = getUsedVariables(oldFormula1) ++ getUsedVariables(oldFormula2)
    val assignedVars: Set[String] = getAssignedVariables(oldFormula1) ++ getAssignedVariables(oldFormula2)
    return encapsulate(ast, assignedVars, usedVars)
  }

  private def getTerm(f: Formula): Z3AST = {
    return (f.asInstanceOf[Z3Formula]).getTerm
  }

  private def getVariables(f: Formula): Set[String] = {
    return (f.asInstanceOf[Z3Formula]).getVariables
  }

  private def getAssignedVariables(f: Formula): Set[String] = {
    return (f.asInstanceOf[Z3Formula]).getAssignedVariables
  }

  private def getUsedVariables(f: Formula): Set[String] = {
    return (f.asInstanceOf[Z3Formula]).getUsedVariables
  }

  def extractAssignedVariables(f: Formula): Set[String] = {
    getAssignedVariables(f)
  }

  def extractUsedVariables(f: Formula): Set[String] = {
    getUsedVariables(f)
  }

  def isBoolean(pF: Formula): Boolean = {
    val sort = z3.getSort(getTerm(pF))
    sort.isBoolSort
  }

  def simplify(f: Formula) : Formula = {
    encapsulate(z3.simplifyAst(getTerm(f)), f)
  }

  def makeTrue: Formula = {
    return encapsulate(z3.mkTrue)
  }

  def makeFalse: Formula = {
    return encapsulate(z3.mkFalse)
  }

  def makeNot(f: Formula): Formula = {
    return encapsulate(z3.mkNot(getTerm(f)), f)
  }

  def makeAnd(f1: Formula, f2: Formula): Formula = {
    return encapsulate(z3.mkAnd(getTerm(f1), getTerm(f2)),f1 ,f2)
  }

  def makeOr(f1: Formula, f2: Formula): Formula = {
    return encapsulate(z3.mkOr(getTerm(f1), getTerm(f2)), f1, f2)
  }

  def makeImplies(f1: Formula, f2: Formula): Formula = {
    return encapsulate(z3.mkImplies(getTerm(f1), getTerm(f2)), f1, f2)
  }

  def makeEquivalence(f1: Formula, f2: Formula): Formula = {
    return encapsulate(z3.mkIff(getTerm(f1), getTerm(f2)), f1, f2)
  }

  def makeIfThenElse(cond: Formula, f1: Formula, f2: Formula): Formula = {
    return encapsulate(z3.mkITE(getTerm(cond), getTerm(f1), getTerm(f2)), f1, f2)
  }

  def makeNumber(pI: Int): Formula = {
    return encapsulate(z3.mkInt(pI, intSort))
  }

  def makeNumber(pI: String): Formula = {
    return makeNumber(Integer.parseInt(pI))
  }

  def makeNegate(pF: Formula): Formula = {
    return encapsulate(z3.mkBVNeg(getTerm(pF)), pF)
  }

  def makePlus(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVAdd(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeMinus(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVSub(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeDivide(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVSdiv(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeModulo(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVSmod(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeMultiply(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVMul(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeEqual(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkEq(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeGt(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVSgt(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeGeq(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVSge(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeLt(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVSlt(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeLeq(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVSle(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeBitwiseNot(pF: Formula): Formula = {
    return encapsulate(z3.mkBVNot(getTerm(pF)), pF)
  }

  def makeBitwiseAnd(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVAnd(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeBitwiseOr(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVOr(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeBitwiseXor(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVXor(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeShiftLeft(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVShl(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeShiftRight(pF1: Formula, pF2: Formula): Formula = {
    return encapsulate(z3.mkBVLshr(getTerm(pF1), getTerm(pF2)), pF1, pF2)
  }

  def makeUIF(pName: String, pArgs: FormulaList): Formula = {
    if (pName.startsWith("assert"))
      return (pArgs.asInstanceOf[Z3FormulaList]).getFormulas(0)
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def makeUIF(pName: String, pArgs: FormulaList, pIdx: Int): Formula = {
    return makeUIF(makeName(pName, pIdx), pArgs)
  }

  def makeString(pI: Int): Formula = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def makeVariable(pVar: String, pIdx: Int): Formula = {
    return makeVariable(makeName(pVar, pIdx))
  }

  def makeVariable(pVar: String): Formula = {
    return makeVariable(pVar, defaultSort)
  }

  def makeVariable(pVar: String, sort: Z3Sort): Formula = {
    return encapsulate(makeVariableAST(pVar, sort), pVar)
  }

  private def makeVariableAST(pVar: String): Z3AST = {
    makeVariableAST(pVar, defaultSort)
  }

  private def makeVariableAST(pVar: String, sort: Z3Sort): Z3AST = {
    z3.mkConst(z3.mkStringSymbol(pVar), sort)
  }

  def makePredicateVariable(pVar: String, pIdx: Int): Formula = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def makeAssignment(pF1: Formula, pF2: Formula): Formula = {
    encapsulate(z3.mkEq(getTerm(pF1), getTerm(pF2)), getUsedVariables(pF1), getUsedVariables(pF2))
  }

  def makeList(pF: Formula): FormulaList = {
    val formulas : Seq[Z3Formula] = Seq(pF.asInstanceOf[Z3Formula])
    return new Z3FormulaList(formulas)
  }

  def makeList(pF1: Formula, pF2: Formula): FormulaList = {
    val formulas : Seq[Z3Formula] = Seq(pF1.asInstanceOf[Z3Formula], pF1.asInstanceOf[Z3Formula])
    return new Z3FormulaList(formulas)
  }

  def makeList(pFs: java.util.List[Formula]): FormulaList = {
    var formulas : List[Z3Formula] = List.empty
    import scala.collection.JavaConversions._
    for (pF:Formula <- pFs){
      formulas = (pF.asInstanceOf[Z3Formula]) :: formulas
    }
    return new Z3FormulaList(formulas)
  }

  def makeExists(f: Formula, vars : Set[String], weight : Int = 1) : Formula = {
    val vars2 = vars.map(x => makeVariableAST(x))
    return encapsulate(z3.mkExistsConst(weight, Seq.empty, vars2.toSeq, getTerm(f)), getAssignedVariables(f) -- vars, getUsedVariables(f) -- vars);
  }

  def makeForall(f: Formula, vars : Set[String], weight : Int = 1) : Formula = {
    val varsp = vars.map(x => makeVariableAST(x))
    return encapsulate(z3.mkForAllConst(weight, Seq.empty, varsp.toSeq, getTerm(f)), getAssignedVariables(f) -- vars, getUsedVariables(f) -- vars);
  }

  def parseInfix(s: String): Formula = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def parse(s: String): Formula = {
    encapsulate(z3.parseSMTLIB2String(s))
  }

  def instantiate(f: Formula, ssa: SSAMap): Formula = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  @Deprecated def uninstantiate(pF: Formula): Formula = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def extractAtoms(f: Formula, splitArithEqualities: Boolean, conjunctionsOnly: Boolean): java.util.Collection[Formula] = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def extractVariablesS(f: Formula): Set[String] = {
    return getVariables(f)
  }

  def extractVariables(f: Formula): java.util.Set[String] = {
    return JavaConversions.setAsJavaSet(extractVariablesS(f))
  }

  def dumpFormula(pT: Formula): String = {
    z3.astToString(getTerm(pT))
  }

  def getBitwiseAxioms(f: Formula): Formula = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def createPredicateVariable(pAtom: Formula): Formula = {
    throw new UnsupportedOperationException("Not yet implemented")
  }

  def replace(pF: Formula, oldName:String, newName:String): Formula = {
    replace(pF, Array(oldName), Array(newName))
  }

  def replace(pF: Formula, oldNames:Array[String], newNames:Array[String]): Formula = {
    encapsulate(z3.substitute(getTerm(pF), oldNames.map(makeVariableAST(_)), newNames.map(makeVariableAST(_))),
    Z3Helper.setReplace[String](getAssignedVariables(pF), oldNames, newNames), Z3Helper.setReplace[String](getUsedVariables(pF), oldNames, newNames))
  }

  def makeValid(f: Formula) : Formula = {
    if (getVariables(f) == Set.empty)
      return f
    return makeForall(f, getVariables(f))
  }

}