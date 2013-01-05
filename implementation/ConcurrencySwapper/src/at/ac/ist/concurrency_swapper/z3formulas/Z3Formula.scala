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

import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import z3.scala._
import z3.scala.Z3StringSymbol
import z3.scala.Z3AppAST
import z3.scala.Z3NumeralAST
import z3.scala.Z3IntSymbol
import scala.Some

class Z3Formula(ast: Z3AST, context: Z3Context, assignedVariables: Set[String], usedVariables: Set[String]) extends Formula {

  def getTerm: Z3AST = {
    return this.ast
  }

  def getVariables: Set[String] = {
    return this.assignedVariables ++ this.usedVariables
  }

  def getAssignedVariables: Set[String] = {
    return this.assignedVariables
  }

  def getUsedVariables: Set[String] = {
    return this.usedVariables
  }

  def isTrue: Boolean = {
    context.getBoolValue(getTerm) match {
      case Some(x) => return x
      case None => return false
    }
  }

  def isFalse: Boolean = {
    context.getBoolValue(getTerm) match {
      case Some(x) => return x
      case None => return false
    }
  }

  def customPrint : String = customPrint(this.ast, Seq.empty)

  def customPrint(ast : Z3AST, boundVars: Seq[String]) : String = {
    context.getASTKind(ast) match {
      case Z3NumeralAST(Some(x)) => x.toString
      case Z3NumeralAST(None) => "Unk_numeral"
      case Z3AppAST(fun, args) =>
        val funname = funToString(fun)
        if (args.length == 0) funname else
        if (args.length == 1) funname + " " + customPrint(args(0), boundVars) else
        "(" + args.map(customPrint(_, boundVars)).mkString(" " + funname + " ") + ")"
      case Z3VarAST(i) => {
        boundVars(boundVars.length - i - 1)
      }
      case Z3QuantifierAST(forall, vars, body) =>
        val symbol = if (forall) "∀" else "∃"
        symbol + vars.mkString(",") + "." + customPrint(body, boundVars ++ vars) + ""
      case Z3UnknownAST => "Unk_ast"
    }
  }

  def funToString(fun: Z3FuncDecl) : String = {
    context.getDeclKind(fun) match {
      case Z3DeclKind.OpTrue => "true"
      case Z3DeclKind.OpFalse => "true"
      case Z3DeclKind.OpAnd => "∧"
      case Z3DeclKind.OpOr => "∨"
      case Z3DeclKind.OpIff => "≡"
      case Z3DeclKind.OpImplies => "⇒"
      case Z3DeclKind.OpEq => "="
      case Z3DeclKind.OpLT => "<"
      case Z3DeclKind.OpGT => ">"
      case Z3DeclKind.OpLE => "≤"
      case Z3DeclKind.OpGT => "≥"
      case Z3DeclKind.OpAdd => "+"
      case Z3DeclKind.OpSub => "-"
      case Z3DeclKind.OpMul => "*"
      case Z3DeclKind.OpDiv => "/"
      case Z3DeclKind.OpNot => "¬"
      case Z3DeclKind.OpDistinct => "≠"
      case Z3DeclKind.OpUninterpreted =>
        val symbol = fun.getName
        context.getSymbolKind(symbol) match {
          case Z3IntSymbol(i) => "\"" + i + "\""
          case Z3StringSymbol(s) => s
        }
      case _ => "unk_op"
    }
  }

  override def toString: String = customPrint
}



