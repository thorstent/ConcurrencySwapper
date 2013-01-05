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

import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge
import at.ac.ist.concurrency_swapper.helpers.{VariableAnalysisResult, SomeVars, GatedCommand}
import at.ac.ist.concurrency_swapper.structures.Statement

object TranslationChain extends  VariableAnalysis {
  val finalChain : List[FinalTranslation] = List(AtomicEnd, AtomicStart, Assume, Assert, DefaultTranslation)
  val intermediateChain : List[IntermediateTranslation] = List(Down, Up, PointerDeref, PointerInit)

  private def firstApplicable[x <: Acceptable](chain: List[x], edge:CFAEdge): Option[x] = {
    chain.find(_.accepts(edge))
  }

  private def applyIntermediate(edge:CFAEdge,forFormula:Boolean,originalStatement:Statement) : List[CFAEdge] = {
    var continue = true
    var edges = List(edge)
    while (continue)
    {
      continue = false
      edges = edges.flatMap(e=>firstApplicable(intermediateChain, e) match {case Some(x) => continue = true;if (forFormula) x.translateForFormula(e,originalStatement) else x.translate(e,originalStatement); case None => List(e)})
    }
    return edges
  }

  def translatePoirot(edge: CFAEdge,originalStatement:Statement): List[(CFAEdge,String)] = {
    val edges = applyIntermediate(edge,false,originalStatement)
    val str = edges.map(e => (e,firstApplicable(finalChain, e) match { case Some(x) => x.translatePoirot(e,originalStatement); case None => throw new Exception("No applicable translation")}))
    return str
  }

  def accepts(edge: CFAEdge) = true

  def getFormula(stmt:Statement): GatedCommand = {
    val edge = stmt.getEdge
    val edges = applyIntermediate(edge,true,stmt)
    val gateds = edges.map(e => firstApplicable(finalChain, e) match { case Some(x) => x.getFormula(e); case None => throw new Exception("No applicable translation")})
    return gateds.tail.foldLeft(gateds.head)((s,x) => GatedCommand.SequentialComposition(s,x))
  }

  def printEdge(stmt:Statement): String = {
    val edges = List(stmt.getEdge) // don't apply chain here
    val str = edges.map(e => firstApplicable(finalChain, e) match { case Some(x) => x.printEdge(e); case None => throw new Exception("No applicable translation")})
    return str.mkString(";\n")
  }

  def usedVariables(edge: CFAEdge):VariableAnalysisResult = {
    intermediateChain.find(e => e.accepts(edge) && e.isInstanceOf[VariableAnalysis]) match
    {
      case Some(x) => x.asInstanceOf[VariableAnalysis].usedVariables(edge)
      case None =>
        finalChain.find(e => e.accepts(edge) && e.isInstanceOf[VariableAnalysis]) match
        {
          case Some(x) => x.asInstanceOf[VariableAnalysis].usedVariables(edge)
          case None => SomeVars(Set.empty)
        }
    }
  }

  def changedVariables(edge: CFAEdge):VariableAnalysisResult = {
    intermediateChain.find(e => e.accepts(edge) && e.isInstanceOf[VariableAnalysis]) match
    {
      case Some(x) => x.asInstanceOf[VariableAnalysis].changedVariables(edge)
      case None =>
        finalChain.find(e => e.accepts(edge) && e.isInstanceOf[VariableAnalysis]) match
        {
          case Some(x) => x.asInstanceOf[VariableAnalysis].changedVariables(edge)
          case None => SomeVars(Set.empty)
        }
    }
  }
}
