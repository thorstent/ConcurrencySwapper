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

import java.io._
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdgeType
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAFunctionDefinitionNode
import org.sosy_lab.cpachecker.cfa.objectmodel.CFANode
import org.sosy_lab.cpachecker.util.predicates.interfaces.Formula
import org.sosy_lab.cpachecker.util.predicates.interfaces.FormulaManager
import org.sosy_lab.cpachecker.util.predicates.interfaces.TheoremProver
import ac.at.ist.concurrency_swapper.z3formulas.{Z3Helper, Z3TheoremProver, Z3FormulaManager}
import scala.Some
import at.ac.ist.concurrency_swapper.translation.TranslationChain
import at.ac.ist.concurrency_swapper.structures.Statement
import z3.scala.Z3Context
import collection.mutable

object ParallelAnalysis {


  def ComposeEdge(edges : List[CFAEdge]) : (GatedCommand, String) = {
    var stmt: String = ""
    var cmd: GatedCommand = null
    for (e <- edges)
    {
      if (EdgeTypeOk(e))
      {
        stmt += ExpressionHelpers.printEdge(e)
        if (!stmt.endsWith(";")) stmt += ";"
        stmt += " "
        val c: GatedCommand = GatedCommand.ConvertEdge(e)
        if (cmd == null)
          cmd = c
        else
          cmd = GatedCommand.SequentialComposition(cmd, c)
      }
    }
    if (edges.length > 1) stmt = "[ " + stmt + " ]"
    (cmd, stmt)
  }

  //output: (command, name, prettyprinted)
  def GetAllEdges(startNode: CFANode, naming: Int=>String, nextNum: Int) : List[List[(GatedCommand,String,String)]] = {
    if (startNode.getNumLeavingEdges == 0)
      return List(List.empty);
    else if (startNode.getNumLeavingEdges == 1)
    {
      val leavingEdge = startNode.getLeavingEdge(0)
      if (leavingEdge.getEdgeType eq CFAEdgeType.FunctionReturnEdge)
        return List(List.empty)
      if (EdgeTypeOk(leavingEdge))
      {
        val laterEdges = GetAllEdges(leavingEdge.getSuccessor, naming, nextNum + 1)
        val (gc,printed) = ComposeEdge(List(leavingEdge))
        val edge = (gc, naming(nextNum), printed)
        return laterEdges.map(edge :: _)
      }
      else
        return GetAllEdges(leavingEdge.getSuccessor, naming, nextNum)
    }
    else
    {
      var i: Int = 0
      var nextNode : CFANode = null
      var edges: List[List[(GatedCommand,String,String)]] = List.empty
      var laterEdges: List[List[(GatedCommand,String,String)]] = null
      while (i < startNode.getNumLeavingEdges) {
        val (node, edgs) = ExpressionHelpers.getUnifyingNode(startNode.getLeavingEdge(i))
        if (nextNode == null) {
          nextNode = node
          laterEdges = GetAllEdges(nextNode, naming, nextNum + 1)
        }
        else
          if (nextNode != node)
          throw new UnsupportedOperationException("this sort of graph is not supported")
        val (gc,printed) = ComposeEdge(edgs)
        val edge = (gc, naming(nextNum) + "." + i, printed)
        edges ++= laterEdges.map(edge :: _)
        i += 1
      }
      return edges
    }
  }

  private def EdgeTypeOk(edge: CFAEdge): Boolean = {
    if (edge.getEdgeType eq CFAEdgeType.BlankEdge) return false
    return true
  }

  def Init(fm1: Z3FormulaManager, prover1: Z3TheoremProver) {
    fm = fm1
    prover = prover1
    z3 = fm1.getZ3()
  }

  private var fm: Z3FormulaManager = null
  private var prover: Z3TheoremProver = null
  private var z3: Z3Context = null

  // gatedcommand, name of the command (unique number), printed command string
  def SequentialComposition(cs1:(GatedCommand,String, String), cs2: (GatedCommand,String, String)) : (GatedCommand, String) = {
    val (c1, _, s1) = cs1
    val (c2, _, s2) = cs2
    (GatedCommand.SequentialComposition(c1, c2), s1 + "; " + s2)
  }

  def SequentialComposition(cs1:(GatedCommand, String), cs2: (GatedCommand, String)) : (GatedCommand, String) = {
    val (c1, s1) = cs1
    val (c2, s2) = cs2
    (GatedCommand.SequentialComposition(c1, c2), s1 + "; " + s2)
  }

  def AnalyseFunctions(func1: (CFAFunctionDefinitionNode,String), func2: (CFAFunctionDefinitionNode,String)) {
    val edgesEx1 = ParallelAnalysis.GetAllEdges(func1._1, _.toString, 1)
    val edgesEx2 = ParallelAnalysis.GetAllEdges(func2._1, n => (97 + n).asInstanceOf[Char].toString, 0)
    var formulaLog: BufferedWriter = null
    formulaLog = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("output/formulas.txt"),"UTF-16"));
    var programLog: BufferedWriter = null
    programLog = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("output/program.txt"),"UTF-16"));

    var ex2 = 0
    var ex1 = 0
    for (edges2 <- edgesEx2)
    {
      ex2 += 1
      for (edges1 <- edgesEx1)
      {
        ex1 += 1
        programLog.write("Execution " + ex1 + "." + ex2 + "\n")
        programLog.write("------------------------------------------------------\n")
        AnalyseExecution(func1._2, func2._2, edges1, edges2, formulaLog, programLog)
        programLog.write("\n\n")
      }
    }

    formulaLog.close
    programLog.close
  }


  private def AnalyseExecution(func1: String, func2: String, edges1: List[(GatedCommand, String, String)], edges2: List[(GatedCommand, String, String)], formulaLog: BufferedWriter, programLog: BufferedWriter) {
    programLog.write(func1 + "() {\n")
    AnalyseFunction(edges1, edges2, formulaLog, programLog)

    programLog.write("}\n\n")
    programLog.write(func2 + "() {\n")
    AnalyseFunction(edges2, edges1, formulaLog, programLog)
    programLog.write("}\n")
  }

  private def minLength(s:String,l:Int) : String = {
    if (s.length > l) s else {
      val chars : Array[Char] = Array.fill(l-s.length)(' ');
      s + new String(chars)
    }
  }

  private def AnalyseFunction(edges1: List[(GatedCommand, String, String)], edges2: List[(GatedCommand, String, String)], formulaLog: BufferedWriter, programLog: BufferedWriter) {
    for (alpha <- edges1) {
      var rightBlocker : Set[String] = Set.empty // all nodes that block us from making this a right-mover
      var leftBlocker : Set[String] = Set.empty // all nodes that block us from making this a left-mover
      for (beta <- edges2) {
        val alphabeta = SequentialComposition(alpha, beta)
        val betaalpha = SequentialComposition(beta, alpha)
        if (!TestSimulation(alphabeta, betaalpha, formulaLog)) rightBlocker += beta._2
        if (!TestSimulation(betaalpha, alphabeta, formulaLog)) leftBlocker += beta._2
      }
      var mover = "N"
      if (leftBlocker.isEmpty && rightBlocker.isEmpty) mover = "LR"
      else if (leftBlocker.isEmpty) mover = "L"
      else if (rightBlocker.isEmpty) mover = "R"
      val right = if (rightBlocker.isEmpty) "" else "(not R: " + rightBlocker.mkString(",") + ") "
      val left = if (leftBlocker.isEmpty) "" else "(not L: " + leftBlocker.mkString(",") + ")"
      mover = "(" + mover + ")"
      programLog.write(minLength("(" + alpha._2 + ")",6) + minLength(alpha._3, 35) + " " + mover + " " + right + left + "\n")
    }
  }

  var MoverCache = new mutable.HashMap[(List[Int],List[Int],Boolean),Boolean]()

  // check if it is a right or leftmover (for leftmover check set right to false)
  private def IsMover(stmt1: List[Statement], stmt2: List[Statement], right: Boolean, out: BufferedWriter):Boolean = {
    MoverCache.get(stmt1.map(_.getNumber),stmt2.map(_.getNumber),right) match {
      case Some(x) => return x
      case None =>
        // we reset z3 as otherwise some scary things happen
        z3.delete()
        z3 = new Z3Context(Z3Helper.getEmptyConfig)
        fm.setZ3(z3)
        prover = new Z3TheoremProver(z3)
        prover.init

        out.write("Org. Commands: (1) " + stmt1(0) + "\n")
        out.write("Org. Commands: (2) " + stmt2(0) + "\n")
        out.flush()

        val stmt1p = stmt1.map(s => (TranslationChain.getFormula(s),TranslationChain.printEdge(s)))
        val stmt2p = stmt2.map(s => (TranslationChain.getFormula(s),TranslationChain.printEdge(s)))
        // combine all the commands
        val stmt1c = stmt1p.tail.foldLeft(stmt1p.head)(SequentialComposition)
        val stmt2c = stmt2p.tail.foldLeft(stmt2p.head)(SequentialComposition)
        out.write("Commands: (1) " + stmt1c._2 + "\t" + stmt1c._1 + "\n")
        out.write("Commands: (2) " + stmt2c._2 + "\t" + stmt2c._1 + "\n")
        val cs12 = SequentialComposition(stmt1c, stmt2c)
        val cs21 = SequentialComposition(stmt2c, stmt1c)
        val res = if (right)
          TestSimulation(cs12, cs21, out)
        else
          TestSimulation(cs21, cs12, out)

        MoverCache += (stmt1.map(_.getNumber),stmt2.map(_.getNumber),right) -> res

        res
    }
  }

  def IsRightMover(stmt1: List[Statement], stmt2: List[Statement], out: BufferedWriter) = IsMover(stmt1,stmt2, true, out)

  def IsLeftMover(stmt1: List[Statement], stmt2: List[Statement], out: BufferedWriter) = IsMover(stmt1,stmt2, false, out)

  def TestSimulation(cs12:(GatedCommand,String), cs21: (GatedCommand,String), out: BufferedWriter) : Boolean = {
    val (c12, s12) = cs12
    val (c21, s21) = cs21
    out.flush() // let's save the file in case we crash
    var (f1, f2) = GatedCommand.Simulates(c12, c21)
    f1 = fm.makeValid(f1)
    f2 = fm.makeValid(f2)
    val f1p = fm.simplify(f1)
    val f2p = fm.simplify(f2)
    prover.push(f1p)
    val sat1: Option[Boolean] = prover.checkSat()
    prover.pop
    prover.push(f2p)
    val sat2: Option[Boolean] = prover.checkSat()
    val model = prover.getZ3Model
    prover.pop
    if (out != null) {
      out.write("Testing: (1;2) " + s12 + "\t" + c12 + "\n")
      out.write("Testing: (2;1) " + s21 + "\t" + c21 + "\n")
      out.write("Discharging (1):")
      out.write("\nNormal:    \t")
      out.write(f1.toString)
      out.write("\nSimplified:\t")
      out.write(f1p.toString)
      out.write("\n")
      out.write("Valid:\t")
      out.write(sat1 match {
        case Some(x) => x.toString();
        case None => "Unknown"
      })
      out.write("\n")
      out.write("Discharging (2):")
      out.write("\nNormal:    \t")
      out.write(f2.toString)
      out.write("\nSimplified:\t")
      out.write(f2p.toString)
      out.write("\n")
      out.write("Valid:\t")
      out.write(sat2 match {
        case Some(x) => x.toString();
        case None => "Unknown"
      })
      if (model != null){
        out.write("\n")
        out.write("Model:\n")
        out.write(model.toString)
      }
      //out.write("\n")
      //out.write("Model:\n")
      out.write("\n\n")
      out.flush()
    }
    return sat2 match {
      case Some(x) =>
        // we must also look at sat1 here
        sat1 match {
          case Some(y) => x && y
          case None => false
        }
      case None => false
    }
  }
}