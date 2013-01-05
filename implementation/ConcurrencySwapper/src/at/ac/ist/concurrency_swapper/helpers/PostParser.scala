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

import org.sosy_lab.cpachecker.cfa.objectmodel.{CFAFunctionDefinitionNode, CFAEdgeType, CFANode, CFAEdge}
import at.ac.ist.concurrency_swapper.structures._
import org.sosy_lab.cpachecker.cfa.objectmodel.c.{FunctionCallEdge, AssumeEdge}
import org.sosy_lab.cpachecker.cfa.ast.IASTExpression
import collection.mutable.ListBuffer
import java.lang.IndexOutOfBoundsException

object PostParser {
  def getFunctionNotSortable(name: String, node: CFAFunctionDefinitionNode) = {
    val f = getFunction(name, node)
    f.setSortable(false)
    f
  }

  private def getIf(node: CFANode): (CFANode,If) = {
    if (node.getNumLeavingEdges>1) {
      var joinNode: CFANode = null
      var then:List[Structure] = null
      var els:List[Structure] = null
      var condition:IASTExpression = null
      var i = node.getNumLeavingEdges - 1
      while (i >= 0) {
        val edge = node.getLeavingEdge(i)
        if (joinNode == null)
        {
          val (join,t) = getProcessNode(edge.getSuccessor)
          joinNode = join
          then = t
          condition = edge.asInstanceOf[AssumeEdge].getExpression()
        }
        else
        {
          val (join,e) = getProcessNode(edge.getSuccessor)
          if (joinNode != null && join != joinNode)
            throw new Exception("Wrong joining")
          els = e
        }
        i -= 1
      }
      return (joinNode,new If(condition, then, els))
    }
    else
    {
      throw new Exception("Not an if node")
    }
  }

  private def getJoinNodeWhile(node: CFANode): (CFANode,While) = {
    var cond:IASTExpression = null
    var loop:List[Structure] = null
    var nextNode:CFANode = null
    if (node.getNumLeavingEdges == 2)
    {
      val condEdge = node.getLeavingEdge(1)
      cond = condEdge.asInstanceOf[AssumeEdge].getExpression()
      val (_,l) = getProcessNode(condEdge.getSuccessor, node)
      loop = l
      nextNode = node.getLeavingEdge(0).getSuccessor
    }
    else
    {
      val condEdge = node.getLeavingEdge(0)
      cond = ExpressionHelpers.makeIntConst(1)
      val (_,l) = getProcessNode(condEdge.getSuccessor, node)
      loop = l
    }
    val wh = new While(cond,loop)
    return (nextNode,wh)
  }

  private def getProcessNode(node: CFANode,knownJoinNode:CFANode = null) : (CFANode,List[Structure]) = {
    val lb = new ListBuffer[Structure]()

    // helper to see if the while should be finished
    def shouldLoopStop(node:CFANode,ignoreJoinNode:Boolean):Boolean = {
      if (node == null)
        return true
      if (node == knownJoinNode)
        return true
      try {
        if (node.getNumEnteringEdges >=1 && (node.getLeavingEdge(0).getEdgeType eq CFAEdgeType.FunctionReturnEdge))
          return true
        // a bug in CFAchecker forces us to add this check
      } catch
      {
        case e: IndexOutOfBoundsException => return true
      }
      if (!node.isLoopStart && node.getNumEnteringEdges > 1 && !ignoreJoinNode && !node.isInstanceOf[CFAFunctionDefinitionNode])
        return true
      return false
    }

    var nextNode = node
    var ignoreJoinNode = false // Normally we exit the loop as soon as we see a joining, assuming we reached the end of the
    // if. However, if we just returned from an if we should ignore that
    while (!shouldLoopStop(nextNode, ignoreJoinNode)) {
      ignoreJoinNode = false
      if (nextNode.isLoopStart) {
        val(n,wh) = getJoinNodeWhile(nextNode)
        nextNode = n
        lb += wh
      }
      else if (nextNode.getNumLeavingEdges>1) {
        val(n,ifs) = getIf(nextNode)
        nextNode = n
        lb += ifs
        ignoreJoinNode = true
      }
      else if (nextNode.getLeavingEdge(0).getEdgeType == CFAEdgeType.FunctionCallEdge)
      {
        val edge = nextNode.getLeavingEdge(0)
        lb += new FunctionCallStatement(edge.asInstanceOf[FunctionCallEdge])
        // we skip over the function
        nextNode = edge.asInstanceOf[FunctionCallEdge].getSummaryEdge().getSuccessor
      }
      else
      {
        val edge = nextNode.getLeavingEdge(0)
        if (edge.getEdgeType != CFAEdgeType.BlankEdge) {
          lb += new Statement(edge)
        }
        nextNode = edge.getSuccessor
      }
    }

    return (nextNode,lb.result)
  }

  // level 1 makes functions
  def getFunction(name:String,node:CFAFunctionDefinitionNode):Function = {
    val (_,l) = getProcessNode(node)
    val fun = new Function(name,l,node)
    return fun
  }

  // here we collect the names of locks in the program
  def secondRound(p:Program) = {
    def processStructure(str:Structure):Boolean = {
      if (str.isInstanceOf[Statement]) {
        val stmt = str.asInstanceOf[Statement]
        ExpressionHelpers.getFunctionDef(stmt.getEdge) match {
          case None => ()
          case Some((name,arg1)) =>
            if (name == "lock") {
              ExpressionHelpers.getName(arg1) match {
                case None => null
                case Some(name) => stmt.getProgramLevel().addLockName(name)
            }
          }
        }
      }
      return true
    }
    p.processAllStructuresByOne(processStructure)
  }

  def postParse(threads : List[(CFAFunctionDefinitionNode, String)],otherFunctions : List[(CFAFunctionDefinitionNode, String)] , originalProgram:String):Program = {
    val functions = otherFunctions.map(t => PostParser.getFunction(t._2, t._1)) ++ threads.map(t => if (t._2.endsWith("_ns")) PostParser.getFunctionNotSortable(t._2, t._1) else PostParser.getFunction(t._2, t._1))
    val p = new Program(functions, originalProgram, threads.map(_._2).toSet)
    secondRound(p)
    return p
  }
}
