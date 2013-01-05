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

import org.sosy_lab.cpachecker.cfa.ast._
import org.sosy_lab.cpachecker.cfa.objectmodel._
import collection.mutable.ListBuffer
import c.{FunctionDefinitionNode, StatementEdge, AssumeEdge, FunctionCallEdge}
import java.math.BigInteger
import scala.Some
import scala.Some
import at.ac.ist.concurrency_swapper.translation.OtherLock

object ExpressionHelpers {
  def dummyNode(functionName:String) = {
    new CFANode(0, functionName)
  }
  val dummyFunctionExitNode = new CFAFunctionExitNode(0, "")
  val dummyFunctionDefNode = new FunctionDefinitionNode(0, "", null, dummyFunctionExitNode, new java.util.ArrayList[IASTParameterDeclaration](), new java.util.ArrayList[String]()) {}

  def getChangedVariables(stmt: IASTNode): Set[String] = {
    if (stmt.isInstanceOf[IASTDeclaration])
      return Set.empty + ((stmt.asInstanceOf[IASTDeclaration]).getName)
    if (stmt.isInstanceOf[IASTAssignment]) {
      val exp: IASTExpression = (stmt.asInstanceOf[IASTAssignment]).getLeftHandSide
      if (exp.isInstanceOf[IASTIdExpression]) {
        return Set.empty + ((exp.asInstanceOf[IASTIdExpression]).getName)
      }
    }
    return Set.empty
  }

  def getUsedVariables(stmt: IASTNode): Set[String] = {
    import scala.collection.JavaConversions._
    if (stmt.isInstanceOf[IASTIdExpression]) {
      return Set.empty + ((stmt.asInstanceOf[IASTIdExpression]).getName)
    }
    if (stmt.isInstanceOf[IASTDeclaration]) {
      val init: IASTInitializer = (stmt.asInstanceOf[IASTDeclaration]).getInitializer
      if (init != null && init.isInstanceOf[IASTInitializerExpression])
        return getUsedVariables((init.asInstanceOf[IASTInitializerExpression]).getExpression)
    }
    if (stmt.isInstanceOf[IASTAssignment]) {
      val exp: IASTRightHandSide = (stmt.asInstanceOf[IASTAssignment]).getRightHandSide
      return getUsedVariables(exp)
    }
    if (stmt.isInstanceOf[IASTFunctionCallStatement]) {
      val exp = stmt.asInstanceOf[IASTFunctionCallStatement].getFunctionCallExpression
      return exp.getParameterExpressions.foldLeft(Set.empty:Set[String])((list, ast) => getUsedVariables(ast) ++ list)
    }
    return Set.empty
  }

  def getUsedVariables(exp: IASTRightHandSide) : Set[String] = {
    var result : Set[String] = Set.empty
    if (exp.isInstanceOf[IASTBinaryExpression]) {
      result = result ++ getUsedVariables((exp.asInstanceOf[IASTBinaryExpression]).getOperand1)
      result = result ++ getUsedVariables((exp.asInstanceOf[IASTBinaryExpression]).getOperand2)
      return result;
    }
    if (exp.isInstanceOf[IASTIdExpression]) {
      return Set.empty + ((exp.asInstanceOf[IASTIdExpression]).getName)
    }
    return Set.empty
  }

  // get the node where the two join again
  def getUnifyingNode(edge: CFAEdge) : (CFANode,List[CFAEdge]) = {
    var edges = List(edge)
    var currEdge = edge
    var nextnode = edge.getSuccessor
    while (nextnode.getNumEnteringEdges == 1)
    {
      currEdge = nextnode.getLeavingEdge(0)
      edges ++= List(currEdge)
      nextnode = currEdge.getSuccessor
    }
    (nextnode, edges)
  }

  // gives us the next node and the edges
  // if current node is join node then we go from there (if ignoreFirstIsJoin)
  // if current node is split node we return immediatelly
  def getLongestLine(node: CFANode, ignoreFirstIsJoin: Boolean = false) : (CFANode,List[CFAEdge]) = {
    var nextNode = node
    val res = new ListBuffer[CFAEdge]
    var first = ignoreFirstIsJoin
    while ((first || nextNode.getNumEnteringEdges <= 1) && (nextNode.getNumLeavingEdges == 1)) {
      val edge = nextNode.getLeavingEdge(0)
      if (edge.getEdgeType eq CFAEdgeType.FunctionReturnEdge)
        return (nextNode, res.toList)
      res += edge
      nextNode = edge.getSuccessor
      first = false
    }
    return (nextNode, res.toList)
  }

  def printEdge(edge: CFAEdge) : String = {
    if (edge.getEdgeType eq CFAEdgeType.AssumeEdge)
    {
      return "assume (" + edge.getRawAST.toASTString.trim.stripPrefix("[").stripSuffix("]") + ")"
    }
    else
      return edge.getRawAST.toASTString.stripSuffix(";")
  }

  // if function defition returns the name and the first argument
  def getFunctionDef(edge: CFAEdge): Option[(String,IASTExpression)] = {
    if (edge.getRawAST.isInstanceOf[IASTFunctionCallStatement]) {
      val stmt = edge.getRawAST.asInstanceOf[IASTFunctionCallStatement]
      val expr = stmt.getFunctionCallExpression.getFunctionNameExpression
      if (expr.isInstanceOf[IASTIdExpression]) {
        val name = expr.asInstanceOf[IASTIdExpression].getName
        var arg1 : IASTExpression = null
        if (stmt.getFunctionCallExpression.getParameterExpressions().size() > 0)
          arg1 = stmt.getFunctionCallExpression.getParameterExpressions().get(0)
        return Some((name,arg1))
      }
    }
    return None;
  }

  def getName(expr: IASTExpression) : Option[String] = {
    if (expr.isInstanceOf[IASTIdExpression])
      Some(expr.asInstanceOf[IASTIdExpression].getName)
    else None
  }

  def getFunctionName(edge:CFAEdge) = edge.getPredecessor.getFunctionName

  def makeDeclaration(name:String,global:Boolean) = new IASTDeclaration("", null, global, StorageClass.AUTO,
    new IASTNamedTypeSpecifier(false, false, "int"), name, new IASTInitializerExpression("", null, makeIntConst(0)))

  def makeName(name:String,global:Boolean=true) = new IASTIdExpression("", null, null, name, makeDeclaration(name,global))

  def makeBinaryOperation(op1: IASTExpression, op2: IASTExpression, operator: IASTBinaryExpression.BinaryOperator) =
    new IASTBinaryExpression("", null, null, op1, op2, operator)

  def makeUnaryOperation(op1: IASTExpression, operator: IASTUnaryExpression.UnaryOperator) =
    new IASTUnaryExpression("", null, null, op1, operator)

  def makeIntConst(number:Long) = new IASTIntegerLiteralExpression(number.toString, null, null, new BigInteger(number.toString()))

  def makeFunctionCall(name:String, args: List[IASTExpression] = List(), functionName:String) = {
    import scala.collection.JavaConversions._
    new IASTFunctionCallExpression("", null, null, makeName(name), args, null)
  }

  def makeFunctionEdge(name:String, args: List[IASTExpression] = List(), functionName:String): CFAEdge = {
    import scala.collection.JavaConversions._
    val ast:IASTFunctionCallStatement = new IASTFunctionCallStatement(name + "(" + args.map(_.toASTString).mkString(",") + ")",null,makeFunctionCall(name, args, functionName))
    val edge = new FunctionCallEdge(name + "(" + args.map(_.toASTString).mkString(",") + ")", ast, 0, dummyNode(functionName), dummyFunctionDefNode, args, null)
    return edge
  }

  def makeFunctionEdgeT(name:String, args: List[IASTExpression] = List(), functionName:String): CFAEdge with OtherLock = {
    import scala.collection.JavaConversions._
    val ast:IASTFunctionCallStatement = new IASTFunctionCallStatement(name + "(" + args.map(_.toASTString).mkString(",") + ")",null,makeFunctionCall(name, args, functionName))
    val edge = new FunctionCallEdge(name + "(" + args.map(_.toASTString).mkString(",") + ")", ast, 0, dummyNode(functionName), dummyFunctionDefNode, args, null) with OtherLock
    return edge
  }

  def makeAssertEdge(assert:IASTExpression, functionName:String):CFAEdge = {
    makeFunctionEdge("assert", List(assert), functionName)
  }

  def makeAssertEdgeT(assert:IASTExpression, functionName:String):CFAEdge with OtherLock = {
    makeFunctionEdgeT("assert", List(assert), functionName)
  }

  def makeAssignmentEdge(variable:String, value:IASTExpression, functionName:String, global:Boolean = true): CFAEdge = {
    val stmt: IASTStatement = new IASTExpressionAssignmentStatement(variable + " = " + value.toASTString, null, makeName(variable, global), value)
    return new StatementEdge(stmt, 0, dummyNode(functionName), dummyNode(functionName))
  }

  def makeAssignmentEdge(variable:String, value:IASTFunctionCallExpression, functionName:String): CFAEdge = {
    val stmt: IASTStatement = new IASTFunctionCallAssignmentStatement(variable + " = " + value.toASTString(), null, makeName(variable), value)
    return new StatementEdge(stmt, 0, dummyNode(functionName), dummyNode(functionName))
  }

  def makeAssumeEdge(assumption: IASTExpression, functionName:String): CFAEdge = {
    new AssumeEdge(assumption.toASTString, 0, dummyNode(functionName), dummyNode(functionName), assumption, true)
  }
}