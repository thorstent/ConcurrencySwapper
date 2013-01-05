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

package at.ac.ist.concurrency_swapper

import ac.at.ist.concurrency_swapper.z3formulas.{Z3Helper, Z3TheoremProver, Z3FormulaManager}
import helpers._
import helpers.After
import helpers.PlaceAtomicSectionFunction
import modelchecker.CtexStmt
import modelchecker.poirot.InvokePoirot
import org.sosy_lab.cpachecker.cfa.objectmodel.{CFAEdge, CFAFunctionDefinitionNode}
import java.io._
import scala.Some
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.LogManager
import structures.{Structure,Function}
import translation.Down
import z3.scala.{Z3Context, Z3Config}
import org.sosy_lab.cpachecker.cfa.{CFA, CFACreator}
import collection.mutable.ListBuffer
import java.util.Date
import scala.Some
import scala.collection.mutable.Map


object MainAlgorithm {
  val advancedPrinting = false

  type stmts = List[StatementOrder.Statement]

  // we return the correct program, number of iterations and time it took in seconds
  // then we return the poirotTime
  def algorithm(threads : List[(CFAFunctionDefinitionNode, String)],
                otherFunctions : List[(CFAFunctionDefinitionNode, String)],
                originalProgram:String, filename:String)
          : (String,Int,Double,Double) = {

    val folder = filename.substring(0,filename.length-2)
    val dir = new File(folder)
    dir.mkdir();
    val files = dir.list()
    var i = 0
    while (i < files.length)
    {
      (new File(folder,files(i))).delete()
      i += 1
    }

    val formulaLog = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("output/formulalog.txt"),"UTF-16"))
    var iteration = 0
    Down.deadlockAnalysis = false
    ParallelAnalysis.MoverCache.clear
    val startDate = new Date()
    var poirotTime = 0.0
    var previousBugid = 0 // this variable holds the bug id of the last bug to see if we fixed something

    var phiList = List(new StatementOrder(threads, otherFunctions, originalProgram))
    while (phiList != List.empty)
    {
      val phi = phiList.head
      phiList = phiList.tail
      val (currentProgram,_) = phi.printProgram(PrintType.Poirot)
      iteration += 1
      // let's print the order
      phi.printOrder("output")
      // and the program
      print ("Starting iteration " + iteration + "... ")
      var writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder + "/iteration%02d.c" format iteration)))
      writer.write(phi.printProgram(PrintType.Normal)._1)
      writer.close()
      writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder + "/iteration%02dp.c" format iteration)))
      writer.write(phi.printProgram(PrintType.Poirot)._1)
      writer.close()

      val (ok, ctex,bugid,time) = modelCheck(originalProgram, phi)
      poirotTime += time
      if (!ok && ctex == null) {
        // the ctex was not ok, we will not continue from here
        println("Done. (Dead End) (Poirot Time: " + time + "s)")
      } else {
        if(!ok) {
          println("Done. (Ctex length: "+ctex.length+" lines) (Poirot Time: " + time + "s)")
          if (advancedPrinting) println("Ctex: " + ctex)
        } else {
          println("Done. (Poirot Time: " + time + "s)")
        }
        if (previousBugid != 0 && previousBugid != bugid) {
          phiList = List.empty // we don't consider previous alternatives because we work on a new bug no
          println("Fixed one bug in iteration " + iteration)
        }
        previousBugid = bugid
        if (ok && Down.deadlockAnalysis) {
          formulaLog.close()
          val seconds = ((new Date()).getTime - startDate.getTime) / 1000.0
          return (phi.printProgram(PrintType.Normal)._1, iteration, seconds, poirotTime)
        }
        if (ok) {
          Down.deadlockAnalysis = true
          println("Starting deadlock analysis")
          phiList = List(phi)
        } else {
          printCtex(ctex,iteration, folder)
          val psi = analyseCtex(ctex, phi, formulaLog)
          //phiList = phi.integrate(psi) ++ phiList
          phiList = phiList ++ phi.integrate(psi)
          phiList.filter(so => so.printProgram(PrintType.Poirot)._1 != currentProgram)
        }
      }
    }
    println("We ran out of reorderings without getting a correct program")
    formulaLog.close()
    return null // this instruction is never executed
  }

  private def modelCheck(program : String, so: StatementOrder) : (Boolean, List[CtexStmt],Int,Double) = {
    InvokePoirot.invokeChecker(so)
  }

  def printCtex(ctex: List[CtexStmt],iteration:Int,folder:String) = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(folder + "/ctex%02d.txt" format iteration)))
    for (c <- ctex) {
      writer.write(c.toString())
    }
    writer.close
  }

  // here we have a list of statements and the thread they belong to
  // we also give him a set of instructions that can actually move (no point in creating a constraint of instructions that cannot
  // move anyhow.
  def analyseCtex(ctex: List[CtexStmt], phi: StatementOrder, out:BufferedWriter) : (List[Constraint[Structure]]) = {
    // returns last thread, the instructions before the context switch and the instructions after the switch
    def findContextSwitch(tail : List[CtexStmt]) : Option[(Int,List[CtexStmt],List[CtexStmt])] = {
      var rest = tail
      var lastThread = tail.head.getThread
      var lastStatements:List[CtexStmt] = List.empty // here we store the list of instructions we saw before the switch
      var lastCtex:CtexStmt = tail.head
      rest = rest.tail
      while (rest != List.empty)
      {
        lastStatements ::= lastCtex
        if (rest.head.getThread != lastThread)
          return Some(lastThread,lastStatements.reverse, rest)
        lastThread = rest.head.getThread
        lastCtex = rest.head
        rest = rest.tail
      }
      return None
    }

    // returns a list of the instructions in the middle and the rest of the instructions of same thread
    def findContextSwitchBack(tail : List[CtexStmt], backThread :Int) : Option[(List[CtexStmt],List[CtexStmt])] = {
      var rest = tail
      val originalThread = tail.head.getThread
      var lastThread:Int = tail.head.getThread
      val lb = new ListBuffer[CtexStmt]
      lb += tail.head
      rest = rest.tail
      while (rest != List.empty)
      {
        if (rest.head.getThread == backThread) {
          // we must find out which middle thread to choose if there are several
          // we either take the first one, or the one with an assertion
          var middle = lb.result
          if (middle.exists(_.getAssertionFailure)) {
            val thread = middle.filter(_.getAssertionFailure)(0).getThread
            middle = middle.filter(_.getThread == thread)
          } else {
            middle = middle.takeWhile(_.getThread == originalThread)
          }
          return Some(middle, rest)
        }
        lastThread = rest.head.getThread
        lb += rest.head
        rest = rest.tail
      }
      return None
    }

    // Returns a partial order
    def generalizeCtex(ctex : List[CtexStmt]) : (List[CtexStmt], Set[(Int, Int)]) = {
      val po = (for(x <- ctex.indices; y <- ctex.indices; if (x < y)) yield {
        ((x, y))
      }).toSet;
      val rpo = po.filter{
        case (x,y) => {
          (ctex(x).getThread != ctex(y).getThread &&
            // !ParallelAnalysis.IsRightMover(ctex(x).getStatement, ctex(y).getStatement, out))
            !ParallelAnalysis.IsLeftMover(ctex(y).getStatement, ctex(x).getStatement, out))
        }
      };
      // println("Remaining constraints: ")
      // for((x, y) <- rpo)
      //   println(ctex(x) + " < " + ctex(y));
      (ctex, rpo)
    }

    def buildEliminationGraph(ctex : List[CtexStmt], cpo : Set[(Int, Int)]) 
      : (List[Int], Set[(Int, Int)], Map[(Int, Int), Constraint[Structure]]) = {
      val nodes = ctex.indices.toList;
      val cpoEdges = cpo;

      val cons = Map[(Int, Int), Constraint[Structure]]();

      // Copy phi 
      val seqEdges : Set[(Int, Int)] = (
        for(x <- nodes; y <- nodes;
          if x != y
          && ctex(x).getThread == ctex(y).getThread
          && phi.isOrdered(ctex(x).getStatement(0).getNumber, ctex(y).getStatement(0).getNumber))
        yield {
          cons((x,y)) = SequentialOrder.asInstanceOf[Constraint[Structure]]
          ((x, y))
        }).toSet;

      val constrEdges = (
        for(x <- nodes; y <- nodes; if x != y && ctex(x).getThread == ctex(y).getThread) yield {
          if(seqEdges.contains((x,y))) {
            // Nothing
            Nil : List[(Int, Int)]
          } else if(seqEdges.contains((y,x))) { 
            // Atomic Section
            // cons((x,y)) = new PlaceAtomicSectionFunction[Structure](line.getStatement(0).getFunctionLevel())
            Nil : List[(Int, Int)]
          } else {
            // Rearrangement constraint
            try {
            cons((x,y)) = new After[Structure](getSameLevel(ctex(x), ctex(y)))
            List((x,y))
            } catch {
              case _ => List.empty
            }
          }
        }).flatten.toSet

      if (advancedPrinting) {println("Conc edges: " + cpoEdges)
      println("Seq edges: " + seqEdges)
      println("Constr edges: " + constrEdges)}

      val graph = (nodes, cpoEdges ++ seqEdges ++ constrEdges, cons);

      graph
    }

    def findCycleFixes(ctex : List[CtexStmt], nodes : List[Int], edges : Set[(Int, Int)], edgeConstraints : Map[(Int, Int), Constraint[Structure]]) : List[Constraint[Structure]] = {
      // We do only two thread bugs
      val badStmt = ctex.findIndexOf(_.getAssertionFailure)

      val t1 = ctex(badStmt).getThread
      val threads = ctex.map(_.getThread).toSet

      var currThread = -1;
      var currBlockNo = 0;
      val blockThreadMap = new scala.collection.mutable.HashMap[Int, Int];
      val blockNos : scala.collection.immutable.Map[Int, Int] = (for(x <- ctex.indices) yield {
        if(ctex(x).getThread == currThread)
          (x, currBlockNo)
        else {
          currBlockNo = currBlockNo + 1;
          currThread = ctex(x).getThread
          blockThreadMap(currBlockNo) = currThread
          (x, currBlockNo)
        }
      }).toMap

      val allPossCons = for(t2 <- threads; if t2 != t1) yield {

        def preference(cyc : ((Int, Int, Int, Int), Constraint[Structure])) : Int = {
          val ((s1,s2,s3,s4), cons) = cyc;
          var pref = 0;
          if (blockNos(s4) == blockNos(s1))
            pref = pref - 10
          else if(!Range(blockNos(s1),blockNos(s4)).tail.exists(x => blockThreadMap(x) == blockThreadMap(blockNos(s2)))) // No other thread block b/w s4 and s1
            pref = pref - 10
          if(ctex(s2).getThread == t1)
            pref = pref + 1
          if(!Range(blockNos(s1),blockNos(s4)).tail.exists(x => blockThreadMap(x) == blockThreadMap(blockNos(s1)))) // No other thread block b/w s4 and s1
            pref = pref + 1
          if(s2 == badStmt || s3 == badStmt)
            pref = pref + 2
          // if(edgeConstraints.get((s2, badStmt)) == Some(SequentialOrder) 
              // && edgeConstraints.get((badStmt, s3)) == Some(SequentialOrder)) {
            // pref = pref + 1
          // }
          pref = pref * 100;
          cons match {
            case After(first, second) => {
              pref = pref - first.getBlockSize*50 - second.getBlockSize*50
              if ((first.isLock() || second.isLock()) && !Down.deadlockAnalysis)
                pref = pref - 5000
            }
            case _ => {
              throw new Exception("This shouldn't happen yet!")
            }
          }
          pref
        }

        val l2rEdges = edges.filter{
          case (x,y) => ctex(x).getThread == t1 && ctex(y).getThread == t2 
        };
        val r2lEdges = edges.filter{
          case (x,y) => ctex(x).getThread == t2 && ctex(y).getThread == t1 
        };

        if (advancedPrinting) {println("Thread: " + t2);
        println("L2R Edges: " + l2rEdges)
        println("R2L Edges: " + r2lEdges)}

        val possCons = (
          for(lr <- l2rEdges; rl <- r2lEdges) yield {
            val ((s1, s2), (s3, s4)) = if(lr._1 < rl._1) (lr, rl) else (rl, lr);

            // Possible cycle  s1 -> s2 -> s3 -> s4
            // println("Trying to switch " + s1 + " and " + s4)
            // println("(s1, s4) : " + edgeConstraints.get((s1,s4)));
            // println("(s4, s1) : " + edgeConstraints.get((s4,s1)));

            val s1s4Switch = {
              edgeConstraints.get((s4,s1)) != None && 
              edgeConstraints.get((s3,s2)) != Some(SequentialOrder) &&
              edgeConstraints.get((s1,s4)) != Some(SequentialOrder) &&
              edgeConstraints.get((s4,s1)) != Some(SequentialOrder)
            }

            if(s1s4Switch)
              Some(((s1,s2,s3,s4), edgeConstraints((s4, s1))), preference((s1,s2,s3,s4), edgeConstraints((s4, s1))))
            else
              None
          }).flatten.toList;

        possCons
      }

      val sortedCons2 = (
        allPossCons
          .toList
          .flatten
          .sortWith((a, b) => a._2 > b._2));

      val sortedCons = sortedCons2.map(_._1._2);

      def unique(set: List[Constraint[Structure]], ls: List[Constraint[Structure]]): List[Constraint[Structure]] = ls match {
        case x :: xs if set contains x => unique(set, xs)
        case x :: xs => x :: unique(x :: set, xs)
        case Nil => Nil
      }

      val uniqueSortedCons = unique(Nil, sortedCons);

      if (advancedPrinting) println("Possible fixes: " + uniqueSortedCons)

      uniqueSortedCons
    }


    // checks how many statements stmt can be moved to the right
    def moveRight(stmt:stmts, stmtlist:List[stmts]) : Int = {
      var res = 0
      for (s<-stmtlist) {
        if (ParallelAnalysis.IsRightMover(stmt, s, out))
          res += 1
        else
          return res
      }
      return res
    }

    // checks how many statements stmt can me moved to the left
    def moveLeft(stmt:stmts, stmtlist:List[stmts]) : Int = {
      var res = 0
      for (s<- stmtlist.reverse) {
        if (ParallelAnalysis.IsLeftMover(stmt, s, out))
          res += 1
        else
          return res
      }
      return res
    }

    // last return value is the rest list of items
    def getNextInterleaving(tail : List[CtexStmt]) : Option[(List[CtexStmt],CtexStmt,CtexStmt,List[CtexStmt],List[CtexStmt])] = {
      var rest = tail
      // an interleaving has to be a contextswitch like this:
      // S1;S2;...S3;S4, S1 and S4 in T1, S2 and S3 in T2
      findContextSwitch(rest) match {
        case None => return None
        case Some((t1,slist,rest1)) =>
          // slist is the list of instructions up to the context switch
          // now let's find the next switch back
          findContextSwitchBack(rest1, t1) match {
            case None => return getNextInterleaving(rest1)
            case Some((stmtlist,rest2)) =>
              // check how these statements move and find the instruction that we want
              // move instruction right until we find one that doesn't move
              val t1 = slist.last.getThread // thread before the context switch
              val t4 = rest2.head.getThread // thread after the context switch
              assert(t1 == t4, "thread order wrong")
              val head = stmtlist.head
              val last = stmtlist.takeWhile(_.getThread == head.getThread).last
              assert(head.getThread == last.getThread, "thread order wrong")
              var startList = slist.reverse
              var endList = rest2
              while (startList!=List.empty && moveRight(startList.head.getStatement, stmtlist.map(_.getStatement)) == stmtlist.length) {
                endList ::= startList.head
                startList = startList.tail
              }
              // if we still have the right thread then this is our instruction for the counterexample
              if (startList!=List.empty)
                return Some(startList, head, last, endList, rest1)

              // if that was not a success we try from the other side
              startList = slist.reverse
              endList = rest2
              while (endList!=List.empty && moveLeft(endList.head.getStatement, stmtlist.map(_.getStatement)) == stmtlist.length) {
                startList ::= endList.head
                endList = endList.tail
              }
              // if we still have the right thread then this is our instruction for the counterexample
              if (endList!=List.empty)
                return Some(startList, head, last, endList, rest1)
              else
                return getNextInterleaving(rest1)
          }
          return None
      }
    }

    def getSameLevel(s1:CtexStmt, s2:CtexStmt):(Structure,Structure) = {
      def getParent(s:(Structure,CtexStmt,Int)):(Structure,CtexStmt,Int) = {
        val (str,ctex,i) = s
        if (str.isInstanceOf[Function]) {
          if (i >= ctex.getCalledFrom.length)
            return (str.getParent(),ctex,i)
          return (ctex.getCalledFrom(i),ctex,i+1)
        } else {
          return (str.getParent(),ctex,i)
        }
      }
      // here we give him the ctex this stems from and the int is the position
      val (s1p,s2p) = Structure.getSameLevel[(Structure,CtexStmt,Int)]((s1.getStatement.head,s1,0),(s2.getStatement.head,s2,0),getParent,s => if (s._1== null) 0 else s._1.getNumber)
      return (s1p._1,s2p._1)
    }


    def addToConstraint(sa:CtexStmt, sb:CtexStmt, constraint:ListBuffer[Constraint[Structure]]) = {
      if (sa.getStatement(0).getNumber != sb.getStatement(0).getNumber) {
          constraint += new After[Structure](getSameLevel(sa, sb))
      }
    }

    var cycles:List[Constraint[Structure]] = List.empty
    if (!Down.deadlockAnalysis) {
      val (cex, cpo) = generalizeCtex(ctex);
      val (nodes, edges, edgeConstraints) = buildEliminationGraph(cex, cpo);
      cycles = findCycleFixes(ctex, nodes, edges, edgeConstraints);
    }

    var rest = ctex
    val primeConstraints = new ListBuffer[Constraint[Structure]] // prime options
    val secondConstraints = new ListBuffer[Constraint[Structure]] // less likely option

    if(!cycles.isEmpty) {
      primeConstraints ++= cycles
    } else {
      // traditional method for finding reorderings
      while (rest != List.empty)
      getNextInterleaving(rest) match {
        case None => rest = List.empty
        case Some((s1l, s2, s3, s4l, newrest)) =>
          addToConstraint(s4l.head, s1l.head, primeConstraints)
          addToConstraint(s3,s2, primeConstraints)

          // we try all the remaining combinations of the outer thread after that
          for (s1<-s1l) {
            for (s4 <- s4l) {
              if (s1 != s1l.head && s4 != s4l.head)
                addToConstraint(s4,s1, secondConstraints)
            }
          }
          rest = newrest
      }
    }
    // lastly, let's place some locks

    // get the line with the assertion failure
    ctex.find(_.getAssertionFailure) match {
      case Some(line) =>
        primeConstraints += new PlaceAtomicSectionFunction[Structure](line.getStatement(0).getFunctionLevel())
        // then we go up and do it for the other function we came from
        for (s <- line.getCalledFrom) {
          primeConstraints += new PlaceAtomicSectionFunction[Structure](s.getFunctionLevel())
        }
      case None => throw new Exception("This shouldn't happen")
    }

    return primeConstraints.result ++ secondConstraints.result
  }

  def processFile(creator:CFACreator, originalFile:String, newFile:String) = {
    println("Processing file " + originalFile)
    // read file from string
    val reader = new FileInputStream(originalFile)
    val originalProgram = Helpers.readToString(reader)
    reader.close()

    val cfa: CFA = creator.parseFileAndCreateCFA(originalFile)
    var funcs = new ListBuffer[(CFAFunctionDefinitionNode,String)]
    var otherFuncs = new ListBuffer[(CFAFunctionDefinitionNode,String)]
    import scala.collection.JavaConversions._
    for (func <- cfa.getAllFunctions.entrySet) {
      if (func.getKey.startsWith("th")) {
        funcs += ((func.getValue, func.getKey))
      } else if (func.getKey != "main") {
        otherFuncs += ((func.getValue, func.getKey))
      }
    }

    val res = algorithm(funcs.result(), otherFuncs.result(), originalProgram, originalFile)
    if (res != null)
    {
      val (newProgram, iterations, time, poirotTime) = res
      val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(newFile)));
      writer.write(newProgram)
      writer.close()

      println("Wrote correct program to " + newFile)
      println("Iterations: " + iterations + ", Time : " + time, "s, Percent in Poirot: " + poirotTime/time*100 + "%")
    } else {
      println("Failed")
    }
    println("\n\n")
  }

  def main(args: Array[String]) {
    val creator = Init

    processFile(creator, args(0), "output/correctProgram.c")
  }


  def Init:CFACreator = {
    var cpaConfig: Configuration = null
    var logManager: LogManager = null
    val config: Configuration.Builder = Configuration.builder
    cpaConfig = config.build
    logManager = new LogManager(cpaConfig)
    val config1: Z3Config = Z3Helper.getEmptyConfig
    val ctx: Z3Context = new Z3Context(config1)
    fm = new Z3FormulaManager(ctx)

    prover = new Z3TheoremProver(ctx)
    prover.init
    Init(cpaConfig, logManager)

    new CFACreator(cpaConfig, logManager)
  }

  var fm: Z3FormulaManager = null
  var prover: Z3TheoremProver = null
  private def Init(cpaConfig: Configuration, logManager: LogManager) {
    GatedCommand.Init(fm)
    FormulaHelpers.Init(fm)
    StateFormula.Init(fm, cpaConfig, logManager)
    ParallelAnalysis.Init(fm, prover)
  }
}
