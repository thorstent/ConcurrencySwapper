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

package at.ac.ist.concurrency_swapper.modelchecker.poirot

import java.io._
import at.ac.ist.concurrency_swapper.modelchecker.{CtexStmt, InvokableChecker}
import at.ac.ist.concurrency_swapper.helpers.{ExpressionHelpers, PrintType, Helpers, StatementOrder}
import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer
import java.util.{Date, Scanner}
import scala._
import scala.Predef._
import at.ac.ist.concurrency_swapper.structures.{Statement, FunctionCallStatement}
import at.ac.ist.concurrency_swapper.translation.{OtherLock, Down}
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge

object InvokePoirot extends InvokableChecker{

  private class CtexLine(thread:Int, function:String, line:Int, addedLater:Boolean) {
    private var returnLines: List[Int] = List.empty
    private var isReturn = false
    private var isAssertionFailure = false

    def getThread = thread
    def getFunction = function
    def getLine = line
    def getReturnLines = returnLines
    def setReturnLines(lines:List[Int]) {returnLines = lines}
    def addReturnLine(line:Int) {returnLines ++= List(line)}

    def getIsAssertionFailure = isAssertionFailure
    def setIsAssertionFailure(assertionFailure:Boolean) {this.isAssertionFailure = assertionFailure}

    def getIsReturn = isReturn
    def setIsReturn(isReturn:Boolean) {this.isReturn = isReturn}

    def getAddedLater = addedLater

  }

  private def writeOutFile(filename:String, content:String) = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("PoirotStage/" + filename),"ASCII"));
    writer.write(content)
    writer.close()
  }

  private def copy(in: InputStream, out: OutputStream) = {
    val data = new Array[Byte](1024)
    var read = 0
    while (in.available() > 0)
    {
      read = in.read(data)
      out.write(data, 0, read)
    }
  }

  private def createStage() = {
    // create folder if doesn't exist and clean out files
    val dir = new File("PoirotStage")
    dir.mkdir();
    val files = dir.list()
    var i = 0
    while (i < files.length)
    {
      (new File("PoirotStage",files(i))).delete()
      i += 1
    }

    // copy our scripts from the resources
    var out = new FileOutputStream("PoirotStage/Makefile")
    var in = this.getClass.getResourceAsStream("PoirotMakefile.txt");
    copy(in, out)
    in.close()
    out.close()
    if(System.getProperty("os.name") == "Linux") {
      out = new FileOutputStream("PoirotStage/analysis.bat")
      in = this.getClass.getResourceAsStream("analysis_linux.txt");
    } else {
      out = new FileOutputStream("PoirotStage/analysis.bat")
      in = this.getClass.getResourceAsStream("analysis.txt");
    }
    copy(in, out)
    in.close()
    out.close()
  }

  private def createProcess() : (String,String) = {
    val pb = if(System.getProperty("os.name") == "Linux") {
      new ProcessBuilder("wine", "cmd",  "/c", "analysis.bat")
    } else {
      new ProcessBuilder("cmd",  "/c", "analysis.bat")
    }
    if(System.getProperty("os.name") == "Linux") {
      val env = pb.environment();
      env.put("WINEDEBUG", "-all");
    }
    //val pb = new ProcessBuilder("cmd",  "/c", "analysis.bat")
    pb.directory(new File("PoirotStage"))
    pb.redirectInput()
    pb.redirectError()
    val p = pb.start()
    val out = Helpers.readToString(p.getInputStream)
    val err = Helpers.readToString(p.getErrorStream)

    val exit = p.waitFor()
    return (out, err)
  }

  private def printProgram(so: StatementOrder) : Map[Int,List[(CFAEdge,StatementOrder.Statement)]] = {
    // thread printing
    val (code,res) = so.printProgram(PrintType.Poirot)
    Helpers.writeToFile("PoirotStage/program.c", code)
    return res
  }

  private val patternTrace = Pattern.compile("^(\\d+)\\s\\d+\\s\\d+\\s\\d+\\s(\\w+)\\|[^\\|]*\\.c\\|(\\d+)\\|")
  // this finds
  private val patternCallstack = Pattern.compile("(\\w+)\\|[^\\|]*\\.c\\|(\\d+)\\|")
  private val patternCommand = Pattern.compile("^(\\d+)\\s\\d+\\s\\d+\\s\\d+\\s(.+)")

  private def parseCtex(statementmap: Map[Int,List[(CFAEdge,StatementOrder.Statement)]]) : (List[CtexLine],Int) = {
    val lb = new ListBuffer[CtexLine]
    val ctexFile = new File("PoirotStage/corral_out_trace.txt")
    if (!ctexFile.exists)
      throw new Exception("Poirot created no counter-example (is it running at all?)")
    val scanner = new Scanner(ctexFile)

    var bug:(CFAEdge,Statement) = null
    var bugThread = 0
    // we have to ignore the last line before a context switch or a

    var lastThread = 0
    var lastLine = 0
    var lastCtex:CtexLine = null
    var lastFunction = ""
    while (scanner.hasNextLine)
    {
      val line = scanner.nextLine()
      if (line != null) {
        val matchesTrace = patternTrace.matcher(line)
        val matchesCommand = patternCommand.matcher(line)
        if (matchesTrace.find())
        {
          val thread = matchesTrace.group(1).toInt
          val functionName = matchesTrace.group(2)
          val lineNumber = matchesTrace.group(3).toInt
          var thisStmt = 0
          if (statementmap.contains(lineNumber))
            thisStmt = statementmap(lineNumber)(0)._2.getNumber
          if (functionName == lastFunction && thread == lastThread && lastLine != lineNumber && statementmap.contains(lastLine) && thisStmt != statementmap(lastLine)(0)._2.getNumber){
            if (!statementmap(lastCtex.getLine)(0)._2.isInstanceOf[FunctionCallStatement])
              lb += lastCtex
          }
          lastCtex = new CtexLine(thread, functionName, lineNumber, false)
          // let us see if there is a stacktrace for this function call
          matchesTrace.usePattern(patternCallstack)
          while (matchesTrace.find())
            lastCtex.addReturnLine(matchesTrace.group(2).toInt)
          lastThread = thread
          lastLine = lineNumber
          lastFunction = functionName
        } else if (matchesCommand.find()) {
          val thread = matchesCommand.group(1).toInt
          val command = matchesCommand.group(2)
          if (command == "ASSERTION FAILS") {
            lastCtex.setIsAssertionFailure(true)
            lb += lastCtex
            bug = statementmap(lastCtex.getLine)(0)
            bugThread = lastCtex.getThread
          }
          else if (command.startsWith("RETURN from") && !lb.result().isEmpty) {
            lb.result().last.setIsReturn(true)
          }
          else if (command == "Done" && !lb.result().isEmpty) {
            lb.result().last.setIsReturn(false) // this is a false return in this case
          }
        }
      }
    }
    scanner.close()
    // if the bug is a deadlock we should use its name to identify the bug
    if (Down.accepts(bug._2.getEdge)) {
      // get name of the lock
      ExpressionHelpers.getFunctionDef(bug._2.getEdge) match {
        case None => throw new Exception("This cannot happen")
        case Some((_,arg1)) =>
          ExpressionHelpers.getName(arg1) match {
            case None => throw new Exception("This cannot happen")
            case Some(name) =>
              // get the name of the other lock involved
              val otherlock = bug._1.asInstanceOf[OtherLock].getOtherLock
              return (lb.result(),name.hashCode+otherlock.hashCode)
          }
      }
    } else {
      // get thread trace
      val bugtrace = getThreadTrace(lb.result)
      if (bugtrace.length < 2)
        return (null,0)
      val prevThread = bugtrace(bugtrace.lastIndexOf(bugThread)-1)
      return (lb.result(),bugThread.hashCode() + bug._2.getNumber.hashCode() + prevThread.hashCode())
    }

  }

  private def getThreadTrace(ctex:List[CtexLine]) = {
    var lastThreads:List[Int] = List.empty // stack of last threads
    for (c <- ctex) {
      if ((lastThreads eq List.empty) || lastThreads.head != c.getThread)
        lastThreads ::= c.getThread
    }
    lastThreads.reverse // so they are in the correct order
  }

  private def completeCtex(ctex: List[CtexLine], statementmap: Map[Int,List[(CFAEdge,StatementOrder.Statement)]]): List[CtexLine] = {
    var ctex2 = ctex
    def lastCtex(thread:Int):CtexLine = ctex.filter(_.getThread == thread).last
    val lastThreads:List[Int] = getThreadTrace(ctex)
    for (t <- lastThreads.reverse.tail) {
      val lCtex = lastCtex(t)
      var nextLine = lCtex.getLine+1
      if (lCtex.getIsReturn) {
         nextLine = lCtex.getReturnLines(0)+1
      }
      var found = false
      if (statementmap.contains(nextLine)) {
        val newCtex = new CtexLine(t,"",nextLine, true)
        newCtex.setReturnLines(lCtex.getReturnLines)
        ctex2 = ctex2 ++ List(newCtex)
        found = true
      }

      var lines = lCtex.getReturnLines
      while (!found && !lines.isEmpty) {
        nextLine = lines.head + 1
        lines = lines.tail
        if (statementmap.contains(nextLine)) {
          val newCtex = new CtexLine(t,"",nextLine, true)
          newCtex.setReturnLines(lines)
          ctex2 = ctex2 ++ List(newCtex)
          found = true
        }
      }
    }
    return ctex2
  }

  private def makeStmtFromCtexLine(l:CtexLine, statementmap: Map[Int,List[(CFAEdge,StatementOrder.Statement)]]) : CtexStmt = {
    new CtexStmt(statementmap(l.getLine).map(_._2),l.getReturnLines.map(statementmap(_)(0)._2),l.getThread,l.getIsAssertionFailure,l.getAddedLater)
  }

  // it returns if there is a bug or not, if there is it also returns the trace and the id of the bug
  // the id is used to determine if the bug is the same or if it is fixed
  // and we return the time we spend in poirot
  def invokeChecker(so: StatementOrder):(Boolean, List[CtexStmt],Int,Double) = {
    createStage()
    val statementmap = printProgram(so)
    val startTime = new Date()
    val (out, err) = createProcess()
    val time = ((new Date()).getTime - startTime.getTime) / 1000.0
    //println(out)
    //println(err)
    if (out.contains("Program has no bugs"))
      return (true, List.empty,0, time)
    else
    {
      var (ctex, bugid) = parseCtex(statementmap)
      if (ctex == null)
        return (false, null,0,time) // useless counterexample, dead end
      if (ctex.isEmpty)
        throw new Exception("no counter example generated")
      ctex = completeCtex(ctex, statementmap)
      return (false, ctex.map(makeStmtFromCtexLine(_,statementmap)), bugid, time)
      return null
    }
  }
}
