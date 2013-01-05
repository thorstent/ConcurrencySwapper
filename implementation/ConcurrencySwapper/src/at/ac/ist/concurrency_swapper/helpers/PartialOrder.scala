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

import java.io.BufferedWriter
import java.io.FileWriter
import collection.mutable.ListBuffer
import collection.mutable

class PartialOrder[T] {

  private var elements: Set[T] = Set.empty
  private var arrowsTo: Map[T, Set[T]] = Map.empty
  private var arrowsFrom: Map[T, Set[T]] = Map.empty
  private var printer: T => String = (t:T) => t.toString
  private var elementBuckets: Map[T, Int] = Map.empty

  def this(po:PartialOrder[T]) {
    this()
    this.elements = po.elements
    this.arrowsTo = po.arrowsTo
    this.arrowsFrom = po.arrowsFrom
    this.printer = po.printer
    this.elementBuckets = po.elementBuckets
  }

  def this(printer: T => String) {
    this()
    this.printer = printer
  }

  def isOrdered(x:T, y:T) : Boolean = arrowsTo.contains(x) && arrowsTo(x).contains(y)

  def setPrinter(printer:T => String) {this.printer = printer}

  def getElements() = elements

  def Clear {
    elements = Set.empty
    arrowsTo = Map.empty // arrow from A to B
    arrowsFrom = Map.empty // arrows from B to A
    elementBuckets = Map.empty
  }

  def AddElement(element: T, bucket: Int) {
    if (elements.contains(element))
    {
      if (elementBuckets(element) != bucket)
        throw new IllegalArgumentException("element was added previously with different bucket")
    }
    else {
      elements += element
      elementBuckets += element -> bucket
    }
    if (!arrowsTo.contains(element)) {
      arrowsTo += element -> Set.empty
    }
    if (!arrowsFrom.contains(element)) {
      arrowsFrom += element -> Set.empty
    }
  }

  def getBucket(element: T) = elementBuckets(element)

  def Contains(element: T) : Boolean = {
    return elements.contains(element)
  }

  private def Circle(element: T, arrows: Map[T,Set[T]]): Boolean = {
    def circled(element: T, seen: Set[T]):Boolean = {
      if (seen.contains(element))
        return true
      else {
        for (e <- arrows(element))
          if (circled(e, seen + element))
            return true
      }
      return false
    }
    return circled(element, Set.empty)
  }

  def Add(pointsFrom: T, pointsTo: T) {
    if (!elements.contains(pointsFrom) || !elements.contains(pointsTo)) {
      throw new IllegalStateException("Elements not yet added to order")
    }
    if (elementBuckets(pointsFrom) != elementBuckets(pointsTo))
      throw new IllegalArgumentException("in different bucket")
    if (Circle(pointsFrom, arrowsTo + (pointsFrom -> (arrowsTo(pointsFrom) + pointsTo))))
      throw new IllegalArgumentException("adding this connection would create a circle")
    if (arrowsTo(pointsFrom).contains(pointsTo))
      throw new IllegalArgumentException("this is already added")
    arrowsTo += pointsFrom -> (arrowsTo(pointsFrom) + pointsTo)
    arrowsFrom += pointsTo -> (arrowsFrom(pointsTo) + pointsFrom)
  }

  // we can assume there are no circles
  // y is the type of the elements we want to sort
  def OrderItems[Y](items : Seq[Y], map:Y => T) : List[Y] = {
    // we create a reverse map for items
    val revMap = new mutable.MapBuilder[T,Y,Map[T,Y]](Map.empty)
    for (i<-items)
      revMap += ((map(i),i))
    val revMap2 = revMap.result()

    val res = new ListBuffer[Y]()
    def addElement(element: Y) : Unit = {
      for (e <- arrowsFrom(map(element)))
      {
        addElement(revMap2(e))
      }
      if (items.contains(element) && !res.contains(element))
      {
        res += element
      }
    }
    for (i <- items) {
      addElement(i)
    }
    return res.toList
  }

  def AddConstraint(constrl: List[Constraint[T]]):Boolean = {
    for (constr <- constrl) {
      constr match {
        case After(e1, e2) => try {Add(e1, e2)} catch { case e:Exception => return false }
      }
    }
    return true
  }

  def ExportToDOT(Filename: String) {
    val out: BufferedWriter = new BufferedWriter(new FileWriter(Filename))
    out.write("digraph " + "Order" + " {\n")
    var numberMap: Map[T, String] = Map.empty
    var counter: Int = 0
    for (e <- elements) {
      numberMap += e -> ("e" + Integer.toString(counter))
      counter += 1
      out.write(numberMap(e))
      out.write(" [shape=\"box\"]" + " [label=\"" + printer(e).replace("\"", "\\\"").replace("\n","") + "\"]\n")
    }
    for (s <- arrowsFrom.keySet) {
      for (t <- arrowsFrom(s)) {
        out.write(numberMap(s) + " -> " + numberMap(t) + "\n")
      }
    }
    out.write("}")
    out.close
  }

}
