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

import at.ac.ist.concurrency_swapper.modelchecker.poirot.TranslateForPoirot
import org.sosy_lab.cpachecker.cfa.objectmodel.CFAEdge
import at.ac.ist.concurrency_swapper.helpers.{ExpressionHelpers, GatedCommand}

trait FinalTranslation extends TranslateForPoirot with Acceptable with TranslateForPrint {
  def getFormula(edge:CFAEdge): GatedCommand
}
