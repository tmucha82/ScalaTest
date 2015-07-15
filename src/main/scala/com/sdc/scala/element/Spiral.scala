package com.sdc.scala.element

import com.sdc.scala.element.Element.create

object Spiral {
  val space = create(" ")
  val corner = create("+")

  def spiral(nEdges: Int, direction: Int): Element = {
    if (nEdges == 1)
      corner
    else {
      val sp = spiral(nEdges - 1, (direction + 3) % 4)
      def verticalBar = create('|', 1, sp.height)
      def horizontalBar = create('-', sp.width, 1)
      if (direction == 0)
        (corner beside horizontalBar) above (sp beside space)
      else if (direction == 1)
        (sp above space) beside (corner above verticalBar)
      else if (direction == 2)
        (space beside sp) above (horizontalBar beside corner)
      else
        (verticalBar above corner) beside (space above sp)
    }
  }
}

