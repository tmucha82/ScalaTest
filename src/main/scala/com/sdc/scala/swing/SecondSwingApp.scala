package com.sdc.scala.swing

import scala.swing._
import scala.swing.event.ButtonClicked

/**
  * Created by Tomek on 2017-04-13.
  */
object SecondSwingApp extends SimpleSwingApplication {
  override def top: Frame = new MainFrame {
    title = "Second Swing App"
    val button = new Button{
      text = "Click me"
    }
    listenTo(button)
    val label = new Label {
      text = "No button clicks registered"
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += button
      contents += label
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }

    var nClicks = 0
    reactions += {
      case ButtonClicked(b) => {
        nClicks += 1
        label.text = "Number of button clicks: "+ nClicks
      }
    }
  }
}
