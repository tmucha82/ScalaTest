package org.coursera.scala.functional.week4

import org.scalatest.FunSuite

/**
  * Created by tomaszmucha on 26.11.2016.
  */
class BoolTest extends FunSuite {

  test("operator &&") {
    assert(True === (True && True))
    assert(False === (False && True))
    assert(False === (True && False))
    assert(False === (False && False))
  }

  test("operator ||") {
    assert(True === (True || True))
    assert(True === (False || True))
    assert(True === (True || False))
    assert(False === (False || False))
  }

  test("operator !") {
    assert(True === !False)
    assert(False === !True)
  }

  test("operator ==") {
    assert(True === (True == True))
    assert(True === (False == False))
    assert(False === (True == False))
    assert(False === (False == True))
  }

  test("operator !=") {
    assert(False === (True != True))
    assert(False === (False != False))
    assert(True === (True != False))
    assert(True === (False != True))
  }

  test("operator <") {
    assert(False === (True < True))
    assert(False === (False < False))
    assert(False === (True < False))
    assert(True === (False < True))
  }
}
