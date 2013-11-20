package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    checkOrGate(in1, in2, out)
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    checkOrGate(in1, in2, out)
  }

  def checkOrGate(in1: Wire, in2: Wire, out: Wire) {
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal == false, "0 or 0")

    in1.setSignal(true)
    run
    assert(out.getSignal == true, "1 or 0")

    in2.setSignal(true)
    run
    assert(out.getSignal == true, "1 or 1")

    in1.setSignal(false)
    run
    assert(out.getSignal == true, "0 or 1")
  }

  test("demux0") {
    val in1, out1 = new Wire
    demux(in1, List.empty, List(out1))

    in1.setSignal(false)
    run
    assert(out1.getSignal == false, "in:0")

    in1.setSignal(true)
    run
    assert(out1.getSignal == true, "in:1")
  }

  test("demux1") {
    val in1, c1, out1, out2 = new Wire
    demux(in1, List(c1), List(out1, out2))

    in1.setSignal(false)
    c1.setSignal(false)
    run
    assert(out1.getSignal == false && out2.getSignal == false, "in:0, c:0")

    c1.setSignal(true)
    run
    assert(out1.getSignal == false && out2.getSignal == false, "in:0, c:0")

    in1.setSignal(true)
    c1.setSignal(false)
    run
    assert(out1.getSignal == true && out2.getSignal == false, "in:1, c:0")

    c1.setSignal(true)
    run
    assert(out1.getSignal == false && out2.getSignal == true, "in:1, c:1")

  }

  test("demux2") {
    val in1, c1, c2, out1, out2, out3, out4 = new Wire
    val outWires = List(out1, out2, out3, out4)
    demux(in1, List(c1, c2), outWires)

    in1.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(false)
    run
    assertWires(outWires, List(false, false, false, false))    

    c1.setSignal(true)
    run
    assertWires(outWires, List(false, false, false, false))    
    
    c2.setSignal(true)
    run
    assertWires(outWires, List(false, false, false, false))

    in1.setSignal(true)
    c1.setSignal(false)
    c2.setSignal(false)
    run
    assertWires(outWires, List(true,false,false,false))
    
    c1.setSignal(false)
    c2.setSignal(true)
    run
    assertWires(outWires, List(false,true,false,false))
    
    c1.setSignal(true)
    c2.setSignal(false)
    run
    assertWires(outWires, List(false,false,true,false))
    
    c1.setSignal(true)
    c2.setSignal(true)
    run
    assertWires(outWires, List(false,false,false,true))
  }

  def assertWires(wires: List[Wire], signals: List[Boolean]) {
    wires match {      
      case w :: ws =>
        assert(w.getSignal == signals.head)
        assertWires(ws, signals.tail)
      case Nil => ()
    } 
  }

}
