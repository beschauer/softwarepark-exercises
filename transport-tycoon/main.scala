case class Location(name: String)

val factory = Location("F")
val port = Location("P")
val warehouseA = Location("A")
val warehouseB = Location("B")

object Cargo extends Enumeration {
  type Cargo = Value
  val A, B = Value
}
import Cargo._

case class MeansOfTransport(id: Int, kind: String, time: Int, wayToFactory: Int) extends Ordered[MeansOfTransport] {
  def addTime(t: Int): MeansOfTransport = copy(time = this.time + t + wayToFactory, wayToFactory = t)

  def setTime(t: Int, back: Int): MeansOfTransport = copy(time = t, wayToFactory = back)

  def nextAvailableAtFactory = this.time + this.wayToFactory

  def compare(that: MeansOfTransport): Int = this.nextAvailableAtFactory compare that.nextAvailableAtFactory
}

case object MeansOfTransport {
  def fresh = (id: Int, kind: String) => MeansOfTransport(id, kind, 0, 0)
}

case class Fleet(trucks: List[MeansOfTransport], ship: MeansOfTransport)

case object Fleet {
  val initialFleet = Fleet(List(MeansOfTransport.fresh(1, "truck"), MeansOfTransport.fresh(2, "truck")), MeansOfTransport.fresh(3, "ship"))
}

def findNextAvailableTruck(fleet: Fleet) = fleet.trucks.min

def withTruck(fleet: Fleet, truck: MeansOfTransport) = truck ::
  fleet.trucks.filterNot(t => t.id == truck.id)

def handleCargoA(fleet: Fleet) = {
  val updatedTruck = findNextAvailableTruck(fleet).addTime(1)
  val updatedShip =
    if (fleet.ship.time < updatedTruck.time) fleet.ship.setTime(updatedTruck.time + 4, 4)
    else fleet.ship.addTime(4)
  fleet.copy(trucks = withTruck(fleet, updatedTruck), ship = updatedShip)
}

def handleCargoB(fleet: Fleet) = {
  val updatedTruck = findNextAvailableTruck(fleet).addTime(5)
  fleet.copy(trucks = withTruck(fleet, updatedTruck))
}

def runFleet(cargos: Seq[Cargo]): Fleet = cargos.foldLeft(Fleet.initialFleet)((fleet: Fleet, cargo) =>
  cargo match {
    case A => handleCargoA(fleet)
    case B => handleCargoB(fleet)
  }
)

def run(cargos: Cargo*): Int = {
  val resFleet = runFleet(cargos)
  //  println("----- " + resFleet)
  (resFleet.ship :: resFleet.trucks).map(mot => mot.time).max
}

def showResult(runner: Seq[Cargo] => Int)(cargo: Cargo*)(expected: Int = -1): Unit = {
  val input = cargo
    //    .map(c => c.id)
    .mkString(", ")
  val res = runner(cargo)
  val testInfo = if (expected < 0) ""
    else if (res == expected) s"    passed"
    else s"expected $expected    failed"
  println(s"$input -> $res; $testInfo")
}


showResult(run)(A)(5)
showResult(run)(A, B)(5)
showResult(run)(B, B)(5)
showResult(run)(A, B, B)(7)
showResult(run)(A, A, B, A, B, B, A, B)(29)
showResult(run)(A, B, B, B, A, B, A, A, A, B, B, B)()






