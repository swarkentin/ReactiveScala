package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val daysUntilSick: Int = 6
    val daysUntilDead: Int = 14
    val daysUntilImmune: Int = 16
    val daysUntilHealth: Int = 18

    val timeBetweenMoves: Int = 4

    val startingInfectionRate: Double = 0.01 //1%
    val chanceToInfect: Double = 0.40 //40%  
    val chancetoDie: Double = 0.25 // 25%
    var toInfect: Int = (startingInfectionRate * population.toDouble).toInt
  }

  import SimConfig._

  def addInfectedPerson(): Boolean = {
    if (SimConfig.toInfect > 0) {
      SimConfig.toInfect = SimConfig.toInfect - 1
      true
    } else {
      false
    }
  }

  val persons: List[Person] = (0 to SimConfig.population).toList.map(n => new Person(n))

  class Person(val id: Int) {
    var infected = addInfectedPerson
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    private var actions: List[Simulator#Action] = List()

    def addAction(a: Simulator#Action) {
      actions = a :: actions
      a()
    }

    def move() {

      afterDelay(randomBelow(timeBetweenMoves) + 1) {
        if (!dead) {
          pickNextRoom(row, col)
          catchInfection

          move
        }
      }
      def pickNextRoom(r: Int, c: Int) {
        val left = if (r == 0) Tuple2(roomRows - 1, c) else Tuple2((r - 1) % roomRows, c)
        val right = Tuple2((r + 1) % roomRows, c)
        val up = if (c == 0) Tuple2(r, roomColumns - 1) else Tuple2(r, (c - 1) % roomColumns)
        val down = Tuple2(r, (c + 1) % roomColumns)

        //List all the next rooms
        val nextRooms = List(left, right, up, down)

        //Come up with a list of rooms where nobody in the adjacent room
        //is visibly sick
        val nextUnsickRooms = nextRooms.filter(
          r => !persons.exists(p => p.row == r._1 && p.col == r._2
            && (p.sick || p.dead)))

        //If there is a healthy looking room to move to, pick a random one. Otherwise, don't move.    
        if (nextUnsickRooms.nonEmpty) {
          val nextRoom = nextUnsickRooms(randomBelow(nextUnsickRooms.length))
          row = nextRoom._1
          col = nextRoom._2
        }
      }
    }

    def becomeSick() {
      afterDelay(SimConfig.daysUntilSick) {
        if (!sick) {
          sick = true
          becomeDead()
        }
      }
    }

    def becomeDead() {
      afterDelay(SimConfig.daysUntilDead - SimConfig.daysUntilSick) {
        val willDie = random <= SimConfig.chancetoDie
        if (willDie) {
          dead = true
        } else {
          becomeImmune
        }
      }
    }

    def becomeImmune() {
      afterDelay(SimConfig.daysUntilImmune - SimConfig.daysUntilDead) {
        sick = false;
        immune = true;
        becomeHealthy
      }
    }

    //End of star power =(
    def becomeHealthy() {
      afterDelay(SimConfig.daysUntilHealth - SimConfig.daysUntilImmune) {
        sick = false;
        infected = false;
        immune = false;
      }
    }

    def catchInfection() {
      if (!infected && !dead) {
        val infectedInRoom = persons.exists(p => p.row == row && p.col == col && p.infected)
        if (infectedInRoom) {
          val gotInfected = random <= chanceToInfect
          if (gotInfected) {
            infected = true
            becomeSick()
          }
        }
      }
    }

    addAction(move)
    if (infected)
      addAction(becomeSick)
  }
}
