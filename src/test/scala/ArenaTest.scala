package orchardwang
import org.scalatest.FunSuite
import org.scalatest._

import orchardwang.util._

class ArenaTest extends FlatSpec with Matchers
{
  "Arena" should "return size with .walls()" in {
    val arena = new Arena(100, 111, 10)
    val sz:(Int,Int) = arena.walls
    sz._1 shouldEqual (100)
    sz._2 shouldEqual (111)

    // Wind up the global random number generator.
    val rng = MersenneTwisterSrz.getInstance()
    rng.seed( 944 , 224746 )
  }

  it should "produce eat signal on top of food" in {
    val arena = new Arena(100, 111, 10)
    arena.addFoodAt(23,37)
    arena.addFoodAt(81,7)
    val perc = arena.percept( 23,37,2 )
    perc._1 should be < -90.0
    perc._2 should be < -90.0
    perc._3 shouldEqual 1
    val percb = arena.percept( 81,7,0 )
    percb._1 should be < -90.0
    percb._2 should be < -90.0
    percb._3 shouldEqual 1
  }

  it should "return food at smallest distance" in {
    val arena = new Arena(100, 111, 10)
    arena.addFoodAt(6,6)
    arena.addFoodAt(98,2)
    arena.addFoodAt(50,58)
    arena.addFoodAt(2,98)
    arena.addFoodAt(3,3)
    arena.addFoodAt(98,97)
    val perc = arena.percept( 50,50,1 )
    val exD = 8.0/111.0
    perc._1 shouldEqual (exD +- 0.01)
    perc._2 shouldEqual (0.0 +- 0.01)
    perc._3 shouldEqual 0
  }

  it should "return normalized distances" in {
    val arena = new Arena(100, 111, 10)
    arena.addFoodAt(50,109)
    val perc = arena.percept( 50,1,2 )
    perc._1 shouldEqual (1.0 +- 0.1)
    arena.addFoodAt( 50,3 )
    val percb = arena.percept(50,1,2 )
    percb._1 shouldEqual (0.05 +- 0.05)
  }

  "Arena. random food drop" should "return nearest food" in {
    val arena = new Arena(100, 111, 50)
    arena.addAllFood()
    val perc = arena.percept( 1,1,2 )
    perc._1 should be < 0.5
    perc._3 shouldEqual 0
  }

  "Arena. repeated percept()" should "clear all food" in {
    val arena = new Arena(20,20,50)
    arena.addAllFood()
    arena.addFoodAt(19,19)
    (arena.isEmpty) shouldEqual false
    for{
      y <- (0 until 20)
      x <- (0 until 20)
    } arena.percept(x,y,3)
    (arena.isEmpty) shouldEqual true
  }

  "Arena.percept()" should "return correct angle and distance" in {
    val arena = new Arena(100, 100, 10)
    arena.addFoodAt(98, 97)
    arena.addFoodAt(3, 4)
    arena.addFoodAt(2, 97)
    arena.addFoodAt(55, 56)
    arena.addFoodAt(96, 3)
    val east = arena.percept(50, 50, 0)
    east._1 shouldEqual (0.15 +- 0.15)
    east._2 shouldEqual (0.25 +- 0.2)
    val north = arena.percept(50, 50, 1)
    north._1 shouldEqual (east._1 +- 0.01)
    north._2 shouldEqual (-0.25 +- 0.2)
    val west = arena.percept(50, 50, 2)
    west._1 shouldEqual (north._1 +- 0.01)
    west._2 shouldEqual (-0.75 +- 0.2)
    val south = arena.percept(50, 50, 3)
    south._1 shouldEqual (west._1 +- 0.01)
    south._2 shouldEqual (0.75 +- 0.2)
  }
}