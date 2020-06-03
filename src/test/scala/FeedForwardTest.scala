import org.scalatest.FunSuite
import org.scalatest._
import orchardwang.util._
import orchardwang.neural._

import scala.math.abs

class FeedForwardTest extends FlatSpec with Matchers
{

  val rngA = MersenneTwisterSrz.getInstance()
  rngA.seed( 413718, 856211 )
  val rngTT = MersenneTwisterSrz.createInstance()
  val rngB = MersenneTwisterSrz.getInstance(1)
  rngB.seed( 123,456 )

  private def printlion(slion: Any): Unit = {
     //println(slion)
  }

  "FeedForwardNN" should "return size with .size()" in {
    val ff = new FeedForwardNN(17, 23, 7)
    val sz: (Int, Int, Int) = ff.size
    sz._1 shouldEqual (17)
    sz._2 shouldEqual (23)
    sz._3 shouldEqual (7)
  }

  "FeedForwardNN. zero weights" should "act like missing connections" in {

    val synsIH: List[(Int, Int, Double)] = List(
      (0, 0, 1.0),
      (0, 1, 0),
      (0, 2, 0),
      (1, 0, 0),
      (1, 1, 0),
      (1, 2, 1.0));

    val synsHO: List[(Int, Int, Double)] = List(
      (0, 0, 1.0),
      (1, 0, -3.0),
      (2, 0, 1.0));

    val ff = new FeedForwardNN(2, 3, 1)

    ff.noBias()
    ff.setSynapses('H', synsIH)
    ff.setSynapses('O', synsHO)
    ff.setInputPattern(List(1.0, 2.0))
    ff.cycle()

    val op = ff.getOutputPattern
    op(0) shouldEqual (0.5279347942714185 +- 0.0001)
  }

  "FeedForwardNN. biases" should "change output" in {

    val synsIH: List[(Int, Int, Double)] = List(
      (0, 0, 1.0),
      (0, 1, 0),
      (0, 2, 0),
      (1, 0, 0),
      (1, 1, 0),
      (1, 2, 1.0));

    val synsHO: List[(Int, Int, Double)] = List(
      (0, 0, 1.0),
      (1, 0, -3.0),
      (2, 0, 1.0));

    val ff = new FeedForwardNN(2, 3, 1)

    ff.noBias()
    ff.applyBias('H', 2, -1.0)
    ff.setSynapses('H', synsIH)
    ff.setSynapses('O', synsHO)
    ff.setInputPattern(List(1.0, 2.0))
    ff.cycle()

    val oplow = ff.getOutputPattern
    oplow(0) should be < (0.5277);

    ff.noBias()
    ff.applyBias('H', 2, 1.0)
    ff.setInputPattern(List(1.0, 2.0))
    ff.cycle()

    val ophi = ff.getOutputPattern
    ophi(0) should be > (0.528)
  }

  def makeSynapse(rng:MersenneTwisterSrz , mag:Double ):Double = {
    mag * (rng.nextDouble()-0.5)
  }

  "FeedForwardNN. different input" should "change output" in {

    val rngdi = MersenneTwisterSrz.getInstance()


    val synsIH:List[(Int,Int,Double)] = for{
      i <- (0 until 4).toList
      j <- (0 until 4).toList
    } yield (  (  i,j, makeSynapse(rngdi,4.0)  ) )

    val synsHO:List[(Int,Int,Double)] = for{
      i <- (0 until 4).toList
      j <- (0 until 4).toList
    } yield (  (  i,j, makeSynapse(rngdi,4.0) )  )

    val ff = new FeedForwardNN(4, 4, 4)

    ff.noBias()
    ff.setSynapses('H', synsIH)
    ff.setSynapses('O', synsHO)


    ff.setInputPattern(List(1.0, 0.0, 1.0, 0.0 ))
    ff.cycle()
    val opfirst = ff.getOutputPattern
    opfirst.foreach( printlion(_))

    printlion("___ ")

    ff.setInputPattern(List(-1.0, 1.0, -1.0, 1.0 ))
    ff.cycle()
    val opchange = ff.getOutputPattern
    opchange.foreach( printlion(_))

    val diffs = for( i <- 0 until 4 ) yield{
      scala.math.abs(opfirst(i) - opchange(i))
    }

    diffs.foreach( printlion(_) )

    for( d <- diffs ) {
      d should be > (0.0482)
    }
  }

  "FeedForwardNN. same input" should "give same output" in {

    val rngsi = MersenneTwisterSrz.getInstance(1)

    val synsIH:List[(Int,Int,Double)] = for{
      i <- (0 until 4).toList
      j <- (0 until 4).toList
    } yield (  (  i,j, makeSynapse(rngsi,4.0)  ) )

    val synsHO:List[(Int,Int,Double)] = for{
      i <- (0 until 4).toList
      j <- (0 until 4).toList
    } yield (  (  i,j, makeSynapse(rngsi,4.0) )  )

    val ff = new FeedForwardNN(4, 4, 4)

    ff.noBias()
    ff.setSynapses('H', synsIH)
    ff.setSynapses('O', synsHO)

    ff.setInputPattern(List(1.0, 0.0, 1.0, 0.0 ))
    ff.cycle()
    val opfirst = ff.getOutputPattern

    ff.cycle()
    ff.cycle()
    ff.cycle()
    ff.cycle()
    val opchange = ff.getOutputPattern

    val diffs = for( i <- 0 until 4 ) yield{
      scala.math.abs(opfirst(i) - opchange(i))
    }

    diffs.foreach( printlion(_) )

    for( d <- diffs ) {
      d shouldEqual (0.0 +- 0.000001)
    }
  }
}