import org.scalatest.FunSuite
import org.scalatest._
import org.scalatest.BeforeAndAfterEach
import scala.math.abs
// //
import orchardwang.util._
import orchardwang.neural._


class FeedForwardTest extends FlatSpec with Matchers with BeforeAndAfterAll
{
  override def beforeAll() {
    val rngA = MersenneTwisterSrz.getInstance()
    rngA.seed(414718, 855211)
    val rngTT = MersenneTwisterSrz.createInstance()
    val rngB = MersenneTwisterSrz.getInstance(1)
    rngB.seed(123, 456)

    val rngCreateC = MersenneTwisterSrz.createInstance()
    val rngC = MersenneTwisterSrz.getInstance(2)
    rngC.seed( 5234687, 587234 )
  }

  private def printlion(slion: Any): Unit = {
     //println(slion)
  }

  def makeSynapse(rng:MersenneTwisterSrz , mag:Double ):Double = {
    mag * (rng.nextDouble()-0.5)
  }

  "FeedForwardNN. flattenToLinear()" should "match growFromLinear()" in {
    val rng = MersenneTwisterSrz.getInstance(2)

    /* Make a random 5,3,4 net  : ffa */
    val ffa = new FeedForwardNN(5, 3, 4)
    (0 until 3).foreach(
      b => ffa.applyBias( 'H' , b , rng.nextDouble() )
    );

    (0 until 4).foreach(
      b => ffa.applyBias( 'O' , b , rng.nextDouble() )
    );

    val synsIH:List[(Int,Int,Double)] = for{
      i <- (0 until 3).toList
      j <- (0 until 5).toList
    } yield (  (  j,i, makeSynapse(rng,4.0)  ) ) // (j,i !!

    //   def setSynapses( layer:Char , syns:List[(Int,Int,Double)]  ):Unit = {
    ffa.setSynapses( 'H' , synsIH )

    val synsHO:List[(Int,Int,Double)] = for{
      i <- (0 until 4).toList
      j <- (0 until 3).toList
    } yield (  (  j,i, makeSynapse(rng,4.0) )  ) //  (j,i !!
    ffa.setSynapses( 'O' , synsHO )


    /* Make a another random 5,3,4 net  : ffb */
    val ffb = new FeedForwardNN(5, 3, 4)

    (0 until 3).foreach(
      b => ffb.applyBias( 'H' , b , rng.nextDouble() )
    );

    (0 until 4).foreach(
      b => ffb.applyBias( 'O' , b , rng.nextDouble() )
    );

    val bsynsIH:List[(Int,Int,Double)] = for{
      i <- (0 until 3).toList
      j <- (0 until 5).toList
    } yield (  (  j,i, makeSynapse(rng,4.0)  ) ) // (j,i !!

    //   def setSynapses( layer:Char , syns:List[(Int,Int,Double)]  ):Unit = {
    ffb.setSynapses( 'H' , bsynsIH )

    val bsynsHO:List[(Int,Int,Double)] = for{
      i <- (0 until 4).toList
      j <- (0 until 3).toList
    } yield (  (  j,i, makeSynapse(rng,4.0) )  ) //  (j,i !!
    ffb.setSynapses( 'O' , bsynsHO )

    val identPattern =   List(1.7, 2.0, 3.0 , -1.0 )

    //def setInputPattern( pattern:List[Double] ):Unit = {
    ffa.setInputPattern( identPattern )
    ffb.setInputPattern( identPattern )
    ffa.cycle()
    ffb.cycle()

    /* Outputs should be different */
    val ffbout = ffb.getOutputPattern
    val ffaout = ffa.getOutputPattern
    val diffsEqual = for( n <- (0 until 4) ) yield {
      scala.math.abs( ffbout(n) - ffaout(n) )
    }

    for( d <- diffsEqual ) {
      d should be > (0.1)
    }


    /* Extract a linearized version of ffa */
    val linearA = ffa.flattenToLinear()

    // Duplicate it into ffb
    ffb.growFromLinear( linearA )

    val dpattern = List(1.06, 2.03, 3.05 , -1.023 )
    ffa.setInputPattern( dpattern )
    ffb.setInputPattern( dpattern )
    ffa.cycle()
    ffb.cycle()

    /* Outputs should be identical */
    val ffboutid = ffb.getOutputPattern
    val ffaoutid = ffa.getOutputPattern
    val diffsNE = for( n <- (0 until 4) ) yield {
      ( ffboutid(n) - ffaoutid(n) )
    }

    for( d <- diffsNE ) {
      d shouldEqual (0.0 +- 0.0000001 )
    }
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

//

