import org.scalatest.FunSuite
import org.scalatest._
import orchardwang.util._
import orchardwang.neural._

class FSNeuralNetworkTest extends FunSuite {
  test("orchardwang.neural.NeuralNetwork test") {
    val nn = new NeuralNetwork(23)
    assert(nn.size === 23)
  }
}


class NeuralNetworkTest extends FlatSpec with Matchers
{
  "NeuralNetwork" should "return size with .size()" in {
    val nn = new NeuralNetwork(23)
    val sz = nn.size
    sz shouldEqual (23)
  }

  "circular network" should "saturate to a stable firing" in {

    val syns:List[(Int,Int,Double)] = List(
      (0,1,   1.0),
      (1,2,   1.0),
      (2,3,   1.0),
      (3,4,   1.0),
      (4,0,   1.0) );

    val nn = new NeuralNetwork(5)

    nn.weightless()
    nn.noBias()
    nn.quiescent( )
    nn.setSynapses( syns )
    nn.applyInput(0, 2.0 )
    for( c <- (0 until 800) ) {
      nn.cycle()
    }

    val allout = for( i <- (0 until 5) ) yield {
      nn.nodeOutput(i)
    }

    for( osig <- allout ){
      osig shouldEqual (0.65904 +- 0.0001)
    }
  }

  "linear network" should " not saturate" in {

    val syns:List[(Int,Int,Double)] = List(
      (0,1,   1.0),
      (1,2,   1.0),
      (2,3,   1.0),
      (3,4,   1.0) );

    val nn = new NeuralNetwork(5)

    nn.weightless()
    nn.noBias()
    nn.quiescent( )
    nn.setSynapses( syns )
    nn.applyInput(0, 2.0 )
    for( c <- (0 until 1600) ) {
      nn.cycle()
    }

    val allout = for( i <- (0 until 5) ) yield {
      nn.nodeOutput(i)
    }

    for( osig <- allout ){
      println(osig)
    }

    for( osig <- allout ){
      osig should be < 0.659
    }
  }



  "Weights" should "add on to node 2" in {
    val nn = new NeuralNetwork(4)

    val syns:List[(Int,Int,Double)] = List(
      (0,1,   1.5174),
      (1,3,   1.5174),
      (3,0,   1.5174),
      (0,2,   0.33),
      (1,2,   0.33),
      (3,2,   0.33) );

    nn.weightless()
    nn.noBias()
    nn.quiescent( )
    nn.setSynapses( syns )
    nn.applyInput(0, 0.659046 )

    val twoh:Array[Double] = Array.ofDim[Double](3)

    nn.cycle()
    twoh(0) = nn.nodeOutput(2)
    nn.cycle()
    twoh(1) = nn.nodeOutput(2)
    nn.cycle()
    twoh(2) = nn.nodeOutput(2)

    twoh(0) should be > 0.55
    twoh(1) should be > twoh(0)
    twoh(2) should be > twoh(1)
  }

  "Negative synaptic weights" should "suppress node 4" in {
    val nn = new NeuralNetwork(9)

    val syns:List[(Int,Int,Double)] = List    (
      (0,1,   1 ),
      (0,2,   1 ),
      (1,5,   1.5174),
      (2,8,   1.5174),
      (5,3,   1.9174),
      (5,6,   1.9174),
      (5,7,   1.9174),
      (8,3,   1.9174),
      (8,6,   1.9174),
      (8,7,   1.9174),
      (3,4,   -1.0),
      (6,4,   -1.0),
      (7,4,   -1.0) );

    nn.weightless()
    nn.noBias()
    nn.quiescent( )
    nn.setSynapses( syns )
    nn.applyInput(0, 1 )

    val fourhist:Array[Double] = Array.ofDim[Double](9)

    for( c <- (0 until 9) ) {
      nn.cycle()
      fourhist(c) = nn.nodeOutput(4)
      println(fourhist(c))
    }

    for( c <- (0 until 9) ) {
      fourhist(c) should be < 0.20
    }

    fourhist(1) should be < fourhist(0)
    fourhist(2) should be < fourhist(1)
    fourhist(3) should be < fourhist(2)
    fourhist(4) should be > fourhist(3)
  }

  "Different input signals" should "produce different output" in {
    val nn = new NeuralNetwork(9)

    val rng = MersenneTwisterSrz.getInstance()
    rng.seed(23162,80385)

    val allreentry:IndexedSeq[(Int,Int,Double)] =
      for{
        i <- (2 until 9)
        j <- (2 until 9)
      } yield ( (i,j, 8.0*(rng.nextDouble()-0.5) ) )

    val zeroconn:IndexedSeq[(Int,Int,Double)] = for(
      j <- (2 until 9) ) yield {
      (0,j,  -2.2 )  }

    val oneconn:IndexedSeq[(Int,Int,Double)] = for(
      j <- (2 until 9) ) yield {
      (1,j, 2.2 )  }


    nn.weightless()
    nn.setSynapses( oneconn.toList ++ zeroconn.toList ++ allreentry.toList )

    nn.noBias()
    nn.quiescent( )
    nn.applyInput(0, 0.0 )
    nn.applyInput(1, 1.0 )  // Excite network

    val maxcycles = 700

    for( c <- (0 until maxcycles) ) {
      nn.cycle()
    }

    val exciteout = for( i <- (0 until 9) ) yield {
      nn.nodeOutput(i)
    }
    println("exciteout")
    for( osig <- exciteout ){
      //println(osig)
    }

    nn.noBias()
    nn.quiescent()
    nn.applyInput(0, 1.0 )
    nn.applyInput(1, 0.0 )  // suppress network

    for( c <- (0 until maxcycles) ) {
      nn.cycle()
    }

    val suppressout = for( i <- (0 until 9) ) yield {
      nn.nodeOutput(i)
    }
    println("suppressout")
    for( osig <- suppressout ){
      //println(osig)
    }

    val diffs = for( d <- (2 until 9) ) yield {
      scala.math.abs( suppressout(d) - exciteout(d) )
    }

    println("differences")
    for( d <- diffs ) println(d);

    // We need "most" of the differences to exceed 0.1
    val exceeded = diffs.map (
      d  => {if( d > 0.1 ) 1 else 0}
    ).toList;

    // At least 5 of them should exceed 0.1
    (exceeded.sum) should be >= 5

    // Sum of differnces should exceed 1.2
    (diffs.sum) > 1.2
  }

}

