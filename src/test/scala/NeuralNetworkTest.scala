import org.scalatest.FunSuite
import org.scalatest._
import org.scalatest.BeforeAndAfterEach
import scala.math.abs
//
import orchardwang.util._
import orchardwang.neural._

class FSNeuralNetworkTest extends FunSuite {
  test("orchardwang.neural.NeuralNetwork test") {
    val nn = new NeuralNetwork(23)
    assert(nn.size === 23)
  }
}


class NeuralNetworkTest extends FlatSpec with Matchers with BeforeAndAfterAll
{
  override def beforeAll() {
    val rngTT = MersenneTwisterSrz.createInstance()
    val rngB = MersenneTwisterSrz.getInstance(1)
    //rngB.seed(123, 456)

    val rngCreateC = MersenneTwisterSrz.createInstance()
    val rngC = MersenneTwisterSrz.getInstance(2)
    //rngC.seed( 4234687, 687234 )

    val rngCreateD = MersenneTwisterSrz.createInstance()
    val rngD = MersenneTwisterSrz.getInstance(3)
    rngD.seed( 7236218, 4423523 )
  }

	private def printlion( slion:Any ):Unit = {
		// println(slion)
	}

  def makeSynapse(rng:MersenneTwisterSrz , mag:Double ):Double = {
    mag * (rng.nextDouble()-0.5)
  }



  "NeuralNetwork flattenToLinear() " should "match growFromLinear()" in {
    val rng = MersenneTwisterSrz.getInstance(3)

    /* Make a random network of 14 nodes  : rnna */
    val rnna = new NeuralNetwork(  14 )
    (0 until 14).foreach(
      b => rnna.applyBias( b , rng.nextDouble() )
    );
    val synsA:List[(Int,Int,Double)] = for{
      i <- (0 until 14).toList
      j <- (0 until 14).toList
      if( i != j )
    } yield (  (  j,i, makeSynapse(rng,28.0)  ) ) // (j,i !!
    rnna.setSynapses( synsA )

    /* Make a random network of 14 nodes  : rnnb */
    val rnnb = new NeuralNetwork(  14 )
    (0 until 14).foreach(
      b => rnnb.applyBias( b , rng.nextDouble() )
    );
    val synsB:List[(Int,Int,Double)] = for{
      i <- (0 until 14).toList
      j <- (0 until 14).toList
      if( i != j )
    } yield (  (  j,i, makeSynapse(rng,28.0)  ) ) // (j,i !!
    rnnb.setSynapses( synsB )

    val identPattern:List[Double] = for( n <- (0 until 14).toList ) yield {
      makeSynapse( rng , 2.0 )
    }

    val idxpair:List[(Int,Double)] = ((0 until 14).toList)  zip identPattern

    // Both networks have the same input pattern.
    for(p <- idxpair) {
      //   def applyInput( i:Int , signal:Double ):Unit = direction match {
      rnna.applyInput( p._1 , p._2 )
      rnnb.applyInput( p._1 , p._2 )
    }

    ;
    // Progress both networks by 18 cycles.
    (0 until 18).foreach( _ => rnna.cycle() )
    ;
    (0 until 18).foreach( _ => rnnb.cycle() )


    val diffsNE = for(  oo <- (0 until 14)  ) yield {
      scala.math.abs(  (rnna.nodeOutput(oo)) - (rnnb.nodeOutput(oo))  )
    }

    // All outputs should differ.
    for( d <- diffsNE ) {
       d should be > 0.000001
    }

    // 6, or more, of the 14 should differ by a larger amount.
    val bigDiff:List[Int] = for( d <- diffsNE.toList ) yield {
      if( d > 0.1 ) { 1 } else { 0 }
    }
    val bigDifftotal = bigDiff.reduceLeft( (a,b) => a+b )
    bigDifftotal should be > 5

    // Extract a linear "genome" for the first network.
    val lgen = rnna.flattenToLinear()

    // Duplicate the "genome" into the 2nd network.
    rnnb.growFromLinear( lgen )

    // Create a random input pattern : idypair
    val ddPattern:List[Double] = for( n <- (0 until 14).toList ) yield {
      makeSynapse( rng , 2.856 )
    }
    val idypair:List[(Int,Double)] = ((0 until 14).toList)  zip ddPattern

    // Apply idypair to both networks as input.
    for(p <- idypair) {
      //   def applyInput( i:Int , signal:Double ):Unit = direction match {
      rnna.applyInput( p._1 , p._2 )
      rnnb.applyInput( p._1 , p._2 )
    }

    ;
    // Progress both networks by 18 cycles.
    (0 until 18).foreach( _ => rnna.cycle() )
    ;
    (0 until 18).foreach( _ => rnnb.cycle() )

    // Their outputs should be identical in every way.
    val diffsEq =  for(  oo <- (0 until 14)  ) yield {
      scala.math.abs(  (rnna.nodeOutput(oo)) - (rnnb.nodeOutput(oo))  )
    }
    for( d <- diffsEq ) {
      d shouldEqual (0.00 +- 0.000000001 )
    }
  }


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
      printlion(osig.toString)
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
      printlion(fourhist(c))
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
    printlion("exciteout")
    for( osig <- exciteout ){
      printlion(osig)
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
   
	printlion("suppressout")
    suppressout.foreach( printlion(_) )
   

    val diffs = for( d <- (2 until 9) ) yield {
      scala.math.abs( suppressout(d) - exciteout(d) )
    }

    printlion("differences")
    for( d <- diffs ) printlion(d);

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

