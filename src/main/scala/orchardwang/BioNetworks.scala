package orchardwang

import orchardwang.genetic._
import orchardwang.neural._

class BioNetworks extends Phenotype
{
  private final val ponderCycles = 17
  private final val brainNeurons = 11

  /*
   see  https://i.imgur.com/E8pXzd4.png
    The brain here has an additional input node
     to the right of  "angle". That is used to signal
    that the Forager has eaten something. */
  val brain = new  NeuralNetwork( brainNeurons )


  /*  see https://i.imgur.com/V4SgAor.png  */
  val plasticityWeights = new FeedForwardNN( 4 , 4 , 1 )
  val plasticityBiases = new FeedForwardNN( 2 , 3 , 1 )


  /*
  *   brain 11 neurons , fully recurrent.
  *   synapse update network. 9 neurons.   Feed-forward. (4 input. 4 hidden. 1 output)
  *   bias update network.  6  neurons.  Feed-forward.  (2 input. 3 hidden. 1 output)

  *   4     biases   (exclude 2 on input layer)
  *   9     synaptic weights
  *   5     biases    (exclude 4 on input layer)
  *   20    synaptic weights
  *   11    biases
  *   110   synaptic weights
  * -------
  *  159     total genes
  * */
  /* WARNING this method has side effects on public members. */
  /*  This does not return a new copy and breaks referential transparency. */
  def expressFromGenes( geno:Genotype ):Unit = {
    growToBiasesNet( geno )
    growToWmodNet( geno )
    growToBrain( geno )
  }


  private def growToBiasesNet( geno:Genotype):Unit = {
    //  (0 .. 3)   biases on neurons.
    for( p <-  (0 until 4) ) {
      plasticityBiases.applyBias( 'H' , p , geno.nucleotideBase(p) )
    }

    val ijconn = for{
      j <- (0 until 3)
      i <- (0 until 2)
    } yield(  (i,j) )

    // ( 4 .. 9 )  6 syn weights on lower layer.
    val ijtrip = ijconn zip (4 until 10)

    val ijflat:IndexedSeq[(Int,Int,Int)] = for( t <- ijtrip ) yield {
      ( t._1._1 , t._1._2 , t._2 )
    }

    for( t <- ijflat ) {
      //  def applySynapse( layer:Char, fromNode:Int , toNode:Int , weight:Double ):Unit = {
      plasticityBiases.applySynapse( 'H',  t._1 , t._2 , geno.nucleotideBase( t._3 ) )
    }

    // (10 .. 12)   3 syn weights on upper layer
    plasticityBiases.applySynapse( 'O', 0 , 0 , geno.nucleotideBase( 10 )  )
    plasticityBiases.applySynapse( 'O', 1 , 0 , geno.nucleotideBase( 11 )  )
    plasticityBiases.applySynapse( 'O', 2 , 0 , geno.nucleotideBase( 12 )  )
  }


  private def growToWmodNet( geno:Genotype):Unit = {
    //  (13 .. 17)   biases on neurons.
    for( p <-  (13 until 18) ) {
      plasticityWeights.applyBias( 'H' , p-13 , geno.nucleotideBase(p) )
    }

    val ijconn = for{
      j <- (0 until 4)
      i <- (0 until 4)
    } yield(  (i,j) )

    // ( 18 .. 33 )  16 syn weights on lower layer.
    val ijtrip = ijconn zip (18 until 33)

    val ijflat:IndexedSeq[(Int,Int,Int)] = for( t <- ijtrip ) yield {
      ( t._1._1 , t._1._2 , t._2 )
    }

    for( t <- ijflat ) {
      //  def applySynapse( layer:Char, fromNode:Int , toNode:Int , weight:Double ):Unit = {
      plasticityWeights.applySynapse( 'H',  t._1 , t._2 , geno.nucleotideBase( t._3 ) )
    }

    // (34 .. 37)   4 syn weights on upper layer
    plasticityWeights.applySynapse( 'O', 0 , 0 , geno.nucleotideBase( 34 )  )
    plasticityWeights.applySynapse( 'O', 1 , 0 , geno.nucleotideBase( 35 )  )
    plasticityWeights.applySynapse( 'O', 2 , 0 , geno.nucleotideBase( 36 )  )
    plasticityWeights.applySynapse( 'O', 3 , 0 , geno.nucleotideBase( 37 )  )
  }

  private def growToBrain( geno:Genotype ):Unit = {
    //  (38 .. 48)  11  biases on neurons.
    for( p <-  (38 until 49) ) {
        //      def applyBias( node:Int , b:Double ):Unit
        brain.applyBias( p-38 , geno.nucleotideBase(p) )
    }

    val ijconn = for{
      j <- (0 until 11)
      i <- (0 until 11)
      if( i != j )
    } yield(  (i,j) )

    // ( 49 .. 158 )  110 syn weights on lower layer.
    val ijtrip = ijconn zip (49 until 158)

    val ijflat:IndexedSeq[(Int,Int,Int)] = for( t <- ijtrip ) yield {
      ( t._1._1 , t._1._2 , t._2 )
    }

    val synwtrip:IndexedSeq[(Int,Int,Double)] = for( t <- ijflat ) yield {
      ( t._1 , t._2 , geno.nucleotideBase(t._3) )
    }

    brain.setSynapses(  synwtrip.toList )
  }
}

//

