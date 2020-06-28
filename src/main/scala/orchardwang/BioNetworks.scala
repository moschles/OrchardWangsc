package orchardwang

import orchardwang.genetic._
import orchardwang.neural._

class BioNetworks extends Phenotype
{
  private final val ponderCycles = 17
  private final val brainNeurons = 11
  private final val maxbias:Double = 166.0    // This is 15*11 + 1
  private final val maxSynapse:Double = 15.0
  private final val synDex:IndexedSeq[(Int,Int)] = for {
    i <- (0 until brainNeurons)
    j <- (0 until brainNeurons)
    if( i != j )
  } yield ( (i,j) )

  /*
   see  https://i.imgur.com/E8pXzd4.png
    The brain here has an additional input node
     to the right of  "angle". That is used to signal
    that the Forager has eaten something. */
  // Member is intentionally public for use by dynamics simulator.
  val brain = new  NeuralNetwork( brainNeurons )


  /*  see https://i.imgur.com/V4SgAor.png  */
  // Members are intentionally public for use by dynamics simulator.
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
    val biasUpdateGenes:List[Double] = for( g <- (0 until 13).toList ) yield {
      geno.nucleotideBase(g)
    }
     val weightUpdateGenes:List[Double] = for( g <- (13 until 38).toList ) yield {
       geno.nucleotideBase(g)
     }
    val brainConnectome:List[Double] = for( g <- (38 until 159).toList ) yield {
      geno.nucleotideBase(g)
    }

    plasticityBiases.growFromLinear (   biasUpdateGenes.toArray )
    plasticityWeights.growFromLinear( weightUpdateGenes.toArray )
    brain.growFromLinear            (   brainConnectome.toArray )
  }


  def cycleWithLearning(  ):Unit = {
    for( pc <-  (0 until ponderCycles) ) {
      // Cycle the brain
      brain.cycle()

      for( idx <- synDex ) { // For each of the brain's synapses.


        val fromNode = idx._2  // confusing.
        val toNode = idx._1
        val existingWeight = brain.Msynapse( fromNode , toNode )
        val presynapticInputSignal = brain.nodeOutput( fromNode )
        val allInputsWeighted = brain.totalWeightedInput( toNode )
        val existingBias = brain.nodeBias( toNode )
        val PWinputPattern:List[Double] = List( existingWeight,
          presynapticInputSignal,
          allInputsWeighted,
          existingBias );

        // Ask the FF network for a delta-w weight, literally by cycling it.
        plasticityWeights.setInputPattern( PWinputPattern )
        plasticityWeights.cycle()
        val deltawij = plasticityWeights.nodeOutput( 0 , 'O' )

        // Change this synapse by deltawij incrementally.
        // Linearly map its value to the range [-0.005 , 0.005]
        val newSynapse = existingWeight +  ( deltawij*0.005 )
        brain.applySynapse( fromNode , toNode , clamp(newSynapse,maxSynapse) )
      }

      for( toNodeb <- (0 until brain.size) ) { // For each of the brain's neurons.

        val allInputsWeighted = brain.totalWeightedInput( toNodeb )
        val existingBias = brain.nodeBias( toNodeb )
        val PBinputPattern:List[Double] = List( allInputsWeighted , existingBias )

        // Ask the other FF networks for a delta-Beta bias, literally by cycling it.
        plasticityBiases.setInputPattern( PBinputPattern )
        plasticityBiases.cycle( )
        val deltaBij = plasticityBiases.nodeOutput( 0 , 'O' )

        // Alter this node's bias by deltaBij incrementally.
        // Linearly map its value to the range [-0.008 , 0.008]
        val newBias = existingBias + (deltaBij*0.008)
        brain.applyBias( toNodeb ,  clamp(newBias , maxbias) )
      }
    }
  }


  // * // * //

  private def clamp( ad:Double , cl:Double ):Double = {
    require( cl > 0.0 )
    if( ad < 0.0 ) {
      if( ad < (-cl) ) { -cl } else { ad }
    }  else {
      if( ad > cl ) { cl } else { ad }
    }
  }

}

//

