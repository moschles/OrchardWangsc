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
  // Member is intentionally public for use by dynamics simulator.
  val brain = new  NeuralNetwork( brainNeurons )


  /*  see https://i.imgur.com/V4SgAor.png  */
  // Members are intentionally public for use by dynamics simlator.
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
}

//

