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
}

//

