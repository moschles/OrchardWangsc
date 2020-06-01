package orchardwang

import orchardwang.genetic._
import orchardwang.neural._

class BioNetworks extends Phenotype
{
  private final val ponderCycles = 17
  private final val brainNeurons = 11

  val brain = new  NeuralNetwork( brainNeurons )

  /*   TODO

    TODO : these need to be a separate FeedForwardNN class!

  val plasticityWeights = new FeedForwardNN( 4 , 4 , 1 )
  val plasticityBiases = new FeedForwardNN( 2 , 3 , 1 )
    */
}

//

