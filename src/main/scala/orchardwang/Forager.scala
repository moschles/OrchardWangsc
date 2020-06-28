package orchardwang

import scala.collection.immutable.IndexedSeq
import orchardwang.genetic._
import orchardwang.neural._

class Forager ( initgenotype:Genotype ) extends Agent( initgenotype )
{
  private var compass = 0
  private var posX = 2
  private var posY = 2
  private var randomGeneratorIdx = 0
  // * //

  /**
   * Translate the genes to the agent's phenotype.
   * Calling code that invokes this method on an existing instantiation
   * of an Agent will not modify that Agent.
   * E.g. myAgent remains unmodified after a call to
   *
   *    myAgent.express( myGenotype )
   *
   * However , the above call would return the associated Phenotype.
   * To obtain a modified Agent, use the constructor,
   *
   *   val myAgent = new Agent( myGenotype )
   *
   * @param g the supplied genotype
   * @return Return the resulting Phenotype.
   * @see Agent
   */
  def express( g:Genotype ):Phenotype = {
    val bionets:BioNetworks = new BioNetworks()
    bionets.expressFromGenes( g )
    bionets
  }


  /**
   * Mutate this Agent by a given mutation rate 'rate'
   *
   * @param rate   the rate of mutation.
   * @param params variable number of integer arguments.
   *                 Optionally specify different kinds of mutation.
   * @return the new Agent that is mutated and expressed.
   */
  def mutate( rate: Double , params:Int*  ):Agent = {
    val previousGenes:Genotype = getGenotype
    val mutantGenes = previousGenes.mutate( rate, params:_* )
    val mutantForager:Forager = new Forager(mutantGenes)
    mutantForager
  }

  /**
   * Mate this Agent with 'mother' agent to create a child Agent,
   * or a collection of children.
   *
   * @param params Variable number of arguments for the type of crossover desired,
   *               the number of children, and so on.
   * @param mother cross mother with 'this' Agent
   * @return A linear sequence of one or more children.
   */
  def crossover( mother:Agent , params:Int*  ):IndexedSeq[Agent] = {
    val fatherG:Genotype = this.getGenotype
    val motherG:Genotype = mother.getGenotype
    val childG:Genotype = fatherG.crossover( motherG , params:_* )
    val childForager:Forager = new Forager(childG)
    val childColl:List[Forager] = List( childForager )
    childColl.toIndexedSeq
  }


  def deepCopy( ):Forager = {
    val previousGenes:Genotype = getGenotype
    val dupGenes = previousGenes.deepCopy()
    val dcForager:Forager = new Forager( dupGenes )
    dcForager
  }


/**
 * Coordinates in an Arena follow mathematical notation, where
 * +Y goes up the chalkboard, and -Y goes down the chalkboard.
 * Agent directions follow this axis convention.
 *    agdir = 0   agent is facing in +X
 *    agdir = 1   agent is facing in +Y
 *    agdir = 2   agent is facing in -X
 *    adgir = 3   agent is facing in -Y   */
  def turnRight():Unit = {
    val newcompass = compass match {
      case 0 => 3
      case 1 => 0
      case 2 => 1
      case 3 => 2
      case _ => compass
    }
    compass = newcompass
  }

  def turnLeft():Unit = {
    val newcompass = compass match {
      case 0 => 1
      case 1 => 2
      case 2 => 3
      case 3 => 0
      case _ => compass
    }
    compass = newcompass
  }

  def move( xWall:Int , yWall:Int ): Unit = {
    val newPosition:(Int,Int) = compass match {
      case 0 => if( (posX+1) < xWall ) {
          (posX+1 , posY)
        } else {
          (posX , posY)
        }
      case 1 => if( (posY+1) < yWall ) {
          (posX , posY+1)
        } else {
          (posX , posY)
        }
      case 2 => if( (posX-1) >= 0 ) {
          (posX-1 , posY)
        } else {
          (posX , posY)
        }
      case 3 => if( (posY-1) >= 0 ) {
           (posX , posY-1)
        } else {
          (posX , posY)
        }
      case _ => ( (posX,posY) )
    }

    posX = newPosition._1
    posY = newPosition._2
  }

  def doNothing():Unit = {
    //
  }

  def setOrientation(  orient:(Int,Int,Int) ):Unit = {
    posX = orient._1
    posY = orient._2
    compass = orient._3
  }

  def getOrientation : (Int,Int,Int)  = {
    val ret:(Int,Int,Int) = (posX,posY,compass)
    ret
  }

  def getRandIndex:Int = randomGeneratorIdx

  def setRandIndex( ri:Int ):Unit = {
    val deepcopy:Array[Int] = Array.ofDim[Int](1)
    deepcopy(0) = ri
    randomGeneratorIdx = deepcopy(0)
  }


  def decisionActionCycle( currentPercept:Array[Double] ):(Int,Int) = {
    require( currentPercept.length == 5 )
    val biology:Option[BioNetworks] = asBioNetType( getPhenotype )
    val action:(Int,Int) =  biology match {
      case Some(bn) => {
        val brain:NeuralNetwork = bn.brain
        brain.quiescent()

        /*   1 Apply curentPercept() to neurons 0 thru 4 as "input" */
        for( cp <- (0 until 5) ) {
          brain.applyInput( cp , currentPercept(cp)  )
        }

        /* 2 Cycle the brain network using a synaptic and bias update network.*/
        bn.cycleWithLearning( )

        /* 3 Read the action pair from neurons 9 and 10 */
        val brainDec:NeuralNetwork = bn.brain
        val decisionLayer:(Double,Double) =
          (brainDec.nodeOutput(9) , brainDec.nodeOutput(10));

        /* 4 Round those outputs to integer 0 or 1. */
        val decision:(Int,Int) =
          (roundSignal(decisionLayer._1) , roundSignal(decisionLayer._2))

        /*5 Send that integer pair back as the result of this match clause. */
        decision
      }

      case None => {
        System.err.println("Forager.decisionActionCycle() expected BioNetworks derived class of Phenotype.")
        val wo = 1
        val du = 2
        require(wo==du)
        (0,0)
      }
    }

    action
  }

  // * // * //

  private def asBioNetType( p:Phenotype ):Option[BioNetworks] = p match {
    case ( bionet:BioNetworks ) =>  Some(bionet)
    case _ => None
  }

  private def roundSignal( s:Double ):Int = if( s < 0.50000001 ) { 0 } else { 1 }

}

//




//
