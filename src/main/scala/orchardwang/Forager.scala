package orchardwang

import scala.collection.LinearSeq
import orchardwang.genetic._

/*
* private val genotype:Genotype = initgenotype.deepCopy()
  private val phenotype:Phenotype = express(initgenotype)
  private var fitness:Double =0.0
  * */

class Forager ( initgenotype:Genotype ) extends Agent( initgenotype )
{
  private var compass = 0
  private var posX = 2
  private var posY = 2
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
  def mutate( rate: Double , params:Int*  ):Agent = ???

  /**
   * Mate this Agent with 'mother' agent to create a child Agent,
   * or a collection of children.
   *
   * @param params Variable number of arguments for the type of crossover desired,
   *               the number of children, and so on.
   * @param mother cross mother with 'this' Agent
   * @return A linear sequence of one or more children.
   */
  def crossover( mother:Agent , params:Int*  ):LinearSeq[Agent] = ???
}


//
