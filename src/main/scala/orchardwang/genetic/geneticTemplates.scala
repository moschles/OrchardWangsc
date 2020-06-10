package orchardwang
package genetic

import java.lang.reflect.GenericArrayType
import scala.collection.LinearSeq
import scala.collection.mutable.ArraySeq






/**
 *  The physical environment in which an agent is tested for fitness.
 */
abstract  class Environment
{

}



/**
 * The genotype of an agent.
 */
abstract class Genotype
{
  /**
   * Create a "garden of Eden" genotype.
   * These are genotypes used in the first generation of a genetic algorithm.
   * @return  a new Genotype with initial random genes.
   */
  def eden():Genotype

  /**
   * Mutate this genotype and return the mutated version.
   *
   * @param rate. The rate of mutation.
   * @param params variable number of integer arguments.
   *                Optionally specify different kinds of mutation. 
   * @return a new Genotype after mutation.
   */
   def mutate( rate:Double , params:Int* ):Genotype

  /**
   * Cross 'this' genotype with 'that' genotype.
   * Return a single, new genotype.
   * Often crossover produces two children with the mother in front,
   * or the mother in the back side of the genes. To produce multiple
   * children, crossover() should be called multiple times each time with
   * different 'params' supplied.
   *
   * @param params variable number of integer arguments.
   *               This can be empty for simple crossovers.
   *               These arguments can specify the type and manner of crossover desired.
   * @param that  the genotype to cross with 'this'.
   * @return the new genotype after crossover.
   */
  def crossover( that:Genotype , params:Int* ):Genotype

  /**
   * Return the length of this Genotype.
   * This is often needed when calculating an appropriate mutation rate.
   *
   * @return the length of this genotype.
   */
  def length:Int

  /**
   * Return the gene at the given location.
   * @param loc  the locus as an index in the genotype sequence
   * @return  the gene located there, as a Double
   */
  def nucleotideBase( loc:Int ):Double

  /**
   * Return the gene at the given location.
   * @param loc the locus as an index in the genotype sequence
   * @return the gene located there, as an Int
   */
  def inucleotideBase( loc:Int ):Int

  def deepCopy():Genotype
}


/**
 * The phenotype of an agent.
 */
abstract class Phenotype
{
  def expressFromGenes( geno:Genotype ):Unit
}

/**
 * An agent class.
 * Use this for a single "candidate" ,
 * or a single "organism", or a single "solution", depending on taste and needs.
 */
abstract class Agent( initgenotype:Genotype )
{
  private val genotype:Genotype = initgenotype.deepCopy()
  private val phenotype:Phenotype = express(initgenotype)
  private var fitness:Double =0.0
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
  def express( g:Genotype ):Phenotype


  /**
   * Mutate this Agent by a given mutation rate 'rate'
   *
   * @param rate   the rate of mutation.
   * @param params variable number of integer arguments.
   *                 Optionally specify different kinds of mutation.
   * @return the new Agent that is mutated and expressed.
   */
  def mutate( rate: Double , params:Int*  ):Agent

  /**
   * Mate this Agent with 'mother' agent to create a child Agent,
   * or a collection of children.
   *
   * @param params Variable number of arguments for the type of crossover desired,
   *               the number of children, and so on.
   * @param mother cross mother with 'this' Agent
   * @return A linear sequence of one or more children.
   */
  def crossover( mother:Agent , params:Int*  ):LinearSeq[Agent]

  // Generic getters and setters.
  def getGenotype:Genotype = genotype
  def getPhenotype:Phenotype = phenotype
  def setFitness( f : Double):Unit = {
    fitness = f
  }
  def getFitness:Double = fitness
}

/**
 * Fitness tests over large populations is the most  computationally expensive
 * portion of a genetic algorithm.
 *
 * This FitnessMachine is relegated to its own class,
 *  since this will likely be multithreaded.
 */
abstract class FitnessMachine
{
  /**
   * Test every Agent in 'gen' within its associated Environment 'envs'.
   * Produce a list of fitnesses of these runs.
   *
   * @param gen a linear sequence of Agents to test.
   * @param envs  one or more Environments to test fitness.
   * @return a linear sequence of fitness values for the associated ith agent.
   */
  def runAllTests( gen:LinearSeq[Agent] , envs:LinearSeq[Environment] ):LinearSeq[Double]
}

/**
 * A genetic algorithm template with a (mostly) non-mutable state.
 * This is a template for a genetic algorithm with a fixed population size,
 * and with a known number of maximum generations.
 *
 * This class is meant to be "cycled" by repeated calls to
 *
 *     genalgo.tester.runAllTests( .... )
 *     genalgo.nextGeneration()
 *
 * The first method call, genalgo.tester.runAllTests(...)  takes
 * a sequence of Agents and returns a corresponding sequence of
 * fitness for them, stored as ArraySeq[Double]
 *
 * The second method call, genalgo.nextGeneration() , accepts
 * the current generation, and produces the next one as output.
 *
 * Calling code is expected to create, and supply these sequences.
 * The calling code decides whether these sequences
 * are mutable or immutable states.
 *
 * @param populationsize
 * @param mxgenerations
 * @see FitnessMachine
 */
abstract class GeneticAlgorithm( populationsize:Int , mxgenerations:Int )
{
  private val population:Int = populationsize
  private val maxgenerations:Int = mxgenerations
  val tester:FitnessMachine

  def this() = this(100,1000)
  def this(n:Int) = this( n , 1000)

  def nextGeneration( generation:ArraySeq[Agent],
                      fitnesses:ArraySeq[Double]): ArraySeq[Agent]
}

//
