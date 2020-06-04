package orchardwang

import orchardwang.genetic._
import orchardwang.util._

object CrossoverMethod {
  final val cutFront:Int = 1
  final val cutBack:Int = 3
  final val homologousFront:Int  =5
  final val homologousBack:Int = 7
}

object MutateMethod {
  final val point:Int = 2
  final val trustRegion:Int = 4
}

/*
* The Genotype of the Forager is literally a monolithic Array of floating-point numbers.
*   This allows various kinds of mutation strategies, some of which mimic the
*  "Trust Region" method from numerical optimization. Crossover becomes trivial.
*
*   lockedLength is calculated in the following manner
*
*   brain 11 neurons , fully recurrent.
*   synapse update network. 9 neurons.   Feed-forward. (4 input. 4 hidden. 1 output)
*   bias update network.  6  neurons.  Feed-forward.  (2 input. 3 hidden. 1 output)
*
*   6     biases
*   9     synaptic weights
*   9     biases
*   20    synaptic weights
*   11    biases
*   110   synaptic weights
* -------
*  165     total genes
* */

class NWBgenes(  createGenes:Array[Double] , callingRNG:MersenneTwisterSrz) extends Genotype
{
  private val rand:MersenneTwisterSrz = callingRNG
  private final val lockedLength:Int = 165
  private final val maxbias:Double = 166.0    // This is 15*11 + 1
  private final val maxSynapse:Double = 15.0
  private val gene:Array[Double] = Array.ofDim[Double](lockedLength)

  //*// Constructor //*//
  require( createGenes.length == lockedLength )
  for( g <- (0 until lockedLength) ) { gene(g) = createGenes(g) }

  /**
   * Create a "garden of Eden" genotype.
   * These are genotypes used in the first generation of a genetic algorithm.
   * @return  a new Genotype with initial random genes.
   */
  def eden():Genotype = {
    val edenG = mutate( 1.01 , MutateMethod.point )
    edenG
  }

  /**
   * Mutate this genotype and return the mutated version.
   *
   * @param rate. The rate of mutation.
   * @param params variable number of integer arguments.
   *                Optionally specify different kinds of mutation.
   * @return a new Genotype after mutation.
   */
  def mutate( rate:Double , params:Int* ):Genotype = {
    // For the time being, only implement point-mutations.
    // Ignore 'params'
    val mutgenes = Array.ofDim[Double](lockedLength)
    for( g <- (0 until lockedLength) ) {
      val thislocus = rand.nextDouble()
      if( thislocus > rate ) {
        mutgenes(g) = gene(g)
      } else {
        val mult = locusMultiplier(g)
        mutgenes(g) = mult * (rand.nextDouble() -0.5)
      }
    }

    val mutant = new NWBgenes( mutgenes , rand )
    mutant
  }

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
  def crossover( that:Genotype , params:Int* ):Genotype = {
    val aparams = params.toArray
    val umethod = if( aparams.isEmpty ) {
      CrossoverMethod.homologousFront
    } else {
      aparams(0)
    }

    val newbases:Array[Double] = Array.ofDim[Double](lockedLength)

    umethod match {
      case CrossoverMethod.homologousFront | CrossoverMethod.homologousBack =>
        for(b <- 0 until lockedLength ) {
          val r = rand.nextInt()
          newbases(b) = if( (r%2) == 0 ) {
              gene(b)
            } else {
              that.nucleotideBase(b)
            }
        }
      case CrossoverMethod.cutFront => {
        require(aparams.length > 1)
        val cuttingLocus = aparams(1)
        for (b <- 0 until cuttingLocus) {
          newbases(b) =  that.nucleotideBase(b)
        }
        for( b <- (cuttingLocus until lockedLength) ) {
          newbases(b) = gene(b)
        }
      }
      case CrossoverMethod.cutBack => {
        require(aparams.length > 1)
        val cuttingLocus = aparams(1)
        for (b <- 0 until cuttingLocus) {
          newbases(b) = gene(b)
        }
        for( b <- (cuttingLocus until lockedLength) ) {
          newbases(b) = that.nucleotideBase(b)
        }
      }
      case _ => System.err.println("NWBgenes.crossover() could not interpret 2nd parameter.")
    }

    new NWBgenes( newbases , rand )
  }

  /**
   * Return the length of this Genotype.
   * This is often needed when calculating an appropriate mutation rate.
   *
   * @return the length of this genotype.
   */
  def length:Int = lockedLength



  /**
   * Return the gene at the given location.
   * @param loc  the locus as an index in the genotype sequence
   * @return  the gene located there.
   */
  def nucleotideBase( loc:Int ):Double = gene(loc)


  /**
   * Return the gene at the given location.
   * @param loc the locus as an index in the genotype sequence
   * @return the gene located there, as an Int
   */
  def inucleotideBase( loc:Int ):Int=0

  /**
   * Nominally, any child genotype inherits the random number generator
   * of its parent. However, this is not appropriate in a multithreaded context,
   * where children will be fitness-tested on a different thread than their parent.
   *
   * @param reRNG  the new generator to assign.
   * @return  return a new Genotype which is copy of the original
   *          but with a new generator.
   */
  def reassignRandomGenerator( reRNG:MersenneTwisterSrz ):Genotype = {
    val shallowgenes = gene;  // Shallow copy the reference.
    val copygeno = new NWBgenes( shallowgenes , reRNG )
    copygeno
  }

  def deepCopy():Genotype = {
    val mutgenes = Array.ofDim[Double](lockedLength)
    for( g <- (0 until lockedLength) ) {
      mutgenes(g) = gene(g)
    }
    val copygeno = new NWBgenes( mutgenes , rand )
    copygeno
  }

  private def locusMultiplier( loc:Int ):Double = {
    if( NWBgenes.isBiasGene(loc) ) {
      (2.0*maxbias)
    } else {
      (2.0*maxSynapse)
    }
  }
}


/**
 * NWBgenes companion object
 */
object NWBgenes
{
  private final val sequences:List[Int] = List(  6,  9,  9, 20, 11,110 )
  private final val seqtype:List[Char]  = List('b','w','b','w','b','w' )
  private val pairs = sequences zip seqtype
  private val reps:List[String] = for( p <-pairs) yield{ p._2.toString * (p._1)}
  private val builder:StringBuilder = new StringBuilder
  for (r<-reps) { builder.append(r) }
  private val bs:String = builder.toString
  private val seqbool = for( c <- bs ) yield { if(c=='b') true else false }
  private val biascheck:Array[Boolean] = seqbool.toArray

  def isBiasGene( gg:Int ):Boolean = biascheck(gg)
}