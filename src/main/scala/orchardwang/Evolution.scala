package orchardwang

import scala.collection.mutable.ArraySeq
import scala.collection.mutable.Queue
import orchardwang.genetic._
import orchardwang.neural._
import orchardwang.util._



/**
 * Instantiate GeneticAlgorithm to Evolution
 * Target Evolution to a genetic algorithm which
 * evolves Foragers in Arenas
 */
class Evolution( populationsize:Int , mxgenerations:Int ) extends GeneticAlgorithm[Forager,Arena]( populationsize:Int , mxgenerations:Int )
{
  // Instantiate from abstract parent class.
  val tester = new ArenaSimulator()

  // Instantiate from abstract parent class.
  def nextGeneration( generation:ArraySeq[Forager],
                      fitnesses:ArraySeq[Double]): ArraySeq[Forager] =
  {
    if( prevGeneration.isEmpty ) {

      // Grow the ongoing archive of statitics for this run.
      val nfentry:FitnessEntry = SimStatistics.fitEntryFactory( fitnesses )
      fitHistory += nfentry

      // Store a record of this generation.
      for( f <- generation ) {
        val copy_f = f.deepCopy()
        prevGeneration += copy_f
      }

      // Store a record of the fitnesses.
      val popt = fitnesses.length
      require( popt == fitnesses.length )
      for( i <- (0 until popt) ) {
        prevFitnesses(i) = fitnesses(i)
      }

      // There were no progenitors. All candidate Foragers persist.
      generation

    } else {

      require(prevGeneration.length == population )
      require(prevGeneration.length == generation.length )


      val selectCand:List[(Char,Double)] = for( i <- (0 until population).toList ) yield {
        if( fitnesses(i) >= prevFitnesses(i) ) {
          ('c',fitnesses(i))   // 'c' for current
        } else {
          ('p',prevFitnesses(i) )   // 'p' for previous
        }
      }
      val sCandEn:List[Char] = selectCand.map( p => p._1 )
      val selectFit:List[Double] = selectCand.map( p => p._2 )
      val selectFitArr:Array[Double] = selectFit.toArray

      /* The next generation is any new mutant Forager whose fitness is greater than
      * or equal to its progenitor.  */
      val sCandX = sCandEn.zipWithIndex
      val nextGen:List[Forager] = for( p <- sCandX ) yield {
        if( p._1 == 'c' ) {
          generation(  p._2 )
        } else {
          prevGeneration.apply( p._2 )
        }
      }

      // Grow the ongoing archive of statitics for this run.
      val nfentry = SimStatistics.fitEntryFactory( selectFit )
      fitHistory += nfentry

      // Store a record of what happened here.
      prevGeneration.clear()
      for( f <- nextGen ) {
        val copy_f = f.deepCopy()
        prevGeneration += copy_f
      }

      // Make a record of the selected fitnesses.
      for( i <- (0 until population) ) {
        prevFitnesses(i) = selectFitArr(i)
      }

      // Return the Foragers as an unsorted array.
      buildArrSeqForager(nextGen)
    }
  }

  // * // * //
  private val fitHistory:Queue[FitnessEntry] = Queue.empty
  private val prevGeneration:Queue[Forager] = Queue.empty
  private val prevFitnesses:Array[Double] = Array.ofDim[Double](population)

  private def buildArrSeqForager( lss:List[Forager] ):ArraySeq[Forager] = {
    val retunq:Array[Forager] = lss.toArray
    val ASstub:ArraySeq[Forager] = ArraySeq()
    val ret = ASstub.++(retunq)
    ret
  }
}
/*
private var mt:ArraySeq[fauxUINT32] = {
    val tmparr = (1 to pN.toInt).toArray
    val mt_ini:ArraySeq[fauxUINT32] = for(i <- tmparr) yield{new fauxUINT32()}
    mt_ini
  }


* */

/*
abstract class GeneticAlgorithm[   A <: Agent , E <: Environment   ]( populationsize:Int , mxgenerations:Int )
{
  private val population:Int = populationsize
  private val maxgenerations:Int = mxgenerations
  val tester:FitnessMachine[A,E]

  def this() = this(100,1000)
  def this(n:Int) = this( n , 1000)

  def nextGeneration( generation:ArraySeq[A],
                      fitnesses:ArraySeq[Double]): ArraySeq[E]
}
*/
