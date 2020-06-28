package orchardwang

import scala.collection.mutable.ArraySeq

class FitnessEntry( alowest:Double,
                    aq1:Double, amed:Double, aq3:Double,
                    ahighest:Double )
{
  private val lowest = alowest
  private val Q1 = aq1
  private val median = amed
  private val Q3 = aq3
  private val highest = ahighest

  def getLowest:Double = lowest
  def getQuartiles:(Double, Double, Double) = (Q1,median,Q3)
  def getHighest:Double = highest
}

object SimStatistics
{
  def fitEntryFactory( fitnesses:ArraySeq[Double] ):FitnessEntry =
  {
    val len = fitnesses.length
    val quarter:Int = len / 4
    val wfit:List[Double] = for( i <- (0 until len).toList ) yield { fitnesses(i) }

    commonFactory( len , quarter , wfit )
  }

  def fitEntryFactory( fitnesses:List[Double] ):FitnessEntry =
  {
    val len = fitnesses.length
    val quarter:Int = len / 4

    commonFactory( len , quarter , fitnesses )
  }

  private def commonFactory( len:Int, quarter:Int , wfit:List[Double] ):FitnessEntry =
  {
    val wfitP1 = wfit.sorted
    val wfitP2 = wfitP1.drop( quarter )
    val wfitP3 = wfitP2.drop( quarter )
    val wfitP4 = wfitP3.drop( quarter )
    require( wfitP4.nonEmpty )
    val rmedian = (wfit.sum) /  (len.toDouble)

    val ret = new FitnessEntry( wfitP1.head , wfitP2.head, rmedian, wfitP4.head , wfitP4.last )
    ret
  }
}

