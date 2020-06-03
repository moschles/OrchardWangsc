package orchardwang
package neural

import scala.math

/**
 * Feed-forward neural network class.
 *  Very simple network with a single hidden layer. 3 total layers.
 *  Inputs to this network are literally a vector of 'inputTotal' numbers.
 *
 * @param inputTotal  total nodes of the input layer
 * @param hiddenTotal   total nodes of the hidden layer.
 * @param outputTotal total nodes of the output layer
 */
class FeedForwardNN( inputTotal:Int , hiddenTotal:Int , outputTotal:Int )
{
  private val output_sz = outputTotal
  private val hidden_sz = hiddenTotal
  private val input_sz = inputTotal

  private val Obias:Array[Double] = Array.ofDim[Double]( output_sz )
  private val Hbias:Array[Double] = Array.ofDim[Double]( hidden_sz )

  private val w_HtO:Array[Array[Double]]        = Array.ofDim[Double]( output_sz , hidden_sz )
  private val w_ItH:Array[Array[Double]]        = Array.ofDim[Double]( hidden_sz , input_sz )

  private val Ospikes:Array[Double] = Array.ofDim[Double]( output_sz )
  private val Hspikes:Array[Double] = Array.ofDim[Double]( hidden_sz )
  private val Ispikes:Array[Double] = Array.ofDim[Double]( input_sz )

  private final val e:Double = scala.math.E
  private final val Oiter:Array[Int] = (0 until output_sz).toArray
  private final val Hiter:Array[Int] = (0 until hidden_sz).toArray
  private final val Iiter:Array[Int] = (0 until input_sz).toArray

  // * // * //

  def this() = this( 2,3,1 )

  def size:(Int,Int,Int) = {
    val ret:(Int,Int,Int) = (input_sz, hidden_sz, output_sz)
    ret
  }

  def applyInput( i:Int , signal:Double ):Unit = {
    Ispikes(i) = signal
  }

  def setInputPattern( pattern:List[Double] ):Unit = {
    val enumer = Iiter.toList
    val enumsig:List[(Int,Double)] = enumer zip pattern
    for( p <- enumsig ) { applyInput(p._1 , p._2) }
  }

  def nodeOutput( i:Int , layer:Char ):Double = layer match {
    case 'O' => Ospikes(i)
    case 'H' => Hspikes(i)
    case 'I' => Ispikes(i)
    case _ => Ospikes(i)
  }

  def getOutputPattern:Array[Double] = {
    val deepcopy:Array[Double] = Array.ofDim( output_sz )
    for( i <- (0 until output_sz) ) {
      deepcopy(i) = Ospikes(i)
    }
    deepcopy
  }

  def weightless():Unit = {
    for {
      i <- Oiter
      j <- Hiter
    } w_HtO(i)(j) = 0.0

    for {
      i <- Hiter
      j <- Iiter
    } w_ItH(i)(j) = 0.0
  }

  def noBias():Unit = {
    for( n <- Oiter ) Obias(n) = 0.0
    for( n <- Hiter ) Hbias(n) = 0.0
  }

  def applySynapse( layer:Char, fromNode:Int , toNode:Int , weight:Double ):Unit = {
    require( layer != 'I' )
    val i = toNode
    val j = fromNode
    layer match {
      case 'O' => w_HtO(i)(j) = weight
      case 'H' => w_ItH(i)(j) = weight
      case _ =>  {
        System.err.println( "FeedForwardNN.applySynapse()  bad layer param.")
        require( i < -99 )
      }
    }
  }

  def applyBias( layer:Char, n:Int , bias:Double ):Unit = {
    layer match {
      case 'O' => Obias(n) = bias
      case 'H' => Hbias(n) = bias
      case _ =>{
        System.err.println( "FeedForwardNN.applyBias()  bad layer param.")
        require( n < -99 )
      }
    }
  }

  def setSynapses( layer:Char , syns:List[(Int,Int,Double)]  ):Unit = {
    require( layer != 'I' )
    layer match {
      case 'O' | 'H' => for( w <- syns ) {
        applySynapse(layer, w._1 , w._2 , w._3 )
      }
      case _ =>{
        System.err.println( "FeedForwardNN.setSynapses()  bad layer param.")
        require( layer == 'O' )
      }
    }
  }

  override def toString():String = {
    val ret = s"FeedForwardNN($input_sz , $hidden_sz , $output_sz)"
    ret
  }


  def cycle():Unit = {
    for( iH <- Hiter ) {
      Hspikes(iH) = Hactivation(iH, Ispikes )
    }
    for( iO <- Oiter ) {
      Ospikes(iO) = Oactivation(iO , Hspikes)
    }
  }

  private def Oactivation( i:Int , insignals:Array[Double] ):Double = {
    val weightedsigs = for( j <- Hiter ) yield {  (w_HtO(i)(j)) * insignals(j) }
    val t = weightedsigs.sum + Obias(i)
    logisticFxn(t)
  }

  private def Hactivation( i:Int , insignals:Array[Double] ):Double = {
    val weightedsigs = for( j <- Iiter ) yield {  insignals(j) *(w_ItH(i)(j)) }
    val t = weightedsigs.sum  + Hbias(i)
    logisticFxn(t)
  }

  private def logisticFxn( t:Double ):Double = {
    1.0 / (  1.0 + scala.math.pow(e , (-t) )   )
  }
}
