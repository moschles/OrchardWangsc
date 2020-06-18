package orchardwang
package neural

import scala.math
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

/*
  RNN  class.   Recurrent Neural Network with zero diagonal connectivity matrix.
 */
class NeuralNetwork( n:Int )
{
  private val nodes:Int = n;

  val connectMatrix:Array[Array[Boolean]] = Array.ofDim[Boolean](n,n)
  val w:Array[Array[Double]]        = Array.ofDim[Double](n,n)
  val bias:Array[Double] = Array.ofDim[Double](n)
  val alphaspikes:Array[Double]   = Array.ofDim[Double](n)
  val betaspikes:Array[Double]    = Array.ofDim[Double](n)
  /*
  *  'b'  => next .cycle() outputs to betaspikes
  *  'a'  => next .cycle() outputs to alphaspikes
  *    */
  private final val iter:Array[Int] = (0 until n).toArray
  private final val newline:String = new String(  System.getProperty("line.separator") )
  private final val e:Double = scala.math.E
  private var direction:Char = 'b'

  /*
  This RNN is going to have plastic synapses for incremental learning over an
  agent's lifetime. The synaptic update function requires the total of
  weighted input signals into a neuron, independent of the bias parameter.
  This TWI  ("total Weighted Input")  must be cached, or we would be forced
  to recalculate them again during the alteration of synapses. */
  private val TWI_cache:Array[Double] = Array.ofDim[Double](nodes)


  /* * */
  /* constructor */
  for( i <-iter){ w(i)(i) = 0.0 }
  /* * */


  def this() = this(8)

  def size:Int = nodes

  def applyInput( i:Int , signal:Double ):Unit = direction match {
    case 'a' => betaspikes(i) = signal
    case _ => alphaspikes(i) = signal
  }

  /*
    direction = 'a';  consider beta and output to alpha
    cycle()
    direction = 'b'  because cycle() toggled it.  Output is still in alpha

    reverse

    direction = 'b';  consider alpha and output to beta
    cycle()
    direction = 'a'  because cycle() toggled it.  Output is still in beta
   */
  def nodeOutput( i:Int ):Double = direction match {
    case 'a' => betaspikes(i)
    case _ => alphaspikes(i)
  }

  def nodeBias( i:Int ):Double = bias(i)

  def Msynapse( fromNode:Int , toNode:Int  ):Double = w(toNode)(fromNode)

  def totalWeightedInput( i:Int ):Double = TWI_cache(i)

  def totalWeightedInputS:Array[Double] = TWI_cache

  def quiescent():Unit = {
    /* Following Orchard+Wang, a quiescent network is set so all spikes are 0.5
        A suppressed or "non-firing" neuron actually emits a positive signal,

                    0.0 < signal <= 0.5
     */
    for( i <-iter ) {
      alphaspikes(i) = 0.5
      betaspikes(i) = 0.5
    }
  }


  def cycle():Unit = {
    val fxnsel = cycle_direction(direction)
    fxnsel( direction )
    dtoggle()
  }

  def weightless():Unit = {
    for {
      i <- iter
      j <- iter
    } w(i)(j) = 0.0
  }

  def noBias():Unit = {
    for( n <- iter ) bias(n) = 0.0
  }

  def applySynapse( fromNode:Int , toNode:Int , weight:Double ):Unit =
    if( fromNode != toNode ) {
      val i = toNode
      val j = fromNode
      w(i)(j) = weight
    }



  def applyBias( node:Int , b:Double ):Unit = {
	  bias(node) = b
  }


  def setSynapses( syns:List[(Int,Int,Double)]  ):Unit = {
    for( weight <- syns ) {
      applySynapse(weight._1  , weight._2 , weight._3)
    }
  }

  override def toString():String = {
    val builder:StringBuilder = new StringBuilder
    builder.append("weights")
    builder.append(newline)
    builder.append("  ")
    for( c <- (0 until size)) {
      builder.append("      ")
      builder.append( columnInt(c) )
    }
    for{
      i <- iter
      j <- iter
    } builder.append( ( weightPrinter(i,j) ) )

    builder.append(newline)
    builder.append("Biases :")
    builder.append(newline)
    for( i <- iter ) {
      if( i > 0 ) {
        builder.append(",")
      }
      builder.append(i.toString + "=")
      val ithB = bias(i)
      builder.append( f"$ithB%.3f" )
    }
    builder.append(newline)
    direction match {
      case 'a' => builder.append("direction : beta -> alpha")
      case _ => builder.append("direction : alpha -> beta")
    }

    builder.append(newline)
    builder.append("alphaspikes")
    for( i <- iter ) {
      if( i > 0 ) {
        builder.append(",")
      }
      builder.append(i.toString + "=")
      val ithB = alphaspikes(i)
      builder.append( f"$ithB%.3f" )
    }

    builder.append(newline)
    builder.append("betaspikes")
    for( i <- iter ) {
      if( i > 0 ) {
        builder.append(",")
      }
      builder.append(i.toString + "=")
      val ithB = betaspikes(i)
      builder.append( f"$ithB%.3f" )
    }

    builder.toString()
  }

  /**
   * flattenToLinear() is used in internal conversions to-and-from Genotype
   * representations.  THis is NOT used for disk backup and restore.
   * @return An array containing the neuron biases concatenated with the
   *         network's synaptic weights.
   */
  def flattenToLinear():Array[Double] =  {
    val Q: Queue[Double] = Queue.empty[Double]

    for (b <- bias) {
      Q += b
    }

    val ijconn = for{
      i <- (0 until nodes)
      j <- (0 until nodes )
      if( i != j )
    } yield(  (i,j) )

    for( s <- ijconn ) {
      Q += w( s._1 )( s._2 )
    }

    require( Q.length == genomeLength )
    val ret = (Q.toList).toArray
    require( ret.length == genomeLength )
    ret
  }

  /**
   *  growFromLinear() is used in internal conversions to-and-from Genotype
   *  representations.  THis is NOT used for disk backup and restore.
   *
   * @param larr , Must be an array with the biases in front concatenated with the
   *             synaptic weights on the back.
   * @return  improperly sized Array 'larr' will fail requirements.
   */
  def growFromLinear( larr:Array[Double]):Unit = {
    val sz = genomeLength
    require( larr.length == sz )

    // Use a stack to avoid calculating any indeces, at all.
    val stkrev:Stack[Double] = Stack.empty[Double]
    stkrev.pushAll( larr )
    require( stkrev.size  == sz )
    val stk:Stack[Double] = stkrev.reverse
    require( stk.size  == sz )

    for ( b <-  (0 until bias.length ) ){
      bias(b) = stk.pop()
    }

    require( stk.isEmpty == false )

    val ijconn = for{
      i <- (0 until nodes )
      j <- (0 until nodes )
      if( i != j )
    } yield(  (i,j) )

    for( s <- ijconn ) {
      w( s._1 )( s._2 ) = stk.pop()
    }

    require( stk.isEmpty )
  }

  // * // * //

  private def cycle_direction( d:Char ):(Char=>Unit) = d match {
    case 'a' => cycle_BA
    case _ => cycle_AB
  }

  private def cycle_AB( d:Char ):Unit = {
    for(  i <- iter ) {
      betaspikes(i) = activation( i , alphaspikes )
    }
  }

  private def cycle_BA( d:Char ):Unit = {
    for(  i <- iter ) {
      alphaspikes(i) = activation( i , betaspikes )
    }
  }


  private def activation( i:Int , insignals:Array[Double] ):Double = {
    val weightedsigs = for( j <- iter if(i !=j) ) yield {  (w(i)(j)) * insignals(j) }
    val twi = weightedsigs.sum   // Total weighted input
    val t = twi + bias(i)
    TWI_cache(i) = twi

    1.0 / (  1.0 + scala.math.pow(e , (-t) )   )
  }

  private def dtoggle():Unit = {
    val nd = if(direction=='a') 'b' else 'a'
    direction = nd
  }


  private def genomeLength:Int = {
    val weighttotal:Int = ((nodes * nodes) - nodes)
    val biasestotal:Int = nodes
    ( weighttotal + biasestotal )
  }

  private def columnInt( ci:Int ):String = if( ci > 9) {
      ci.toString
  } else {
     " " + ci.toString
  }

  private def columnDouble( cd:Double ):String = {
    val pad = if( scala.math.abs(cd) >= 10.0 ) {
      ""
    } else {
      " "
    }
    val wval = if( cd < 0.0 ) {
      val fm = f"$cd%.3f"
      " "+fm
    } else {
       val fm = f"$cd%.3f"
      " +"+fm
    }
    (pad+wval)
  }

  private def weightPrinter( i:Int , j:Int ):String = {
    val gutter =if( j == 0 ) {
      (new String(newline)) + columnInt(i)
    } else {
      ""
    }

    val wstr = if( i == j) {"        "} else { columnDouble(w(i)(j)) }
    (gutter + wstr)
  }

}


//
