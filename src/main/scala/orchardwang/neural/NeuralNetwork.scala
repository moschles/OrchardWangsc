package orchardwang
package neural

import scala.math

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
  private final var direction:Char = 'b'
  private final val iter:Array[Int] = (0 until n).toArray
  private final val newline:String = new String(  System.getProperty("line.separator") )
  private final val e:Double = scala.math.E


  // As this class forces an n-by-n representation, many of the nodes
  //  will not even be connected to the network, or have sparse connectivity.
  //  The following serve as caches to discover orphaned nodes and
  //  skip over them during .cycle()

  private val connections:Array[Int] = for(i<-iter) yield{-1}
  private val connectCache:Array[List[Int]] = for(i<-iter) yield{
    (List(901)).take(0)
  }


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
    val weightedsigs = for( j <- iter ) yield {  (w(i)(j)) * insignals(j) }
    val t = weightedsigs.sum + bias(i)

    1.0 / (  1.0 + scala.math.pow(e , (-t) )   )
  }
  private def dtoggle():Unit = {
    val nd = if(direction=='a') 'b' else 'a'
    direction = nd
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
