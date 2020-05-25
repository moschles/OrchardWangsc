package orchardwang
package neural

import scala.collection.mutable.Stack


class NeuralNetwork( n:Int )
{
  private val nodes:Int = n;
  val connectMatrix:Array[Array[Boolean]] = Array.ofDim[Boolean](n,n)
  val w:Array[Array[Double]]        = Array.ofDim[Double](n,n)
  val biases:Array[Double] = Array.ofDim[Double](n)
  val alphaspikes:Array[Double]   = Array.ofDim[Double](n)
  val betaspikes:Array[Double]    = Array.ofDim[Double](n)
  /*
  *  'b'  => next .cycle() outputs to betaspikes
  *  'a'  => next .cycle() outputs to alphaspikes
  *    */
  private var direction:Char = 'b'

  // As this class forces an n-by-n representation, many of the nodes
  //  will not even be connected to the network, or have sparse connectivity.
  //  The following serve as caches to discover orphaned nodes and
  //  skip over them during .cycle()
  val iter:Array[Int] = (0 until n).toArray
  val connections:Array[Int] = for(i<-iter) yield{-1}
  val connectCache:Array[Stack[Int]] = for(i<-iter) yield{
    NeuralNetwork.getEmptyIntegerStack
  }

  def this() = this(8)
  def size:Int = nodes

  def cycle():Unit = ???
  def applyInput( i:Int , signal:Double ):Unit = ???
  def readOutput( i:Int , signal:Double ):Unit = ???
  def quiescent():Unit = ???

  private def cycle_direction( input:Array[Double] , output:Array[Double] ) = ???
}


// We need an inelegant work-around to overcome
// the problem of initializing an empty Stack inside of
// the constructor of a class.
object NeuralNetwork
{
  def getEmptyIntegerStack:Stack[Int] = {
    val eistk:Stack[Int] = Stack(901)
    eistk.pop()
    eistk
  }
}