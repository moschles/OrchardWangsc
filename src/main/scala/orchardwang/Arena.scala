package orchardwang


import orchardwang.genetic._
import orchardwang.util._
import scala.collection.mutable.HashSet
import scala.annotation.tailrec

/**
 * Coordinates in an Arena follow mathematical notation, where
 * +Y goes up the chalkboard, and -Y goes down the chalkboard.
 * Agent directions follow this axis convention.
 *    agdir = 0   agent is facing in +X
 *    agdir = 1   agent is facing in +Y
 *    agdir = 2   agent is facing in -X
 *    adgir = 3   agent is facing in -Y
 *
 * @param w  width of Arena to create
 * @param h height of Arena to create
 * @param foodtotal total amount of food to add at one call to addAllFood()
 */
class Arena ( w:Int , h:Int , foodtotal:Int) extends Environment
{
  private val width = w
  private val height = h
  private val arena_sz:Double = (scala.math.max(width,height)).toDouble
  private val foodPerDrop:Int = foodtotal
  private val food:HashSet[(Int,Int)] = HashSet.empty[(Int,Int)]
  private val distance_factor = 1.0 / arena_sz

  // * // * //

  /**
   * The distance is normalized so that a value of 1 corresponds
   * to 100 grid units (the width of the arena). The angle is also
   * normalized so that a value of 0 corresponds to straight ahead,
   * 0.5 and -0.5 correspond to directly left and right (respectively),
   * and a value of (+/- 1) indicates the food is in the direction opposite
   * where the organism is facing.
   *
   * @param agx  Agent's x-coordinate
   * @param agy  Agent's y-coordinate
   * @param agdir  Agent's direction
   * @return   (nearest food's distance ,
   *            nearests food's angle ,
   *            0=food at distance, 1=agent is on top of a food now)
   */
  def percept( agx:Int , agy:Int, agdir:Int ):(Double,Double,Int) = {
    val loca:(Int,Int) = (agx,agy)
    if( food.apply(loca)  ) {
      food -= loca
      val ret:(Double,Double,Int) = (-99.0,-99.0,1)
      ret
    } else {
      val nearinfo:(Int,Int,Double) = nearest(agx,agy)
      val angle_signal = angle( agx,agy, agdir, nearinfo._1 , nearinfo._2 )
      val ret:(Double,Double,Int) = (nearinfo._3*distance_factor , angle_signal, 0 )
      ret
    }
  }

  def addAllFood():Int = tr_dropFood(0)

  def addFoodAt( fx:Int, fy:Int ):Unit = {
    val loca:(Int,Int) = (fx,fy)
    food += loca
  }

  def clear():Unit = {   food.clear()   }

  def isEmpty:Boolean = {(food.size < 1)}

  def walls:(Int,Int) = (width,height)


  // * // * //
  @tailrec
  private def tr_dropFood(unique:Int):Int = if( unique>=foodPerDrop) {
    unique
  } else {
    val rng = MersenneTwisterSrz.getInstance()
    val dx = rng.nextInt() % width
    val dy = rng.nextInt() % height
    val coord:(Int,Int) = (dx,dy)
    food += coord
    val uni = food.size
    tr_dropFood(uni)
  }

  private def nearest( x:Int, y:Int ):(Int,Int,Double) = {
    val trip:List[(Int,Int,Double)] = for( f <- food.toList ) yield {
      val x1:Double = f._1.toDouble
      val y1:Double = f._2.toDouble
      val x2:Double = x
      val y2:Double = y
      val xd:Double = x1-x2
      val yd:Double = y1-y2
      (f._1 , f._2, scala.math.sqrt( xd*xd + yd*yd ))
    }
    require( trip.isEmpty == false )
    val neark = trip.reduceLeft( (p,q) => if(p._3  < q._3) p else q )

    neark
  }

  private def angle(agx:Int, agy:Int, agdir:Int,  fx:Int, fy:Int):Double = {
    val agent_dir_vect:(Double,Double) = agdir match {
      case 0 => (1.0  , 0.0 )
      case 1 => (0.0  , 1.0 )
      case 2 => (-1.0 , 0.0 )
      case 3 => (0.0  , -1.0 )
      case _ => (1.0  , 0.0 )
    }

    val foodspan:(Double,Double) = unitVector(fx-agx , fy-agy)
    val rawdot = dotProduct(agent_dir_vect,foodspan)
    val revdot  = rawdot * (-1.0)
    val halfdot = revdot * 0.5
    val signal_nd = halfdot + 0.5
    val handedness:Double = agdir match {
      case 0 => if(fy>agy) 1.0 else -1.0
      case 1 => if(fx<agx) 1.0 else -1.0
      case 2 => if(fy<agy) 1.0 else -1.0
      case 3 => if(fx>agx) 1.0 else -1.0
      case _ => 1.0
    }

    (signal_nd * handedness)
  }

  private def unitVector( vx:Int, vy:Int ):(Double,Double) = {
    val xf:Double = vx.toDouble
    val yf:Double = vy.toDouble
    val m:Double = scala.math.sqrt( xf*xf + yf*yf )
    require(m > 0.0)
    val ret:(Double,Double) = (xf/m , yf/m)
    ret
  }

  private def dotProduct( v:(Double,Double) , u:(Double,Double) ):Double = {
    (v._1*u._1  + v._2*u._2 )
  }
}

