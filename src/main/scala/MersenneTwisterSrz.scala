package orchardwang
import java.lang.Long.parseLong

import scala.collection.mutable.ArraySeq
import scala.annotation.tailrec

sealed class fauxUINT32( ini:Long )
{
  private val mask:Long = 0x0FFFFFFFFL
  private var x:Long = ini&mask;

  def this() = this(0L)
  def this(n:Int) = this( n.toLong )
  def this(hex:String) = this(
    {
      ((java.lang.Long.parseLong(hex,16)).toLong) & (0x0FFFFFFFFL)
    }
  );

  def apply(Li:Long):Long = {
    x=Li&mask;
    Li
  }
  def apply(i:Int):Int ={
    x=( i.toLong ) & mask;
    i
  }

  override def toString: String = {
    val capX:String = f"$x%08x"
    capX
  }

  def peek():Long ={x}
  def copy ( ):fauxUINT32 = {
    new fauxUINT32(x)
  }
  def odd():Boolean = {
    val parbit = x & 1
    if( parbit > 0 ) true else false
  }

  def subtract(that:Int):fauxUINT32 = {
    val b:Long = that.toLong
    val r:Long = if(x>=b) {
      (x-b)
    } else {
      val bl:Long = b - x
      val bstep:Long = bl-1
      val xstep:Long = 0x0FFFFFFFFL
      (xstep-bstep)
    }
    new fauxUINT32(r)
  }

  /*
    +
    *
    xor
    shftR
    shftL
    or
    and
    mod
  */
  def + (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x + that.x)&mask
    new fauxUINT32(r)
  }
  def + (that: Long): fauxUINT32 = {
    val r:Long = (x + that)&mask
    new fauxUINT32(r)
  }
  def + (that: Int): fauxUINT32 = {
    val r:Long = (x + that.toLong)&mask
    new fauxUINT32(r)
  }
  def * (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x * that.x)&mask
    new fauxUINT32(r)
  }
  def * (that: Long): fauxUINT32 = {
    val r:Long = (x * that)&mask
    new fauxUINT32(r)
  }
  def * (that: Int): fauxUINT32 = {
    val r:Long = (x * that.toLong)&mask
    new fauxUINT32(r)
  }
  def xor (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x ^ that.x)&mask
    new fauxUINT32(r)
  }
  def xor (that: Long): fauxUINT32 = {
    val r:Long = (x ^ that)&mask
    new fauxUINT32(r)
  }
  def xor (that: Int): fauxUINT32 = {
    val r:Long = (x ^ that.toLong)&mask
    new fauxUINT32(r)
  }
  def >> (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x >> that.x)
    new fauxUINT32(r)
  }
  def >> (that: Long): fauxUINT32 = {
    val r:Long = (x >> that)
    new fauxUINT32(r)
  }
  def >> (that: Int): fauxUINT32 = {
    val r:Long = (x >> that.toLong)
    new fauxUINT32(r)
  }
  def << (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x << that.x)&mask
    new fauxUINT32(r)
  }
  def << (that: Long): fauxUINT32 = {
    val r:Long = (x << that)&mask
    new fauxUINT32(r)
  }
  def << (that: Int): fauxUINT32 = {
    val r:Long = (x << that.toLong)&mask
    new fauxUINT32(r)
  }
  def or (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x | that.x)&mask
    new fauxUINT32(r)
  }
  def or (that: Long): fauxUINT32 = {
    val r:Long = (x | that)&mask
    new fauxUINT32(r)
  }
  def or (that: Int): fauxUINT32 = {
    val r:Long = (x | that.toLong)&mask
    new fauxUINT32(r)
  }
  def and (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x & that.x)
    new fauxUINT32(r)
  }
  def and (that: Long): fauxUINT32 = {
    val r:Long = (x & that)
    new fauxUINT32(r)
  }
  def and (that: Int): fauxUINT32 = {
    val r:Long = (x & that.toLong)
    new fauxUINT32(r)
  }
  def mod (that: fauxUINT32): fauxUINT32 = {
    val r:Long = (x % that.x)
    new fauxUINT32(r)
  }
  def mod (that: Long): fauxUINT32 = {
    val r:Long = (x % that)
    new fauxUINT32(r)
  }
  def mod (that: Int): fauxUINT32 = {
    val r:Long = (x % that.toLong)
    new fauxUINT32(r)
  }

}


object Chattm
{
  private var debugflag = true
  def dbg( ds:String ): Boolean =
  {
    if( debugflag ) { System.err.println(ds) }
    debugflag
  }
  def on(): Boolean = {
    debugflag = true
    debugflag
  }
  def off(): Boolean = {
    debugflag = false
    debugflag
  }
  def isOn(): Boolean = {
    debugflag
  }
}


final class MersenneTwisterSrz( instanceNumber:Int )
{
  implicit def str2faux( hex:String):fauxUINT32 = {
    new fauxUINT32(hex)
  }
  implicit def int2faux( i:Int ):fauxUINT32 = {
    new fauxUINT32(i)
  }
  implicit def long2faux( Li:Long):fauxUINT32 = {
    new fauxUINT32(Li)
  }

  private val singletonInstanceNumber = instanceNumber
  private var userCalledSeed:Boolean = false
  private val fileFormatIdentifier = List("mkf)VoqS","nIBCjnuj","mersenne",")twister","statemti")


  private var index:Int  =0;
  private val pN:Int       = 624
  private val M:Int        = 397
  private val U:Int  = 11
  private val S:Int  = 7
  private val T:Int  = 15
  private val L:Int  = 18
  private val F:fauxUINT32  = 1812433253L
  private val C:fauxUINT32  = "EFC60000"
  private val A:fauxUINT32  = "9908B0DF"
  private val B:fauxUINT32  = "9D2C5680"
  private val MASK_LOWER:fauxUINT32 = "7FFFFFFF"
  private val MASK_UPPER:fauxUINT32 = "80000000"
  private val fdenom:Float  = (0x0FFFFFFFFL ).toFloat
  private val ddenom:Double = (0x0FFFFFFFFL ).toDouble

  private val sA:fauxUINT32 = "A"
  private val sB:fauxUINT32 = "B"
  private var seedG:ArraySeq[fauxUINT32] = ArraySeq( sA , sB );
  private var mt:ArraySeq[fauxUINT32] = {
    val tmparr = (1 to pN.toInt).toArray
    val mt_ini:ArraySeq[fauxUINT32] = for(i <- tmparr) yield{new fauxUINT32()}
    mt_ini
  }

  Chattm.off()

  //--------------------------------------------------

  def getSeed( ):( Long, Long ) = {
    ( seedG(0).peek() , seedG(1).peek() )
  }

  def seed( s1:Long, s2:Long ):Unit = {
    index = 0
    seedG = ArraySeq( new fauxUINT32(s1) , new fauxUINT32(s2) );
    initCompleteSet( seedG ,2)
    userCalledSeed=true

    if(Chattm.isOn()){
      println("mt()  after .seed()");

      val mttxt = for(el<-mt) yield {el.toString}
      val pairs = ((0 until (mt.length)  )) zip mttxt.toList
      val fewbeg = pairs.take(30)
      val fewrest = pairs.filter(
          p => {
            (p._1 > 29) && ( (p._1 % 7) )==0
          } );
      val fewcat = fewbeg++fewrest

      fewcat.foreach( p => println(s"${p._1}  ${p._2}") )
      println("--------------------")
    }
  }

  def nextLong():Long = {
    require(userCalledSeed)
    val i = if( index >= pN ) {
        twist()
        0
      } else {
        index
      };
    val x = mt(i)
    val z =  x xor  (x >> U);
    val t =  z xor  ((z << S) and B  );
    val w =  t xor  ((t << T) and C  );
    val y =  w xor  (w >> L);
    index = 1+index

    y.peek()
  }

  def nextInt():Int = {
    require(userCalledSeed)
    val y = nextLong()
    val ry:Int =(y & (0x3FFFFFFF)).toInt
    ry
  }

  def nextFloat():Float = {
    require(userCalledSeed)
    val num:Float = nextLong().toFloat
    (num / fdenom)
  }

  def nextDouble():Double = {
    require(userCalledSeed)
    val num:Double = nextLong().toDouble
    (num / ddenom)
  }

  def getInstanceNumber():Int = {singletonInstanceNumber}


  /*
  *     Obtain a snapshot of the generator's internal state and working information.
  *     This state is returned as a List of Strings.
  * */
  def serializeState():List[String] = {
    import scala.collection.mutable.Queue
    var Q:Queue[String] = Queue.empty

    require(userCalledSeed)
    for( v<-fileFormatIdentifier ){
      Q += v
    }

    val fxindex:fauxUINT32 = index
    Q += fxindex.toString
    Q += seedG(0).toString
    Q += seedG(1).toString
    for( v <- mt ) {
      Q += v.toString
    }
    Q.toList
  }

  /*
  *   Resume the generator from a snapshot previously
  *   obtained from .serializeState()
  * */
  def resumeFromState( state:List[String] ):Boolean = {

    // Check the first 5 against the file Format Identifier strings.
    // status == false  indicates a failed match.
    val identity = state.take(5)
    val guard = (identity zip fileFormatIdentifier).filterNot( e => (e._1==e._2) )
    val status:Boolean = (
          state.length == (pN+8)  &&
          guard.isEmpty
      );

    if( status ) {
      val rnginfo = (state.take(8)).takeRight(3)
      val rnginfo2 = rnginfo.tail
      val rnginfo3 = rnginfo2.tail
      val sindex = rnginfo.head
      val seedA = rnginfo2.head
      val seedB = rnginfo3.head
      val fxindex:fauxUINT32 = sindex
      index = (fxindex.peek()).toInt
      seedG = ArraySeq( new fauxUINT32(seedA) , new fauxUINT32(seedB ) )

      // Get only the mt() strings.
      val mtform    :List[String]     = state.drop(8)

      // Convert to native class format.
      val fauxforms  :List[fauxUINT32] = for( v <-mtform ) yield { new fauxUINT32(v) }

      // Apply them to the generator's state.
      mt = buildArrSeqfaux(  fauxforms )
    } else {
      System.err.println("resumeFromState() given a state with invalid format. Seeding from (0,0)")
      seed(0L,0L)
    }

    status
  }

  /*
  *   Save a snapshot of the generator to a file.
  * */
  def serialPause( output_file_name:String ):Boolean = {
    require(userCalledSeed)
    false
  }

  /*
  *   Restore the generator from a snapshot of its state
  *   that was previously saved to disk by .serialPause()
  *    */
  def serialResume( input_file_name:String ):Boolean = {
    false
  }


  // * // * //

  private def initSeed( uA:fauxUINT32 ) = {
    val lfu = tr_initSeed( List.empty , uA , pN, 0 )
    mt = buildArrSeqfaux( lfu )
    index = pN;
  }

  @tailrec
  private def tr_initSeed( Lmt:List[fauxUINT32], uK:fauxUINT32 , maxd:Int, depth:Int  ):List[fauxUINT32] ={
    if( depth >= maxd ) {
      Lmt
    } else {
      Lmt match {
        case Nil => tr_initSeed( List(uK) , uK, maxd , depth+1 );
        case h::tail => {
          val i:fauxUINT32 = depth
          val prep = ( ( h xor (h>>30)) * F ) + i
          tr_initSeed( prep :: Lmt , uK, maxd, depth+1 )
        }
      }
    }
  }

  private def initCompleteSet(
                                bigSeed:ArraySeq[fauxUINT32] ,
                                seedLength:Int ) =  {
    val birthdate:fauxUINT32 = "19650218"
    val Ymagic:fauxUINT32 = 1664525L
    val Zmagic:fauxUINT32 = 1566083941L
    val Tmagic:fauxUINT32 = "80000000"
    var i:Int = 1
    var j:Int = 0
    val ktop:Int = if( pN >seedLength) {pN} else {seedLength}

    initSeed(birthdate)

    val mixfront = ktop until 0 by (-1)
    for( k <- mixfront ) {
      val mtic = mt(i).copy()
      val mtimo = mt(i-1).copy()
      val yardi:fauxUINT32 =  (((mtimo>>30) xor mtimo) * Ymagic) xor mtic
      val fxj:fauxUINT32 = j
      mt(i) = yardi + bigSeed(j) + fxj
      val tmpi = if( (i+1) >= pN ) {
            mt(0) = mt(pN-1)
            1
          }else {
            (i+1)
          }
      val tmpj = if( (j+1) >=  seedLength ) {
        0
      } else {
        (j+1)
      }

      i=tmpi
      j=tmpj
    }

    val mixback = (pN-1) until 0 by (-1)
    for( k<-mixback ) {
      val mtic = mt(i).copy()
      val mtimo = mt(i-1).copy()
      val yardi:fauxUINT32 = (((mtimo>>30) xor mtimo) * Zmagic) xor mtic
      mt(i) = yardi.subtract(i)
      val tmpi = if( (i+1) >= pN ) {
        mt(0) = mt(pN-1)
        1
      } else {
        (i+1)
      }

      i=tmpi
    }

    mt(0) = Tmagic
    twist()
  }


  private def twist():Unit = {
    (0 until pN).foreach( i => {
      val q = (i + 1) % pN
      val x: fauxUINT32 = (mt(i) and MASK_UPPER) + (mt(q) and MASK_LOWER)
      val xA: fauxUINT32 = if (x.odd()) {
        (x >> 1) xor A
      } else {
        (x >> 1)
      }

      val z = (i + M) % pN
      val yardi:fauxUINT32 = mt(z) xor xA
      mt(i) = yardi or 0

      if (Chattm.isOn()) {
        if (i < 3) {
          println(s"   q= $q")
          println(s"   z= $z")
          println("x =" + x.toString)
          println("xA =" + xA.toString)
          println("mt(i) =" + mt(i).toString)
        }
      }

    } );

    index = 0

    if(Chattm.isOn()){
      val mttxt = for(el<-mt) yield {el.toString}
      val pairs = ((0 until (mt.length)  )) zip mttxt.toList
      val fewbeg = pairs.take(30)
      println("mt()  after .twist()");
      fewbeg.foreach( p => println(s"${p._1}  ${p._2}") )
      println("--------------------")
    }
  }

  /*
    *   Old versions of sbt are stupid, and cannot convert a List
    *   to an ArraySeq no matter what intermediate steps are taken.
    *   This method must be hard-coded by hand.
    *   Intellij IDEA and scastie can both do this natively.
    * */
  private def buildArrSeqfaux( lss:List[fauxUINT32] ):ArraySeq[fauxUINT32] = {
    var retunq:Array[fauxUINT32] = lss.toArray
    val ASstub:ArraySeq[fauxUINT32] = ArraySeq()
    val retseq = ASstub.++(retunq)
    retseq
  }
}


// // Companion object // //

object MersenneTwisterSrz
{  /*
   Implement multiple singletons.
   Calling code is expected to perform

      myRNG = MersenneTwisterSrz.getInstance()

   every time to get a ref to a singleton.

   If there are multiple instances ,

     particularRNG = MersenneTwisterSrz.getInstance(n)

   To obtain the nth instance they created using createInstance().
   The 0th instance is automatically created by the constructor.   */
  private var batchOfInstances:ArraySeq[MersenneTwisterSrz] =
    ArraySeq( (new MersenneTwisterSrz(0)) );

  def total( ):Int = this.synchronized{ batchOfInstances.length }

  def getInstance(n:Int = 0):MersenneTwisterSrz = this.synchronized{
    require( n <= (total) )
    batchOfInstances(n)
  }

  /*
  ! ONLY call this if you need more than one random number generator throughout
    the entire runtime of a program.
    Scenarios where you would require this would be multithreaded programs
    where various threads require their own generator.
    If you only need one generator, this method should never be called. */
  def createInstance( ):MersenneTwisterSrz = this.synchronized{
    val MTS = new MersenneTwisterSrz( total() );
    val boxedMTS:ArraySeq[MersenneTwisterSrz] = ArraySeq(MTS)
    val tmp = batchOfInstances ++ boxedMTS
    batchOfInstances = tmp
    MTS
  }


}
