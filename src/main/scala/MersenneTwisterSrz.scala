package orchardwang
package util

import scala.collection.mutable.Queue
import scala.collection.mutable.ArraySeq
import scala.annotation.tailrec
import scala.util.{Try,Success,Failure}
import scala.io.BufferedSource
import scala.io.Source
import java.lang.Long.parseLong
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.{FileNotFoundException, IOException}


final class MersenneTwisterSrz( instanceNumber:Int )
{
  implicit def str2faux( hex:String):fauxUINT32 = {
    val duck:fauxUINT32 = new fauxUINT32()
    duck.setter(hex)
    duck
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

 

  //--------------------------------------------------

  def getSeed( ):( Long, Long ) = {
    ( seedG(0).peek() , seedG(1).peek() )
  }

  def seed( us:Int ): Unit = {
    val s1:Long = if( us <0 ) {
      ((-1) * us).toLong
    } else {
      us.toLong
    }
    val s2: Long = 0x02BD10fB9L
    seed( s1 , s2 )
  }


  def seed( s1:Long, s2:Long ):Unit = {
    index = 0
    seedG = ArraySeq( new fauxUINT32(s1) , new fauxUINT32(s2) );
    initCompleteSet( seedG ,2)
    userCalledSeed=true
  }

  def seed( s1:String, s2:String ):Unit = {
    val sA: fauxUINT32 = new fauxUINT32()
    val sB: fauxUINT32 = new fauxUINT32()
    sA.setter(s1)
    sB.setter(s2)
    seed(sA.peek(), sB.peek())
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

      val fxseedA:fauxUINT32 = new fauxUINT32()
      val fxseedB:fauxUINT32 = new fauxUINT32()
      fxseedA.setter(seedA)
      fxseedB.setter(seedB)
      seedG = ArraySeq( fxseedA , fxseedB )

      // Get only the mt() strings.
      val mtform    :List[String]     = state.drop(8)

      // Create a list of wooden ducks.
      val fauxducks  :List[(fauxUINT32,String)] = for( v <-mtform ) yield {
        (new fauxUINT32(),v)
      }

      // Convert the pairs to the native format.
      val fauxforms :List[fauxUINT32] = for( d <- fauxducks ) yield {
        (d._1).setter( d._2 )
        d._1
      }
      // Apply them to the generator's state.
      mt = buildArrSeqfaux(  fauxforms )

      // Allow invocations without tripping require()
      userCalledSeed = true
    } else {
      System.err.println(
        "MersenneTwisterSrz.resumeFromState() provided state had invalid format. Seeding from (0,0)");
      seed(0L,0L)
    }

    status
  }


  /*
  *   Save a snapshot of the generator to a file.
  * */
  def serialPause( output_file_name:String ):Boolean = {
    require(userCalledSeed)
    var status:Boolean = false
    val state = serializeState()
    val CSVcontent = CSVlines( Queue.empty , state )
    val ofile = new File(output_file_name)
    val openHandler:Try[FileWriter] = Try {
      new FileWriter(ofile)
    }

    openHandler match {
      case Success(fw) => {
        val bw = new BufferedWriter(fw)
        try {
          for( line <-CSVcontent ) {
            bw.write(line)
            bw.newLine()
          }
        } catch {
          case e:IOException =>
            val verbose = " , MersenneTwisterSrz.serialPause() interrupted during write."
            System.err.println(s"$output_file_name" + verbose )
        } finally {
          bw.close()
          status = true
        }
      }
      case Failure(e) =>
        val verbose = " , MersenneTwisterSrz.serialPause() could not open for writing."
        System.err.println(s"$output_file_name"+verbose)
    }

    status
  }

  /*
  *   Restore the generator from a snapshot of its state
  *   that was previously saved to disk by .serialPause()
  *    */
  def serialResume( input_file_name:String ):Boolean = {
    var status:Boolean = false
    val openHandler:Try[BufferedSource] = Try {
      Source.fromFile(input_file_name)
    }

    openHandler match {
      case Success(bs) => {
        val builder = new StringBuilder
        try {
          for (line <- (bs.getLines())) {
            builder.append(line.trim)
          }
        } catch {
          case e: IOException =>
            val verbose = " , MersenneTwisterSrz.serialResume() interrupted during read."
            System.err.println(s"$input_file_name" + verbose)
        } finally {
          bs.close()
          val CSVcontents = builder.toString()

          // Literally remove the final comma.
          // Seriously.
          val chop = CSVcontents.length - 1
          if( chop > 4800 ) {
            val uCSV = CSVcontents.substring(0, chop)

            val arraystate = uCSV.split(",")
            val state = arraystate.toList
            status = resumeFromState(state)
          } else {
            val verbose = " , MersenneTwisterSrz.serialResume() wrong file format."
            System.err.println(s"$input_file_name"+verbose)
          }
        }
      }

      case Failure(e) =>
        val verbose = " , MersenneTwisterSrz.serialResume() file not found."
        System.err.println(s"$input_file_name"+verbose)
    }

    status
  }


  // * // * //

  private def initSeed( uA:fauxUINT32 ) = {
    val lfu = tr_initSeed( List.empty , uA , pN, 0 )
    val lfurev = lfu.reverse
    mt = buildArrSeqfaux( lfurev )
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
                                seedLength:Int ):Unit =  {
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

    } );

    index = 0

     
  }

  @tailrec
  private def CSVlines( csvl:Queue[String] , consume:List[String] ):Queue[String] = consume match {
    case Nil => csvl
    case _ => {
      val htl = consume.splitAt(8)
      val builder = new StringBuilder
      for( el <- htl._1 ) {
        builder.append(el)
        builder.append(",")
      }
      csvl += (builder.toString())
      CSVlines( csvl , htl._2 ) // recursion
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




sealed protected class fauxUINT32( ini:Long )
{
  private var x:Long = ini&mask;
  // * //
  
  
  
  def this() = this(0L)
  def this(n:Int) = this( n.toLong )

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



  def parseGraceful(hex:String): Long = {
    try {
      (java.lang.Long.parseLong(hex,16)).toLong
    } catch {
      case e:Exception => {
        System.err.println("Bad format of hexadecimal integer {"+hex+"}")
        (0L)
      }
    }
  }


  def setter(Li:Long):Long = {
    x=Li&mask;
    Li
  }
  def setter(i:Int):Int ={
    x=( i.toLong ) & mask;
    i
  }

  def setter(xhx:String):String = {
    val overlx:Long = parseGraceful(xhx)
    setter(overlx)
    xhx
  }
  
  
  // Make this a method to avoid doubling the size of Arrays
  private def mask:Long = 0x0FFFFFFFFL
	
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

 
