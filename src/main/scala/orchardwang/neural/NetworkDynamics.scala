package orchardwang
package neural


import scala.util.{Try,Success,Failure}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

sealed class NNDynamicsParams( nn:NeuralNetwork ,
                        ipattern:Array[Double],
                        ucycles:Int,
                        filename:String,
                        height:Int,
                        width:Int) {
  private val network:NeuralNetwork = nn;
  private val inputPattern:Array[Double] = ipattern
  private val cycles:Int = ucycles
  private val outfilename:String = filename
  private val imgHeight:Int = height
  private val imgWidth:Int = width
  def get:(NeuralNetwork,Array[Double],Int,String,Int,Int) = {
    val ret = (
      network,      // ._1
      inputPattern, // ._2
      cycles,       // ._3
      outfilename,  // ._4
      imgHeight,    // ._5
      imgWidth);    // ._6
    ret
  }
}

class NetworkDynamics( params:NNDynamicsParams )
{
  private val dyn = params.get
  private val rows = dyn._3 + 1
  private val fireThick:Int = ( dyn._6 / dyn._1.size ).toInt
  private val fireTall:Int = ( dyn._5 / rows ).toInt
  private val ufireThick:Int = if(fireThick<1) 1 else fireThick
  private val ufireTall:Int = if(fireTall<1) 1 else fireTall
  private val actWidth:Int =  ufireThick * (dyn._1.size)
  private val actHeight:Int = ufireTall * rows
  private val history:Array[Array[Double]] = Array.ofDim[Double](rows,dyn._1.size)
  private val rawImage:BufferedImage = new BufferedImage(actWidth+2, actHeight+2, BufferedImage.TYPE_INT_RGB)

  def performCycles():Unit = {
    val iter = (0 until dyn._1.size).toArray
    val itercyc = (0 until dyn._3)



    dyn._1.quiescent()
    for( i <- iter ) {
      dyn._1.applyInput(i, dyn._2(i))
      history(0)(i) = dyn._2(i)
    }
    for( c <- itercyc ) {
      dyn._1.cycle()
      for( i <- iter ) {
        history(c+1)(i) = dyn._1.nodeOutput(i)
      }
    }

    for( r <- (0 until rows) ) {
      for ( col <- iter ) {
        val tl = (col*ufireThick, r*ufireTall)
        val br = (tl._1 + ufireThick , tl._2 + ufireTall)

        val tlx = if( tl._1 >= actWidth ) actWidth-1 else tl._1
        val tly = if( tl._2 >= actHeight ) actHeight-1 else tl._2
        val brx = if( br._1 >= actWidth ) actWidth-1 else br._1
        val bry = if( br._2 >= actHeight ) actHeight-1 else br._2


        val coora = ( tlx , tly , brx, bry )
        val fire = history(r)(col)
        val cola = if( fire < 0.5 ) {
          ( 1.9*fire,  0.95*fire, 0.455*fire)
        } else {
          ( 0.2*fire , fire, 0.2*fire )
        }
        box( coora , cola )
      }
    }
  }

  def writeToDisk():Unit = {

    val openHandler:Try[java.io.File] = Try {
      new File(dyn._4)
    }

    openHandler match {
      case Success(fw) => {
        try {
          javax.imageio.ImageIO.write(rawImage, "png",  fw )
        } catch {
          case e:Exception =>
            System.err.println( dyn._4 + " , NetworkDynamics.writeToDisk() interrupted.");
        } finally {
           println("  Wrote to " + dyn._4 )
        }
      }
      case Failure(e) =>
        val verbose = " , NetworkDynamics.writeToDisk() could not open for writing."
        System.err.println( dyn._4 + verbose)
    }


  }

  private def box( coords:(Int,Int,Int,Int) ,
                   color:(Double,Double,Double) ):Unit ={
    val R:Long = (color._1 * 255.0).toLong
    val G:Long = (color._2 * 255.0).toLong
    val B:Long = (color._3 * 255.0).toLong
    val RGB:Long = (R<<16)|(G<<8)|B
    val iRBG:Int = (RGB & 0x00FFFFFF).toInt
    for{
      i <- (coords._2 until coords._4)
      j <- (coords._1 until coords._3)
    } rawImage.setRGB(j,i,iRBG)
  }
}
