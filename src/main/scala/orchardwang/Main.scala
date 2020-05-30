package orchardwang

import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.io.File
import java.io.PrintWriter
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.DataOutputStream
import java.io.FileInputStream
import java.io.InputStream
import java.io.DataInputStream
import scala.annotation.tailrec
import vegas._

import orchardwang.util._
import orchardwang.neural._

sealed class vegasPlotTests 
{
	@tailrec
	def tr_countOccurances (co:Char , v:List[Char] , accum:Int ):Int = v match {
		case Nil => accum
		case h::tail => {
			val p = if(co==h) 1  else 0
			val ppa = p+accum
			tr_countOccurances(co,tail,ppa)
		}
	}

	def countOccurances( co:Char , s:String ):Int = {tr_countOccurances( co , s.toList, 0 )}

	/*
		Map("country" -> "USA", "population" -> 314),
						Map("country" -> "UK", "population" -> 64),
						Map("country" -> "DK", "population" -> 80)
	* */
	def makeLittleVegasiMap( Xaxislabel:String ,
													Xaxiscat:String,
													Yaxislabel:String,
													ydata:Int ):Map[String,Any] = {
			val ret = Map( Xaxislabel->Xaxiscat , Yaxislabel->ydata )
			ret
	}

	def performCharacterOccurance( datafilename : Array[String]): Unit = 
	{
		
 		
		val infileName = 
			if( datafilename.length > 0 ) datafilename(0) 
			else "input.txt" 
			
		val outfileName = 
			if( datafilename.length > 1 ) datafilename(1) 
			else "output.html" 
		
		val builder = StringBuilder.newBuilder
		try {
			val txtsource = Source.fromFile(infileName)
			for (line <- txtsource.getLines) {
				builder.append(line.toString + ",")
			}
			txtsource.close()
		} catch {
			case e: FileNotFoundException => println(s"File not found $infileName ")
			case e: IOException => println(s"IOException while reading $infileName ")
		}
		
		if( builder.nonEmpty ) {

			// Count the occurances of lowercase letters in the file.
			val filecontents = builder.toString()
			val lowerchars = "abcdefghijklmnopqrstuvwxyz"
			val chfilter = lowerchars.toSet
			val filteredcontents = filecontents.filter(  chfilter )

			val tt = lowerchars.toList
			val lettercounts:List[(Char,Int)] = for(el<-tt)
				yield {
					val y:(Char,Int) = (el  , countOccurances(el,filteredcontents))
					y
				}

			lettercounts.foreach( dup => println(  s"${dup._1} -> ${dup._2}") )


			// Create a plot using vegas.
			val vegasdata:Seq[Map[String,Any]] = for( el<-lettercounts ) yield {
				makeLittleVegasiMap("letter" , el._1.toString , "occurance" , el._2  )
			}

			val occurance_plot = Vegas("Country Pop").
				withData( vegasdata).encodeX("letter", Nom).encodeY("occurance", Quant).mark(Bar);

			val htmlplot:String = occurance_plot.html.pageHTML("HTMLn")

			// Write the HTML plotter to the output file.
			val fWriter = new PrintWriter( new File(outfileName) )

			try {
				fWriter.write( htmlplot )
			} catch {
				case e: FileNotFoundException => println(s"File not found $outfileName ")
				case e: IOException => println(s"IOException while writing $outfileName ")
			} finally {
				fWriter.close()
			}
		}
	}

}

///////////////////////
//   Main object
object Main 
{
	def main( args:Array[String]): Unit = {
		testNetworkDynamics( args )
	}

  def testNetworkDynamics( args:Array[String] ):Unit =
  {
    if( args.length > 3 ) {
      val nodes: Int = (java.lang.Integer.parseInt(args(0))).toInt
      val rngs1: Long = java.lang.Long.parseLong(args(1)).toLong
      val rngs2: Long = java.lang.Long.parseLong(args(2)).toLong
      val cycles: Int = (java.lang.Integer.parseInt(args(3))).toInt

      val rng = MersenneTwisterSrz.getInstance()
      rng.seed(rngs1, rngs2)

      val nn = new NeuralNetwork(nodes)
      val allreentry: IndexedSeq[(Int, Int, Double)] =
        for {
          i <- (0 until nodes)
          j <- (0 until nodes)
        } yield ((i, j, 14.0 * (rng.nextDouble() - 0.5)))


      nn.weightless()
      nn.setSynapses(allreentry.toList)
      nn.noBias()

      val nninput: Array[Double] = Array.ofDim(nodes)
      for (n <- 0 until nodes) {
        val rfire = rng.nextDouble()
        nninput(n) = if( rfire < 0.6 ) 1.0 else 0.0
      }


      val nndp = new NNDynamicsParams(
        nn,
        nninput,
        cycles,
        "dynamics.png",
        580, 400);

      val dmachine = new NetworkDynamics(nndp)
      dmachine.performCycles()
      dmachine.writeToDisk()
    } else {
      println( "{total nodes}  {seed1}  {seed2}  {total cycles}" )
    }
  }

  
  def testFloatingPointFiles( args:Array[String] ): Unit = {
    if( args.length > 1 ) {
      if( args(0) == "-w" ) {
        val rng = MersenneTwisterSrz.getInstance()
        rng.seed( 378 , 864 )
        val sn = for( i <- (0 until 5)) yield{ 9.0*(rng.nextDouble()-0.5) }
        sn.foreach( k => println(f"$k%.13f")  )

        val outputstm:OutputStream = new FileOutputStream( args(1) )
        val dos:DataOutputStream = new DataOutputStream(outputstm)
        dos.writeLong(1010101L)
        dos.writeLong(3232538L)
        dos.writeLong(8765309L)
        for( k <- sn ) {
          dos.writeDouble(k)
        }
        dos.close()
        outputstm.close()
      }

      if( args(0) == "-r" ) {
        val binaryFileIdentifier:Array[Long] = Array(
        1010101L,3232538L,8675309L       );
        val wrongFileFormat:String = new String(  args(1) + " , wrong file format" )
        val inputstm:InputStream = new FileInputStream( args(1) )
        val dis:DataInputStream = new DataInputStream(inputstm)
        val ident:Array[Long] = Array.ofDim[Long](3)
        try{
          ident(0) = dis.readLong()
          ident(1) = dis.readLong()
          ident(2) = dis.readLong()
        } catch {
          case e:Exception => System.err.println( wrongFileFormat )
        }

        val paird = ident zip binaryFileIdentifier
        val comp = for( p<-paird if( p._1 == p._2)) yield {1}
        if( comp.length == 3 ) {
          val content =
          for( i <- (0 until 5) ) yield { dis.readDouble() }
          content.foreach( k => println(f"read $k%.13f")  )
        } else {
          System.err.println(wrongFileFormat)
        }
        dis.close()
        inputstm.close()
      }
    } else {
      println("No command line arguments.")
      println("  -r inputfile")
      println("  -w outputfilename")
    }
  }


  def testNeuralNetwork( args:Array[String] ): Unit = {
    if( args.size > 3 ) {
      val nodes:Int = (java.lang.Integer.parseInt(args(0) ) ).toInt
      val rngs1:Long = java.lang.Long.parseLong(args(1) ).toLong
      val rngs2:Long = java.lang.Long.parseLong(args(2)  ).toLong
      val cycles:Int = (java.lang.Integer.parseInt(args(3))  ).toInt

      val rng = MersenneTwisterSrz.getInstance()
      rng.seed( rngs1 , rngs2 )

      val nn = new NeuralNetwork( nodes )
      val allreentry:IndexedSeq[(Int,Int,Double)] =
        for{
          i <- (0 until nodes )
          j <- (0 until nodes)
        } yield ( (i,j, 14.0*(rng.nextDouble()-0.5) ) )

      val zeroconn:IndexedSeq[(Int,Int,Double)] = for(
        j <- (1 until nodes) ) yield {
        (0,j,  -2.2 )  }

      val oneconn:IndexedSeq[(Int,Int,Double)] = for(
        j <- (2 until nodes) ) yield {
        (1,j, 2.2 )  }

      nn.weightless()
      nn.setSynapses( allreentry.toList )
      nn.setSynapses( oneconn.toList ++ zeroconn.toList )

      nn.noBias()
      nn.quiescent( )
      nn.applyInput(0, 0.0 )
      nn.applyInput(1, 1.0 )  // Excite network

      println("Cycle 0")
      print( nn.toString() )

      for( cyc <- (1 to cycles)) {
        nn.cycle()
        println(" ")
        println("=========================")
        println("Cycle "+ cyc.toString )
        print( nn.toString() )
      }
    } else println("No command line args.")
  }

  def testMersenne( args:Array[String]): Unit = {
    if( args.size > 1 ) {
      val azero = args(0)
      val aone = args(1)
      println(s"args(0)= $azero")
      println(s"args(1)= $aone")
      val RNG = MersenneTwisterSrz.getInstance()

      if( args(0) == "-s" ) {
        println("Starting a new seeded generator.")
        RNG.seed(0xAEEDEF89L , 0x76D43210L )
        val bigrands = for (el <- (0 until 5000 )) yield {
          RNG.nextLong()
        }
        val lastr = bigrands.take(30)
        val lastrtxt = for (el <- lastr) yield {
          f"$el%08x"
        }
        for (el <- lastrtxt) println("  " + el)
        println("Saving generator state to file " + args(1))
        RNG.serialPause( args(1) )
      }


      if( args(0)== "-r" ) {
        println("Resuming from state file " + args(1))
        if( RNG.serialResume( args(1) ) ) {
          val bigrands = for (el <- (0 until 5000)) yield {
            RNG.nextLong()
          }
          val lastr = bigrands.takeRight(25)
          val lastrtxt = for (el <- lastr) yield {
            f"$el%08x"
          }
          for (el <- lastrtxt) println("  " + el)
        }
      }

      if( args(0)== "-ni" ) {
        val rng = MersenneTwisterSrz.getInstance()
        rng.seed( 0x123L , 0x234L )

        // Generate the first 750,000 numbers.
        for( i <-  (0 until 750000)) {
          val r = rng.nextLong()
        }

        // Print the next 30
        for( i <- (0 until 30) ) {
          val r = rng.nextLong()
          println( f"$r%08x" )
        }
      }



    } else println("No command line args.")
  }

}
