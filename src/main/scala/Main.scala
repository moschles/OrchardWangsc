
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.Queue
import scala.annotation.tailrec

object Chatter 
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
}

object isLetter
{
	private val lcase:Set[Char] = "qwertyuiopasdfghjklzxcvbnm".toCharArray.toSet
	private val ucase:Set[Char] = "QWERTYUIOPASDFGHJKLZXCVBNM".toCharArray.toSet
	private val setAlpha = lcase union ucase
	def check( c:Char ):Boolean = setAlpha.contains(c) 
}

class LetterCaesar( ch:Char )
{
	private val lowA = 'A'.toInt
	private val highZ = 'Z'.toInt
	val useCh = if( ch.toInt < lowA || ch.toInt > highZ ) 'X' else ch 
	def +( that:LetterCaesar ):LetterCaesar = {
		val code = ( (useCh.toInt-lowA) + 
			(that.useCh.toInt-lowA) ) % 26
		new LetterCaesar( (code+lowA).toChar ) 
	}
	def dig():Char = useCh 
}

object Main 
{
	def fileToChars( fName:String ):List[Char] = {
		val builder = StringBuilder.newBuilder
		try {
			for (line <- Source.fromFile(fName).getLines) {
				builder.append(line.toString)
			}			
		} catch {
			case e: FileNotFoundException => println(s"File not found $fName ")
			case e: IOException => println(s"IOException while reading $fName ")
		}
		val bastr = builder.toString()
		bastr.toCharArray.toList 
	}
	
	def symbolsToString( clist:List[Char] ):String = {
		val builder = StringBuilder.newBuilder
		for( el <- clist ){ builder.append(el.toString) }
		builder.toString()
	}
	
	
	def countOccurances( fc:Char , clist:List[Char]):(Char,Int) = {
		val onlyfc = clist.filter(_==fc)
		(fc, onlyfc.length)
	}

	def main(args: Array[String]): Unit = 
	{
		Chatter.on()
		Chatter.dbg("Entered main.")
		
		val infileName = 
			if( args.length > 0 ) args(0) 
			else "input.txt" 
			
		val outfileName = "output.txt"
		
		val builder = StringBuilder.newBuilder
		try {
			for (line <- Source.fromFile(infileName).getLines) {
				builder.append(line.toString + ",")
			}			
		} catch {
			case e: FileNotFoundException => println(s"File not found $infileName ")
			case e: IOException => println(s"IOException while reading $infileName ")
		}
		
		if( builder.nonEmpty ) {
			val fWriter = new PrintWriter( new File(outfileName) )
			 
			try {				
				fWriter.write( builder.toString ) 
			} catch {
				case e: IOException => e.printStackTrace
			} finally {
				fWriter.close() 
			}
		}
		
	}
}
