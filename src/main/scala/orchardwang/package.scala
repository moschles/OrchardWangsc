package object orchardwang
{  
  private val debugFlag = true

  def chatter(s:String):Unit = System.err.println(s);
  def debug(code: => Any ):Unit = if( debugFlag ) {code}  
  
  private val beginRuntime:Double = System.nanoTime() / 1.00e9d
  def timeNow:String = {
    val nanoNow:Double = System.nanoTime() / 1.00e9d
    val nanoRun:Double = nanoNow - beginRuntime
    f"$nanoRun%1.3f"
  }
}
