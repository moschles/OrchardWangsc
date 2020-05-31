package orchardwang
import org.scalatest.FunSuite
import org.scalatest._

class ArenaTest extends FlatSpec with Matchers
{
  "Arena" should "return size with .walls()" in {
    val arena = new Arena(100, 111, 10)
    val sz = arena.walls
    sz._1 shouldEqual (100)
    sz._2 shouldEqual (111)
  }
}