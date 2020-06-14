package orchardwang

import scala.collection.immutable.IndexedSeq
import orchardwang.neural._
import orchardwang.genetic._


/**
 * Instantiate FitnessMachine to ArenaSimualator.
 * Target to Foragers operating in Arenas.
 */
class ArenaSimulator extends FitnessMachine[Forager,Arena]
{
  /**
   * Test every Agent in 'gen' within its associated Environment 'envs'.
   * Produce a list of fitnesses of these runs.
   *
   * @param gen a linear sequence of Agents to test.
   * @param envs  one or more Environments to test fitness.
   * @return a linear sequence of fitness values for the associated ith agent.
   */
  def runAllTests( gen:IndexedSeq[Forager] , envs:IndexedSeq[Arena]):IndexedSeq[Double] = {
    val lgen = gen.toList

    val lenvs:List[Arena] = if( gen.length == envs.length ) {
      envs.toList
    } else {
      for( e <- envs.toList ) yield {envs(0)}
    }

    val agentWithenviro:List[(Forager,Arena)] = lgen zip lenvs

    /* For the time being, perform this task laboriously as a single thread. */
    val ret = for( p <- agentWithenviro ) yield {
      fitnessTest( p._1 , p._2 )
    }
    ret.toIndexedSeq
  }
  

  /**
   * Place an agent into an environment to perform a single fitness test.
   *
   * @param agent an Agent to test for performance.
   * @param env an Environment within which to perform.
   * @return the resulting fitness from agent performing in env
   */
  def fitnessTest( agent:Forager , env:Arena ):Double = {
    0.0
  }
}

//



