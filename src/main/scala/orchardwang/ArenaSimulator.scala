package orchardwang

import scala.collection.immutable.IndexedSeq
import orchardwang.genetic._
import orchardwang.neural._
import orchardwang.util._

/**
 * Instantiate FitnessMachine to ArenaSimulator.
 * Target to Foragers operating in Arenas.
 */
class ArenaSimulator extends FitnessMachine[Forager,Arena]
{
  private final val lifetime = 3000
  private final val expiretime = 91
  private final val angleOffsetTests = 32

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
   * A "fitness" test is the average over 32 lifetimes, where each lifetime
   * is 3000 time steps.
   *
   * I have no idea why Orchard and Wang chose 32 lifetimes,
   * These Arena environments are not nearly rich enough for this to matter.
   * We could average over 3 or 4 lifetimes without loss of precision.
   *
   * @param agent an Agent to test for performance.
   * @param env an Environment within which to perform.
   * @return the resulting fitness from agent performing in env
   */
  def fitnessTest( agent:Forager , env:Arena ):Double = {
    val rindex = agent.getRandIndex
    val rng = MersenneTwisterSrz.getInstance( rindex )
    val all_offsets = for( o <- (0 until angleOffsetTests) ) yield {
      val r = (rng.nextDouble()) * 1.6
      ( r - 0.8 )
    }
    val individualF = for( lt <- (0 until angleOffsetTests) ) yield {
      forageWithOffset( agent , env , all_offsets(lt) )
    }
    val aot:Double = angleOffsetTests.toDouble
    ( individualF.sum / aot )
  }

  // * // * //

  /*
  The Forager will try to obtain food in an Arena with a broken angle sensor.
    The angle sensor is giving the Forager a consistently false reading of the angle
    to the nearest food.  The Forager must adapt on-the-fly to this disparity.
    Hypothetically, this adaptation is due to synaptic plasticity of its neural network.
  * */
  /*
        Each organism has a “brain”, a neural network with  ~four~(sic) , five
      inputs and two outputs. The two output nodes specify the
      action that the organism will take: (0,0) means do nothing;
      (0,1) means turn 90 deg right; (1,0) means turn 90 deg left; and (1,1)
      means move forward.
   */
  private def forageWithOffset( agent:Forager , env:Arena , angleOffset:Double ):Double = {
    val inputPattern: Array[Double] = Array.ofDim[Double](5)
    var eaten = 0
    var prevAction: (Int, Int) = (0, 0)
    var expCountdown = expiretime
    var arenaWalls:(Int,Int) = env.walls

    env.setRandIndex(agent.getRandIndex)
    env.clear()
    agent.setOrientation((49, 49, 1))


    for (artime <- (0 until lifetime)) {
      if (env.isEmpty) {
        env.addAllFood()
        expCountdown = expiretime
      } else {
        if (expCountdown < 1) {
          env.clear()
          env.addAllFood()
          expCountdown = expiretime
        }
      }
      val orient:(Int, Int, Int) = agent.getOrientation


      /* Perceive environment from current position.
    * @return  ._1  nearest food's distance ,
   *           ._2   nearests food's angle ,
   *           ._3   0=food at distance, 1=agent is on top of a food now * */
      val rawPercept: (Double, Double, Int) = env.percept(orient._1, orient._2, orient._3)

      inputPattern(0) = (prevAction._1).toDouble
      inputPattern(1) = (prevAction._2).toDouble
      if (orient._3 == 1) {
        eaten = eaten + 1
        inputPattern(2) = -1.0 // angle to nearest food
        inputPattern(3) = 0.0 // distance to nearest food
        inputPattern(4) = 1.0 // eating signal
      } else {
        inputPattern(2) = badSensor(rawPercept._2, angleOffset) // angle to nearest food
        inputPattern(3) = rawPercept._1 // distance to nearest food
        inputPattern(4) = 0.0 // eating signal
      }

      /* Have the Forager make a decision based on the current inputPattern */
      val action: (Int, Int) = agent.decisionActionCycle(inputPattern)

      /* Update the Forager 'agent' in this Arena. */
      /* The two output nodes specify the
          action that the organism will take:
          (0,0) means do nothing;
          (0,1) means turn 90 deg right;
          (1,0) means turn 90 deg left;
           and (1,1) means move forward.*/
      (action._1 , action._2)  match {
        case (0,0) => agent.doNothing()
        case (0,1) => agent.turnRight()
        case (1,0) => agent.turnLeft()
        case (1,1) => agent.move( arenaWalls._1 , arenaWalls._2 )
        case (_,_) => agent.doNothing()
      }
    }

    eaten.toDouble
  }


  private def badSensor( rawAngle:Double , offset:Double ):Double = {
    val badAngle = rawAngle + offset
    val ret = if( badAngle > 1.0  ) {
      val overtwist = badAngle - 1.0
      ((-1.0)+overtwist)
    } else {
      if( badAngle < (-1.0) ) {
        val undertwist = badAngle + 1.0
        (1.0+undertwist)
      } else {
        badAngle
      }
    }
    ret
  }


}

//



