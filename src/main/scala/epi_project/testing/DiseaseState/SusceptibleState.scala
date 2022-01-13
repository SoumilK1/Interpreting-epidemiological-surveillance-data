package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Node, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing.InfectionStatus._
import epi_project.testing._

case class SusceptibleState(toBeAsymptomatic:Boolean) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Susceptible)
  }

  var leavingSusceptible:Boolean = false

  override def perTickAction(context: Context, agent: StatefulAgent): Unit = {
    leavingSusceptible = shouldBeInfected(context,agent)
  }


  def shouldBeInfected(context: Context,agent: StatefulAgent):Boolean = {
    val schedule = context.fetchScheduleFor(agent).get

    val currentStep = context.getCurrentStep
    val placeType: String = schedule.getForStep(currentStep)

//    if (placeType == "Hospital"){
//      println("Location is Hospital", agent.asInstanceOf[Person].infectionState,
//        agent.asInstanceOf[Person].essentialWorker)
//    }
    val places = agent.getConnections(agent.getRelation(placeType).get).toList
    if (places.nonEmpty) {
      val place = places.head
      val decodedPlace = agent.asInstanceOf[Person].decodeNode(placeType, place)

      val InfectedFraction: Double = fetchInfectedFraction(decodedPlace, placeType, context)
      val ExposureProb: Double = Disease.lambdaS * Disease.dt * InfectedFraction
      val InfectionState = biasedCoinToss(ExposureProb)

      return InfectionState
    }
    false
  }


//  def isAsymptomatic(context: Context,agent: StatefulAgent):Boolean = {
//    if (biasedCoinToss(Disease.gamma)){
//      return true
//    }
//    false
//  }

  def goToAsymptomatic(context: Context,agent: StatefulAgent):Boolean = leavingSusceptible && toBeAsymptomatic
  def goToPresymptomatic(context: Context,agent: StatefulAgent):Boolean = leavingSusceptible && !toBeAsymptomatic





  def fetchInfectedFraction(node: Node,placeType: String, context: Context): Double = {

    val cache = context.perTickCache
    val key = (placeType, node.internalId)

    cache.getOrUpdate(key,() => calculateInfectedFraction(node, placeType,context)).asInstanceOf[Double]
  }


  def calculateInfectedFraction(node: Node,placeType : String, context: Context): Double = {


    //val total = node.getConnectionCount(node.getRelation[Person]().get, "currentLocation" equ placeType)

    val sus_or_rec = node.getConnectionCount(node.getRelation[Person]().get,("currentLocation" equ placeType ) and  (("infectionState" equ Susceptible) or ("infectionState" equ Recovered)))

    val hos = node.getConnectionCount(node.getRelation[Person]().get, ("infectionState" equ Hospitalized) and ("currentLocation" equ placeType ))

    val infected = node.getConnectionCount(node.getRelation[Person]().get, (("infectionState" equ Asymptomatic) or ("infectionState" equ Presymptomatic) or ("infectionState" equ MildlyInfected) or ("infectionState" equ SeverelyInfected)) and ("currentLocation" equ placeType))
      //node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ Presymptomatic)+ node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ MildlyInfected) + node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ SeverelyInfected) + node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ Hospitalized)

    val infected_and_quarantined: Double = node.getConnectionCount(node.getRelation[Person]().get,("currentLocation" equ placeType) and ("beingTested" equ 2))

    val totalCount:Double = sus_or_rec + infected + Disease.contactProbability*hos
    val infectedCount:Double = (infected - infected_and_quarantined) + Disease.contactProbability*hos +
      Disease.contactProbability*infected_and_quarantined

    infectedCount/totalCount
  }




  addTransition(
    when = goToAsymptomatic,
      to = AsymptomaticState()
  )

  addTransition(
    when = goToPresymptomatic,
    to = PresymptomaticState(toBeMildlyInfected = biasedCoinToss(Disease.delta))
  )
}

