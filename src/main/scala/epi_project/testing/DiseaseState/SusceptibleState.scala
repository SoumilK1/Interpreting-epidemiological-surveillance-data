package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Network, Node, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing.InfectionStatus._
import epi_project.testing.{Disease, Person}
import org.apache.commons.math3.ml.neuralnet.Network

case class SusceptibleState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Susceptible)
  }

  def isAsymptomatic(context: Context,agent: StatefulAgent):Boolean = {
    val schedule = context.fetchScheduleFor(agent).get

    val currentStep = context.getCurrentStep
    val placeType: String = schedule.getForStep(currentStep)

    val places = agent.getConnections(agent.getRelation(placeType).get).toList
    if (places.nonEmpty) {
      val place = places.head
      val decodedPlace = agent.asInstanceOf[Person].decodeNode(placeType, place)

      val InfectedFraction: Double = fetchInfectedFraction(decodedPlace, placeType, context)
      val ExposureProb: Double = Disease.lambdaS * Disease.dt * InfectedFraction
      val InfectionState = biasedCoinToss(ExposureProb)

      if (InfectionState){
        if (biasedCoinToss(Disease.gamma)) {
          return true
        }
      }
      false
    }
    false
  }

  def isPresymptomatic(context: Context,agent: StatefulAgent):Boolean = {
    val schedule = context.fetchScheduleFor(agent).get

    val currentStep = context.getCurrentStep
    val placeType: String = schedule.getForStep(currentStep)

    val places = agent.getConnections(agent.getRelation(placeType).get).toList
    if (places.nonEmpty) {
      val place = places.head
      val decodedPlace = agent.asInstanceOf[Person].decodeNode(placeType, place)

      val InfectedFraction: Double = fetchInfectedFraction(decodedPlace, placeType, context)
      val ExposureProb: Double = Disease.lambdaS * Disease.dt * InfectedFraction
      val InfectionState = biasedCoinToss(ExposureProb)

      if (InfectionState){
        if (biasedCoinToss(1- Disease.gamma)) {
          return true
        }
      }
      false
    }
    false
  }




  def fetchInfectedFraction(node: Node,placeType: String, context: Context): Double = {

    val cache = context.perTickCache
    val key = (placeType,node.internalId)

    cache.getOrUpdate(key, ()=> calculateNew(node)).asInstanceOf[Double]
  }

  def calculateNew(node: Node): Double = {

    val total = node.getConnectionCount(node.getRelation[Person]().get)
    val infected = node.getConnectionCount(node.getRelation[Person]().get,"infectionState" equ Asymptomatic) + node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ Presymptomatic)+ node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ MildlyInfected) + node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ SeverelyInfected) + node.getConnectionCount(node.getRelation[Person]().get, "infectionState" equ Hospitalized)

    infected.toDouble/total.toDouble
  }


  addTransition(
    when = isAsymptomatic,
      to = context => AsymptomaticState()
  )

  addTransition(
    when = isPresymptomatic,
    to = PresymptomaticState()
  )
}

