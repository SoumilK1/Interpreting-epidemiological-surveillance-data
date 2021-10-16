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

  def shouldBeInfected(context: Context,agent: StatefulAgent):Boolean = {
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

      return InfectionState
    }
    false
  }



  def isAsymptomatic(context: Context,agent: StatefulAgent):Boolean = {
    if (shouldBeInfected(context,agent)){
      if (biasedCoinToss(Disease.gamma)){
        return true
      }
    }
    false
  }

  def isPresymptomatic(context: Context,agent: StatefulAgent):Boolean = {
    if (shouldBeInfected(context,agent)){
      if (biasedCoinToss( 1 - Disease.gamma)){
        return true
      }
    }
    false
  }

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
    val infectedCount:Double = infected + Disease.contactProbability*hos - (1- Disease.contactProbability)*infected_and_quarantined

    infectedCount/totalCount

  }


//  def calculateInfectedFraction_Hospital(node:Node,placeType:String,context: Context):Double = {
//
//    if (placeType == "HOSPITAL"){
//      val totalHCW = node.getConnectionCount(node.getRelation[Person]().get,("currentLocation" equ placeType) and ("essentialWorker" equ 1))
//      val patients = node.getConnectionCount(node.getRelation[Person]().get,"infectionState" equ Hospitalized)
//
//      val total = totalHCW + 0.1*patients
//
//      return patients.toDouble/total.toDouble
//    }
//  }

  addTransition(
    when = isAsymptomatic,
      to = context => AsymptomaticState()
  )

  addTransition(
    when = isPresymptomatic,
    to = PresymptomaticState()
  )
}

