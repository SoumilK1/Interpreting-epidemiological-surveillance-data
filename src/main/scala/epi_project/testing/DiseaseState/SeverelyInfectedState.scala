package com.bharatsim.examples.epidemiology.testing_latest.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Network, Node, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import com.bharatsim.examples.epidemiology.testing_latest.InfectionStatus._
import com.bharatsim.examples.epidemiology.testing_latest.{Disease, Person}

case class SeverelyInfectedState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",SeverelyInfected)
  }

  def isRecovered(context: Context,agent: StatefulAgent):Boolean = {
    val exitSI = Disease.lambdaSI*Disease.dt
    val InfectionState = biasedCoinToss(exitSI)
    val enterR = biasedCoinToss(1 - Disease.sigma)

    if (InfectionState && enterR){
      return true
    }
    false
  }

  def isHospitalized(context: Context,agent: StatefulAgent):Boolean = {
    val exitSI = Disease.lambdaSI*Disease.dt
    val InfectionState = biasedCoinToss(exitSI)
    val enterH = biasedCoinToss(Disease.sigma)

    if (InfectionState && enterH){
      return true
    }
    false
  }

  addTransition(
    when = isRecovered,
     to = RecoveredState()
  )

  addTransition(
    when = isHospitalized,
      to = HospitalizedState()
  )
}
