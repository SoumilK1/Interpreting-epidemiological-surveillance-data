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

case class PresymptomaticState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Presymptomatic)
  }

  def isMildlyInfected(context: Context,agent: StatefulAgent):Boolean = {
    val exitPSM = Disease.lambdaP*Disease.dt
    val InfectionState = biasedCoinToss(exitPSM)
    val enterMI = biasedCoinToss(Disease.delta)

    if (InfectionState && enterMI){
      return true
    }
    false
  }

  def isSeverelyInfected(context: Context,agent: StatefulAgent):Boolean = {
    val exitPSM = Disease.lambdaP*Disease.dt
    val InfectionState = biasedCoinToss(exitPSM)
    val enterSI = biasedCoinToss(1 - Disease.delta)

    if (InfectionState && enterSI){
      return true
    }
    false
  }

  addTransition(
    when = isMildlyInfected,
      to = MildlyInfectedState()
  )

  addTransition(
    when = isSeverelyInfected,
    to = SeverelyInfectedState()
  )
}
