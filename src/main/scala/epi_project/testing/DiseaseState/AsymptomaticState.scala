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


case class AsymptomaticState() extends State  {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Asymptomatic)
  }

  def isRecovered(context: Context,agent: StatefulAgent):Boolean = {
    val RecoveryProb = Disease.lambdaA*Disease.dt
    val RecoveryState = biasedCoinToss(RecoveryProb)

    RecoveryState
  }

  addTransition(
    when = isRecovered,
    to = context => RecoveredState()
  )
}
