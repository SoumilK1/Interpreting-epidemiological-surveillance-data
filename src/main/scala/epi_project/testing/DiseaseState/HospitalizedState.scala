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



case class HospitalizedState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Hospitalized)
  }

  def isRecovered(context: Context,agent: StatefulAgent):Boolean = {
    val RecoveryProb = Disease.lambdaH*Disease.dt
    val InfectionState = biasedCoinToss(RecoveryProb)

    InfectionState
  }

  addTransition(
    when = isRecovered,
     to = RecoveredState()
  )
}
