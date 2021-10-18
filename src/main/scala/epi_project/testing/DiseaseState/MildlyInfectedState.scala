package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing._
import epi_project.testing.InfectionStatus._


case class MildlyInfectedState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",MildlyInfected)
  }

  def isRecovered(context: Context,agent: StatefulAgent): Boolean = {
    val RecoveryProb = Disease.lambdaMI*Disease.dt
    val InfectionState = biasedCoinToss(RecoveryProb)

    InfectionState
  }

  addTransition(
    when = isRecovered,
    to = RecoveredState()
  )
}
