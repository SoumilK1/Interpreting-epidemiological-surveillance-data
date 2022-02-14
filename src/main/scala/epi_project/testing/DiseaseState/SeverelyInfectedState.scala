package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing.{Disease, Person}
import epi_project.testing.InfectionStatus._

case class SeverelyInfectedState(toBeHospitalized:Boolean) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",SeverelyInfected)
  }

  var leaveSeverelyInfected:Boolean = false

  override def perTickAction(context: Context, agent: StatefulAgent): Unit = {
    leaveSeverelyInfected = shouldLeaveSI(context,agent)
  }


  def shouldLeaveSI(context: Context,agent: StatefulAgent):Boolean = {
    val exitSI = Disease.lambdaSI*Disease.dt
    val InfectionState = biasedCoinToss(exitSI)

    InfectionState
  }

  def goToHospitalized(context: Context,agent: StatefulAgent):Boolean = leaveSeverelyInfected && toBeHospitalized
  def goToRecovered(context: Context,agent: StatefulAgent):Boolean = leaveSeverelyInfected && !toBeHospitalized


  addTransition(
    when = goToRecovered,
     to = RecoveredState()
  )

  addTransition(
      when = goToHospitalized,
      to = agent => HospitalizedState(toBeDead =
        biasedCoinToss(Disease.mu*agent.asInstanceOf[Person].ageStratifiedMuMultiplier))
  )
}
