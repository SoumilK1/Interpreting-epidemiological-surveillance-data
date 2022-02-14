package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing._
import epi_project.testing.InfectionStatus._

case class PresymptomaticState(toBeMildlyInfected:Boolean) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Presymptomatic)
  }

  var leavingPresymptomatic:Boolean = false

  override def perTickAction(context: Context, agent: StatefulAgent): Unit ={
    leavingPresymptomatic = shouldExitPresymptomatic(context,agent)
  }


  def shouldExitPresymptomatic(context: Context,agent: StatefulAgent):Boolean = {
    val exitPSM = Disease.lambdaP*Disease.dt
    val InfectionState = biasedCoinToss(exitPSM)

    InfectionState
  }

  def goToMildlyInfected(context: Context,agent: StatefulAgent):Boolean = leavingPresymptomatic && toBeMildlyInfected
  def goToSeverelyInfected(context: Context,agent: StatefulAgent):Boolean = leavingPresymptomatic && !toBeMildlyInfected

  addTransition(
    when = goToMildlyInfected,
      to = MildlyInfectedState()
  )

  addTransition(
    when = goToSeverelyInfected,
    to = agent => SeverelyInfectedState(toBeHospitalized =
      biasedCoinToss(Disease.sigma*agent.asInstanceOf[Person].ageStratifiedSigmaMultiplier))
  )
}
