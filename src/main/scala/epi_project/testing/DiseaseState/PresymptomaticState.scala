package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing._
import epi_project.testing.InfectionStatus._

case class PresymptomaticState(toBeSeverelyInfected:Double) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Presymptomatic)
    Disease.totalNumberOfInfected = Disease.totalNumberOfInfected + 1.0
    Disease.newlyInfectedEveryDay = Disease.newlyInfectedEveryDay + 1.0
    agent.updateParam("dayAtWhichPersonIsInfected", (context.getCurrentStep/Disease.numberOfTicksInADay) + 1)
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

  def goToMildlyInfected(context: Context,agent: StatefulAgent):Boolean = leavingPresymptomatic &&
    !(biasedCoinToss(toBeSeverelyInfected * agent.asInstanceOf[Person].ageStratifiedDeltaMultiplier))
  def goToSeverelyInfected(context: Context,agent: StatefulAgent):Boolean = leavingPresymptomatic &&
    (biasedCoinToss(toBeSeverelyInfected * agent.asInstanceOf[Person].ageStratifiedDeltaMultiplier))

  addTransition(
    when = goToMildlyInfected,
      to = MildlyInfectedState()
  )

  addTransition(
    when = goToSeverelyInfected,
    to = agent => SeverelyInfectedState(toBeHospitalized = Disease.sigma)
  )
}
