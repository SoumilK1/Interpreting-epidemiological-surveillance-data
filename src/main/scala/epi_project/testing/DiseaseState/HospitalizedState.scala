package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing._
import epi_project.testing.InfectionStatus._

import scala.math.{tan, tanh}


case class HospitalizedState(toBeDead:Double) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Hospitalized)
  }

  var leaveHospitalisedState:Boolean = false

  override def perTickAction(context: Context, agent: StatefulAgent): Unit = {
    leaveHospitalisedState = shouldLeaveHospitalisedState(context,agent)
    if (context.getCurrentStep % Disease.numberOfTicksInADay == 0){
      var numberOfDays = agent.asInstanceOf[Person].numberOfDaysSpentInHospital
      agent.updateParam("numberOfDaysSpentInHospital", numberOfDays + 1)
    }
  }


  def shouldLeaveHospitalisedState(context: Context,agent: StatefulAgent):Boolean = {
    val exitH = Disease.lambdaH*Disease.dt
    val InfectionState = biasedCoinToss(exitH)

    InfectionState
  }

//  def isRecovered(context: Context,agent: StatefulAgent):Boolean = {
//    val RecoveryProb = Disease.lambdaH*Disease.dt
//    val InfectionState = biasedCoinToss(RecoveryProb)
//
//    InfectionState
//  }

  def isRecovered(context: Context,agent: StatefulAgent):Boolean = (leaveHospitalisedState) &&
    (!(biasedCoinToss(toBeDead * agent.asInstanceOf[Person].ageStratifiedMuMultiplier)))
  def isDead(context: Context,agent: StatefulAgent):Boolean = (leaveHospitalisedState) &&
    (biasedCoinToss(toBeDead * (1 + math.tanh(0.35*agent.asInstanceOf[Person].numberOfDaysSpentInHospital))
      *agent.asInstanceOf[Person].ageStratifiedMuMultiplier))


  addTransition(
    when = isRecovered,
     to = RecoveredState()
  )
  addTransition(
    when = isDead,
    to = DeadState()
  )
}
