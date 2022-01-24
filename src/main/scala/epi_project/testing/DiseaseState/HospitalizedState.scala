package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing._
import epi_project.testing.InfectionStatus._


case class HospitalizedState(toBeDead:Boolean) extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Hospitalized)
  }

  var leaveHospitalisedState:Boolean = false

  override def perTickAction(context: Context, agent: StatefulAgent): Unit = {
    leaveHospitalisedState = shouldLeaveHospitalisedState(context,agent)
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

  def isRecovered(context: Context,agent: StatefulAgent):Boolean = (leaveHospitalisedState) && (!toBeDead)
  def isDead(context: Context,agent: StatefulAgent):Boolean = (leaveHospitalisedState) && (toBeDead)


  addTransition(
    when = isRecovered,
     to = RecoveredState()
  )
  addTransition(
    when = isDead,
    to = DeadState()
  )
}
