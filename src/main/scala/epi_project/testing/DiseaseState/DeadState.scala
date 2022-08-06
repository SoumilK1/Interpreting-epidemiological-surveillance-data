package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import epi_project.testing.InfectionStatus._
import epi_project.testing.{Disease, Person}


case class DeadState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Dead)
    agent.updateParam("isEligibleForTargetedTesting",false)

    agent.updateParam("isEligibleForRandomTesting",false)
    agent.updateParam("isAContact",0)

    Disease.numberOfDeadOnEachDay = Disease.numberOfDeadOnEachDay + 1.0

    if (agent.asInstanceOf[Person].lastTestDay < 0){
      Disease.numberOfUntestedDeadOnEachDay = Disease.numberOfUntestedDeadOnEachDay + 1.0
      agent.updateParam("dayAtWhichPersonIsIdentified",context.getCurrentStep/Disease.numberOfTicksInADay)
    }

  }

}