package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import epi_project.testing.InfectionStatus._


case class DeadState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Dead)
  }

}