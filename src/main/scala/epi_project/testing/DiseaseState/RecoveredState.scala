package epi_project.testing.DiseaseState

import com.bharatsim.engine.Context
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.models.StatefulAgent
import epi_project.testing.Disease
import epi_project.testing.InfectionStatus._


case class RecoveredState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState",Recovered)
    Disease.newlyRecoveredEveryDay = Disease.newlyRecoveredEveryDay + 1.0

  }

}
