package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import epi_project.testing.InfectionStatus._

class SEIROutputSpec(context: Context) extends CSVSpecs {
  override def getHeaders: List[String] =
    List(
      "Step",
      "Susceptible",
      "Infected",
      "Presymptomatic",
      "Asymptomatic",
      "MildlyInfected",
      "SeverelyInfected",
      "Hospitalized",
      "Recovered",
    )

  override def getRows(): List[List[Any]] = {
    val graphProvider = context.graphProvider
    val label = "Person"
    val row = List(
      context.getCurrentStep,
      graphProvider.fetchCount(label, "infectionState" equ Susceptible),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic) + graphProvider.fetchCount(label,"infectionState" equ Asymptomatic) + graphProvider.fetchCount(label,"infectionState" equ MildlyInfected) + graphProvider.fetchCount(label,"infectionState" equ SeverelyInfected) + graphProvider.fetchCount(label,"infectionState" equ Hospitalized),
      graphProvider.fetchCount(label,"infectionState" equ Presymptomatic),
      graphProvider.fetchCount(label,"infectionState" equ Asymptomatic),
      graphProvider.fetchCount(label,"infectionState" equ MildlyInfected),
      graphProvider.fetchCount(label,"infectionState" equ SeverelyInfected),
      graphProvider.fetchCount(label,"infectionState" equ Hospitalized),
      graphProvider.fetchCount(label, "infectionState" equ Recovered),
    )
    List(row)
  }
}
