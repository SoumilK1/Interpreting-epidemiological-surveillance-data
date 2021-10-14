package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.models.{Agent, Node}
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
      "Number of people eligible for testing",
      "Number of tests conducted"
    )

  override def getRows(): List[List[Any]] = {
    val graphProvider = context.graphProvider
    val label = "Person"

    //val locations = context.graphProvider.fetchNodes(placeType)
    val row = List(
      context.getCurrentStep,
      graphProvider.fetchCount(label, "infectionState" equ Susceptible),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic) + graphProvider.fetchCount(label, "infectionState" equ Asymptomatic) + graphProvider.fetchCount(label, "infectionState" equ MildlyInfected) + graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected) + graphProvider.fetchCount(label, "infectionState" equ Hospitalized),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic),
      graphProvider.fetchCount(label, "infectionState" equ MildlyInfected),
      graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalized),
      graphProvider.fetchCount(label, "infectionState" equ Recovered),
      graphProvider.fetchCount(label, "isScheduledForTesting" equ true),
      Disease.numberOfTestsDoneAtEachTick
    )
    List(row)
  }


//  def decodeNode(classType: String, node: GraphNode): Node = {
//    classType match {
//      case "House" => node.as[House]
//      case "Office" => node.as[Office]
//      case "School" => node.as[School]
//      case "Hospital" => node.as[Hospital]
//    }
//  }
}