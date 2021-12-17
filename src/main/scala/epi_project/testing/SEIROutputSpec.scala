package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import epi_project.testing.InfectionStatus._

class SEIROutputSpec(context: Context) extends CSVSpecs {
  override def getHeaders: List[String] =
    List(
      "Step",
      "Susceptible",
      "Asymptomatic",
      "Presymptomatic",
      "MildlyInfected",
      "SeverelyInfected",
      "Recovered",
      "Hospitalized",
      "Infected",
      "EligibleForContactTracing",
      "EligibleForTargetedTest",
      "EligibleForRandomTest",
      "RTPCRTestsConducted",
      "RATTestsConducted"
    )

  override def getRows(): List[List[Any]] = {
    val graphProvider = context.graphProvider
    val label = "Person"

    val row = List(
        if (context.getCurrentStep % 2 == 0) {
          context.getCurrentStep},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ Susceptible)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ Asymptomatic)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ Presymptomatic)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ MildlyInfected)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ Recovered)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ Hospitalized)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "infectionState" equ Presymptomatic) + graphProvider.fetchCount(label, "infectionState" equ Asymptomatic) + graphProvider.fetchCount(label, "infectionState" equ MildlyInfected) + graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected) + graphProvider.fetchCount(label, "infectionState" equ Hospitalized)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "isAContact" equ true)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "isEligibleForTargetedTesting" equ true)},
        if (context.getCurrentStep % 2 == 0) {
          graphProvider.fetchCount(label, "isEligibleForRandomTesting" equ true)},
        if (context.getCurrentStep % 2 == 0) {
          Disease.numberOfRTPCRTestsDoneOnEachDay},
        if (context.getCurrentStep % 2 == 0) {
          Disease.numberOfRATTestsDoneOnEachDay},
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