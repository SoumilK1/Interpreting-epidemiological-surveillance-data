package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import epi_project.testing.InfectionStatus._

class SEIROutputSpec(context: Context) extends CSVSpecs {
  override def getHeaders: List[String] =
    List(
      "Time",
      "Susceptible",
      "Asymptomatic",
      "Presymptomatic",
      "MildlyInfected",
      "SeverelyInfected",
      "Recovered",
      "Hospitalized",
      "Infected",
      "EligibleForTargetedTest",
      "TestedByTargetedTest",
      "EligibleForContactTracing",
      "TestedByContactTracing",
      "EligibleForRandomTest",
      "TestedByRandomTest",
      "RTPCRTestsConducted",
      "RATTestsConducted",
      "TotalTestsConducted",
      "TestPositivityRate",
      "NumberOfPositiveTests"
    )

  override def getRows(): List[List[Any]] = {
    //    if (context.getCurrentStep % 2 == 0){
    val graphProvider = context.graphProvider
    val label = "Person"

    val row = List(
      context.getCurrentStep*Disease.dt,
      graphProvider.fetchCount(label, "infectionState" equ Susceptible),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic),
      graphProvider.fetchCount(label, "infectionState" equ MildlyInfected),
      graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected),
      graphProvider.fetchCount(label, "infectionState" equ Recovered),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalized),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic) + graphProvider.fetchCount(label, "infectionState" equ Asymptomatic) + graphProvider.fetchCount(label, "infectionState" equ MildlyInfected) + graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected) + graphProvider.fetchCount(label, "infectionState" equ Hospitalized),
      graphProvider.fetchCount(label, "isEligibleForTargetedTesting" equ true),
      graphProvider.fetchCount(label, "testCategory" equ 1),
      graphProvider.fetchCount(label, "isAContact" equ true),
      graphProvider.fetchCount(label, "testCategory" equ 2),
      graphProvider.fetchCount(label, "isEligibleForRandomTesting" equ true),
      graphProvider.fetchCount(label, "testCategory" equ 3),
      Disease.numberOfRTPCRTestsDoneAtEachTick,
      Disease.numberOfRATTestsDoneAtEachTick,
      Disease.totalNumberOfTestsDone,
      (Disease.numberOfPositiveTestsAtEachTick/(Disease.numberOfRTPCRTestsDoneAtEachTick + Disease.numberOfRATTestsDoneAtEachTick))*100,
      Disease.numberOfPositiveTestsAtEachTick
    )
    List(row)
  }
}
//}




