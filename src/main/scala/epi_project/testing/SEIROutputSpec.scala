
package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.DoubleValue
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import epi_project.testing
import epi_project.testing.InfectionStatus._

import scala.collection.mutable

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
      "Dead",
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
      "NumberOfPositiveTests",
      "DeathRate",
      "CumulativeInfectedFraction",
      "NumberOfUninfectedPeopleGettingTested",
      "True R(t)",
      "Identified R(t)",
      "NewlyInfected",
      "True Case Doubling Rate",
      "Identified CDR"
    )

  override def getRows(): List[List[Any]] = {
    //    if (context.getCurrentStep % 2 == 0){
    val graphProvider = context.graphProvider
    val label = "Person"

//    val countMap = mutable.HashMap.empty[String, Int]
//    val nodes = graphProvider.fetchNodes(label)
//    nodes.foreach(node => {
//      val infectedState = node.getParams.apply("infectionState").toString
//      val existingCount = countMap.getOrElse(infectedState, 0)
//      countMap.put(infectedState, existingCount + 1)
//    })


    //    println(Disease.numberOfDeadOnEachDay)

    var TPR:Double = (Disease.numberOfPositiveTestsAtEachTick/(Disease.numberOfRTPCRTestsDoneAtEachTick + Disease.numberOfRATTestsDoneAtEachTick))*100
    if ((Disease.numberOfRTPCRTestsDoneAtEachTick + Disease.numberOfRATTestsDoneAtEachTick)==0){
      TPR = 0.0
    }
//    var CFR:Double =((graphProvider.fetchCount(label,"infectionState" equ Dead)) /(Disease.numberOfPositiveTestsAtEachTick
//      + graphProvider.fetchCount(label,(("infectionState" equ Dead) and ("lastTestDay" lt  0)))))*100
//    if((Disease.numberOfPositiveTestsAtEachTick +
//      graphProvider.fetchCount(label,(("infectionState" equ Dead) and ("lastTestDay" lt  0)))==0  )){
//      CFR = 0.0
//    }

    var Death_rate: Double = (Disease.numberOfDeadOnEachDay / (Disease.numberOfPositiveTestsAtEachTick +
      Disease.numberOfUntestedDeadOnEachDay)) * 100
    if (Disease.numberOfPositiveTestsAtEachTick +
      Disease.numberOfUntestedDeadOnEachDay == 0.0) {
      Death_rate = 0.0
    }


    var activeCases:Double = graphProvider.fetchCount(label, "infectionState" equ Presymptomatic) + graphProvider.fetchCount(label, "infectionState" equ Asymptomatic)  + graphProvider.fetchCount(label, "infectionState" equ MildlyInfected)+ graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected)+ graphProvider.fetchCount(label, "infectionState" equ Hospitalized)

    if (context.getCurrentStep% Disease.numberOfTicksInADay != Disease.numberOfTicksInADay - 1){
      activeCases = 0.0
    }
//
//    var infectedOnDayN_1:Double =  activeCases -
//      Disease.newlyInfectedEveryDay + Disease.newlyRecoveredEveryDay
//
//    var R_t : Double = (Disease.newlyInfectedEveryDay)/(infectedOnDayN_1)
//    if (context.getCurrentStep % Disease.numberOfTicksInADay != 0){
//      R_t = 0.0
//    }
//
//    if (context.getCurrentStep%Disease.numberOfTicksInADay == 0) {
//      println("active", activeCases)
//      println("newIf", Disease.newlyInfectedEveryDay)
//      println("newRe", Disease.newlyRecoveredEveryDay)
//      println(infectedOnDayN_1)
//    }
//    if (context.getCurrentStep <= Disease.numberOfTicksInADay){
//      Disease.cumulativeInfectedTillDayN_1 = 0.0
//    }
//
//    var newlyInfectedOnDayN_1:Double = Disease.totalNumberOfInfected - Disease.newlyInfectedEveryDay - Disease.cumulativeInfectedTillDayN_1
//    if (context.getCurrentStep % Disease.numberOfTicksInADay != 0){
//      newlyInfectedOnDayN_1 = 0.0
//    }
//    println("step",context.getCurrentStep)
//    println("total",Disease.totalNumberOfInfected)
//    println("newIf",Disease.newlyInfectedEveryDay)
//    println("newIfN_1",newlyInfectedOnDayN_1)
//
//    var R_t:Double = Disease.newlyInfectedEveryDay/newlyInfectedOnDayN_1
//
//    if (context.getCurrentStep%Disease.numberOfTicksInADay != 0 || context.getCurrentStep <= Disease.numberOfTicksInADay){
//      R_t = 0.0
//    }
//
//    Disease.cumulativeInfectedTillDayN_1 = Disease.cumulativeInfectedTillDayN_1 + newlyInfectedOnDayN_1

    var newlyInfectedOnDayN_1:Double = graphProvider.fetchCount(label,"dayAtWhichPersonIsInfected" equ (context.getCurrentStep/Disease.numberOfTicksInADay))
    var newlyInfectedOnDayN_2:Double = graphProvider.fetchCount(label,"dayAtWhichPersonIsInfected" equ (context.getCurrentStep/Disease.numberOfTicksInADay) - 1)

    var newlyInfectedOnDayN:Double = graphProvider.fetchCount(label,"dayAtWhichPersonIsInfected" equ (context.getCurrentStep/Disease.numberOfTicksInADay) + 1)

    if (context.getCurrentStep >= Disease.numberOfTicksInADay){
    if (context.getCurrentStep%Disease.numberOfTicksInADay==0){
      newlyInfectedOnDayN = newlyInfectedOnDayN_1
      newlyInfectedOnDayN_1 = newlyInfectedOnDayN_2
    }}

    var R_t:Double = newlyInfectedOnDayN/newlyInfectedOnDayN_1
    if(context.getCurrentStep%Disease.numberOfTicksInADay != 0 || newlyInfectedOnDayN_1==0.0){
      R_t = 0.0
    }

    var newlyIdentifiedOnDayN_1:Double = graphProvider.fetchCount(label,"dayAtWhichPersonIsIdentified" equ (context.getCurrentStep/Disease.numberOfTicksInADay))
    var newlyIdentifiedOnDayN_2:Double = graphProvider.fetchCount(label,"dayAtWhichPersonIsIdentified" equ (context.getCurrentStep/Disease.numberOfTicksInADay) - 1)
    var newlyIdentifiedOnDayN:Double = graphProvider.fetchCount(label,"dayAtWhichPersonIsIdentified" equ (context.getCurrentStep/Disease.numberOfTicksInADay) + 1)

    if (context.getCurrentStep >= Disease.numberOfTicksInADay){
      if (context.getCurrentStep%Disease.numberOfTicksInADay == 0){
        newlyIdentifiedOnDayN = newlyIdentifiedOnDayN_1
        newlyIdentifiedOnDayN_1 = newlyIdentifiedOnDayN_2
      }
    }

    var Iden_R_t:Double = newlyIdentifiedOnDayN/newlyIdentifiedOnDayN_1
    if (context.getCurrentStep%Disease.numberOfTicksInADay !=0 || newlyIdentifiedOnDayN_1 == 0.0){
      Iden_R_t = 0.0
    }


    var infectedAtSomeTick:Double = Disease.initialSeed
    var initialTick = Disease.caseDoublingTick

    var numberOfInfected:Double = graphProvider.fetchCount(label, "infectionState" equ Presymptomatic) + graphProvider.fetchCount(label, "infectionState" equ Asymptomatic) + graphProvider.fetchCount(label, "infectionState" equ MildlyInfected) + graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected) + graphProvider.fetchCount(label, "infectionState" equ Hospitalized)

    if (numberOfInfected > 2*infectedAtSomeTick){
      Disease.caseDoublingTick = context.getCurrentStep
    }
    var CDR:Double = (Disease.caseDoublingTick - initialTick).toDouble/Disease.numberOfTicksInADay.toDouble
    if (numberOfInfected < 2*infectedAtSomeTick){
      CDR = 0.0
    }
    if (numberOfInfected > 2*infectedAtSomeTick){
      Disease.initialSeed = numberOfInfected.toInt
      initialTick = Disease.caseDoublingTick
    }



    var identifiedAtSomeTick:Double = Disease.identifiedSeed
    var initialIdentifiedTick = Disease.IdentifiedDoublingTick
    var numberOfIdentified:Double = graphProvider.fetchCount(label,"beingTested" equ 2)

    if (numberOfIdentified > 2*identifiedAtSomeTick && context.getCurrentStep >= Disease.testingStartedAt + Disease.testDelay*Disease.numberOfTicksInADay){
      Disease.IdentifiedDoublingTick = context.getCurrentStep
    }

    var Iden_CDR:Double = (Disease.IdentifiedDoublingTick - initialIdentifiedTick).toDouble/Disease.numberOfTicksInADay.toDouble
    if ((numberOfIdentified <= 2*identifiedAtSomeTick) || ((numberOfIdentified == identifiedAtSomeTick) )){
      Iden_CDR = 0.0
    }

    if (numberOfIdentified > 2*identifiedAtSomeTick && context.getCurrentStep >= Disease.testingStartedAt + Disease.testDelay*Disease.numberOfTicksInADay){
      Disease.identifiedSeed = numberOfIdentified.toInt
      initialIdentifiedTick = Disease.IdentifiedDoublingTick
      if ( ((numberOfIdentified == infectedAtSomeTick) && numberOfIdentified != 0 && infectedAtSomeTick!=0)){
        initialIdentifiedTick = context.getCurrentStep
      }

    }



    val row = List(
      context.getCurrentStep*Disease.dt,
//      countMap.getOrElse(Susceptible.toString, 0),
      graphProvider.fetchCount(label, "infectionState" equ Susceptible),
//      countMap.getOrElse(Asymptomatic.toString,0),
      graphProvider.fetchCount(label, "infectionState" equ Asymptomatic),
//      countMap.getOrElse(Presymptomatic.toString,0),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic),
//      countMap.getOrElse(MildlyInfected.toString,0),
      graphProvider.fetchCount(label, "infectionState" equ MildlyInfected),
//      countMap.getOrElse(SeverelyInfected.toString,0),
      graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected),
//      countMap.getOrElse(Recovered.toString,0),
      graphProvider.fetchCount(label, "infectionState" equ Recovered),
//      countMap.getOrElse(Hospitalized.toString,0),
      graphProvider.fetchCount(label, "infectionState" equ Hospitalized),
//      countMap.getOrElse(Dead.toString,0),
      graphProvider.fetchCount(label,"infectionState" equ Dead),
//      countMap.getOrElse(Asymptomatic.toString,0) + countMap.getOrElse(Presymptomatic.toString,0) + countMap.getOrElse(MildlyInfected.toString,0) + countMap.getOrElse(SeverelyInfected.toString,0) + countMap.getOrElse(Hospitalized.toString,0),
      graphProvider.fetchCount(label, "infectionState" equ Presymptomatic) + graphProvider.fetchCount(label, "infectionState" equ Asymptomatic) + graphProvider.fetchCount(label, "infectionState" equ MildlyInfected) + graphProvider.fetchCount(label, "infectionState" equ SeverelyInfected) + graphProvider.fetchCount(label, "infectionState" equ Hospitalized),

      graphProvider.fetchCount(label, "isEligibleForTargetedTesting" equ true),
      graphProvider.fetchCount(label, "testCategory" equ 1),
      graphProvider.fetchCount(label, "isAContact" equ 1) +
        (graphProvider.fetchCount(label, "isAContact" equ 2)),
      graphProvider.fetchCount(label, "testCategory" equ 2),
      graphProvider.fetchCount(label, "isEligibleForRandomTesting" equ true),
      graphProvider.fetchCount(label, "testCategory" equ 3),
      Disease.numberOfRTPCRTestsDoneAtEachTick,
      Disease.numberOfRATTestsDoneAtEachTick,
      Disease.totalNumberOfTestsDone,
      TPR,
      Disease.numberOfPositiveTestsAtEachTick,
      Death_rate,
      Disease.totalNumberOfInfected/100000,
      Disease.numberOfUninfectedPeopleTested,
      R_t,
      Iden_R_t,
      Disease.newlyInfectedEveryDay,
      CDR,
      Iden_CDR
    )
    List(row)
  }
}
//}



