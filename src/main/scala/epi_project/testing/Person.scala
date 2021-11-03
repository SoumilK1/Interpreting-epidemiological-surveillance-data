package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.models.{Node, StatefulAgent}
import epi_project.testing.InfectionStatus._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._

case class Person(id: Long,
                  houseId:Long,
                  age: Int,
                  infectionState: InfectionStatus,
                  infectionDur: Int,
                  essentialWorker:Int,
                  beingTested:Int = 0,
                  isEligibleForTargetedTesting:Boolean = false,
                  isEligibleForRandomTesting:Boolean = false,
                  lastTestResult:Boolean = false,
                  lastTestDay:Int = -20000,
                  currentLocation:String = "House",
                  quarantineStartedAt:Int = 0) extends StatefulAgent {

  private val incrementInfectionDay: Context => Unit = (context: Context) => {
    if (isPresymptomatic && context.getCurrentStep % Disease.numberOfTicksInADay == 0)
      updateParam("infectionDur", infectionDur + 1)
  }

  private val checkCurrentLocation: Context => Unit = (context: Context) => {
    val schedule = context.fetchScheduleFor(this).get
    val locationNextTick: String = schedule.getForStep(context.getCurrentStep + 1)
    if (currentLocation != locationNextTick) {
      this.updateParam("currentLocation", locationNextTick)
      //println(currentLocation)
    }
  }

//  private val checkEligibilityForTargetedRTPCRTesting:Context => Unit = (context:Context) => {
//    //println(isEligibleForTestingAgain(context))
//    if ((context.activeInterventionNames.contains("get_tested"))&&
//      (context.getCurrentStep%Disease.numberOfTicksInADay==0)&&
//      (Disease.numberOfRTPCRTestsDoneOnEachDay < Disease.numberOfRTPCRTestsAvailable) &&
//      (isSymptomatic) &&
//      (!isBeingTested)) {
//      updateParam("isScheduledForRTPCRTesting", true)
//      Disease.numberOfRTPCRTestsDoneOnEachDay = Disease.numberOfRTPCRTestsDoneOnEachDay + 1
//    }
//  }
//
//  private val checkEligibilityForTargetedRATTesting:Context => Unit = (context:Context) => {
//    //println(isEligibleForTestingAgain(context))
//    if ((context.activeInterventionNames.contains("get_tested"))&&
//      (context.getCurrentStep%Disease.numberOfTicksInADay==0)&&
//      (Disease.numberOfRATTestsDoneOnEachDay < Disease.numberOfRATTestsAvailable) &&
//      (isSymptomatic) &&
//      (!isBeingTested) &&
//      (!isScheduledForRTPCRTesting)&&
//      (Disease.numberOfRTPCRTestsDoneOnEachDay >= Disease.numberOfRTPCRTestsAvailable)) {
//      updateParam("isScheduledForRATTesting", true)
//      Disease.numberOfRATTestsDoneOnEachDay = Disease.numberOfRATTestsDoneOnEachDay + 1
//    }
//  }
//
//  private val checkEligibilityForRandomRTPCRTesting:Context => Unit = (context:Context) =>{
//    if ((context.activeInterventionNames.contains("get_tested"))&&
//      (context.getCurrentStep%Disease.numberOfTicksInADay==0)){
//      if((Disease.numberOfRTPCRTestsDoneOnEachDay < Disease.numberOfRTPCRTestsAvailable) &&
//         (!isScheduledForRATTesting)&&
//         (!isScheduledForRTPCRTesting)&&
//         (!isBeingTested)&&
//         (isSusceptible || isAsymptomatic || isPresymptomatic)&&
//         (getSymptomaticCount(context) < Disease.numberOfRTPCRTestsAvailable)){
//        updateParam("isScheduledForRandomRTPCRTesting",true)
//        Disease.numberOfRTPCRTestsDoneOnEachDay = Disease.numberOfRTPCRTestsDoneOnEachDay+1
//      }
//    }
//  }

//  private val checkEligibilityForRandomRATTesting: Context => Unit = (context:Context) => {
//    if ((context.activeInterventionNames.contains("get_tested")) &&
//      (context.getCurrentStep % Disease.numberOfTicksInADay == 0)) {
//      if ((Disease.numberOfRATTestsDoneOnEachDay < Disease.numberOfRATTestsAvailable) &&
//        (!isScheduledForRATTesting) &&
//        (!isScheduledForRTPCRTesting) &&
//        (!isScheduledForRandomRTPCRTesting) &&
//        (!isBeingTested) &&
//        (isSusceptible || isAsymptomatic || isPresymptomatic) &&
//        (getSymptomaticCount(context) < Disease.numberOfRATTestsAvailable + Disease.numberOfRTPCRTestsAvailable)) {
//        updateParam("isScheduledForRandomRATTesting", true)
//        Disease.numberOfRATTestsDoneOnEachDay = Disease.numberOfRATTestsDoneOnEachDay + 1
//
//
//      }
//    }
//  }

  private val checkEligibilityForTargetedTesting:Context => Unit = (context: Context)=>{
    if((context.activeInterventionNames.contains("get_tested"))&&
      (isSymptomatic)&&
      (!isBeingTested)){
      updateParam("isEligibleForTargetedTesting",true)
    }
  }

  private val checkEligibilityForRandomTesting:Context => Unit = (context: Context)=>{
    //println(!isBeingTested)
    if((context.activeInterventionNames.contains("get_tested"))&&
      (!isHospitalized)&&
      (!isBeingTested)){
      updateParam("isEligibleForRandomTesting",true)
    }
  }

  private val checkForContacts:Context => Unit = (context:Context) => {
    if (isDelayPeriodOver(context)){
      if (lastTestResult){
//        val family = this.getConnections(houseId.toString).toList
        val house = this.houseId
        println(house)
        if (this.houseId == house){
          updateParam("isAContact",true)
        }
      }
    }
  }


  private val declarationOfResults:Context => Unit = (context:Context) => {
    if (beingTested == 1 && isDelayPeriodOver(context)){
      if (lastTestResult) {
        updateParam("beingTested", 2)
        updateParam("quarantineStartedAt", (context.getCurrentStep * Disease.dt).toInt)
      }
      //TODO RAT tests don't have delay
      if (!lastTestResult){
        updateParam("beingTested",0)
      }
    }
  }

  private val quarantinePeriodOver:Context => Unit = (context:Context) => {
    if (beingTested == 2 &&
      ((((context.getCurrentStep*Disease.dt).toInt)- quarantineStartedAt) >= Disease.quarantineDuration)){
      updateParam("beingTested",0)
    }
  }

  def isSusceptible: Boolean = infectionState == Susceptible
  def isAsymptomatic: Boolean = infectionState == Asymptomatic

  def isPresymptomatic: Boolean = infectionState == Presymptomatic

  def isMildlyInfected:Boolean = infectionState == MildlyInfected

  def isSeverelyInfected:Boolean = infectionState == SeverelyInfected

  def isHospitalized:Boolean = infectionState == Hospitalized

  def isRecovered: Boolean = infectionState == Recovered

  def isBeingTested:Boolean = beingTested == 1 || beingTested == 2


  def isAwaitingResult:Boolean = beingTested == 1

  def isSymptomatic: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected

  def isQuarantined:Boolean = beingTested == 2

  def isEligibleForRTPCR:Boolean = isEligibleForTargetedTesting

//  def isEligibleForTestingAgain(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain

  def isDelayPeriodOver(context: Context):Boolean = ((context.getCurrentStep*Disease.dt).toInt) - lastTestDay >= Disease.testDelay



  def getSymptomaticCount(context: Context): Int = {
    context.graphProvider.fetchCount("Person", ("infectionState" equ MildlyInfected) or ("infectionState" equ SeverelyInfected))
  }





  def decodeNode(classType: String, node: GraphNode): Node = {
    classType match {
      case "House" => node.as[House]
      case "Office" => node.as[Office]
      //case "School" => node.as[School]
      case "Hospital" => node.as[Hospital]
    }
  }


  addBehaviour(incrementInfectionDay)
  addBehaviour(checkCurrentLocation)
  addBehaviour(checkEligibilityForTargetedTesting)
  addBehaviour(checkEligibilityForRandomTesting)
  addBehaviour(declarationOfResults)
  addBehaviour(quarantinePeriodOver)
  //addBehaviour(checkForContacts)


  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  //addRelation[School]("STUDIES_AT")
  addRelation[Hospital]("WORKS_IN/ADMITTED")

}
