package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Agent, Node, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing.InfectionStatus._
import epi_project.testing.Main.testing

<<<<<<< HEAD
case class Person(id: Long,
                  age: Int,
                  infectionState: InfectionStatus,
                  infectionDur: Int,
                  essentialWorker:Int,
                  beingTested:Int = 0,
                  isScheduledForTesting:Boolean = false,
                  lastTestResult:String = "n",
                  lastTestDay:Int = -20000,
                  currentLocation:String = "House",
                  quarantineStartedAt:Int = 0) extends StatefulAgent {
=======
case class Person(id: Long, age: Int, infectionState: InfectionStatus, infectionDur: Int, beingTested:Int = 0,isEligible:Boolean = false, testStatus:String = "n", lastTestDay:Int = 0) extends StatefulAgent {
>>>>>>> 1db83b647150178fc192d3230657413f70180851

  private val incrementInfectionDay: Context => Unit = (context: Context) => {
    if (isPresymptomatic && context.getCurrentStep % Disease.numberOfTicksInADay == 0)
      updateParam("infectionDur", infectionDur + 1)
  }

<<<<<<< HEAD
  private val checkCurrentLocation: Context => Unit = (context: Context) => {
    val schedule = context.fetchScheduleFor(this).get
    val locationNextTick: String = schedule.getForStep(context.getCurrentStep + 1)
    if (currentLocation != locationNextTick) {
      this.updateParam("currentLocation", locationNextTick)
      //println(currentLocation)
    }
  }

  private val checkEligibilityForTesting:Context => Unit = (context:Context) => {
    //println(isEligibleForTestingAgain(context))
    if (context.activeInterventionNames.contains("get_tested") &&
      (Disease.numberOfTestsDoneAtEachTick < Disease.numberOfTestsAvailable) &&
      (isSymptomatic) &&
      (!isBeingTested) &&
      (isEligibleForTestingAgain(context))){
      updateParam("isScheduledForTesting",true)
=======
  private val checkEligibilityForTesting:Context => Unit = (context:Context) => {
    if (getRecoveredCount(context)>2000 && Disease.numberOfTestsDoneAtEachTick < Disease.numberOfTestsAvailable && isSymptomatic && !isBeingTested && !isPositive && isEligibleForTestingAgain(context)){
      updateParam("isEligible",true)
>>>>>>> 1db83b647150178fc192d3230657413f70180851
      Disease.numberOfTestsDoneAtEachTick = Disease.numberOfTestsDoneAtEachTick + 1
      //println(Disease.numberOfTestsDoneAtEachTick)
    }
  }

  private val declarationOfResults:Context => Unit = (context:Context) => {
    if (beingTested == 1 && isDelayPeriodOver(context)){
      if (lastTestResult == "p"){
        updateParam("beingTested",2)
        updateParam("quarantineStartedAt",context.getCurrentStep/Disease.numberOfTicksInADay)
      }
      if (lastTestResult == "n"){
        updateParam("beingTested",0)
      }
    }
  }

  private val quarantinePeriodOver:Context => Unit = (context:Context) => {
    if (beingTested == 2 &&
      (context.getCurrentStep/Disease.numberOfTicksInADay- quarantineStartedAt >= Disease.quarantineDuration)){
      updateParam("beingTested",0)
    }
  }

<<<<<<< HEAD
=======
  private val declarationOfResults:Context => Unit = (context:Context) => {
    if (beingTested == 1 && isDelayPeriodOver(context)){
      if (testStatus == "p"){
        updateParam("beingTested",2)
      }
      if (testStatus == "n"){
        updateParam("beingTested",0)
      }
    }
  }


>>>>>>> 1db83b647150178fc192d3230657413f70180851

  def isSusceptible: Boolean = infectionState == Susceptible

  def isPresymptomatic: Boolean = infectionState == Presymptomatic

  def isMildlyInfected:Boolean = infectionState == MildlyInfected

  def isSeverelyInfected:Boolean = infectionState == SeverelyInfected

  def isHospitalized:Boolean = infectionState == Hospitalized

  def isRecovered: Boolean = infectionState == Recovered

  def isBeingTested:Boolean = beingTested == 1 || beingTested == 2

<<<<<<< HEAD
  def isAwaitingResult:Boolean = beingTested == 1

  def isSymptomatic: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected

  def isQuarantined:Boolean = beingTested == 2

  def isEligible:Boolean = isScheduledForTesting

  def isEligibleForTestingAgain(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain

  def isDelayPeriodOver(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.testDelay

  
=======
  def isSymptomatic: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected

  def isPositive:Boolean = testStatus == "p"

  def isEligibleForTesting:Boolean = isEligible

  def isEligibleForTestingAgain(context: Context):Boolean = ((context.getCurrentStep)/Disease.numberOfTicksInADay - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain) && lastTestDay != 0

  def isDelayPeriodOver(context: Context):Boolean = (context.getCurrentStep)/Disease.numberOfTicksInADay - lastTestDay >= Disease.testDelay && lastTestDay != 0
>>>>>>> 1db83b647150178fc192d3230657413f70180851

  private def getRecoveredCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Recovered)
  }



  def decodeNode(classType: String, node: GraphNode): Node = {
    classType match {
      case "House" => node.as[House]
      case "Office" => node.as[Office]
      case "School" => node.as[School]
      case "Hospital" => node.as[Hospital]
    }
  }

  addBehaviour(incrementInfectionDay)
<<<<<<< HEAD
  addBehaviour(checkCurrentLocation)
  addBehaviour(checkEligibilityForTesting)
  addBehaviour(declarationOfResults)
  addBehaviour(quarantinePeriodOver)
=======
  addBehaviour(checkEligibilityForTesting)
  addBehaviour(declarationOfResults)
>>>>>>> 1db83b647150178fc192d3230657413f70180851


  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[School]("STUDIES_AT")
  addRelation[Hospital]("WORKS_IN/ADMITTED")

}
