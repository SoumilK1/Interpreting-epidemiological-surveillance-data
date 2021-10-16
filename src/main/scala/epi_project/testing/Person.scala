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

  private val checkEligibilityForTesting:Context => Unit = (context:Context) => {
    //println(isEligibleForTestingAgain(context))
    if (context.activeInterventionNames.contains("get_tested") &&
      (Disease.numberOfTestsDoneAtEachTick < Disease.numberOfTestsAvailable) &&
      (isSymptomatic) &&
      (!isBeingTested) &&
      (isEligibleForTestingAgain(context))) {
      updateParam("isScheduledForTesting", true)
      Disease.numberOfTestsDoneAtEachTick = Disease.numberOfTestsDoneAtEachTick + 1
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






  def isSusceptible: Boolean = infectionState == Susceptible

  def isPresymptomatic: Boolean = infectionState == Presymptomatic

  def isMildlyInfected:Boolean = infectionState == MildlyInfected

  def isSeverelyInfected:Boolean = infectionState == SeverelyInfected

  def isHospitalized:Boolean = infectionState == Hospitalized

  def isRecovered: Boolean = infectionState == Recovered

  def isBeingTested:Boolean = beingTested == 1 || beingTested == 2


  def isAwaitingResult:Boolean = beingTested == 1

  def isSymptomatic: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected

  def isQuarantined:Boolean = beingTested == 2

  def isEligible:Boolean = isScheduledForTesting

  def isEligibleForTestingAgain(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain

  def isDelayPeriodOver(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.testDelay



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
  addBehaviour(checkCurrentLocation)
  addBehaviour(checkEligibilityForTesting)
  addBehaviour(declarationOfResults)
  addBehaviour(quarantinePeriodOver)


  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[School]("STUDIES_AT")
  addRelation[Hospital]("WORKS_IN/ADMITTED")

}
