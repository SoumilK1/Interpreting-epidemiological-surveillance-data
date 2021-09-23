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

case class Person(id: Long, age: Int, infectionState: InfectionStatus, infectionDur: Int, beingTested:Int = 0,isEligible:Boolean = false, testStatus:String = "n", lastTestDay:Int = 0) extends StatefulAgent {

  private val incrementInfectionDay: Context => Unit = (context: Context) => {
    if (isPresymptomatic && context.getCurrentStep % Disease.numberOfTicksInADay == 0)
      updateParam("infectionDur", infectionDur + 1)
  }

  private val checkEligibilityForTesting:Context => Unit = (context:Context) => {
    if (getRecoveredCount(context)>2000 && Disease.numberOfTestsDoneAtEachTick < Disease.numberOfTestsAvailable && isSymptomatic && !isBeingTested && !isPositive && isEligibleForTestingAgain(context)){
      updateParam("isEligible",true)
      Disease.numberOfTestsDoneAtEachTick = Disease.numberOfTestsDoneAtEachTick + 1
    }
  }

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



  def isSusceptible: Boolean = infectionState == Susceptible

  def isPresymptomatic: Boolean = infectionState == Presymptomatic

  def isMildlyInfected:Boolean = infectionState == MildlyInfected

  def isSeverelyInfected:Boolean = infectionState == SeverelyInfected

  def isHospitalized:Boolean = infectionState == Hospitalized

  def isRecovered: Boolean = infectionState == Recovered

  def isBeingTested:Boolean = beingTested == 1 || beingTested == 2

  def isSymptomatic: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected

  def isPositive:Boolean = testStatus == "p"

  def isEligibleForTesting:Boolean = isEligible

  def isEligibleForTestingAgain(context: Context):Boolean = ((context.getCurrentStep)/Disease.numberOfTicksInADay - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain) && lastTestDay != 0

  def isDelayPeriodOver(context: Context):Boolean = (context.getCurrentStep)/Disease.numberOfTicksInADay - lastTestDay >= Disease.testDelay && lastTestDay != 0

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
  addBehaviour(checkEligibilityForTesting)
  addBehaviour(declarationOfResults)


  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[School]("STUDIES_AT")
  addRelation[Hospital]("ADMITTED_AT")

}
