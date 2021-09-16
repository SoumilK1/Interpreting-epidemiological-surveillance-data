package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Agent, Node, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing.InfectionStatus._

case class Person(id: Long, age: Int, infectionState: InfectionStatus, infectionDur: Int, isTested:String = "false") extends StatefulAgent {

  private val incrementInfectionDay: Context => Unit = (context: Context) => {
    if (isPresymptomatic && context.getCurrentStep % Disease.numberOfTicksInADay == 0) {
      updateParam("infectionDur", infectionDur + 1)
    }
  }

  private val testing:Context => Unit = (context:Context) => {
    if (isSick && Disease.numberOfTestsDoneAtEachTick < Disease.numberOfTestsAvailable && !Tested && getRecoveredCount(context) >= 2000){
      updateParam("isTested","true")
      Disease.numberOfTestsDoneAtEachTick = Disease.numberOfTestsDoneAtEachTick + 1
    }
  }

  def isSusceptible: Boolean = infectionState == Susceptible

  def isPresymptomatic: Boolean = infectionState == Presymptomatic

  def isMildlyInfected:Boolean = infectionState == MildlyInfected

  def isSeverelyInfected:Boolean = infectionState == SeverelyInfected

  def isHospitalized:Boolean = infectionState == Hospitalized

  def isRecovered: Boolean = infectionState == Recovered

  def Tested:Boolean = isTested == "true"

  def isSick: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected


  private def getRecoveredCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Recovered)
  }



  def decodeNode(classType: String, node: GraphNode): Node = {
    classType match {
      case "House" => node.as[House]
      case "Office" => node.as[Office]
      case "School" => node.as[School]
    }
  }

  addBehaviour(incrementInfectionDay)
  addBehaviour(testing)


  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[School]("STUDIES_AT")
  addRelation[Hospital]("ADMITTED_AT")

}
