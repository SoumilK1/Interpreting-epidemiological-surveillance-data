package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.models.{Node, StatefulAgent}
import epi_project.testing.InfectionStatus._

case class Person(id: Long,
                  age: Int,
                  infectionState: InfectionStatus,
                  infectionDur: Int,
                  essentialWorker:Int,
                  beingTested:Int = 0,
                  isScheduledForRTPCRTesting:Boolean = false,
                  isScheduledForRATTesting:Boolean = false,
                  isScheduledForRandomRTPCRTesting:Boolean = false,
                  isScheduledForRandomRATTesting:Boolean = false,
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

  private val checkEligibilityForTargetedRTPCRTesting:Context => Unit = (context:Context) => {
    //println(isEligibleForTestingAgain(context))
    if ((context.activeInterventionNames.contains("get_tested"))&&
      (context.getCurrentStep%Disease.numberOfTicksInADay==0)&&
      (Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.numberOfRTPCRTestsAvailable) &&
      (isSymptomatic) &&
      (!isBeingTested)) {
      updateParam("isScheduledForRTPCRTesting", true)
      Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick + 1
    }
  }
  private val checkEligibilityForTargetedRATTesting:Context => Unit = (context:Context) => {
    //println(isEligibleForTestingAgain(context))
    if ((context.activeInterventionNames.contains("get_tested"))&&
      (context.getCurrentStep%Disease.numberOfTicksInADay==0)&&
      (Disease.numberOfRATTestsDoneAtEachTick < Disease.numberOfRATTestsAvailable) &&
      (isSymptomatic) &&
      (!isBeingTested) &&
      (!isScheduledForRTPCRTesting))
    {
      updateParam("isScheduledForRATTesting", true)
      Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick + 1
    }
  }
  private val checkEligibilityForRandomTesting:Context => Unit = (context:Context) =>{
    if ((context.activeInterventionNames.contains("get_tested"))&&
      (context.getCurrentStep%Disease.numberOfTicksInADay==0)){
      if((Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.numberOfRTPCRTestsAvailable) &&
         (!isScheduledForRATTesting)&&
         (!isScheduledForRTPCRTesting)&&
         (beingTested!=2)&&
         (!isHospitalized)){
        updateParam("isScheduledForRandomRTPCRTesting",true)
        Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick+1
      }
      else if((Disease.numberOfRATTestsDoneAtEachTick < Disease.numberOfRATTestsAvailable)&&
        (!isScheduledForRATTesting)&&
        (!isScheduledForRTPCRTesting)&&
        (!isScheduledForRandomRTPCRTesting)&&
        (beingTested!=2)&&
        (!isHospitalized)){
        updateParam("isScheduledForRandomRATTesting",true)
        Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick + 1
      }

    }
  }

  private val declarationOfResults:Context => Unit = (context:Context) => {
    if (beingTested == 1 && isDelayPeriodOver(context)){
      if (lastTestResult == true){
        updateParam("beingTested",2)
        updateParam("quarantineStartedAt",(context.getCurrentStep*Disease.dt).toInt)
      }
      if (lastTestResult == false){
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

  def isEligibleForRTPCR:Boolean = isScheduledForRTPCRTesting

//  def isEligibleForTestingAgain(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain

  def isDelayPeriodOver(context: Context):Boolean = ((context.getCurrentStep*Disease.dt).toInt) - lastTestDay >= Disease.testDelay

  def isScheduledForTesting:Boolean = isScheduledForRATTesting || isScheduledForRTPCRTesting






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
  addBehaviour(checkEligibilityForTargetedRTPCRTesting)
  addBehaviour(checkEligibilityForTargetedRATTesting)
  addBehaviour(checkEligibilityForRandomTesting)
  addBehaviour(declarationOfResults)
  addBehaviour(quarantinePeriodOver)


  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[School]("STUDIES_AT")
  addRelation[Hospital]("WORKS_IN/ADMITTED")

}
