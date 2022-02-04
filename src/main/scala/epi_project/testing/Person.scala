package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.models.{Node, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import epi_project.testing.InfectionStatus._

case class Person(id: Long,
                  houseId:Long,
                  officeId:Long,
                  age: Int,
                  infectionState: InfectionStatus,
                  infectionDur: Int,
                  essentialWorker:Int,
                  roadId:Long,
                  cemeteryId:Long,
                  beingTested:Int = 0,
                  isEligibleForTargetedTesting:Boolean = false,
                  isEligibleForRandomTesting:Boolean = false,
                  lastTestResult:Boolean = false,
                  lastTestDay:Int = -20000,
                  currentLocation:String = "House",
                  quarantineStartedAt:Int = 0,
                  isAContact:Int =0,
                  testCategory:Int = 0,
                  contactIsolationStartedAt:Int = 0) extends StatefulAgent {


  private val incrementInfectionDay: Context => Unit = (context: Context) => {
    if (isPresymptomatic && context.getCurrentStep % Disease.numberOfTicksInADay == 0)
      updateParam("infectionDur", infectionDur + 1)
  }

  private val checkCurrentLocation: Context => Unit = (context: Context) => {
    val schedule = context.fetchScheduleFor(this).get
    val locationNextTick: String = schedule.getForStep(context.getCurrentStep + 1)
    if (currentLocation != locationNextTick) {
      this.updateParam("currentLocation", locationNextTick)

    }
  }

  private val checkEligibilityForTargetedTesting:Context => Unit = (context: Context)=>{
    if((context.activeInterventionNames.contains("get_tested"))&&
      (isSymptomatic)&&
      (!isBeingTested)){


      /**
       *
       *
       * The following if statement ensures that not every symptomatic person isEligibleForTargetedTesting
       *
       */


      if(biasedCoinToss(Disease.probabilityOfReportingSymptoms)){
        updateParam("isEligibleForTargetedTesting",true)
      }
    }
  }

  private val checkEligibilityForRandomTesting:Context => Unit = (context: Context)=>{
    //println(!isBeingTested)
    if((context.activeInterventionNames.contains("get_tested"))&&
      (!isHospitalized)&&
      (!isDead)&&
      (!isBeingTested)){
      updateParam("isEligibleForRandomTesting",true)
    }
  }

  private val declarationOfResults_checkForContacts:Context => Unit = (context:Context) => {
    if (beingTested == 1 && isDelayPeriodOver(context)){
      if (lastTestResult){
        if (Disease.DoesContactTracingHappen == "y"){
          val places = getConnections(getRelation("House").get).toList
          val place = places.head
          val home = decodeNode("House", place)

          val family = home.getConnections(home.getRelation[Person]().get).toList

          for (i <- family.indices){
            val familyMember = family(i).as[Person]
            if ((familyMember.beingTested == 0) && (familyMember.isAContact == 0) && (!familyMember.isHospitalized) &&(!familyMember.isDead)) {
              familyMember.updateParam("isAContact", 1)
            }
          }
          if (essentialWorker == 0){
            val workplaces = getConnections(getRelation("Office").get).toList
            val workplace = workplaces.head
            val office = decodeNode("Office", workplace)

            val workers = office.getConnections(office.getRelation[Person]().get).toList

            for (i <- workers.indices) {
              val Colleague = workers(i).as[Person]
              if ((Colleague.beingTested == 0) && (Colleague.isAContact==0) && (!Colleague.isHospitalized) &&(!Colleague.isDead)){
                if (biasedCoinToss(Disease.colleagueFraction)) {
                  if(Colleague.isSymptomatic){
                    Colleague.updateParam("isAContact", 2)
                  }
                  if(!Colleague.isSymptomatic){
                    Colleague.updateParam("isAContact",3)
                    Colleague.updateParam("beingTested",2)
                    Colleague.updateParam("contactIsolationStartedAt",(context.getCurrentStep * Disease.dt).toInt)
                  }
                }
              }
            }
          }
        }
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

  /**
   * The following behaviour makes sure that low-risk contacts who are asymptomatic are not quarantined/isolated for more than 7 days
   *
   *
   *
   *
   */

  private val contactIsolationPeriodOver:Context => Unit = (context:Context) => {
    if ((isAContact==3)
      &&((((context.getCurrentStep*Disease.dt).toInt)-contactIsolationStartedAt) >= Disease.isolationDuration))
      {
        updateParam("isAContact",0)
        updateParam("beingTested",0)
      }
  }

  private val printStuff:Context => Unit = (context:Context) =>{
    if (context.getCurrentStep == Disease.numberOfTicks){


    }
  }

  def isSusceptible: Boolean = infectionState == Susceptible

  def isAsymptomatic: Boolean = infectionState == Asymptomatic

  def isPresymptomatic: Boolean = infectionState == Presymptomatic

  def isMildlyInfected:Boolean = infectionState == MildlyInfected

  def isSeverelyInfected:Boolean = infectionState == SeverelyInfected

  def isHospitalized:Boolean = infectionState == Hospitalized

  def isRecovered: Boolean = infectionState == Recovered

  def isDead: Boolean = infectionState == Dead

  def isBeingTested:Boolean = beingTested == 1 || beingTested == 2

  def isAwaitingResult:Boolean = beingTested == 1

  def isSymptomatic: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected

  def isQuarantined:Boolean = beingTested == 2

  def isEligibleForRTPCR:Boolean = isEligibleForTargetedTesting

//  def isEligibleForTestingAgain(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain

  def isDelayPeriodOver(context: Context):Boolean = ((context.getCurrentStep*Disease.dt).toInt) - lastTestDay >= Disease.testDelay

  def decodeNode(classType: String, node: GraphNode): Node = {
    classType match {
      case "House" => node.as[House]
      case "Office" => node.as[Office]
      case "Hospital" => node.as[Hospital]
    }
  }


  addBehaviour(incrementInfectionDay)
  addBehaviour(checkCurrentLocation)
  addBehaviour(checkEligibilityForTargetedTesting)
  addBehaviour(checkEligibilityForRandomTesting)
  addBehaviour(declarationOfResults_checkForContacts)
  addBehaviour(quarantinePeriodOver)
  addBehaviour(contactIsolationPeriodOver)
  addBehaviour(printStuff)



  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  //addRelation[School]("STUDIES_AT")
  addRelation[Hospital]("WORKS_IN/ADMITTED")
  addRelation[Cemetery]("BURIED_IN")

}
