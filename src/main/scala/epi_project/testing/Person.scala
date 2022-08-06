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
                  neighbourhoodID:Long,
                  age: Int,
                  ageStratifiedDeltaMultiplier:Double,
                  ageStratifiedSigmaMultiplier:Double,
                  ageStratifiedMuMultiplier:Double,
                  ageStratifiedGammaMultiplier:Double,
                  infectionState: InfectionStatus,
                  infectionDur: Int,
                  essentialWorker:Int,
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
                  contactIsolationStartedAt:Int = 0,
                  numberOfDaysSpentInSI:Int = 0,
                  typeOfTestGiven:Int = 0,
                  numberOfDaysSpentInHospital:Int = 0,
                  deadCount:Int = 0,
                  deathDay:Int = 0,
                  dayAtWhichPersonIsInfected:Int = -20,
                  dayAtWhichPersonIsIdentified:Int = -30) extends StatefulAgent {


  private val incrementInfectionDay: Context => Unit = (context: Context) => {
    if (isPresymptomatic && context.getCurrentStep % Disease.numberOfTicksInADay == 0)
      updateParam("infectionDur", infectionDur + 1)
  }

  private val incrementNumberOfDaysInSI:Context => Unit = (context:Context) => {
    if (isSeverelyInfected && context.getCurrentStep % Disease.numberOfTicksInADay == 0){
      updateParam("numberOfDaysSpentInSI", numberOfDaysSpentInSI + 1)
    }
  }

  private val checkCurrentLocation: Context => Unit = (context: Context) => {
    val schedule = context.fetchScheduleFor(this).get
    val locationNextTick: String = schedule.getForStep(context.getCurrentStep + 1)
    if (currentLocation != locationNextTick) {
      this.updateParam("currentLocation", locationNextTick)

    }
  }

  private val checkEligibilityForTargetedSusceptiblePeople: Context => Unit =(context:Context)=>{
    if((isSusceptible)&&(isNotTested)){
      if (biasedCoinToss(Disease.basalFraction)){
        updateParam("isEligibleForTargetedTesting",true)
        updateParam("beingTested",3)
        Disease.numberOfPeopleSelfReported+=1
      }
    }
  }

  private val checkEligibilityForTargetedTesting:Context => Unit = (context: Context)=>{

    if(
//
      (isSymptomatic)&&

      (isNotTested)) {


      /**
       *
       *
       * The following if statement ensures that not every symptomatic person isEligibleForTargetedTesting
       *
       */

      if (isSeverelyInfected) {
        if (biasedCoinToss(Disease.probabilityOfReportingSevereSymptoms)) {
          updateParam("isEligibleForTargetedTesting", true)
          //println("id",id,"status",beingTested)
          updateParam("beingTested", 3)
          Disease.numberOfPeopleSelfReported = Disease.numberOfPeopleSelfReported + 1
        }
      }

      if (isMildlyInfected) {
        if (biasedCoinToss(Disease.probabilityOfReportingMildSymptoms)) {
          updateParam("isEligibleForTargetedTesting", true)
          //println("id",id,"status",beingTested)
          updateParam("beingTested", 3)
          Disease.numberOfPeopleSelfReported = Disease.numberOfPeopleSelfReported + 1
        }
      }
    }
    //TODO - Add contacts?

//    if (isSusceptible){
//      if (biasedCoinToss(Disease.probabilityOfNotHavingCOVID)){
//        updateParam("beingTested",3)
//        updateParam("isEligibleForTargetedTesting",true)
//        Disease.numberOfPeopleSelfReported = Disease.numberOfPeopleSelfReported + 1
//      }
//    }
  }

  private val checkEligibilityForRandomTesting:Context => Unit = (context: Context)=> {
    if (Disease.DoesRandomTestingHappen == "y") {
      if((context.activeInterventionNames.contains("get_tested"))&&
        (!isHospitalized) &&
        (!isDead) &&
        (isNotTested)) {
        updateParam("isEligibleForRandomTesting", true)
      }
    }
  }

  private val declarationOfResults_checkForContacts:Context => Unit = (context:Context) => {
    if ((beingTested == 1) && (typeOfTestGiven == 1) && (isDelayPeriodOver(context))){
      if (lastTestResult){
        if (Disease.DoesContactTracingHappen == "y"){
          val places = getConnections(getRelation("House").get).toList
          val place = places.head
          val home = decodeNode("House", place)

          val family = home.getConnections(home.getRelation[Person]().get).toList

          for (i <- family.indices){
            val familyMember = family(i).as[Person]
            //println("id",familyMember.id,"Status", familyMember.beingTested)
            if ((familyMember.beingTested == 0) && (familyMember.isAContact == 0) && (!familyMember.isHospitalized) &&(!familyMember.isDead)) {
              //println("GGs-2")
              familyMember.updateParam("isAContact", 1)
              familyMember.updateParam("beingTested",3)
            }
          }
          if (essentialWorker == 0){
            val workplaces = getConnections(getRelation("Office").get).toList
            val workplace = workplaces.head
            val office = decodeNode("Office", workplace)

            val workers = office.getConnections(office.getRelation[Person]().get).toList

            for (i <- workers.indices) {
              val Colleague = workers(i).as[Person]
              if ( (Colleague.beingTested == 0) && (Colleague.isAContact==0) && (!Colleague.isHospitalized) &&(!Colleague.isDead)){
                //println("GGs-3")
                if (biasedCoinToss(Disease.colleagueFraction)) {
                  if(Colleague.isSymptomatic){
                    Colleague.updateParam("isAContact", 2)
                    Colleague.updateParam("beingTested",3)
                  }
                  if(!Colleague.isSymptomatic){
                    Colleague.updateParam("isAContact",3)
                    Colleague.updateParam("beingTested",4)
                    Colleague.updateParam("contactIsolationStartedAt",(context.getCurrentStep * Disease.dt).toInt)
                  }
                }
              }
            }
          }

          val neighbourhoods = getConnections(getRelation("Neighbourhood").get).toList
          val neighbourhood = neighbourhoods.head
          val area = decodeNode("Neighbourhood",neighbourhood)


          val neighbours = area.getConnections(area.getRelation[Person]().get).toList

          for (i <- neighbours.indices) {
            val sameNeighbourhoodPerson = neighbours(i).as[Person]
            if ( (sameNeighbourhoodPerson.beingTested == 0) && (sameNeighbourhoodPerson.isAContact==0) && (!sameNeighbourhoodPerson.isHospitalized) &&(!sameNeighbourhoodPerson.isDead)){
                //println("GGs-3")
              if (biasedCoinToss(Disease.neighbourFraction)) {
                if(sameNeighbourhoodPerson.isSymptomatic){
                  sameNeighbourhoodPerson.updateParam("isAContact", 2)
                  sameNeighbourhoodPerson.updateParam("beingTested",3)
                }
                if(!sameNeighbourhoodPerson.isSymptomatic){
                  sameNeighbourhoodPerson.updateParam("isAContact",3)
                  sameNeighbourhoodPerson.updateParam("beingTested",4)
                  sameNeighbourhoodPerson.updateParam("contactIsolationStartedAt",(context.getCurrentStep * Disease.dt).toInt)
                }
              }
            }
          }
        }
        updateParam("beingTested", 2)
        updateParam("quarantineStartedAt", (context.getCurrentStep * Disease.dt).toInt)
        updateParam("dayAtWhichPersonIsIdentified",(context.getCurrentStep/Disease.numberOfTicksInADay)+1)

        if (context.getCurrentStep - Disease.testDelay*Disease.numberOfTicksInADay == Disease.testingStartedAt){
          Disease.identifiedSeed = Disease.identifiedSeed + 1
        }
      }

      if (!lastTestResult){
        updateParam("beingTested",0)
      }
    }

    if ((beingTested == 1) && (typeOfTestGiven == 2)){
      if (lastTestResult){
        if (Disease.DoesContactTracingHappen == "y"){
          val places = getConnections(getRelation("House").get).toList
          val place = places.head
          val home = decodeNode("House", place)

          val family = home.getConnections(home.getRelation[Person]().get).toList

          for (i <- family.indices){
            val familyMember = family(i).as[Person]
            //println("id",familyMember.id,"Status", familyMember.beingTested)
            if ((familyMember.beingTested == 0) && (familyMember.isAContact == 0) && (!familyMember.isHospitalized) &&(!familyMember.isDead)) {
              //println("GGs-2")
              familyMember.updateParam("isAContact", 1)
              familyMember.updateParam("beingTested",3)
            }
          }
          if (essentialWorker == 0){
            val workplaces = getConnections(getRelation("Office").get).toList
            val workplace = workplaces.head
            val office = decodeNode("Office", workplace)

            val workers = office.getConnections(office.getRelation[Person]().get).toList

            for (i <- workers.indices) {
              val Colleague = workers(i).as[Person]
              if ( (Colleague.beingTested == 0) && (Colleague.isAContact==0) && (!Colleague.isHospitalized) &&(!Colleague.isDead)){
                //println("GGs-3")
                if (biasedCoinToss(Disease.colleagueFraction)) {
                  if(Colleague.isSymptomatic){
                    Colleague.updateParam("isAContact", 2)
                    Colleague.updateParam("beingTested",3)
                  }
                  if(!Colleague.isSymptomatic){
                    Colleague.updateParam("isAContact",3)
                    Colleague.updateParam("beingTested",4)
                    Colleague.updateParam("contactIsolationStartedAt",(context.getCurrentStep * Disease.dt).toInt)
                  }
                }
              }
            }
          }

          val neighbourhoods = getConnections(getRelation("Neighbourhood").get).toList
          val neighbourhood = neighbourhoods.head
          val area = decodeNode("Neighbourhood",neighbourhood)


          val neighbours = area.getConnections(area.getRelation[Person]().get).toList

          for (i <- neighbours.indices) {
            val sameNeighbourhoodPerson = neighbours(i).as[Person]
            if ( (sameNeighbourhoodPerson.beingTested == 0) && (sameNeighbourhoodPerson.isAContact==0) && (!sameNeighbourhoodPerson.isHospitalized) &&(!sameNeighbourhoodPerson.isDead)){
              //println("GGs-3")
              if (biasedCoinToss(Disease.neighbourFraction)) {
                if(sameNeighbourhoodPerson.isSymptomatic){
                  sameNeighbourhoodPerson.updateParam("isAContact", 2)
                  sameNeighbourhoodPerson.updateParam("beingTested",3)
                }
                if(!sameNeighbourhoodPerson.isSymptomatic){
                  sameNeighbourhoodPerson.updateParam("isAContact",3)
                  sameNeighbourhoodPerson.updateParam("beingTested",4)
                  sameNeighbourhoodPerson.updateParam("contactIsolationStartedAt",(context.getCurrentStep * Disease.dt).toInt)
                }
              }
            }
          }
        }
        updateParam("beingTested", 2)
        updateParam("quarantineStartedAt", (context.getCurrentStep * Disease.dt).toInt)
        updateParam("dayAtWhichPersonIsIdentified",(context.getCurrentStep/Disease.numberOfTicksInADay)+1)

        if (context.getCurrentStep - Disease.testDelay*Disease.numberOfTicksInADay == Disease.testingStartedAt){
          Disease.identifiedSeed = Disease.identifiedSeed + 1
        }
      }

      if (!lastTestResult){
        updateParam("beingTested",0)
      }
    }
  }



  private val quarantinePeriodOver:Context => Unit = (context:Context) => {
    if ((beingTested == 2)  && (lastTestDay >= 0) &&
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

  private val countParams: Context => Unit = (context:Context) => {

    if (context.getCurrentStep % Disease.numberOfTicksInADay == 0) {
      Disease.numberOfDeadOnEachDay = 0.0
      Disease.numberOfUntestedDeadOnEachDay = 0.0
      Disease.newlyInfectedEveryDay = 0.0
      Disease.newlyRecoveredEveryDay = 0.0
//      Disease.numberOfActiveCasesOnEachDay = 0.0
    }
    if (context.getCurrentStep == 1){
      if (isAsymptomatic){
        updateParam("dayAtWhichPersonIsInfected",1)
      }
    }



  }

//  private val printStuff:Context => Unit = (context:Context) => {
//    if (id == 500){
////      println("behaviour")
//      println("RIP",Disease.numberOfDeadOnEachDay)
//    }
//  }


  def isSusceptible: Boolean = infectionState == Susceptible

  def isAsymptomatic: Boolean = infectionState == Asymptomatic

  def isPresymptomatic: Boolean = infectionState == Presymptomatic

  def isMildlyInfected:Boolean = infectionState == MildlyInfected

  def isSeverelyInfected:Boolean = infectionState == SeverelyInfected

  def isHospitalized:Boolean = infectionState == Hospitalized

  def isRecovered: Boolean = infectionState == Recovered

  def isDead: Boolean = infectionState == Dead

  def isBeingTested:Boolean = beingTested == 1 || beingTested == 2

  def isNotTested:Boolean = beingTested == 0

  def isAwaitingResult:Boolean = beingTested == 1

  def isSymptomatic: Boolean = infectionState == MildlyInfected || infectionState == SeverelyInfected

  def isQuarantined:Boolean = beingTested == 2

  def isEligibleForTargetTest:Boolean = isEligibleForTargetedTesting

//  def isEligibleForTestingAgain(context: Context):Boolean = (context.getCurrentStep/Disease.numberOfTicksInADay) - lastTestDay >= Disease.daysAfterWhichEligibleForTestingAgain

  def isDelayPeriodOver(context: Context):Boolean = ((context.getCurrentStep*Disease.dt).toInt) - lastTestDay >= Disease.testDelay

  def decodeNode(classType: String, node: GraphNode): Node = {
    classType match {
      case "House" => node.as[House]
      case "Office" => node.as[Office]
      case "Hospital" => node.as[Hospital]
      case "Neighbourhood" => node.as[Neighbourhood]
    }
  }


  addBehaviour(incrementInfectionDay)
  addBehaviour(incrementNumberOfDaysInSI)
  addBehaviour(checkCurrentLocation)
  addBehaviour(checkEligibilityForTargetedSusceptiblePeople)

  addBehaviour(checkEligibilityForTargetedTesting)
  addBehaviour(checkEligibilityForRandomTesting)
  addBehaviour(declarationOfResults_checkForContacts)
  addBehaviour(quarantinePeriodOver)
  addBehaviour(contactIsolationPeriodOver)
  addBehaviour(countParams)
  //addBehaviour(printStuff)




  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[Hospital]("WORKS_IN/ADMITTED")
  addRelation[Cemetery]("BURIED_IN")
  addRelation[Neighbourhood]("LIVES_IN")

}
