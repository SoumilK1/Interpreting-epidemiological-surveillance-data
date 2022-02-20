package epi_project.testing

import com.bharatsim.engine.ContextBuilder._
import com.bharatsim.engine._
import com.bharatsim.engine.actions.StopSimulation
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.dsl.SyntaxHelpers._
import com.bharatsim.engine.execution.Simulation
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.ingestion.{GraphData, Relation}
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.intervention.SingleInvocationIntervention
import com.bharatsim.engine.listeners.{CsvOutputGenerator, SimulationListenerRegistry}
import com.bharatsim.engine.models.Agent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import com.typesafe.scalalogging.LazyLogging
import epi_project.testing.DiseaseState._
import epi_project.testing.InfectionStatus._

import java.util.Date

object Main extends LazyLogging {
  private val initialInfectedFraction = 0.001

  private val myTick: ScheduleUnit = new ScheduleUnit(1)
  private val myDay: ScheduleUnit = new ScheduleUnit(myTick * 2)
8
  var testing_begins_at:Double = 0.05
  val total_population = 10000


  var filename = "dummy_new_testing_priority"
  println("before", Disease.numberOfDailyTests,Disease.RATTestSensitivity,Disease.RATTestFraction,
    Disease.RTPCRTestSensitivity,Disease.RTPCRTestFraction)

  def main(args: Array[String]): Unit = {
//
//    val d = DataGeneratorForTestingPaper
//    d.main("inputcsv/ResidentialArea10k")
//    System.exit(0)
    /**
     * Run the following code by uncommenting for getting a synthetic population.
     * Do not use .csv at the end of file name.
     *
      val d = DataGeneratorForTestingPaper
      d.main("inputcsv/ResidentialArea10k")
      System.exit(0)
     */


    /** ARGUMENTS
     * The following block of code contains all the arguments that one can use while running.
     * Default arguments can be found in the Disease Class
     *
     *
     */

//    testing_begins_at = args(0).toDouble
//    Disease.numberOfDailyTests = args(1).toInt
//    Disease.RATTestSensitivity = args(2).toDouble
//    Disease.RATTestFraction = args(3).toDouble
//    Disease.RTPCRTestSensitivity = args(4).toDouble
//    Disease.RTPCRTestFraction = args(5).toDouble
//    Disease.DoesContactTracingHappen = args(6)
//    filename = args(7)



    println("after", Disease.numberOfDailyTests,Disease.RATTestSensitivity,Disease.RATTestFraction,
      Disease.RTPCRTestSensitivity,Disease.RTPCRTestFraction)

    var beforeCount = 1
    val simulation = Simulation()

    simulation.ingestData(implicit context => {
      ingestCSVData("inputcsv/"+"ResidentialArea10k.csv", csvDataExtractor)
      logger.debug("Ingestion done")
    })

    simulation.defineSimulation(implicit context => {

      testing
      create12HourSchedules()

      registerAction(
        StopSimulation,
        (c: Context) => {
          c.getCurrentStep == Disease.numberOfTicks
        }
      )

      beforeCount = getInfectedCount(context)

      registerAgent[Person]
      registerState[SusceptibleState]
      registerState[RecoveredState]
      registerState[AsymptomaticState]
      registerState[PresymptomaticState]
      registerState[MildlyInfectedState]
      registerState[SeverelyInfectedState]
      registerState[HospitalizedState]
      registerState[DeadState]



      val currentTime = new Date().getTime

      /**
       * Giving output in the SEIR manner
       *
       *
       *
       *
       */





      SimulationListenerRegistry.register(
        new CsvOutputGenerator("csv/" + "testing_begins_at_" + testing_begins_at +
          "_DTR_" + Disease.numberOfDailyTests + "_RATSen_" + Disease.RATTestSensitivity + "_RATFrac_" + Disease.RATTestFraction +
          "_RTPCRSen_" + Disease.RTPCRTestSensitivity + "_RTPCRFrac_" + Disease.RTPCRTestFraction + "_ContactTracingHappen_"
          + Disease.DoesContactTracingHappen + filename +
          ".csv", new SEIROutputSpec(context))

      )

    })


    /**
     *
     * GIVING OUTPUT IN THE NEW WAY(csv type 1)

     Columns
     =======


     Person ID(only if tested)
     Test Result
     Final infection status

     */



    simulation.onCompleteSimulation { implicit context =>
      val outputGenerator = new CsvOutputGenerator("EPID_csv/"+"EPID_output_testing_begins_at_" + testing_begins_at +
        "_DTR_" + Disease.numberOfDailyTests + "_RATSen_" + Disease.RATTestSensitivity + "_RATFrac_" + Disease.RATTestFraction +
        "_RTPCRSen_" + Disease.RTPCRTestSensitivity + "_RTPCRFrac_" + Disease.RTPCRTestFraction + "_ContactTracingHappen_"
        + Disease.DoesContactTracingHappen + filename +".csv", new EPIDCSVOutput("Person", context))
      outputGenerator.onSimulationStart(context)
      outputGenerator.onStepStart(context)
      outputGenerator.onSimulationEnd(context)

      printStats(beforeCount)
      teardown()
    }

    val startTime = System.currentTimeMillis()
    simulation.run()
    val endTime = System.currentTimeMillis()
    logger.info("Total time: {} s", (endTime - startTime) / 1000)
  }



  /**
   * Creating Schedules of Agents
   *
   *
   *
   *
   * */


  private def create12HourSchedules()(implicit context: Context): Unit = {
    val employeeSchedule = (myDay, myTick)
      .add[House](0, 0)
      .add[Office](1, 1)
//
//    val newEmployeeSchedule = (myDay,myTick)
//      .add[House](0,2)
//      .add[Office](3,4)
//      .add[Neighbourhood](5,5)


    val contactSchedule = (myDay,myTick)
      .add[House](0,1)

    val deathSchedule = (myDay,myTick)
      .add[Cemetery](0,1)

    val hospitalizedSchedule = (myDay,myTick)
      .add[Hospital](0,1)

    val isolationSchedule = (myDay,myTick)
      .add[House](0,1)

    val healthCareWorkerSchedule = (myDay,myTick)
      .add[House](0,0)
      .add[Hospital](1,1)

    registerSchedules(
      (deathSchedule,(agent:Agent,_:Context)=> agent.asInstanceOf[Person].isDead,1),
      (hospitalizedSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].isHospitalized,1),
      (contactSchedule,(agent:Agent,_:Context)=> agent.asInstanceOf[Person].isAContact==3,1),
      (isolationSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].beingTested == 3,1),
      //TODO: Rename the contactSchedule
      //TODO: Currently we are setting is a contact to false after getting(this is problematic if we have to test later)

      (employeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].essentialWorker == 0, 2),
      (healthCareWorkerSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].essentialWorker == 1,2)
    )
  }

  private def roundToAgeRange(age: Int): Int = {
    (age / 10) * 10 + 9
  }

  private def csvDataExtractor(map: Map[String, String])(implicit context: Context): GraphData = {

    val citizenId = map("Agent_ID").toLong
    val age = map("Age").toInt
    val initialInfectionState = if (biasedCoinToss(initialInfectedFraction)) "Asymptomatic" else "Susceptible"


    val homeId = map("HouseID").toLong

    val officeId = map("WorkPlaceID").toLong
    val hospitalId = map("HospitalID").toLong

    val neighbourhoodId = map("Neighbourhood_ID").toLong

    val essentialWorker = map("essential_worker").toInt
    val cemeteryId = map("CemeteryID").toLong

    val muMultiplier :Double = Disease.ageStratifiedMuMultiplier.getOrElse(roundToAgeRange(age), Disease.ageStratifiedMuMultiplier(99))

    val sigmaMultiplier:Double = Disease.ageStratifiedSigmaMultiplier.getOrElse(roundToAgeRange(age),Disease.ageStratifiedSigmaMultiplier(99))


    val citizen: Person = Person(
      citizenId,
      homeId,
      officeId,
      neighbourhoodId,
      age,
      sigmaMultiplier,
      muMultiplier,
      InfectionStatus.withName(initialInfectionState),
      0,
      essentialWorker,
      cemeteryId
    )


    if (initialInfectionState == "Susceptible"){
      citizen.setInitialState(SusceptibleState(toBeAsymptomatic = biasedCoinToss(Disease.gamma)))
    }
    else if (initialInfectionState=="Asymptomatic"){
      citizen.setInitialState(AsymptomaticState())
    }

    val home = House(homeId)
    val staysAt = Relation[Person, House](citizenId, "STAYS_AT", homeId)
    val memberOf = Relation[House, Person](homeId, "HOUSES", citizenId)


    val graphData = GraphData()
    graphData.addNode(citizenId, citizen)
    graphData.addNode(homeId, home)
    graphData.addRelations(staysAt, memberOf)



    if (essentialWorker == 0) {
      val office = Office(officeId)
      val worksAt = Relation[Person, Office](citizenId, "WORKS_AT", officeId)
      val employerOf = Relation[Office, Person](officeId, "EMPLOYER_OF", citizenId)

      graphData.addNode(officeId, office)
      graphData.addRelations(worksAt, employerOf)
    }




    val hospital = Hospital(hospitalId)
    val worksIn = Relation[Person,Hospital](citizenId,"WORKS_IN/ADMITTED",hospitalId)
    val employs = Relation[Hospital,Person](hospitalId,"EMPLOYS/PROVIDES_CARE",citizenId)

    graphData.addNode(hospitalId,hospital)
    graphData.addRelations(worksIn,employs)

    val cemetery =Cemetery(cemeteryId)
    val restsIn = Relation[Person,Cemetery](citizenId,"BURIED_IN",cemeteryId)
    val restingPlace = Relation[Cemetery,Person](cemeteryId,"RESTING_PLACE_OF",citizenId)
    graphData.addNode(cemeteryId,cemetery)
    graphData.addRelations(restsIn,restingPlace)

    val neighbourhood = Neighbourhood(neighbourhoodId)
    val livesIn = Relation[Person,Neighbourhood](citizenId,"LIVES_IN",neighbourhoodId)
    val isNeighbourhoodOf = Relation[Neighbourhood,Person](neighbourhoodId,"IS_NEIGHBOURHOOD_OF",citizenId)

    graphData.addNode(neighbourhoodId,neighbourhood)
    graphData.addRelations(livesIn,isNeighbourhoodOf)

    graphData
  }


  private def testing(implicit context: Context):Unit = {
    var TestingStartedAt = 0
    val InterventionName = "get_tested"

    val ActivationCondition = (context:Context) => getRecoveredCount(context) >= testing_begins_at*total_population

    val FirstTimeExecution = (context:Context) => TestingStartedAt = context.getCurrentStep
    val DeactivationCondition = (context:Context) => context.getCurrentStep == 400

   

    val perTickAction = (context:Context) => {
//      if(context.getCurrentStep % Disease.numberOfTicksInADay==0){

        Disease.numberOfRTPCRTestsDoneAtEachTick = 0
        Disease.numberOfRATTestsDoneAtEachTick = 0
        Disease.numberOfPositiveTestsAtEachTick = 0

      val populationIterableForTesting: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
        ("testCategory" equ 1) or  ("testCategory" equ 2) or ("testCategory" equ 3))

      populationIterableForTesting.foreach(node => {
        val TestedPerson = node.as[Person]
        TestedPerson.updateParam("testCategory", 0)
      })


      /**
       * TESTING FUNCTION FOR HIGH RISK CONTACTS
       *
       *
       *
       *
       *
       */
      val HighRiskContacts: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
        ("isAContact" equ 1))

      HighRiskContacts.foreach(node => {
        val HighRiskContact = node.as[Person]

        /**
         * RT-PCR Testing for High Risk Contacts
         *
         *
         *
         *
         */

        //println(Disease.RTPCRTestFraction*Disease.numberOfDailyTests, Disease.numberOfRTPCRTestsAvailable)
        if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.dt* Disease.RTPCRTestFraction * Disease.numberOfDailyTests){
          HighRiskContact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
          HighRiskContact.updateParam("beingTested",1)
          HighRiskContact.updateParam("testCategory",2)
          HighRiskContact.updateParam("isEligibleForTargetedTesting",false)
          HighRiskContact.updateParam("isEligibleForRandomTesting",false)
          HighRiskContact.updateParam("isAContact",0)


          Disease.tested_person_id = HighRiskContact.id

          if((!HighRiskContact.isSusceptible)&&(!HighRiskContact.isRecovered)&&(!HighRiskContact.isDead) && (biasedCoinToss(Disease.RTPCRTestSensitivity)) && (biasedCoinToss(Disease.probabilityOfHavingCOVID))){
            HighRiskContact.updateParam("lastTestResult",true)
            Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1

          }
          else{
            HighRiskContact.updateParam("lastTestResult",false)
          }
          Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick+1
          Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1

        }

        /**
         * RAT Testing for High Risk Contacts
         *
         *
         *
         *
         */
        if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
          (Disease.numberOfRATTestsDoneAtEachTick< Disease.dt * Disease.RATTestFraction * Disease.numberOfDailyTests)&&
          (HighRiskContact.beingTested != 1)  && (HighRiskContact.id != Disease.tested_person_id)) {
          HighRiskContact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
          HighRiskContact.updateParam("beingTested",1)
          HighRiskContact.updateParam("testCategory",2)
          HighRiskContact.updateParam("isEligibleForTargetedTesting",false)
          HighRiskContact.updateParam("isEligibleForRandomTesting",false)
          HighRiskContact.updateParam("isAContact",0)
          //            println("testHappens")
          if((!HighRiskContact.isSusceptible)&&(!HighRiskContact.isRecovered) &&(!HighRiskContact.isDead) && (biasedCoinToss(Disease.RATTestSensitivity)) && (biasedCoinToss(Disease.probabilityOfHavingCOVID))){
            HighRiskContact.updateParam("lastTestResult",true)
            Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
          }
          else{
            HighRiskContact.updateParam("lastTestResult",false)
          }
          Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
          Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
        }
      })

      /**
       * TESTING FUNCTION FOR SELF-REPORTED SYMPTOMATICS AND LOW RISK SYMPTOMATIC CONTACTS
       *
       *
       *
       *
       */


      val SelfReportedSymptomaticAndLowRiskSymptomaticContacts: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
          ("isEligibleForTargetedTesting" equ true) or (("isAContact" equ 2)))

        SelfReportedSymptomaticAndLowRiskSymptomaticContacts.foreach(node => {
          val SelfRepLowRiskSymptomatic = node.as[Person]


          /**
           * RT-PCR Tests for Self Reported Symptomcatic and Symptomatic Low Risk Contacts
           *
           *
           *
           *
           */
          if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.dt* Disease.RTPCRTestFraction * Disease.numberOfDailyTests){
            SelfRepLowRiskSymptomatic.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            SelfRepLowRiskSymptomatic.updateParam("beingTested",1)
            if(SelfRepLowRiskSymptomatic.isEligibleForTargetedTesting){
              SelfRepLowRiskSymptomatic.updateParam("testCategory",1)
            }
            if(SelfRepLowRiskSymptomatic.isAContact==2){
              SelfRepLowRiskSymptomatic.updateParam("testCategory",2)

            }
            SelfRepLowRiskSymptomatic.updateParam("isEligibleForTargetedTesting",false)
            SelfRepLowRiskSymptomatic.updateParam("isEligibleForRandomTesting",false)
            SelfRepLowRiskSymptomatic.updateParam("isAContact",0)

            Disease.tested_person_id = SelfRepLowRiskSymptomatic.id

            if((!SelfRepLowRiskSymptomatic.isRecovered) && (!SelfRepLowRiskSymptomatic.isDead)&&(biasedCoinToss(Disease.RTPCRTestSensitivity)) && (biasedCoinToss(Disease.probabilityOfHavingCOVID))){
              SelfRepLowRiskSymptomatic.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
            }
            else{
              SelfRepLowRiskSymptomatic.updateParam("lastTestResult",false)
            }
            Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1


          }


          /**
           * RAT Testing For Self Reported Symptomatics and Low Risk Symptomatics
           *
           *
           *
           */



          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
            (Disease.numberOfRATTestsDoneAtEachTick< Disease.dt * Disease.RATTestFraction * Disease.numberOfDailyTests)&&
            (SelfRepLowRiskSymptomatic.beingTested != 1) && (SelfRepLowRiskSymptomatic.id != Disease.tested_person_id)) {
            SelfRepLowRiskSymptomatic.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            SelfRepLowRiskSymptomatic.updateParam("beingTested",1)

            if(SelfRepLowRiskSymptomatic.isEligibleForTargetedTesting){
              SelfRepLowRiskSymptomatic.updateParam("testCategory",1)
            }
            if(SelfRepLowRiskSymptomatic.isAContact==2){
              SelfRepLowRiskSymptomatic.updateParam("testCategory",2)

            }
            SelfRepLowRiskSymptomatic.updateParam("isEligibleForTargetedTesting",false)
            SelfRepLowRiskSymptomatic.updateParam("isEligibleForRandomTesting",false)
            SelfRepLowRiskSymptomatic.updateParam("isAContact",0)
//            println("testHappens")
            if((!SelfRepLowRiskSymptomatic.isRecovered) &&(!SelfRepLowRiskSymptomatic.isDead)&& (biasedCoinToss(Disease.RATTestSensitivity)) && (biasedCoinToss(Disease.probabilityOfHavingCOVID))){
              SelfRepLowRiskSymptomatic.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
            }
            else{
              SelfRepLowRiskSymptomatic.updateParam("lastTestResult",false)
            }
            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }
        })

      /**
       *
       *
       * Random Testing Code is commented as below, uncomment it to see the code.
       *
       *
       */



      //        val populationIterableForRandomTesting: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
//          ("isEligibleForRandomTesting" equ true) and ("isAContact" equ false) and ("isEligibleForTargetedTesting" equ false))
//
//        populationIterableForRandomTesting.foreach(node => {
//          val randomPerson = node.as[Person]
//
//
//          if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests&&
//            (randomPerson.beingTested == 0)){
//            randomPerson.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
//            randomPerson.updateParam("beingTested",1)
//            randomPerson.updateParam("testCategory",3)
//            randomPerson.updateParam("isEligibleForRandomTesting",false)
//            randomPerson.updateParam("isEligibleForTargetedTesting",false)
//            randomPerson.updateParam("isAContact",false)
//
//            Disease.tested_person_id = randomPerson.id
////            println("testHappens")
//            if((!randomPerson.isSusceptible) && (!randomPerson.isRecovered)&& biasedCoinToss(Disease.RTPCRTestSensitivity)){
//              randomPerson.updateParam("lastTestResult",true)
//              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
//            }
//            else{
//              randomPerson.updateParam("lastTestResult",false)
//            }
//            Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick + 1
//            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
//          }
//
//          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
//            (Disease.numberOfRATTestsDoneAtEachTick< Disease.dt * Disease.RATTestFraction * Disease.numberOfDailyTests)&&
//            (randomPerson.beingTested == 0) && (randomPerson.id != Disease.tested_person_id)){
//            randomPerson.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
//            randomPerson.updateParam("beingTested",1)
//            randomPerson.updateParam("testCategory",3)
//            randomPerson.updateParam("isEligibleForRandomTesting",false)
//            randomPerson.updateParam("isEligibleForTargetedTesting",false)
//            randomPerson.updateParam("isAContact",false)
////            println("testHappens")
//            if((!randomPerson.isSusceptible) && (!randomPerson.isRecovered)&& biasedCoinToss(Disease.RATTestSensitivity)){
//              randomPerson.updateParam("lastTestResult",true)
//              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
//            }
//            else{
//              randomPerson.updateParam("lastTestResult",false)
//            }
//            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
//            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
//          }
//
//        })

//      if(context.getCurrentStep%Disease.numberOfTicksInADay==1){
//        Disease.numberOfRTPCRTestsDoneOnEachDay = 0
//        Disease.numberOfRATTestsDoneAtEachTick = 0
//      }
    }

    val Testing = SingleInvocationIntervention(InterventionName,ActivationCondition,DeactivationCondition,FirstTimeExecution,perTickAction)

    val QuarantinedSchedule = (myDay,myTick).add[House](0,1)

    registerIntervention(Testing)

    registerSchedules(
      (QuarantinedSchedule,(agent:Agent, _:Context) => {
        val Intervention = context.activeInterventionNames.contains(InterventionName)

        Intervention && agent.asInstanceOf[Person].isQuarantined

      },
      1
      ))
  }

  private def printStats(beforeCount: Int)(implicit context: Context): Unit = {
    val afterCountSusceptible = getSusceptibleCount(context)
    val afterCountInfected = getInfectedCount(context)
    val afterCountRecovered = getRecoveredCount(context)

    logger.info("Infected before: {}", beforeCount)
    logger.info("Infected after: {}", afterCountInfected)
    logger.info("Recovered: {}", afterCountRecovered)
    logger.info("Susceptible: {}", afterCountSusceptible)
  }

  private def getSusceptibleCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Susceptible)
  }

  private def getInfectedCount(context: Context): Int = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Asymptomatic)
  }

  private def getRecoveredCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Recovered)
  }


}

