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

  private var initialInfectedFraction = 0.005


  private val myTick: ScheduleUnit = new ScheduleUnit(1)
  private val myDay: ScheduleUnit = new ScheduleUnit(myTick * 6)

  var testing_begins_at:Double = 0.001
  val total_population = 100000

  var filename = "dummy_run_2Meow-newhggh"

  println("before", Disease.numberOfDailyTests,Disease.RATTestSensitivity,Disease.RATTestFraction,
    Disease.RTPCRTestSensitivity,Disease.RTPCRTestFraction)

  def main(args: Array[String]): Unit = {

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

    initialInfectedFraction = args(0).toDouble
    Disease.fractionOfPeopleSelfReportedToStartTesting = args(1).toDouble
    Disease.numberOfDailyTests = args(2).toInt
    Disease.RATTestSensitivity = args(3).toDouble
    Disease.RATTestFraction = args(4).toDouble
    Disease.RTPCRTestSensitivity = args(5).toDouble
    Disease.RTPCRTestFraction = args(6).toDouble
    Disease.DoesContactTracingHappen = args(7)
    Disease.DoesRandomTestingHappen = args(8)
    Disease.colleagueFraction = args(9).toDouble
    Disease.neighbourFraction = args(10).toDouble
    Disease.basalFraction = args(11).toDouble
    Disease.EPID_required = args(12)
    filename = args(13)
    Disease.lambdaS = args(14).toDouble
////



    println("after", Disease.numberOfDailyTests,Disease.RATTestSensitivity,Disease.RATTestFraction,
      Disease.RTPCRTestSensitivity,Disease.RTPCRTestFraction)

    var beforeCount = 1
    val simulation = Simulation()

    simulation.ingestData(implicit context => {
      ingestCSVData("inputcsv/"+"100k.csv", csvDataExtractor)
      logger.debug("Ingestion done")
    })

    simulation.defineSimulation(implicit context => {

      testing
      create12HourSchedules()

      registerAction(
        StopSimulation,
        (c: Context) => {
          c.getCurrentStep == 1200
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





//      SimulationListenerRegistry.register(
//        new CsvOutputGenerator("csv/" + "initialInfectedFraction" + initialInfectedFraction +
//          "_DTR_" + Disease.numberOfDailyTests + "_RATSen_" + Disease.RATTestSensitivity + "_RATFrac_" + Disease.RATTestFraction +
//          "_RTPCRSen_" + Disease.RTPCRTestSensitivity + "_RTPCRFrac_" + Disease.RTPCRTestFraction + "_ContactTracingHappen_"
//          + Disease.DoesContactTracingHappen +  "_RandomTesting_" + Disease.DoesRandomTestingHappen + "_beta_" + Disease.lambdaS + filename +
//          ".csv", new SEIROutputSpec(context))
//      )

      if (Disease.EPID_required == "y") {
        SimulationListenerRegistry.register(
          new CsvOutputGenerator("EPID_csv/" + "EPID_output_initialInfectedFraction" + initialInfectedFraction +
            "_DTR_" + Disease.numberOfDailyTests + "_RATSen_" + Disease.RATTestSensitivity + "_RATFrac_" + Disease.RATTestFraction +
            "_RTPCRSen_" + Disease.RTPCRTestSensitivity + "_RTPCRFrac_" + Disease.RTPCRTestFraction + "_ContactTracingHappen_"
            + Disease.DoesContactTracingHappen + "_RandomTesting_" + Disease.DoesRandomTestingHappen + "_beta_" + Disease.lambdaS + filename + ".csv", new EPIDCSVOutput("Person", context))
        )

      }


//          SimulationListenerRegistry.register(
//            new CsvOutputGenerator("CFR_csv/" + "CFR_output_initialInfectedFraction" + initialInfectedFraction +
//              "_DTR_" + Disease.numberOfDailyTests + "_RATSen_" + Disease.RATTestSensitivity + "_RATFrac_" + Disease.RATTestFraction +
//              "_RTPCRSen_" + Disease.RTPCRTestSensitivity + "_RTPCRFrac_" + Disease.RTPCRTestFraction + "_ContactTracingHappen_"
//              + Disease.DoesContactTracingHappen + "_RandomTesting_" + Disease.DoesRandomTestingHappen + "_beta_" + Disease.lambdaS + filename + ".csv", new CFROutput("Person", context))
//          )


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


    /**
     =ScheduleForEmployeeInNeighbourhood=

     */

    val employeeSchedule = (myDay, myTick)
      .add[House](0,2)
      .add[Office](3,4)
      .add[Neighbourhood](5,5)

    val healthCareWorkerSchedule = (myDay,myTick)
      .add[House](0,2)
      .add[Hospital](3,4)
      .add[Neighbourhood](5,5)

    val nonEmployeeSchedule = (myDay,myTick)
      .add[House](0,4)
      .add[Neighbourhood](5,5)


    /**
     * Schedule for people in Hospital/Isolated/Quarantined/Dead
     */


    val contactSchedule = (myDay,myTick)
      .add[House](0,5)

    val deathSchedule = (myDay,myTick)
      .add[Cemetery](0,5)

    val hospitalizedSchedule = (myDay,myTick)
      .add[Hospital](0,5)

    val isolationSchedule = (myDay,myTick)
      .add[House](0,5)



    registerSchedules(
      (deathSchedule,(agent:Agent,_:Context)=> agent.asInstanceOf[Person].isDead,1),
      (hospitalizedSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].isHospitalized,1),
      (contactSchedule,(agent:Agent,_:Context)=> agent.asInstanceOf[Person].isAContact==3,1),
      (isolationSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].beingTested == 3,1),
      //TODO: Rename the contactSchedule
      //TODO: Currently we are setting is a contact to false after getting(this is problematic if we have to test later)

      (employeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].essentialWorker == 0 &&
        agent.asInstanceOf[Person].age <= 60 && agent.asInstanceOf[Person].age >= 5 , 2),
      (nonEmployeeSchedule,(agent:Agent, _: Context) => agent.asInstanceOf[Person].essentialWorker == 0 &&
        (agent.asInstanceOf[Person].age <5 || agent.asInstanceOf[Person].age > 60),2),
      (healthCareWorkerSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].essentialWorker == 1
        && agent.asInstanceOf[Person].age >= 25 && agent.asInstanceOf[Person].age <=60,2),
      (nonEmployeeSchedule,(agent:Agent, _: Context) => agent.asInstanceOf[Person].essentialWorker == 1 &&
        (agent.asInstanceOf[Person].age < 25 || agent.asInstanceOf[Person].age > 60),2)
    )
  }

  private def roundToAgeRange(age: Int): Int = {
    (age / 5) * 5 + 5
  }

  private def roundToAge9(age:Int):Int = {
    (age/10)*10 + 9
  }

  private def csvDataExtractor(map: Map[String, String])(implicit context: Context): GraphData = {

    val citizenId = map("Agent_ID").toLong
    val age = map("Age").toInt
    val initialInfectionState = if (biasedCoinToss(initialInfectedFraction)) "Asymptomatic" else "Susceptible"

    val homeId = map("HouseID").toLong

    val officeId = map("WorkPlaceID").toLong
    val hospitalId = map("HospitalID").toLong

    val neighbourhoodId = map("Neighbourhood_ID").toLong

//    val initialInfectionState = if ((officeId <= 100) && (biasedCoinToss(0.5))) "Asymptomatic" else "Susceptible"

    val essentialWorker = map("essential_worker").toInt
    val cemeteryId = map("CemeteryID").toLong

    val deltaMultiplier:Double = Disease.ageStratifiedDeltaMultiplier.getOrElse(roundToAge9(age), Disease.ageStratifiedDeltaMultiplier(99))

    val muMultiplier :Double = Disease.ageStratifiedMuMultiplier.getOrElse(roundToAge9(age), Disease.ageStratifiedMuMultiplier(99))

    val gammaMultiplier :Double = Disease.ageStratifiedGammaMultiplier.getOrElse(roundToAge9(age),Disease.ageStratifiedGammaMultiplier(99))

    val sigmaMultiplier:Double = Disease.ageStratifiedSigmaMultiplier.getOrElse(roundToAgeRange(age),Disease.ageStratifiedSigmaMultiplier(100))

    val citizen: Person = Person(
      citizenId,
      homeId,
      officeId,
      neighbourhoodId,
      age,
      deltaMultiplier,
      sigmaMultiplier,
      muMultiplier,
      gammaMultiplier,
      InfectionStatus.withName(initialInfectionState),
      0,
      essentialWorker,
      cemeteryId,
    )


    if (initialInfectionState == "Susceptible"){
      citizen.setInitialState(SusceptibleState(toBeAsymptomatic = biasedCoinToss(Disease.gamma*gammaMultiplier)))
    }
    else if (initialInfectionState=="Asymptomatic"){
      citizen.setInitialState(AsymptomaticState())
//      citizen.updateParam("dayAtWhichPersonIsInfected",1)
      Disease.initialSeed = Disease.initialSeed + 1
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
    //val ActivationCondition = (context:Context) => getRecoveredCount(context) >= testing_begins_at*total_population
    val ActivationCondition = (context:Context) => Disease.numberOfPeopleSelfReported > Disease.numberOfPeopleSelfReportedToStartTesting

    val FirstTimeExecution = (context:Context) => {
      TestingStartedAt = context.getCurrentStep
      if (context.getCurrentStep % Disease.numberOfTicksInADay != 0) {
        Disease.testingStartedAt = (context.getCurrentStep / Disease.numberOfTicksInADay + 1) * (Disease.numberOfTicksInADay)
        Disease.IdentifiedDoublingTick = Disease.testingStartedAt + Disease.testDelay*Disease.numberOfTicksInADay
      }
      if (context.getCurrentStep % Disease.numberOfTicksInADay == 0){
        Disease.testingStartedAt = context.getCurrentStep
        Disease.IdentifiedDoublingTick = Disease.testingStartedAt + Disease.testDelay*Disease.numberOfTicksInADay
      }


    }
    val DeactivationCondition = (context:Context) => context.getCurrentStep == Disease.numberOfTicks

    val perTickAction = (context:Context) => {
//      if(context.getCurrentStep % Disease.numberOfTicksInADay==0){
      if(context.getCurrentStep%Disease.numberOfTicksInADay==0) {

        Disease.numberOfRTPCRTestsDoneAtEachTick = 0
        Disease.numberOfRATTestsDoneAtEachTick = 0
        Disease.numberOfPositiveTestsAtEachTick = 0
        Disease.numberOfUninfectedPeopleTested = 0.0

      }


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
//      val HighRiskContacts: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
//        ("isAContact" equ 1))
//
//      HighRiskContacts.foreach(node => {
//        val HighRiskContact = node.as[Person]

        /**
         * RT-PCR Testing for High Risk Contacts
         *
         *
         *
         *
         */

        //println(Disease.RTPCRTestFraction*Disease.numberOfDailyTests, Disease.numberOfRTPCRTestsAvailable)
//        if((context.getCurrentStep%Disease.numberOfTicksInADay==0)){
//          if((Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.RTPCRTestFraction * Disease.numberOfDailyTests)){
//            HighRiskContact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
//            HighRiskContact.updateParam("beingTested",1)
//            HighRiskContact.updateParam("testCategory",2)
//            HighRiskContact.updateParam("isEligibleForTargetedTesting",false)
//            HighRiskContact.updateParam("isEligibleForRandomTesting",false)
//            HighRiskContact.updateParam("isAContact",0)
//            HighRiskContact.updateParam("typeOfTestGiven", 1)
//
//
//            Disease.tested_person_id = HighRiskContact.id

//            if((!HighRiskContact.isSusceptible)&&(!HighRiskContact.isRecovered)&&(!HighRiskContact.isDead) && (biasedCoinToss(Disease.RTPCRTestSensitivity))){
//              HighRiskContact.updateParam("lastTestResult",true)
//              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
//              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
//            }
//            else{
//              HighRiskContact.updateParam("lastTestResult",false)
//              Disease.numberOfUninfectedPeopleTested = Disease.numberOfUninfectedPeopleTested + 1.0
//            }
//            Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick+1
//            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
//          }

          /**
           * RAT Testing for High Risk Contacts
           *
           *
           *
           *
           */
//          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
//            (Disease.numberOfRATTestsDoneAtEachTick< Disease.RATTestFraction * Disease.numberOfDailyTests)&&
//            (HighRiskContact.beingTested != 1)  && (HighRiskContact.id != Disease.tested_person_id)) {
//            HighRiskContact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
//            HighRiskContact.updateParam("beingTested",1)
//            HighRiskContact.updateParam("testCategory",2)
//            HighRiskContact.updateParam("isEligibleForTargetedTesting",false)
//            HighRiskContact.updateParam("isEligibleForRandomTesting",false)
//            HighRiskContact.updateParam("isAContact",0)
//            HighRiskContact.updateParam("typeOfTestGiven", 2)
//            //            println("testHappens")
//            if((!HighRiskContact.isSusceptible)&&(!HighRiskContact.isRecovered) &&(!HighRiskContact.isDead) && (biasedCoinToss(Disease.RATTestSensitivity))){
//              HighRiskContact.updateParam("lastTestResult",true)
//              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
//              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
//            }
//            else{
//              HighRiskContact.updateParam("lastTestResult",false)
//              Disease.numberOfUninfectedPeopleTested = Disease.numberOfUninfectedPeopleTested + 1.0
//            }
//            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
//            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
//          }
//      }})

      /**
       * TESTING FUNCTION FOR SELF-REPORTED SYMPTOMATICS AND LOW RISK SYMPTOMATIC CONTACTS
       *
       *
       *
       *
       */


      val EligibleforTargetedTest_CT: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
          ("isEligibleForTargetedTesting" equ true) or ("isAContact" equ 2) or ("isAContact" equ 1) )

        EligibleforTargetedTest_CT.foreach(node => {
          val PersonEligibleforSR_CT = node.as[Person]

          /**
           * RT-PCR Tests for Self Reported Symptomcatic and Symptomatic Low Risk Contacts
           *
           *
           *
           *
           */
          if(context.getCurrentStep%Disease.numberOfTicksInADay==0){
          if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.RTPCRTestFraction * Disease.numberOfDailyTests){
            PersonEligibleforSR_CT.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            PersonEligibleforSR_CT.updateParam("beingTested",1)
            if(PersonEligibleforSR_CT.isEligibleForTargetedTesting){
              PersonEligibleforSR_CT.updateParam("testCategory",1)
            }
            if(PersonEligibleforSR_CT.isAContact==2 || PersonEligibleforSR_CT.isAContact == 1){
              PersonEligibleforSR_CT.updateParam("testCategory",2)

            }
            PersonEligibleforSR_CT.updateParam("isEligibleForTargetedTesting",false)
            PersonEligibleforSR_CT.updateParam("isEligibleForRandomTesting",false)
            PersonEligibleforSR_CT.updateParam("isAContact",0)
            PersonEligibleforSR_CT.updateParam("typeOfTestGiven",1)

            Disease.tested_person_id = PersonEligibleforSR_CT.id

            if((!PersonEligibleforSR_CT.isSusceptible) && (!PersonEligibleforSR_CT.isRecovered) && (!PersonEligibleforSR_CT.isDead)&&(biasedCoinToss(Disease.RTPCRTestSensitivity))){
              PersonEligibleforSR_CT.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
            }
            else{
              PersonEligibleforSR_CT.updateParam("lastTestResult",false)
              Disease.numberOfUninfectedPeopleTested = Disease.numberOfUninfectedPeopleTested + 1.0
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



          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
            (Disease.numberOfRATTestsDoneAtEachTick< Disease.RATTestFraction * Disease.numberOfDailyTests)&&
            (PersonEligibleforSR_CT.beingTested != 1) && (PersonEligibleforSR_CT.id != Disease.tested_person_id)) {
            PersonEligibleforSR_CT.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            PersonEligibleforSR_CT.updateParam("beingTested",1)

            if(PersonEligibleforSR_CT.isEligibleForTargetedTesting){
              PersonEligibleforSR_CT.updateParam("testCategory",1)
            }
            if(PersonEligibleforSR_CT.isAContact==2 || PersonEligibleforSR_CT.isAContact == 1){
              PersonEligibleforSR_CT.updateParam("testCategory",2)

            }
            PersonEligibleforSR_CT.updateParam("isEligibleForTargetedTesting",false)
            PersonEligibleforSR_CT.updateParam("isEligibleForRandomTesting",false)
            PersonEligibleforSR_CT.updateParam("isAContact",0)
            PersonEligibleforSR_CT.updateParam("typeOfTestGiven",2)
//            println("testHappens")
            if((!PersonEligibleforSR_CT.isSusceptible) && (!PersonEligibleforSR_CT.isRecovered) &&(!PersonEligibleforSR_CT.isDead)&& (biasedCoinToss(Disease.RATTestSensitivity))){
              PersonEligibleforSR_CT.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
            }
            else{
              PersonEligibleforSR_CT.updateParam("lastTestResult",false)
              Disease.numberOfUninfectedPeopleTested = Disease.numberOfUninfectedPeopleTested + 1.0
            }
            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }
        }})

      /**
       *
       *
       * Random Testing Code is commented as below, uncomment it to see the code.
       *
       *
       */



      val populationIterableForRandomTesting: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
          ("isEligibleForRandomTesting" equ true) and ("isAContact" equ 0) and ("isEligibleForTargetedTesting" equ false))

        populationIterableForRandomTesting.foreach(node => {
          val randomPerson = node.as[Person]


          if(context.getCurrentStep%Disease.numberOfTicksInADay==0){
          if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.RTPCRTestFraction * Disease.numberOfDailyTests&&
            (randomPerson.beingTested == 0)){
            randomPerson.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            randomPerson.updateParam("beingTested",1)
            randomPerson.updateParam("testCategory",3)
            randomPerson.updateParam("isEligibleForRandomTesting",false)
            randomPerson.updateParam("isEligibleForTargetedTesting",false)
            randomPerson.updateParam("isAContact",0)
            randomPerson.updateParam("typeOfTestGiven",1)

            Disease.tested_person_id = randomPerson.id
//            println("testHappens")
            if((!randomPerson.isSusceptible) && (!randomPerson.isRecovered)&& biasedCoinToss(Disease.RTPCRTestSensitivity)){
              randomPerson.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
            }
            else{
              randomPerson.updateParam("lastTestResult",false)
              Disease.numberOfUninfectedPeopleTested = Disease.numberOfUninfectedPeopleTested + 1.0
            }
            Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick + 1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }

          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
            (Disease.numberOfRATTestsDoneAtEachTick< Disease.RATTestFraction * Disease.numberOfDailyTests)&&
            (randomPerson.beingTested == 0) && (randomPerson.id != Disease.tested_person_id)){
            randomPerson.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            randomPerson.updateParam("beingTested",1)
            randomPerson.updateParam("testCategory",3)
            randomPerson.updateParam("isEligibleForRandomTesting",false)
            randomPerson.updateParam("isEligibleForTargetedTesting",false)
            randomPerson.updateParam("isAContact",0)
            randomPerson.updateParam("typeOfTestGiven",2)
//            println("testHappens")
            if((!randomPerson.isSusceptible) && (!randomPerson.isRecovered)&& biasedCoinToss(Disease.RATTestSensitivity)){
              randomPerson.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
              Disease.totalNumberOfPositiveTests = Disease.totalNumberOfPositiveTests + 1
            }
            else{
              randomPerson.updateParam("lastTestResult",false)
              Disease.numberOfUninfectedPeopleTested = Disease.numberOfUninfectedPeopleTested + 1.0
            }
            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }

        }})

//      if(context.getCurrentStep%Disease.numberOfTicksInADay==1){
//        Disease.numberOfRTPCRTestsDoneAtEachTick = 0
//        Disease.numberOfRATTestsDoneAtEachTick = 0
//      }
    }

    val Testing = SingleInvocationIntervention(InterventionName,ActivationCondition,DeactivationCondition,FirstTimeExecution,perTickAction)

    val QuarantinedSchedule = (myDay,myTick).add[House](0,5)

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

