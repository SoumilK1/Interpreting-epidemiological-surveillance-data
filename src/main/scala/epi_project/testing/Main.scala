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

  var testing_begins_at:Double = 0.2
  val total_population = 10000


  var filename = "dummy"
  println("before", Disease.numberOfDailyTests,Disease.RATTestSensitivity,Disease.RATTestFraction,
    Disease.RTPCRTestSensitivity,Disease.RTPCRTestFraction)

  def main(args: Array[String]): Unit = {

    val d = DataGeneratorForTestingPaper
    d.main("ResidentialArea10k")
    System.exit(0)

    testing_begins_at = args(0).toDouble
    Disease.numberOfDailyTests = args(1).toInt
    Disease.RATTestSensitivity = args(2).toDouble
    Disease.RATTestFraction = args(3).toDouble
    Disease.RTPCRTestSensitivity = args(4).toDouble
    Disease.RTPCRTestFraction = args(5).toDouble
    Disease.DoesContactTracingHappen = args(6)

    filename = args(7)

    println("after", Disease.numberOfDailyTests,Disease.RATTestSensitivity,Disease.RATTestFraction,
      Disease.RTPCRTestSensitivity,Disease.RTPCRTestFraction)

    var beforeCount = 1
    val simulation = Simulation()

    simulation.ingestData(implicit context => {
      ingestCSVData("inputcsv/"+"dummy10k_paper.csv", csvDataExtractor)
      logger.debug("Ingestion done")
    })

    simulation.defineSimulation(implicit context => {

      testing
      create12HourSchedules()

      registerAction(
        StopSimulation,
        (c: Context) => {
          c.getCurrentStep == 400
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



      val currentTime = new Date().getTime

      SimulationListenerRegistry.register(
        new CsvOutputGenerator("csv/" + "testing_begins_at_" + testing_begins_at +
          "_DTR_" + Disease.numberOfDailyTests + "_RATSen_" + Disease.RATTestSensitivity + "_RATFrac_" + Disease.RATTestFraction +
          "_RTPCRSen_" + Disease.RTPCRTestSensitivity + "_RTPCRFrac_" + Disease.RTPCRTestFraction + "_ContactTracingHappen_"
          + Disease.DoesContactTracingHappen + filename +
          ".csv", new SEIROutputSpec(context))
      )
    })



    simulation.onCompleteSimulation { implicit context =>
      printStats(beforeCount)
      teardown()
    }

    val startTime = System.currentTimeMillis()
    simulation.run()
    val endTime = System.currentTimeMillis()
    logger.info("Total time: {} s", (endTime - startTime) / 1000)
  }

  private def create12HourSchedules()(implicit context: Context): Unit = {
    val employeeSchedule = (myDay, myTick)
      .add[House](0, 0)
      .add[Office](1, 1)

//    val studentSchedule = (myDay, myTick)
//      .add[House](0, 0)
//      .add[School](1, 1)


    val hospitalizedSchedule = (myDay,myTick)
      .add[Hospital](0,1)


    val healthCareWorkerSchedule = (myDay,myTick)
      .add[House](0,0)
      .add[Hospital](1,1)

    registerSchedules(
      (hospitalizedSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].isHospitalized,1),
      (employeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].essentialWorker == 0, 1),
      //(studentSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age < 25 && agent.asInstanceOf[Person].essentialWorker == 0, 3),
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
    //val schoolId = map("school_id").toLong
    val officeId = map("WorkPlaceID").toLong
    val hospitalId = map("HospitalID").toLong

    val essentialWorker = map("essential_worker").toInt

    val citizen: Person = Person(
      citizenId,
      homeId,
      officeId,
      age,
      InfectionStatus.withName(initialInfectionState),
      0,
      essentialWorker
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

//    if (age < 25 && essentialWorker == 0){
//
//      val school = School(schoolId)
//      val studiesAt = Relation[Person, School](citizenId, "STUDIES_AT", schoolId)
//      val studentOf = Relation[School, Person](schoolId, "TEACHES", citizenId)
//
//      graphData.addNode(schoolId, school)
//      graphData.addRelations(studiesAt, studentOf)
//    }


    val hospital = Hospital(hospitalId)
    val worksIn = Relation[Person,Hospital](citizenId,"WORKS_IN/ADMITTED",hospitalId)
    val employs = Relation[Hospital,Person](hospitalId,"EMPLOYS/PROVIDES_CARE",citizenId)

    graphData.addNode(hospitalId,hospital)
    graphData.addRelations(worksIn,employs)


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

      val populationIterableForTargetedTestingAndContacts: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
        ("isEligibleForTargetedTesting" equ true) and ("isAContact" equ true))

      populationIterableForTargetedTestingAndContacts.foreach(node => {
        val SymptomaticContact = node.as[Person]

        //println(Disease.RTPCRTestFraction*Disease.numberOfDailyTests, Disease.numberOfRTPCRTestsAvailable)
        if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.dt* Disease.RTPCRTestFraction * Disease.numberOfDailyTests){
          SymptomaticContact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
          SymptomaticContact.updateParam("beingTested",1)
          SymptomaticContact.updateParam("testCategory",1)
          SymptomaticContact.updateParam("isEligibleForTargetedTesting",false)
          SymptomaticContact.updateParam("isEligibleForRandomTesting",false)
          SymptomaticContact.updateParam("isAContact",false)

          Disease.tested_person_id = SymptomaticContact.id
          //            println("testHappens")
          if((!SymptomaticContact.isRecovered) && (biasedCoinToss(Disease.RTPCRTestSensitivity))){
            SymptomaticContact.updateParam("lastTestResult",true)
            Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
          }
          else{
            SymptomaticContact.updateParam("lastTestResult",false)
          }
          Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick+1
          Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1

          //            println(person.id,person.lastTestResult,person.beingTested)
        }

        //println(Disease.numberOfDailyTests,Disease.RATTestFraction,Disease.numberOfRATTestsAvailable)
        if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
          (Disease.numberOfRATTestsDoneAtEachTick< Disease.dt * Disease.RATTestFraction * Disease.numberOfDailyTests)&&
          (SymptomaticContact.beingTested == 0) && (SymptomaticContact.id != Disease.tested_person_id)) {
          SymptomaticContact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
          SymptomaticContact.updateParam("beingTested",1)
          SymptomaticContact.updateParam("testCategory",1)
          SymptomaticContact.updateParam("isEligibleForTargetedTesting",false)
          SymptomaticContact.updateParam("isEligibleForRandomTesting",false)
          SymptomaticContact.updateParam("isAContact",false)
          //            println("testHappens")
          if((!SymptomaticContact.isRecovered) && (biasedCoinToss(Disease.RATTestSensitivity))){
            SymptomaticContact.updateParam("lastTestResult",true)
            Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
          }
          else{
            SymptomaticContact.updateParam("lastTestResult",false)
          }
          Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
          Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
        }
      })


      val populationIterableForTargetedTesting: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
          ("isEligibleForTargetedTesting" equ true) and ("isAContact" equ false))

        populationIterableForTargetedTesting.foreach(node => {
          val person = node.as[Person]

          //println(Disease.RTPCRTestFraction*Disease.numberOfDailyTests, Disease.numberOfRTPCRTestsAvailable)
          if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.dt* Disease.RTPCRTestFraction * Disease.numberOfDailyTests){
            person.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            person.updateParam("beingTested",1)
            person.updateParam("testCategory",1)
            person.updateParam("isEligibleForTargetedTesting",false)
            person.updateParam("isEligibleForRandomTesting",false)
            person.updateParam("isAContact",false)

            Disease.tested_person_id = person.id
//            println("testHappens")
            if((!person.isRecovered) && (biasedCoinToss(Disease.RTPCRTestSensitivity))){
              person.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            }
            else{
              person.updateParam("lastTestResult",false)
            }
            Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1

//            println(person.id,person.lastTestResult,person.beingTested)
          }

          //println(Disease.numberOfDailyTests,Disease.RATTestFraction,Disease.numberOfRATTestsAvailable)
          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
            (Disease.numberOfRATTestsDoneAtEachTick< Disease.dt * Disease.RATTestFraction * Disease.numberOfDailyTests)&&
            (person.beingTested == 0) && (person.id != Disease.tested_person_id)) {
            person.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            person.updateParam("beingTested",1)
            person.updateParam("testCategory",1)
            person.updateParam("isEligibleForTargetedTesting",false)
            person.updateParam("isEligibleForRandomTesting",false)
            person.updateParam("isAContact",false)
//            println("testHappens")
            if((!person.isRecovered) && (biasedCoinToss(Disease.RATTestSensitivity))){
              person.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            }
            else{
              person.updateParam("lastTestResult",false)
            }
            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }
        })

        val populationIterableForContactTracing:Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
        ("isAContact" equ true) and ("isEligibleForTargetedTesting" equ false))

        populationIterableForContactTracing.foreach(node => {
          val contact = node.as[Person]

          if((Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests)&&
            (contact.beingTested == 0)){
            contact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            contact.updateParam("beingTested",1)
            contact.updateParam("testCategory",2)
            contact.updateParam("isEligibleForTargetedTesting",false)
            contact.updateParam("isEligibleForRandomTesting",false)
            contact.updateParam("isAContact",false)

            Disease.tested_person_id = contact.id

            if((!contact.isSusceptible) && (!contact.isRecovered) && biasedCoinToss(Disease.RTPCRTestSensitivity)){
              contact.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            }
            else{
              contact.updateParam("lastTestResult",false)
            }
            Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick + 1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }

          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
            (Disease.numberOfRATTestsDoneAtEachTick< Disease.dt * Disease.RATTestFraction * Disease.numberOfDailyTests)&&
            (contact.beingTested == 0) && (contact.id != Disease.tested_person_id)){
            contact.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            contact.updateParam("beingTested",1)
            contact.updateParam("testCategory",2)
            contact.updateParam("isEligibleForTargetedTesting",false)
            contact.updateParam("isEligibleForRandomTesting",false)
            contact.updateParam("isAContact",false)
            if((!contact.isSusceptible) && (!contact.isRecovered) && biasedCoinToss(Disease.RATTestSensitivity)){
              contact.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            }
            else{
              contact.updateParam("lastTestResult",false)
            }
            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }
        })


        val populationIterableForRandomTesting: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person",
          ("isEligibleForRandomTesting" equ true) and ("isAContact" equ false) and ("isEligibleForTargetedTesting" equ false))

        populationIterableForRandomTesting.foreach(node => {
          val randomPerson = node.as[Person]


          if(Disease.numberOfRTPCRTestsDoneAtEachTick < Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests&&
            (randomPerson.beingTested == 0)){
            randomPerson.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            randomPerson.updateParam("beingTested",1)
            randomPerson.updateParam("testCategory",3)
            randomPerson.updateParam("isEligibleForRandomTesting",false)
            randomPerson.updateParam("isEligibleForTargetedTesting",false)
            randomPerson.updateParam("isAContact",false)

            Disease.tested_person_id = randomPerson.id
//            println("testHappens")
            if((!randomPerson.isSusceptible) && (!randomPerson.isRecovered)&& biasedCoinToss(Disease.RTPCRTestSensitivity)){
              randomPerson.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            }
            else{
              randomPerson.updateParam("lastTestResult",false)
            }
            Disease.numberOfRTPCRTestsDoneAtEachTick = Disease.numberOfRTPCRTestsDoneAtEachTick + 1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }

          if((Disease.numberOfRTPCRTestsDoneAtEachTick >= Disease.dt * Disease.RTPCRTestFraction * Disease.numberOfDailyTests) &&
            (Disease.numberOfRATTestsDoneAtEachTick< Disease.dt * Disease.RATTestFraction * Disease.numberOfDailyTests)&&
            (randomPerson.beingTested == 0) && (randomPerson.id != Disease.tested_person_id)){
            randomPerson.updateParam("lastTestDay", (context.getCurrentStep*Disease.dt).toInt)
            randomPerson.updateParam("beingTested",1)
            randomPerson.updateParam("testCategory",3)
            randomPerson.updateParam("isEligibleForRandomTesting",false)
            randomPerson.updateParam("isEligibleForTargetedTesting",false)
            randomPerson.updateParam("isAContact",false)
//            println("testHappens")
            if((!randomPerson.isSusceptible) && (!randomPerson.isRecovered)&& biasedCoinToss(Disease.RATTestSensitivity)){
              randomPerson.updateParam("lastTestResult",true)
              Disease.numberOfPositiveTestsAtEachTick = Disease.numberOfPositiveTestsAtEachTick + 1
            }
            else{
              randomPerson.updateParam("lastTestResult",false)
            }
            Disease.numberOfRATTestsDoneAtEachTick = Disease.numberOfRATTestsDoneAtEachTick+1
            Disease.totalNumberOfTestsDone = Disease.totalNumberOfTestsDone + 1
          }

        })

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

