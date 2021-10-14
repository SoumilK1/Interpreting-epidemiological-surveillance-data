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
import epi_project.testing.Disease
import epi_project.testing.DiseaseState._
import epi_project.testing.InfectionStatus._
import com.typesafe.scalalogging.LazyLogging

import java.util.Date

object Main extends LazyLogging {
  private val initialInfectedFraction = 0.001

  private val myTick: ScheduleUnit = new ScheduleUnit(1)
  private val myDay: ScheduleUnit = new ScheduleUnit(myTick * 2)

  def main(args: Array[String]): Unit = {
    var beforeCount = 1
    val simulation = Simulation()

    simulation.ingestData(implicit context => {
      ingestCSVData("D:\\Soumil\\Project\\COVID-19_epi_project\\src\\main\\dummy10k_hospitals.csv", csvDataExtractor)
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
//      registerState[DeadState]


      val currentTime = new Date().getTime

      SimulationListenerRegistry.register(
        new CsvOutputGenerator("src/main/" + "test_DTR_0.1%" + ".csv", new SEIROutputSpec(context))
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

    val studentSchedule = (myDay, myTick)
      .add[House](0, 0)
      .add[School](1, 1)

//    val quarantinedSchedule = (myDay,myTick)
//      .add[House](0, 2)

    val hospitalizedSchedule = (myDay,myTick)
      .add[Hospital](0,1)

    //to-do - add hospital Ids for hospitalized people


    val healthCareWorkerSchedule = (myDay,myTick)
      .add[House](0,0)
      .add[Hospital](1,1)

    registerSchedules(
      //(quarantinedSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isPresymptomatic, 1),
      (hospitalizedSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].isHospitalized,1),
      (employeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age >= 25 && agent.asInstanceOf[Person].essentialWorker == 0, 2),
      (studentSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age < 25, 3),
      (healthCareWorkerSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].essentialWorker == 1,4)
    )
  }

  private def roundToAgeRange(age: Int): Int = {
    (age / 10) * 10 + 9
  }

  private def csvDataExtractor(map: Map[String, String])(implicit context: Context): GraphData = {

    val citizenId = map("Agent_ID").toLong
    val age = map("Age").toInt
    val initialInfectionState = if (biasedCoinToss(initialInfectedFraction)) "Asymptomatic" else "Susceptible"


    val homeId = map("HID").toLong
    val schoolId = map("school_id").toLong
    val officeId = map("WorkPlaceID").toLong
    val hospitalId = map("Hospital ID").toLong

    val essentialWorker = map("essential_worker").toInt
//    val betaMultiplier : Double =
//      Disease.ageStratifiedBetaMultiplier.getOrElse(roundToAgeRange(age),Disease.ageStratifiedBetaMultiplier(99))

    val citizen: Person = Person(
      citizenId,
      age,
      InfectionStatus.withName(initialInfectionState),
      0,
      essentialWorker
    )


    if (initialInfectionState == "Susceptible"){
      citizen.setInitialState(SusceptibleState())
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

    if (age >= 25 && essentialWorker == 0) {
      val office = Office(officeId)
      val worksAt = Relation[Person, Office](citizenId, "WORKS_AT", officeId)
      val employerOf = Relation[Office, Person](officeId, "EMPLOYER_OF", citizenId)

      graphData.addNode(officeId, office)
      graphData.addRelations(worksAt, employerOf)
    } else {
      val school = School(schoolId)
      val studiesAt = Relation[Person, School](citizenId, "STUDIES_AT", schoolId)
      val studentOf = Relation[School, Person](schoolId, "TEACHES", citizenId)

      graphData.addNode(schoolId, school)
      graphData.addRelations(studiesAt, studentOf)
    }


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
    val ActivationCondition = (context:Context) => getRecoveredCount(context) > 2000
    val FirstTimeExecution = (context:Context) => TestingStartedAt = context.getCurrentStep
    val DeactivationCondition = (context:Context) => context.getCurrentStep == 400

    // TODO: change Deactivation condition to infected count, as more realistic

    val perTickAction = (context:Context) => {

      val populationIterable: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person", "isScheduledForTesting" equ true)

      populationIterable.foreach(node => {
        val person = node.as[Person]

        //println("Testing happens")
        person.updateParam("lastTestDay", context.getCurrentStep/Disease.numberOfTicksInADay)
        person.updateParam("beingTested",1)
        person.updateParam("isScheduledForTesting",false)

        if (biasedCoinToss(Disease.testSensitivity)){
          person.updateParam("lastTestResult","p")
        }
        else {
          person.updateParam("lastTestResult","n")
        }

      })
      //TO-DO: Increment tests here
      //println(Disease.numberOfTestsDoneAtEachTick)

      Disease.numberOfTestsDoneAtEachTick = 0
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

