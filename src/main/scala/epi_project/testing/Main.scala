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
  private val initialInfectedFraction = 0.03

  private val myTick: ScheduleUnit = new ScheduleUnit(1)
  private val myDay: ScheduleUnit = new ScheduleUnit(myTick * 3)

  def main(args: Array[String]): Unit = {
    var beforeCount = 1
    val simulation = Simulation()

    simulation.ingestData(implicit context => {
      ingestCSVData("D:\\Soumil\\Project\\COVID-19_epi_project\\src\\main\\dummy10k.csv", csvDataExtractor)
      logger.debug("Ingestion done")
    })

    simulation.defineSimulation(implicit context => {

      //addLockdown
      //addLockdown_below60
      testing
      create12HourSchedules()

      registerAction(
        StopSimulation,
        (c: Context) => {
          c.getCurrentStep == 500
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
        new CsvOutputGenerator("src/main/" + "test1" + ".csv", new SEIROutputSpec(context))
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
      .add[House](2,2)

    val studentSchedule = (myDay, myTick)
      .add[House](0, 0)
      .add[School](1, 1)
      .add[House](2,2)

//    val quarantinedSchedule = (myDay,myTick)
//      .add[House](0, 2)

    val hospitalizedSchedule = (myDay,myTick)
      .add[Hospital](0,2)

    registerSchedules(
      //(quarantinedSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isPresymptomatic, 1),
      (hospitalizedSchedule,(agent:Agent,_:Context) => agent.asInstanceOf[Person].isHospitalized,1),
      (employeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age >= 25, 2),
      (studentSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age < 25, 3)
    )
  }

  private def roundToAgeRange(age: Int): Int = {
    (age / 10) * 10 + 9
  }

  private def csvDataExtractor(map: Map[String, String])(implicit context: Context): GraphData = {

    val citizenId = map("Agent_ID").toLong
    val age = map("Age").toInt
    val initialInfectionState = if (biasedCoinToss(initialInfectedFraction)) "Presymptomatic" else "Susceptible"


    val homeId = map("HHID").toLong
    val schoolId = map("school_id").toLong
    val officeId = map("WorkPlaceID").toLong

//    val betaMultiplier : Double =
//      Disease.ageStratifiedBetaMultiplier.getOrElse(roundToAgeRange(age),Disease.ageStratifiedBetaMultiplier(99))

    val citizen: Person = Person(
      citizenId,
      age,
      InfectionStatus.withName(initialInfectionState),
      0,
    )

    if (initialInfectionState == "Susceptible"){
      citizen.setInitialState(SusceptibleState())
    }
    else if (initialInfectionState=="Presymptomatic"){
      citizen.setInitialState(PresymptomaticState())
    }

    val home = House(homeId)
    val staysAt = Relation[Person, House](citizenId, "STAYS_AT", homeId)
    val memberOf = Relation[House, Person](homeId, "HOUSES", citizenId)

    val graphData = GraphData()
    graphData.addNode(citizenId, citizen)
    graphData.addNode(homeId, home)
    graphData.addRelations(staysAt, memberOf)

    if (age >= 25) {
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

    graphData
  }


  private def testing(implicit context: Context):Unit = {
    var TestingStartedAt = 0
    val InterventionName = "testing_symptoms"
    val ActivationCondition = (context:Context) => getRecoveredCount(context) >= 2000
    val FirstTimeExecution = (context:Context) => TestingStartedAt = context.getCurrentStep
    val DeactivationCondition = (context:Context) => getSusceptibleCount(context) < 1000

    // TODO: change Deactivation condition to infected count, as more realistic


    val Testing = SingleInvocationIntervention(InterventionName,ActivationCondition,DeactivationCondition,FirstTimeExecution,
      context => {

//        val populationIterable: Iterable[GraphNode] = context.graphProvider.fetchNodes("Person")
        //println(Disease.numberOfTestsDoneAtEachTick,getRecoveredCount(context))

        Disease.numberOfTestsDoneAtEachTick = 0
      }
    )


    val QuarantinedSchedule = (myDay,myTick).add[House](0,2)
    registerIntervention(Testing)

    registerSchedules(
      (QuarantinedSchedule,(agent:Agent, _:Context) => {
        val isQuarantined = context.activeInterventionNames.contains(InterventionName)
        isQuarantined && agent.asInstanceOf[Person].Tested
      },
      1
      ))
  }


//  private def addLockdown(implicit context: Context): Unit={
//    var LockdownActivatedAt = 0
//    val InterventionName = "lockdown_above60"
//    val ActivationCondition = (context:Context) => getInfectedCount(context) >= 1000
//    val FirstTimeExecution = (context:Context) => LockdownActivatedAt = context.getCurrentStep
//    val DeactivationCondition = (context:Context) => {
//      context.getCurrentStep == LockdownActivatedAt + 63
//    }
//    val lockdown_above60 = SingleInvocationIntervention(InterventionName,ActivationCondition,DeactivationCondition,FirstTimeExecution)
//    val LockdownSchedule = (myDay,myTick).add[House](0,2)
//
//    registerIntervention(lockdown_above60)
//    registerSchedules(
//      (LockdownSchedule,(agent: Agent, _: Context) => {
//        val isLockdown = context.activeInterventionNames.contains(InterventionName)
//        isLockdown
//      },
//      1
//    ))
//  }

  //private def addLockdown_below60(implicit context: Context): Unit = {
    //var LockdownActivatedAt = 0
    //val InterventionName = "lockdown_below60"
    //val FirstTimeExecution = (context:Context) => LockdownActivatedAt = context.getCurrentStep
    //val ActivationCondition = (context:Context) => getInfectedCount(context) >= 1000
    //val DeactivationCondition = (context:Context) => {
      //context.getCurrentStep == LockdownActivatedAt + 21
    //}

    //val lockdown_below60 = SingleInvocationIntervention(InterventionName,ActivationCondition,DeactivationCondition,FirstTimeExecution)
    //val LockdownSchedule = (myDay,myTick).add[House](0,2)

    //registerIntervention(lockdown_below60)
    //registerSchedules(
      //(LockdownSchedule,(agent: Agent, _: Context) => {
        //val isBelow60 = agent.asInstanceOf[Person].age < 60
        //val isLockdown = context.activeInterventionNames.contains(InterventionName)
        //isLockdown && isBelow60
      //},
        //1
      //))
  //}



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
    context.graphProvider.fetchCount("Person", "infectionState" equ Presymptomatic)
  }

  private def getRecoveredCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Recovered)
  }

}

