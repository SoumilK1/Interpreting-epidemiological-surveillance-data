package epi_project.testing

import scala.collection.immutable.HashMap

object Disease {

  /**
   *
   =ParametersForSchedule=

   *
   *
   */




  final val stayAtHouseStartTick = 0
  final val stayAtHomeEndTick = 2
  final val goToOfficeStartTick = 3
  final val goToOfficeEndTick = 4
  final val stayInNeighbourhoodStartTick = 5
  final val stayInNeighbourhoodEndTick = 5
  final val numberOfTicks: Int = 2400

  /**
   *
   =Parameters of the Model are described below=


   */
  final val lambdaS: Double = 0.25
  final val gamma: Double = 1d/2
  final val lambdaA : Double = 0.143

  final val lambdaP:Double = 1d/2
  final val delta:Double = 0.85

  final val lambdaMI:Double = 0.1

  final val lambdaSI:Double = 0.5
  final val sigma:Double = 0.8

  final val lambdaH: Double = 0.1
  final val mu: Double = 0.2

  val contactProbability:Double = 0.1

  var numberOfTicksInADay: Int = 6
  final val dt:Double = 1d/numberOfTicksInADay


  var numberOfDailyTests: Int = 60
  var RTPCRTestFraction:Double = 0.5
  var RATTestFraction:Double = 1 - RTPCRTestFraction

  var numberOfRTPCRTestsDoneAtEachTick:Double = 0
  var numberOfRTPCRTestsAvailable:Double = numberOfDailyTests * RTPCRTestFraction

  var numberOfRATTestsDoneAtEachTick:Double = 0
  var numberOfRATTestsAvailable:Double = numberOfDailyTests * RATTestFraction

  var totalNumberOfTestsDone:Double = 0
  var numberOfPositiveTestsAtEachTick:Double = 0

  var totalNumberOfPositiveTests:Double = 0

  val daysAfterWhichEligibleForTestingAgain:Int = 7

  val testDelay:Int = 2

  var RTPCRTestSensitivity:Double = 1.0

  var RATTestSensitivity:Double = 0.7

  val quarantineDuration:Int = 14

  //TODO: Figure out if Quarantine and Isolation makes sense

  val isolationDuration:Int =7

  val colleagueFraction:Double = 0.1

  var DoesContactTracingHappen:String = "y"

  var tested_person_id:Long = 0

  var numberOfPeopleSelfReported:Int = 0

  val numberOfPeopleSelfReportedToStartTesting:Int = 50

  val probabilityOfReportingSymptoms:Double = 0.8


  val probabilityOfHavingCOVID:Double = 0.5

  //TODO: this probability is not 0.95? (try to make an expression, use Sero-Survey data)

  final val ageStratifiedMuMultiplier = HashMap(
    9 -> 0.34,
    19 -> 0.67,
    29 -> 1.0,
    39 -> 1.0,
    49 -> 1.0,
    59 -> 1.0,
    69 -> 1.0,
    79 -> 1.24,
    89 -> 1.47,
    99 -> 1.47
  )

  final val ageStratifiedSigmaMultiplier = HashMap(
    9 -> 0.34,
    19 -> 0.67,
    29 -> 1.0,
    39 -> 1.0,
    49 -> 1.0,
    59 -> 1.0,
    69 -> 1.0,
    79 -> 1.24,
    89 -> 1.47,
    99 -> 1.47
  )
}

/**

 beingTested = 0 -> Hasn't been tested before/can be eligible to get tested again
 beingTested = 1 -> Has been tested recently and is awaiting result
 beingTested = 2 -> Tested positive and is in quarantine
 beingTested = 3 -> Isolated until getting a test
 beingTested = 4 -> Isolated but not eligible for testing

 testCategory = 1 -> Targeted Testing
 testCategory = 2 -> Contact
 testCategory = 3 -> Random Testing

 isAContact = 0 -> Not a contact
 isAContact = 1 -> High Risk Contact/Household
 isAContact = 2 -> Low Risk Contact who is symptomatic
 isAContact = 3 -> Low Risk Asymptomatic/Presymptomatic/Susceptible/Recovered quarantined for 7 days


 */
