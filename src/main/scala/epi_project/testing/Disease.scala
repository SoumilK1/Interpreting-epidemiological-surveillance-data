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

  var EPID_required:String = "n"

  /**
   *
   =Parameters of the Model are described below=


   */
  final val lambdaS: Double = 0.25
  final val gamma: Double = 1.0
  //TODO: GAMMA IS NOT SUFFICIENT. Right now keep it constant
  final val lambdaA : Double = 0.143

  final val lambdaP:Double = 1d/2
  final val delta:Double = 0.0

  final val lambdaMI:Double = 0.1

  final val lambdaSI:Double = 1d/7
  final val sigma:Double = 1.0

  final val lambdaH: Double = 1d/7
  final val mu: Double = 1.0

  val contactProbability:Double = 0.1

  var numberOfTicksInADay: Int = 6
  final val dt:Double = 1d/numberOfTicksInADay


  var numberOfDailyTests: Int = 100
  var RTPCRTestFraction:Double = 1.0
  var RATTestFraction:Double = 1.0 - RTPCRTestFraction

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

  var colleagueFraction:Double = 0.1

  var neighbourFraction:Double = 0.02

  var basalFraction:Double = 0.0004

  var DoesContactTracingHappen:String = "y"

  var DoesRandomTestingHappen:String = "y"

  var tested_person_id:Long = 0

  var numberOfPeopleSelfReported:Int = 0

  val totalPopulation:Int = 100000

  var fractionOfPeopleSelfReportedToStartTesting:Double = 0.05

  var numberOfPeopleSelfReportedToStartTesting:Double = fractionOfPeopleSelfReportedToStartTesting*totalPopulation

  val probabilityOfReportingSymptoms:Double = 0.8

  val probabilityOfNotHavingCOVID:Double = 0.02

  var numberOfDeadOnEachDay: Double = 0.0

  var numberOfUntestedDeadOnEachDay: Double = 0.0

  var totalNumberOfInfected:Double = 0.0

  var numberOfUninfectedPeopleTested:Double = 0.0

  //TODO: this probability is not 0.95? (try to make an expression, use Sero-Survey data)

//  final val ageStratifiedDeltaMultiplier = HashMap(
//    5 -> 0.103,
//    10 -> 0.103,
//    15 -> 0.22,
//    20 -> 0.22,
//    25 -> 0.47,
//    30 -> 0.47,
//    35 -> 0.99,
//    40 -> 2.1,
//    45 -> 2.1,
//    50 -> 4.4,
//    55 -> 4.4,
//    60 -> 8.9,
//    65 -> 8.9,
//    70 -> 17.1,
//    75 -> 17.1,
//    80 -> 30.3,
//    85 -> 30.3,
//    90 -> 30.3,
//    95 -> 30.3,
//    100 -> 30.3
//  )



  /*
  The sigma multiplier is from Esposito et al.
  The other multipliers are from INDSCI-SIM
   */
  final val ageStratifiedSigmaMultiplier = HashMap(
    5 -> 0.0088,
    10 -> 0.0088,
    15 -> 0.024,
    20 -> 0.024,
    25 -> 0.063,
    30 -> 0.063,
    35 -> 0.17,
    40 -> 0.17,
    45 -> 0.46,
    50 -> 0.46,
    55 -> 1.2,
    60 -> 1.2,
    65 -> 3.3,
    70 -> 3.3,
    75 -> 8.3,
    80 -> 8.3,
    85 -> 19.4,
    90 -> 19.4,
    95 -> 19.4,
    100 -> 19.4
  )
  final val ageStratifiedGammaMultiplier = HashMap(
     9 -> 0.5,
    19 -> 0.45,
    29 -> 0.4,
    39 -> 0.35,
    49 -> 0.3,
    59 -> 0.25,
    69 -> 0.2,
    79 -> 0.15,
    89 -> 0.1,
    99 -> 0.1,
  )
  final val ageStratifiedMuMultiplier = HashMap(
       9 -> 0.0185,
       19 -> 0.0187,
       29 -> 0.0143,
       39 -> 0.0166,
       49 -> 0.034,
       59 -> 0.05,
       69 -> 0.097,
       79 -> 0.21,
       89 -> 0.22,
       99 -> 0.22,
        )
    final val ageStratifiedDeltaMultiplier = HashMap(
       9 -> 0.999,
      19 -> 0.997,
      29 -> 0.988,
      39 -> 0.968,
      49 -> 0.951,
      59 -> 0.898,
      69 -> 0.834,
      79 -> 0.757,
      89 -> 0.727,
      99 -> 0.727
    )

// Sigma multiplier - https://www.medrxiv.org/content/10.1101/2021.07.29.21261282v2.full.pdf


//  final val ageStratifiedMuMultiplier = HashMap(
//    5 -> 0.002,
//    10 -> 0.002,
//    15 -> 0.002,
//    20 -> 0.002,
//    25 -> 0.002,
//    30 -> 0.002,
//    35 -> 0.074,
//    40 -> 0.074,
//    45 -> 0.074,
//    50 -> 0.074,
//    55 -> 0.604,
//    60 -> 0.604,
//    65 -> 0.604,
//    70 -> 1.416,
//    75 -> 1.416,
//    80 -> 1.416,
//    85 -> 1.416,
//    90 -> 1.416,
//    95 -> 1.416,
//    100 -> 1.416
//  )

  //Mu multiplier - https://www.medrxiv.org/content/10.1101/2020.11.17.20228155v2.full.pdf
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

 typeOfTestGiven = 1 -> RT-PCR
 typeOfTestGiven = 2 -> RAT

 */
