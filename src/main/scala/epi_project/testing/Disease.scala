package epi_project.testing

import scala.collection.immutable.HashMap

object Disease {
  final val lambdaS: Double = 0.222
  final val gamma: Double = 1d/2
  final val lambdaA : Double = 0.143

  final val lambdaP:Double = 1d/2
  final val delta:Double = 0.85

  final val lambdaMI:Double = 0.1

  final val lambdaSI:Double = 0.5
  final val sigma:Double = 0.8

  final val lambdaH: Double = 0.1

  val contactProbability:Double = 0.1

  var numberOfTicksInADay: Int = 2
  final val dt:Double = 1d/numberOfTicksInADay

  var numberOfRTPCRTestsDoneAtEachTick:Int = 0
  var numberOfRTPCRTestsAvailable:Int = 50

  var numberOfRATTestsDoneAtEachTick:Int = 0
  var numberOfRATTestsAvailable:Int = 50

  val daysAfterWhichEligibleForTestingAgain:Int = 7

  val testDelay:Int = 2

  val RTPCRTestSensitivity:Double = 0.9

  val RATTestSensitivity:Double = 0.7

  val quarantineDuration:Int = 14


  final val ageStratifiedBetaMultiplier = HashMap(
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

// BeingTested = 0 -> Hasn't been tested before/can be eligible to get tested again
// BeingTested = 1 -> Has been tested recently and is awaiting result
// BeingTested = 2 -> Tested positive and is in quarantine

