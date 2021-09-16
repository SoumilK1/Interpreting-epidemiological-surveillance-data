package epi_project.testing

import com.bharatsim.engine.distributions.Exponential

import scala.collection.immutable.HashMap

object Disease {
  final val lambdaS: Double = 1d/4
  final val gamma: Double = 1d/2
  final val lambdaA : Double = 0.143

  final val lambdaP:Double = 1d/2
  final val delta:Double = 0.85

  final val lambdaMI:Double = 0.1

  final val lambdaSI:Double = 0.5
  final val sigma:Double = 0.8

  final val lambdaH: Double = 0.1

  final val numberOfTicksInADay: Int = 3
  final val dt:Double = 1d/numberOfTicksInADay

  var numberOfTestsDoneAtEachTick:Int = 0
  val numberOfTestsAvailable:Int = 100

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
