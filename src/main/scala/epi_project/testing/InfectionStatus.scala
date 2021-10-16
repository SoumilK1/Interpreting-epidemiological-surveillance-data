package com.bharatsim.examples.epidemiology.testing_latest

import com.bharatsim.engine.basicConversions.StringValue
import com.bharatsim.engine.basicConversions.decoders.BasicDecoder
import com.bharatsim.engine.basicConversions.encoders.BasicEncoder

object InfectionStatus extends Enumeration {
  type InfectionStatus = Value
  val Susceptible,Asymptomatic,Presymptomatic,MildlyInfected,SeverelyInfected,Hospitalized, Recovered,Dead = Value

  implicit val infectionStatusDecoder: BasicDecoder[InfectionStatus] = {
    case StringValue(v) => withName(v)
    case _ => throw new RuntimeException("Infection status was not stored as a string")
  }

  implicit val infectionStatusEncoder: BasicEncoder[InfectionStatus] = {
    case Susceptible => StringValue("Susceptible")
    case Asymptomatic => StringValue("Asymptomatic")
    case Presymptomatic => StringValue("Presymptomatic")
    case MildlyInfected => StringValue("MildlyInfected")
    case SeverelyInfected => StringValue("SeverelyInfected")
    case Hospitalized => StringValue("Hospitalized")
    case Recovered => StringValue("Recovered")
    case Dead => StringValue("Dead")
  }
}
