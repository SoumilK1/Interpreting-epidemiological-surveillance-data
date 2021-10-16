package com.bharatsim.examples.epidemiology.testing_latest

import com.bharatsim.engine.models.Network

case class House(id: Long) extends Network {
  addRelation[Person]("HOUSES")

  override def getContactProbability(): Double = 1
}
