package epi_project.testing

import com.bharatsim.engine.models.Network

case class Road(id: Long) extends Network {
  addRelation[House]("CONTAINS_HOUSE")

  override def getContactProbability(): Double = 1
}
