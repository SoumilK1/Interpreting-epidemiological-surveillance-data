package epi_project.testing

import com.bharatsim.engine.models.Network

case class Cemetery(id: Long) extends Network {
  addRelation[Person]("RESTING_PLACE_OF")

  override def getContactProbability(): Double = 1
}
