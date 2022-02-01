package epi_project.testing

import com.bharatsim.engine.models.Network

case class House(id: Long) extends Network {
  addRelation[Person]("HOUSES")
  addRelation[Road]("PART_OF")

  override def getContactProbability(): Double = 1
}
