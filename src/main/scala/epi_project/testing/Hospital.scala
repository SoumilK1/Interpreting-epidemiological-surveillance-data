package epi_project.testing

import com.bharatsim.engine.models.Network


case class Hospital(id: Long) extends Network {
  addRelation[Person]("HOSPITALIZES")

  override def getContactProbability(): Double = 0.1
}



