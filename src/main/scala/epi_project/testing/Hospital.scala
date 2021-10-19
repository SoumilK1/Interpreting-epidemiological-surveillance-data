package epi_project.testing

import com.bharatsim.engine.models.Network


case class Hospital(id: Long) extends Network {
  addRelation[Person]("EMPLOYS/PROVIDES_CARE")

  override def getContactProbability(): Double = 1.0
}



