package epi_project.testing

import com.bharatsim.engine.models.Network

case class Neighbourhood(id:Long) extends Network{
  addRelation[Person]("IS_NEIGHBOURHOOD_OF")

  override def getContactProbability(): Double = 1
}
