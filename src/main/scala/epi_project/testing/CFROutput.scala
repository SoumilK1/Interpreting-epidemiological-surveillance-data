
package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.listeners.CSVSpecs
import com.bharatsim.engine.models.Node
import com.bharatsim.engine.basicConversions.encoders._
import com.bharatsim.engine.basicConversions.decoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import com.bharatsim.engine.models.Node
import scala.collection.mutable.ListBuffer

import scala.collection.mutable.ListBuffer


class CFROutput(placeType:String, context: Context) extends CSVSpecs{
  override def getHeaders: List[String] =
    List(
      "AgentID",
      "Age",
      "InfectionState",
      "dayAtWhichIdentified",
      "dayAtWhichInfected",
      "lastTestDay",
      "lastTestResult",
    )

  override def getRows(): List[List[Any]] = {
    val rows = ListBuffer.empty[List[String]]
    val locations = context.graphProvider.fetchNodes(placeType)

    locations.foreach(oneLocation => {
      val decodedLoc = decodeNode(placeType, oneLocation)
      val locId = getId(placeType, oneLocation).toString
      val age = decodedLoc.asInstanceOf[Person].age.toString
      val infectionStatus = decodedLoc.asInstanceOf[Person].infectionState.toString
      val IdentificationDay = decodedLoc.asInstanceOf[Person].dayAtWhichPersonIsIdentified.toString
      val InfectionDay = decodedLoc.asInstanceOf[Person].dayAtWhichPersonIsInfected.toString
      val lastTestDay = decodedLoc.asInstanceOf[Person].lastTestDay.toString
      val lastTestResult = decodedLoc.asInstanceOf[Person].lastTestResult.toString

      if(context.getCurrentStep==1200){
        rows.addOne(List(locId,age,infectionStatus,IdentificationDay,InfectionDay,lastTestDay,lastTestResult))
      }

    })
      rows.toList
  }


  def decodeNode(classType: String, node: GraphNode): Node = {
    classType match {
      //      case "House" => node.as[House]
      //      case "Office" => node.as[Office]
      case "Person" => node.as[Person]
    }
  }
  def getId(classType: String, node: GraphNode) : Long = {
    classType match {
      //      case "House" => node.as[House].id
      //      case "Office" => node.as[Office].id
      case "Person" => node.as[Person].id
    }
  }


}

