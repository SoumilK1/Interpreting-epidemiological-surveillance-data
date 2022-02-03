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

class EPIDCSVOutput (placeType:String,context: Context) extends CSVSpecs {
  override def getHeaders: List[String] =
    List(
      "PersonID",
      "LastTestTick",
      "TestResult",
      "FinalInfectionStatus"
    )
  override def getRows(): List[List[Any]] = {

    val rows = ListBuffer.empty[List[String]]
    val locations = context.graphProvider.fetchNodes(placeType)

    locations.foreach(oneLocation => {
      val decodedLoc = decodeNode(placeType, oneLocation)
      val locId = getId(placeType, oneLocation).toString
      val lastTestTick_name = decodedLoc.asInstanceOf[Person].lastTestDay.toString
      val ResultOfTest = decodedLoc.asInstanceOf[Person].lastTestResult.toString
      val infection = decodedLoc.asInstanceOf[Person].infectionState.toString



      if(lastTestTick_name.toInt != -20000){
        rows.addOne(List(locId,lastTestTick_name,ResultOfTest,infection))

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