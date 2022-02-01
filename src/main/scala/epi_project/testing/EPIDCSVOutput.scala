package epi_project.testing

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs

class EPIDCSVOutput (context: Context) extends CSVSpecs {
  override def getHeaders: List[String] =
    List(
      "PositiveCaseID",
      "ContactID"
    )

  override def getRows(): List[List[Any]] = {
    val graphProvider1 = context.graphProvider
    val label = "Person"

    val row = List(
      graphProvider1.fetchNodes(label,("lastTestResult" equ true)).last,
      graphProvider1.fetchNodes(label,"isAContact" equ true)

    )
    List(row)
  }
}