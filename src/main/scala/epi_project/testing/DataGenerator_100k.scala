package epi_project.testing

import com.bharatsim.engine.utils.Probability.biasedCoinToss
import com.github.tototoshi.csv.CSVWriter
import com.bharatsim.engine.distributions.LogNormal

import java.util.SplittableRandom
import scala.annotation.tailrec

object DataGenerator_100k {

  val headers = List(
    "Agent_ID",
    "Age",
    "essential_worker",
    "HouseID",
    "WorkPlaceID",
    "HospitalID",
    "RoadID",
    "CemeteryID"
  )

  val totalPopulation = 100_000
  val ESSENTIAL_WORKER_FRACTION = 0.008

  private val averageEmployeesPerOffice = 40
  val totalOffices = totalPopulation / averageEmployeesPerOffice

  val averageStudentsPerSchool = 0
  val totalSchools = 0

  val totalHospitals: Int = 2

  val random = new SplittableRandom()
  def ceil(x: Double): Double = java.lang.Math.ceil(x)

}
