package epi_project.testing

import com.bharatsim.engine.utils.Probability.biasedCoinToss
import com.github.tototoshi.csv.CSVWriter
import com.bharatsim.engine.distributions.LogNormal

import java.util.SplittableRandom
import scala.annotation.tailrec

/**
 * Utility to create a population like the one used in ``Optimizing Testing for COVID-19 in India``
 * In order to use it, after importing the package, add the following to your Main class

       val d = DataGeneratorForTestingPaper
       d.main("some_filename")
       System.exit(0)

 * The `System.exit(0)` at the end is to make sure the rest of the class isn't run. If you wish for the
 * rest of the class to run, you can remove this line.
 */

object DataGeneratorForTestingPaper {
//  val headers = List(
//    "Agent_ID",
//    "Age",
//    "PublicTransport_Jobs",
//    "essential_worker",
//    "Adherence_to_Intervention",
//    "AdminUnitName",
//    "H_Lat",
//    "H_Lon",
//    "HID",
//    "school_id",
//    "WorkPlaceID",
//    "Hospital ID"
//  )
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

  val totalPopulation = 10_000
    val ESSENTIAL_WORKER_FRACTION = 0.0
    val PUBLIC_TRANSPORT_FRACTION = 0.0

  private val averageEmployeesPerOffice = 40
  val totalOffices = totalPopulation / averageEmployeesPerOffice

  val averageStudentsPerSchool = 0
  val totalSchools = 0

  val totalHospitals: Int = 2

  val random = new SplittableRandom()
  def ceil(x: Double): Double = java.lang.Math.ceil(x)


  @tailrec
  private def generateRow(rowNum: Int, writer: CSVWriter): Unit = {
    val id = rowNum
    val age = random.nextInt(25, 100)                 // Philip: No school-goers, only employees
    val houseId= random.nextInt(1, totalPopulation / 4 + 1)
    val roadId:Int = (houseId/40)
    val isEmployee = age >= 25
    val isStudent = !isEmployee
    val officeId = if (isEmployee) random.nextInt(1, totalOffices + 1) else 0
    val schoolId = if (isStudent) random.nextInt(1, totalSchools + 1) else 0
    val publicTransport = if (biasedCoinToss(PUBLIC_TRANSPORT_FRACTION)) 1 else 0
    val isEssentialWorker = if (isEmployee && biasedCoinToss(ESSENTIAL_WORKER_FRACTION)) 1 else 0
    val hospitalId = random.nextInt(1, totalHospitals + 1)
    val violatesLockdown: Double = random.nextDouble(0.0, 1.0)
    val scale = math pow(10, 1)
    val village_town = "some_village"
    val latitude = random.nextDouble()
    val longitude = random.nextDouble()

    writer.writeRow(
//      List(
//        id,
//        age,
//        publicTransport,
//        isEssentialWorker,
//        (math round violatesLockdown * scale) / scale,
//        village_town,
//        latitude,
//        longitude,
//        houseId,
//        schoolId,
//        officeId,
//        hospitalId
//      )
      List(
        id,
        age,
        isEssentialWorker,
        houseId,
        officeId,
        hospitalId,
        roadId,
        1
      )
    )

    if (rowNum < totalPopulation) {
      generateRow(rowNum + 1, writer)
    }
  }

  private def generate(fileName: String): Unit = {
    val writer = CSVWriter.open(fileName+".csv")

    println("Total schools", totalSchools)
    println("Total offices", totalOffices)
    println("Total hospitals", totalHospitals)

    writer.writeRow(headers)
    generateRow(1, writer)
  }

  def main(fileName: String): Unit = {
    generate(fileName)
  }
}
