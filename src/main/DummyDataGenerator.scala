package com.bharatsim.model

import com.bharatsim.engine.utils.Probability.biasedCoinToss
import com.github.tototoshi.csv.CSVWriter

import scala.annotation.tailrec
import scala.util.Random

object DummyDataGenerator {
  val headers = List(
    "Agent_ID",
    "Age",
    "PublicTransport_Jobs",
    "essential_worker",
    "Adherence_to_Intervention",
    "AdminUnitName",
    "H_Lat",
    "H_Lon",
    "HID",
    "school_id",
    "WorkPlaceID",
    "Hospital ID"
  )

  val totalPopulation = 10_000
  val ESSENTIAL_WORKER_FRACTION = 0.02
  val LEAKAGE_FRACTION = 0.1
  val PUBLIC_TRANSPORT_FRACTION = 0.3

  private val averageEmployeesPerOffice = 100
  val totalOffices = (totalPopulation / 2) / averageEmployeesPerOffice

  val averageStudentsPerSchool = 100
  val totalSchools = (totalPopulation / 2) / averageStudentsPerSchool

  val averageHealthCareWorkersPerHospital = 10
  val totalHospitals:Int = (ESSENTIAL_WORKER_FRACTION*totalPopulation).toInt/averageHealthCareWorkersPerHospital

  val random = new Random()

  val writer = CSVWriter.open("dummy10k_hospitals.csv")

  @tailrec
  private def generateRow(rowNum: Int): Unit = {
    val id = rowNum
    val age = random.between(0, 100)
    val houseId = random.between(1, totalPopulation / 4 + 1)
    val isEmployee = age >= 25
    val isStudent = !isEmployee
    val officeId = if (isEmployee) random.between(1, totalOffices + 1) else 0
    val schoolId = if (isStudent) random.between(1, totalSchools + 1) else 0
    val publicTransport = if (biasedCoinToss(PUBLIC_TRANSPORT_FRACTION)) 1 else 0
    val isEssentialWorker = if (isEmployee && biasedCoinToss(ESSENTIAL_WORKER_FRACTION)) 1 else 0
    val hospitalId =  random.between(1,totalHospitals + 1)
    val violatesLockdown: Double = random.between(0.0, 1.0)
    val scale = math pow (10, 1)
    val village_town = "some_village"
    val latitude = Random.nextFloat()
    val longitude = Random.nextFloat()

    writer.writeRow(
      List(
        id,
        age,
        publicTransport,
        isEssentialWorker,
        (math round violatesLockdown * scale) / scale,
        village_town,
        latitude,
        longitude,
        houseId,
        schoolId,
        officeId,
        hospitalId
      )
    )

    if (rowNum < totalPopulation) {
      generateRow(rowNum + 1)
    }
  }

  private def generate(): Unit = {
    println("Total schools", totalSchools)
    println("Total offices", totalOffices)
    println("Total hospitals",totalHospitals)
    writer.writeRow(headers)
    generateRow(1)
  }

  def main(m: Array[String]): Unit = {
    generate()
  }
}
