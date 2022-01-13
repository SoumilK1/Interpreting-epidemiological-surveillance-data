# Master Repo - COVID-19-Epidemiology Semester Project

## Some information on some of the Directories

**inputcsv** directory contains all the csv files that represent the synthetic population. `

**csv** is a directory that *you will have to create* at the same level as **inputcsv**. This directory will contain the **output csv** files of the simulation.

## Running the Simulation

To run the simulation, type the following command on the sbt shell

`testing_begins_at = args(0)` (fraction of recovered at which testing begins)

`Disease.numberOfDailyTests = args(1)` (Number of Tests per day)

`Disease.RATTestSensitivity = args(2)` (Decimal)

`Disease.RATTestFraction = args(3)` (Decimal)

`Disease.RTPCRTestSensitivity = args(4)`(Decimal)

`Disease.RTPCRTestFraction = args(5)`(Decimal)

`Disease.DoesContactTracingHappen = args(6)`("y" in case contact tracing should happen, "n" in case it should not happen)

`filename = args(7)`(Name of file that you want to give)
