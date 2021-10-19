# Master Repo - COVID-19-Epidemiology Semester Project

## Some information on some of the Directories

**inputcsv** directory contains all the csv files that represent the synthetic population. `

**csv** is a directory that *you will have to create* at the same level as **inputcsv**. This directory will contain the **output csv** files of the simulation. 

## Running the Simulation

To run the simulation,  type the following command on the sbt shell

`run "testingbeginsat" "numberoftestsavailable"`

testtingbeginsat is a float between 0 and 1
numberoftestsavailable is an int(chosen as 0.1%/0.2%/0.5% of the population)


