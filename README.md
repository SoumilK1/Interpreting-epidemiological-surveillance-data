# README

This is the repository accompanying Interpreting epidemiological surveillance data: A modelling study based on Pune City. 
https://www.medrxiv.org/content/10.1101/2024.09.13.24313615v1 
## General Information 

An introduction to the framework, BharatSim, can be found in the following website:

https://bharatsim.readthedocs.io/en/latest/

Information on setting up the development environment can be found here:

https://bharatsim.readthedocs.io/en/latest/setup.html




## Some information on some of the Directories

### inputcsv 
This directory contains all the csv files that represent the input synthetic population. The information in these csv files is in the following format. 

| Agent_ID | Age | essential_worker | HouseID | school_id | WorkPlaceID | HospitalID | RoadID |
|----------|-----|------------------|---------|-----------|-------------|------------|--------|
| 1        | 57  | 0                | 1281    | 42        | 1           | 32         | 129    |
| 2        | 55  | 0                | 2463    | 16        | 1           | 61         | 247    |
| 3        | 35  | 0                | 1687    | 222       | 1           | 42         | 169    |
| 4        | 87  | 0                | 1193    | 250       | 2           | 29         | 120    |
| 5        | 27  | 0                | 964     | 237       | 1           | 24         | 97     |

### csv
 
This directory needs to be created and all output files are put here. The output files in this directory will have the following format:

| Time     | Susceptible | Asymptomatic | Presymptomatic | MildlyInfected | SeverelyInfected | Recovered | Hospitalized | Dead | Infected | EligibleForTargetedTest | TestedByTargetedTest | EligibleForContactTracing | TestedByContactTracing | EligibleForRandomTest | TestedByRandomTest | RTPCRTestsConducted | RATTestsConducted | TotalTestsConducted | TestPositivityRate | NumberOfPositiveTests | CaseFatalityRate |
|----------|-------------|--------------|----------------|----------------|------------------|-----------|--------------|------|----------|-------------------------|----------------------|---------------------------|------------------------|-----------------------|--------------------|---------------------|-------------------|---------------------|--------------------|-----------------------|------------------|
| 0.166667 | 9990        | 10           | 0              | 0              | 0                | 0         | 0            | 0    | 10       | 0                       | 0                    | 0                         | 0                      | 0                     | 0                  | 0                   | 0                 | 0                   | 0                  | 0                     | 0                |
| 0.333333 | 9990        | 10           | 0              | 0              | 0                | 0         | 0            | 0    | 10       | 0                       | 0                    | 0                         | 0                      | 0                     | 0                  | 0                   | 0                 | 0                   | 0                  | 0                     | 0                |
| 0.5      | 9990        | 10           | 0              | 0              | 0                | 0         | 0            | 0    | 10       | 0                       | 0                    | 0                         | 0                      | 0                     | 0                  | 0                   | 0                 | 0                   | 0                  | 0                     | 0                |
| 0.666667 | 9990        | 9            | 0              | 0              | 0                | 1         | 0            | 0    | 9        | 0                       | 0                    | 0                         | 0                      | 0                     | 0                  | 0                   | 0                 | 0                   | 0                  | 0                     | 0                |


## Running the Simulation

To run the simulation, type the following command on the sbt shell in your intellij

```
run args(0) args(1) args(2) args(3) args(4) args(5) args(6) args(7)
```

`testing_begins_at = args(0)` (fraction of recovered at which testing begins)

`Disease.numberOfDailyTests = args(1)` (Number of Tests per day)

`Disease.RATTestSensitivity = args(2)` (Decimal)

`Disease.RATTestFraction = args(3)` (Decimal)

`Disease.RTPCRTestSensitivity = args(4)`(Decimal)

`Disease.RTPCRTestFraction = args(5)`(Decimal)

`Disease.DoesContactTracingHappen = args(6)`("y" in case contact tracing should happen, "n" in case it should not happen)

`filename = args(7)`(Name of file that you want to give)

## Assembly

The assembly command allows for us to create a JAR file. This can be done by writing the following command in the sbt shell

```
assembly
```
### Running the Jar file
The created JAR File can be found in `target/scala-2.13`, and is named such that it has "assembly" in its string. 

To run the JAR file, you might need to create a new directory which has the following tree:

```
.
├── input csv/
│   └──ResidentialArea10k.csv
├── EPID_csv
├── csv
└── <JARFILENAME>.jar
```
to run the JAR file you need to run the following command on your terminal

```
java -jar <JARFILENAME>.jar args(0) args(1) args(2) args(3) args(4) args(5) args(6) args(7)
```

For example, we have:

```
java -jar EPI.jar 0.1 100 0.7 0.3 0.98 0.7 "y" my_file

```
**Note**: make sure that you write args(6) within inverted commas

## Information on the code
Compartments

![](./Eight-Compartmental-Model.png)


Thresholds
-----------

- Total number of ticks per day = 400

- The number of ticks per day = 2

- Total Population = 10,000


Description
-----------

### Behaviours


1. `checkEligibilityForTargetedTesting` checks whether a person is
   eligible for getting a targeted test (on the next day). Only
   symptomatic people who report their symptoms are eligible for
   getting a targeted test and the probability that a symptomatic
   person will report his/her symptoms is currently set to 0.9. At the
   end of the behaviour, the flag\
   `isEligibleForTargetedTesting` is set to be `true` for all those who
   are eligible.

2. `checkEligibilityForRandomTesting` checks whether a person is
   eligible for getting a random test. All agents barring those who are
   hospitalized, quarantined or already waiting for a test result can
   be eligible for a random test. The flag `isEligibleForRandomTesting`
   is set to be `true` for all eligible agents.

3. `declarationOfResults-checkForContacts` declares the test result of
   those people who are awaiting a test result. Currently, the test
   results are declared 2 days after the test is conducted. If the test
   result is true for a person, their contacts are identified as
   follows:

1. **High Risk Contacts** are the people who are living in the same
   house as the positive agent. They are given a flag `isAContact`
   = 1, and they are isolated until they are tested.

2. **Low Risk Contacts** are the people working in the same office
   as the positive agent. They are randomly selected based on a
   `biasedCoinToss` which reflects the fact that the positive
   person wouldn't have come into contact with all the people
   working in his office. The randomly selected contacts are then
   checked for symptoms.

3. If they are symptomatic, they are classified as **Low Risk
   Symptomatic Contacts** and they are flagged as `isAContact` =2.
   Until they are given a test, they are isolated. The low risk
   contacts who are asymptomatic are classified as **Low Risk
   Asymptomatic Contacts**. They are flagged as `isAContact` = 3.
   Such people are not tested. However, they are isolated in their
   homes for a period of 7 days.

4. `quarantinePeriodOver` checks the number of days for which a
   positive person is quarantined. Once this number reaches 14, the
   person is now no longer in quarantine. For all the agents in
   quarantine, the `beingTested` flag is set to 2. At the end of this
   behaviour, all those agents whose quarantine period is over are
   flagged as `beingTested` = 0.

5. `isolationPeriodOver` checks the number of days for which a low risk
   asymptomatic contact is isolated. If this number equals 7, then the
   contact is no longer under isolation. At the end of this behavior
   the `isAContact` flag is set to 0 (as technically they are no longer
   a contact) and the `beingTested` flag is set to 0.


## Interventions

### Testing

Testing is started when the total number of recovered agents crosses a
threshold value. When testing starts in the population, the `testing`
intervention is activated. There are two types of tests - RT-PCR and
RATs out of which, RT-PCR tests have a higher sensitivity. There are a
fixed number of tests available on each day, and since RT-PCR tests have
a higher sensitivity, they are used up first. Only when the RT-PCR tests
available on a day are exhausted, the RATs are used.\
The priority for testing is as follows:-

1. High risk contacts

2. Self reported symptomatics and low risk symptomatic contacts


Once testing is done, the following flags are updated:-

- `lastTestDay` gets updated to the day on which the person is last
  tested (People can be tested more than once).

- `beingTested` is changed to 1, since people who got a test are now
  awaiting the test result.

- `testCategory` is updated depending on how the person was classified
  before getting the test. If the person was a contact, `testCategory`
  is flagged as 2, else, if the person was a self reported
  symptomatic, `testCategory` is flagged as 1.

- `isEligibleForTargetedTesting` is set to `false` and `isAContact` is
  set to 0, since a just tested person is no longer eligible for
  testing.

- `lastTestResult` is set to `true` or `false` depending on whether
  the person is positive or negative. However, the test results are
  announced to the agents only after a certain delay.


[comment]: <> (Schedules)

[comment]: <> (- Office workers spend tick 0 at home and tick 1 in the office.)

[comment]: <> (- Health care workers spend tick 0 at home and tick 1 in the hospital)

[comment]: <> (- People tested positive spend both ticks at home for 14 days.)

[comment]: <> (- Low risk asymptomatic contacts spend both ticks at home for 7 days.)

[comment]: <> (- Hospitalized people spend the entire day in the hospital until they)

[comment]: <> (  recover or die.)

[comment]: <> (- Dead people are buried in a cemetery.)



