# README

This is the repository accompanying Interpreting epidemiological surveillance data: A modelling study based on Pune City. 
https://www.medrxiv.org/content/10.1101/2024.09.13.24313615v1 
## General Information 

An introduction to the framework, BharatSim, can be found in the following website:

https://bharatsim.readthedocs.io/en/latest/

Information on setting up the development environment can be found here:

https://bharatsim.readthedocs.io/en/latest/setup.html
 
## Data

- all data for the manuscript is present in the simulation_data folder. Each testing and public health response scenario is present in a different folder.
- Contact tracing data from Pune city, used to calculate the summary data, gathered by the Pune Knowledge Cluster is available on https://www.pkc.org.in/pkc-focus-area/health/covid-19-pune-clinical-database/
## Information on the directories

### inputcsv 
This directory contains all the csv files that represent the input synthetic population. The information in these csv files is in the following format. 

| Agent_ID | Age | essential_worker | HouseID | WorkPlaceID | HospitalID | CemeteryID | Neighbourhood_ID |
|----------|-----|------------------|---------|-------------|------------|------------|------------------|
| 1        | 13  | 0                | 1008    | 619         | 1          | 1          | 11               |
| 2        | 41  | 0                | 10198   | 2084        | 3          | 1          | 100              |
| 3        | 57  | 0                | 17      | 1183        | 5          | 1          | 1                |
| 4        | 58  | 0                | 19844   | 2236        | 1          | 1          | 198              |
| 5        | 38  | 0                | 1349    | 1805        | 5          | 1          | 15               |
| 6        | 54  | 0                | 17668   | 1601        | 2          | 1          | 175              |
| 7        | 13  | 0                | 20570   | 1078        | 3          | 1          | 205              |
| 8        | 26  | 0                | 17483   | 1452        | 2          | 1          | 173              |
| 9        | 25  | 0                | 9054    | 776         | 5          | 1          | 90               |


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
java -jar abc.jar <initialInfectedFraction> <fractionOfPeopleSelfReportedToStartTesting> <numberOfDailyTests> <RATTestSensitivity> <RATTestFraction> <RTPCRTestSensitivity> <RTPCRTestFraction> <DoesContactTracingHappen> <DoesRandomTestingHappen> <colleagueFraction> <neighbourFraction> <basalFraction> <EPID_required> <filename> <lambdaS> <activateTesting> <activateQuarantine> <do2400ticks>
```

### Argument Descriptions:
1. **initialInfectedFraction** (`Double`): Fraction of the population that is initially infected.
2. **fractionOfPeopleSelfReportedToStartTesting** (`Double`): Fraction of people who will self-report and start testing for the disease.
3. **numberOfDailyTests** (`Int`): The number of daily tests to be conducted.
4. **RATTestSensitivity** (`Double`): Sensitivity of the Rapid Antigen Tests (RAT).
5. **RATTestFraction** (`Double`): Fraction of tests conducted using RAT.
6. **RTPCRTestSensitivity** (`Double`): Sensitivity of the RT-PCR tests.
7. **RTPCRTestFraction** (`Double`): Fraction of tests conducted using RT-PCR.
8. **DoesContactTracingHappen** (`String`): Whether contact tracing is active (`y` for yes, `n` for no).
9. **DoesRandomTestingHappen** (`String`): Whether random testing is active (`y` for yes, `n` for no).
10. **colleagueFraction** (`Double`): Fraction of contacts within the workplace.
11. **neighbourFraction** (`Double`): Fraction of contacts within the neighborhood.
12. **basalFraction** (`Double`): Baseline transmission fraction.
13. **EPID_required** (`String`): This is an additional variable for outputting a format. This is not used in our simulations.  
14. **filename** (`String`): Output filename to store the results.
15. **lambdaS** (`Double`): The rate of disease spread in the population.
16. **activateTesting** (`String`): Whether testing is activated (`y` for yes, `n` for no).
17. **activateQuarantine** (`String`): Whether quarantine is activated (`y` for yes, `n` for no).
18. **do2400ticks** (`String`): Whether the simulation runs for 2400 ticks (`y` for yes, `n` for no).

### Example Command
```
java -jar abc.jar 0.02 0.5 1000 0.85 0.6 0.95 0.4 "y" "y" 0.3 0.2 0.1 "n" output.csv 0.75 "y" "y" "y"
```

This command runs the simulation with 2% initially infected, 50% of the population self-reporting, 1000 daily tests, 85% RAT sensitivity, 60% RAT test fraction, 95% RT-PCR sensitivity, and boolean values (`y` or `n`) for the relevant options. The results will be saved in `output.csv`.

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

## Information on the code
Compartments

![](./Eight-Compartmental-Model.png)



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



