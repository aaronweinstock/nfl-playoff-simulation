## **NFL Playoff Probability Simulation**

Applies the Glicko rating system to simulate multiple possibilities for the remainder of the current NFL season, in an effort to estimate the likelihood that each team reaches the playoffs at a given point in the season. Playoff probabilities are calculated by applying NFL tiebreaking procedures to identify division winners and wild cards from each simulated season. Overall playoff probabilities, as well as chances to win division or receive wild card, are generated for each team.

Below, brief descriptions of the files in this repository are provided.


### **Code required for simulation**

___

Refer to the six scripts in the `R` folder for the functions required to execute the simulation (sorted below in order of simulation steps).

1. `01_Initialize_Glicko.R`: Based on a user specified start year, obtain the Glicko ratings for all 32 current NFL teams at the end of the previous NFL season (the completed part of the current season is added with a separate function).

2. `02_Simulation_Setup.R`: Use the results of the completed portion of the current NFL season to update the Glicko ratings to their current state; initialize a dataframe with which to organize a simulated season; identify the weeks in which each team plays in-division and in-conference games during the current season (used for tiebreakers in later steps).

3. `03_Season_Simulation.R`: Simulate a season –- predict the remaining results of the current NFL week (using Glicko-style expected wins), update Glicko ratings based on these results, predict the following week, and repeat until the completion of a season.

4. `03a_Division_Tiebreak.R`: For a simulated season, identify division winners using NFL tiebreakers.

5. `03b_Wildcard_Tiebreak.R`: For a simulated season, identify wild card recipients using NFL tiebreakers.

6. `04_Simulation_Output.R`: Given a number of simulated sets of playoff teams, identify and organize overall playoff, division win, and wild card probabilities, and plot simulation probabilities in an easy to comprehend format. In addition, included are functions to load required R packages for this simulation algorithm, and execute the simulation algorithm described by the previously documented functions.


### **Details on Glicko ratings, as applied to this simulation**

___

Refer to the two documents in `Details_on_Glicko_Use` for details on why and how the Glicko rating system is applied to this simulation process.

1. `01_Using_Glicko.pdf`: Describes on the use of the Glicko rating system for this simulation, including brief description of the system, equation for win probability, reasons for applying it to an NFL season simulation, and requisite data for use.

2. `02_Selecting_Default_C.pdf`: Describes on the logic behind the default \(c\) value for the Glicko rating algorithm provided in the simulation functions.


### **Simulation Output**

___

Refer to the following two files (in the main directory) for execution and results of this simulation.

1. `Run.R`: Source the functions in `R`, and run the simulation.

2. `Example_Plot.png`: An example of how simulation results are displayed upon completion of the function (the provided plot gives probabilities after week 12 of the 2018 NFL season).

