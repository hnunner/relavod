# ReLAVOD - Reinforcement Learning Agents in the Volunteer's Dilemma

Welcome to the ReLAVOD project. This project is a stand-alone R program of reinforcement learning agents interacting in the repeated Volunteer's Dilemma (VOD). The purpose of ReLAVOD is to use reinforcement learning to investigate the role of cognitive mechanisms in the emergence of conventions. The VOD is a multi-person, binary choice collective goods game in which the contribution of only one person is necessary and sufficient to produce a benefit for the entire group. Behavioral experiments show that in the symmetric VOD, where all group members have the same costs of volunteering, a turn-taking convention emerges, whereas in the asymmetric VOD, where one “strong” group member has lower costs of volunteering, a solitary-volunteering convention emerges with the strong member volunteering most of the time. ReLAVOD offers a general framework to test reinforcement learning in the repeated VOD and offers three different classes of reinforcement learning models to test the ability of reinforcement learning to replicate empirical findings.

## Installing and running ReLAVOD
The project is a stand-alone software application that can be run within an active R session.

1. [Download R](https://cran.r-project.org/) and install if necessary.
2. **Either** download the ReLAVOD project as zip-file by clicking the __Code >> Download ZIP__ button at the top this page and unzip the downloaded file to extract the project into a folder, **or** clone the repository to your local machine.
3. Open the file *code/composition.R* in a suitable editor.
4. Adjust the parameter settings in the *fitParameters* function.
5. Run the simulation by invoking the *fitParameters* function.

## Data and analysis
When invoking the *fitParameters* function ReLAVOD will create a folder named *simulation* that contains all generated data. The folder structure is divided into the outputs for each model class (Random, ClassicQ, SequenceX, CoordinateX). These folders are further divided into outputs per single parameter combinations. Each folder contains all raw data for the three types of VOD (symmetric, asymmetric 1, asymmetric 2), interaction patterns, goodness of fit plots, and CSV files for the model parameters and comparisons between model and experimental data.
