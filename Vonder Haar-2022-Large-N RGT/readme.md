# R Code to accompany our publication in *Frontiers in Behavioral Neuroscience* 
Vonder Haar, C., Frankot, M. Reck, A. M., Milleson, V. J. & Martens, K. M. Large-N rat data enables phenotyping of risky decision-making: A retrospective analysis of brain injury on the Rodent Gambling Task. Frontiers in Behavioral Neuroscience, 837654 
<br/>https://doi.org/10.3389/fnbeh.2022.837654.

## The included code can be used to reproduce all analyses and most figures for the publication.
## The raw data can be downloaded from http://doi.org/10.34945/F5Q597 <br/> <sub>*Note that some variable names may have been adjusted for database compatibility* </sub>

## Briefly, data from four studies and five total cohorts of rats were compiled. For each study, rats received either sham or TBI procedures and were assessed on the Rodent Gambling Task. Exclusing other manipulations, this provided a total of 109 subject in either TBI or Sham groups. Multiple analyses were undertaken to determine if changes in decision-making due to brain injury were due to alterations in overall sensitivity to outcomes, immediate sensitivity to outcomes, or whether changes were better described by a condition-agnostic clustering approach.

1. The initial flie, found [here](1. Setup.R), will aggregate the rawa data into a more useful format and calculate variables which are derived from the raw analysis
*Double-check variable names in initial setup as changes may have occured during uploading to ODC-TBI*

2. The second file, found [here](2. Matching.R), contains analyses and figures from Experiment 1 of the paper.

3. The third file, found [here](3. Immediate Outcomes.R), contains analyses and figures from Experiment 2 of the paper.

4. The fourth file, found [here](4. Clustering.R), contains analyses and figures from Experiment 3 of the paper.
