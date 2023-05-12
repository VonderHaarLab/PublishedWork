# R code and simulated data to accompany our publication in *Neurospychopharmacology* 
  Frankot M., Mueller, P. Young, M. E. & Vonder Haar, C. Statistical power and false positive rates for interdependent outcomes are strongly influenced by test type: 
  Implications for behavioral neuroscience. Neuropsychopharmacology, epub, ahead of print.
  <br>https://www.nature.com/articles/s41386-023-01592-6

## This code can be used to perform all of the model comparisons listed in the published paper across 1000s of simulated datasets. 
Simulated data are provided in the subdirectory and code is available for simulating new data on the Rodent Gambling Task. R files are provided in the order they are referenced in the publication for the analyses.
Data which served as the basis for these simulations are publicly available at https://odc-tbi.org/data/703.

## The goal of this project was to evaluate how different analyses handled compositional data, a special type of interdependency
Many people use standard linear models (e.g., ANOVA, linear regression) to understand how percentages change as a function of their experimental manipulations.
These apply to many different types of data where different states are dependent or exclusive of one another: choice behavior, sleep, normalized gene expression, etc.
We demonstrate that linear analyses may drastically increase overall false positive rates, but that this can be corrected by adopting methods which deal with the dependecies. 
However, these result in lower statistical power. Thus, we also evaluated Bayesian methods to increase power at reasonable animal sample sizes (e.g., N=10/group).
