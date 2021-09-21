## Election Forecasting and Candidate Uncertainty











### Objective

Reduce forecasting error across all candidates compared to existing models

* 

* What are existing models?
  * Poll averages, e.g. politico
  * Fundamentals models, e.g. Foucault & Nadeau 2012 based on local departmental data
  * Nadeau et al 2017
    * Focus on second round results when prediction becomes easier; for example through the association of popularity of the current president with their vote share whcih has held in France given the frequent absence of two term presidents

Predict the likely runoff candidates and the outcome of these races

* 

Estimate how the race would change if different candidates were to drop out/how this would affect the runoff candidates and the outcome of the second round

* Poll average models are only sensible once all candidates are known which in France do

Estimate the abstention rate

* Abstention rate in the second round election in France is relatively high (combined share of votes from eligible voters for the runoff candidates in 2017 was 26%)



### Method

Estimate the latent support of candidates prior to party primaries

* Consider the transition probabilities of voters from one candidate to another (or abstention) by conditioning a transition matrix on the candidate receiving 0% of the vote
* Allows for estimating the outcomes and effects of different party primary results for the election
* Allows for estimating the abstention rate and its effect on different second round scenarios



Benefit

Insight on modeling abstentions 

































#### Interesting case

* Voters can vote strategically given the presence of a run-off system
  * We want to understand how candidates on the left and right are interrelated i.e. how voters move between them as well as the option of abstaining from voting.
* The party system is weak and candidates can change over the course of the election season
  * We want to use all the available data to estimate parameters and not only polls that include the candidates that are on the ballot in the end.
* 



#### Contribution

* Forecasting model for a democracy with a weak party system
* Estimating how preferences would change under different candidates 
* Estimating how voter switch between candidates with the added difficulty of limited or no access to individual survey responses
* Providing a forecast that considers trends in vote intentions toward election day rather than an extrapolation of the current standing with added uncertainty



#### Model

Vector-autoregression on the log-odds scale of the vote intention by party for all possible candidates

Treating polls that only ask for a subset of candidates as informative about the conditional distribution if non-included candidates were to receive no support ie equivalent to them not participating

Using past elections to inform about the distribution of the sum of the polling errors over subsets of the candidates based on their position on the ideological spectrum in France



