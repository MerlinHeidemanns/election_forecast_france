## Election Forecasting and Candidate Uncertainty





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



