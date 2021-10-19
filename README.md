## Election forecast France

### Sources

* Harris: https://harris-interactive.fr/actualite/sondages-publies/
* ifop: https://www.ifop.com/opinion/?id_category=9#isotope

### Model outline

The fundamental idea of this model is that a plurality of potential candidates - either prior to primaries or prior to the second round - implies that voters have a preferred candidate but may switch when their preferred option is not available. The model thus tries to estimate how voters will align in any arbitrary combination of candidates - again either after the primaries or in the second round. This approach allows us to make statements about the relative potential of candidates prior to their primaries and statements about otherwise unpolled second round matchups such as Chirac - Le Pen in 2002.

Given the complexity and plurality of parameters, the model leverages past polling data to identify the parameters we rely on to properly adjust the raw polling numbers from past data. Due to computational constraints, for past elections the model operates at the level of politics blocs (and abstention) which in France are the extreme left, left, center, right, and extreme right.


### FAQ

**Why does the model use a multinomial distribution rather than an approximation through a binomial or normal distribution?**

**Why does the model implement so many sum-to-zero constraints on the parameters?**

Neither $\tau$ nor $\alpha$ are identifiable without the sum to zero constraints. Within the surveys/polls of a specific pollster the sum of the survey/poll specific $\tau$ parameters can substitute for alpha.

**Why fill the last slot rather than the first?**
Filling the first slot of abstention allows the model to choose arbitrary values for all other parameters because their sum can be balanced out with an essentially free parameter as the abstention slot is free for polls/pollster that do not report abstentions.

### Problems

* The model does not implement sum to zero restrictions correctly for pollsters with only one poll. The restriction over their number of polls does not work.