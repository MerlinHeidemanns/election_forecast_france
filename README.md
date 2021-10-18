## Election forecast France

### Sources

* Harris: https://harris-interactive.fr/actualite/sondages-publies/
* ifop: https://www.ifop.com/opinion/?id_category=9#isotope


### FAQ

**Why does the model use a multinomial distribution rather than an approximation through a binomial or normal distribution?**

**Why does the model implement so many sum-to-zero constraints on the parameters?**

Neither $\tau$ nor $\alpha$ are identifiable without the sum to zero constraints. Within the surveys/polls of a specific pollster the sum of the survey/poll specific $\tau$ parameters can substitute for alpha.


### Problems

* The model does not implement sum to zero restrictions correctly for pollsters with only one poll. The restriction over their number of polls does not work.