## Election forecast France

### Sources

* Harris: https://harris-interactive.fr/actualite/sondages-publies/
* ifop: https://www.ifop.com/opinion/?id_category=9#isotope


### To do

* Integrate past polling data
* Compile polling data

### Features

* What if a party from the previous election disappears or a new party appears?
    * Appearance: Party previously received 0
    * Disappearance: Party currently receives 0
    * Ergo: Bigger covariance matrix initially that covers the whole set, at the
    start conditional matrix with disappearing parties being set to 0, 