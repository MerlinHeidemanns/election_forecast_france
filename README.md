## Election forecast France

### To do

* Integrate past polling data
* Compile polling data

### Features

* What if a party from the previous election disappears or a new party appears?
    * Appearance: Party previously received 0
    * Disappearance: Party currently receives 0
    * Ergo: Bigger covariance matrix initially that covers the whole set, at the
    start conditional matrix with disappearing parties being set to 0, 