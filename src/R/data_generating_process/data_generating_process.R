## Clean
rm(list = ls())
## Libraries
library(tidyverse)
## Simulation
### Idea
#' - spatial model: parties are positioned somewhere and voters select the party
#' closest to them
### Simulate parties
N_parties <- 5
party_positions <- data.frame(
  party_x = runif(N_parties, 0, 1),
  party_y = runif(N_parties, 0, 1),
  party_id = 1:N_parties
)
### Simulate voters
N_voters <- 1000
voter_positions <- data.frame(
  voter_x = runif(N_voters, 0, 1),
  voter_y = runif(N_voters, 0, 1),
  voter_id = 1:N_voters
)
## Determine voter position
voter_party <- full_join(party_positions, voter_positions, by = character()) %>%
  mutate(distance = sqrt((voter_x - party_x)^2 +
                         (voter_y - party_y)^2)) %>%
  group_by(voter_id) %>%
  filter(distance == min(distance))
ggplot(voter_party, aes(x = voter_x, y = voter_y, color = as.factor(party_id))) +
  geom_point()




