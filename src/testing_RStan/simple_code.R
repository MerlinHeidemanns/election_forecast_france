
NPolls <- 30
T <- 100
NElections <- 3

tElections <- c(1, 1:100[1:100%%50 == 0]))
tPolls <- sample(1:100[!1:100 %in% tElections], NPolls, replace = TRUE)
nPolls <- rep(1000, NPolls)
