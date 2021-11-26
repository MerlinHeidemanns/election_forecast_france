


cutpoints <- c(-2,-1,0,1,2)
df <- lapply(seq(-3, 3, length.out = 100), function(x){
  prob <- c()
  prob <- c(prob,
            1 - pnorm(x - cutpoints[1]))
  for (j in 2:length(cutpoints)){
    prob <- c(prob,
              pnorm(x - cutpoints[j - 1]) - pnorm(x - cutpoints[j]))
  }
  prob <- c(prob,
            pnorm(x - cutpoints[length(cutpoints)]))
  data.frame(prob = prob,
             x = x,
             i = 1:length(prob)) %>%
    return()
}) %>%
  do.call("bind_rows", .)

ggplot(df, aes(x = x, y = prob, color = as.factor(i))) +
  geom_line()