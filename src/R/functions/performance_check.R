#' performance_check
#' @param fit CmdstanR object with profiling
#' @returns plt object graphing the time of the different profiles

performance_check <- function(fit){
  profiles <- fit$profiles()
  profiles <- lapply(1:length(profiles), function(x){
    profiles[[x]] %>%
      mutate(chain = x) %>%
      return(.)
  }) %>%
    do.call("bind_rows", .)

  profiles <- profiles %>%
    arrange(-total_time) %>%
    group_by(name) %>%
    mutate(mean_total_time = mean(total_time))
  profiles <- profiles %>%
    mutate(name = factor(name, levels =
                           profiles %>%
                           distinct(name, mean_total_time) %>%
                           arrange(-mean_total_time) %>%
                           pull(name)))
  plt <- ggplot(profiles, aes(x = name, y = total_time)) +
    geom_point(position = position_dodge(width = 1)) +
    coord_flip() +
    theme_light() +
    theme(axis.title.y = element_blank()) +
    labs(x = "Time (s)",
         caption = "Dots = chains")
  return(plt)
}
