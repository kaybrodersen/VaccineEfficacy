# Simple inference on the efficacy of a vaccine given trial results.
# Author: khbrodersen@gmail.com.

suppressPackageStartupMessages({
  library(boot)
  library(dplyr)
})

LoadData <- function() {
  # Returns trial data.
  #
  # Returns:
  #   A data frame with one row per subject and columns:
  #     treated (logical), infected (logical).

  data <- data.frame(
    treated = c(rep(TRUE, 19478),
                rep(FALSE, 19478)),
    infected = c(rep(TRUE, 9),
                 rep(FALSE, 19469),
                 rep(TRUE, 85),
                 rep(FALSE, 19393))
  )
  return(data)
}

CalculateEffect <- function(data, indices = seq(nrow(data))) {
  # Calculates the efficacy based on the subset indicated by `indices`.
  #
  # Args:
  #   data: A data frame with one row per subject and columns:
  #     treated (logical), infected (logical).
  #  indices: Row indices for subsetting.
  #
  # Returns:
  #   Efficacy on the desired subset, estimated as:
  #   P(infected | treated) / P(infected | not treated) - 1.

  subset <- data[indices, ]
  effect <- sum(subset$treated & subset$infected) /
    sum(subset$treated) *
    sum(!subset$treated) /
    sum(!subset$treated & subset$infected) - 1
  return(effect)
}

Main <- function() {
  set.seed(42)
  inference <- LoadData() %>%
    boot::boot(CalculateEffect, R = 100, parallel = "multicore") %>%
    boot::boot.ci(type = "basic")
  summary <- c(
    effect = inference$t0,
    lower = inference$basic[4],
    upper = inference$basic[5]
  )
  return(summary)
}

if (sys.nframe() == 0L) {
  Main()
}
