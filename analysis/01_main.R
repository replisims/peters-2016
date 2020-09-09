# Code pertaining to the replication attempt of
# Peters et al 2006

# Define simulation parameters as specified by Peters et al 2006 ----------------


# Fixed design parameters -------------------------------------------------

n_iter <- 1000

bias_table <- list(
  moderate =  list(
    p_table = c(0.05, 0.2, 0.5, 1),
    sec_table = c(1, 0.75, 0.5, 0.25)
  ),
  severe = list(
    p_table = c(0.05, 0.2, 1),
    sec_table = c(1, 0.75, 0.25)
  )
)

prob_cg_distr <- function() {
  runif(1, min = 0.3, max = 0.7)
}
n_cg_distr <-
  function() {
    exp(rnorm(1, mean = 5, sd = sqrt(0.3))) %>% round()
  }

# Variable design parameters ----------------------------------------------

bias_type <- c("p", "es")
bias_percentage <- c(0, .14, .40)
bias_strength <- c("moderate", "severe")
odds_ratio <- c(1, 1.2, 1.5, 3, 5) #5
heterogeneity <- c(0, 0.2, 1.5, 5) #4
ma_size <- c(6, 16, 30, 90) #4

# Compile dataframe with scenarios ----------------------------------------

scenarios <- compile_scenarios(
  bias_type = bias_type,
  bias_percentage = bias_percentage,
  bias_strength = bias_strength,
  odds_ratio = odds_ratio,
  heterogeneity = heterogeneity,
  ma_size = ma_size,
  prob_cg_distr = prob_cg_distr,
  n_cg_distr = n_cg_distr,
  bias_table = bias_table
)

save(scenarios, file = "data/scenarios.rda")

# Run simulation ----------------------------------------------------------

library(tidyverse)

# Run in batches of 100 replications at a time
1:10 %>% walk( ~{sim_data <-
  run_sim(iteration_range = ((.x - 1)*100 + 1) :(.x*100),
          scenarios = scenarios)

  saveRDS(sim_data, file = paste0("analysis/data/sim_data", .x, ".rds"))
  })


