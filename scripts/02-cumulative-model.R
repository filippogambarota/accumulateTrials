# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(broom.mixed)
devtools::load_all()

# Data cleaning ------------------------------------------------------------

clean_task <- function(data) {
  data <- data |>
    # Keep only correct trials
    filter(correct == 1) |>

    # Remove trials without congruence information
    drop_na(congruence)

  # Treat congruence as a factor
  data$congruence <- factor(data$congruence)

  # Sum-to-zero coding scaled as -0.5 / 0.5
  contrasts(data$congruence) <- -contr.sum(2) / 2

  data
}


# Model fitting ------------------------------------------------------------

fit_model <- function(data, calc.derivs = TRUE) {
  lmer(
    rt ~ congruence + (congruence | id),
    data = data,
    control = lmerControl(
      optimizer = "bobyqa",
      calc.derivs = calc.derivs
    )
  )
}

fit_model_log <- function(data, calc.derivs = TRUE) {
  data$lrt <- log(data$rt)

  lmer(
    lrt ~ congruence + (congruence | id),
    data = data,
    control = lmerControl(
      optimizer = "bobyqa",
      calc.derivs = calc.derivs
    )
  )
}

# Model-output helpers -----------------------------------------------------

get_model_params <- function(x) {
  broom.mixed::tidy(x, conf.int = TRUE)
}

get_model_emmeans <- function(x) {
  em <- suppressMessages(
    suppressWarnings(
      emmeans(x, pairwise ~ congruence)
    )
  )

  em <- suppressWarnings(data.frame(em))

  return(em)
}


# Analysis settings --------------------------------------------------------

start <- 32
step <- 5

# Import clean data --------------------------------------------------------

data("simon")
data("snarc")
data("tswitch")

# Combine tasks ------------------------------------------------------------

tasks <- list(
  simon = simon,
  snarc = snarc,
  tswitch = tswitch
)

# Clean task-level datasets ------------------------------------------------

tasks_clean <- lapply(tasks, clean_task)

tasks_clean <- tibble(
  task = names(tasks_clean),
  data = tasks_clean
)

# Cumulative trial datasets ------------------------------------------------

# For each task, create a list of cumulative datasets.
# The first dataset contains `start` trials per participant.
# Each following dataset adds `step` additional trials per participant.

tasks_clean$data_cum <- lapply(
  tasks_clean$data,
  accumulate_trials,
  "id",
  start,
  step
)

# Reshape cumulative datasets ---------------------------------------------

tasks_clean <- tasks_clean |>
  select(-data) |>
  unnest_longer(
    data_cum,
    indices_to = "block",
    values_to = "data"
  )

# Fit cumulative models ----------------------------------------------------

# Fit one mixed-effects model for each task and cumulative-trial block.

tasks_clean$fit <- lapply(tasks_clean$data, fit_model)
tasks_clean$fit_log <- lapply(tasks_clean$data, fit_model_log)

# Extract model summaries --------------------------------------------------

# Fixed and random-effect parameters
tasks_clean$params <- lapply(tasks_clean$fit, get_model_params)
tasks_clean$params_log <- lapply(tasks_clean$fit_log, get_model_params)

# Estimated marginal means and pairwise contrasts
tasks_clean$emmeans <- lapply(tasks_clean$fit, get_model_emmeans)
tasks_clean$emmeans_log <- lapply(tasks_clean$fit_log, get_model_emmeans)

# Westfall-style standardized effect size
tasks_clean$es <- lapply(tasks_clean$fit, westfall_d)

# Add cumulative trial number ----------------------------------------------

tasks_clean <- tasks_clean |>
  mutate(
    trial = block2trial(block, start = start, step = step),
    .by = task
  )

# Save output ---------------------------------------------------------------

tasks_clean_main <- select(tasks_clean, -ends_with("log"))
tasks_clean_log <- select(tasks_clean, -c(emmeans, es, params, fit))

saveRDS(tasks_clean_main, "objects/task_cum.rds")
saveRDS(tasks_clean_log, "objects/task_cum_log.rds")
