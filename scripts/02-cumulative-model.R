# Fit cumulative mixed-effects models across increasing trial counts.
#
# Inputs:  data/simon.rda, data/snarc.rda, data/tswitch.rda
# Outputs: objects/task_cum*.rds and shiny/data*.rds
# Run after: scripts/01-pre-processing.R

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lme4)
library(emmeans)
library(broom.mixed)
library(pbapply)
devtools::load_all()

lapply <- pblapply

# Data cleaning ------------------------------------------------------------

clean_task <- function(data) {
  data <- data |>
    # Keep only correct trials
    filter(correct == 1) |>

    # Remove trials without congruence information
    drop_na(cond)

  # Treat congruence/switch as a factor
  data$cond <- factor(data$cond)

  # Sum-to-zero coding scaled as -0.5 / 0.5
  contrasts(data$cond) <- -contr.sum(2) / 2

  data
}


# Model fitting ------------------------------------------------------------

fit_model <- function(data, calc.derivs = TRUE) {
  lmer(
    rt ~ cond + (cond | id),
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
    lrt ~ cond + (cond | id),
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
      emmeans(x, pairwise ~ cond)
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

tasks_clean$fit <- pblapply(tasks_clean$data, fit_model)
tasks_clean$fit_log <- pblapply(tasks_clean$data, fit_model_log)

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
    trial = block2trial(
      block,
      start = start,
      step = step,
      max_trial = max(vapply(
        data,
        function(x) max(tabulate(match(x$id, unique(x$id)))),
        integer(1)
      ))
    ),
    .by = task
  )

# Cumulative model by-subject ---------------------------------------------

tasks_clean_by_subj <- tasks_clean |>
    select(task, data, trial) |>
    unnest(data) |>
    group_by(task, id, trial) |>
    nest() |>
    ungroup()

tasks_clean_by_subj$fit <- pbapply::pblapply(tasks_clean_by_subj$data, function(x){
    fit <- lm(rt ~ cond, data = x, contrasts = list(cond = -contr.sum(2)/2))
    res <- tidy(fit, conf.int = TRUE)
    add_row(res, term = "sigma", estimate = sigma(fit))
})


# Save output ---------------------------------------------------------------

tasks_clean_main <- select(tasks_clean, -ends_with("log"))
tasks_clean_log <- select(tasks_clean, -c(emmeans, es, params, fit))

saveRDS(tasks_clean_main, "objects/task_cum.rds")
saveRDS(tasks_clean_log, "objects/task_cum_log.rds")
saveRDS(select(tasks_clean_main, trial, task, params, emmeans, es), "shiny/data.rds")
saveRDS(tasks_clean_by_subj, "objects/task_cum_by_subj.rds")
