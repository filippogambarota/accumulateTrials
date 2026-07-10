# Packages ----------------------------------------------------------------

library(here)
library(lme4)
library(furrr)
library(future)
library(broom.mixed)
library(purrr)
library(dplyr)
devtools::load_all()

# Reproducibility ----------------------------------------------------------

set.seed(1234)

# Resampling settings ------------------------------------------------------

# Number of resampling iterations for each task x block x N combination
B <- 5000

# Participant sample sizes to evaluate
N_vec <- c(15, 50, 100)

# Model fitting ------------------------------------------------------------

fit_model <- function(data, calc.derivs = FALSE) {
  lmer(
    rt ~ congruence + (congruence | id),
    data = data,
    contrasts = list(congruence = -contr.sum(2) / 2),
    control = lmerControl(
      optimizer = "bobyqa",
      calc.derivs = calc.derivs
    )
  )
}

# Subsampling --------------------------------------------------------------

subsample_data <- function(data, n) {
  ids <- unique(data$id)

  # Avoid requesting more participants than available
  if (n > length(ids)) {
    n <- length(ids)
  }

  # Sample participants without replacement
  ids_sub <- sample(ids, n, replace = FALSE)

  # Keep all trials from selected participants
  data <- data[data$id %in% ids_sub, , drop = FALSE]

  # Preserve participant and trial order
  data[order(data$id, data$ntrial), ]
}


# Model-output extraction --------------------------------------------------

get_res <- function(fit) {
  # Extract fixed and random-effect estimates
  res <- broom.mixed::tidy(fit)

  # Add convergence and singularity diagnostics
  diag <- check_lmer_fit(fit)

  bind_cols(
    res,
    diag[rep(1, nrow(res)), ]
  )
}


# Safe model fitting -------------------------------------------------------

safe_fit <- purrr::possibly(fit_model, otherwise = NULL)


# Single resampling iteration ----------------------------------------------

do_one_resample <- function(data, n) {
  # Select n participants
  di <- subsample_data(data, n)

  # Fit model safely
  fit <- safe_fit(di)

  # Return structured missing output if model fitting fails
  if (is.null(fit)) {
    return(tibble(
      effect = NA_character_,
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      df = NA_real_,
      p.value = NA_real_,
      converged = FALSE,
      singular = NA,
      status = "error"
    ))
  }

  get_res(fit)
}


# Repeated resampling for one condition ------------------------------------

do_resampling <- function(data, B, n) {
  res <- lapply(seq_len(B), function(b) {
    out <- do_one_resample(data, n)
    out$boot <- b
    out
  })

  dplyr::bind_rows(res)
}

# Import cumulative task datasets -----------------------------------------

tasks <- readRDS("objects/task_cum.rds")

# Create resampling design -------------------------------------------------

# Each row corresponds to one task x cumulative-trial block x N combination.
tasks_boot <- tasks |>
  select(task, block, trial, data) |>
  tidyr::expand_grid(N = N_vec)

# Optional quick test subset
# tasks_boot <- tasks_boot[1:10, ]

# Parallel resampling ------------------------------------------------------

if (!interactive()) {
  plan(multisession, workers = 70)
}

tasks_boot$res <- future_pmap(
  tasks_boot,
  ~ do_resampling(data = ..4, B = B, n = ..5),
  .options = furrr_options(seed = TRUE)
)

if (!interactive()) {
  plan(sequential)
}


# Save output --------------------------------------------------------------

saveRDS(tasks_boot, "objects/task_resampling.rds")
