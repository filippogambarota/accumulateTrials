# Pre-process raw behavioral data for the three tasks.
#
# Inputs:  data/raw/*-raw.csv
# Outputs: data/simon.rda, data/snarc.rda, data/tswitch.rda
# Run before: scripts/02-cumulative-model.R

# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
devtools::load_all()

# General preprocessing function ------------------------------------------

pre_processing <- function(data, min_rt = 0, max_rt = Inf, out_trials = Inf, acc_th = 0.8) {
  out <- data |>

    # Standardize reaction-time column name
    rename(rt = Reaction.Time) |>

    # Count original number of trials per participant
    group_by(id) |>
    mutate(nt_original = n()) |>
    ungroup() |>

    # Remove participants with anomalous number of trials
    filter(nt_original <= out_trials) |>

    # Create cumulative trial index within participant
    arrange(id, Trial.Number) |>
    group_by(id) |>
    mutate(ntrial = row_number()) |>
    ungroup() |>

    # Remove RT outliers
    filter(rt <= max_rt & rt >= min_rt) |>

    # Compute participant accuracy
    group_by(id) |>
    mutate(acc = mean(Correct)) |>
    ungroup() |>

    # Remove participants with low accuracy
    filter(acc >= acc_th) |>

    # Keep common columns across tasks
    select(
          id,
          any_of("Congruence"),
          cond,
          correct = Correct,
          acc,
          rt,
          ntrial
      )

  names(out) <- tolower(names(out))
  out
}


# Import raw data ----------------------------------------------------------

simon <- read.csv("data/raw/simon-raw.csv")
snarc <- read.csv("data/raw/snarc-raw.csv")
tswitch <- read.csv("data/raw/tswitch-raw.csv")

# Harmonize congruence variable -------------------------------------------

simon$cond <- simon$Congruence

# In task switching, the analysis condition is derived from the Switch variable.
# Switch == 1 is recoded as "i"; Switch == 0 is recoded as "c".
tswitch$cond <- ifelse(tswitch$Switch == 1, "i", "c")

# The SNARC has an error in the labels of congruent and incongruent
# trials, so we switch the labels.
snarc$cond <- ifelse(snarc$Congruence == "i", "c", "i")

# Apply preprocessing ------------------------------------------------------

simon_clean <- pre_processing(
  simon,
  min_rt = 150,
  max_rt = 1500,
  out_trials = 320
)

snarc_clean <- pre_processing(
  snarc,
  min_rt = 150,
  max_rt = 1500,
  out_trials = 320
)

tswitch_clean <- pre_processing(
  tswitch,
  min_rt = 150,
  max_rt = 1500,
  out_trials = 330
)

# Save clean data ----------------------------------------------------------

simon <- simon_clean
snarc <- snarc_clean
tswitch <- tswitch_clean

save(simon, file = "data/simon.rda")
save(snarc, file = "data/snarc.rda")
save(tswitch, file = "data/tswitch.rda")
