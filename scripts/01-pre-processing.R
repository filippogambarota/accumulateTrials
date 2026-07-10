# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

# General preprocessing function ------------------------------------------

pre_processing <- function(data, out_rt = Inf, out_trials = Inf, acc_th = 0.8) {
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
    filter(rt < out_rt) |>

    # Compute participant accuracy
    group_by(id) |>
    mutate(acc = mean(Correct)) |>
    ungroup() |>

    # Remove participants with low accuracy
    filter(acc >= acc_th) |>

    # Keep common columns across tasks
    select(
      id,
      Congruence,
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

# In task switching, congruence is derived from the Switch variable.
# Switch == 1 is recoded as "c"; Switch == 0 is recoded as "i".
tswitch$Congruence <- ifelse(tswitch$Switch == 1, "i", "c")

# The SNARC has an error in the labels of congruent and incogruent
# we simply have to switch the labels
snarc$Congruence <- ifelse(snarc$Congruence == "i", "c", "i")

# Apply preprocessing ------------------------------------------------------

simon_clean <- pre_processing(
  simon,
  out_rt = 1200,
  out_trials = 320
)

snarc_clean <- pre_processing(
  snarc,
  out_rt = 1200
)

tswitch_clean <- pre_processing(
  tswitch,
  out_rt = 1200,
  out_trials = 330
)

# Save clean data ----------------------------------------------------------

saveRDS(simon_clean, "data/simon.rds")
saveRDS(snarc_clean, "data/snarc.rds")
saveRDS(tswitch_clean, "data/tswitch.rds")
