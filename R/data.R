#' Simon Task Trial-Level Data
#'
#' Preprocessed trial-level reaction-time data from a Simon task. The dataset
#' contains the harmonized columns used by the cumulative-trial analyses.
#'
#' @format A tibble with 65,601 rows and 6 variables:
#' \describe{
#'   \item{id}{Integer participant identifier.}
#'   \item{congruence}{Character condition code. `"c"` indicates congruent
#'     trials and `"i"` indicates incongruent trials.}
#'   \item{correct}{Integer accuracy indicator, coded `1` for correct responses
#'     and `0` for incorrect responses.}
#'   \item{acc}{Numeric participant-level accuracy after preprocessing.}
#'   \item{rt}{Numeric reaction time in milliseconds.}
#'   \item{ntrial}{Integer trial index within participant after preprocessing.}
#' }
#'
#' @details
#' The data are harmonized from the raw Simon task files by keeping common
#' variables across tasks, removing participants below the accuracy threshold,
#' and removing reaction-time outliers. Downstream cumulative models use the
#' subset with `correct == 1` and non-missing `congruence`.
#'
#' @source Raw task data in `data/raw/simon-raw.csv`, processed by
#'   `scripts/01-pre-processing.R`.
#'
#' @keywords internal
"simon"

#' SNARC Task Trial-Level Data
#'
#' Preprocessed trial-level reaction-time data from a Spatial Numerical
#' Association of Response Codes (SNARC) task. The dataset contains the
#' harmonized columns used by the cumulative-trial analyses.
#'
#' @format A tibble with 67,615 rows and 6 variables:
#' \describe{
#'   \item{id}{Integer participant identifier.}
#'   \item{congruence}{Character condition code. `"c"` indicates congruent
#'     trials and `"i"` indicates incongruent trials, after correcting the raw
#'     congruence labels.}
#'   \item{correct}{Integer accuracy indicator, coded `1` for correct responses
#'     and `0` for incorrect responses.}
#'   \item{acc}{Numeric participant-level accuracy after preprocessing.}
#'   \item{rt}{Numeric reaction time in milliseconds.}
#'   \item{ntrial}{Integer trial index within participant after preprocessing.}
#' }
#'
#' @details
#' The data are harmonized from the raw SNARC task files by keeping common
#' variables across tasks, correcting the congruence labels, removing
#' participants below the accuracy threshold, and removing reaction-time
#' outliers. Downstream cumulative models use the subset with `correct == 1`
#' and non-missing `congruence`.
#'
#' @source Raw task data in `data/raw/snarc-raw.csv`, processed by
#'   `scripts/01-pre-processing.R`.
#'
#' @keywords internal
"snarc"

#' Task-Switching Trial-Level Data
#'
#' Preprocessed trial-level reaction-time data from a task-switching paradigm.
#' The dataset contains the harmonized columns used by the cumulative-trial
#' analyses.
#'
#' @format A tibble with 64,351 rows and 6 variables:
#' \describe{
#'   \item{id}{Integer participant identifier.}
#'   \item{congruence}{Character condition code derived from the raw switching
#'     variable. `"c"` indicates non-switch trials, `"i"` indicates switch
#'     trials, and `NA` indicates trials without a defined switch condition.}
#'   \item{correct}{Integer accuracy indicator, coded `1` for correct responses
#'     and `0` for incorrect responses.}
#'   \item{acc}{Numeric participant-level accuracy after preprocessing.}
#'   \item{rt}{Numeric reaction time in milliseconds.}
#'   \item{ntrial}{Integer trial index within participant after preprocessing.}
#' }
#'
#' @details
#' The data are harmonized from the raw task-switching files by deriving the
#' analysis condition from the switching variable, keeping common variables
#' across tasks, removing participants below the accuracy threshold, removing
#' participants with inconsistent trial counts, and removing reaction-time
#' outliers. Downstream cumulative models use the subset with `correct == 1`
#' and non-missing `congruence`.
#'
#' @source Raw task data in `data/raw/tswitch-raw.csv`, processed by
#'   `scripts/01-pre-processing.R`.
#'
#' @keywords internal
"tswitch"
