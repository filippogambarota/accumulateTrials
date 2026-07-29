#' Simon Task Trial-Level Data
#'
#' Preprocessed trial-level reaction-time data from a Simon task. The dataset
#' contains the harmonized columns used by the cumulative-trial analyses.
#'
#' @format A tibble with 65,073 rows and 7 variables:
#' \describe{
#'   \item{id}{Integer participant identifier.}
#'   \item{congruence}{Character congruence code from the raw `Congruence`
#'     column. `"c"` indicates congruent trials and `"i"` indicates
#'     incongruent trials.}
#'   \item{cond}{Character analysis condition. For the Simon task, this is a
#'     copy of `congruence`, with `"c"` indicating congruent trials and `"i"`
#'     indicating incongruent trials.}
#'   \item{correct}{Integer accuracy indicator, coded `1` for correct responses
#'     and `0` for incorrect responses.}
#'   \item{acc}{Numeric participant-level accuracy after preprocessing.}
#'   \item{rt}{Numeric reaction time in milliseconds.}
#'   \item{ntrial}{Integer cumulative trial index within participant, created
#'     after removing participants with anomalous original trial counts and
#'     before reaction-time filtering.}
#' }
#'
#' @details
#' The data are harmonized from the raw Simon task files by keeping common
#' variables across tasks, removing participants below the accuracy threshold,
#' and removing reaction-time outliers. Downstream cumulative models use the
#' subset with `correct == 1` and non-missing `cond`.
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
#' @format A tibble with 68,576 rows and 7 variables:
#' \describe{
#'   \item{id}{Integer participant identifier.}
#'   \item{congruence}{Character congruence code from the raw `Congruence`
#'     column. The raw SNARC labels are inverted relative to the analysis
#'     condition.}
#'   \item{cond}{Character analysis condition after correcting the inverted raw
#'     SNARC congruence labels. `"c"` indicates congruent trials and `"i"`
#'     indicates incongruent trials.}
#'   \item{correct}{Integer accuracy indicator, coded `1` for correct responses
#'     and `0` for incorrect responses.}
#'   \item{acc}{Numeric participant-level accuracy after preprocessing.}
#'   \item{rt}{Numeric reaction time in milliseconds.}
#'   \item{ntrial}{Integer cumulative trial index within participant, created
#'     before reaction-time filtering.}
#' }
#'
#' @details
#' The data are harmonized from the raw SNARC task files by keeping common
#' variables across tasks, correcting the congruence labels, removing
#' participants below the accuracy threshold, and removing reaction-time
#' outliers. Downstream cumulative models use the subset with `correct == 1`
#' and non-missing `cond`.
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
#' @format A tibble with 65,844 rows and 7 variables:
#' \describe{
#'   \item{id}{Integer participant identifier.}
#'   \item{congruence}{Character congruence code from the raw `Congruence`
#'     column, with values `"C"` and `"I"` and missing values for trials without
#'     a recorded congruence code. This column is retained from the raw data but
#'     is not the condition used in the cumulative models.}
#'   \item{cond}{Character analysis condition derived from the raw `Switch`
#'     column. `"c"` indicates non-switch trials (`Switch == 0`), `"i"`
#'     indicates switch trials (`Switch == 1`), and `NA` indicates trials
#'     without a defined switch condition.}
#'   \item{correct}{Integer accuracy indicator, coded `1` for correct responses
#'     and `0` for incorrect responses.}
#'   \item{acc}{Numeric participant-level accuracy after preprocessing.}
#'   \item{rt}{Numeric reaction time in milliseconds.}
#'   \item{ntrial}{Integer cumulative trial index within participant, created
#'     after removing participants with anomalous original trial counts and
#'     before reaction-time filtering.}
#' }
#'
#' @details
#' The data are harmonized from the raw task-switching files by deriving the
#' analysis condition from the switching variable, keeping common variables
#' across tasks, removing participants below the accuracy threshold, removing
#' participants with inconsistent trial counts, and removing reaction-time
#' outliers. Downstream cumulative models use the subset with `correct == 1`
#' and non-missing `cond`.
#'
#' @source Raw task data in `data/raw/tswitch-raw.csv`, processed by
#'   `scripts/01-pre-processing.R`.
#'
#' @keywords internal
"tswitch"
