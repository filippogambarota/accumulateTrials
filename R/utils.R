#' Create cumulative trial datasets by participant
#'
#' Creates a list of cumulative datasets by progressively adding trials within
#' each participant. For each cumulative trial value, the function keeps the
#' first `m` trials for each participant and combines participants into a single
#' dataset. If a participant has fewer than `m` trials, all available trials for
#' that participant are retained.
#'
#' @param data A data frame containing trial-level data.
#' @param .id Character string. Name of the participant identifier column.
#' @param start Integer. Number of trials to include in the first cumulative
#'   dataset. Default is `32`.
#' @param step Integer. Increment in the number of trials between consecutive
#'   cumulative datasets. Default is `1`.
#' @param .id_keep Optional vector of participant identifiers to retain before
#'   creating cumulative datasets. Default is `NULL`, in which case all
#'   participants are retained.
#'
#' @return A list of data frames. Each element contains the cumulative data up
#'   to a given trial count for all participants.
#'
#' @details
#' The function assumes that `data` contains a trial-ordering variable named
#' `ntrial`. Data are ordered by participant and `ntrial` before accumulation.
#'
#' The output is rebuilt with base `rbind`, so standard columns and factor
#' classes are retained but custom attributes such as manually assigned factor
#' contrasts should be checked or reset before fitting models.
#'
#' @examples
#' dat <- data.frame(
#'   id = rep(1:3, each = 5),
#'   ntrial = rep(1:5, times = 3),
#'   rt = seq_len(15)
#' )
#'
#' dat_cum <- accumulate_trials(data = dat, .id = "id", start = 2, step = 2)
#'
#' @export
accumulate_trials <- function(
  data,
  .id,
  start = 32,
  step = 1,
  .id_keep = NULL
) {
  data <- data[order(data[[.id]], data$ntrial), ]

  if (!is.null(.id_keep)) {
    data <- data[data[[.id]] %in% .id_keep, ]
  }

  data_by_id <- split(data, data[[.id]])
  maxt <- max(sapply(data_by_id, nrow))
  acc <- seq(start, maxt, step)

  if (utils::tail(acc, 1) < maxt) {
    acc <- c(acc, maxt)
  }

  res_acc <- vector(mode = "list", length = length(acc))

  for (i in 1:length(acc)) {
    res_by_id <- vector(mode = "list", length = length(data_by_id))
    for (j in 1:length(res_by_id)) {
      cd <- data_by_id[[j]]
      cr <- nrow(cd)
      if (cr < acc[i]) {
        end <- cr
      } else {
        end <- acc[i]
      }
      res_by_id[[j]] <- cd[1:end, ]
    }
    res_by_id <- do.call(rbind, res_by_id)
    res_acc[[i]] <- res_by_id
  }

  return(res_acc)
}

#' Convert block indices to cumulative trial numbers
#'
#' Converts a sequence of block indices into cumulative trial counts given an
#' initial block size and a fixed step size for subsequent blocks.
#'
#' @param x Integer vector of block indices.
#' @param start Integer. Number of trials in the first block. Default is `32`.
#' @param step Integer. Number of additional trials added after the first block.
#'   Default is `5`.
#' @param max_trial Optional integer. Maximum cumulative trial count. When
#'   supplied, returned trial counts are capped at this value. This is useful
#'   when the final cumulative dataset contains all remaining trials but the
#'   maximum number of trials is not exactly on the `start + k * step` sequence.
#'
#' @return An integer vector of cumulative trial counts.
#'
#' @details
#' The function maps block `1` to `start`, block `2` to `start + step`, and so
#' on. If `max_trial` is provided, values larger than `max_trial` are replaced
#' with `max_trial`.
#'
#' @examples
#' block2trial(1:5, start = 32, step = 5)
#' block2trial(1:5, start = 32, step = 5, max_trial = 49)
#'
#' @export
block2trial <- function(x, start = 32, step = 5, max_trial = NULL) {
  trials <- start + (as.integer(x) - 1L) * step

  if (!is.null(max_trial)) {
    trials <- pmin(trials, max_trial)
  }

  as.integer(trials)
}

#' Compute a Westfall-style standardized effect size from a mixed model
#'
#' Computes a standardized fixed effect by dividing a selected fixed-effect
#' estimate by the square root of the total model variance. The total variance
#' is defined as the sum of the random-effect variance and the residual
#' variance extracted with [insight::get_variance()].
#'
#' @param model A fitted mixed-effects model, typically of class `lmerMod`.
#' @param term Character string. Name of the fixed-effect term to standardize.
#'   Default is `"condi"`, the condition-effect term used by the cumulative
#'   model objects produced in `scripts/02-cumulative-model.R`.
#' @param c Numeric. Contrast value used to weight the random-slope variance.
#'   Default is `0.5`, matching the centered two-condition coding used in this
#'   project.
#'
#' @return A data frame with one row and the following columns:
#' \describe{
#'   \item{term}{Name of the fixed-effect term.}
#'   \item{beta}{Fixed-effect estimate for `term`.}
#'   \item{var_random}{Random-effect variance returned by
#'     [insight::get_variance()].}
#'   \item{var_residual}{Residual variance returned by
#'     [insight::get_variance()].}
#'   \item{total_var}{Sum of random-effect and residual variance.}
#'   \item{d}{Standardized effect size.}
#' }
#'
#' @details
#' This function is intended for mixed-effects models in which the fixed effect
#' and variance components are on the same response scale. For reaction-time
#' models fitted on raw RTs, the resulting standardized effect is expressed as
#' the fixed effect divided by the model-implied total standard deviation.
#' This follows the variance-component standardization described by Westfall,
#' Kenny, and Judd (2014; BibTeX key: `Westfall2014-im`).
#'
#' The exact interpretation of `var_random` depends on the random-effects
#' structure of the fitted model and on how [insight::get_variance()] summarizes
#' the model's random-effect variance.
#'
#' @references
#' Westfall, J., Kenny, D. A., & Judd, C. M. (2014). Statistical power and
#' optimal design in experiments in which samples of participants respond to
#' samples of stimuli. *Journal of Experimental Psychology: General*, 143,
#' 2020-2045. https://doi.org/10.1037/xge0000014. BibTeX key:
#' `Westfall2014-im`.
#'
#' @examples
#' set.seed(1)
#' n_id <- 20
#' n_rep <- 8
#' dat <- expand.grid(
#'   id = factor(seq_len(n_id)),
#'   rep = seq_len(n_rep),
#'   cond = factor(c("c", "i"))
#' )
#' id_intercept <- rnorm(n_id, 0, 50)
#' id_slope <- rnorm(n_id, 30, 15)
#' dat$rt <- 600 + id_intercept[dat$id] +
#'   ifelse(dat$cond == "i", 40 + id_slope[dat$id], 0) +
#'   rnorm(nrow(dat), 0, 30)
#'
#' fit <- lme4::lmer(
#'   rt ~ cond + (cond | id),
#'   data = dat
#' )
#'
#' westfall_d(fit)
#'
#' @export
westfall_d <- function(model, term = "condi", c = 0.5) {
    b <- lme4::fixef(model)[term]
    V <- insight::get_variance(model)
    VP <- V$var.intercept
    VPxC <- V$var.slope
    VE <- V$var.residual
    VT <- VP + VPxC*c^2 + VE
    d <- b / sqrt(VT)
    data.frame(
        term = term,
        b = unname(b),
        d = unname(d),
        VP = unname(VP),
        VPxC = unname(VPxC),
        VE = unname(VE),
        VT = unname(VT)
    )
}

#' Check convergence and singularity of a mixed-effects model
#'
#' Checks whether a fitted `lme4` mixed-effects model converged and whether the
#' fitted model is singular. The function returns a compact diagnostic summary
#' that can be appended to model-output tables.
#'
#' @param mm A fitted mixed-effects model of class `merMod`, such as an object
#'   returned by [lme4::lmer()] or [lme4::glmer()].
#' @param tol Numeric. Tolerance used by [lme4::isSingular()] to assess
#'   singularity. Default is `1e-4`.
#'
#' @return A tibble with one row and three columns:
#' \describe{
#'   \item{converged}{Logical. `TRUE` if no convergence messages are present in
#'     `mm@optinfo$conv$lme4`.}
#'   \item{singular}{Logical. `TRUE` if the model is singular according to
#'     [lme4::isSingular()].}
#'   \item{status}{Character. One of `"ok"`, `"converged_singular"`,
#'     `"not_converged"`, or `"not_converged_singular"`.}
#' }
#'
#' @details
#' The convergence check is based on the `lme4` convergence information stored
#' in `mm@optinfo$conv$lme4`. A model is treated as converged when this object is
#' `NULL` or contains no messages after unlisting.
#'
#' Singularity is evaluated separately with [lme4::isSingular()]. Therefore, a
#' model can be classified as converged but singular.
#'
#' @examples
#' set.seed(1)
#' n_id <- 20
#' n_rep <- 8
#' dat <- expand.grid(
#'   id = factor(seq_len(n_id)),
#'   rep = seq_len(n_rep),
#'   cond = factor(c("c", "i"))
#' )
#' id_intercept <- rnorm(n_id, 0, 50)
#' id_slope <- rnorm(n_id, 30, 15)
#' dat$rt <- 600 + id_intercept[dat$id] +
#'   ifelse(dat$cond == "i", 40 + id_slope[dat$id], 0) +
#'   rnorm(nrow(dat), 0, 30)
#'
#' fit <- lme4::lmer(
#'   rt ~ cond + (cond | id),
#'   data = dat
#' )
#'
#' check_lmer_fit(fit)
#'
#' @export
check_lmer_fit <- function(mm, tol = 1e-4) {
  if (!inherits(mm, "merMod")) {
    stop("Error: must pass a merMod object")
  }

  conv_msg <- mm@optinfo$conv$lme4
  converged <- is.null(conv_msg) || length(unlist(conv_msg)) == 0
  singular <- lme4::isSingular(mm, tol = tol)

  status <- dplyr::case_when(
    converged & !singular ~ "ok",
    converged & singular ~ "converged_singular",
    !converged & !singular ~ "not_converged",
    !converged & singular ~ "not_converged_singular"
  )

  tibble::tibble(
    converged = converged,
    singular = singular,
    status = status
  )
}
