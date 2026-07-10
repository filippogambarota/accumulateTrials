#' Split a vector of trials into fixed-size blocks
#'
#' Assigns each element of a vector to a block of size `k`. The last block may
#' contain fewer than `k` elements if the length of `x` is not an exact multiple
#' of `k`.
#'
#' @param x A vector. Usually a vector of trial indices or observations.
#' @param k Integer. The number of elements per block.
#'
#' @return An integer vector of the same length as `x`, giving the block number
#'   for each element.
#'
#' @examples
#' split_trials(1:10, k = 3)
#'
#' @export
split_trials <- function(x, k) {
  n <- ceiling(length(x) / k)
  parts <- rep(1:n, each = k)
  parts[1:length(x)]
}


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
#' @examples
#' dat_cum <- accumulate_trials(data = dat, .id = "id", start = 32, step = 5)
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
#' @param x A vector of block indices. Only the length of `x` is used.
#' @param start Integer. Number of trials in the first block. Default is `32`.
#' @param step Integer. Number of additional trials added after the first block.
#'   Default is `5`.
#'
#' @return An integer vector of cumulative trial counts.
#'
#' @examples
#' block2trial(1:5, start = 32, step = 5)
#'
#' @export
block2trial <- function(x, start = 32, step = 5) {
  tt <- c(start, rep(step, length(x) - 1))
  cumsum(tt)
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
#'   Default is `"congruence1"`.
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
#'
#' The exact interpretation of `var_random` depends on the random-effects
#' structure of the fitted model and on how [insight::get_variance()] summarizes
#' the model's random-effect variance.
#'
#' @examples
#' fit <- lme4::lmer(
#'   rt ~ congruence + (congruence | id),
#'   data = dat,
#'   contrasts = list(congruence = contr.sum(2) / 2)
#' )
#'
#' westfall_d(fit, term = "congruence1")
#'
#' @export
westfall_d <- function(model, term = "congruencei") {
  beta <- lme4::fixef(model)[term]

  vv <- insight::get_variance(model)

  total_var <- vv$var.random + vv$var.residual

  d <- beta / sqrt(total_var)

  data.frame(
    term = term,
    beta = unname(beta),
    var_random = unname(vv$var.random),
    var_residual = unname(vv$var.residual),
    total_var = unname(total_var),
    d = unname(d)
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
#' fit <- lme4::lmer(
#'   rt ~ congruence + (congruence | id),
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
