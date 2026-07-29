# Shared plotting helpers for paper.qmd and supplementary.qmd.

task_labels <- c(
  simon = "Simon",
  snarc = "SNARC",
  tswitch = "Task-switching"
)

condition_labels <- function(task, cond) {
  dplyr::case_when(
    task == "tswitch" & cond == "i" ~ "Switch",
    task == "tswitch" & cond == "c" ~ "Non-switch",
    cond == "i" ~ "Incongruent",
    cond == "c" ~ "Congruent",
    TRUE ~ as.character(cond)
  )
}

# Common plot settings ------------------------------------------------------

plot_base_size <- 11

plot_lwd <- 0.65
plot_lwd_strong <- 0.80
plot_lwd_ref <- 0.45

point_size <- 1.25
point_alpha <- 0.35
line_alpha <- 0.85
ribbon_alpha <- 0.22

common_theme <- function(base_size = plot_base_size) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      axis.text = ggplot2::element_text(size = base_size - 1),
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      strip.background = ggplot2::element_rect(fill = "grey92", color = NA),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 2,
        hjust = 0.5,
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size,
        hjust = 0.5,
        margin = ggplot2::margin(b = 8)
      ),
      plot.caption = ggplot2::element_text(size = base_size - 2, hjust = 0),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.25),
      panel.spacing = grid::unit(1.1, "lines"),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
}

set_paper_theme <- function() {
  ggplot2::theme_set(common_theme())
  ggplot2::update_geom_defaults(
    "line",
    list(linewidth = plot_lwd, alpha = line_alpha)
  )
  ggplot2::update_geom_defaults("smooth", list(linewidth = plot_lwd_strong))
  ggplot2::update_geom_defaults(
    "point",
    list(size = point_size, alpha = point_alpha)
  )
  ggplot2::update_geom_defaults(
    "ribbon",
    list(alpha = ribbon_alpha, colour = NA)
  )
  ggplot2::update_geom_defaults(
    "boxplot",
    list(linewidth = plot_lwd_ref, outlier.size = 1.1)
  )
}

geom_paper_line <- function(
    ...,
    linewidth = plot_lwd_strong,
    alpha = line_alpha) {
  ggplot2::geom_line(..., linewidth = linewidth, alpha = alpha)
}

geom_paper_ribbon <- function(..., alpha = ribbon_alpha) {
  ggplot2::geom_ribbon(..., alpha = alpha, colour = NA)
}

geom_paper_hline <- function(yintercept = 0, ...) {
  ggplot2::geom_hline(
    yintercept = yintercept,
    linewidth = plot_lwd_ref,
    linetype = "dashed",
    ...
  )
}

geom_paper_point <- function(..., size = point_size, alpha = point_alpha) {
  ggplot2::geom_point(..., size = size, alpha = alpha)
}

term_titles <- list(
  "(Intercept)" = latex2exp::TeX("$\\beta_0$"),
  "congruence1" = latex2exp::TeX("$\\beta_1$"),
  "congruencei" = latex2exp::TeX("$\\beta_1$"),
  "cond1" = latex2exp::TeX("$\\beta_1$"),
  "condi" = latex2exp::TeX("$\\beta_1$"),
  "sd__(Intercept)" = latex2exp::TeX("$\\sigma_P$"),
  "sd__congruence1" = latex2exp::TeX("$\\sigma_{P \\times C}$"),
  "sd__congruencei" = latex2exp::TeX("$\\sigma_{P \\times C}$"),
  "sd__cond1" = latex2exp::TeX("$\\sigma_{P \\times C}$"),
  "sd__condi" = latex2exp::TeX("$\\sigma_{P \\times C}$"),
  "sd__Observation" = latex2exp::TeX("$\\sigma_E$"),
  "sigma" = latex2exp::TeX("$\\sigma_E$")
)

term_facet_labeller <- ggplot2::as_labeller(
  vapply(term_titles, function(x) deparse(x[[1]]), character(1)),
  default = ggplot2::label_parsed
)

cum_plot <- function(data, x_limits = NULL, title = NULL) {
  data <- data |>
    dplyr::mutate(
      conf.low = dplyr::if_else(is.na(conf.low), estimate, conf.low),
      conf.high = dplyr::if_else(is.na(conf.high), estimate, conf.high)
    )

  ggplot2::ggplot(data, ggplot2::aes(x = trial, y = estimate)) +
    geom_paper_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high)) +
    geom_paper_line() +
    ggplot2::scale_x_continuous(limits = x_limits) +
    ggplot2::labs(title = title, x = "Cumulative trial", y = NULL)
}

emm_limits <- function(data) {
  lower_name <- intersect(c("asymp.LCL", "lower.CL", "conf.low"), names(data))[1]
  upper_name <- intersect(c("asymp.UCL", "upper.CL", "conf.high"), names(data))[1]

  list(lower = lower_name, upper = upper_name)
}

plot_cumulative_task <- function(data, task_id, task_title = task_labels[[task_id]]) {
  cum_dat <- data |>
    dplyr::filter(task == task_id) |>
    tidyr::unnest(params) |>
    dplyr::filter(!stringr::str_detect(term, "^cor__"))

  x_limits <- range(cum_dat$trial, na.rm = TRUE)
  emm_names <- names(data$emmeans[[which(lengths(data$emmeans) > 0)[1]]])
  cond_col <- intersect(c("congruence", "cond"), emm_names)[1]

  emm_dat <- data |>
    dplyr::filter(task == task_id) |>
    tidyr::unnest(emmeans) |>
    dplyr::mutate(
      condition = condition_labels(task, .data[[cond_col]]),
      condition = factor(
        condition,
        levels = c("Congruent", "Incongruent", "Non-switch", "Switch")
      )
    ) |>
    dplyr::filter(contrast != "c - i")

  ci_names <- emm_limits(emm_dat)

  p_emm <- ggplot2::ggplot(
    emm_dat,
    ggplot2::aes(x = trial, y = emmean, color = condition, fill = condition)
  ) +
    geom_paper_ribbon(
      ggplot2::aes(ymin = .data[[ci_names$lower]], ymax = .data[[ci_names$upper]]),
      alpha = ribbon_alpha
    ) +
    geom_paper_line() +
    ggplot2::scale_x_continuous(limits = x_limits) +
    ggplot2::labs(title = NULL, x = "Cumulative trial", y = NULL) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(
        fill = scales::alpha("white", 0.75),
        color = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = scales::alpha("white", 0.75),
        color = NA
      )
    )

  make_term_plot <- function(term_candidates) {
    term_present <- intersect(term_candidates, unique(cum_dat$term))[1]

    if (is.na(term_present)) {
      return(patchwork::plot_spacer())
    }

    title <- term_titles[[term_present]]
    if (is.null(title)) title <- term_present

    cum_dat |>
      dplyr::filter(term == term_present) |>
      cum_plot(x_limits = x_limits, title = title)
  }

  plot_list <- list(
    make_term_plot("(Intercept)"),
    make_term_plot(c("congruence1", "congruencei", "cond1", "condi")),
    p_emm,
    make_term_plot("sd__(Intercept)"),
    make_term_plot(c("sd__congruence1", "sd__congruencei", "sd__cond1", "sd__condi")),
    make_term_plot(c("sd__Observation", "sigma"))
  )

  patchwork::wrap_plots(plot_list, ncol = 3) +
    patchwork::plot_layout(axes = "collect", axis_titles = "collect") +
    patchwork::plot_annotation(title = task_title)
}
