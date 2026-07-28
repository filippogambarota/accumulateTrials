library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
}

find_data_file <- function() {
    candidates <- c("data.rds", file.path("shiny", "data.rds"))
    existing <- candidates[file.exists(candidates)]

    if (length(existing) == 0) {
        stop("Cannot find data.rds. Run the app from shiny/ or from the project root.")
    }

    existing[[1]]
}

flatten_list_column <- function(data, column) {
    pieces <- lapply(seq_len(nrow(data)), function(i) {
        item <- data[[column]][[i]]

        if (is.null(item) || NROW(item) == 0) {
            return(NULL)
        }

        cbind(
            task = data$task[[i]],
            trial = data$trial[[i]],
            as.data.frame(item, stringsAsFactors = FALSE)
        )
    })

    bind_rows(pieces)
}

condition_labels <- function(task, congruence) {
    case_when(
        task == "tswitch" & congruence == "i" ~ "Switch",
        task == "tswitch" & congruence == "c" ~ "Non-switch",
        congruence == "i" ~ "Incongruent",
        congruence == "c" ~ "Congruent",
        TRUE ~ as.character(congruence)
    )
}

# Plot settings copied from the cumulative plots in paper/paper.qmd.
plot_base_size <- 11
plot_lwd <- 0.65
plot_lwd_strong <- 0.80
plot_lwd_ref <- 0.45
point_size <- 1.25
point_alpha <- 0.35
line_alpha <- 0.85
ribbon_alpha <- 0.22

common_theme <- function(base_size = plot_base_size) {
    theme_minimal(base_size = base_size) +
        theme(
            text = element_text(size = base_size),
            axis.title = element_text(face = "bold", size = base_size + 1),
            axis.text = element_text(size = base_size - 1),
            plot.title = element_text(
                face = "bold",
                size = base_size + 2,
                hjust = 0.5,
                margin = margin(b = 8)
            ),
            plot.subtitle = element_text(
                size = base_size,
                hjust = 0.5,
                margin = margin(b = 8)
            ),
            legend.title = element_blank(),
            legend.text = element_text(size = base_size - 1),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25),
            panel.spacing = grid::unit(1.1, "lines"),
            plot.margin = margin(6, 6, 6, 6)
        )
}

geom_paper_line <- function(..., linewidth = plot_lwd_strong, alpha = line_alpha) {
    geom_line(..., linewidth = linewidth, alpha = alpha)
}

geom_paper_ribbon <- function(..., alpha = ribbon_alpha) {
    geom_ribbon(..., alpha = alpha, colour = NA)
}

geom_paper_hline <- function(yintercept = 0, ...) {
    geom_hline(
        yintercept = yintercept,
        linewidth = plot_lwd_ref,
        linetype = "dashed",
        ...
    )
}

raw_data <- readRDS(find_data_file())

task_labels <- c(
    simon = "Simon",
    snarc = "SNARC",
    tswitch = "Task-switching"
)

params_data <- flatten_list_column(raw_data, "params") |>
    filter(!grepl("^cor__", term)) |>
    mutate(
        task_label = unname(task_labels[task]),
        term = recode(term, "condi" = "congruencei", "sd__condi" = "sd__congruencei")
    )

emmeans_data <- flatten_list_column(raw_data, "emmeans") |>
    filter(contrast != "c - i") |>
    mutate(
        task_label = unname(task_labels[task]),
        congruence = condition_labels(task, cond),
        congruence = factor(
            congruence,
            levels = c("Congruent", "Incongruent", "Non-switch", "Switch")
        )
    )

es_data <- flatten_list_column(raw_data, "es") |>
    mutate(task_label = unname(task_labels[task]))

# Components used to compute Cohen's d with a selectable denominator.
# sigma_px_c already includes any coding-related factor used upstream.
d_components <- params_data |>
    filter(
        term %in% c(
            "congruencei",
            "sd__(Intercept)",
            "sd__congruencei",
            "sd__Observation"
        )
    ) |>
    select(task, trial, term, estimate) |>
    distinct() |>
    pivot_wider(names_from = term, values_from = estimate) |>
    transmute(
        task,
        trial,
        beta1 = congruencei,
        sigma_p = `sd__(Intercept)`,
        sigma_px_c = sd__congruencei,
        sigma_e = sd__Observation
    )

d_variability_choices <- c(
    "P" = "P",
    "P x C" = "PxC",
    "E" = "E"
)

d_variability_labels <- c(
    P = "sigma_P^2",
    PxC = "sigma_PxC^2",
    E = "sigma_E^2"
)


metric_choices <- c(
    "\\(\\beta_0\\) - fixed intercept" = "beta0",
    "\\(\\beta_1\\) - condition effect" = "beta1",
    "\\(\\mathrm{SE}_{\\beta_1}\\) - standard error" = "se_beta1",
    "\\(t\\) - test statistic" = "t",
    "\\(d\\) - standardized effect size" = "d",
    "Estimated marginal means" = "emmeans",
    "\\(\\sigma_P\\) - participant SD" = "sigma_p",
    "\\(\\sigma_{P \\times C}\\) - condition-effect SD" = "sigma_px_c",
    "\\(\\sigma_E\\) - residual SD" = "sigma_e"
)

metric_specs <- list(
    beta0 = list(
        title = "beta[0]",
        subtitle = "Fixed intercept",
        y = "Estimate (ms)",
        source = "params",
        term = "(Intercept)",
        value = "estimate",
        lower = "conf.low",
        upper = "conf.high",
        zero = FALSE
    ),
    beta1 = list(
        title = "beta[1]",
        subtitle = "Fixed condition effect",
        y = "Estimate (ms)",
        source = "params",
        term = "congruencei",
        value = "estimate",
        lower = "conf.low",
        upper = "conf.high",
        zero = TRUE
    ),
    se_beta1 = list(
        title = "SE[beta[1]]",
        subtitle = "Standard error of the fixed condition effect",
        y = "Standard error",
        source = "params",
        term = "congruencei",
        value = "std.error",
        lower = NULL,
        upper = NULL,
        zero = FALSE
    ),
    t = list(
        title = "t",
        subtitle = "Absolute test statistic for the fixed condition effect",
        y = "t",
        source = "params",
        term = "congruencei",
        value = "statistic",
        lower = NULL,
        upper = NULL,
        transform = abs,
        zero = FALSE
    ),
    d = list(
        title = "d",
        subtitle = "Standardized effect size",
        y = "Cohen's d",
        source = "computed_d",
        value = "d",
        lower = NULL,
        upper = NULL,
        zero = FALSE
    ),
    emmeans = list(
        title = "Estimated marginal means",
        subtitle = "Condition-specific model estimates",
        y = "Reaction time (ms)",
        source = "emmeans",
        value = "emmean",
        lower = "asymp.LCL",
        upper = "asymp.UCL",
        zero = FALSE
    ),
    sigma_p = list(
        title = "sigma[P]",
        subtitle = "Participant-level random-intercept SD",
        y = "Standard deviation (ms)",
        source = "params",
        term = "sd__(Intercept)",
        value = "estimate",
        lower = "conf.low",
        upper = "conf.high",
        zero = FALSE
    ),
    sigma_px_c = list(
        title = "sigma[P x C]",
        subtitle = "Participant-by-condition random-slope SD",
        y = "Standard deviation (ms)",
        source = "params",
        term = "sd__congruencei",
        value = "estimate",
        lower = "conf.low",
        upper = "conf.high",
        zero = FALSE
    ),
    sigma_e = list(
        title = "sigma[E]",
        subtitle = "Residual SD",
        y = "Standard deviation (ms)",
        source = "params",
        term = "sd__Observation",
        value = "estimate",
        lower = "conf.low",
        upper = "conf.high",
        zero = FALSE
    )
)

task_ids <- names(task_labels)
trial_ranges <- raw_data |>
    group_by(task) |>
    summarise(
        min_trial = min(trial, na.rm = TRUE),
        max_trial = max(trial, na.rm = TRUE),
        .groups = "drop"
    )

math_selectize_options <- list(
    render = htmlwidgets::JS(
        "{
      option: function(item, escape) {
        return '<div class=\"math-select-option\">' + item.label + '</div>';
      },
      item: function(item, escape) {
        return '<div class=\"math-select-item\">' + item.label + '</div>';
      }
    }"
    ),
    onDropdownOpen = htmlwidgets::JS(
        "function($dropdown) {
      if (window.typesetSelectMath) window.typesetSelectMath($dropdown[0]);
    }"
    ),
    onChange = htmlwidgets::JS(
        "function(value) {
      if (window.typesetSelectMath) window.typesetSelectMath(this.$control[0]);
    }"
    ),
    onInitialize = htmlwidgets::JS(
        "function() {
      if (window.typesetSelectMath) window.typesetSelectMath(this.$control[0]);
    }"
    )
)

b1power <- function(d,
                    n,
                    k,
                    vp,
                    vpc,
                    ve,
                    alpha = 0.05) {

    stopifnot(
        d >= 0,
        n > 0,
        k > 0,
        vp >= 0,
        vpc >= 0,
        ve >= 0,
        abs(vp + vpc + ve - 1) < sqrt(.Machine$double.eps)
    )

    b1 <- d

    se <- sqrt(
        vpc / n +
            4 * ve / (n * k)
    )

    z <- b1 / se
    zcrit <- qnorm(1 - alpha / 2)

    pnorm(-zcrit - z) + 1 - pnorm(zcrit - z)
}

effect_over_time <- function(
        trial,
        beta0,
        delta = 0,
        p = 1,
        trial_start = min(trial),
        trial_end = max(trial)
) {
    stopifnot(
        p > 0,
        trial_end > trial_start
    )

    u <- (trial - trial_start) / (trial_end - trial_start)
    u <- pmin(pmax(u, 0), 1)

    beta0 + delta * u^p
}

ui <- page_sidebar(
    title = "Cumulative trial estimates",
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#2f6f73"
    ),
    tags$head(
        tags$script(
            type = "text/x-mathjax-config",
            HTML("
        MathJax.Hub.Config({
          skipStartupTypeset: true,
          tex2jax: {
            inlineMath: [['\\\\(', '\\\\)']],
            processEscapes: true
          }
        });
      ")
        ),
        tags$script(
            src = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML",
            type = "text/javascript"
        ),
        tags$script(HTML("
      window.typesetSelectMath = function(node) {
        var attempts = 0;
        function run() {
          if (window.MathJax && MathJax.Hub) {
            MathJax.Hub.Queue(['Typeset', MathJax.Hub, node]);
          } else if (attempts < 25) {
            attempts += 1;
            window.setTimeout(run, 100);
          }
        }
        run();
      };
    ")),
        tags$style(HTML("
      body { background: #f7f8f8; }
      .bslib-sidebar-layout > .sidebar { background: #ffffff; }
      .main-card .card-body { padding-top: 0.75rem; }
      .math-select-option, .math-select-item { line-height: 1.55; }
      .math-select-option .MathJax, .math-select-item .MathJax { font-size: 1em !important; }
      table { font-size: 0.9rem; }
      .power-variance-row {
        display: grid;
        grid-template-columns: 28px 1fr;
        column-gap: 6px;
        align-items: end;
      }
      .power-variance-row > .form-group:first-child { margin-bottom: 1.15rem; }
      .power-variance-disabled { opacity: 0.5; }
    ")),
        tags$script(HTML("
          Shiny.addCustomMessageHandler('setPowerVarianceState', function(x) {
            ['vp', 'vpc', 've'].forEach(function(name) {
              var disabled = x.disabled === name;
              $('#power_use_' + name).prop('disabled', disabled);
              $('#power_' + name).prop('disabled', disabled);
              $('#power_' + name).closest('.power-variance-row')
                .toggleClass('power-variance-disabled', disabled);
            });
          });

          $(document).on('shiny:connected', function() {
            if (window.typesetSelectMath) {
              window.typesetSelectMath(document.body);
            }
          });

          $(document).on('shiny:inputchanged', function(event) {
            if (event.name === 'main_tab' && event.value === 'power') {
              window.setTimeout(function() {
                if (window.typesetSelectMath) {
                  window.typesetSelectMath(document.body);
                }
              }, 0);
            }
          });
        "))
    ),
    sidebar = sidebar(
        width = 320,
        conditionalPanel(
            condition = "input.main_tab === 'cumulative'",
            checkboxGroupInput(
                "task",
                "Task",
                choices = setNames(task_ids, task_labels[task_ids]),
                selected = "simon",
                inline = FALSE
            ),
            tagList(
                selectizeInput(
                    "metric",
                    "Y-axis",
                    choices = metric_choices,
                    selected = "beta1",
                    options = math_selectize_options
                ),
                tags$script(HTML(
                    "document.querySelector('script[data-for=\\\"metric\\\"]')
              ?.setAttribute(
                'data-eval',
                '[\\\"render\\\",\\\"onDropdownOpen\\\",\\\"onChange\\\",\\\"onInitialize\\\"]'
              );"
                ))
            ),
            conditionalPanel(
                condition = "input.metric === 'd'",
                checkboxGroupInput(
                    "d_variability",
                    "Variability source",
                    choices = c("P" = "P", "P x C" = "PxC", "E" = "E"),
                    selected = c("PxC", "E"),
                    inline = FALSE
                ),
                helpText("The selected variance components are summed, then square-rooted.")
            ),
            sliderInput(
                "trial_range",
                "Cumulative trial segment",
                min = min(raw_data$trial, na.rm = TRUE),
                max = max(raw_data$trial, na.rm = TRUE),
                value = range(raw_data$trial, na.rm = TRUE),
                step = 1,
                sep = ""
            ),
            checkboxInput("show_ci", "Show confidence intervals when available", TRUE),
            tags$hr(),
            numericInput("y_min", "Y-axis minimum", value = 0, step = "any"),
            numericInput("y_max", "Y-axis maximum", value = 1, step = "any"),
            actionButton(
                "reset_y_limits",
                "Reset automatic limits",
                class = "btn-outline-secondary"
            )
        ),
        conditionalPanel(
            condition = "input.main_tab === 'power'",
            radioButtons(
                "power_effect_pattern",
                "Effect pattern",
                choices = c(
                    "Stable" = "stable",
                    "Changes over trials" = "varying"
                ),
                selected = "stable",
                inline = TRUE
            ),
            conditionalPanel(
                condition = "input.power_effect_pattern === 'stable'",
                numericInput(
                    "power_d",
                    "Stable effect size (d)",
                    value = 0.30,
                    min = 0,
                    step = 0.05
                )
            ),
            conditionalPanel(
                condition = "input.power_effect_pattern === 'varying'",
                numericInput(
                    "power_d0",
                    "Initial effect size, d(1)",
                    value = 0.40,
                    min = 0,
                    step = 0.05
                ),
                numericInput(
                    "power_delta",
                    "Total change, delta",
                    value = -0.20,
                    step = 0.05
                ),
                numericInput(
                    "power_shape",
                    "Shape parameter (p)",
                    value = 0.50,
                    min = 0.05,
                    step = 0.05
                ),
                helpText(
                    "The final effect is d(1) + delta. ",
                    "p < 1 gives early change, p = 1 linear change, and p > 1 late change."
                )
            ),
            numericInput("power_n", "Participants (n)", value = 50, min = 1, step = 1),
            numericInput("power_k", "Maximum trials (k)", value = 100, min = 2, step = 1),
            tags$div(
                class = "power-variance-row",
                checkboxInput("power_use_vp", NULL, value = FALSE),
                numericInput("power_vp", tags$span(HTML("\\(\\sigma_P^2\\)")), value = 0.40, min = 0, max = 1, step = 0.05)
            ),
            tags$div(
                class = "power-variance-row",
                checkboxInput("power_use_vpc", NULL, value = TRUE),
                numericInput("power_vpc", tags$span(HTML("\\(\\sigma_{P \\times C}^2\\)")), value = 0.20, min = 0, max = 1, step = 0.05)
            ),
            tags$div(
                class = "power-variance-row",
                checkboxInput("power_use_ve", NULL, value = TRUE),
                numericInput("power_ve", tags$span(HTML("\\(\\sigma_E^2\\)")), value = 0.40, min = 0, max = 1, step = 0.05)
            ),
            numericInput("power_alpha", "Alpha", value = 0.05, min = 0.0001, max = 0.50, step = 0.01),
            helpText("Select two variance components. The third is disabled and calculated as 1 minus their sum.")
        )
    ),
    div(
        style = "margin-bottom: 1rem;",
        radioButtons(
            "main_tab",
            label = NULL,
            choices = c(
                "Cumulative estimates" = "cumulative",
                "Power" = "power"
            ),
            selected = "cumulative",
            inline = TRUE
        )
    ),
    conditionalPanel(
        condition = "input.main_tab === 'cumulative'",
        uiOutput("plot_header"),
        plotOutput("main_plot", height = "560px")
    ),
    conditionalPanel(
        condition = "input.main_tab === 'power'",
        navset_card_tab(
            nav_panel(
                "Power curve",
                plotOutput("power_plot", height = "540px")
            ),
            nav_panel(
                "Effect pattern",
                plotOutput("power_effect_plot", height = "540px")
            )
        )
    )
)

server <- function(input, output, session) {
    observeEvent(input$task, {
        selected_tasks <- input$task

        validate(
            need(length(selected_tasks) > 0, "Select at least one task.")
        )

        current <- trial_ranges |>
            filter(task %in% selected_tasks) |>
            summarise(
                min_trial = min(min_trial, na.rm = TRUE),
                max_trial = max(max_trial, na.rm = TRUE)
            )

        updateSliderInput(
            session,
            "trial_range",
            min = current$min_trial,
            max = current$max_trial,
            value = c(current$min_trial, current$max_trial)
        )
    }, ignoreInit = FALSE)

    spec <- reactive({
        metric_specs[[input$metric %||% "beta1"]]
    })

    d_variance_expression <- reactive({
        selected <- input$d_variability
        req(length(selected) > 0)

        component_expressions <- list(
            P = expression(sigma[P]^2)[[1]],
            PxC = expression(sigma[P %*% C]^2)[[1]],
            E = expression(sigma[E]^2)[[1]]
        )

        terms <- unname(component_expressions[selected])

        Reduce(
            function(left, right) bquote(.(left) + .(right)),
            terms
        )
    })

    d_subtitle <- reactive({
        variance_expression <- d_variance_expression()

        bquote(
            d == frac(beta[1], sqrt(.(variance_expression)))
        )
    })

    plotted_data <- reactive({
        req(input$trial_range)
        selected_tasks <- input$task

        validate(
            need(length(selected_tasks) > 0, "Select at least one task.")
        )

        current_spec <- spec()

        data <- switch(
            current_spec$source,
            params = params_data |> filter(task %in% selected_tasks, term == current_spec$term),
            emmeans = emmeans_data |> filter(task %in% selected_tasks),
            es = es_data |> filter(task %in% selected_tasks),
            computed_d = {
                selected <- input$d_variability

                validate(
                    need(
                        length(selected) > 0,
                        "Select at least one variance component for Cohen's d."
                    )
                )

                d_components |>
                    filter(task %in% selected_tasks) |>
                    mutate(
                        variance_sum =
                            (if ("P" %in% selected) sigma_p^2 else 0) +
                            (if ("PxC" %in% selected) sigma_px_c^2 else 0) +
                            (if ("E" %in% selected) sigma_e^2 else 0),
                        denominator = sqrt(variance_sum),
                        d = beta1 / denominator
                    )
            }
        )

        data |>
            filter(between(trial, input$trial_range[[1]], input$trial_range[[2]])) |>
            mutate(
                value = .data[[current_spec$value]],
                value = if (!is.null(current_spec$transform)) current_spec$transform(value) else value,
                task_label = unname(task_labels[task]),
                series = if ("congruence" %in% names(data)) {
                    paste(task_label, as.character(congruence), sep = " - ")
                } else {
                    task_label
                },
                lower = if (!is.null(current_spec$lower) && current_spec$lower %in% names(data)) {
                    .data[[current_spec$lower]]
                } else {
                    NA_real_
                },
                upper = if (!is.null(current_spec$upper) && current_spec$upper %in% names(data)) {
                    .data[[current_spec$upper]]
                } else {
                    NA_real_
                },
                lower = if_else(is.na(lower), value, lower),
                upper = if_else(is.na(upper), value, upper)
            )
    })

    metric_header_math <- c(
        beta0 = "\\(\\beta_0\\)",
        beta1 = "\\(\\beta_1\\)",
        se_beta1 = "\\(\\mathrm{SE}_{\\beta_1}\\)",
        t = "\\(t\\)",
        d = "\\(d\\)",
        emmeans = "Estimated marginal means",
        sigma_p = "\\(\\sigma_P\\)",
        sigma_px_c = "\\(\\sigma_{P \\times C}\\)",
        sigma_e = "\\(\\sigma_E\\)"
    )

    output$plot_header <- renderUI({
        metric_label <- metric_header_math[[input$metric %||% "beta1"]]

        tagList(
            tags$span(
                HTML(paste0(
                    paste(unname(task_labels[input$task]), collapse = ", "),
                    " &ndash; ",
                    metric_label
                ))
            ),
            tags$script(HTML("
        window.setTimeout(function() {
          var node = document.getElementById('plot_header');
          if (node && window.typesetSelectMath) {
            window.typesetSelectMath(node);
          }
        }, 0);
      "))
        )
    })

    automatic_y_limits <- reactive({
        data <- plotted_data()

        validate(
            need(nrow(data) > 0, "No data are available for the selected options.")
        )

        p <- ggplot(data, aes(x = trial, y = value, color = series, fill = series))

        has_ci <- !all(is.na(data$lower)) &&
            !all(is.na(data$upper)) &&
            any(data$lower != data$value | data$upper != data$value, na.rm = TRUE)

        if (isTRUE(input$show_ci) && has_ci) {
            p <- p +
                geom_paper_ribbon(
                    aes(ymin = lower, ymax = upper, group = series),
                    show.legend = FALSE
                )
        }

        p <- p +
            geom_paper_line(aes(group = series)) +
            scale_x_continuous(
                limits = input$trial_range,
                breaks = pretty_breaks(n = 8)
            )

        limits <- ggplot_build(p)$layout$panel_params[[1]]$y.range

        if (length(limits) != 2 || any(!is.finite(limits))) {
            return(c(0, 1))
        }

        limits
    })

    compact_limit <- function(x, digits = 4) {
        if (!is.finite(x)) {
            return(x)
        }

        # Keep a small, scale-independent number of significant digits in the
        # sidebar without changing the underlying automatic-limit calculation.
        signif(x, digits = digits)
    }

    update_y_limits <- function() {
        limits <- automatic_y_limits()
        compact_limits <- vapply(limits, compact_limit, numeric(1))

        updateNumericInput(session, "y_min", value = compact_limits[[1]])
        updateNumericInput(session, "y_max", value = compact_limits[[2]])
    }

    observeEvent(
        list(
            input$task,
            input$metric,
            input$trial_range,
            input$d_variability,
            input$show_ci
        ),
        update_y_limits(),
        ignoreInit = FALSE
    )

    observeEvent(input$reset_y_limits, {
        update_y_limits()
    })

    output$main_plot <- renderPlot({
        data <- plotted_data()
        current_spec <- spec()

        validate(need(nrow(data) > 0, "No data are available for the selected options."))

        p <- ggplot(data, aes(x = trial, y = value, color = series, fill = series))

        has_ci <- !all(is.na(data$lower)) &&
            !all(is.na(data$upper)) &&
            any(data$lower != data$value | data$upper != data$value, na.rm = TRUE)

        if (isTRUE(input$show_ci) && has_ci) {
            p <- p +
                geom_paper_ribbon(
                    aes(ymin = lower, ymax = upper, group = series),
                    show.legend = FALSE
                )
        }

        p <- p +
            geom_paper_line(aes(group = series)) +
            scale_x_continuous(
                limits = input$trial_range,
                breaks = pretty_breaks(n = 8)
            ) +
            scale_y_continuous(
                labels = label_number_auto()
            ) +
            labs(
                title = paste(unname(task_labels[input$task]), collapse = ", "),
                subtitle = if (identical(input$metric, "d")) d_subtitle() else current_spec$subtitle,
                x = "Cumulative trial",
                y = current_spec$y
            ) +
            common_theme()

        validate(
            need(is.finite(input$y_min), "Enter a valid lower Y-axis limit."),
            need(is.finite(input$y_max), "Enter a valid upper Y-axis limit."),
            need(input$y_min < input$y_max, "The lower Y-axis limit must be smaller than the upper limit.")
        )

        p <- p + coord_cartesian(ylim = c(input$y_min, input$y_max))

        if (isTRUE(current_spec$zero)) {
            p <- p + geom_paper_hline(0)
        }

        if (length(input$task) == 1 && current_spec$source != "emmeans") {
            p <- p + theme(legend.position = "none")
        }

        p
    }, res = 120)

    selected_power_variances <- reactive({
        selected <- c(
            vp = isTRUE(input$power_use_vp),
            vpc = isTRUE(input$power_use_vpc),
            ve = isTRUE(input$power_use_ve)
        )

        names(selected)[selected]
    })

    observe({
        selected <- selected_power_variances()
        disabled <- if (length(selected) == 2) {
            setdiff(c("vp", "vpc", "ve"), selected)
        } else {
            character(0)
        }

        session$sendCustomMessage(
            "setPowerVarianceState",
            list(disabled = if (length(disabled)) disabled else NULL)
        )

        if (length(disabled) == 1) {
            selected_values <- vapply(
                selected,
                function(component) input[[paste0("power_", component)]] %||% NA_real_,
                numeric(1)
            )

            if (all(is.finite(selected_values))) {
                calculated <- 1 - sum(selected_values)
                updateNumericInput(
                    session,
                    paste0("power_", disabled),
                    value = round(calculated, 6)
                )
            }
        }
    })

    power_variances <- reactive({
        selected <- selected_power_variances()

        validate(
            need(length(selected) == 2, "Select exactly two variance components.")
        )

        values <- c(
            vp = input$power_vp,
            vpc = input$power_vpc,
            ve = input$power_ve
        )

        calculated <- setdiff(names(values), selected)
        values[[calculated]] <- 1 - sum(values[selected])

        validate(
            need(all(is.finite(values[selected])), "Variance components must be finite."),
            need(all(values[selected] >= 0), "Variance components must be non-negative."),
            need(sum(values[selected]) <= 1, "The two selected variance components cannot sum to more than 1.")
        )

        values
    })

    power_effect_data <- reactive({
        validate(
            need(is.finite(input$power_k) && input$power_k >= 2, "k must be at least 2.")
        )

        trial_values <- seq_len(as.integer(input$power_k))

        if (identical(input$power_effect_pattern, "stable")) {
            validate(
                need(is.finite(input$power_d) && input$power_d >= 0, "d must be non-negative.")
            )

            instantaneous_d <- rep(input$power_d, length(trial_values))
        } else {
            validate(
                need(is.finite(input$power_d0) && input$power_d0 >= 0, "The initial effect must be non-negative."),
                need(is.finite(input$power_delta), "delta must be finite."),
                need(is.finite(input$power_shape) && input$power_shape > 0, "p must be positive."),
                need(input$power_d0 + input$power_delta >= 0, "The final effect must be non-negative.")
            )

            instantaneous_d <- effect_over_time(
                trial = trial_values,
                beta0 = input$power_d0,
                delta = input$power_delta,
                p = input$power_shape,
                trial_start = 1,
                trial_end = input$power_k
            )
        }

        tibble(
            k = trial_values,
            instantaneous_d = instantaneous_d,
            cumulative_d = cumsum(instantaneous_d) / trial_values
        )
    })

    power_data <- reactive({
        variances <- power_variances()
        effects <- power_effect_data()

        validate(
            need(is.finite(input$power_n) && input$power_n > 0, "n must be positive."),
            need(is.finite(input$power_alpha) && input$power_alpha > 0 && input$power_alpha < 1, "alpha must be between 0 and 1.")
        )

        effects |>
            mutate(
                power = b1power(
                    d = cumulative_d,
                    n = input$power_n,
                    k = k,
                    vp = variances[["vp"]],
                    vpc = variances[["vpc"]],
                    ve = variances[["ve"]],
                    alpha = input$power_alpha
                )
            )
    })

    output$power_plot <- renderPlot({
        data <- power_data()
        variances <- power_variances()

        ggplot(data, aes(x = k, y = power)) +
            geom_paper_line() +
            geom_hline(
                yintercept = 0.80,
                linewidth = plot_lwd_ref,
                linetype = "dashed"
            ) +
            scale_x_continuous(
                limits = c(1, input$power_k),
                breaks = pretty_breaks(n = 8)
            ) +
            scale_y_continuous(
                limits = c(0, 1),
                breaks = seq(0, 1, by = 0.20),
                labels = label_percent(accuracy = 1)
            ) +
            labs(
                title = "Statistical power",
                subtitle = if (identical(input$power_effect_pattern, "stable")) {
                    paste0(
                        "Stable d = ", input$power_d,
                        ", n = ", input$power_n,
                        ", trials from 1 to ", input$power_k,
                        ", vp = ", round(variances[["vp"]], 3),
                        ", vpc = ", round(variances[["vpc"]], 3),
                        ", ve = ", round(variances[["ve"]], 3)
                    )
                } else {
                    paste0(
                        "d(1) = ", input$power_d0,
                        ", final d = ", round(input$power_d0 + input$power_delta, 3),
                        ", p = ", input$power_shape,
                        ", n = ", input$power_n
                    )
                },
                x = "Trials (k)",
                y = "Power"
            ) +
            common_theme() +
            theme(legend.position = "none")
    }, res = 120)

    output$power_effect_plot <- renderPlot({
        data <- power_effect_data() |>
            select(k, instantaneous_d, cumulative_d) |>
            pivot_longer(
                cols = c(instantaneous_d, cumulative_d),
                names_to = "series",
                values_to = "d"
            ) |>
            mutate(
                series = recode(
                    series,
                    instantaneous_d = "Trial-specific effect",
                    cumulative_d = "Cumulative average effect"
                )
            )

        ggplot(data, aes(x = k, y = d, linetype = series)) +
            geom_paper_hline(0) +
            geom_paper_line(aes(group = series)) +
            scale_x_continuous(
                limits = c(1, input$power_k),
                breaks = pretty_breaks(n = 8)
            ) +
            scale_y_continuous(labels = label_number_auto()) +
            labs(
                title = "Effect pattern over trials",
                subtitle = if (identical(input$power_effect_pattern, "stable")) {
                    paste0("Stable effect: d = ", input$power_d)
                } else {
                    paste0(
                        "d(1) = ", input$power_d0,
                        ", final d = ", round(input$power_d0 + input$power_delta, 3),
                        ", p = ", input$power_shape
                    )
                },
                x = "Trial",
                y = "Effect size (d)",
                linetype = NULL
            ) +
            common_theme()
    }, res = 120)

}

shinyApp(ui, server)
