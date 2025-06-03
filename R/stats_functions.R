code_descriptives <- function(s, vnames, dec = 3) {

  if (ncol(s) == 2) s <- s |> dplyr::group_by(v2)

  s_t <- s |>
    dplyr::summarise(
      n = sum(!is.na(v1)),
      mean = mean(v1),
      median = stats::median(v1),
      s = stats::sd(v1),
      SE = stats::sd(v1)/sqrt(sum(!is.na(v1))),
      CV = stats::sd(v1)/mean(v1)*100,
      min = min(v1),
      max = max(v1),
      range = max(v1) - min(v1),
      Q1 = as.numeric(stats::quantile(v1, probs = (.25))),
      Q3 = as.numeric(stats::quantile(v1, probs = (.75))),
      IQR = stats::IQR(v1)
    ) |>
    dplyr::select(-any_of("v2")) |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column("stat") |>
    dplyr::ungroup()

  if (ncol(s) == 1) names(s_t) <- c("stat", vnames[1])
  else names(s_t) <- c("stat", levels(s$v2))

  s_gt <- s_t |>
    gt::gt(rowname_col = "stat") |>
    gt::fmt_number(
      decimals = dec,
      drop_trailing_zeros = TRUE
    ) |>
    gt::cols_align("right") |>
    gt::opt_table_font(size = 12)

  if (ncol(s) == 2) {
    s_gt <- s_gt |>
      gt::tab_spanner(
        label = vnames[1],
        columns = everything()
      ) |>
      gt::opt_table_font(size = 12)
  }

  s_text <- shiny::HTML(
    "<p><h4>Descriptive statistics </h4></p>
		<p>missing values have been removed</p>"
  )

  stats_out_list <- list(s_text, s_gt)

}

code_freqtable <- function(s, vnames) {

  if (ncol(s) == 1) {
    s_t <- s |>
      dplyr::group_by(v1) |>
      dplyr::summarize(frequency = dplyr::n()) |>
      dplyr::ungroup()
    names(s_t) <-  c(vnames[1], "frequency")
  } else {
    s_t <- s |>
      dplyr::group_by(v1, v2) |>
      dplyr::summarise(frequency = dplyr::n()) |>
      tidyr::pivot_wider(
        names_from = v2,
        values_from = frequency
      ) |>
      dplyr::ungroup()
    names(s_t) <- c(vnames[1], levels(s$v2))
  }

  s_gt <- s_t |>
    gt::gt(rowname_col = vnames[1]) |>
    gt::cols_align("right") |>
    gt::opt_table_font(size = 12)

  s_text <- shiny::HTML(
    "<p><h4>Frequency</h4></p>"
  )

  stats_out_list <- list(s_text, s_gt)
}

code_xsq_gof <- function(s, vnames, exp_f) {

  levs <- levels(s$v1)
  if (length(levs) > 1) {

    if (sum(exp_f) != 1)
      warn_str <- "The expected relative frequencies do not total 1. The entered values have been rescaled to relative values."
    else warn_str <- NULL
    s_xsq <- stats::chisq.test(table(s$v1), p = exp_f, rescale.p = TRUE)
    if (s_xsq$p.value < 0.0005)  pval <- "< 0.001"
    else pval <- round(s_xsq$p.value, 3)
    s_t <- data.frame(s_xsq$observed)
    names(s_t) <- c(vnames[1], "observed")
    s_t$expected <- s_xsq$expected

    s_gt <- s_t |>
      gt::gt() |>
      gt::fmt_number(
        decimals = 2,
        drop_trailing_zeros = TRUE
      ) |>
      gt::cols_align("right") |>
      gt::opt_table_font(size = 12)

    s_text <- shiny::HTML(paste0(
      "<p><h4>&Chi;<sup>2</sup> goodness-of-fit test</h4></p>",
      "<p>H<sub>0</sub>: the population frequency distribution of ", vnames[1],
      " is equal to the hypothesized distribution</p>",
      warn_str,
      "<p>&Chi;<sup>2</sup> test statistic =  ", round(s_xsq$statistic, 3),
      "</p><p>d.f. =  ", s_xsq$parameter, "</p><p>p value =  ", pval, "</p>"
    ))


  } else {
    s_text <- HTML(paste0(
      "<p>This test requires a variable with 2 or more levels.</p>",
      "<p>The variable ", vnames[1], " has ", length(levs), "."
    ))
    s_gt <- NULL
  }

  stats_out_list <- list(s_text, s_gt)

}

code_xsq_toa <- function(s, vnames) {

  levs_x <- levels(s$v1)
  levs_y <- levels(s$v2)

  if (length(levs_x) > 1 & length(levs_y) > 1) {

    s_xsq <- stats::chisq.test(table(s$v1, s$v2))
    if (s_xsq$p.value < 0.0005)  pval <- "< 0.001"
    else pval <- round(s_xsq$p.value, 3)

    s_obs <- data.frame(s_xsq$observed)
    names(s_obs) <- c(vnames[1], vnames[2], "Freq")
    s_obs <- s_obs |>
      tidyr::pivot_wider(names_from = vnames[2], values_from = Freq)
    s_exp <- data.frame(s_xsq$expected) |>
      tibble::rownames_to_column(var = vnames[1])
    names(s_exp) <- c(vnames[1], levs_y)

    s_obs_gt <- s_obs |>
      gt::gt() |>
      gt::fmt_number(
        decimals = 2,
        drop_trailing_zeros = TRUE
      ) |>
      gt::tab_header("observed") |>
      gt::cols_align("right") |>
      gt::opt_table_font(size = 12)

    s_exp_gt <- s_exp |>
      gt::gt() |>
      gt::fmt_number(
        decimals = 2,
        drop_trailing_zeros = TRUE
      ) |>
      gt::tab_header("expected") |>
      gt::cols_align("right") |>
      gt::opt_table_font(size = 12)

    s_text <- shiny::HTML(paste0(
      "<p><h4>&Chi;<sup>2</sup> test of association</h4></p>",
      "<p>H<sub>0</sub>: ", vnames[1], " is independent of ", vnames[2], "</p>",
      "<p>&Chi;<sup>2</sup> test statistic =  ", round(s_xsq$statistic, 3), "</p>",
      "<p>d.f. =  ", s_xsq$parameter, "</p><p>p value =  ", pval, "</p>"
    ))

  } else {

    s_text <- shiny::HTML(paste0(
      "<p>This test requires variables with 2 or more levels.</p>",
      "<p>The variable ", vnames[1], " has ", length(levs_x), " and the variable ",
      vnames[2], " has ", length(levs_y), "."
    ))

    s_obs_gt <- NULL
    s_exp_gt <- NULL
  }

  stats_out_list <- list(s_text, s_obs_gt, s_exp_gt)

}

code_correlation <- function(s, vnames) {

  s_c <- stats::cor.test(s$v1, s$v2)
  if (s_c$p.value < 0.0005)  pval <- "< 0.001"
  else pval <- round(s_c$p.value, 3)
  # - # correlation
  s_text <- HTML(paste0(
    "<p><h4>Correlation</h4></p>",
    "<p>x = ", vnames[1], ", y = ",vnames[2], "</p>",
    "<p>H<sub>0</sub>: no correlation</p>",
    "<p>r = ", round(s_c$estimate, 3), "</p><p>d.f. = ", s_c$parameter, "</p>",
    "<p>p = ", pval, "</p>"
  ))

  s_gt <- NULL
  stats_out_list <- list(s_text, s_gt)
}

code_tpaired <- function(s, vnames) {

  tx <- stats::t.test(s$v1, s$v2, paired = TRUE)
  if (tx$p.value < 0.0005 ) pval <- "p < 0.001"
  else pval <- paste("p = ",round(tx$p.value,4))

  s_text <- shiny::HTML(paste0(
    "<p><h4>Paired sample t test</h4></p>",
    "<p>y1 = ", vnames[1], "<br>y2 = ", vnames[2], "</p>",
    "<p>H<sub>0</sub>: &mu;<sub>", vnames[1], "-", vnames[2], "</sub> = 0</p>",
    "<p>x&#773;<sub>", vnames[1], "-", vnames[2], "</sub> = ", signif(tx$estimate,5), "</p>",
    "<p>t = ", round(tx$statistic, 3), ", df = ", round(tx$parameter, 1), ", ", pval, "</p>"
  ))

  s_gt <- NULL
  stats_out_list <- list(s_text, s_gt)
}

code_t1sample <- function(s, vnames, mu_null = 0) {

  tx <- stats::t.test(s$v1, mu = mu_null)
  if (tx$p.value < 0.0005 ) pval <- "p < 0.001"
  else pval <- paste("p = ",round(tx$p.value,4))

  s_text <- shiny::HTML(paste0(
    "<p><h4>One sample t test</h4></p>",
    "<p>H<sub>0</sub>: &mu;<sub>", vnames[1], "</sub> = ", mu_null, "</p>",
    "<p>x&#773;<sub>", vnames[1], "</sub> = ", signif(tx$estimate, 5), "</p>",
    "<p>t = ", round(tx$statistic, 3), ", df = ", round(tx$parameter, 1), ", ", pval, "</p>"
  ))

  s_gt <- NULL
  stats_out_list <- list(s_text, s_gt)
}

code_t2sample <- function(s, vnames) {

  grps <- levels(s$v1)

  if (length(grps) == 2) {
    tx <- stats::t.test(s$v2 ~ s$v1)
    if (tx$p.value < 0.0005 ) pval <- "p < 0.001"
    else pval <- paste("p = ",round(tx$p.value,4))

    s_text <- shiny::HTML(paste0(
      "<p><h4>Two sample t test</h4></p>",
      "<p>y = ", vnames[2], "<br>x = ", vnames[1],
      "<p>H<sub>0</sub>: &mu;<sub>", grps[1], "</sub> = &mu;<sub>", grps[2], "</sub></p>",
      "<p>x&#773;<sub>", grps[1], "</sub> = ", signif(tx$estimate[1],5),
      ", x&#773;<sub>", grps[2], "</sub> = ", signif(tx$estimate[2],5), "</p>",
      "<p>t = ", round(tx$statistic, 3), ", df = ", round(tx$parameter, 1), ", ", pval, "</p>"
    ))
  } else {
    s_text <- shiny::HTML(paste0(
      "<p>This test requires an independent variable with 2 levels.</p>",
      "<p>The variable ", vnames[1], " has ", length(grps), " levels.</p>"
    ))
  }

  s_gt <- NULL
  stats_out_list <- list(s_text, s_gt)
}


code_linear_model <- function(s, vnames, log_y = FALSE, posthoc_test = FALSE, assumption_test = FALSE) {

  if (log_y) {
    if (min(s$v2) == 0) {
      s <- s |> dplyr::mutate("log1p_v2" = log1p(v2))
      s_var <- paste0("<p>x = ", vnames[1], "<br>y = ln(", vnames[2], " + 1)</p>")
      mod <- stats::aov(s$log1p_yvar ~ s$v1)
    } else if (min(s$v2) > 0) {
      s <- s |> dplyr::mutate("log_v2" = log(v2))
      s_var <- paste0("<p>x = ", vnames[1], "<br>y = ln(", vnames[2], ")</p>")
      mod <- stats::aov(s$log_v2 ~ s$v1)
    } else {
      s_var <- paste0("<p>x = ", vnames[1], "<br>y = ", vnames[2],
                      "</p><p>(y has negative values and was not transformed)</p>")
      mod <- stats::aov(s$v2 ~ s$v1)
    }
  } else {
    s_var <- paste0("<p>x = ", vnames[1], "<br>y = ", vnames[2], "</p>")
    mod <- stats::aov(s$v2 ~ s$v1)
  }

  s_t <- summary(mod)[[1]] |> tibble::rownames_to_column()
  names(s_t) <- c("source","df","SS","MS","F","p")
  s_t <- s_t |> dplyr::relocate("source", "SS", "df", "MS","F","p")
  s_t$source <- c(vnames[1], "residual")
  s_t <- s_t |>
    dplyr::mutate(
      SS = round(SS, 2),
      MS = round(SS, 2),
      F = round(F, 2)
    )

  if (s_t[1,6] < 0.0005) s_t[1,6] <- "< 0.001"
  else s_t[1,6] <- round(s_t[1,6],3)
  s_t[2,5] <- "---"
  s_t[2,6] <- "---"

  s_anova_gt <- s_t |>
    gt::gt() |>
    gt::fmt_number(drop_trailing_zeros = TRUE) |>
    gt::cols_align("right") |>
    gt::tab_header("ANOVA table") |>
    gt::opt_table_font(size = 12) |>
    gt::opt_align_table_header("left")

  # post hoc
  if (is.factor(s$v1) && posthoc_test) {
    s_post <- stats::TukeyHSD(mod)
    s_post <- data.frame(s_post[[1]])
    s_post$p.adj <- round(s_post$p.adj, 3)
    s_post$p.adj[s_post$p.adj < 0.0005] <- "< 0.001"
    names(s_post) <- c("difference", "lower", "upper", "p (adjusted)")
    s_post <- tibble::rownames_to_column(s_post, var = "levels")
    s_post_gt <- s_post |>
      gt::gt() |>
      gt::fmt_number(decimals = 3) |>
      gt::cols_align("right") |>
      gt::tab_header("Tukey post-hoc tests") |>
      gt::opt_table_font(size = 12) |>
      gt::opt_align_table_header("left")
  } else {
    s_post_gt <- NULL
  }

  if (is.numeric(s$v1)) {
    s_text <- shiny::HTML(paste0(
      "<p><h4>Regression</h4></p>",
      s_var,
      "<p>H<sub>0</sub>: slope = 0</p>",
      "<p>Regression line: slope = ", round(mod$coefficients[2], 2),
      ", intercept = ", round(mod$coefficients[1], 2), "</p>",
      "<p>r<sup>2</sup>= ", round(s_t$SS[1]/s_t$SS[2], 3), "</p>"
    ))
  } else {
    s_text <- shiny::HTML(paste0(
      "<p><h4>Analysis Of Variance</h4></p>",
      s_var,
      "<p>H<sub>0</sub>: &mu;<sub>", vnames[2], "</sub> equal at all levels of ", vnames[1], "</p>"
    ))
  }

  # assumptions
  if (assumption_test) {

    if (is.factor(s$v1)) {
      lv <- car::leveneTest(mod)
      names(lv) <- c("df", "F", "p")
      if (lv$p[1] < 0.001) lvp <- "p < 0.001"
      else lvp <- paste0("p = ", round(lv$p[1],3))
      s_lev <- paste0("Levene's test: F = ",
                      round(lv$F[1],3), ", df = ", lv$df[1],", ", lv$df[2], ", ", lvp)
    } else {
      s_lev <- NULL
    }

    res <- unique(mod$residuals)
    ks <- stats::ks.test(res, "pnorm", mean = mean(res), sd = sd(res))
    if (ks$p.value < 0.001) ksp <- "p < 0.001"
    else ksp <- paste0("p = ", round(ks$p.value,3))
    s_ks <- paste0("Kolmogorov-Smirnov test: D = ",
                   round(ks$statistic,3), ", ", ksp)

    tq <- stats::qqnorm(mod$residuals, plot.it = FALSE)
    tq.data <- data.frame("theoretical" = tq$x, "sample" = tq$y, "fitted" = mod$fitted.values, "residual" = mod$residuals)
    x.q <- stats::quantile(tq.data$theoretical, c(0.25, 0.75), type = 5)
    y.q <- stats::quantile(tq.data$sample, c(0.25, 0.75), type = 5)
    q.slope <- diff(y.q) / diff(x.q)
    q.int   <- y.q[1] - q.slope * x.q[1]

    s_qq.plot <- ggplot2::ggplot(tq.data, ggplot2::aes(x = theoretical, y = sample)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_abline(slope = q.slope, intercept = q.int) +
      theme_bgr()

    s_var.plot <- ggplot2::ggplot(tq.data, ggplot2::aes(x = fitted, y = residual)) +
      ggplot2::geom_point(shape = 1, position = ggplot2::position_jitter(height = 0, width = .01)) +
      theme_bgr()

    s_assumptions <- list(
      shiny::HTML(paste0(
        "<p><strong>Tests of assumptions</strong></p>",
        "<p>Normality of residuals</p>",
        s_ks,
        "<p>QQ plot</p>"
      )),
      shiny::renderPlot(s_qq.plot, width = 480, height = 360),
      shiny::HTML(paste0(
        "<p>Homogeneity of variance</p>",
        s_lev,
        "<p>Residual variance plot</p>"
      )),
      shiny::renderPlot(s_var.plot, width = 480, height = 360)
    )
  }

  else s_assumptions <- NULL

  stats_out_list <- list(s_text, s_anova_gt, s_post_gt, s_assumptions)

}
