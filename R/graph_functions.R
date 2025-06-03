theme_bgr <- function(font_size = 12) {

  ggplot2::theme_classic(base_size = font_size) +
    ggplot2::theme(
      text              = ggplot2::element_text(family = "sans", size = font_size),
      axis.text         = ggplot2::element_text(size = ggplot2::rel(1), color = "black"),
      axis.ticks        = ggplot2::element_line(color = "black"),
      legend.background = ggplot2::element_rect(color = NA, fill = NA),
      legend.text       = ggplot2::element_text(size = ggplot2::rel(1)),
      legend.title      = ggplot2::element_text(size = ggplot2::rel(1)),
      legend.position   = "right",
      plot.title        = ggplot2::element_text(size = ggplot2::rel(1), face = "plain", hjust = 0.5),
      strip.background  = ggplot2::element_blank(),
      strip.text        = ggplot2::element_text(colour = "black",
                                       size = ggplot2::rel(1),
                                       margin = ggplot2::margin(font_size/2, font_size/2, font_size/2,font_size/2))
    )

}

code_histogram <- function(df, bins = 7) {

  vnames <- names(df)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vnames[1]]])) +
    ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(count)/sum(ggplot2::after_stat(count))),
             color = "black", fill = "white", width = 1) +
    ggplot2::scale_x_binned(n.breaks = bins)
  if (ncol(df) == 2)
    p <- p + ggplot2::facet_wrap(~ .data[[vnames[2]]], scales = "free", nrow = 1)

  p

}

code_frequency <- function(df) {

  vnames <- names(df)
  if (ncol(df) == 2) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vnames[1]]], fill = .data[[vnames[2]]])) +
      ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(count)/sum(ggplot2::after_stat(count))),
               color = "black", position = "dodge")
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vnames[1]]])) +
      ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(count)/sum(ggplot2::after_stat(count))), color = "black", fill = "white")
  }

}

code_boxplot <- function(df) {

  vnames <- names(df)
  if (ncol(df) == 3) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vnames[1]]], y = .data[[vnames[2]]], fill = .data[[vnames[3]]])) +
      ggplot2::geom_boxplot(color = "black", position = "dodge")
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vnames[1]]], y = .data[[vnames[2]]])) +
      ggplot2::geom_boxplot(color = "black", fill = "white")
  }

}

code_bargraph <- function(df, ebars = "none") {

  vnames <- names(df)
	
  if (ncol(df) == 3) {
		names(df) <- c("v1", "v2", "v3")
		df_summ <- df |>
			dplyr::group_by(v1, v3) |>
			dplyr::summarise(
				v_mean = mean(v2),
				v_sd = stats::sd(v2),
				v_se = stats::sd(v2)/sqrt(sum(!is.na(v2)))
			) |>
			dplyr::ungroup()

		p <- ggplot2::ggplot(df_summ, ggplot2::aes(x = v1, y = v_mean, fill = v3)) +
			ggplot2::geom_bar(stat = "identity", color = "black", position = "dodge")
  } else {
    names(df) <- c("v1", "v2")
		df_summ <- df |>
		dplyr::group_by(v1) |>
			dplyr::summarise(
				v_mean = mean(v2),
				v_sd = stats::sd(v2),
				v_se = stats::sd(v2)/sqrt(sum(!is.na(v2)))
			) |>
			dplyr::ungroup()

		p <- ggplot2::ggplot(df_summ, ggplot2::aes(x = v1, y = v_mean)) +
			ggplot2::geom_bar(stat = "identity", color = "black", 
				fill = "white", position = "dodge")
  }

  if (ebars == "sd") {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = v_mean - v_sd, ymax = v_mean + v_sd),
                         color = "black", width = 0.3, position = ggplot2::position_dodge(0.9))
  } else if (ebars == "se") {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = v_mean - v_se, ymax = v_mean + v_se),
                         color = "black", width = 0.3, position = ggplot2::position_dodge(0.9))
  }
  p
}

code_scatter <- function(df, symsize = 1, rline = FALSE) {

  vnames <- names(df)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[vnames[1]]], y = .data[[vnames[2]]])) +
    ggplot2::geom_point(color = "black", shape = 16, size = symsize)
  if (ncol(df) == 3)
    p <- p + ggplot2::facet_wrap(~ .data[[vnames[3]]], scales = "free", nrow = 1)
  if (rline) {
    p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5)
  }
  p
}

