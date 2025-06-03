variables_that_are <- function(df, type = "categorical") {

  if (type == "numeric") dplyr::select(df, where(is.numeric))
  else dplyr::select(df, -where(is.numeric))

}


build_data_summary <- function(df) {

  d_head <- paste("<p>", nrow(df), " observations of ", ncol(df), " variables</p>", sep = "")
  d_sub1 <- "<p><b>Categorical variables</b></p>"
  d_sub2 <- "<p><b>Numeric variables</b></p>"

  df.cat <- variables_that_are(df, "categorical")

  if (ncol(df.cat) > 0) {
    v <- lapply(df.cat, levels)
    a <- names(v)
    b <- rep("x", length(a))
    for (i in 1:length(v)) {
      b[i] <- paste(v[[i]], collapse = ", ")
    }
    d_cat <- data.frame("variable" = a, "levels" = b)
  } else {
    d_cat <- (data.frame(x = "No categorical variables found."))
  }

  df.num <- variables_that_are(df, "numeric")

  if (ncol(df.num) > 0) {
    d_min  <- df.num |> dplyr::summarise(dplyr::across(dplyr::everything(), ~ min(.x, na.rm = TRUE)))
    d_max  <- df.num |> dplyr::summarise(dplyr::across(dplyr::everything(), ~ max(.x, na.rm = TRUE)))
    d_mean <- df.num |> dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))
    d_sd   <- df.num |> dplyr::summarise(dplyr::across(dplyr::everything(), ~ stats::sd(.x, na.rm = TRUE)))
    d_num <- data.frame("variable" = names(df.num), "min" = t(d_min), "max" = t(d_max), "mean" = t(d_mean), "sd" = t(d_sd))
  } else {
    d_num <- (data.frame(x = "No numeric variables found."))
  }

  d_out <- list(
    shiny::HTML(paste0("<p>", nrow(df), " observations of ", ncol(df), " variables</p>")),
    shiny::strong("Categorical variables"), shiny::renderTable(d_cat),
    shiny::strong("Numeric variables"), shiny::renderTable(d_num)
  )

  do.call(tagList, d_out)

}
