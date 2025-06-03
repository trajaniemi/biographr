#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  values <- shiny::reactiveValues()

  # --- # --- # --- # Data # --- # --- # --- #

  # --- #--- # Get data from a file # --- # --- #

  w_data <- shiny::eventReactive(input$go_data, {
    if (input$data_type == 1) {
      d <- dunes()
    } else if (input$data_type == 2) {
      d <- heart()
    } else {
      if (is.null(input$data_file)) {
        return(data.frame(x = "Select your datafile."))
      } else {
        ext <- tools::file_ext(input$data_file$name)

        d <- switch(ext,
                    xlsx = readxl::read_excel(input$data_file$datapath,
                                      na = c("NA","na","n/a","N/A",""),
                                      .name_repair = "universal"),
                    csv = read_csv(input$data_file$datapath,
                                   na = c("NA","na","n/a","N/A",""),
                                   col_types = cols()),
                    shiny::validate("Invalid file. Please upload a .csv or .xlsx file.")
        )
      }
    }
    d <- d |> dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor))
  })

  # --- # --- # Summarize data # --- # --- #

  data_summary <- shiny::eventReactive(input$go_data, {
    ds <- build_data_summary(w_data())
  })
  output$data_summ <- shiny::renderUI(data_summary())

  # --- # --- # --- # Graphs # --- # --- # --- #

  # --- # --- # Update options # --- # --- #

  shiny::observeEvent(input$go_data, {
    shiny::req(w_data())

    c_vars <- w_data() |> dplyr::select(dplyr::where(is.factor)) |> names()
    n_vars <- w_data() |> dplyr::select(dplyr::where(is.numeric)) |> names()

    shiny::updateSelectInput(session, "x_num", choices = n_vars)
    shiny::updateSelectInput(session, "x_cat", choices = c_vars)
    shiny::updateSelectInput(session, "y_var", choices = n_vars)
    shiny::updateSelectInput(session, "z_var", choices = c("no groups" = ".", c_vars))

  })

  shiny::observeEvent(input$x_cat, {
    shiny::updateTextInput(session, "lab_xcat", value = input$x_cat)
    shinyjqui::updateOrderInput(session, "change_x", items = levels(w_data()[[input$x_cat]]))
  })

  shiny::observeEvent(input$x_num, {
    shiny::updateTextInput(session, "lab_xnum", value = input$x_num)
  })

  shiny::observeEvent(input$y_var, {
    shiny::updateTextInput(session, "lab_y", value = input$y_var)
  })

  shiny::observeEvent(input$z_var, {
    if (input$z_var != ".") {
      shiny::updateTextInput(session, "lab_z", value = input$z_var)
      shinyjqui::updateOrderInput(session, "change_z", items = levels(w_data()[[input$z_var]]))
    }
  })

  # --- # --- # Create ggplot code # --- # --- #

  gg_code <- shiny::eventReactive(input$go_plot, {

    if (input$g_type == "hist") {
      d <- w_data() |> dplyr::select(dplyr::all_of(input$x_num))
    } else if (input$g_type == "freq") {
      d <- w_data() |> dplyr::select(dplyr::all_of(input$x_cat))
      d[[input$x_cat]] <- factor(d[[input$x_cat]], levels = input$change_x)
    } else if (input$g_type == "scatter") {
      d <- w_data() |> dplyr::select(dplyr::all_of(c(input$x_num, input$y_var)))
    } else {
      d <- w_data() |> dplyr::select(dplyr::all_of(c(input$x_cat, input$y_var)))
      d[[input$x_cat]] <- factor(d[[input$x_cat]], levels = input$change_x)
    }

    if (input$z_var != ".") {
      dz <- w_data() |> dplyr::select(dplyr::all_of(input$z_var))
      dz[[input$z_var]] <- factor(dz[[input$z_var]], levels = input$change_z)
      d <- d |> cbind(dz)
    }

    d <- tidyr::drop_na(d)

    if (input$g_type == "hist") p <- code_histogram(d, bins = input$bins)
    else if (input$g_type == "freq") p <- code_frequency(d)
    else if (input$g_type == "box") p <- code_boxplot(d)
    else if (input$g_type == "bar") p <- code_bargraph(d, ebars = input$error_bars)
    else if (input$g_type == "scatter") p <- code_scatter(d, symsize = input$sym_size, rline = input$rline)

    if (input$g_type == "hist" || input$g_type == "freq") lab_y <- "frequency"
    else lab_y <- input$lab_y
    if (input$g_type == "hist" || input$g_type == "scatter") lab_x <- input$lab_xnum
    else lab_x <- input$lab_xcat

    p <- p +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::labs(x = lab_x, y = lab_y) +
      theme_bgr(font_size = input$font_size)

    if (input$z_var != ".") p <- p + ggplot2::scale_fill_grey(start = 1, end = 0.3, name = input$lab_z)

    return(p)
  })

  shiny::observeEvent(input$go_plot, {
    shinyjs::show("download_plot")
  })

  output$out_ggplot <- shiny::renderPlot(
    width = 480,
    height = 360,
    { gg_code() }
  )

  # --- # --- # Download graph # --- # --- #

  output$download_plot <- shiny::downloadHandler(
    filename = function(){
      fn <- paste0("plot ", Sys.time(), ".", input$file_type_plot)
      stringr::str_replace_all(fn, ":", "_")
    },
    content = function(file){
      ggplot2::ggsave(
        file,
        plot = gg_code(),
        width = input$plot_w,
        height = input$plot_h,
        units = "in",
        device = input$file_type_plot)
    }
  )

  # --- # --- # --- # Statistics # --- # --- # --- #

  # --- # --- # Update options # --- # --- #

  shiny::observe({
    if (input$stat_type == "d") {
      shiny::updateRadioButtons(session, "stat",
                         choices = list(
                           "descriptive statistics" = "descr",
                           "frequency tables" = "freq"))
    } else if (input$stat_type == "x") {
      shiny::updateRadioButtons(session, "stat",
                         choices = list(
                           "chi-square goodness of fit test" = "xs1",
                           "chi-square test of association" = "xs2"))
    } else if (input$stat_type == "t") {
      shiny::updateRadioButtons(session, "stat",
                         choices = list(
                           "one-sample t test" = "t1",
                           "paired samples t test" = "tp",
                           "two-sample t test" = "t2",
                           "ANOVA" = "anova"))
    } else {
      shiny::updateRadioButtons(session, "stat",
                         choices = list(
                           "correlation" = "corr",
                           "regression" = "regr"))
    }
  })

  shiny::observe({
    shiny::req(w_data())

    c_vars <- w_data() |> dplyr::select(dplyr::where(is.factor)) |> names()
    n_vars <- w_data() |> dplyr::select(dplyr::where(is.numeric)) |> names()

    if (input$stat %in% c("descr", "t1", "tp", "corr", "regr")) {
      v1_choices <- n_vars
    } else {
      v1_choices <- c_vars
    }

    if (input$stat == "xs2") {
      v2_choices <- c_vars
    } else if (input$stat == "descr" || input$stat == "freq") {
      v2_choices <- c("no groups" = ".", c_vars)
    } else {
      v2_choices <- n_vars
    }

    if (input$stat %in% c("t2", "anova", "regr")) {
      v1_name <- "Independent variable"
      v2_name <- "Dependent variable"
    } else if (input$stat == "descr" || input$stat == "freq") {
      v1_name <- "Variable to summarize"
      v2_name <- "Grouping variable (optional)"
    } else if (input$stat == "xs1" || input$stat == "t1") {
      v1_name <- "Variable to test"
      v2_name <- NULL
    } else {
      v1_name <- "Variable 1"
      v2_name <- "Variable 2"
    }

    shiny::updateSelectInput(session, "stat_v1", label = v1_name, choices = v1_choices)

    shiny::updateSelectInput(session, "stat_v2", label = v2_name, choices = v2_choices)
  })

  shiny::observeEvent(input$stat_v1, {
    if (input$stat %in% c("freq", "xs1", "xs2")) {
      shinyjqui::updateOrderInput(session, "change_v1", items = levels(w_data()[[input$stat_v1]]))
    }
  })

  shiny::observeEvent(input$stat_v2, {
    if (is.factor(w_data()[[input$stat_v2]])) {
      shinyjqui::updateOrderInput(session, "change_v2", items = levels(w_data()[[input$stat_v2]]))
    }
  })

  chi_wid <- shiny::reactive({
    shiny::req(input$stat_v1, input$stat == "xs1")
    levs <- levels(w_data()[[input$stat_v1]])
    val <- round(1/length(levs),2)
    cw <- lapply(1:length(levs), function (i) {
      ui_name <- paste("lev", i, "exp", sep = "_")
      shiny::numericInput(ui_name, levs[i], val)
    })
    do.call(tagList, cw)
  })

  output$chi_exp_wid <- shiny::renderUI({ chi_wid() })

  # --- # --- # Calculate stats # --- # --- #

  shiny::observeEvent(input$go_stats, {

    shiny::req(w_data())

    if (input$stat == "xs1" || input$stat == "t1" || input$stat_v2 == ".") {
      s <- w_data() |> dplyr::select(dplyr::all_of(input$stat_v1)) |> tidyr::drop_na()
      names(s) <- c("v1")
      vnames <- c(input$stat_v1)
    } else {
      s <- w_data() |> dplyr::select(dplyr::all_of(c(input$stat_v1, input$stat_v2))) |> tidyr::drop_na()
      names(s) <- c("v1", "v2")
      vnames <- c(input$stat_v1, input$stat_v2)
    }

    if (input$stat %in% c("freq", "xs1", "xs2")) {
      s$v1 <- factor(s$v1, levels = input$change_v1)
    }

    if (is.factor(s$v2)) s$v2 <- factor(s$v2, levels = input$change_v2)

    if (input$stat == "xs1") {
      levs <- levels(s$v1)
      v1_exp <- rep(1, length(levs))
      for (i in 1:length(levs)) {
        ui_name <- paste("lev", i, "exp", sep = "_")
        v1_exp[i] <- input[[ui_name]]
      }
    }

    if (input$stat == "descr") stats_out <- code_descriptives(s, vnames, input$dec)
    else if (input$stat == "freq") stats_out <- code_freqtable(s, vnames)
    else if (input$stat == "xs1") stats_out <- code_xsq_gof(s, vnames, exp_f = v1_exp)
    else if (input$stat == "xs2") stats_out <- code_xsq_toa(s, vnames)
    else if (input$stat == "corr") stats_out <- code_correlation(s, vnames)
    else if (input$stat == "tp") stats_out <- code_tpaired(s, vnames)
    else if (input$stat == "t1") stats_out <- code_t1sample(s, vnames, input$mu_null)
    else if (input$stat == "t2") stats_out <- code_t2sample(s, vnames)
    else stats_out <- code_linear_model(s, vnames, log_y = input$log_y,
                                        posthoc_test = input$tukey, assumption_test = input$assumptions)

    values$stats_text <- stats_out[[1]]
    output$stats_text <- shiny::renderUI(stats_out[[1]])

    if (input$stat %in% c("t1", "tp", "t2", "corr")) {
      values$stats_table_1 <- NULL
      output$stats_table_1 <- NULL
    } else {
      values$stats_table_1 <- stats_out[[2]]
      output$stats_table_1 <- gt::render_gt(stats_out[[2]], align = "left")
    }
    if (input$stat == "anova" && input$tukey) {
      values$stats_table_2 <- stats_out[[3]]
      output$stats_table_2 <- gt::render_gt(stats_out[[3]], align = "left")
    } else if (input$stat == "xs2") {
      values$stats_table_2 <- stats_out[[3]]
      output$stats_table_2 <- gt::render_gt(stats_out[[3]], align = "left")
    } else {
      values$stats_table_2 <- NULL
      output$stats_table_2 <- NULL
    }

    if ((input$stat == "anova" || input$stat == "regr") && input$assumptions) {
      output$stats_assumptions <- shiny::renderUI(stats_out[[4]])
    } else {
      output$stats_assumptions <- NULL
    }

  })

  down_wid <- shiny::eventReactive(input$go_stats, {
    shiny::req(values$stats_text)

    if (is.null(values$stats_table_1)) {
      w <- list("No downloads are available.")
    } else if (input$stat == "anova" && input$tukey == TRUE) {
      w <- list(
        shiny::strong("Download ANOVA table"),
        shiny::radioButtons("file_type_table_1", "", choices = c("docx", "rtf"), inline = T),
        shiny::downloadButton("download_table_1", "Download"),
        shiny::br(), shiny::br(),
        shiny::strong("Download Tukey table"),
        shiny::radioButtons("file_type_table_2", "", choices = c("docx", "rtf"), inline = T),
        shiny::downloadButton("download_table_2", "Download")
      )
    } else if (input$stat == "anova" || input$stat == "regr") {
      w <- list(
        shiny::strong("Download ANOVA table"),
        shiny::radioButtons("file_type_table_1", "", choices = c("docx", "rtf"), inline = T),
        shiny::downloadButton("download_table_1", "Download")
      )
    }	else {
      w <- list(
        shiny::strong("Download table"),
        shiny::radioButtons("file_type_table_1", "", choices = c("docx", "rtf"), inline = T),
        shiny::downloadButton("download_table_1", "Download")
      )
    }

    do.call(tagList, w)

  })

  output$download_widgets <- shiny::renderUI({ down_wid() })

  # --- # --- # Download tables # --- # --- #

  output$download_table_1 <- shiny::downloadHandler(
    filename = function(){
      fn <- paste0("table ", Sys.time(), ".", input$file_type_table_1)
      stringr::str_replace_all(fn, ":", "_")
    },
    content = function(file){
      gt::gtsave(values$stats_table_1, file)
    }
  )

  output$download_table_2 <- shiny::downloadHandler(
    filename = function(){
      fn <- paste0("table ", Sys.time(), ".", input$file_type_table_2)
      stringr::str_replace_all(fn, ":", "_")
    },
    content = function(file){
      gt::gtsave(values$stats_table_2, file)
    }
  )

  # --- # --- # Info tab items # --- # --- #

  output$dune_metadata <- shiny::renderTable(
    dune_metadata <- get_metadata_table("dunes")
  )

  output$fhs_metadata <- shiny::renderTable(
    fhs_metadata <- get_metadata_table("heart")
  )


  # --- # --- # --- # Exit # --- # --- # --- #

  shiny::observeEvent(input$exit_app, {
    output$exit_message <- shiny::renderText("Exiting in 3 seconds...")
    shinyjs::delay(1000, output$exit_message <- shiny::renderText("Exiting in 2 seconds..."))
    shinyjs::delay(2000, output$exit_message <- shiny::renderText("Exiting in 1 second..."))
    shinyjs::delay(3000, shinyjs::js$closeWindow())
    shinyjs::delay(3050, shiny::stopApp())
  })

  session$onSessionEnded(shiny::stopApp)
}

