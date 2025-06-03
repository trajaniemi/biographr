#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(

      shinydashboard::dashboardHeader(
        title = "bioGraphR"
      ),

      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Data", tabName = "data_tab"),
          shinydashboard::menuItem("Graphs", tabName = "graph_tab"),
          shinydashboard::menuItem("Statistics", tabName = "stats_tab"),
          shinydashboard::menuItem("Info",   		tabName = "info_tab"),
          shiny::actionButton("exit_app", "Exit"),
          shiny::textOutput("exit_message")
        )
      ),

      shinydashboard::dashboardBody(

        shinyjs::useShinyjs(),
		shinyjs::extendShinyjs(
			text = "shinyjs.closeWindow = function() { window.close(); }",
			functions = c("closeWindow")),
	  
	  shinydashboard::tabItems(

          # --- # --- # Data tab # --- # --- #
          shinydashboard::tabItem(tabName = "data_tab",
                  shiny::fluidRow(
                    shiny::column(width = 4,
                           shinydashboard::box(title = "Open a dataset", status = "primary",
                               solidHeader = TRUE, width = NULL,

                               "You may use a practice dataset, or upload your own data.",
                               shiny::radioButtons("data_type", "",
                                            choices = list(
                                              "Dune environment" = 1,
                                              "Framingham heart study" = 2,
                                              "Upload a file (.xlsx or .csv)" = 3
                                            ),
                                            selected = 1
                               ),
                               shiny::conditionalPanel("input.data_type == 3",
                                                "Select a file: ",
                                                shiny::fileInput("data_file", "", multiple = FALSE)
                               ),
                               shiny::actionButton("go_data", "LOAD DATA")
                           )
                    ),
                    shiny::column(width = 8,
                           shinydashboard::box(title = "Data summary", status = "primary",
                               solidHeader = TRUE, width = NULL,
                               shiny::uiOutput("data_summ")
                           )
                    )
                  )
          ),

          # --- # --- # Graph tab # --- # --- #
          shinydashboard::tabItem(tabName = "graph_tab",
                  shiny::fluidRow(
                    shiny::column(4,
                           shinydashboard::box(title = "Graphs", width = NULL,
                               status = "primary", solidHeader = TRUE,

                               "Choose a graph type, variables, and options below. Then click 'PLOT' to plot the graph.",
                               shiny::br(), shiny::br(),
                               shiny::actionButton("go_plot", "PLOT")
                           ),

                           shinydashboard::tabBox(width = NULL,

                                  shiny::tabPanel("Graph type",
                                           shiny::radioButtons("g_type", "",
                                                        choices = list(
                                                          "histogram" = "hist",
                                                          "frequency plot" = "freq",
                                                          "boxplot" = "box",
                                                          "bar graph" = "bar",
                                                          "scatterplot" = "scatter"
                                                        )
                                           )
                                  ),

                                  shiny::tabPanel("Variables",
                                           shiny::conditionalPanel(
                                             "input.g_type == 'freq' || input.g_type == 'box' || input.g_type == 'bar'",
                                             shiny::selectInput("x_cat", "X variable", choices = ""),
                                             shinyjqui::orderInput("change_x", "Drag and drop values to reorder", items = "")
                                           ),
                                           shiny::conditionalPanel(
                                             "input.g_type == 'hist' || input.g_type == 'scatter'",
                                             shiny::selectInput("x_num", "X variable", choices = "")
                                           ),
                                           shiny::conditionalPanel(
                                             "input.g_type == 'box' || input.g_type == 'bar' || input.g_type == 'scatter'",
                                             shiny::selectInput("y_var", "Y variable", choices = "")
                                           ),
                                           shiny::selectInput("z_var", "Grouping variable", choices = ""),
                                           shiny::conditionalPanel(
                                             "input.z_var != '.'",
                                             shinyjqui::orderInput("change_z", "Drag and drop values to reorder", items = "")
                                           )
                                  ),

                                  shiny::tabPanel("Options",
                                           shiny::conditionalPanel(
                                             "input.g_type == 'freq' || input.g_type == 'box' || input.g_type == 'bar'",
                                             shiny::textInput("lab_xcat", "X axis label", "")
                                           ),
                                           shiny::conditionalPanel(
                                             "input.g_type == 'hist' || input.g_type == 'scatter'",
                                             shiny::textInput("lab_xnum", "X axis label", "")
                                           ),
                                           shiny::conditionalPanel(
                                             "input.g_type == 'box' || input.g_type == 'bar' || input.g_type == 'scatter'",
                                             shiny::textInput("lab_y", "Y axis label", "")
                                           ),
                                           shiny::conditionalPanel(
                                             "input.z_var != '.'",
                                             shiny::textInput("lab_z", "Grouping variable label", "")
                                           ),
                                           shiny::conditionalPanel(
                                             "input.g_type == 'hist'",
                                             shiny::numericInput("bins", "# of bins", 10)
                                           ),
                                           shiny::conditionalPanel(
                                             "input.g_type == 'bar'",
                                             shiny::radioButtons("error_bars", "Error bars",
                                                          choices = list(
                                                            "none" = "none",
                                                            "standard deviation" = "sd",
                                                            "standard error" = "se"
                                                          ), selected = "none")
                                           ),
                                           shiny::conditionalPanel(
                                             "input.g_type == 'scatter'",
                                             shiny::numericInput("sym_size", "Symbol size", 1),
                                             shiny::checkboxInput("rline", "Show regression line", FALSE)
                                           ),
                                           shiny::numericInput("font_size", "Font size", 12)
                                  ),

                                  shiny::tabPanel("Download",
                                           shiny::radioButtons("file_type_plot", "File type", choices = c("png", "pdf", "jpg"), inline = T),
                                           shiny::numericInput("plot_w", "Image width (in)", 4),
                                           shiny::numericInput("plot_h", "Image height (in)", 3),
                                           shinyjs::hidden(downloadButton("download_plot", "Download"))
                                  )
                           )
                    ),
                    shiny::column(8,
                           shinydashboard::box(title = "Graph output", width = NULL,
                               status = "primary", solidHeader = TRUE,
                               shiny::plotOutput("out_ggplot")
                           )
                    )
                  )
          ),

          # --- # --- # Stats tab # --- # --- #
          shinydashboard::tabItem(tabName = "stats_tab",
                  shiny::fluidRow(
                    shiny::column(width = 3,
                           shinydashboard::box(title = "Statistics", status = "primary",
                               solidHeader = TRUE, width = NULL,
                               "Choose an analysis, variables, and options below. Then click 'CALCULATE' to get results.",
                               shiny::br(), shiny::br(),
                               shiny::actionButton("go_stats", "CALCULATE")
                           ),

                           shinydashboard::tabBox(width = NULL,

                                  shiny::tabPanel("Analysis",
                                           shiny::radioButtons("stat_type", "",
                                                        choices = list(
                                                          "summarize data (descriptives, frequency tables)" = "d",
                                                          "compare means (t-tests, ANOVA)" = "t",
                                                          "compare frequencies (chi square tests)" = "x",
                                                          "test associations (correlation and regression)" = "r"
                                                        )
                                           ),
                                           shiny::radioButtons("stat", "", choices = "")
                                  ),

                                  shiny::tabPanel("Variables",
                                           shiny::selectInput("stat_v1", label = "", choices = ""),
                                           shiny::conditionalPanel("input.stat == 'freq' || input.stat == 'xs1' || input.stat == 'xs2'",
                                                            shinyjqui::orderInput("change_v1", "Drag and drop values to reorder", items = "")
                                           ),
                                           shiny::conditionalPanel("input.stat != 't1' && input.stat != 'xs1'",
                                                            shiny::selectInput("stat_v2", label = "", choices = "")
                                           ),
                                           shiny::conditionalPanel(
                                             "(input.stat == 'descr' && input.stat_v2 != '.') ||
									 (input.stat == 'freq' && input.stat_v2 != '.') ||
									 input.stat == 'xs2'",
                                             shinyjqui::orderInput("change_v2", "Drag and drop values to reorder", items = "")
                                           )
                                  ),

                                  shiny::tabPanel("Options",
                                           shiny::conditionalPanel("input.stat == 'freq'",
                                                            shiny::numericInput("dec", "Maximum decimal places to report", 3)
                                           ),
                                           shiny::conditionalPanel("input.stat == 't1'",
                                                            shiny::numericInput("mu_null", HTML("Hypothesized value of &mu;"), 0)
                                           ),
                                           shiny::conditionalPanel("input.stat == 'anova' || input.stat == 'regr'",
                                                            shiny::checkboxInput("log_y", "transform y -> ln(y)", FALSE)
                                           ),
                                           shiny::conditionalPanel("input.stat == 'anova'",
                                                            shiny::checkboxInput("tukey", "Tukey post-hoc tests", FALSE)
                                           ),
                                           shiny::conditionalPanel("input.stat == 'anova' || input.stat == 'regr'",
                                                            shiny::checkboxInput("assumptions", "Test assumptions", FALSE)
                                           ),
                                           shiny::conditionalPanel("input.stat == 'xs1'",
                                                            "Enter hypothesized relative frequencies. Values should sum to 1.",
                                                            shiny::uiOutput("chi_exp_wid")
                                           )
                                  ),

                                  shiny::tabPanel("Download",
                                           shiny::uiOutput("download_widgets")
                                  )
                           )
                    ),
                    shiny::column(width = 8,
                           shinydashboard::box(title = "Statistics results", width = NULL,
                               status = "primary", solidHeader = TRUE,
                               shiny::uiOutput("stats_text"),
                               gt::gt_output("stats_table_1"),
                               gt::gt_output("stats_table_2"),
                               shiny::uiOutput("stats_assumptions")
                           )
                    )
                  )
          ),

          # --- # --- # Info tab # --- # --- #
          shinydashboard::tabItem(tabName = "info_tab",
                  shiny::fluidRow(
                    shiny::column(width = 12,
                           shinydashboard::box(title = "Using the app", status = "primary",
                               solidHeader = TRUE, width = NULL,

                               shiny::h3("Data input"),
                               "Data files may be uploaded as an Excel file (.xls or .xlsx) or comma-delimited text (.csv).", br(), br(),
                               "Files should be formatted as follows:",
                               tags$ul(
                                 tags$li("In Excel files, the data are on the first worksheet."),
                                 tags$li("Each column is a single variable, and each row is a single case (observational unit)."),
                                 tags$li("The first row contains variable names. Variable names begin with a letter and do not contain spaces."),
                                 tags$li("Missing values are indicated by an empty cell or a cell containing NA.")
                               ),

                               shiny::hr(),
                               shiny::h3("Variable types"),
                               "Variables are automatically classified as categorical or numeric.", shiny::br(), shiny::br(),
                               "If your variable should be numeric, but is not recognized as numeric, check your Excel file to make sure the values in the column contain only numbers, and no letters or other text. (Cells with missing values may have NA in them.)", shiny::br(), shiny::br(),
                               "If your variable should be categorical, but is classified as numeric, use letters (or a combination of letters and numbers) instead of numbers to code the values.", shiny::br(), shiny::br(),
                               "If variables continue to be assigned incorrectly, try saving the file in .csv format.",

                               shiny::hr(),
                               shiny::h3("Data choices"),
                               "Variable choices are based on the type of graph or statistical analysis selected.", shiny::br(), shiny::br(),
                               "Observations with missing values of any of the selected variables are excluded from the graph or analysis.",

                               shiny::hr(),
                               shiny::h3("Downloads"),
                               "Graphs can be downloaded as .png, .pdf, or .jpg files. A font size of 10 is recommended for download. The font size can be changed in the 'Font' options tab.",
                               "Tables can be downloaded as Word files (.docx) or in rich text format (.rtf), which can be opened by most text editing software.",

                               shiny::hr(),
                               shiny::h3("About"),
                               "BioGraphR is designed to produce graphs and statistical analyses needed by biology undergraduates in lab courses.", shiny::br(), shiny::br(),
                               "This app was written in R version 4.4.3 in 2024 by Tara K. Rajaniemi."
                           ),

                           shinydashboard::box(
                             title = "Practice datasets", status = "primary",
                             solidHeader = TRUE, width = NULL,

                             shiny::h3("Dune environment"),
                             "These data are from the coastal dunes at Waquoit Bay Estuarine Research Reserve. Data collected by Dr. Tara Rajaniemi in 2006 and 2007.",
                             shiny::br(), shiny::br(),
                             shiny::strong("Metadata:"),
                             shiny::tableOutput("dune_metadata"),

                             shiny::h3("Framingham heart study"),
                             "This is a teaching dataset derived from the Framingham Heart Study, made available by the National Heart, Lung, and Blood Institute at https://biolincc.nhlbi.nih.gov/teaching/. Data here are a subset of the complete dataset (only the first observation period, for patients with no previous heart disease). Some variables have been removed or recoded.",
                             shiny::br(), shiny::br(),
                             shiny::strong("Metadata:"),
                             shiny::tableOutput("fhs_metadata")
                           )
                    )
                  )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "biographr"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "UMDstyle.css"),
    shinyjs::useShinyjs()

  )
}
