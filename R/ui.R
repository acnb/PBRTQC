ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "PBRTQC simulation"),
  shinydashboard::dashboardSidebar(disable = TRUE),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shinydashboard::box(
                title = 'Instructions',
                width = 4,
                shiny::p(
                    paste(
                        "Please provide a file comma separated",
                        "values file (.csv) with a 'column' day and a column",
                        "'measurement'. The days should be numbered consecutively.",
                        "Measurements should only contain",
                        "numeric values (not someting like '>1'). The '.'",
                        "character needs to be used for decimal points."
                    )
                ),
                shiny::p(
                  shiny::a("This file", href = "https://www.bietenbeck.net/NA.csv", target = '_blank'),
                    "contains an example with sodium measurements."
                )
            ),
            shinydashboard::box(
                title = "Data input",
                width = 4,
                shiny::fileInput(
                    "file1",
                    "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                ),
                
                # Input: Checkbox if file has header ----
                shiny::checkboxInput("header", "Header", TRUE),
                
                # Input: Select separator ----
                shiny::radioButtons(
                    "sep",
                    "Separator",
                    choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                    ),
                    selected = ","
                ),
                
                # Input: Select quotes ----
                shiny::radioButtons(
                    "quote",
                    "Quote",
                    choices = c(
                        None = "",
                        "Double Quote" = '"',
                        "Single Quote" = "'"
                    ),
                    selected = '"'
                )
            ),
            shinydashboard::box(
                title = "Measurements per day",
                width = 4,
                shiny::plotOutput("plotMeasPerDay"),
                shiny::textOutput("nDays")
            )
        ),
        shinyjs::hidden(shiny::fluidRow(
            class = "statistics",
            shinydashboard::box(
                title = "Distribution of measurements after truncation",
                width = 12,
                shiny::plotOutput("plotOverviewPerDay")
            ),
        )),
        shinyjs::hidden(shiny::fluidRow(
            class = "settings",
            shinydashboard::box(
                title = "Settings",
                width = 12,
                shiny::plotOutput("plotOverview"),
                shiny::uiOutput('slider'),
                shiny::numericInput(
                    'perc',
                    "Acceptable number of days with false alarm (%)",
                    min = 0,
                    max = 15,
                    value = 10
                ),
                shiny::numericInput(
                    'bias',
                    "Allowable bias (%)",
                    value = 3,
                    min = 1,
                    max = 50
                ),
                shiny::selectInput(
                    "algo",
                    'Algorithm',
                    choices =
                        c(
                            'Moving Mean with Winsorizing' = 'mean',
                            'Moving Mean with Removal of Outliers' = 'mean.del',
                            'Exponential (weighted) Moving Average with Winsorizing' = 'ema',
                            'Exponential (weighted) Moving Average with Removal of Outliers' = 'ema.del',
                            'Moving Median with Winsorizing' = 'median',
                            'Moving Median with Removal of Outliers' = 'median.del'
                        )
                ),
                shiny::radioButtons(
                    "algorithmMode",
                    "Algorithm mode",
                    choices = c(
                        "Algorithm is restarted each day" = "newEachDay",
                        "Algorithm reuses values from previous days" = "continuous"
                    ),
                    selected = "newEachDay"
                ),
                shiny::actionButton ("sim", "   Simulate", icon = 
                                       shiny::icon("cogs"))
            )
        )),
        shinyjs::hidden(shiny::fluidRow(
            class = "results",
            shinydashboard::box(
                title = "Best Overall MNPed",
                id = "MNPedbox",
                width = 4,
                shiny::plotOutput("plotMNPedOverall"),
                shiny::verbatimTextOutput("textMNPedOverall")
            ),
            shinydashboard::box(
                title = "Best Overall 95NPed",
                id = "q95Pedbox",
                width = 4,
                shiny::plotOutput("plot95NPedOverall"),
                shiny::verbatimTextOutput("text95NPedOverall")
            ),
           shinydashboard::box(
                title = "best settings based on allowable bias",
                id = "biasbox",
                width = 4,
                shiny::plotOutput("plotBestBias"),
                shiny::verbatimTextOutput("textBestBias")
            )
        )),
        shiny::fluidRow(shinydashboard::box(
            title = "",
            width = 12,
            shiny::uiOutput("info")
        ))
    )
)