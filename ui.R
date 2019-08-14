library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(
    dashboardHeader(title = "PBRTQC simulation"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        useShinyjs(),
        fluidRow(
            box(title = "Data input", width = 6,
                fileInput("file1", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                
                # Input: Select separator ----
                radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons("quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"')
            ),
          box(title = 'Instructions', 
              width = 6,
              p(paste("Please provide a file comma separated",
                    "values file (.csv) with a 'column' day and a column",
                    "'measurement'. The days should be numbered consecutively.",
                    "Measurements should only contain",
                    "numeric values (not someting like '>1'). The '.'",
                    "character needs to be used for decimal points.")),
              p(a("This file", href="https://www.bietenbeck.net/NA.csv"),
                "contains an example with sodium measurements."
                ))   
        ),
        hidden(
        fluidRow(class = "settings",
            box(title = "Settings", width = 12,
                plotOutput("plotOverview"),
                uiOutput('slider'),
                numericInput('perc', "Acceptable number of days with false alarm (%)", 
                             min = 0, max = 15, value = 10),
                numericInput('bias', "Allowable bias (%)", value = 3, min = 1, max=50), 
                selectInput("algo", 'Algorithm', choices =
                                c('Moving Mean with Winsorizing' = 'mean', 
                                  'Moving Mean with Removal of Outliers' = 'mean.del',
                                  'Exponential (weighted) Moving Average with Winsorizing' = 'ema',
                                  'Exponential (weighted) Moving Average with Removal of Outliers' = 'ema.del',
                                  'Moving Median with Winsorizing' = 'median', 
                                  'Moving Median with Removal of Outliers' = 'median.del'
                                  )),
                actionButton("sim", "   Simulate", icon = icon("cogs"))
                )
        )),
        hidden(
        fluidRow(class = "results", 
            box(title = "Best Overall MNPed",  id = "MNPedbox",
                width = 4,
                plotOutput("plotMNPedOverall"),
                verbatimTextOutput("textMNPedOverall")
                ),
            box(title = "Best Overall 95NPed", id = "q95Pedbox",
                width = 4,
                plotOutput("plot95NPedOverall"),
                verbatimTextOutput("text95NPedOverall")
            ),
            box(title = "best settings based on allowable bias", id = "biasbox",
                width = 4,
                plotOutput("plotBestBias"),
                verbatimTextOutput("textBestBias")
            )
        )),
        fluidRow(
            box(title = "",
                width = 12,
                uiOutput("info")
            )
        )
    )
)