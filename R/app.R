app <- function(...){
  version <- "0.3.0"
  
  options(shiny.maxRequestSize=7*1024^2)
  
  createPlot <- function(data, accBias, maxFirstDeceted){
    ggplot2::ggplot(data,
                    ggplot2::aes(x=bias, y=MNPed,
                                 ymax=q95NPed, ymin = MNPed ))+
      ggplot2::geom_vline(xintercept = accBias) +
      ggplot2::geom_vline(xintercept = -accBias) +
      ggplot2::geom_line(color = "red") +
      ggplot2::coord_cartesian(ylim = c(0, maxFirstDeceted)) +
      ggplot2::geom_ribbon(alpha = .2, colour = NA, show.legend = FALSE,
                           fill = 'red')+
      ggplot2::scale_color_discrete(name = "") +
      ggplot2::scale_x_continuous(labels = scales::percent) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1))
  }
  
  algList <- list(
    'mean' = 'Moving Mean with Winsorizing' , 
    'mean.del' = 'Moving Mean with Removal of Outliers',
    'ema' = 'Exponential (weighted) Moving Average with Winsorizing',
    'ema.del' = 'Exponential (weighted) Moving Average with Removal of Outliers',
    'median' = 'Moving Median with Winsorizing', 
    'median.del' = 'Moving Median with Removal of Outliers',
    'regAdjEMA' = 'Regression adjusted PBRTQC'
  )
  
  dataToText <- function(data, tll, tul){
    res <- paste("Settings:\n", 'truncation:', tll, '-', tul, "\n")
    if(is.na(data$blockSize[1])){
      res <- paste(res, "No appropriate settings found.")
    }
    else{
      text <- data |>
        dplyr::mutate(t = paste('algorithm:', algList[[type]], "\n", 
                                'block size:', blockSize, "\n",
                                "control limits:", lcl, '-', ucl))
      
      res <- paste(res, text$t, collapse = "\n")
    }
    
    res
  }
  
  server <- function(input, output) {
    csvData <- shiny::reactive({ 
      shiny::req(input$file1)
      
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          
          if(!input$header){
            colnames(df) <- c('day', 'measurement')
          }
          
          
          if(!("day" %in% colnames(df) & 'measurement' %in% colnames(df))){
            showNotification("Please upload csv file with columns 'day' and 'measurement'.",
                             type = "error")
            return(NULL)
          }
          
          if(!all(is.numeric(df$measurement))){
            df <- df |>
              dplyr::filter(is.numeric(measurement))
            showNotification("Non-numeric measurements are removed.", type = "warning")
          }
          
          nDays <- length(unique(df$day))
          overall <- nrow(df)
          
          showNotification(paste(overall, "measurements on", nDays, " days uploaded."), 
                           type = "message")
          
          df
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          showNotification(e, type = "error")
          stop(safeError(e))
        }
      )
      
      
    })
    
    minTruncation <- reactive({ 
      input$truncation[1]
    })
    
    maxTruncation <- reactive({ 
      input$truncation[2]
    })
    
    output$slider <- renderUI({
      min <- min(csvData()$measurement)
      max <- max(csvData()$measurement)
      
      sliderInput("truncation", label = "truncation", 
                  value = c(min ,max), min=min, max=max, step = (max-min)/500)
    })
    
    output$plotMeasPerDay <- renderPlot({
      shiny::req(csvData())
      
      measPerDay <- csvData() |>
        dplyr::count(day)
      
      ggplot2::ggplot(measPerDay, ggplot2::aes(y=n)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(y = "Number of measurements per day") +
        ggplot2::guides(x = "none")
    })
    
    observeEvent(ignoreInit = TRUE, 
                 list(input$file1, input$header, input$sep, input$quote), {
      if(!is.null(csvData())){
        shinyjs::show(selector = ".settings")
        shinyjs::show(selector = ".statistics")
      }  
      else{
        shinyjs::hide(selector = ".settings")
        shinyjs::hide(selector = ".statistics")
      }
      shinyjs::hide(selector = ".results")
    })
    
    observeEvent(input$truncation, {
      rangeTruncation <- maxTruncation() - minTruncation()
      
      
      output$plotOverview <- renderPlot({
        ggplot2::ggplot(csvData() , ggplot2::aes(x=measurement)) +
          ggplot2::geom_density() +
          ggplot2::geom_vline(xintercept = minTruncation(), color = 'red') +
          ggplot2::geom_vline(xintercept = maxTruncation(), color = 'red') +
          ggplot2::coord_cartesian(xlim = c(pmax(min(csvData()$measurement), minTruncation()-0.05*rangeTruncation),
                                            pmin(max(csvData()$measurement), maxTruncation()+0.05*rangeTruncation)))
      })
      
      statsPerDay <- csvData() |>
        dplyr::mutate(measurement = dplyr::if_else(measurement > maxTruncation(), as.double(maxTruncation()), 
                                                   as.double(measurement))) |>
        dplyr::mutate(measurement = dplyr::if_else(measurement < minTruncation(), as.double(minTruncation()),
                                                   as.double(measurement))) |>
        dplyr::group_by(day) |>
        dplyr::summarise(
          min = min(measurement),
          `25%` = quantile(measurement, 0.25),
          median = median(measurement),
          `75%` = quantile(measurement, 0.75),
          max = max(measurement)
        ) |>
        tidyr::pivot_longer(!day, names_to = "stat", values_to = 'val') |>
        dplyr::mutate(stat = factor(stat, levels= c('max', '75%', 'median', '25%', 'min')))
      
      
      
      output$plotOverviewPerDay <- renderPlot({
        ggplot2::ggplot(statsPerDay, ggplot2::aes(x=day, y=val, color=stat)) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x') +
          ggplot2::geom_hline(yintercept = minTruncation(), color = 'red') +
          ggplot2::geom_hline(yintercept = maxTruncation(), color = 'red') +
          ggplot2::coord_cartesian(ylim = c(minTruncation()-0.05*rangeTruncation,
                                            maxTruncation()+0.05*rangeTruncation)) +
          ggplot2::scale_color_manual(values=c("#fdae61", "#abd9e9",
                                               "#2c7bb6", "#abd9e9", '#fdae61'),
                                      labels = c('max', '75%', 'median', '25%', 'min'))
      })
      
      nDays <- length(unique(csvData()$day))
      overall <- nrow(csvData())
      
      output$nDays <- renderText({ paste(overall, "measurements on",
                                         nDays, " days uploaded." )})
      
    })
    
    observeEvent(input$sim, {
      shinyjs::disable("sim")
      
      
      # changes these to allow more simulations
      biases <- input$bias/100 * c(1.5, -1, -.5, 0, .5, 1, 1.5)
      blockSizes <- seq(10, 150, by = 35) 
      
      funcsAll <- switch (input$algo,
                          'mean' = list("mean" = rollMean),
                          'mean.del' = list("mean.del" = rollMean.del),
                          'median' = list( 'median' = rollMed),
                          'median.del' = list('median.del' = rollMed.del),
                          'ema' = list('EMA' = truncatedEMA),
                          'ema.del' = list('EMA.del' = truncatedEMA.del),
                          'regAdjEMA' = list('regAdjEMA' = regAdjEMA)
      )
      
      data <- csvData()
      
      truncationLimits <- data.frame('lowerTrunc' = minTruncation(),
                                     'upperTrunc' = maxTruncation(), 
                                     'truncation' = 'userSelected')
      
      startTime <- Sys.time()
      
      calcContinous <- input$algorithmMode == 'continuous'
      
      res <- shiny::withProgress(message = 'Simulation in progress',
                                 simPBRTQC(data,
                                           blockSizes, truncationLimits, biases, funcsAll, 
                                           input$perc/100, calcContinous)
      )
      stopTime <- Sys.time()
      
      timeDiff <- difftime(stopTime, startTime, units = 'mins')
      
      showNotification(paste(round(timeDiff, 2), "minutes passed."),
                       type = "warning")
      showNotification("Evaluating results...")
      
      resultEval <- res |>
        dplyr::group_by(blockSize, truncation, bias, type) |>
        dplyr::summarise(ANPed = dplyr::if_else(
          sum(is.finite(firstDetected))/dplyr::n() > .95,
          mean(dplyr::if_else(is.finite(firstDetected), firstDetected, 1000)),
          NA_real_),
          q95NPed= quantile(firstDetected, probs = .95),
          MNPed = median(firstDetected),
          q975 = quantile(firstDetected, probs = .975),
          q025 = quantile(firstDetected, probs = .025),
          n = dplyr::n())
      
      highestDetected <- res |>
        dplyr::filter(is.finite(firstDetected)) |>
        dplyr::pull(firstDetected) |>
        max()
      
      bestSettingsByAlg <- resultEval |>
        dplyr::mutate(MNPed = dplyr::if_else(is.finite(MNPed), MNPed, highestDetected*1.1)) |>
        dplyr::mutate(q95NPed = dplyr::if_else(is.finite(q95NPed), q95NPed, highestDetected*1.1)) |>
        dplyr::group_by(type, blockSize) |>
        dplyr::summarise(sumMNped = sum(MNPed), sumq95NPed = sum(q95NPed))
      
      bestSettingsByAlgOverall <- bestSettingsByAlg |>
        dplyr::group_by(type) |>
        dplyr::filter(sumMNped == min(sumMNped)) |>
        dplyr::filter(sumq95NPed == min(sumq95NPed)) |>
        dplyr::filter(blockSize == min(blockSize)) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
      
      bestSettingsByAlgSafest <- bestSettingsByAlg |>
        dplyr::group_by(type) |>
        dplyr::filter(sumq95NPed == min(sumq95NPed)) |>
        dplyr::filter(sumMNped == min(sumMNped)) |>
        dplyr::filter(blockSize == min(blockSize)) |>
        dplyr::slice(1) |>
        dplyr::ungroup()
      
      bestSettingsEarliestPos <- resultEval |>
        dplyr::filter(bias == input$bias/100) |>
        dplyr::filter(is.finite(MNPed)) |>
        dplyr::group_by(type) |>
        dplyr::filter(MNPed == min(MNPed)) |>
        dplyr::filter(q95NPed == min(q95NPed)) |>
        dplyr::filter(blockSize == min(blockSize)) |>
        dplyr::slice(1) |>
        dplyr::select(type, blockSize) |>
        dplyr::ungroup()
      
      cls <- res |>
        dplyr::select(type, blockSize, lcl, ucl) |>
        unique()|>
        dplyr::ungroup()
      
      overall <- resultEval |>
        dplyr::inner_join(bestSettingsByAlgOverall, by = c("type", "blockSize")) |>
        dplyr::left_join(cls, by= c("type", "blockSize"))
      
      safest <-  resultEval |>
        dplyr::inner_join(bestSettingsByAlgSafest, by = c("type", "blockSize")) |>
        dplyr::left_join(cls, by= c("type", "blockSize"))
      
      bestBias <-  resultEval |>
        dplyr::inner_join(bestSettingsEarliestPos, by = c("type", "blockSize")) |>
        dplyr::left_join(cls, by= c("type", "blockSize"))
      
      output$plotMNPedOverall <- shiny::renderPlot(createPlot(overall, input$bias/100, highestDetected))
      output$plot95NPedOverall <- shiny::renderPlot(createPlot(safest, input$bias/100, highestDetected))
      output$plotBestBias <- shiny::renderPlot(createPlot(bestBias, input$bias/100, highestDetected))
      
      
      output$text95NPedOverall <- 
        shiny::renderText(dataToText(safest[1,], minTruncation(), 
                                     maxTruncation()))
      output$textMNPedOverall <- 
        shiny::renderText(dataToText(overall[1,], minTruncation(), 
                                     maxTruncation()))
      output$textBestBias <- 
        shiny::renderText(dataToText(bestBias[1,], minTruncation(), 
                                     maxTruncation()))
      
      #Advanced Simulation output
      if(exists("tempStore")){
        advanced_results <- analyse_models(tempStore)

        purrr::walk2(advanced_results[["plot"]], 
                     seq_along(advanced_results[["plot"]]),
                     function(p, idx){
                       output[[paste("advancedPlots", idx, sep = "_")]] <- 
                         shiny::renderPlot({p})
                       }
                     )


        output$advancedPlots <-
          shiny::renderUI({

            plot_output_list <-
              purrr::map2(advanced_results[['plot']],
                          seq_along(advanced_results[["plot"]]),
                          function(p, idx){
                           plotname <- paste("advancedPlots", idx, sep = "_")
                           shiny::plotOutput(plotname, inline=TRUE)
                           }
                         )
            
            do.call(shiny::tagList, plot_output_list)
          })
      }
      
      
      shinyjs::enable("sim")
      shinyjs::show(selector = ".results")
    })
    
    url <- a("here", href="https://github.com/acnb/PBRTQC/", target='_blank')
    
    output$info <- shiny::renderUI({p('PBRTQC simulation version',
                                      version, 
                                      "by Andreas Bietenbeck.",
                                      br(),
                                      "Use at your own risk.",
                                      "Further instructions",
                                      "and bug reports can be found",
                                      a("here", href="https://github.com/acnb/PBRTQC/"),
                                      ".")})
  }
  
  
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
                'Moving Median with Removal of Outliers' = 'median.del',
                'Regression Adjusted PBRTQC' = 'regAdjEMA'
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
      shinyjs::hidden(
        shiny::fluidRow(
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
        ),
        shiny::fluidRow(
          class = "results",
          shinydashboard::box(
            title = "Advanced Plots",
            id = "advancedPlots",
            shiny::uiOutput("advancedPlots"),
            width = 6
          ),
          shinydashboard::box(title = "Advanced Data", id = "advancedTables", width = 6, )
        )),
      shiny::fluidRow(shinydashboard::box(
        title = "",
        width = 12,
        shiny::uiOutput("info")
      ))
    )
  )
  
  shiny::shinyApp(ui, server, ...)
}