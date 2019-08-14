library(shiny)
library(shinydashboard)
library(shinyjs)
library(here)
library(tidyverse)
library(rowr)

version <- "0.1.0"

options(shiny.maxRequestSize=7*1024^2)

createPlot <- function(data, accBias, maxFirstDeceted){
    ggplot(data,
           aes(x=bias, y=MNPed,
               ymax=q95NPed, ymin = MNPed ))+
        geom_vline(xintercept = accBias) +
        geom_vline(xintercept = -accBias) +
        geom_line(color = "red") +
        coord_cartesian(ylim = c(0, maxFirstDeceted)) +
        geom_ribbon(alpha = .2, colour = NA, show.legend = FALSE,
                    fill = 'red')+
        scale_color_discrete(name = "") +
        scale_x_continuous(labels = scales::percent) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1))
}

algList <- list(
    'mean' = 'Moving Mean with Winsorizing' , 
    'mean.del' = 'Moving Mean with Removal of Outliers',
    'ema' = 'Exponential (weighted) Moving Average with Winsorizing',
    'ema.del' = 'Exponential (weighted) Moving Average with Removal of Outliers',
    'median' = 'Moving Median with Winsorizing', 
    'median.del' = 'Moving Median with Removal of Outliers')


dataToText <- function(data, tll, tul){
    res <- paste("Settings:\n", 'truncation:', tll, '-', tul, "\n")
    if(is.na(data$blockSize[1])){
        res <- paste(res, "No appropriate settings found.")
    }
    else{
        text <- data %>%
            mutate(t = paste('algorithm:', algList[[type]], "\n", 
                             'block size:', blockSize, "\n",
                             "control limits:", lcl, '-', ucl))
        
        res <- paste(res, text$t, collapse = "\n")
    }

    res
}

replaceNAWithPrevious <- function(vecWoNA, NApos){
    if(length(NApos) == 0){
        return(vecWoNA)
    }
    else{
        pos <- pmax(NApos-(1:length(NApos)), 1)
        return(vecWoNA[sort(c(pos, 1:length(vecWoNA)))])
    }
}


rollingFunc <- function(f, ...) {
    function(measurement, blockSize, ...){
        c(rep(NA_real_, blockSize - 1),
          rollApply(measurement,
                    fun = f,
                    minimum = blockSize,
                    window = blockSize, 
                    align = "right",
                    ...))
    }
}


truncatedMean <- function(measurement, ll, ul){
    measurement[measurement < ll] <- ll
    measurement[measurement > ul] <- ul
    mean(measurement)
}

logMean <- function(measurement, ll, ul){
    measurement[measurement < ll] <- ll
    measurement[measurement > ul] <- ul
    measurement <- measurement + 10^5
    mean(log(measurement))
}

truncatedMed <- function(measurement, ll, ul){
    measurement[measurement < ll] <- ll
    measurement[measurement > ul] <- ul
    median(measurement)
}

truncatedEMA <- function(measurement, blockSize, ll, ul){
    measurement[measurement < ll] <- ll
    measurement[measurement > ul] <- ul
    
    if(length(measurement) <= blockSize){
        return(rep(NA, length(measurement)))
    }
    else{
        EMA(measurement, n = blockSize)
    }
}

truncatedlogEMA <- function(measurement, blockSize, ll, ul){
    measurement[measurement < ll] <- ll
    measurement[measurement > ul] <- ul
    measurement <- measurement + 10^5
    
    if(length(measurement) <= blockSize){
        return(rep(NA, length(measurement)))
    }
    else{
        EMA(log(measurement), n = blockSize)
    }
}

truncatedEMA.del <- function(measurement, blockSize, ll, ul){
    idx <- which(measurement >= ll & measurement <= ul)
    NAidx <- which(!(measurement >= ll & measurement <= ul))
    
    if(length(measurement[idx]) <= blockSize){
        return(rep(NA, length(measurement)))
    }
    else{
        res <- EMA(measurement[idx], n = blockSize)
        replaceNAWithPrevious(res, NAidx) 
    }
}

rollMean <- rollingFunc(truncatedMean)
rollLogMean <- rollingFunc(logMean)
rollMed <-  rollingFunc(truncatedMed)

rollMean.del <- rollingFunc(truncatedMean.del)
rollLogMean.del <- rollingFunc(logMean.del)
rollMed.del <-  rollingFunc(truncatedMed.del)


####
simFunction <- function(data, bias, fxs, blockSize,
                        lowerTrunc, upperTrunc, 
                        lead, controlLimits){
    
    d <- calcFunctions(data, bias, fxs, blockSize,
                       lowerTrunc, upperTrunc, 
                       lead)
    
    d %>%
        left_join(controlLimits, by=c("type")) %>%
        group_by(day, set, type) %>%
        mutate(detected = value < lcl | value > ucl) %>%
        summarise(firstDetected = suppressWarnings(min(counter[detected], 
                                                       na.rm = TRUE)), 
                  lcl = lcl[1], ucl = ucl[1]) %>%
        ungroup()
}


calcFunctions <- function(data, bias, fxs, blockSize,
                          lowerTrunc, upperTrunc, 
                          lead){
    data %>%
        dplyr::select(measurement, day, set) %>%
        group_by(day, set) %>%
        mutate(leadtime = 1:n() <= lead) %>%
        mutate(newValue = case_when(
            leadtime ~ measurement,
            !leadtime ~ measurement * (1 + bias)
        )) %>%
        mutate_at(vars(newValue), 
                  .funs = fxs, blockSize = blockSize, 
                  ll = lowerTrunc, ul = upperTrunc) %>%
        filter(!leadtime) %>%
        dplyr::select(-leadtime) %>%
        mutate(counter = 1:n()) %>%
        gather(type, value, -day, -measurement,
               -set, -counter, -newValue) %>%
        ungroup()
}

###

findControlLimits <- function(data, blockSize, 
                              lowerTrunc, upperTrunc, fxs, percAccAlarms){
    
    lowerPerc <- percAccAlarms/2
    upperPerc <- 1 - (percAccAlarms/2)
    
    data %>% 
        dplyr::select(day, measurement) %>%
        group_by(day) %>%
        mutate_at(vars(measurement),
                  .funs = fxs, blockSize = blockSize, 
                  ll = lowerTrunc, ul = upperTrunc) %>%
        gather(type, value, -day, -measurement) %>%
        group_by(day, type) %>%
        filter(!is.na(value)) %>%
        summarise(minday = min(value),
                  maxday = max(value)) %>%
        group_by(type) %>%
        summarise(lcl =  quantile(minday, lowerPerc, names = FALSE),
                  ucl =  quantile(maxday, upperPerc, names = FALSE)
        ) %>%
        ungroup()
}

simPBRTQCapp <- function(data, blockSizes, truncationLimits, biases, fxs, 
                         percAccAlarms){
    
    varsCls <- cbind(data.frame(blockSize = blockSizes), truncationLimits) 
    
    incProgress(0, detail = 'Control limits')
    ss <- Sys.time()
    cls <- varsCls %>%
        rowwise() %>%
        mutate(r =  (function(blockSize, lowerTrunc, upperTrunc){
            res <- findControlLimits(blockSize = blockSize, 
                                     lowerTrunc = lowerTrunc,
                                     upperTrunc = upperTrunc, 
                                     data = data,
                                     fxs = fxs, percAccAlarms)
            list(res)
        })(blockSize, lowerTrunc, upperTrunc)) %>%
        ungroup() %>%
        unnest() %>%
        dplyr::select(truncation, type, blockSize,  ucl, lcl)
    
    ee <- Sys.time()
    lead <- max(blockSizes)
    
    params <- varsCls %>%
        crossing(tibble(bias = biases))
    
    nVars <- nrow(params)
    
    results <- params %>%
        rowwise() %>%
        mutate(r =  (function(bias, bs, trunc,
                              lowerTrunc, upperTrunc){
            
            incProgress(1/nVars, detail = paste('Bias:', bias, 'block size:', bs))
            
            thisCls <- cls %>%
                filter(truncation == trunc & blockSize == bs) %>%
                dplyr::select(-truncation, -blockSize)
            
            numDays <- length(unique(data$day))   
            
            if (numDays > 100){
                data <- data %>%
                    filter(day %in% base::sample(unique(data$day, 100)))
            }
            
            res <- simFunction(data = data %>% filter(),
                               blockSize = blockSize, 
                               lowerTrunc = lowerTrunc,
                               upperTrunc = upperTrunc,
                               controlLimits = thisCls,
                               lead = lead,
                               bias = bias,
                               fxs = fxs)
            list(res)
        })(bias, blockSize, truncation, lowerTrunc, upperTrunc)) %>%
        ungroup() %>%
        unnest()
    
    results
}

server <- function(input, output) {
    
    csvData <- reactive({ 
        req(input$file1)
        
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
                    df <- df %>%
                        filter(is.numeric(measurement))
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
    
    observeEvent(input$file1, {
        if(!is.null(csvData())){
            shinyjs::show(selector = ".settings")
        }  
        else{
            shinyjs::hide(selector = ".settings")
        }
        shinyjs::hide(selector = ".results")
    })
    
    observeEvent(input$truncation, {
        output$plotOverview <- renderPlot({
            ggplot(csvData() , aes(x=measurement)) +
                geom_density() +
                geom_vline(xintercept = minTruncation(), color = 'red') +
                geom_vline(xintercept = maxTruncation(), color = 'red') +
                coord_cartesian(xlim = c(pmax(min(csvData()$measurement), 0.5*minTruncation()),
                                         pmin(max(csvData()$measurement), 2*maxTruncation())))
        })
    })
    
    observeEvent(input$sim, {
        shinyjs::disable("sim")

        biases <- input$bias/100 * c(1.5, -1, -.5, .5, 1, 1.5)
        blockSizes <- seq(10, 150, by = 35) 
        
        funcsAll <- switch (input$algo,
                            'mean' = list("mean" = rollMean),
                             'mean.del' = list("mean.del" = rollMean.del),
                            'median' = list( 'median' = rollMed),
                           'median.del' = list('median.del' = rollMed.del),
                            'ema' = list('EMA' = truncatedEMA),
                             'ema.del' = list('EMA.del' = truncatedEMA.del)
        )
        
        data <- csvData()
        data$set <- 'train'
        
        truncationLimits <- data.frame('lowerTrunc' = minTruncation(),
                                       'upperTrunc' = maxTruncation(), 
                                       'truncation' = 'userSelected')
        
        startTime <- Sys.time()
        
        res <- withProgress(message = 'Simulation in progress',
                            simPBRTQCapp(data,
                  blockSizes, truncationLimits, biases, funcsAll, input$perc/100)
        )
        stopTime <- Sys.time()
        
        timeDiff <- difftime(stopTime, startTime, units = 'mins')
        
        showNotification(paste(round(timeDiff, 2), "minutes passed."),
                         type = "warning")
        showNotification("Evaluating results...")
        
        resultEval <- res %>%
            group_by(blockSize, truncation, bias, type) %>%
            summarise(ANPed = if_else(
                sum(is.finite(firstDetected))/n() > .95,
                mean(if_else(is.finite(firstDetected), firstDetected, 1000)),
                NA_real_),
                q95NPed= quantile(firstDetected, probs = .95),
                MNPed = median(firstDetected),
                q975 = quantile(firstDetected, probs = .975),
                q025 = quantile(firstDetected, probs = .025),
                n = n())
        
        browser()
        
        highestDetected <- res %>%
            filter(is.finite(firstDetected)) %>%
            pull(firstDetected) %>%
            max()
        
        bestSettingsByAlg <- resultEval %>%
            mutate(MNPed = if_else(is.finite(MNPed), MNPed, highestDetected*1.1)) %>%
            mutate(q95NPed = if_else(is.finite(q95NPed), q95NPed, highestDetected*1.1)) %>%
            group_by(type, blockSize) %>%
            summarise(sumMNped = sum(MNPed), sumq95NPed = sum(q95NPed))
        
        bestSettingsByAlgOverall <- bestSettingsByAlg %>%
            group_by(type) %>%
            filter(sumMNped == min(sumMNped)) %>%
            filter(sumq95NPed == min(sumq95NPed)) %>%
            filter(blockSize == min(blockSize)) %>%
            slice(1) %>%
            ungroup()
        
        bestSettingsByAlgSafest <- bestSettingsByAlg %>%
            group_by(type) %>%
            filter(sumq95NPed == min(sumq95NPed)) %>%
            filter(sumMNped == min(sumMNped)) %>%
            filter(blockSize == min(blockSize)) %>%
            slice(1) %>%
            ungroup()
        
        bestSettingsEarliestPos <- resultEval %>%
            filter(bias == input$bias/100) %>%
            filter(is.finite(MNPed)) %>%
            group_by(type) %>%
            filter(MNPed == min(MNPed)) %>%
            filter(q95NPed == min(q95NPed)) %>%
            filter(blockSize == min(blockSize)) %>%
            slice(1) %>%
            dplyr::select(type, blockSize) %>%
            ungroup()
        
        cls <- res %>%
            dplyr::select(type, blockSize, lcl, ucl) %>%
            unique()%>%
            ungroup()
        
        overall <- resultEval %>%
            inner_join(bestSettingsByAlgOverall, by = c("type", "blockSize")) %>%
            left_join(cls, by= c("type", "blockSize"))
        
        safest <-  resultEval %>%
            inner_join(bestSettingsByAlgSafest, by = c("type", "blockSize")) %>%
            left_join(cls, by= c("type", "blockSize"))
        
        bestBias <-  resultEval %>%
            inner_join(bestSettingsEarliestPos, by = c("type", "blockSize")) %>%
            left_join(cls, by= c("type", "blockSize"))
        
        output$plotMNPedOverall <- renderPlot(createPlot(overall, input$bias/100, highestDetected))
        output$plot95NPedOverall <- renderPlot(createPlot(safest, input$bias/100, highestDetected))
        output$plotBestBias <- renderPlot(createPlot(bestBias, input$bias/100, highestDetected))

        
        output$text95NPedOverall <- 
            renderText(dataToText(safest[1,], minTruncation(), 
                                  maxTruncation()))
        output$textMNPedOverall <- 
            renderText(dataToText(overall[1,], minTruncation(), 
                                  maxTruncation()))
        output$textBestBias <- 
            renderText(dataToText(bestBias[1,], minTruncation(), 
                                  maxTruncation()))
        
        shinyjs::enable("sim")
        shinyjs::show(selector = ".results")
    })
    
    url <- a("here", href="https://github.com/acnb/PBRTQC/")
    
    output$info <- renderUI({p('PBRTQC simulation version',
                               version, 
                               "by Andreas Bietenbeck.",
                               br(),
                               "Use at your own risk.",
                               "Further instructions",
                               "and bug reports can be found",
                              a("here", href="https://github.com/acnb/PBRTQC/"),
                              ".")})
}