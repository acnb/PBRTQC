data <- vroom::vroom(file.choose())
biases <- seq(from = -.5, to = .5, by = .1)
blockSizes <- c(20, 50)
fxs <- list("mean" = rollMean, 'median' = rollMed)

truncationLimits <- data.frame('lowerTrunc' = 110,
                               'upperTrunc' = 160, 
                               'truncation' = 'userSelected')

res <- simPBRTQC(data, blockSizes, truncationLimits, biases, fxs, 
                    0.01, TRUE)
   
