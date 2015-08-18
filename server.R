
##  Central Limit Theorem Explorer

##  Server code for multiple Distributions

library(shiny)
library(stats)
library(xtable)
library(triangle)

##  Code before shinyServer() executes once at start, per user session.

##  To define population size
max_SS  <- 500                  # could put this in global.R
max_NS  <- 500                  # file to share with ui.R.
popSize <- max_NS * max_SS

##  Fixed parameters for the Distributions

##  Exponential
lambda  <- 0.1

##  Uniform
uMin <- 0
uMax <- 20

##  Triangular
a <- 0
c <- b <- 15

##  Artificial Step-Function
xArt <- c(1, 6, 18)             # vector of integers to sample
pArt <- c(0.4, 0.1, 0.5)        # probability weights for elements in xArt
                                # sum of pArt = 1; Mean of distribution = 10.

##  Add parameters for new Distribution here.

##  Text and location coordinate for Means of Samples plot
mText <- c("mu", "Var", "SE *", "CLT Prediction", 'cMu', 'cVar', 'cSE',
                                 "from Simulation", 'mMu', 'mVar', 'mSE',
                                "* Standard Error of Sample Means")

xMText <- c(25, 27, 29, 21, 25, 27, 29, 21, 25, 27, 29, 24)
#yMText <- c(0.3, 0.3, 0.3, 0.25, 0.25, 0.25, 0.25, 0.2, 0.2, 0.2, 0.2, 0.15)

##  Text and location coordinate for Samples plot
sText <- c("mu", "Var", "Sample", 'sMu', 'sVar', "Population *", 'pMu', 'pVar',
                                                "* Infinite Population assumed")
xSText <- c(27, 29, 23, 27, 29, 23, 27, 29, 25.5)
#ySText <- c(0.3, 0.3, 0.25, 0.25, 0.25, 0.2, 0.2, 0.2, 0.15)

bye <- "Thanks for using CLT Explorer"
##  Code for the Server function

shinyServer(function(input, output, session) {

    ##  Use a Reactive function to perform tasks that are
    ##  common to the Plot and Summary tab functions.

    session$onSessionEnded(function() {
        session$close()
        #stopApp(paste("CLT Explorer: User Closed Window!"))
    })

    rList <- reactive({
        observe({
            if(input$Exit > 0){
                output$caption <- renderText({
                    bye
                })
                session$close()
                #stopApp(paste("CLT Explorer Exited Gracefully!"))
            }
        })

        SS <- input$SS            # Sample Size
        NS <- input$NS            # Number of Samples

        ##  Build a population of the requested distribution.
        ##  Rows = Number of Samples, Columns = Sample Size.

        set.seed(10)

        ##  The switch code below will execute only when the user chooses a
        ##  new distribution.  Rather than re-populate the distribution
        ##  whenever SS or NS change, we construct a matrix of maximum size
        ##  and select the requisite number of rows and colummns.  This also
        ##  ensures that (1) the base population is invariant, for a given
        ##  distribution, and (2) the sample size is never large compared
        ##  to the population.  So, we can use the simplified CLT formula for Var.

        SMX <- switch(input$dName,
                      Exponential = matrix(rexp(popSize, lambda), ncol = max_SS),
                      Uniform     = matrix(runif(popSize, uMin, uMax), ncol = max_SS),
                      Triangular  = matrix(rtriangle(popSize, a, b, c), ncol = max_SS),
                      Artificial  = matrix(sample(xArt, popSize, replace = TRUE,
                                                  prob = pArt), ncol = max_SS),

                      #    Add new Distribution populator code here.

                    stopApp("Enter code for ", input$dName, " Distribution.")
                )

        SMN <- apply(SMX[1:NS, 1:SS], 1, mean)          # Calculate Mean of each Sample.

        # Use the 1st column of SMX as a sample of the underlying distribution.

        list(SMX = SMX, SMN = SMN)      # Return SMX, SMN for use by Plot, Summary.
    })

    ##  Function to build output Plots.

    output$plot <- renderPlot({

        NS    <- input$NS           # number of samples
        SS    <- input$SS           # sample size
        dName <- input$dName        # requested distribution

        SMN <- rList()$SMN
        SMX <- rList()$SMX[1,1:SS]  # take one sample

        mMu  <- round(mean(SMN), 2)         # Determine Mean and
        mVar <- round(var(SMN), 2)          # Variance of the Sample Means.
        mSE  <- round(sqrt(var(SMN)), 2)    # Std. Error of Sample Mean.


        sMu  <- round(mean(SMX), 2)     # Determine Mean and
        sVar <- round(var(SMX), 2)      # Variance of the one Sample.


        xSeq <- seq(0, 30, by = 0.5)    # dummy X for generating
                                        # reference distributions.
        set.seed(1)

        ##  Switch code executes only when Distribution type changes.
        switch(dName,
               Exponential = {pMu   <- round(1/lambda, 2);          # Mean and Variance for
                              pVar  <- round(1/(lambda^2), 2);      # infinite population
                              xPop  <- xSeq;                        # xPop, yPop define the
                              yPop  <- dexp(xSeq, lambda)},         # density profile for the
                                                                    # underlying distribution.
               Uniform     = {pMu   <- (uMin + uMax)/2;
                              pVar  <- round((uMax - uMin)^2/12, 2);
                              xPop  <- c(0, 0, 20, 20, 30);
                              yPop  <- c(0, 0.05, 0.05, 0, 0)},

               Triangular  = {pMu   <- round((a + b + c)/3, 2);
                              pVar  <- round((a^2 + b^2 + c^2 -a*b - a*c - b*c)/18, 2);
                              xPop  <- c(0, 15, 15, 30);
                              yPop  <- c(0, 0.1, 0, 0)},

               Artificial  = {pMu   <- sum(xArt * pArt);
                              pVar  <- round(sum((xArt - pMu)^2 * pArt), 2);
                              xPop  <- c(0, 0, 1, 1, 5, 5, 6, 6, 17, 17, 18, 18, 30);
                              yPop  <- c(0, 0.4, 0.4, 0, 0, 0.1, 0.1, 0, 0, 0.5, 0.5, 0, 0)},

               #    Add code for new Distribution here.

               stopApp("How didja get here?  Enter code for ", dName, "Distribution.")
            )

        ##  Statistics predicted by Central Limit Theorem
        cMu  <- pMu
        cVar <- round(pVar/SS, 2)
        cSE  <- round(sqrt(pVar/SS), 2)

        ##  To scale the Y-axis of the plots
        yM    <- hist(SMN, breaks = seq(0, 100, by = 1), plot = FALSE)
        yMmax <- max(yM$density) * 1.25
        yS    <- hist(SMX, breaks = seq(0, 100, by = 1), plot = FALSE)
        ySmax <- max(yS$density) * 1.25

        ##  Scaling the Y-axis requires scaling the y coordinates of the text
        yMText <- c(rep(0.64*yMmax, 3), rep(0.58*yMmax, 4), rep(0.52*yMmax, 4), 0.46*yMmax)
        ySText <- c(rep(0.64*ySmax, 2), rep(0.58*ySmax, 3), rep(0.52*ySmax, 3), 0.46*ySmax)

        ##  Add calculated statistics to text for Means of Samples plot
        mText[5]  <- cMu
        mText[6]  <- cVar
        mText[7]  <- cSE
        mText[9]  <- mMu
        mText[10] <- mVar
        mText[11] <- mSE

        ##  Add calculated statistics to text for single Sample plot
        sText[4] <- sMu
        sText[5] <- sVar
        sText[7] <- pMu
        sText[8] <- pVar

        ##  Calculate quartiles
        qN <- quantile(SMN, probs = c(0.25, 0.5, 0.75))
        qX <- quantile(SMX, probs = c(0.25, 0.5, 0.75))

        ##  For reference Normal curve predicted by CLT
        yNorm <- dnorm(xSeq, mean = cMu, sd = sqrt(cVar))

        par(mfrow = c(2, 1))

        ##  Plot the Means of the Samples
        hist(SMN, breaks = seq(0, 100, by = 1), prob = TRUE,
            main = paste("Distribution of Means from ", NS,
                            " Samples of ", SS, " ", dName,"s", sep = ""),
            col = "light grey", border = "white", xlab = "Sample Means",
            cex.lab = 1.2, ylim = c(0, yMmax), xlim = c(0, 30))

        grid(col = "grey")
        lines(density(SMN, adjust = 1, bw = "sj"), col= "cornflowerblue", lwd = 1)

        lines(xSeq, yNorm, lty = 2, col = 139, lwd = 2)         # reference Normal curve

        legend('topright', c("Quartiles", "Mean", "Sample Means Density",
                             "Smoothed Density", "Normal Density"),
                cex = 0.9, lty = c(1, 1, 1, 1, 2), lwd = c(1, 1, 7, 1, 2),
                col=c("orange", "black", "grey", "cornflowerblue", 139))
        abline(v = qN, col = "orange", lwd = 1)                             # quartiles
        abline(v = mMu, col = "black", lwd = 1)                             # mean

        text(xMText, yMText, labels = mText, cex = 0.9)

        ##  Plot a sample of the underlying distribution
        hist(SMX, breaks = seq(0, 100, by = 1), prob = TRUE,
            main = paste("Distribution of ", SS, " Random ", dName, "s", sep = ""),
            col = "light grey", border = "white",
            xlab = paste("Random", dName, "Variables"),
            cex.lab = 1.2, ylim = c(0, ySmax), xlim = c(0, 30))

        grid(col = "grey")
        lines(density(SMX, adjust = 0.2, from = 0.5, bw = "nrd0"), col= "cornflowerblue", lwd = 1)

        lines(xPop, yPop, lty = 2, col = 139, lwd = 2)      # reference Population density profile

        legend('topright', c("Quartiles", "Mean", "Sample Density",
                             "Smoothed Density", "Population Density"),
                cex = 0.9, lty = c(1, 1, 1, 1, 2), lwd = c(1, 1, 7, 1, 2),
                col = c("orange", "black", "grey", "cornflowerblue", 139))
        abline(v = qX, col = "orange", lwd = 1)                             # quartiles
        abline(v = sMu, col = "black", lwd = 1)                       # mean

        text(xSText, ySText, labels = sText, cex = 0.9)

    }, height = 800, width = 600 )

    ##  Function to build output Summaries.

    output$summary <- renderTable({

        SS    <- input$SS
        NS    <- input$NS
        dName <- input$dName

        SMN  <- rList()$SMN
        SMX  <- rList()$SMX[1,1:SS]
        POP  <- as.vector(rList()$SMX)

        sumSMN  <- summary(SMN)
        sumSMX  <- summary(SMX)
        sumPOP  <- summary(POP)

        sumS    <- rbind(rbind(sumSMN, sumSMX), sumPOP)
        sdCol   <- rbind(rbind(sd(SMN), sd(SMX)), sd(POP))
        sumS    <- cbind(sumS, sdCol)

        rownames(sumS)[1] <- paste("Means of ", NS, " Samples of ", SS, " ",
                                                        dName, "s", sep = "")

        rownames(sumS)[2] <- paste("One Sample of ", SS, " ", dName, "s", sep = "")
        rownames(sumS)[3] <- paste("Population of ", (max_SS * max_SS), " ", dName,
                                                                        "s", sep = "")

        colnames(sumS)[7] <- "Std. Dev."

        xtable(sumS)
    })

})

##  ----------------------------------------------------------------------------------  ##

