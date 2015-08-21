
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


xSeq <- seq(0, 30, by = 0.5)    # dummy X for generating reference distributions.

##  Text and location coordinate for Means of Samples plot
mText <- c("mu", "Var", "SE *", "CLT Prediction", 'cMu', 'cVar', 'cSE',
                                 "from Simulation", 'mMu', 'mVar', 'mSE',
                                "* Standard Error of Sample Means")

xMText <- c(25, 27, 29, 21, 25, 27, 29, 21, 25, 27, 29, 24)

##  Text and location coordinate for Sample plot
sText <- c("mu", "Var", "Sample", 'sMu', 'sVar', "Population *", 'pMu', 'pVar',
                                                "* Infinite Population assumed")
xSText <- c(27, 29, 23, 27, 29, 23, 27, 29, 25.5)

bye <- "Thanks for using CLT Explorer"
##  Code for the Server function

shinyServer(function(input, output, session) {

    session$onSessionEnded(function() {
        session$close()
    })

    ##  Use Reactive functions to perform tasks that are
    ##  common to the Plot and Summary tab functions.

    #   Only if input$dName changes
    pList <- reactive({

        ##  Build a population of the requested distribution.

        ##  This code below will execute only when the user chooses a
        ##  new distribution.  Rather than re-populate the distribution
        ##  whenever SS or NS change, we construct a matrix of maximum size
        ##  and select the requisite number of rows and colummns (in SAM and SMN).
        ##  SMN).  This also ensures that (1) the base population is invariant
        ##  a given distribution, and (2) the sample size is never large compared
        ##  to the population.  So, we can use the simplified CLT formula for Var.

        # Also, find Mean and Variance for infinite population. xPop, yPop
        # define the # density profile for the underlying distribution.

        switch(input$dName,
                  Exponential = {POP  <- matrix(rexp(popSize, lambda), ncol = max_SS);
                                 pMu  <- round(1/lambda, 2);
                                 pVar <- round(1/(lambda^2), 2);
                                 xPop <- xSeq;
                                 yPop <- dexp(xSeq, lambda)},

                  Uniform     = {POP  <- matrix(runif(popSize, uMin, uMax), ncol = max_SS);
                                 pMu  <- (uMin + uMax)/2;
                                 pVar <- round((uMax - uMin)^2/12, 2);
                                 xPop <- c(0, 0, 20, 20, 30);
                                 yPop <- c(0, 0.05, 0.05, 0, 0)},

                  Triangular  = {POP  <- matrix(rtriangle(popSize, a, b, c), ncol = max_SS);
                                 pMu  <- round((a + b + c)/3, 2);
                                 pVar <- round((a^2 + b^2 + c^2 -a*b - a*c - b*c)/18, 2);
                                 xPop <- c(0, 15, 15, 30);
                                 yPop <- c(0, 0.1, 0, 0)},

                  Artificial  = {POP  <- matrix(sample(xArt, popSize, replace = TRUE, prob = pArt),
                                                                                    ncol = max_SS);
                                 pMu  <- sum(xArt * pArt);
                                 pVar <- round(sum((xArt - pMu)^2 * pArt), 2);
                                 xPop <- c(0, 0, 1, 1, 5, 5, 6, 6, 17, 17, 18, 18, 30);
                                 yPop <- c(0, 0.4, 0.4, 0, 0, 0.1, 0.1, 0, 0, 0.5, 0.5, 0, 0)},

                  #    Add new Distribution populator code here.
                  stopApp("Enter code for ", input$dName, " Distribution.")

            ) # end switch

        list(POP = POP, pMu = pMu, pVar = pVar,     # Return Population Matrix & statistics
                        xPop = xPop, yPop = yPop)   # for use my plot and summary function.
    })

    #   Only if input$SS changes
    SAM <- reactive(
        pList()$POP[1,1:input$SS]                   # take one sample from the Population.
    )

    #   Only if input$NS changes
    SMN <- reactive(
        apply(pList()$POP[1:input$NS, 1:input$SS], 1, mean)     # take NS samples and find Means.
    )

    ##  Function to build output Plots.

    output$plot <- renderPlot({

        NS    <- input$NS           # number of samples
        SS    <- input$SS           # sample size
        dName <- input$dName        # requested distribution

        POP  <- pList()$POP         #
        pMu  <- pList()$pMu         # for
        pVar <- pList()$pVar        # cleaner
        xPop <- pList()$xPop        # code
        yPop <- pList()$yPop        #

        SMN <- SMN()    # only for
        SAM <- SAM()    # cleaner code

        mMu  <- round(mean(SMN), 2)         # Determine Mean and
        mVar <- round(var(SMN), 2)          # Variance of the Sample Means.
        mSE  <- round(sqrt(var(SMN)), 2)    # Std. Error of Sample Mean.

        sMu  <- round(mean(SAM), 2)         # Determine Mean and
        sVar <- round(var(SAM), 2)          # Variance of the one Sample.

        ##  Statistics predicted by Central Limit Theorem
        cMu  <- pMu
        cVar <- round(pVar/SS, 2)
        cSE  <- round(sqrt(pVar/SS), 2)

        ##  To scale the Y-axis of the plots
        yM    <- hist(SMN, breaks = seq(0, 100, by = 1), plot = FALSE)
        yMmax <- max(yM$density) * 1.25
        yS    <- hist(SAM, breaks = seq(0, 100, by = 1), plot = FALSE)
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
        qX <- quantile(SAM, probs = c(0.25, 0.5, 0.75))

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
        lines(density(SMN, adjust = 1, bw = "nrd0"), col= "cornflowerblue", lwd = 1)

        lines(xSeq, yNorm, lty = 2, col = 139, lwd = 2)         # reference Normal curve

        legend('topright', c("Quartiles", "Mean", "Sample Means Density",
                             "Smoothed Density", "Normal Density"),
                cex = 0.9, lty = c(1, 1, 1, 1, 2), lwd = c(1, 1, 7, 1, 2),
                col=c("orange", "black", "grey", "cornflowerblue", 139))
        abline(v = qN, col = "orange", lwd = 1)                             # quartiles
        abline(v = mMu, col = "black", lwd = 1)                             # mean

        text(xMText, yMText, labels = mText, cex = 0.9)

        ##  Plot a sample of the underlying distribution
        hist(SAM, breaks = seq(0, 100, by = 1), prob = TRUE,
            main = paste("Distribution of ", SS, " Random ", dName, "s", sep = ""),
            col = "light grey", border = "white",
            xlab = paste("Random", dName, "Variables"),
            cex.lab = 1.2, ylim = c(0, ySmax), xlim = c(0, 30))

        grid(col = "grey")
        lines(density(SAM, adjust = 0.2, from = 0.5, bw = "nrd0"), col= "cornflowerblue", lwd = 1)

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

        SMN  <- SMN()                   # only for
        SAM  <- SAM()                   # cleaner code

        POP  <- as.vector(pList()$POP)

        sumSMN  <- summary(SMN)
        sumSAM  <- summary(SAM)
        sumPOP  <- summary(POP)

        sumS    <- rbind(rbind(sumSMN, sumSAM), sumPOP)
        sdCol   <- rbind(rbind(sd(SMN), sd(SAM)), sd(POP))
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

