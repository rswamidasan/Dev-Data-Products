
##  UI code for Central Limit Theorem Demonstrator

##  Code for multiple Distributions

library(shiny)

shinyUI(fluidPage(

    titlePanel("Central Limit Theorem Explorer"),

    ##  Sidebar with controls to select the random distribution type,
    ##  the number of samples and the sample size.

    sidebarLayout(
        sidebarPanel(
            h6("[Please do not use the Refresh button - the app will terminate.]", style = 'color:red'),
            h5("The Central Limit Theorem (CLT) states that when a distribution with mean(mu)
                and finite variance (Var) is randomly sampled, the mean of the samples is
                normally distributed with mean = mu and variance = Var/N,when N, the sample
                size, is sufficiently large."),

            p(h5("To see the CLT in action:")),
            p(h5(tags$ol(tags$li("Choose the distribution you wish to explore from the drop-down menu."),
                         tags$li("Adjust the Sample Size or the Number of Samples.")))),

            selectInput("dName", "Distribution type:",
                       c("Exponential - lambda = 1/10" = "Exponential",
                         "Uniform - min = 0, max = 20" = "Uniform",
                         "Triangular - a = 0, b = c = 15" = "Triangular",
                         "Artificial Step Function - mu = 10" = "Artificial"

                         #  Add new Distribution to menu here.
                        )),

            sliderInput("SS", "Sample Size:",
                        value = 30, min = 5, step = 5, max = 500),

            sliderInput("NS",
                        "Number of Samples:",
                        value = 30, min = 5, step = 5, max = 500),

            p(h5("The Density Plot tab displays histograms of the Sample Means and a single Sample.
                 The Summary tab shows statistics of the Sample Means, a single Sample and the
                 underlying Population.")),

            p(h5("This shiny app demonstrates that:")),
            p(h5(tags$ol(tags$li("The CLT applies regardless of the underlying distribution."),
                         tags$li("The CLT is relevant to Continuous and Discrete variables.")))),

            p(h5("Use the Explorer to see* that for a given population with parameters mu and Var:")),
            p(h5(tags$ol(tags$li("The Sample Means Variance varies inversely as Sample Size."),
                         tags$li("The Sample Means Variance is insensitive to the Number of Samples."),
                         tags$li("The reduction in the Standard Error of the Sample Mean diminishes
                                  as Sample Size increases.")))),

            p(h5(a(href = "https://github.com/rswamidasan/Dev_Data_Products", target="_blank", "Source Code"))),

            actionButton("Exit", "Exit Gracefully", style = 'color:red'),

            h5(textOutput("caption", container = span)),

            p(h6("*: Subject to limitations of Simulation; the population size is not truly infinite."))

        ),

        ##  The Main Panel tabset includes density plots and
        ##  summaries of the Sample Means and a single Sample.

        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Density Plot", plotOutput("plot")),
                tabPanel("Summary", tableOutput("summary"))
            )
        )
    )
))
