# This is an app to assist with capital budgetting.  It can 
# be used to calculate NPV and IRR, perform scenario 
# analysis and run Monte Carlo simulations.  Input values 
# and most results can be exported as an excel file to the 
# user's computer

# look for FinCal and install if needed
if (!"FinCal" %in% installed.packages()) 
    install.packages("FinCal")

library(FinCal)
library(shiny)
library(ggplot2)

# function to calculate npv with variable discount rate
vNPV <- function (drvec, cfvec) {
    n <- seq(0, length(drvec))
    drvec <- c(0, drvec)
    pv <- cfvec / ((1+drvec) ^ n)
    return(sum(pv))
}

# function to generate NPVs for SA with variable CF
genNPVs_cf <- function (xVec, dr, cfVec) {
    npvVec <- c()
    # fixed discount rate
    if (length(dr) == 1) {
        for (i in seq(1, 101))
            npvVec <- c(npvVec, npv(
                dr, 
                c(cfVec[1], cfVec[2:length(cfVec)] * (1 + xVec[i]/100))))
    # uneven discount rates
    }else{
        for (i in seq(1, 101))
            npvVec <- c(npvVec, vNPV(
                dr, 
                c(cfVec[1], cfVec[2:length(cfVec)] * (1 + xVec[i]/100))))
    }
    return(npvVec)
}

# function to generate NPVs for SA with variable r
genNPVs_dr <- function (xVec, dr, cfVec) {
    npvVec <- c()
    # fixed discount rate
    if (length(dr) == 1) {
        for (i in seq(1, 101))
            npvVec <- c(npvVec, npv(dr * (1 + xVec[i]/100), cfVec))
    # uneven discount rates    
    }else{
        for (i in seq(1, 101))
            npvVec <- c(npvVec, npv(dr * (1 + xVec[i]/100), cfVec))
    }
    return(npvVec)
}

# function to generate NPVs for MC
mcNPVs <- function(randomize, dr, cfVec){
    randNPVs <- c()
    for (i in seq(10000)){
        randCF <- c()
        randDR <- c()
        if (randomize$cf[[1]] == TRUE & randomize$cf[[2]] == "normal"){
            randCF <- cfVec * (1 + rnorm(length(cfVec), 
                                         sd = as.numeric(randomize$cf[[3]])))
        }else if (randomize$cf[[1]]){
            randCF <- cfVec * (1 + runif(length(cfVec), 
                                         min = as.numeric(randomize$cf[[4]]), 
                                         max = as.numeric(randomize$cf[[5]])))
        }else{
            randCF <- cfVec
        }
        if (randomize$r[[1]] == TRUE & randomize$r[[2]] == "normal"){
            randDR <- dr * (1 + rnorm(length(dr), 
                                      sd = as.numeric(randomize$r[[3]])
                ))
        }else if (randomize$r[[1]]){
            randDR <- dr * (1 + runif(length(dr), 
                                      min = as.numeric(randomize$r[[4]]), 
                                      max = as.numeric(randomize$r[[5]])))
        }else{
            randDR <- dr
        }
        if (length(randDR) == 1){
            randNPVs <- c(randNPVs, npv(randDR, randCF))
        }else{
            randNPVs <- c(randNPVs, vNPV(randDR, randCF))
        }
    }
    return(randNPVs)
}



###############################################################################
################################### UI ########################################
###############################################################################

ui <- navbarPage(title = "Capital Budgetting App", 
    
####### ABOUT PANEL #######
    tabPanel(
        "About",
        h2("Capital Budgetting App"),
        p("This is an app designed to assist with captial budgetting and 
          financial analyses which may require calculating or estimating net 
          present value (NPV) or the internal rate of return (IRR) from a 
          series of cash flows.  This app allows the user to input an initial 
          outlay and up to 20 subsequent cash flows.  The present value of 
          future cash flows is calculated using either a single fixed discount 
          rate or different rates for each period."),
        p("To use the app, just click on the tabs at the top of the page.")
    ),
    
####### NPV & IRR PANEL #######
    tabPanel(
        "NPV & IRR",
        
        wellPanel(
            fluidRow(
            # number of periods input
                column(3, 
                    numericInput(
                        "periods", label = "Number of periods", 
                        value = 5, min = 1, max = 20)
                    ),

            # discount rate type radio buttons
                column(3, 
                       radioButtons(
                           "discount_type", label = "Type of discount rate", 
                           choices = list("fixed" = 0, "variable" = 1), 
                           selected = 0)
                       ),
            
            # input for fixed discount rate (dependent ui)
                column(3, 
                       uiOutput("fixed_dr")
                       ))
            ),
        hr(),
        h4("Enter net cash flows for each period"), 
        
        # cash flow inputs (dependent ui)
        fluidRow(
            column(1, div(style = "height:60px;width:80px", 
                          textInput("cf_0", label = "t = 0", value = 0))),
            column(1, uiOutput("cf1")), 
            column(1, uiOutput("cf2")), 
            column(1, uiOutput("cf3")), 
            column(1, uiOutput("cf4")), 
            column(1, uiOutput("cf5")), 
            column(1, uiOutput("cf6")), 
            column(1, uiOutput("cf7")), 
            column(1, uiOutput("cf8")), 
            column(1, uiOutput("cf9")), 
            column(1, uiOutput("cf10"))
            ),
        fluidRow(
            column(1, uiOutput("cf11")), 
            column(1, uiOutput("cf12")), 
            column(1, uiOutput("cf13")), 
            column(1, uiOutput("cf14")), 
            column(1, uiOutput("cf15")), 
            column(1, uiOutput("cf16")), 
            column(1, uiOutput("cf17")), 
            column(1, uiOutput("cf18")), 
            column(1, uiOutput("cf19")), 
            column(1, uiOutput("cf20"))
        ),
        htmlOutput("line"),
        h4(textOutput("header")),
        
        # variable discount rate inputs (dependent ui)
        fluidRow(
            column(1, uiOutput("dr1"), offset = 1), 
            column(1, uiOutput("dr2")), 
            column(1, uiOutput("dr3")), 
            column(1, uiOutput("dr4")), 
            column(1, uiOutput("dr5")), 
            column(1, uiOutput("dr6")), 
            column(1, uiOutput("dr7")), 
            column(1, uiOutput("dr8")), 
            column(1, uiOutput("dr9")), 
            column(1, uiOutput("dr10")) 
        ),
        fluidRow(
            column(1, uiOutput("dr11")), 
            column(1, uiOutput("dr12")), 
            column(1, uiOutput("dr13")), 
            column(1, uiOutput("dr14")), 
            column(1, uiOutput("dr15")), 
            column(1, uiOutput("dr16")), 
            column(1, uiOutput("dr17")), 
            column(1, uiOutput("dr18")), 
            column(1, uiOutput("dr19")), 
            column(1, uiOutput("dr20")) 
        ),
        hr(),
        
        # Calculate, NPV and IRR headers
        fluidRow(
            column(3, h4("Calculate results")),
            column(3, h4("NPV")),
            column(3, h4("IRR"))),
        
        # Calculate button and NPV and IRR outputs
        fluidRow(
            column(3, actionButton("calc", label = "Calculate")),
            column(3, textOutput("npv")),
            column(3, textOutput("irr"))
        )
    ), 
    
####### SCENARIO ANALYSIS PANEL #######
    tabPanel(
        "Scenario Analysis",
        wellPanel(
            fluidRow(
                column(3, 
                    selectInput(
                        "saType", "Choose the indepedent variable", 
                        choices = c("Cash Flows", "Discount Rate(s)")
                        )
                    ),
                column(9, 
                       sliderInput(
                           "varRange", "Choose the range of variation (%)", 
                           min = -100, max = 100, value = c(-40, 40)
                           )
                       )
                )
            ),
        fluidRow(
            column(3,
                  actionButton("createPlot", label = "Create Plot")
                  )
            ),
        plotOutput("saPlot")
        ),

####### MONTE CARLO PANEL #######
    tabPanel(
        "Monte Carlo Simulation", 
        wellPanel(
            fluidRow(
                column(2, div(style = "height:5px", 
                              h5("Variables", style = "font-weight:bold"))), 
                column(3, div(style = "height:5px", 
                              h5("Distribution", style = "font-weight:bold"))),
                column(3, div(style = "height:5px", 
                              h5("Dispersion", style = "font-weight:bold")))
            ),
            fluidRow(
                column(2,
                       div(style = "height:20px",
                       h1(""),
                       checkboxInput("cfVar", label = "cash flows")
                       )),
                column(3, 
                       div(style = "height:20px",
                       selectInput(
                           "cfDist", label = "",  
                           choices = list("normal", "uniform")
                           ))
                       ),
                uiOutput("cfLab1"),
                uiOutput("cfNum1"),
                uiOutput("cfLab2"),
                uiOutput("cfNum2")
            ), 
            fluidRow(
                column(2,
                       div(style = "height:20px",
                       h1(""), 
                       checkboxInput("drVar", label = "discount rate")
                )),
                column(3, 
                       div(style = "height:20px", 
                       selectInput(
                           "drDist", label = "", 
                           choices = list("normal", "uniform")
                           ))
                       ),
                uiOutput("drLab1"),
                uiOutput("drNum1"),
                uiOutput("drLab2"),
                uiOutput("drNum2")
                ), 
            fluidRow(
                column(2,
                       div(style = "height:20px", h1(""), 
                           checkboxInput("gVar", label = "growth rate")
                           )
                       ),
                column(3, 
                       div(style = "height:20px", 
                           selectInput("gDist", 
                                       label = "", 
                                       choices = list("normal", "uniform")
                                       ))
                       ),
                uiOutput("gLab1"),
                uiOutput("gNum1"),
                uiOutput("gLab2"),
                uiOutput("gNum2")
            )),
        sidebarLayout(
            sidebarPanel(
                actionButton("doMC", label = "Run Simulations"),
                h1(""),
                checkboxGroupInput(
                    "mcLines", label = "Add Lines", 
                    choices = list(
                        "mean", "median", "mid 60%", "mid 80%", "mid 90%")
                    )
            ), 
            mainPanel(
                plotOutput("mcPlot")
            )
        )
    )
)


###############################################################################
################################# SERVER ######################################
###############################################################################

server <- function(input, output) {
    
####### NPV & IRR #######
    # numeric ui input for fixed discount rate
    output$fixed_dr <- renderUI({
        if (input$discount_type == 0)
            numericInput(inputId = "discount", label = "Enter discount rate",
                         value = 0.00)
    })
    
    # numeric ui inputs for cash flows
    output$cf1 <- renderUI({
        if (input$periods > 0)
            div(style = "height:60px;width:80px", 
                textInput("cf_1", label = "t = 1", value = 0))
    })
    output$cf2 <- renderUI({
        if (input$periods > 1)
            div(style = "height:60px;width:80px", 
                textInput("cf_2", label = "t = 2", value = 0))
    })
    output$cf3 <- renderUI({
        if (input$periods > 2)
            div(style = "height:60px;width:80px", 
                textInput("cf_3", label = "t = 3", value = 0))
    })
    output$cf4 <- renderUI({
        if (input$periods > 3)
            div(style = "height:60px;width:80px", 
                textInput("cf_4", label = "t = 4", value = 0))
    })
    output$cf5 <- renderUI({
        if (input$periods > 4)
            div(style = "height:60px;width:80px", 
                textInput("cf_5", label = "t = 5", value = 0))
    })
    output$cf6 <- renderUI({
        if (input$periods > 5)
            div(style = "height:60px;width:80px", 
                textInput("cf_6", label = "t = 6", value = 0))
    })  
    output$cf7 <- renderUI({
        if (input$periods > 6)
            div(style = "height:60px;width:80px", 
                textInput("cf_7", label = "t = 7", value = 0))
    })
    output$cf8 <- renderUI({
        if (input$periods > 7)
            div(style = "height:60px;width:80px", 
                textInput("cf_8", label = "t = 8", value = 0))
    })
    output$cf9 <- renderUI({
        if (input$periods > 8) 
            div(style = "height:60px;width:80px", 
                textInput("cf_9", label = "t = 9", value = 0))
    })
    output$cf10 <- renderUI({
        if (input$periods > 9)
            div(style = "height:60px;width:80px", 
                textInput("cf_10", label = "t = 10", value = 0))
    })    
    output$cf11 <- renderUI({
        if (input$periods > 10)
            div(style = "height:60px;width:80px", 
                textInput("cf_11", label = "t = 11", value = 0))
    })
    output$cf12 <- renderUI({
        if (input$periods > 11)
            div(style = "height:60px;width:80px", 
                textInput("cf_12", label = "t = 12", value = 0))
    })
    output$cf13 <- renderUI({
        if (input$periods > 12)
            div(style = "height:60px;width:80px", 
                textInput("cf_13", label = "t = 13", value = 0))
    })
    output$cf14 <- renderUI({
        if (input$periods > 13)
            div(style = "height:60px;width:80px", 
                textInput("cf_14", label = "t = 14", value = 0))
    })
    output$cf15 <- renderUI({
        if (input$periods > 14)
            div(style = "height:60px;width:80px", 
                textInput("cf_15", label = "t = 15", value = 0))
    })
    output$cf16 <- renderUI({
        if (input$periods > 15)
            div(style = "height:60px;width:80px", 
                textInput("cf_16", label = "t = 16", value = 0))
    })  
    output$cf17 <- renderUI({
        if (input$periods > 16)
            div(style = "height:60px;width:80px", 
                textInput("cf_17", label = "t = 17", value = 0))
    })
    output$cf18 <- renderUI({
        if (input$periods > 17)
            div(style = "height:60px;width:80px", 
                textInput("cf_18", label = "t = 18", value = 0))
    })
    output$cf19 <- renderUI({
        if (input$periods > 18) 
            div(style = "height:60px;width:80px", 
                textInput("cf_19", label = "t = 19", value = 0))
    })
    output$cf20 <- renderUI({
        if (input$periods > 19)
            div(style = "height:60px;width:80px", 
                textInput("cf_20", label = "t = 20", value = 0))
    })    
    
    # hr & header for variable discount rate submission area
    output$line <- renderText(
        if (input$discount_type == 1)
            "<hr>"
    )
    
    output$header <- renderText(
        if (input$discount_type == 1)
            "Enter discount rate for each period"
    )
    
    # numeric ui inputs for variable discount rates
    output$dr1 <- renderUI({
        if (input$periods > 0 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_1", label = "t = 1"))
    })
    output$dr2 <- renderUI({
        if (input$periods > 1 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_2", label = "t = 2"))
    })
    output$dr3 <- renderUI({
        if (input$periods > 2 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_3", label = "t = 3"))
    })
    output$dr4 <- renderUI({
        if (input$periods > 3 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_4", label = "t = 4"))
    })
    output$dr5 <- renderUI({
        if (input$periods > 4 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_5", label = "t = 5"))
    })
    output$dr6 <- renderUI({
        if (input$periods > 5 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_6", label = "t = 6"))
    })
    output$dr7 <- renderUI({
        if (input$periods > 6 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_7", label = "t = 7"))
    })
    output$dr8 <- renderUI({
        if (input$periods > 7 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_8", label = "t = 8"))
    })
    output$dr9 <- renderUI({
        if (input$periods > 8 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_9", label = "t = 9"))
    })
    output$dr10 <- renderUI({
        if (input$periods > 9 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_10", label = "t = 10"))
    })
    output$dr11 <- renderUI({
        if (input$periods > 10 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_11", label = "t = 11"))
    })
    output$dr12 <- renderUI({
        if (input$periods > 11 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_12", label = "t = 12"))
    })
    output$dr13 <- renderUI({
        if (input$periods > 12 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_13", label = "t = 13"))
    })
    output$dr14 <- renderUI({
        if (input$periods > 13 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_14", label = "t = 14"))
    })
    output$dr15 <- renderUI({
        if (input$periods > 14 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_15", label = "t = 15"))
    })
    output$dr16 <- renderUI({
        if (input$periods > 15 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_16", label = "t = 16"))
    })
    output$dr17 <- renderUI({
        if (input$periods > 16 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_17", label = "t = 17"))
    })
    output$dr18 <- renderUI({
        if (input$periods > 17 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_18", label = "t = 18"))
    })
    output$dr19 <- renderUI({
        if (input$periods > 18 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_19", label = "t = 19"))
    })
    output$dr20 <- renderUI({
        if (input$periods > 19 & input$discount_type == 1)
            div(style = "height:60px;width:80px", 
                textInput("dr_20", label = "t = 20"))
    })
    
    # create reactive functions to generate vectors
    # of cash flows and discount rates
    cf_vector <- reactive({
        c(input$cf_0, input$cf_1, input$cf_2, input$cf_3, input$cf_4, 
          input$cf_5, input$cf_6, input$cf_7, input$cf_8, input$cf_9, 
          input$cf_10, input$cf_11, input$cf_12, input$cf_13, input$cf_14, 
          input$cf_15, input$cf_16, input$cf_17, input$cf_18, input$cf_19, 
          input$cf_20)[1:(input$periods+1)]
        })

    vdr_vector <- reactive({
        c(input$dr_1, input$dr_2, input$dr_3, input$dr_4, input$dr_5, 
          input$dr_6, input$dr_7, input$dr_8, input$dr_9, 
          input$dr_10)[1:input$periods]
        })
    
    # reactive expressions for calculating NPV and IRR
    npv_result <- eventReactive(input$calc, {
        if (input$discount_type == 0) {
            # fixed discount rate
            npv(r = input$discount, cf = as.numeric(cf_vector()))
        }else{
            # variable discount rate
            vNPV(as.numeric(vdr_vector()), as.numeric(cf_vector()))
        }
    })
    irr_result <- eventReactive(input$calc, {irr(as.numeric(cf_vector()))})

    # NPV and IRR outputs
    output$npv <- renderText({npv_result()})
    output$irr <- renderText({irr_result()})
    
####### SCENARIO ANALYSIS #######
    # create vector of x values
    x_vec <- reactive({
        input$varRange[1] + ((input$varRange[2] - input$varRange[1]) / 100) * 
            seq(0, 100)
        })
    # create vector of y values
    y_vec <- eventReactive(
        input$createPlot, {
            if (input$saType == "Cash Flows" & input$discount_type == 0){
                # fixed discount rate 
                genNPVs_cf(as.numeric(x_vec()), input$discount, 
                           as.numeric(cf_vector()))
            }else if (input$saType == "Cash Flows"){
                # variable discount rate
                genNPVs_cf(as.numeric(x_vec()), as.numeric(vdr_vector()),
                           as.numeric(cf_vector()))
            }else if (input$saType != "Cash Flows" & input$discount_type == 0){
                # fixed discount rate
                genNPVs_dr(as.numeric(x_vec()), input$discount, 
                           as.numeric(cf_vector()))
            }else{
                # variable discount rate
                genNPVs_dr(as.numeric(x_vec()), as.numeric(vdr_vector()),
                           as.numeric(cf_vector()))
            }
        })

    # create plot for scenario analyses
    
    output$saPlot <- renderPlot({
        x <- as.numeric(isolate({x_vec()}))
        y <- as.numeric(y_vec())
        df <- data.frame(x, y)
        p <- ggplot(df, aes(x = x, y = y)) + 
            geom_line(color = "green3", size = 3, show.legend = FALSE) + 
            geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
        p + labs(
            list(
                title = paste(
                    "NPV for Varying Levels of", isolate({input$saType})
                    ), 
                x = "Percent Change from Original Values", y = "NPV"
                )
            )
        })
    
    # ui inputs for dispersion measures for Monte Carlo sims
    output$cfLab1 <- renderUI({
        if (input$cfDist == "normal"){
            column(1, p("sd =", style = "margin-top:20px;margin-bottom:0px"))
        }else{
            column(1, p("min =", style = "margin-top:20px;margin-bottom:0px"))
            } 
    })
    output$cfNum1 <- renderUI({
        if (input$cfDist == "normal"){
            column(2, div(style = "height:20px", 
                          numericInput("cfSD", label = "",  value = 0.25, 
                                       min = 0, max = 1, step = 0.01)))
        }else{
            column(2, div(style = "height:20px", 
                          numericInput("cfMin", label = "", value = -0.25, 
                                       min = -1, max = 0, step = 0.01)))
            }
    })
    output$cfLab2 <- renderUI({
        if (input$cfDist == "uniform")
            column(1, p("max =", style = "margin-top:20px;margin-bottom:0px"))
        })
    output$cfNum2 <- renderUI({
        if (input$cfDist == "uniform")
            column(2, div(style = "height:20px", 
                          numericInput("cfMax", label = "", value = 0.25, 
                                       min = 0, max = 1, step = 0.01)))
        })
    output$drLab1 <- renderUI({
        if (input$drDist == "normal"){
            column(1, p("sd =", style = "margin-top:20px;margin-bottom:0px"))
        }else{
            column(1, p("min =", style = "margin-top:20px;margin-bottom:0px"))
        } 
    })
    output$drNum1 <- renderUI({
        if (input$drDist == "normal"){
            column(2, div(style = "height:20px", 
                          numericInput("drSD", label = "",  value = 0.25, 
                                       min = 0, max = 1, step = 0.01)))
        }else{
            column(2, div(style = "height:20px", 
                          numericInput("drMin", label = "", value = -0.25, 
                                       min = -1, max = 0, step = 0.01)))
        }
    })
    output$drLab2 <- renderUI({
        if (input$drDist == "uniform")
            column(1, p("max =", style = "margin-top:20px;margin-bottom:0px"))
    })
    output$drNum2 <- renderUI({
        if (input$drDist == "uniform")
            column(2, div(style = "height:20px", 
                          numericInput("drMax", label = "", value = 0.25, 
                                       min = 0, max = 1, step = 0.01)))
    })
    output$gLab1 <- renderUI({
        if (input$gDist == "normal"){
            column(1, p("sd =", style = "margin-top:20px;margin-bottom:0px"))
        }else{
            column(1, p("min =", style = "margin-top:20px;margin-bottom:0px"))
        } 
    })
    output$gNum1 <- renderUI({
        if (input$gDist == "normal"){
            column(2, div(style = "height:20px", 
                          numericInput("gSD", label = "",  value = 0.25, 
                                       min = 0, max = 1, step = 0.01)))
        }else{
            column(2, div(style = "height:20px", 
                          numericInput("gMin", label = "", value = -0.25, 
                                       min = -1, max = 0, step = 0.01)))
        }
    })
    output$gLab2 <- renderUI({
        if (input$gDist == "uniform")
            column(1, p("max =", style = "margin-top:20px;margin-bottom:0px"))
    })
    output$gNum2 <- renderUI({
        if (input$gDist == "uniform")
            column(2, div(style = "height:20px", 
                          numericInput("gMax", label = "", value = 0.25, 
                                       min = 0, max = 1, step = 0.01)))
    })
    # create plot from Monte Carlo output
    runMC <- eventReactive(input$doMC, {
        randolist <- list(
            "cf" = c(input$cfVar, input$cfDist, input$cfSD, 
                     input$cfMin, input$cfMax),
            "r" = c(input$drVar, input$drDist, input$drSD, 
                    input$drMin, input$drMax)
            )
        if (input$discount_type == 0){
            mcNPVs(randolist, as.numeric(input$discount), 
                   as.numeric(cf_vector()))
        }else{
            mcNPVs(randolist, as.numeric(vdr_vector()), 
                   as.numeric(cf_vector()))
        }
    })
    
    output$mcPlot <- renderPlot({
        p <- ggplot(data.frame("NPV" = runMC()), aes(NPV))
        p + geom_histogram(color = "gray30", fill = "orangered1")
    })
}

shinyApp(ui = ui, server = server)