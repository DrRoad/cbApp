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
    n <- c(0, seq(length(drvec)))
    drvec <- c(1, drvec)
    pv <- cfvec / ((1+drvec) ^ n)
    return(sum(pv))
}

# function to generate NPVs for SA with variable CF
genNPVs_cf <- function (xVec, dr, cfVec) {
    npvVec <- c()
    # fixed discount rate
    if (length(dr) == 1) {
        for (i in seq(1, 101))
            npvVec <- c(
                npvVec, npv(
                    dr, c(
                        cfVec[1], 
                        cfVec[2:length(cfVec)] * 
                            (1 + xVec[i]/100))))
    # uneven discount rates
    }else{
        for (i in seq(1, 101))
            npvVec <- c(
                npvVec, vNPV(
                    dr, c(
                        cfVec[1], 
                        cfVec[2:length(cfVec)] * 
                            (1 + xVec[i]/100))))
    }
    return(npvVec)
}

# function to generate NPVs for SA with variable r
genNPVs_dr <- function (xVec, dr, cfVec) {
    npvVec <- c()
    # fixed discount rate
    if (length(dr) == 1) {
        for (i in seq(1, 101))
            npvVec <- c(npvVec, 
                        npv(dr * (1 + xVec[i]/100), cfVec))
    # uneven discount rates    
    }else{
        for (i in seq(1, 101))
            npvVec <- c(npvVec, 
                        npv(dr * (1 + xVec[i]/100), cfVec))
    }
    return(npvVec)
}



######################################################
####################### UI ###########################
######################################################

ui <- navbarPage(title = "Capital Budgetting App", 
    
####### ABOUT PANEL #######
    tabPanel(
        "About",
        h2("Capital Budgetting App"),
        p("This is an app designed to assist with captial
          budgetting and performing financial analyses which 
          may require calculating or estimating net present
          value (NPV) or the internal rate of return (IRR) 
          from a series of cash flows.  This app allows the 
          user to input an initial outlay and up to 20 
          subsequent cash flows.  Present value of future cash
          flows are calculated using either a single fixed
          discount rate or different rates for each period."),
        p("To use the app, just click on the tabs at the top 
          of the page.")
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
                        value = 5, min = 1, max = 10)
                    ),

            # discount rate type radio buttons
                column(3, 
                       radioButtons(
                           "discount_type", 
                           label = "Type of discount rate", 
                           choices = list(
                               "fixed" = 0, 
                               "variable" = 1), 
                           selected = 0)),
            
            # input for fixed discount rate (dependent ui)
                column(3, 
                       uiOutput("fixed_dr")
                       )
            )),
        hr(),
        h4("Enter net cash flows for each period"), 
        
        # cash flow inputs (dependent ui)
        fluidRow(
            column(2, textInput("cf_0", label = "t = 0", 
                                value = 0)),
            column(2, uiOutput("cf1")), 
            column(2, uiOutput("cf2")), 
            column(2, uiOutput("cf3")), 
            column(2, uiOutput("cf4")), 
            column(2, uiOutput("cf5")), 
            column(2, uiOutput("cf6")), 
            column(2, uiOutput("cf7")), 
            column(2, uiOutput("cf8")), 
            column(2, uiOutput("cf9")), 
            column(2, uiOutput("cf10"))
            ),
        htmlOutput("line"),
        h4(textOutput("header")),
        
        # variable discount rate inputs (dependent ui)
        fluidRow(
            column(2, uiOutput("dr1"), offset = 2), 
            column(2, uiOutput("dr2")), 
            column(2, uiOutput("dr3")), 
            column(2, uiOutput("dr4")), 
            column(2, uiOutput("dr5")), 
            column(2, uiOutput("dr6")), 
            column(2, uiOutput("dr7")), 
            column(2, uiOutput("dr8")), 
            column(2, uiOutput("dr9")), 
            column(2, uiOutput("dr10")) 
        ),
        hr(),
        
        # Calculate, NPV and IRR headers
        fluidRow(
            column(3, h4("Calculate results")),
            column(3, h4("NPV")),
            column(3, h4("IRR"))),
        
        # Calculate button and NPV and IRR outputs
        fluidRow(
            column(3, actionButton("calc", 
                                   label = "Calculate")),
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
                        "saType", 
                        "Choose the indepedent variable", 
                        choices = c("cash flows", 
                                    "discount rate(s)"))),
                column(9,
                       sliderInput(
                        "varRange", 
                        "Choose the range of variation (%)",
                        min = -100, max = 100, 
                        value = c(-40, 40)
                )))),
        fluidRow(
            column(3,
                  actionButton(
                      "createPlot",
                      label = "Create Plot"))
        ),
        textOutput("test"),
        plotOutput("saPlot")
        ),

####### MONTE CARLO PANEL #######
    tabPanel(
        "Monte Carlo Simulation"
    )
)


##########################################################
####################### SERVER ###########################
##########################################################

server <- function(input, output) {
    
####### NPV & IRR #######
    # numeric ui input for fixed discount rate
    output$fixed_dr <- renderUI({
        if (input$discount_type == 0)
            numericInput(inputId = "discount",
                         label = "Enter discount rate",
                         value = 0.00)
    })
    
    # numeric ui inputs for cash flows
    output$cf1 <- renderUI({
        if (input$periods > 0)
            textInput("cf_1", label = "t = 1", value = 0)
    })
    output$cf2 <- renderUI({
        if (input$periods > 1)
            textInput("cf_2", label = "t = 2", value = 0)
    })
    output$cf3 <- renderUI({
        if (input$periods > 2)
            textInput("cf_3", label = "t = 3", value = 0)
    })
    output$cf4 <- renderUI({
        if (input$periods > 3)
            textInput("cf_4", label = "t = 4", value = 0)
    })
    output$cf5 <- renderUI({
        if (input$periods > 4)
            textInput("cf_5", label = "t = 5", value = 0)
    })
    output$cf6 <- renderUI({
        if (input$periods > 5)
            textInput("cf_6", label = "t = 6", value = 0)
    })  
    output$cf7 <- renderUI({
        if (input$periods > 6)
            textInput("cf_7", label = "t = 7", value = 0)
    })
    output$cf8 <- renderUI({
        if (input$periods > 7)
            textInput("cf_8", label = "t = 8", value = 0)
    })
    output$cf9 <- renderUI({
        if (input$periods > 8) 
            textInput("cf_9", label = "t = 9", value = 0)
    })
    output$cf10 <- renderUI({
        if (input$periods > 9)
            textInput("cf_10", label = "t = 10", value = 0)
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
            textInput("dr_1", label = "t = 1")
    })
    output$dr2 <- renderUI({
        if (input$periods > 1 & input$discount_type == 1)
            textInput("dr_2", label = "t = 2")
    })
    output$dr3 <- renderUI({
        if (input$periods > 2 & input$discount_type == 1)
            textInput("dr_3", label = "t = 3")
    })
    output$dr4 <- renderUI({
        if (input$periods > 3 & input$discount_type == 1)
            textInput("dr_4", label = "t = 4")
    })
    output$dr5 <- renderUI({
        if (input$periods > 4 & input$discount_type == 1)
            textInput("dr_5", label = "t = 5")
    })
    output$dr6 <- renderUI({
        if (input$periods > 5 & input$discount_type == 1)
            textInput("dr_6", label = "t = 6")
    })
    output$dr7 <- renderUI({
        if (input$periods > 6 & input$discount_type == 1)
            textInput("dr_7", label = "t = 7")
    })
    output$dr8 <- renderUI({
        if (input$periods > 7 & input$discount_type == 1)
            textInput("dr_8", label = "t = 8")
    })
    output$dr9 <- renderUI({
        if (input$periods > 8 & input$discount_type == 1)
            textInput("dr_9", label = "t = 9")
    })
    output$dr10 <- renderUI({
        if (input$periods > 9 & input$discount_type == 1)
            textInput("dr_10", label = "t = 10")
    })
    
    # create reactive functions to generate vectors
    # of cash flows and discount rates
    cf_vector <- reactive({
        c(input$cf_0, input$cf_1, input$cf_2, input$cf_3, 
           input$cf_4, input$cf_5, input$cf_6, input$cf_7, 
           input$cf_8, input$cf_9, 
           input$cf_10)[1:(input$periods+1)]})

    vdr_vector <- reactive({
        c(input$dr_1, input$dr_2, input$dr_3, input$dr_4, 
          input$dr_5, input$dr_6, input$dr_7, input$dr_8, 
          input$dr_9, input$dr_10)[1:input$periods]
    })
    
    # reactive expressions for calculating NPV and IRR
    npv_result <- eventReactive(input$calc, {
        if (input$discount_type == 0) {
            # fixed discount rate
            npv(r = input$discount, 
                cf = as.numeric(cf_vector()))
        }else{
            # variable discount rate
            vNPV(as.numeric(vdr_vector()), 
                 as.numeric(cf_vector()))
        }
    })
    irr_result <- eventReactive(input$calc, {
        irr(as.numeric(cf_vector()))
    })

    # NPV and IRR outputs
    output$npv <- renderText({npv_result()})
    output$irr <- renderText({irr_result()})
    
####### SCENARIO ANALYSIS #######
    # create vector of x values
    x_vec <- reactive({input$varRange[1] + 
        ((input$varRange[2] - input$varRange[1]) / 100) * 
        seq(0, 100)})
    # create vector of y values
    y_vec <- eventReactive(
        input$createPlot, {
            if (input$saType == "cash flows" & 
                input$discount_type == 0) {
                # fixed discount rate 
                genNPVs_cf(as.numeric(x_vec()), 
                           input$discount, 
                           as.numeric(cf_vector()))
            }else if (input$saType == "cash flows"){
                # variable discount rate
                genNPVs_cf(as.numeric(x_vec()),
                           as.numeric(vdr_vector()),
                           as.numeric(cf_vector()))
            }else if (input$saType != "cash flows" & 
                      input$discount_type == 0) {
                # fixed discount rate
                genNPVs_dr(as.numeric(x_vec()), 
                           input$discount, 
                           as.numeric(cf_vector()))
            }else{
                # variable discount rate
                genNPVs_dr(as.numeric(x_vec()),
                           as.numeric(vdr_vector()),
                           as.numeric(cf_vector()))
            }
        })
    output$test <- renderText({y_vec()})
    
    # create plot for scenario analyses
    
    output$saPlot <- renderPlot({qplot(
        x = as.numeric(isolate({x_vec()})), 
        y = as.numeric(y_vec()))
        })
    
}

shinyApp(ui = ui, server = server)