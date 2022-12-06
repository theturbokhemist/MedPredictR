# library(shiny)
# library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = T),
  dashboardBody(useShinyjs(),
                
                
                tags$head(tags$style(HTML("
                                #textPrediction {
                                  text-align: center;
                                }
                                div.box-header {
                                  text-align: center;
                                }
                                "))),
                
                navbarPage(
                  "MedPredictR (v1.02)",
                  id = "main_navbar",
                  
                  tabPanel("Home", imageOutput("logoImg", height = "320px"),
                           hr(),
                           h3(strong("Project Description and Motivation")),
                           p(style="text-align: justify; font-size = 25px",
                             "For many years, polling has shown that high health care costs are a burden
                             on U.S. families and rank as one of their top financial concerns. Moreover, 
                             there are currently very few publically available tools and data sources that
                             would assist families in learning more about these costs and ways of reducing them.
                             Our mission is to empower citizens by providing them the ability to analyze 
                             totally anonymous and untracable medical expense data to make more informed financial
                             decisions." ),
                           p(style="text-align: justify; font-size = 25px",
                             "MedPredictR is a Shiny application made with 
          the primary purpose of",em("predicting annual medical expenses for uninsured families."),
                             "We employ an ensemble of state of the art regression models in order to maximize
                             the probability of providing an accurate estimation of costs. The data is 
                             collected from a variety of sources including from users theselves that wish to
                             contribute in a crowdsourcing effort. We also provide users with the
                             tools to explore and visualize the underling data and models themselves."),
                  p(style="text-align: justify; font-size = 25px",
                    "We employ an ensemble of state of the art regression models in order to maximize
                             the probability of providing an accurate estimation of costs. The data is 
                             collected from a variety of sources including from users theselves that wish to
                             contribute in a crowdsourcing effort." ),
                           
                           tags$blockquote("Shiny-Box is still under continuous development. 
           Please look forward to future updates! Visit",
                      a(href = "https://github.com/NabiilahArdini/Shiny-Box",
                        "MedPredictR GitHub Page"),
                      "to access the source code."),
                           hr()
                  ),
                  tabPanel(
                    "Predict",
                    
                    h4(strong("Project Description")),
                    p(style="text-align: justify; font-size = 25px",
                      "Shiny-Box is a Shiny application made with 
          the purpose of", 
                      em("demonstrating various use of shiny features"), 
                      "that are frequently asked by users but rarely 
          discussed in a beginner or formal class environment. 
          With an example app and its code, Shiny-Box hopefully 
          will ease your understanding on the mechanism 
          of various Shiny features. Go to",
                      a(href = "https://github.com/NabiilahArdini/Shiny-Box",
                        "Shiny-Box GitHub Page"),
                      "to find more details on the source code."),
                    
                    tags$blockquote("Shiny-Box is still under continuous development. 
           Please look forward to future updates!"),
                    hr(),
                    
                    fluidRow(align="center",
                             column(width = 6, 
                                    numericInput("age", "Age", 40, min = 18, max = 100, step = 2)
                             ),
                             
                             column(width = 6, 
                                    selectInput("region", label = "Region", choices = region)
                                    
                             )
                    ),
                    fluidRow(align="center",
                             column(width = 6, 
                                    numericInput("weight", "Weight (lb)", 150, min = 80, max = 400, step = 10)
                             ),
                             column(width = 6, 
                                    numericInput("height", "Height (inches)", 70, min = 36, max = 96, step = 1)
                             )
                    ),
                    
                    fluidRow(align="center",
                             
                             column(width = 3,
                                    radioButtons("biosex", "Sex",
                                                 choices = list("Male" = "Male", "Female" = "Female"))
                             ),
                             column(width = 3,
                                    radioButtons("smoker", "Smoker",
                                                 choices = list("No" = "No", "Yes" = "Yes"))
                             ),
                             column(width = 6,
                                    sliderInput("children", "Number of Children", 0, 12, 0)
                             )
                    ),
                    
                    fluidRow(align="center",
                             actionButton("predict", "Predict")
                    ),
                    br(),
                    
                    box(h2(strong(textOutput("textPrediction"))), status = "primary", solidHeader = TRUE, 
                        collapsible = F, width = 12, title = "Our model expects your medical expenses to be around:"),
                    
                    fluidRow(align="center",
                             
                             imageOutput("waterfallPlot")
                             
                    ),
                    br(),
                    hr(),

                    
                    fluidRow(align="center",
                             
                               uiOutput(outputId = "waterfallDescription")
                               
                             ),
                    
                    fluidRow(align="center",
                             
                             uiOutput("errorButton")
                    )
                  ),
                  
                  tabPanel(
                    "Visualize",
                    
                    fluidRow(align="center",
                             h5(strong("Feature Exploration"))),
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        selectInput(
                          "feature",
                          label = "Select Feature to Visualize",
                          choices = features
                        )
                      ),
                      mainPanel(plotOutput("featurePlot"))
                    ),
                    br(),
                    hr(),
                    h5(strong("Feature Relationships")),
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        selectInput(
                          "feature2",
                          label = "Select Feature to Correlate",
                          choices = c(features[-7], "All")
                        )
                      ),
                      mainPanel(plotOutput("featurePlot2"))
                    ),
                    br(),
                    hr(),
                    h5(strong("Model Comparisons")),
                    sidebarLayout(
                      sidebarPanel(
                        width = 3,
                        selectInput(
                          "metric",
                          label = "Select Metric to Compare",
                          choices = metrics
                        )
                      ),
                      mainPanel(plotOutput("modelMetricsPlot"))
                    )
                  ),
                  
                  tabPanel(
                    "Crowdsource",
                    
                    h4(strong("Project Description")),
                    p(style="text-align: justify; font-size = 25px",
                      "Shiny-Box is a Shiny application made with 
          the purpose of", 
                      em("demonstrating various use of shiny features"), 
                      "that are frequently asked by users but rarely 
          discussed in a beginner or formal class environment. 
          With an example app and its code, Shiny-Box hopefully 
          will ease your understanding on the mechanism 
          of various Shiny features. Go to",
                      a(href = "https://github.com/NabiilahArdini/Shiny-Box",
                        "Shiny-Box GitHub Page"),
                      "to find more details on the source code."),
                    
                    tags$blockquote("Shiny-Box is still under continuous development. 
           Please look forward to future updates!"),
                    hr(),
                    
                    fluidRow(align="center",
                             column(width = 6, 
                                    numericInput("age", "Age", 40, min = 18, max = 100, step = 2)
                             ),
                             
                             column(width = 6, 
                                    selectInput("region", label = "Region", choices = region)
                                    
                             )
                    ),
                    fluidRow(align="center",
                             column(width = 6, 
                                    numericInput("weight", "Weight (lb)", 150, min = 80, max = 400, step = 10)
                             ),
                             column(width = 6, 
                                    numericInput("height", "Height (inches)", 70, min = 36, max = 96, step = 1)
                             )
                    ),
                    
                    fluidRow(align="center",
                             
                             column(width = 3,
                                    radioButtons("biosex", "Sex",
                                                 choices = list("Male" = "Male", "Female" = "Female"))
                             ),
                             column(width = 3,
                                    radioButtons("smoker", "Smoker",
                                                 choices = list("No" = "No", "Yes" = "Yes"))
                             ),
                             column(width = 6,
                                    sliderInput("children", "Number of Children", 0, 12, 0)
                             )
                    ),
                    fluidRow(align="center",
                             numericInput("cost", "Medical Expenses (Dollars)", 5000, min = 0, max = 100000, step = 1000)
                             
                    ),
                    
                    fluidRow(align="center",
                             actionButton("submit", "Submit")
                             
                    )
                  )
                  
                )
  ))

server <- shinyServer(function(input, output, session) {
  
  output$logoImg <- renderImage({
    
    list(src = "logo.png",
         width = "100%",
         height = 330)
    
  }, deleteFile = F)
  
  counter <- reactiveValues(countervalue = 0) # Defining & initializing the reactiveValues object
  
  observeEvent(input$predict, {
    
    counter$countervalue <- counter$countervalue + 1
    if (counter$countervalue > 3) {
      
      counter$countervalue <- 1
    }
    
  })
  
  results <- eventReactive(input$predict, {
    
    prediction <- data[1,]
    prediction$Age <- as.integer(input$age)
    prediction$Sex <- input$biosex
    prediction$BMI <- input$weight*703/(input$height^2)
    prediction$Children <- input$children
    prediction$Smoker <- input$smoker
    prediction$Region <- input$region
    
    output <- predict(gbm_model2, newdata = prediction)
    output
  })
  
  # output$header2 <- renderText({
  #   a <- results()
  #   paste0("Our model expects your medical expenses to be around:", "")
  # })
  
  output$textPrediction <- renderText({
    paste0(round(results(), digits = 2), " Dollars")
  })
  
  ####Visualization Tab####
  
  reactFeature <- reactive({input$feature})
  
  output$featurePlot <- renderPlot({
    
    if(feature_types[1, reactFeature()] == "numeric") {
      
      ggplot(data, aes_string(reactFeature())) +
        geom_histogram(bins = 10, color="darkblue", fill="lightblue")
      
    } else {
      
      ggplot(data, aes_string(reactFeature(), fill = reactFeature())) +
        geom_bar()
      
    }
  })
  
  reactFeature2 <- reactive({input$feature2})
  
  output$featurePlot2 <- renderPlot({
    
    if(reactFeature2() == "All") {
      
      corr <- cor(data[,sapply(data,is.numeric)])
      ggcorrplot(corr, hc.order = TRUE, type = "lower",
                 lab = TRUE)
      
    } else if(feature_types[1, reactFeature2()] == "numeric") {
      
      ggplot(data, aes_string(x=reactFeature2(), y="Charges", color="Smoker")) + 
        geom_point()+
        geom_smooth(method=lm)
      
    } else {
      
      ggplot(data, aes_string(x=reactFeature2(), y="Charges", fill = reactFeature2())) + 
        geom_boxplot() + stat_summary(fun =mean, geom="point", shape=23, size=4) +
        scale_color_brewer(palette="Dark2")
      
    }
  })
  
  reactMetric <- reactive({input$metric})
  
  output$modelMetricsPlot <- renderPlot({
    
    metricPlotsList[reactMetric()]
    
  })
  
  output$waterfallPlot <- renderImage({
    # Return a list containing the filename
    temp <- results()
    
    list(src = waterfallPlots[counter$countervalue],
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  

  
  output$waterfallDescription <- renderUI({
    tagList(
      h3(strong("Why this prediction?")),
      p(style="text-align: justify; font-size = 25px",
        "The waterfall chart to the left explains why your prediction differs from the average person’s prediction."),
      p(style="text-align: justify; font-size = 25px",
        "The average prediction is shown at the bottom. Each factor that goes into the model is shown 
        in increasing order of impact going up. For example, a blue bar pointing left 
        means that your input for that feature decreases the model’s output from the average output by the listed number."),
      h4(strong("Where did these numbers come from?")),
      p(style="text-align: justify; font-size = 25px",
        "A recent application of an old game theory concept to ML allows for model agnostic explainability. 
        Each feature (input to the model) is playing a game for the output, and the
        game theory calculations give the share of the output that each feature is responsible for. 
        The strategy is called SHapley Additive exPlanations, or SHAP for short. Learn more ",
        a(href = "https://proceedings.neurips.cc/paper/2017/hash/8a20a8621978632d76c43dfd28b67767-Abstract.html",
          "here.")),
      hr()
    )
  }) |> bindEvent(input$predict)
  
  observeEvent(input$predict, {
    output$errorButton <- renderUI({
      actionButton("errorButton", label = "Report Bug")
    })
  })
  
  observeEvent(input$errorButton, {
    #Show a modal when the button is pressed
    shinyalert("State Captured!", "Thank you for submitting a bug report!", type = "success")
  })
  
  observeEvent(input$submit, {
    #Show a modal when the button is pressed
    shinyalert("Submission Successful!", "Thank you for helping improve our model quality!", type = "success")
  })
  
}
)

shinyApp(ui, server)