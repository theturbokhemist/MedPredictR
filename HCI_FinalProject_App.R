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
        "MedPredictR",
        id = "main_navbar",
        
        tabPanel("Home",
                 
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
          
          fluidRow(align="center",
                   column(width = 12,
                   shinyjs::hidden(
                     div(style="text-align: justify",
                       id = "hiddenbox",
                       
                       box(
                         title = "StockPrice Reaction Around The Event Date", 
                         status = "primary", 
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         uiOutput(outputId = "waterfallDescription")
                       )
                     )
                   )
                )
          ),
          

          
          fluidRow(align="center",
                   
                   mainPanel(
                     uiOutput(outputId = "waterfallDescription2")
                     
                   ))
                
                        
          
          #h3(strong(textOutput("header2"))),
          #h2(strong(textOutput("textPrediction"))),
        ),
        
        tabPanel(
          "Visualize",
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
                   actionButton("predict", "Predict")
                   
          )
        )
        
      )
    ))

server <- function(input, output, session) {
  
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
  
  
  observeEvent(input$predict, {
    shinyjs::show(id = "hiddenbox")
  })
  
  
  output$waterfallDescription <- renderText({
    temp <- results()
    HTML(paste0("<b>","bold","</b>", " The waterfall chart to the left explains why your prediction 
    differs from the average person’s prediction.The average prediction is shown at the bottom. 
    Each factor that goes into the model is shown in increasing order of impact going up. 
    For example, a blue bar pointing left means that your input for that feature decreases the model’s 
    output from the average output by the listed number.
"))
  })
  

  
}

shinyApp(ui, server)
