# app + R package
# Define UI for app that draws a histogram ----
test <- reactiveValues()
test$val = 20

ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = isolate(test$val))

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      numericInput(inputId = "age", label = "Age", value = isolate(test$val), min = 0, max = 200, width = "400px"),
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  # test = reactive({format(round(input$bins, digits = 1), nsmall = 1)})

  output$distPlot <- renderPlot({
    test$val = input$bins
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = paste("Histogram of waiting times", temperature:::gender[1,1], test$val)) # use data here!! OK then ...
    # todo HERE!!
    # updateNumericInput(session, "age", label = "Age", value = isolate(test$val))
    write.table(test$val, "test.txt")
  })
}
# a call to the shinyApp function
shinyApp(ui = ui, server = server)

