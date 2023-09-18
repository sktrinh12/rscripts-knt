library(shiny)
library(RColorBrewer)
library(colourpicker)

ui <- fluidPage(
  tags$head(
		tags$script(src = "js.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("Old Faithful Geyser Data"),
    h2("TEST:ALSK:DJ"),
    column(width=4,
				  fluidRow(h5("Node Legend Colours")),
          fluidRow(
            selectInput(inputId = "node_colours_tgt", label = "Target Nodes", choices = c(), selected = NULL, multiple = TRUE),
            colourInput('node_tgt_cpick', '', cpalette[1], showColour = "background"),
				    actionButton("btn1", "show alert"),
				 sliderInput("controller", "Controller:", min = 1, max = 20, value = 15),
              fileInput(
                "kinasefile",
                "Choose Excel File",
                multiple = FALSE,
                accept = c(".xlsx")
              )

)),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  # observeEvent(input$btn1, {
				# message = "hello testing"
				# print(message)
				# session$sendCustomMessage("resetFileInputHandler", message)
  # })

  observeEvent(input$kinasefile, {
				message = "hello testing"
				print(message)
				session$sendCustomMessage("resetFileInputHandler", message)
  })

	observe({
    session$sendCustomMessage(type = 'testmessage',
      message = list(a = 1, b = 'text',
                     controller = input$controller))
  })
}

shinyApp(ui = ui, server = server)
