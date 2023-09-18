library(shiny)
library(shinycssloaders)
library(RColorBrewer)
library(colourpicker)
library(ggplot2)

reactive_data <- reactiveValues(lolliplot = NULL, polarplot = NULL)

cpalette <- brewer.pal(8, "Set2")
# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- fluidPage(
  includeCSS("/Users/spencer.trinhkinnate.com/Documents/Rscripts/shiny-apps/www/styles.css"),
  fluidRow(
    # column(width=12,
    #        # withSpinner(tableOutput('tb'), type = 2),
    # ),
    # column(width=4,
    #        fluidRow(
    #         column(1,
    #           "Fluid 2", style = "background-color:#4d3a7d;"),
    #         column(1,
    #           "Fluid 2", style = "background-color:#4d3a7d;")
    #       )
    # ),
    h2("TEST:ALSK:DJ"),
    column(width=4,
          fluidRow(
            h5("Node Legend Colours"),
            selectInput(inputId = "node_colours_tgt", label = "Target Nodes", choices = c(), selected = NULL, multiple = TRUE),
            colourInput('node_tgt_cpick', 'Text-Label', cpalette[1], showColour = "background"),
              fluidRow(
                box(
                  h5("Visualise mutant kinases"),
                  column(width = 5, h6('Lolli'), actionButton("click_lolli", "Plot")),
                  column(width = 5, h6('Polar'), actionButton("click_polar", "Plot"))
              )
            )
            # column(1,
            #   colourInput('node1', 'N1', cpalette[1])
            # ),
            # column(1,
            #   colourInput('node2', 'N2', cpalette[2])
            # ),
            # column(1,
            #   colourInput('node3', 'N3', cpalette[3])
            # )
        ),
        fluidRow(
            plotOutput("polarplot"), 
            hr(),
            plotOutput("lolliplot")
        )
    )
)
)

server <- function(input, output,session) {
  
  # output$tb <- renderTable({
  #   Sys.sleep(3) # system sleeping for 3 seconds for demo purpose
  #   iris[1:5,]
  # })

  observeEvent(input$click_lolli, {
    cars2 <- cars + rnorm(nrow(cars))
    reactive_data$lolliplot <- ggplot(cars2, aes(x = speed, y = dist)) + geom_point()
  })

  observeEvent(input$click_polar, {
    dt <- mtcars %>% select(hp, cyl) + rnorm(nrow(mtcars))
    reactive_data$polarplot <- ggplot(dt, aes(x = hp, y = cyl)) + geom_point(colour = "blue")
  })

  output$lolliplot <- renderPlot({
    if (is.null(reactive_data$lolliplot)) return()
    reactive_data$lolliplot
  })

  output$polarplot <- renderPlot({
    if (is.null(reactive_data$polarplot)) return()
    reactive_data$polarplot
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
