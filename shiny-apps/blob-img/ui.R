library(shiny)

shinyUI(fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      textInput("userID", "User ID:"),
      selectInput("ExperimentID", "Experiment ID:",
          choices = c(197466,
            197546,
            198586,
            197547,
            197584,
            198987,
            198824,
            198825,
            199004)
        ),
      selectInput("base_url","Choose server:",
                  list("Production" = list("https://dotmatics.kinnate.com/browser/api"),
                       "DEV" = list("http://dotmatics-2022-dev.fount/browser/api"))
                  )
    ),
    mainPanel(
      uiOutput("studyID"),
      uiOutput("modelID"),
      uiOutput("DosingSolutionImg"),
      tableOutput("StudyTable")
    )
  )
))
