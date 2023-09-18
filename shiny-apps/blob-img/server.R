library(shiny)
library(dplyr)
library(httr)
library(jsonlite)

source("fetchData.R")

# Define server logic
shinyServer(function(input, output) {
  
  token_body <- reactive( authDM(input$base_url))      

  # Define StudyInfo reactive object
  StudyInfo <- reactive({
    TableID <- "870"
    ResultID <- c("EXPERIMENT_ID","STUDY_ID","PROJECT_ID","PROTOCOL_TYPE","IN_LIFE_DATE","REPORT_DATE","NOTES","IMAGE_FILE")
    df <- fetchData(token_body(),'ding.yuan',input$ExperimentID,TableID,ResultID,input$base_url)
    print("StudyInfo")
    print(df)
    tail(df,1)
  })
  
  # Define DosingSolutionImg UI element
  output$DosingSolutionImg <- renderUI({ 
    HTML(paste0('<img src="data:image/png;base64,',base64enc::base64encode(StudyInfo()$IMAGE_FILE), '"alt="Dosing Vials" width="300">'))
  })
  
  # Define startDate reactive object
  startDate <- reactive(StudyInfo()$IN_LIFE_DATE[1])
  
  # Define studyID UI element
  output$studyID <- renderUI({
    h3(paste("  Study ID:    ", StudyInfo()$STUDY_ID[1],sep="  "))
  })
  
  # Define modelID UI element
  output$modelID <- renderUI({
    h3(paste("  Model ID:    ", StudyInfo()$NOTES[1],sep="  "))
  })
  
  # Define StudyTable UI element
  output$StudyTable <- renderTable({
    df<- StudyInfo() %>% 
      mutate(REPORT_date = strsplit(as.character(REPORT_DATE), " ",1)[[1]][1]) %>% 
      select(-c("REPORT_DATE", "IMAGE_FILE"))
  })
  
})
