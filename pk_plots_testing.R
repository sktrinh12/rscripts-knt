library(shiny)
library(NonCompart)
library(ncar)
library(dplyr)
#library(ggplot2)
library(plotly)
library(httr)
library(jsonlite)
library(purrr)
library(tibble)
library(DT)
library(timevis)
library(anytime)
library(lubridate)


shinyServer <- function(input, output,session) {
  
  authDM <- function(base_url) {
    url<- paste(base_url,"/authenticate/requestToken/?expiration=6000000",sep = "")
    token_request <- GET(url=url,add_headers(.headers = c("Authorization"= "Basic ZGluZy55dWFuOkl0QGx5MjAxOQ==", "Cookie"= "JSESSIONID=9E316FDD11B662600093CD7B797F4BB9")))
    token_body <- content(token_request, as = 'parsed')
  }
  token_body <- reactive( authDM(input$base_url))  
  
  urlData <- function(token_body,userID,ProjectID,ExperimentID,TableID,ResultID){
    Results <- paste(userID,ProjectID,  paste(paste(TableID,ResultID,sep = "_"),collapse = ","),ExperimentID,sep = "/")
    url<- paste(input$base_url,"/data/",Results,"?token=",token_body,sep = "")
  }
  
  urlCell <- function(token_body,userID,ProjectID,BatchID,TableID,ResultID){
    Results <- paste(userID,ProjectID,  paste(paste(TableID,ResultID,sep = "_"),collapse = ","),sep = "/")
    url<- paste(input$base_url,"/data/",Results,"/",BatchID,"?token=",token_body,sep = "")
  }
  
  fetchData <- function(token_body,userID,ExperimentID,TableID,ResultID) {
    ProjectID <- "6000"
    url <- urlData(token_body,userID,ProjectID,ExperimentID,TableID,ResultID)
    data_got <- GET(url)
    
    if (data_got["status_code"] != "200") {
      token_body <- authDM(input$base_url)
      url <- urlData(token_body,userID,ProjectID,ExperimentID,TableID,ResultID)
      data_got <- GET(url)}
    
    
    data_list <- jsonlite::fromJSON(content(data_got,'text'), simplifyVector = TRUE, flatten = TRUE)
    df_result <- data_list[[ExperimentID]][["dataSources"]][[TableID]]     %>%
      map_df(flatten_df)
  }
  
  fetchCell <- function(token_body,userID,BatchID,TableID,ResultID) {
    ProjectID <- "14000"
    url <- urlCell(token_body,userID,ProjectID,BatchID,TableID,ResultID)
    data_got <- GET(url)
    
    if (data_got["status_code"] != "200") {
      token_body <- authDM(input$base_url)
      url <- urlCell(token_body,userID,ProjectID,BatchID,TableID,ResultID)
      data_got <- GET(url)}
    
    
    data_list <- jsonlite::fromJSON(content(data_got,'text'), simplifyVector = TRUE, flatten = TRUE)
    df_result <- data_list[[BatchID]][["dataSources"]][[TableID]]    %>%
      map_df(flatten_df)
  }
  
  
  
  output$versionNo <- renderText("V0.1")
  
  
  # Get Efficacy data -----------------------------------------------------
  EffData <- reactive({
    TableID <- "886"
    ResultID <- c("SAMPLE_ID","ANIMAL_ID","SAMPLING_TIME","BW_G","L_MM","W_MM","TV_MM_3","OBSERVATION")
    fetchData(token_body(),input$userID,input$ExperimentID,TableID,ResultID)
  })
  output$EffTable = DT::renderDataTable(EffData())
  
  
  
  # Get Dose data -----------------------------------------------------
  DoseData <- reactive({
    TableID <- "871"
    ResultID <- c("EXPERIMENT_ID","DOSE_ID","BATCH_ID","FORMULATION_ID","CONCENTRATION","CONCENTRATION_UNIT","TREATMENT","APPEARANCE","COMMENTS","MEAN_CONC","ACCURACY_PCT","SD")
    fetchData(token_body(),input$userID,input$BatchID,TableID,ResultID)
  })
  output$DoseTable = DT::renderDataTable(datatable(DoseData()))
  
  # Get Dosing data -----------------------------------------------------
  DosingData <- reactive({
    TableID <- "863"
    ResultID <- c("DOSING_ID","SUBJECT_ID","AMOUNT","AMOUNT_UNIT","DOSING_TIME","DOSING_TIME_UNIT")
    fetchData(token_body(),input$userID,input$ExperimentID,TableID,ResultID)
  })
  output$DosingTable = DT::renderDataTable({
    datatable(DosingData(),options = list(autoWidth = TRUE,order = list(list(2, 'asc')))) %>% formatRound('DOSING_TIME',1)
  })
  
  
  # Get Group Info -----------------------------------------------------  
  GroupInfo <- reactive({
    TableID <- "865"
    ResultID <- c("GROUP_ID","ROUTE","FREQUENCY","N_DOSING","SUBJECT_TYPE","SUBJECT_ID","DOSE_ID","DOSE","DOSE_UNIT","DOSING_SITE","IS_FED")
    fetchData(token_body(),input$userID,input$ExperimentID,TableID,ResultID)
  })    
  output$GroupTable = DT::renderDataTable(datatable(GroupInfo()))
  # Get Study info
  StudyInfo <- reactive({
    TableID <- "870"
    ResultID <- c("EXPERIMENT_ID","STUDY_ID","PROJECT_ID","PROTOCOL_TYPE","IN_LIFE_DATE","REPORT_DATE","NOTES","IMAGE_BASE64")
    fetchData(token_body(),input$userID,input$ExperimentID,TableID,ResultID)
  })
  #  studyID <- reactive(StudyInfo()$STUDY_ID[1])
  output$StudyTable = DT::renderDataTable(datatable(StudyInfo()))
  
  output$DosingSolutionImg <- renderUI({ 
    HTML(paste0('<img src="data:image/png;base64,',StudyInfo()$IMAGE_BASE64[1], '"alt="Dosing Vials" width="300">'))
  })