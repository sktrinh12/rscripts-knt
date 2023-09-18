authDM <- function(base_url) {
  url<- paste(base_url,"/authenticate/requestToken/?expiration=6000000",sep = "")
  token_request <- GET(url=url,add_headers(.headers = c("Authorization"= "Basic ZGluZy55dWFuOkl0QGx5MjAxOQ==", "Cookie"= "JSESSIONID=9E316FDD11B662600093CD7B797F4BB9")))
  token_body <- content(token_request, as = 'parsed')
}

urlData <- function(token_body,userID,ProjectID,ExperimentID,TableID,ResultID,base_url){
  Results <- paste(userID,ProjectID,  paste(paste(TableID,ResultID,sep = "_"),collapse = ","),ExperimentID,sep = "/")
  url<- paste(base_url,"/data/",Results,"?token=",token_body,sep = "")
  print(url)
  url
}

base_url <- 'https://dotmatics.kinnate.com/browser/api'
token_body <- authDM(base_url)

fetchData <- function(token_body,userID,ExperimentID,TableID,ResultID,base_url) {
  ProjectID <- "6000"
  url <- urlData(token_body,userID,ProjectID,ExperimentID,TableID,ResultID,base_url)
  data_got <- GET(url)
  
  if (data_got["status_code"] != "200") {
    token_body <- authDM(base_url)
    url <- urlData(token_body,userID,ProjectID,ExperimentID,TableID,ResultID,base_url)
    data_got <- GET(url)}
  
  
  data_list <- jsonlite::fromJSON(content(data_got,'text'), simplifyVector = TRUE, flatten = TRUE)
  df_result <- data_list[[ExperimentID]][["dataSources"]][[TableID]]     %>%
    map_df(flatten_df)
}



data <- fetchData(token_body,'testadmin','208769',
          '886',c("SAMPLE_ID","ANIMAL_ID","SAMPLING_TIME","BW_G","L_MM","W_MM","TV_MM_3","OBSERVATION"), base_url)
data
