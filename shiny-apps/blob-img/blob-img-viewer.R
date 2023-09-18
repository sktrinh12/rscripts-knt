library(shiny)
library(base64enc)
library(rJava)
library(RJDBC)

Sys.setenv(HOSTNAME = "dotoradb.fount")
Sys.setenv(USERNAME = "ds3_userdata")
Sys.setenv(PASSWORD = "ds3_userdata")
Sys.setenv(PORT=1521)
Sys.setenv(SID="orcl_dm")

username <- Sys.info()[["effective_user"]]
root_path <- "/Users"


ojdbc_fp <- file.path(root_path, username, "lib/ojdbc8")
ojdbc_jarfile <- file.path(ojdbc_fp, "ojdbc8.jar")


jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = ojdbc_jarfile)

  ls_vars <- list(
    host=Sys.getenv('HOSTNAME'),
    port=Sys.getenv('PORT'),
    sid=Sys.getenv('SID'),
    username_userdata=Sys.getenv('USERNAME'),
    password_userdata=Sys.getenv('PASSWORD')
  )

connect_string <-
  paste0("jdbc:oracle:thin:@",
         ls_vars$host,
         ":",
         ls_vars$port,
         ":",
         ls_vars$sid)


  print(connect_string)

# userdata schema connection
conn <- dbConnect(jdbcDriver,
            connect_string,
            ls_vars$username_userdata,
            ls_vars$password_userdata)

# Define the UI
ui <- fluidPage(
  titlePanel("BLOB Image Viewer"),
  mainPanel(
    h3("Image Preview"),
    selectInput(inputId = "exp_id", label = "Select exp ID:", 
                choices = c(197466,
                            197546,
                            198586,
                            197547,
                            197584,
                            198987,
                            198824,
                            198825,
                            199004,
                            199711)), 
    uiOutput("img_output")
  )
)

# Define the server
server <- function(input, output, session) {
  
  # Retrieve the BLOB data from the database
  
  result <- reactive({
    query <- paste0("SELECT IMAGE_FILE FROM DS3_USERDATA.FT_PHARM_STUDY WHERE EXPERIMENT_ID = ", input$exp_id)
    stmt <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
    res <- .jcall(stmt, "Ljava/sql/ResultSet;", "executeQuery", query, check=FALSE)
    blobs <- list()
    col_num <- 1L
    i <- 1
    while(.jcall(res, 'Z', 'next')){
      blobs[[i]] <- .jcall(res, '[B', 'getBytes', col_num)
      i <- i + 1
    }
    encoded <- base64enc::base64encode(do.call(c, blobs))
    encoded
  })
  
  
  # Render the image in the HTML tag
  output$img_output <- renderUI({
    tags$img(src = paste0("data:image/png;base64,", result()))
  })
  
  
  
  # Disconnect from the Oracle database on app exit
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
