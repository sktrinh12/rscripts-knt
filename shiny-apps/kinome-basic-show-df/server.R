server <- function(input, output, session) {

  # Return the unfiltered kinome dataset (all exp ids)
  get_unfil_kdata <- reactive({
    req(input$search_cmpd_ids)
    fetch_uf_kdata(input$search_cmpd_ids)
  })

  # Return the sub-final kinome dataset (single exp id)
  get_subfil_kdata <- reactive({
    req(input$search_cmpd_ids, input$exp_id)
    fetch_f_kdata(input$search_cmpd_ids,
                  input$exp_id 
                 )
  })

  # show the final df from compd_id and experiment id after clicking Submit
  observeEvent(input$submit, {
    req(input$search_cmpd_ids, input$exp_id, input$tech_id)
    # Return the final kinome dataset (single exp id + tech)
    df <- fetch_f_kdata(input$search_cmpd_ids,
                        input$exp_id, 
                        input$tech_id
                       )
    output$view <- renderDataTable(df, 
                                  options = list(pageLength = 15)
                   )
  })

  # poll the unique values every 1x10^6 milliseconds (~17 mins)
  current_cmpd_ids <- reactivePoll(1E6, 
                                    session, 
                                    checkFunc = fetch_len_cmpd_ids, 
                                    valueFunc = fetch_unq_cmpd_ids
                      )

  # update the search compound id input box with distinct values
  observe({
          reactive_data$valid_cmp_ids <- current_cmpd_ids()
          updateSelectizeInput(session, 
                              "search_cmpd_ids",
                              choices = reactive_data$valid_cmp_ids,
                              selected = NULL,
                              server = TRUE
          )
  })

  # get parameters from URL
  observe({
      query <- parseQueryString(session$clientData$url_search)
      print(query)
      if (!is.null(query[['search_cmpd_ids']])) {
        updateSelectizeInput(session, 
                              "search_cmpd_ids",
                              choices = reactive_data$valid_cmp_ids,
                              selected = query[['search_cmpd_ids']],
                              server = TRUE
          )
      }
  })

  # reactively update the experiment id input box with CROs
  output$exp_id_output <- renderUI({
      req(input$search_cmpd_ids)
      exp_id_list <- unique(get_unfil_kdata()$EXPERIMENT_ID)
      mdata_result <- sapply(exp_id_list, function(x) fetch_exp_mdata(x))
      if (length(mdata_result) > 0) {
        cros_n_expids <- sapply(seq(1:length(exp_id_list)), function(i) {
                                  paste0(mdata_result[, i]$EXPERIMENT_ID, " (", 
                                         mdata_result[, i]$PROPERTY_VALUE, ")")
                                })
        # dropdown menu that allows for selection of experimental id
        selectInput(inputId = "exp_id", label = "Experiment ID",
                  choices = cros_n_expids,
                  selected = NULL, multiple = FALSE)
      } else { return(NULL) }
  })

  # reactively update the technology input box
  output$tech_output <- renderUI({
      req(input$search_cmpd_ids, input$exp_id)
      tech_list <- unique(get_subfil_kdata()$TECHNOLOGY)
      # print(tech_list)
      if (length(tech_list) > 0) {
        # dropdown menu that allows for selection of experimental id
        selectInput(inputId = "tech_id", label = "Technology",
                  choices = tech_list,
                  selected = NULL, multiple = FALSE)
      } else { return(NULL) }
  })

  #--------------------------------------------
  # when session ends disconnect from Oracle db
  session$onSessionEnded(function() {
    # dbDisconnect(con_userdata)
    # dbDisconnect(con_pinpoint)
  })
}
