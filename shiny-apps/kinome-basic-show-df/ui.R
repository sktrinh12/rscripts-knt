source("helper.R")

ui <- fluidPage(

  # App title
  titlePanel("Compound Query for Kinome Tree Shiny App"),

  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

		  selectizeInput(inputId = 'search_cmpd_ids',
										 label = 'Search by Compound ID',
										 choices = NULL,
										 selected = NULL,
										 multiple = FALSE,
										 options = list(create = FALSE) # if TRUE, allows newly created inputs
			),

			hr(),
			uiOutput("exp_id_output"),
			uiOutput("tech_output"),
			actionButton("submit", "Submit")
			),

    # Main panel for displaying outputs
    mainPanel(

      # Output: dataframe of the kinase panel
      dataTableOutput("view")

    )
  )
)


