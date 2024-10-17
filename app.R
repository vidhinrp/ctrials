# intall.packages("shiny")
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("ct-util.R")
library(shiny)
max_num_studies = 1000

sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
    tags$form(class = "well", ...),
    out
  )
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Helvetica', sans-serif;
        color: #000000;
      }
      h1, h2, h3, h4, h5, h6, .shiny-title {
        font-family: 'Helvetica', sans-serif;
        color: #000000;
      }
      .shiny-input-container {
        font-family: 'Helvetica', sans-serif;
        color: #000000;
      }
      .dataTables_wrapper .dataTables_length, 
      .dataTables_wrapper .dataTables_filter, 
      .dataTables_wrapper .dataTables_info, 
      .dataTables_wrapper .dataTables_processing, 
      .dataTables_wrapper .dataTables_paginate, 
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        font-family: 'Helvetica', sans-serif;
        color: #000000;
      }

      .dataTable {
        font-family: 'Helvetica', sans-serif; 
        background-color: white;
      }
      .dataTable tbody tr {
        background-color: white !important; /
      }
    "))
  ),

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel2(id="sidebar",
      textInput("brief_title_kw", "Brief title keywords"),
      selectizeInput("lead_or_collaborator", "Select Sponsor Type", 
                      choices = c("choose" = "", c("Lead", "Collaborator", "Both")), 
                      selected= "Both"),
      selectizeInput("study_type", "Select Study Type", 
                    choices = c("Interventional", "Observational","Observational [Patient Registry]",
                    "Expanded Access", "All"), selected="All"),
      out = tags$p(HTML("This dashboard provides insights into clinical trials data using ClinicalTrials.gov data.<br><br> 
                         ClinicalTrials.gov is a comprehensive database that provides detailed information 
                         about clinical studies conducted worldwide. It includes key data such as study titles, 
                         NCT IDs, statuses, phases, and medical conditions being investigated.
                         This resource enhances transparency in clinical research and supports informed decision-making for 
                         patients and healthcare professionals.<br><br>
                         Use the dropdown menu to select different options and visualize the corresponding results."))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
         type = "tabs",
         tabPanel("Count by Study Phase", plotOutput("phase_plot")),
         tabPanel("Count of Publications Over Time", plotOutput("concurrent_plot")),
         tabPanel("Count by Conditions Examined", plotOutput("conditions_plot")),
         tabPanel("Count by Countries", plotOutput("country_plot_select")),
         tabPanel("Count by Intervention Type", plotOutput("intervention_plot"))
       ),
      DTOutput("trial_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
           strsplit(",") |>
           unlist() |>
           trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
    ret = ret |>
      merge(sponsors, by="nct_id", all.x=TRUE) |>
      select(-id, -name) |>
      dropdownspons(input$lead_or_collaborator)

    ret |>
      collect()|>
      dropdownstudy(input$study_type)|>
      head(max_num_studies) |>
      collect()
  })

  output$phase_plot = renderPlot({
    get_studies()|>
    plot_phase_histogram()
  })

  output$concurrent_plot = renderPlot({
      get_studies() |>
      select(start_date, completion_date) |>
      plot_concurrent_trials()
  })
  
  output$trial_table = renderDT({
    get_studies() |> 
      select(nct_id, brief_title, start_date, completion_date, lead_or_collaborator) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date, 
             `Sponsor Type` = lead_or_collaborator)
  })
    

  output$conditions_plot = renderPlot({
    get_studies() |>
    collect() |>
    plot_conditions()
  })



  output$intervention_plot = renderPlot({
    get_studies() |>
    collect() |>
    plot_intervention()
  })

  output$country_plot_select = renderPlot({
    get_studies() |>
    collect() |>
    plot_countries()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)