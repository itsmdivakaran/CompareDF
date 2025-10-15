# app.R (Version 2.1 - Bug Fix)
# A Shiny app to compare two SAS (.sas7bdat) datasets, with a tab for data viewing.

# 1. Load necessary libraries
# ---------------------------
# Make sure you have these installed:
# install.packages(c("shiny", "shinythemes", "haven", "arsenal", "dplyr", "DT"))

library(shiny)
library(shinythemes)
library(haven)    # To read SAS files
library(arsenal)  # For the core comparison function
library(dplyr)    # For data manipulation
library(DT)       # For interactive tables

# 2. Define the User Interface (UI)
# ---------------------------------
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("SAS Dataset Comparison Tool"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Instructions"),
      p("1. Upload the two SAS datasets you want to compare."),
      p("2. (Optional) Explore the raw data in the 'View Datasets' tab."),
      p("3. Enter the key variable(s) to match rows (e.g., 'USUBJID')."),
      p("4. Click 'Compare Datasets' to generate the report."),
      hr(),
      
      fileInput("file1", "Upload Dataset 1 (Base)",
                multiple = FALSE,
                accept = c(".sas7bdat")),
      
      fileInput("file2", "Upload Dataset 2 (Comparison)",
                multiple = FALSE,
                accept = c(".sas7bdat")),
      
      textInput("key_vars", "Enter Key Variable(s) (comma-separated)",
                placeholder = "e.g., USUBJID, VISITNUM"),
      
      actionButton("compare_btn", "Compare Datasets", icon = icon("exchange-alt"), class = "btn-primary btn-lg")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "results_tabs",
        
        tabPanel("View Datasets", 
                 h3("Explore Uploaded Data"),
                 p("Use the filter boxes at the top of each column to search for specific data. You can also click on column headers to sort."),
                 tabsetPanel(
                   tabPanel("Dataset 1 (Base)", DT::dataTableOutput("raw_data_1")),
                   tabPanel("Dataset 2 (Comparison)", DT::dataTableOutput("raw_data_2"))
                 )
        ),
        
        tabPanel("Comparison Summary", 
                 h3("Overall Comparison Report"),
                 p("This summary shows metadata differences and a high-level overview of mismatches in values."),
                 verbatimTextOutput("summary_output")
        ),
        tabPanel("Mismatched Rows", 
                 h3("Detailed View of Mismatched Rows"),
                 p("The table below displays only the rows and columns where data values did not match. 'version' indicates the source dataset."),
                 DT::dataTableOutput("mismatches_table")
        ),
        tabPanel("Unique Rows", 
                 h3("Rows Not Found in Both Datasets"),
                 p("These are rows that exist in one dataset but not the other, based on the provided key(s)."),
                 h4("Rows unique to Dataset 1 (Base)"),
                 DT::dataTableOutput("unique1_table"),
                 hr(),
                 h4("Rows unique to Dataset 2 (Comparison)"),
                 DT::dataTableOutput("unique2_table")
        )
      )
    )
  )
)

# 3. Define the Server Logic
# ----------------------------
server <- function(input, output, session) {
  
  data1 <- reactive({
    req(input$file1)
    tryCatch({
      haven::read_sas(input$file1$datapath)
    }, error = function(e) {
      showNotification(paste("Error reading Dataset 1:", e$message), type = "error")
      return(NULL)
    })
  })
  
  data2 <- reactive({
    req(input$file2)
    tryCatch({
      haven::read_sas(input$file2$datapath)
    }, error = function(e) {
      showNotification(paste("Error reading Dataset 2:", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$raw_data_1 <- DT::renderDataTable({
    req(data1())
    DT::datatable(data1(),
                  filter = 'top', 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  output$raw_data_2 <- DT::renderDataTable({
    req(data2())
    DT::datatable(data2(),
                  filter = 'top', 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  comparison_results <- reactiveValues(summary = NULL, mismatches = NULL, unique1 = NULL, unique2 = NULL)
  
  observeEvent(input$compare_btn, {
    
    req(data1(), data2(), input$key_vars)
    
    showNotification("Starting comparison...", type = "message", duration = 5)
    
    tryCatch({
      df1 <- data1()
      df2 <- data2()
      
      keys <- trimws(unlist(strsplit(input$key_vars, ",")))
      
      if (!all(keys %in% names(df1)) || !all(keys %in% names(df2))) {
        stop("One or more key variables not found in both datasets. Please check names.")
      }
      
      comp <- arsenal::comparedf(df1, df2, by = keys, control = comparedf.control(tol.numeric = "absolute"))
      
      unique_df1 <- dplyr::anti_join(df1, df2, by = keys)
      unique_df2 <- dplyr::anti_join(df2, df1, by = keys)
      
      comparison_results$summary <- summary(comp)
      comparison_results$mismatches <- as.data.frame(comp$diffs.table)
      comparison_results$unique1 <- unique_df1
      comparison_results$unique2 <- unique_df2
      
      # THIS IS THE FIXED LINE
      showNotification("Comparison complete! View results in the report tabs.", type = "message", duration = 5)
      
      updateTabsetPanel(session, "results_tabs", selected = "Comparison Summary")
      
    }, error = function(e) {
      showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
    })
  })
  
  output$summary_output <- renderPrint({
    if (is.null(comparison_results$summary)) {
      "The comparison report will appear here after you click the 'Compare Datasets' button."
    } else {
      comparison_results$summary
    }
  })
  
  output$mismatches_table <- DT::renderDataTable({
    req(comparison_results$mismatches)
    DT::datatable(comparison_results$mismatches, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  caption = "Differences in values for rows matched by key variables.")
  })
  
  output$unique1_table <- DT::renderDataTable({
    req(comparison_results$unique1)
    DT::datatable(comparison_results$unique1, 
                  options = list(pageLength = 5, scrollX = TRUE),
                  rownames = FALSE,
                  caption = "These rows were found in Dataset 1 but not in Dataset 2.")
  })
  
  output$unique2_table <- DT::renderDataTable({
    req(comparison_results$unique2)
    DT::datatable(comparison_results$unique2, 
                  options = list(pageLength = 5, scrollX = TRUE),
                  rownames = FALSE,
                  caption = "These rows were found in Dataset 2 but not in Dataset 1.")
  })
}

# 4. Run the Shiny Application
# ----------------------------
shinyApp(ui = ui, server = server)