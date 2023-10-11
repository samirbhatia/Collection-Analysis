library(pacman)                       
p_load(tidyverse, janitor, lubridate, data.table, ggplot2, formattable, bslib, openxlsx)
select <- dplyr::select
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  # Application title
  titlePanel("Collection Analysis"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      dateRangeInput("daterange", "Date range:",
                     start = Sys.Date()-7,
                     end = Sys.Date(),
                     min = "2021-04-01",
                     max = Sys.Date()),
      selectInput("school", "Choose Branch", choices = c("All", unique(pay$school)) ),
      # selectInput("payment_mode", "Mode of Payment", choices = c("All", unique(pay$payment_mode)) ),
      # selectInput("collection_location", "Collection Location", choices = c("All", unique(pay$collection_location)) ),
      # selectInput("where_banked", "Where Banked", choices = c("All", unique(pay$where_banked) )),
      width = 3
    ),
    
    mainPanel(
      # Tab Panel 1
      tabsetPanel(
        tabPanel("Summary", fluidRow(
          plotOutput("colln"),
          dataTableOutput("summary")
          )),
      # Tab Panel 2
        tabPanel("Day-wise Summary", fluidRow(
          # downloadButton("report", "Generate report"),
          plotOutput("dwcolln"),
          dataTableOutput("dwsummary")
          )),
      # Tab Panel 3
        tabPanel("Summary by Collection Type", fluidRow(
          plotOutput("ctcolln"),
          dataTableOutput("ctsummary")
          )),
      # Tab Panel 4
        tabPanel("Detail", fluidRow(
          downloadButton("download", "Download File", style = "margin: 20px"),
          dataTableOutput("payments")
          ))
        ) #tabset panel
      ) #main panel
    ) # sidebarlayout
  ) # fluidpage

  
