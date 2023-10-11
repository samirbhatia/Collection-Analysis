library(shiny)
library(pacman)                       
p_load(tidyverse, janitor, lubridate, data.table, ggplot2, formattable, bslib, openxlsx)
select <- dplyr::select
source("processing.R")

onStop(
  function()
  {
    dbDisconnect(db)
  }
)


Sys.sleep(5)
shinyApp(ui="ui.R", server = "server.R")