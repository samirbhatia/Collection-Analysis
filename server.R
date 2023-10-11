library(pacman)                       
p_load(tidyverse, janitor, lubridate, data.table, ggplot2, formattable, bslib, openxlsx,DT)
select <- dplyr::select

 # Server logic
server <- function(input, output) {
   p <- reactive({
     pay %>% 
       filter(payment_amount_new>0) %>% 
       filter(if(input$school!="All") {school==input$school} else {school==school}) %>% 
       filter(between(as.Date(payment_date), input$daterange[1], input$daterange[2]))
       # filter(if(input$payment_mode!="All") {payment_mode==input$payment_mode} 
             # else {payment_mode==payment_mode}) %>%
       # filter(if(input$where_banked!="All") {where_banked==input$where_banked} 
             # else {where_banked==where_banked}) 
    })
   #########################################################################################################
      # Tab Panel 1
   #########################################################################################################
   output$colln <- renderPlot({
     p() %>% 
       select(School, payment_date, payment_mode, collection_location, where_banked, payment_amount_new) %>% 
       group_by(School) %>% summarise(payment=sum(payment_amount_new)) %>% 
       ggplot(aes(x=School, y=payment, fill=School)) +
       geom_bar(stat="identity")+  
       # scale_y_continuous(labels = scales::unit_format(unit="L", scale=1e-5))+
       scale_y_continuous(labels = scales::label_number_si())+
       labs(x = "", y = "Collection Amount", fill = "")+
       theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
       scale_fill_brewer(palette = "Pastel1")
   })
   
   output$summary <- renderDataTable({
     dt <- p() %>%
       group_by(School) %>% 
       summarise(payment= sum(payment_amount_new, na.rm = TRUE)) %>% 
       rename(Amount = payment) %>%
       adorn_totals(where = "row")
     
     datatable(dt, options = list(columnDefs = list(list(className = 'dt-right', targets = 1, searchable = FALSE)), 
                                  dom = 't', filter=list(position = "top")),
               rownames = FALSE) %>% 
       formatCurrency(columns = "Amount", currency = '', interval = 3, mark = ",", before = FALSE, digits=0)
   })
   
   #########################################################################################################
      # Tab Panel 2 (Daily Collectoin)
   #########################################################################################################
    
    output$dwsummary <- renderDataTable({
      dt <- p() %>% 
        group_by(school, payment_date) %>% 
        summarise(payment= sum(payment_amount_new, na.rm = TRUE)) %>% 
        pivot_wider(names_from = school, values_from = payment, values_fill = 0) %>% 
        adorn_totals(where = c("row", "col")) %>%
        # mutate(across(where(is.numeric), ~accounting(.x, digits = 0))) %>% 
        mutate(payment_date=as.Date(payment_date)) %>% 
        rename(Payment_Date = payment_date)
        
      datatable(dt, option = list(
        columnDefs = list(list(className = 'dt-right', targets = 1:(ncol(dt)-1), searchable = FALSE)), 
        filter=list(position = "top")
      ), rownames = FALSE
      ) %>% 
        formatCurrency(columns = 2:ncol(dt), currency = '', interval = 3, mark = ",", before = FALSE, digits=0)
    })
    
   output$dwcolln <- renderPlot({
     p() %>% 
       select(school, payment_date, payment_mode, collection_location, where_banked, payment_amount_new) %>% 
       group_by(school, payment_date) %>% summarise(payment=sum(payment_amount_new)) %>% 
       ggplot(aes(x=payment_date, y=payment, fill=school)) +
       geom_bar(position = "dodge", stat="identity" ) +
       scale_y_continuous(labels = scales::label_number_si())+
       scale_x_datetime(date_breaks="2 days", date_labels = "%d-%m")+
       theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
       labs(title = "Collection Analysis",
            subtitle = paste0(input$daterange[1], " - ", input$daterange[2]))+
       scale_fill_brewer(palette="Pastel1")
   })
   
   output$report <- downloadHandler(
     # For PDF output, change this to "report.pdf"
     filename = "report.html",
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "report.Rmd")
       file.copy("report.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(n = input$slider)
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
   #########################################################################################################
      # Tab Panel 3 (Summary by Collection Type)
   #########################################################################################################
    
    output$ctsummary <- renderDataTable({
      dt <- p() %>% 
        mutate(payment_mode = case_when(
          str_detect(tolower(payment_mode), "neft") ~ "NEFT",
          str_detect(tolower(payment_mode), "cheque") ~ "Cheque",
          str_detect(tolower(payment_mode), "razor") ~ "Razorpay",
          TRUE ~ "Cash/ Bank"
        )) %>% 
        group_by(school, payment_mode) %>% 
        summarise(payment= sum(payment_amount_new, na.rm = TRUE)) %>% 
        pivot_wider(names_from = school, values_from = payment, values_fill = 0) %>% 
        adorn_totals(where = c("row", "col"))
        
      datatable(dt, option = list(
        columnDefs = list(list(className = 'dt-right', targets = 1:(ncol(dt)-1), searchable = FALSE)), 
        filter=list(position = "top")
      ), rownames = FALSE
      ) %>% 
        formatCurrency(columns = 2:ncol(dt), currency = '', interval = 3, mark = ",", before = FALSE, digits=0)
    })
   
   
   output$ctcolln <- renderPlot({
      p() %>% 
        mutate(payment_mode = case_when(
          str_detect(tolower(payment_mode), "neft") ~ "NEFT",
          str_detect(tolower(payment_mode), "cheque") ~ "Cheque",
          str_detect(tolower(payment_mode), "razor") ~ "Razorpay",
          TRUE ~ "Cash/ Bank"
        )) %>% 
        group_by(school, payment_mode) %>% 
        summarise(payment= sum(payment_amount_new, na.rm = TRUE)) %>% 
        ggplot(aes(x=school, y=payment, fill=payment_mode)) +
        geom_bar(position = "dodge", stat="identity" ) +
        scale_y_continuous(labels = scales::label_number_si())+
        # scale_x_datetime(date_breaks="2 days", date_labels = "%d-%m")+
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
        labs(title = "Collection Mode",
            subtitle = paste0(input$daterange[1], " - ", input$daterange[2]))+
       scale_fill_brewer(palette="Pastel1")
   })
    
   #########################################################################################################
      # Tab Panel 4
   #########################################################################################################
   
    output$payments <- renderDataTable({
      dt <- p() %>%  
        left_join(ipj %>% group_by(school, pay_id, stu_id) %>% summarise(s=sum(amount)), by = c("school", "pay_id")) %>% 
        left_join(stu, by = c("school", "stu_id")) %>% 
        group_by(school, pay_id, payment_date, admission_number, student_name, student_class, student_section) %>% 
        summarise(payment= sum(payment_amount_new, na.rm = TRUE)) %>% 
        mutate(payment_date= as.Date(payment_date)) %>% 
        arrange(payment_date, pay_id) %>% 
        adorn_totals(where = c("row"))
        
      datatable(dt, option = list(
        columnDefs = list(list(className = 'dt-right', targets = 1:(ncol(dt)-1), searchable = FALSE)), 
        filter=list(position = "top")
      ), 
      # rownames = FALSE
      ) %>% 
        formatCurrency(columns = 8, currency = '', interval = 3, mark = ",", before = FALSE, digits=0)
    })
    
    output$download <- downloadHandler(
      filename = function() {
        paste0(today(), " - Collection Report",  ".pdf")
      },
      content = function(file) {
        rmarkdown::render("report.Rmd", output_file=file)
      } )
}

#     output$download <- downloadHandler(
#       filename = function() {
#         paste0(today(), " - Collection Report",  ".xlsx")
#       },
#       content = function(file) {
#         openxlsx::write.xlsx(p() %>% select(!contains("status"), - lf_flag, -lock, -contains("account")), file)
#       } )
# }