function(input, output, session) {
  
  
  # When school is selected, filter data with it
  schoolData <- reactive({
    
    if ( input$school == "All" ) return(data) else data[data$parent %in% input$school, ]
    
  })

  
  
  # Update dept list of the selected school
  observe(
    updateSelectInput(session, 
                      inputId = 'dept', 
                      choices = if ( input$school == "All" ) "Select school first" else c("All", sort(unique(schoolData()$name)))
    )
  )
  
  
  # Filter school data by dept, if selected
  deptData <- reactive({
    
    if ( is.null(input$dept) || input$dept == "Select school first" || input$dept == "All") {
      return(schoolData())
      
    }
    
    isolate(schoolData() %>%  filter(name %in% input$dept))
    
  })
  
  
  # Prepare URLs for datatable
  makedata <- reactive({
    
    totable <- deptData()
    
    totable <- totable %>% 
      mutate(url = paste0("<a target='blank' href='http://dx.doi.org/", doi, "'>", doi, "</a>"),
             oa_url = ifelse(!is.na(urls),
                             paste0("<a target='blank' href='", urls, "'>", urls, "</a>"),
                             urls)) %>% 
      select(url, title, year, times_cited, times_cited_wos, recent_citations, relative_citation_ratio, field_citation_ratio, 
           authors, oa, is_oa, oa_url, fieldcount, parent, name)
    
  })
  
  
  output$plotly <- renderPlotly({
    
    df <- deptData()
    
    colors <- df$color
    
    df$xx <- df[[input$xc]]
    df$yy <- df[[input$yc]]
    
    p <- plot_ly(df, 
                 type = "scatter",
                 x = ~xx,
                 y = ~yy,
                 mode = "markers",
                 hoverinfo = "text",
                 colors = colors,
                 color = I(colors),
                 split = ~parent,
                 symbol = ~symbol,
                 marker = list(size = 10, opacity = 0.7), 
                 hovertext = ~text,
                 textposition = "middle right") %>% 
      layout( xaxis = list( title=input$xc), 
              yaxis = list( title=input$yc ) )
    
  })
  
  
  output$datatable <- DT::renderDataTable({
  
    dat <- datatable(makedata(), 
                     escape = FALSE, 
                     rownames = FALSE, 
                     extensions = "FixedHeader",
                     options = list(paging = TRUE, 
                                    fixedHeader = TRUE,
                                    autoWidth = TRUE,
                                    scrollX = TRUE,
                                    columnDefs = list(list(target = "_all", width = "50px"))))
    return(dat)
    
})
  
  
  
}
