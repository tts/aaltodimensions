function(input, output, session) {
  
  # When school is selected, filter data with it
  schoolData <- reactive({
    
    if ( input$school == "Aalto University" ) return(data) else data[data$parent %in% input$school, ]
    
  })

  
  
  # Update dept list of the selected school
  observe(
    updateSelectInput(session, 
                      inputId = 'dept', 
                      choices = if ( input$school == "Aalto University" ) "Select school first" else c("All", sort(unique(schoolData()$name)))
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
      mutate(url = paste0("<a target='blank' href='https://doi.org/", doi, "'>", doi, "</a>"),
             oa_url = ifelse(!is.na(urls),
                             paste0("<a target='blank' href='", urls, "'>", urls, "</a>"),
                             urls)) %>% 
      rename(oa_virta = oa) %>% 
      select(url, title, year, is_oa, oa_url, times_cited, wos, recent_citations, relative_citation_ratio, field_citation_ratio, 
             tweets, mendeley, altm_score,
             authors, oa_virta, fieldcount, parent, name)
    
  })
  
  
  output$scatter <- renderPlotly({
    
    df <- deptData()
    
    colors <- df$color
    
    title <- if(is.null(input$dept) || input$dept == "Select school first" || input$dept == "All") input$school
        else paste0(input$school, ", ",input$dept)
    
    p <- plot_ly(df, 
                 type = "scatter",
                 x = ~get(input$xc),
                 y = ~get(input$yc),
                 mode = "markers",
                 colors = colors,
                 color = I(colors),
                 split = ~parent,
                 symbol = ~symbol,
                 marker = list(size = 10, opacity = 0.7), 
                 hovertext = ~text) %>% 
      layout( xaxis = list( title = input$xc,
                            type = input$xscale), 
              yaxis = list( title = input$yc,
                            type = input$yscale),
              title = title)
    
  })
  
  
  output$prop_oa <- renderValueBox({
    valueBox(
      "With OA full text",
      ifelse(input$school == "Aalto University",
             with_oa_uniquedois,
             paste0(nrow(deptData()[!is.na(deptData()$urls) & !duplicated(deptData()[,1]), ]), " (", 
                    floor(nrow(deptData()[!is.na(deptData()$urls) & !duplicated(deptData()[,1]), ]
                               ) / nrow(deptData()[!duplicated(deptData()[,1]), ]) * 100), "% )")), 
      icon = icon("calculator"),
      color = "yellow"
    )
    
  })
  
  
  output$heatmap <- renderPlotly({
    
    if(is.null(input$dept) || input$dept == "Select school first" || input$dept == "All")
      return()
    
    dept_data <- f_means %>% 
      filter(name == input$dept)
    
    rnames <- dept_data[["field_name"]]
    
    mat_data <- data.matrix(dept_data[,c(4:8)])
    rownames(mat_data) <- rnames
    
    plot_ly(type = "heatmap",
            x = colnames(mat_data), y = rownames(mat_data), z = mat_data, 
            key = mat_data, source = mat_data) %>% 
      layout(title = paste0("Mean metrics by field in ",input$dept),
             xaxis = ax, yaxis = ay, 
             showlegend = FALSE, 
             autosize = F, 
             width = w, 
             height = h, 
             margin = m)
    
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
  
  
  output$data_dim_aalto = downloadHandler(
    filename = function() {
      filename = 'data_dim_aalto.csv'
    },
    content = function(file) {
      {
        write.csv(deptData(), 'temp.csv', row.names = FALSE)
        file.rename('temp.csv', file)    
      } 
    }
  )
  
  
}
