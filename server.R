function(input, output, session) {
  
  
  # When school is selected, filter data with it
  schoolData <- reactive({
     data %>%
        filter(parent %in% input$school) 
  })

  
  
  # Update dept list of the selected school
  observe(
    updateSelectInput(session, 
                      inputId = 'dept', 
                      choices = sort(c("All", unique(schoolData()$name)))
    )
  )
  
  
  # Filter school data by dept, if selected
  deptData <- reactive({
    if ( input$dept=="All" ) {
      return(schoolData())
    }
    isolate(schoolData() %>%  filter(name %in% input$dept))
  })
  
  
  
  # TO DO replace ggvis with plotly 
  # because ggvis is not developed any longer
  # and there's no geom_jitter
  ggvisdata <- reactive({
    
    show_title <- function(x=NULL) {
      if(is.null(x)) return(NULL)
      key <- x["keys"][[1]]
      deptData()$title[key]
    } 
    
    xvar_name <- input$xc 
    yvar_name <- input$yc
    
    xscale <- input$scale
    
    xc <- prop("x", as.symbol(input$xc))
    yc <- prop("y", as.symbol(input$yc))
    
    df <- deptData()
    
    df$keys <- seq_along(df[,1])
    
    df %>%
      transform(evidence = factor(evidence)) %>%
      ggvis(x = xc, 
            y = yc, 
            key := ~keys, 
            shape = ~evidence,
            fill := ~color,
           # stroke = ~stroke,
           # strokeWidth := 3,
            opacity := 0.70,
            size.hover := 200) %>%
      layer_points() %>%
      group_by(evidence) %>% 
      hide_legend("stroke") %>% 
      add_axis("x", 
               title = xvar_name,
               format = "d", # https://github.com/d3/d3-format#locale_format
               grid = FALSE,
               title_offset = 50,
               tick_padding = 7,
               offset = 10,
               properties = axis_props(
                 labels=list(
                   angle = 50, 
                   fontSize = 10))) %>%
      add_axis("y", 
               title = yvar_name, 
               title_offset = 50) %>% 
      set_options(width = "100%", 
                  height = "600px",
                  renderer = "canvas") %>%
      add_tooltip(show_title) %>%
      scale_numeric(property = "x", 
                    trans = xscale,
                    expand=0) 
    
  })
  
  ggvisdata %>% bind_shiny("gv")
  
  
  makedata <- reactive({
    
    totable <- deptData()
    
    totable <- totable %>% 
      mutate(url = paste0("<a target='blank' href='http://dx.doi.org/", doi, "'>", doi, "</a>"),
             oa_url = ifelse(!is.na(free_fulltext_url),
                             paste0("<a target='blank' href='", free_fulltext_url, "'>", free_fulltext_url, "</a>"),
                             free_fulltext_url)) %>% 
      select(url, title, year, times_cited, recent_citations, relative_citation_ratio, field_citation_ratio, 
           authors, oa, units, fieldcount, name, evidence, oa_url)
    
  })
  
  
  output$datatable <- DT::renderDataTable({
  
    dat <- datatable(makedata(), 
                     escape = FALSE, 
                     rownames = FALSE, 
                     extensions = "FixedHeader",
                     options = list(paging = TRUE, 
                                    fixedHeader = TRUE,
                                    autoWidth = TRUE,
                                    columnDefs = list(list(width = '150px', targets = "_all"))))
                                  
    return(dat)
    
})
  
  
  
}
