sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Scatterplot", tabName = "scatterplot"),
    menuItem("Data", tabName = "table"),
    conditionalPanel(
      condition = "input.tabs == 'scatterplot'",
      selectInput(inputId = "school",
                  label = "school", 
                  choices = schools,
                  multiple = FALSE,
                  selected = "School of Science"),
      selectInput(inputId = "dept",
                  label = "dept",
                  choices = NULL,
                  multiple = FALSE,
                  selected = "All"),
      selectInput("xc", "x axis", as.list(metrics), selected = "times_cited"),
      selectInput("yc", "y axis", as.list(metrics), selected = "authors"),
      selectInput("scale", "x axis scale", as.list(scales), selected = "linear"),
      tags$div(class="form-group shiny-input-container",
               HTML("<p>Citation metrics by <a target='blank' href='http://dimensions.ai'>Dimensions</a>, 
                        Open Access full text evidence and URL by <a target='blank' href='http://unpaywall.org/api/v2'>Unpaywall</a>,
                        article metadata by <a target='blank' href='https://confluence.csc.fi/display/VIR/REST-lukurajapinta'>VIRTA</a>,
                        Aalto University organisations by <a target='blank' href='http://research.aalto.fi'>local CRIS</a>.</p>
                    <p>For citation metrics explanations, see <a href='https://figshare.com/articles/Dimensions_Metrics_API_Documentation/5783694'>Dimensions API Documentation</a></p>
                    <p><b>VIRTA OA codes</b></br>0=Not OA</br>1=OA</br>2=Green OA</br>9=Not known</p>
                    <p>About this app, see <a href='https://blogs.aalto.fi/suoritin/'>this blog post (to come)</a></p>
                    <p>Data as of 26 January 2018</p>")
        )
      ),
    conditionalPanel(
      condition = "input.tabs == 'table'",
      tags$div(class="form-group shiny-input-container", 
               HTML("<p><b>authors</b> = number of authors</p>
                    <p><b>fieldcount</b> = number of different scientific fields</p>
                    <p>For citation metrics explanations, see <a href='https://figshare.com/articles/Dimensions_Metrics_API_Documentation/5783694'>Dimensions API Documentation</a></p>
                    <p><b>VIRTA OA codes</b></br>0=Not OA</br>1=OA</br>2=Green OA</br>9=Not known</p>
                    <p>About this app, see <a href='https://blogs.aalto.fi/suoritin/'>this blog post (to come)</a>")
      )),
    id = "tabs"
  )
)


body <- dashboardBody(

  tabItems(
    
    tabItem("scatterplot",
            fluidRow(
              column(
                width = 8,
                box(title = "Scatterplot by unit",
                    status = "success",
                    solidHeader = TRUE,
                    width = "100%",
                    height = "700px",
                    ggvisOutput("gv"))
              )
              )
            ),
    
    tabItem("table",
            fluidRow(
              column(width = 12,
                     height = "600px",
                     DT::dataTableOutput("datatable", 
                                         width = "100%",
                                         height = "600px"))
              )
    )
    
  ))


dashboardPage(
  dashboardHeader(title = "Citations and OA full text availability of Aalto University publications 2015-2017",
                  titleWidth = "600"),
  sidebar,
  body,
  skin = "black"
)

