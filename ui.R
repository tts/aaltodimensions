sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Scatterplot", tabName = "scatterplot"),
    menuItem("Data", tabName = "table"),
    menuItem("Heatmap by dept", tabName = "heat"),
    conditionalPanel(
      condition = "input.tabs == 'scatterplot'",
      selectInput(inputId = "school",
                  label = "school", 
                  choices = schools,
                  multiple = FALSE,
                  selected = "All"),
      selectInput(inputId = "dept",
                  label = "dept",
                  choices = NULL,
                  multiple = FALSE,
                  selected = NULL),
      selectInput("xc", "x axis", as.list(metrics), selected = "times_cited"),
      selectInput("yc", "y axis", as.list(metrics), selected = "wos"),
      selectInput("xscale", "x axis scale", as.list(scales), selected = "linear"),
      selectInput("yscale", "y axis scale", as.list(scales), selected = "linear"),
      tags$div(class="form-group shiny-input-container",
               HTML("<p>Citation metrics by <a target='blank' href='http://dimensions.ai'>Dimensions</a> and Web of Science, 
                        Open Access full text availability info by <a target='blank' href='http://unpaywall.org/api/v2'>Unpaywall</a>,
                        Mendeley reader counts, tweets and Altmetric score by <a target='blank' href='http://altmetric.com'>Altmetric</a>,
                        article metadata by <a target='blank' href='https://confluence.csc.fi/display/VIR/REST-lukurajapinta'>VIRTA</a>,
                        Aalto University organisations by <a target='blank' href='http://research.aalto.fi'>local CRIS</a>.</p>
                    <p><b>authors</b> = number of authors</p>
                    <p><b>fieldcount</b> = number of different scientific fields</p>
                    <p>For citation metrics explanations, see <a href='https://figshare.com/articles/Dimensions_Metrics_API_Documentation/5783694'>Dimensions API Documentation</a>,
                       and for altmetrics, see <a href='https://help.altmetric.com/support/solutions'>Altmetric data</a></p>
                    <p>About this app, see <a href='https://blogs.aalto.fi/suoritin/'>blog post (to come)</a></p>
                    <p>All data as of 26 Jan 2018 except WoS 4 Feb 2018, and Altmetric 13 Feb 2018</p>")
        )
      ),
    conditionalPanel(
      condition = "input.tabs == 'table'",
      tags$div(class="form-group shiny-input-container", 
               HTML("<p><b>authors</b> = number of authors</p>
                    <p><b>fieldcount</b> = number of different scientific fields</p>
                    <p>For citation metrics explanations, see <a href='https://figshare.com/articles/Dimensions_Metrics_API_Documentation/5783694'>Dimensions API Documentation</a>,
                       and for altmetrics, see <a href='https://help.altmetric.com/support/solutions'>Altmetric data</a></p>
                    <p>About this app, see <a href='https://blogs.aalto.fi/suoritin/'>blog post (to come)</a></p>
                    <p>All data as of 26 Jan 2018 except WoS 4 Feb 2018,and Altmetric 13 Feb 2018</p>")
      )),
    conditionalPanel(
      condition = "input.tabs == 'heat'",
      tags$div(class="form-group shiny-input-container", 
               HTML("<p>For citation metrics explanations, see <a href='https://figshare.com/articles/Dimensions_Metrics_API_Documentation/5783694'>Dimensions API Documentation</a>,
                       and for altmetrics, see <a href='https://help.altmetric.com/support/solutions'>Altmetric data</a></p>
                    <p>About this app, see <a href='https://blogs.aalto.fi/suoritin/'>blog post (to come)</a></p>
                    <p>All data as of 26 Jan 2018 except WoS 4 Feb 2018, and Altmetric 13 Feb 2018<</p>")
      )),
    id = "tabs"
  )
)


body <- dashboardBody(

  tabItems(
    
    tabItem("scatterplot",
            fluidRow(
              column(
                width = 9,
                heigth = "600px",
                plotlyOutput("scatter",
                             width = "100%",
                             height = "600px")
                ),
              column(
                width = 3,
                valueBoxOutput("prop_oa", width = "100%")
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
              ),
            fluidRow(
              column(width = 12,
                     height = "50px",
                     downloadButton("data_dim_aalto", "Download"))
            )
    ),
    
    tabItem("heat",
            fluidRow(
              column(
                width = 10,
                heigth = "600px",
                plotlyOutput("heatmap",
                             width = "100%",
                             height = "600px")
              )
            )
    )
    
  ))


dashboardPage(
  dashboardHeader(title = "Citations and OA full text availability of Aalto University publications 2015-2017",
                  titleWidth = "800"),
  sidebar,
  body,
  skin = "black"
)

