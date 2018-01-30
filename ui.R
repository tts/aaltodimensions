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
               HTML("<p>Citation metrics by <a target='blank' href='http://dimensions.ai'>Dimensions API</a>, 
                        metadata by <a target='blank' href='https://confluence.csc.fi/display/VIR/REST-lukurajapinta'>VIRTA REST API</a> and <a target='blank' href='research.aalto.fi'>Aalto University CRIS</a>.</p>
                    <p>Updated 26 January 2018</p>
                    <p>See details in the Data tab</p>
                    <p>About, see <a href='https://blogs.aalto.fi/suoritin/'>this blog post (to come)</a>")
        )
      ),
    conditionalPanel(
      condition = "input.tabs == 'table'",
      tags$div(class="form-group shiny-input-container", 
               HTML("<p><b>authors</b> = number of authors</p>
                    <p><b>fieldcount</b> = number of different scientific fields</p>
                    <p><b>Times cited</b> are counted from the reference list in all publications indexed at Dimensions.</p>
                    <p><b>Recent citations</b> is the number of citations received in the last two years.</p>
                    <p><b>Relative Citation Ratio</b> (RCR) indicates the relative citation performance of an article when compared to other articles in its area of research.
                    A value of more than 1.0 shows a citation rate above average for this group, when defined by the subject area citation rates of the articles that have cited it. 
                    Articles that are less than 2 years old, or do not have citations, do not have an RCR.</p>
                    <p><b>Field Citation Ratio</b> (FCR) indicates the relative citation performance of an article, when compared to similarly-aged articles in its subject area.
                    A value of more than 1.0 shows citation above average for this group, when defined by FoR Subject Code, publishing year and age. 
                    The FCR is normalized to 1.0 for this selection of articles. Articles that are less than 2 years old do not have an FCR. An article with zero citations has an FCR of 0.</p>
                    <p><b>OA codes</b><dl><dt>0</dt><dd>Not OA</dd><dt>1</dt><dd>OA</dd><dt>2</dt><dd>Green OA</dd><dt>9</dt><dd>Not known</dd></dl></p>")
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
  dashboardHeader(title = "Citations metrics of Aalto University publications 2015-2017",
                  titleWidth = "500"),
  sidebar,
  body,
  skin = "black"
)

