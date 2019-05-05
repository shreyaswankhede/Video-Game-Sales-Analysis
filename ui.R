library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
  skin = "black",
  dashboardHeader(
   disable = TRUE
  ),
  dashboardSidebar(
    
    div("Video Game Sales Analysis",id="headerDiv"),
    hr(),
    sidebarMenu(
      conditionalPanel(
          condition="input.maintab==2",
          selectizeInput(inputId="searchGame",choices=NULL,label='Search Game :')
      ),
      conditionalPanel(
        condition="input.maintab==3",
        selectizeInput(inputId="searchPublisher",choices=NULL,label='Search Publisher :')
      ),
      conditionalPanel(
        condition="input.maintab==4",
        selectizeInput(inputId="searchGenre",choices=NULL,label='Search Genre :')
      ),
      conditionalPanel(
        condition="input.maintab==5",
        selectizeInput(inputId="searchYear",choices=NULL,label='Search Year :')
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      tabBox(
        side = "left",width = "100%",id="maintab",selected = "1",
        tabPanel("Overview",value="1",
                    fluidRow(
                      div("Overview from Year 2000 to 2017",id="overviewdiv")
                    ),
                    fluidRow(
                        infoBoxOutput("bestSellingGame"),
                        infoBoxOutput("WorstSellingGame")
                        
                    ),
                    fluidRow(
                        infoBoxOutput("bestSellingPublisher"),
                        infoBoxOutput("WorstSellingPublisher")
                   
                    ),
                    fluidRow(
                        infoBoxOutput("bestSellingGenre"),
                        infoBoxOutput("WorstSellingGenre")
                   
                 )
               ),
        tabPanel("Game Analysis ",value = 2,
                    fluidRow(
                        fluidRow(column(12,tableOutput("mytable2"))),
                        fluidRow(column(12,plotOutput("plot2")))
                    )  
                ),
        tabPanel("Publisher Analysis",value = 3,
                    fluidRow(
                        fluidRow(column(12,tableOutput("mytable3"))),
                        fluidRow(column(12,plotOutput("plot3")))
                    ) 
                ),
        tabPanel("Genre Analysis",value = 4,
                    fluidRow(
                        fluidRow(column(12,tableOutput("mytable4"))),
                        fluidRow(column(12,conditionalPanel(condition="input.searchGenre!=''",plotOutput("plot4")))),
                        fluidRow(column(12,conditionalPanel(condition="input.searchGenre==''",plotlyOutput("plot7"))))
                    )
                ),
        tabPanel("Year wise Analysis",value = 5,
                   fluidRow(
                        fluidRow(column(12,tableOutput("mytable5"))),
                        fluidRow(column(12,conditionalPanel(condition="input.searchYear!=''",plotOutput("plot5")))),
                        fluidRow(column(12,conditionalPanel(condition="input.searchYear==''",plotlyOutput("plot6"))))
                   ) 
                )
      )
    )
  )
)