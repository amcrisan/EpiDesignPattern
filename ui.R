library(shiny)
library(shinydashboard)

dashboardPage(skin = "black",
  dashboardHeader(),
  dashboardSidebar(
    h2("Tree Options"),
    radioButtons(inputId="treeLayout","Layout",
                      choices=c("Rectanular"="rec",
                        "Circular"="circ"),
                      selected="rec"),
    selectizeInput(inputId="colorBy",
                   label="Color By",
                   choices=c("Date","Country","Region"),
                   multiple=FALSE,
                   selected="Country")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    box(title="Phylogeny",
        width=6,
        plotOutput("treePlot",click = "plot_click",
        dblclick = "plot_dblclick",
        hover = "plot_hover",
        brush = "plot_brush"),
        verbatimTextOutput("info")
        ),
    box(title="Geography",
        width=6)
  )
)

