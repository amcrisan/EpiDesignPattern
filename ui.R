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
        plotOutput("treePlot",
        brush = "plot_brush")#,
        #verbatimTextOutput("info")
        ),
    box(title="Geography",
        leafletOutput("caseMap"),
        width=6),
    box(title="Timeline",
        dygraphOutput("timeline"),
        width=12)
  )
)

