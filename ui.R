shinyUI(fluidPage(
  headerPanel("Explore NONMEM output"),
  
  sidebarPanel(
    fileInput("outputfile",label="Read NONMEM output file")  ,
    br(),
    uiOutput("choose_IDvar"),
    uiOutput("choose_Xvar"),
    uiOutput("choose_Y1var"),
    uiOutput("choose_Y2var"),
    uiOutput("choose_COVvar"),
    uiOutput("choose_COVn"),
    br()
  ),
  
  sidebarPanel(
#    radioButtons("PlotMethod",label=h4("Plot type"),c("XY Scatter plot","profile plot")),
    checkboxGroupInput("PlotOptions",label=h5("Options"),
                      choices=list("lowess"=1,"y=0"=2,"y=x"=3))
    ),
    
  mainPanel(
#    h4("Summary Statistics"),
#    verbatimTextOutput("summary"),
    plotOutput("plot")
  )
))
