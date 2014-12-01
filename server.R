library(shiny)
library(ggplot2)
source("helpers.R")

shinyServer(function(input, output) {
  outputData<<-NULL
  
  Dataset <- reactive({
    if (is.null(input$outputfile)) {
      # User has not uploaded a file yet
      return(data.frame())
    }
    
    argList<-list(header=TRUE,na.strings=".",skip=1)
    outputData <<- as.data.frame(do.call("read.table",c(list(input$outputfile$datapath),argList)))
    return(outputData)
  })

  output$choose_IDvar <- renderUI({
    if(is.null(input$outputfile))
      return()
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)      
    outputData<<-Dataset()

    colnames <- colnames(outputData)

    selectInput("IDvar", "Choose ID variable", 
                choices  =c(" ",colnames) )
  })
  
  output$choose_Xvar <- renderUI({
    if(is.null(input$outputfile))
      return()    
    if(is.null(input$outputfile) | is.null(outputData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(outputData))
    }    
    selectInput("Xvar", "Choose X variable", 
                       choices  = choice.temp)
  })
  
  output$choose_Y1var <- renderUI({
    if(is.null(input$outputfile))
       return()
    if(is.null(input$outputfile) | is.null(outputData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(outputData))
    }
    selectInput("Y1var", "Choose Y variable(points)", 
                choices  =choice.temp )
  })
  
  output$choose_Y2var <- renderUI({
    if(is.null(input$outputfile))
      return()
    if(is.null(input$outputfile) | is.null(outputData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(outputData))
    }
    selectInput("Y2var", "Choose Y variable(line)", 
                choices  =choice.temp )
  })

  output$plot<-renderPlot({
    if(is.null(input$outputfile)| is.null(outputData) | is.null(input$Xvar)| is.null(input$Y1var)| is.null(input$IDvar))
    {  return()
    } else if(input$Xvar==" " | input$Y1var==" " | input$IDvar==" ")
    { return()
    } else  
    {
      X.name<-input$Xvar
      Y1.name<-input$Y1var
      Y2.name<-input$Y2var
      x.lim<-range(outputData[,input$Xvar],na.rm=TRUE)
      if(input$Y2var!=" ")
      { y.lim<-range(c(outputData[,input$Y1var],outputData[,input$Y2var]),na.rm=TRUE)
      } else
      { y.lim<-range(outputData[,input$Y1var],na.rm=TRUE)
      }   
      XYplot.output(outputData,input$Xvar,input$Y1var,input$Y2var,input$IDvar,x.lim,y.lim,input$PlotOptions)
    }
  })

})
