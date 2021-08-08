library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
###
library(ggplot2)
library(MASS)

ui <- dashboardPage(
  dashboardHeader(title=tags$div(tags$b("grapesModeller", style = "color:#ffdc8a")),
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Dr. Pratheesh P Gopinath",
                                 message = "contact me at pratheesh.pg@kau.in",
                                 icon = icon("life-ring")
                               )
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "This is a teaching and 
                                 research tool",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "Probit analysis added",
                                 icon("list-alt"),
                                 status = "success"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 20, color = "green",
                                        "Projection completion"
                               )
                               
                  )
  ),
  dashboardSidebar( 
    sidebarMenu(
      menuItem("Read me", tabName = "readme", 
               icon = icon("list-alt")),
      menuItem("Linear Regression", tabName = "LR", 
               icon = icon("list-alt")),
      menuItem("Dose-Response study", 
               icon = icon("list-alt"),
               menuItem("Probit Analysis", 
                        tabName = "probit")
               ),
      menuItem("Non-Linear models", 
               icon = icon("list-alt"),
               menuItem("cobb douglas function", 
                        tabName = "CDF")
      ),
      menuItem("Time-series Models", tabName = "TSM", 
               icon = icon("list-alt")),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      h5(
        tags$div(
          tags$br(),
          tags$br(),
          tags$br(),
          "Developed by:",
          tags$br(),
          tags$br(),
          tags$b("Dr.Pratheesh P. Gopinath"),
          tags$br(),
          tags$b("Assistant Professor,"),
          tags$br(),
          tags$b("Agricultural Statistics,"),
          tags$br(),
          tags$b("Kerala Agricultural University"),
          tags$br(),
          tags$br(),
          style = "color:#ffdc8a"
        )
      )
      
    )
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
      # First tab content
      tabItem(tabName = "readme",
              uiOutput("note")
      ),
      
      # 2.1 tab content
      tabItem(tabName = "probit",
              fluidRow(
                box( 
                  title = tags$div(tags$b("Upload the File in CSV format here", 
                                          style = "color:#fbffb8")), 
                  fileInput("file1", "Click Browse to upload (csv format only)", 
                            accept=c("text/csv", 
                                     "text/comma-separated-values,text/plain", 
                                     ".csv")),
                  checkboxInput("header", "Header", TRUE),
                  tags$br(),
                  uiOutput("var"),
                  collapsible = TRUE, 
                  background="purple",
                  width=6
                ),
                
                box( 
                  title = tags$div(tags$b("Download Model data sets", 
                                          style = "color:#fbffb8")), 
        
                  uiOutput("dataset"),
                  collapsible = TRUE, 
                  background="light-blue",
                  width=6
                )
              ),
             
              h1("RESULTS"),
              tableOutput("aovSummary"),
              tags$style(type = "text/css", "#aovSummary th,td {border: medium solid #000000;text-align:center}"),
              tags$style(type = "text/css", "#aovSummary td {border: medium solid #000000;text-align:center}"),
              tableOutput("deviance"), # design
              tags$style(type = "text/css", "#deviance th,td {border: medium solid #000000;text-align:center}"),
              tags$style(type = "text/css", "#deviance td {border: medium solid #000000;text-align:center}"),
              tableOutput("goodness"), # design
              tags$style(type = "text/css", "#goodness th,td {border: medium solid #000000;text-align:center}"),
              tags$style(type = "text/css", "#goodness td {border: medium solid #000000;text-align:center}"),
              tableOutput("LD"),
              tags$style(type = "text/css", "#LD th,td {border: medium solid #000000;text-align:center}"),
              tags$style(type = "text/css", "#LD td {border: medium solid #000000;text-align:center}"),
              uiOutput("plotvars"),
              tags$br(),
              uiOutput("plotvars2"),
              tags$br(),
              plotOutput("plotout"),
              tags$br(),
              tags$br()
              ),
      
      # 1. tab content
      tabItem(tabName = "LR",
              h2("Coming Soon!")
              
      ),
      
      # 3. tab content
      tabItem(tabName = "TSM",
              h2("Coming Soon!")
              
      ),
      # 2.2 tab content
      tabItem(tabName = "CDF",
              h2("Coming Soon!")
              
      )
    )
  )
  ,skin = 'purple')


##################################


server <- function(input, output, session) {
  #change theme
  output$myTheme <- renderUI( 
    shinyDashboardThemes(theme = input$theme))
  
  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)) {
      return(NULL)
    }
    dt <- read.csv(csvfile$datapath, 
                   header = input$header, 
                   sep = ",",
                   check.names = FALSE)
    dt
  })

  ################ appear after uploading file
  on_file_upload<-
    eventReactive(input$file1$datapath, {
      list(
        radioButtons("dose", "Please pick the name given for stimulus (Dose) column in your file", choices = names(csvfile())),
        radioButtons("response", "Please pick the name given for response column in your file", choices = names(csvfile())),
        tags$br(),
        checkboxInput(
          "log_trans",
          "Use log10(Dose)", FALSE
        ),
        actionBttn(
          inputId = "submit_log",
          label = "should I use log transformation?",
          color = "danger",
          style = "minimal"
        ),
        tags$br(),
        tags$br(),
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
      
    })
  
  
  output$var <- renderUI({
    on_file_upload()
  })
  

 #shiny alert on log transformation
  
  observeEvent(input$submit_log,{
    shinyalert(
      title = "When to use log transformation",
      text = "First run an analysis without log transformation. 
      If the Residual Deviance is more than 
      Residual Degrees of Freedom, perform analysis by using 
      log transformation. Otherwise no need to use log transformation. [Montgomery et al., 2020]",
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
 
  ##### ANOVA TABLE
  anovatable<-
    eventReactive(input$submit, {
      if (input$log_trans > 0) {
        validate(
          need(input$dose != input$response, "Warning 1: Both input variables selected (Dose and Response) are same. 
               Choose Dose and Response column correctly above for meaningful result")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
        Response<-d[,input$response]
        data<-as.data.frame(cbind(Dose,Response))
        fit <- glm(
          formula = Response ~ log10(Dose), family = binomial(link = "probit"),
          data = data
        )
        result<-summary(fit)
        anovatable<-result$coefficients
        final<-as.data.frame(anovatable)
        final
      }
      else{
        validate(
          need(input$dose != input$response, "Warning 1: Both input variables selected (Dose and Response) are same. 
               Choose Dose and Response column correctly above for meaningful result")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
          Response<-d[,input$response]
          data<-as.data.frame(cbind(Dose,Response))
          fit <- glm(
            formula = Response ~ Dose, family = binomial(link = "probit"),
            data = data
          )
          result<-summary(fit)
          anovatable<-result$coefficients
          final<-as.data.frame(anovatable)
          final
          
      }
      
    }
    )
 
  output$aovSummary <- renderTable({
    anovatable()
  },
  digits = 3,
  caption = ("<b> Parameter Estimates </b>"),
  bordered = TRUE,
  align = "c",
  caption.placement = getOption("xtable.caption.placement", "top"),
  rownames = TRUE
  )
  
 #### Deviance measures
  deviance<-
    eventReactive(input$submit, {
      if (input$log_trans > 0) {
        validate(
          need(input$dose != input$response, "")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
        Response<-d[,input$response]
        data<-as.data.frame(cbind(Dose,Response))
        fit <- glm(
          formula = Response ~ log10(Dose), family = binomial(link = "probit"),
          data = data
        )
        result<-summary(fit)
        anovatable<-result$coefficients
        anovatable<-result$coefficients
        final<-as.data.frame(anovatable)
        Res_Dev<-result$deviance
        Null_Dev<-result$null.deviance
        Res_DF<-result$df.residual
        Null_DF<-result$df.null
        
        firstrow<-cbind(Null_Dev,Null_DF)
        secrow<-cbind(Res_Dev,Res_DF)
        final<-as.data.frame(rbind(firstrow,secrow))
        colnames(final)<-c("Deviance","Degrees of freedom")
        row.names(final)<-c("Null","Residual")
        final
      }
      else{
        validate(
          need(input$dose != input$response, "")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
        Response<-d[,input$response]
        data<-as.data.frame(cbind(Dose,Response))
        fit <- glm(
          formula = Response ~ Dose, family = binomial(link = "probit"),
          data = data
        )
        result<-summary(fit)
        anovatable<-result$coefficients
        anovatable<-result$coefficients
        final<-as.data.frame(anovatable)
        Res_Dev<-result$deviance
        Null_Dev<-result$null.deviance
        Res_DF<-result$df.residual
        Null_DF<-result$df.null
        
        firstrow<-cbind(Null_Dev,Null_DF)
        secrow<-cbind(Res_Dev,Res_DF)
        final<-as.data.frame(rbind(firstrow,secrow))
        colnames(final)<-c("Deviance","Degrees of freedom")
        row.names(final)<-c("Null","Residual")
        final
         }
    })
  
  output$deviance <- renderTable({
    deviance()
  },
  digits = 3,
  caption = ("<b> Deviance measures </b>"),
  bordered = TRUE,
  align = "c",
  caption.placement = getOption("xtable.caption.placement", "top"),
  rownames = TRUE
  )
  
 ### goodness of fit
  goodness<-
    eventReactive(input$submit, {
      if (input$log_trans > 0) {
        validate(
          need(input$dose != input$response, "")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
        Response<-d[,input$response]
        data<-as.data.frame(cbind(Dose,Response))
        fit <- glm(
          formula = Response ~ log10(Dose), family = binomial(link = "probit"),
          data = data
        )
        result<-summary(fit)
        DF<-result$df.residual
        pearson<-sum(residuals(fit, type = "pearson")^2)
        pval1<-pchisq(pearson, df=DF, lower.tail=FALSE)
        deviance<-sum(residuals(fit, type = "deviance")^2)
        pval2<-pchisq(deviance, df=DF, lower.tail=FALSE)
        
        row1<-cbind(round(pearson,4),DF,round(pval1,4))
        row2<-cbind(round(deviance,4),DF,round(pval2,4))
        resfinal<-rbind(row1,row2)
        rownames(resfinal)<-c("pearson","Deviance")
        colnames(resfinal)<-c("Chi square","DF","P-value")
        resfinal
      }
      else{
        validate(
          need(input$dose != input$response, "")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
        Response<-d[,input$response]
        data<-as.data.frame(cbind(Dose,Response))
        fit <- glm(
          formula = Response ~ Dose, family = binomial(link = "probit"),
          data = data
        )
        result<-summary(fit)
        DF<-result$df.residual
        pearson<-sum(residuals(fit, type = "pearson")^2)
        pval1<-pchisq(pearson, df=DF, lower.tail=FALSE)
        deviance<-sum(residuals(fit, type = "deviance")^2)
        pval2<-pchisq(deviance, df=DF, lower.tail=FALSE)
        
        row1<-cbind(round(pearson,4),DF,round(pval1,4))
        row2<-cbind(round(deviance,4),DF,round(pval2,4))
        resfinal<-rbind(row1,row2)
        rownames(resfinal)<-c("pearson","Deviance")
        colnames(resfinal)<-c("Chi square","DF","P-value")
        resfinal
      }
    })
  
  output$goodness <- renderTable({
    goodness()
  },
  digits = 3,
  caption = ("<b> Goodness of fit </b>"),
  bordered = TRUE,
  align = "c",
  caption.placement = getOption("xtable.caption.placement", "top"),
  rownames = TRUE
  )
  
  
  
  
  
  
  
  
  #### LD50
  LDvalue<-
    eventReactive(input$submit, {
      if (input$log_trans > 0) {
        validate(
          need(input$dose != input$response, "")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
        Response<-d[,input$response]
        data<-as.data.frame(cbind(Dose,Response))
        fit <- glm(
          formula = Response ~ log10(Dose), family = binomial(link = "probit"),
          data = data
        )
        LD_values<-dose.p(fit,p=c(0.5,0.9,0.95))
        LD<-rbind(LD_values[1],LD_values[2],LD_values[3])
        LD<-as.data.frame(LD)
        SE<-unname(attributes(LD_values)$SE[, 1])
        SE<-as.data.frame(SE)
        org<-as.data.frame(round(10^LD, 3))
        final<-cbind(LD,SE,org)
        colnames(final)<-c("Dose(transformed values)","SE","Original Values")
        row.names(final)<-c("LD50","LD90","LD95")
        final
      }
      else{
        validate(
          need(input$dose != input$response, "")
        )
        d <- as.data.frame(csvfile())
        Dose<-d[,input$dose]
        Response<-d[,input$response]
        data<-as.data.frame(cbind(Dose,Response))
        fit <- glm(
          formula = Response ~ Dose, family = binomial(link = "probit"),
          data = data
        )
        LD_values<-dose.p(fit,p=c(0.5,0.9,0.95))
        LD<-rbind(LD_values[1],LD_values[2],LD_values[3])
        LD<-as.data.frame(LD)
        SE<-unname(attributes(LD_values)$SE[, 1])
        SE<-as.data.frame(SE)
        final<-cbind(LD,SE)
        colnames(final)<-c("Dose","SE")
        row.names(final)<-c("LD50","LD90","LD95")
        final
        
      }
    })
  
  output$LD<- renderTable({
    LDvalue()
  },
  digits = 3,
  caption = ("<b> Lethal Dose </b>"),
  bordered = TRUE,
  align = "c",
  caption.placement = getOption("xtable.caption.placement", "top"),
  rownames = TRUE
  ) 
        
  ################ plot var
  plotvar<-
    eventReactive(input$submit, {
        actionBttn(
          inputId = "submit_plot",
          label = "Hey modeller draw a plot!",
          color = "royal",
          style = "minimal"
        )
      
    })
  
  plotvar2<-
    eventReactive(input$submit_plot, {
      list(
      checkboxInput("ldline", "show LD 50 line on the plot", TRUE),
      textInput("xlab", "Enter required x-axis label", "Dose Concentration"),
      textInput("ylab", "Enter required y-axis label", "Response (in proportion)"),
      textInput("title", "Give your plot title", "Dose-response curve"),
      textInput("LD", "Give lethal dose label", "LD50 =")
      )
       })  
        
  output$plotvars <- renderUI({
    plotvar()
  })   
  
  output$plotvars2 <- renderUI({
    plotvar2()
  })  
  
#### plotting
 
  output$plotout <- renderPlot({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit_plot)) {
      return()
    }
    if (is.null(input$ldline)) {
      return()
    }
    if (is.null(input$log_trans)) {
      return()
    }

    if (input$submit_plot>0){
      if (input$log_trans>0){
      d <- as.data.frame(csvfile())
      Dose<-d[,input$dose]
      Response<-d[,input$response]
      data<-as.data.frame(cbind(Dose,Response))
      fit <- glm(
        formula = Response ~ log10(Dose), family = binomial(link = "probit"),
        data = data
      )
      LD_values<-dose.p(fit,p=c(0.5,0.9,0.95))
      LD<-rbind(LD_values[1],LD_values[2],LD_values[3])
      LD<-as.data.frame(LD)
      SE<-unname(attributes(LD_values)$SE[, 1])
      SE<-as.data.frame(SE)
      final<-cbind(LD,SE)
    ld50val<-round(final[1,1],3)
      textlabel<- paste(input$LD, ld50val)
      
      if (input$ldline>0){
        p <- ggplot(
          data = data, # specify the data frame with data
          aes(x = log10(Dose), y = Response)
        ) + # specify x and y
          geom_point() + # make a scatter plot
          ylim(0, 1.5) +
          xlab(input$xlab) + # label x-axis
          ylab(input$ylab) + # label y-axis
          ggtitle(input$title) + # add a title
          theme_bw() # a simple theme
        
        
        # Add the line to graph
        p <- p + geom_smooth(
          method = "glm", method.args = list(family = binomial(link = "probit")),
          fullrange = TRUE, se = FALSE
        )
        # Add the text with the LD50 to the graph.
        p <- p + annotate(geom = "text", x = final[1,1], y = 1.25, label = textlabel, color = "black") 
          
        # add line LD_50
        p <- p + geom_segment(aes(x = final[1,1], y = 0.00, xend = final[1,1], yend = 1), linetype = 2)
        p
      }
      else{
        p <- ggplot(
          data = data, # specify the data frame with data
          aes(x = log10(Dose), y = Response)
        ) + # specify x and y
          geom_point() + # make a scatter plot
          ylim(0, 1.5) +
          xlab(input$xlab) + # label x-axis
          ylab(input$ylab) + # label y-axis
          ggtitle(input$title) + # add a title
          theme_bw() # a simple theme
        
        
        # Add the line to graph
        p <- p + geom_smooth(
          method = "glm", method.args = list(family = binomial(link = "probit")),
          fullrange = TRUE, se = FALSE
        )
        p 
      }
      
      }
    
   else{
      d <- as.data.frame(csvfile())
      Dose<-d[,input$dose]
      Response<-d[,input$response]
      data<-as.data.frame(cbind(Dose,Response))
      fit <- glm(
        formula = Response ~ Dose, family = binomial(link = "probit"),
        data = data
      )
      LD_values<-dose.p(fit,p=c(0.5,0.9,0.95))
      LD<-rbind(LD_values[1],LD_values[2],LD_values[3])
      LD<-as.data.frame(LD)
      SE<-unname(attributes(LD_values)$SE[, 1])
      SE<-as.data.frame(SE)
      final<-cbind(LD,SE)
      ld50val<-round(final[1,1],3)
      textlabel<- paste(input$LD, ld50val)
      
      if (input$ldline>0){
      p <- ggplot(
        data = data, # specify the data frame with data
        aes(x = Dose, y = Response)
      ) + # specify x and y
        geom_point() + # make a scatter plot
        ylim(0, 1.5) +
        xlab(input$xlab) + # label x-axis
        ylab(input$ylab) + # label y-axis
        ggtitle(input$title) + # add a title
        theme_bw() # a simple theme
       
     
      # Add the line to graph
      p <- p + geom_smooth(
        method = "glm", method.args = list(family = binomial(link = "probit")),
        fullrange = TRUE, se = FALSE
      )
      # Add the text with the LD50 to the graph.
      p <- p + annotate(geom = "text", x = final[1,1], y = 1.25, label = textlabel, color = "black") 
      
      # add line LD_50
      p <- p + geom_segment(aes(x = final[1,1], y = 0.00, xend = final[1,1], yend = 1), linetype = 2)
      p
      }
      else{
        p <- ggplot(
          data = data, # specify the data frame with data
          aes(x = Dose, y = Response)
        ) + # specify x and y
          geom_point() + # make a scatter plot
          ylim(0, 1.5) +
          xlab(input$xlab) + # label x-axis
          ylab(input$ylab) + # label y-axis
          ggtitle(input$title) + # add a title
          theme_bw() # a simple theme
        
        
        # Add the line to graph
        p <- p + geom_smooth(
          method = "glm", method.args = list(family = binomial(link = "probit")),
          fullrange = TRUE, se = FALSE
        )
        p 
      }
      
      
   }
    }
      
      },bg="transparent") 
  
  
  
  
  
  }

shinyApp(ui, server)