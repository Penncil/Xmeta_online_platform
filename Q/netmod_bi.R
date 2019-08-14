netModBi <- function(input, output, session){
  
  data <- reactive({ 
    validate(need(input$file, message = FALSE))
    inFile <- input$file
    df <- read.csv(inFile$datapath, header = input$header)
    return(df)
  })
  
  output$contents <- renderText({
    if (!is.na(data())){
      "Your dataset has been successfully uploaded. Please choose the tabs on the left panel for further analysis"
    }
  })
  
  output$mytable <- DT::renderDataTable({
    
    dataset = data()
    DT::datatable(dataset, options = list(orderClasses = TRUE))
    
  })
  
  output$summary <- renderPrint({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    print(mn1, digits = 2)
    
  })
  
  output$plot1 <- renderPlot({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    netgraph(mn1, seq = c("plac", "benf", "migl", "acar", "sulf",
                          "metf", "rosi", "piog", "sita", "vild"))
    
  }, height=600)
  
  output$plot2 <- renderPlot({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    netgraph(mn1, start="circle", iterate = TRUE, 
             col = "darkgray", cex = 1.5, 
             points =TRUE, col.points = "black", cex.points = 3,
             col.multiarm = "gray", allfigures = TRUE)
    
  }, height=600)
  
  output$plot3 <- renderPlot({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    forest(mn1, xlim = c(-1.5, 1), ref ="plac",
           leftlabs = "Contrast to Placebo",
           xlab = "HbA1c difference")
    
  }, height=600)
  
  output$plot4 <- renderPlot({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    forest(mn1, xlim = c(-1.5, 1), ref = "plac",
           leftlabs = "Contrast to Placebo",
           xlab = "HbA1c difference",
           pooled = "random")
    
  }, height = 500)
  
  output$summary2 <- renderPrint({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    round(decomp.design(mn1)$Q.decomp,3)
    
  })
  
  output$decomposition <- renderPrint({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    print(decomp.design(mn1)$Q.het.design, digits = 2)
    
  })
  
  output$summary3 <- renderPrint({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    round(decomp.design(mn1)$Q.inc.random, 3)
    
  })
  
  output$introduction <- renderText({
    
    "The Net Heat Plot is a graphical presentation. It displays in a single plot with the following two types of information:
    
    1. for each network estimate, the contribution of each design to this estimate
    2. for each network estimate, the extent ofo inconsistency due to each design"
    
  })
  
  output$explanation <- renderText({
    
    "     1. The grey squares have area propotional to the contribution from the treatment comparison in the column to the treatment comparison to the row.
    
    2. The composition of heterogeneity are displayed in colour on the top-left to bottom-right, with the largest heterogeneity shown in red in the top left corner.
    
    3. Red colour indicates that the evidence for the treatment comparison in the row from the design in the colomn is inconsistent; Conversely, blue indicates the evidence is consistent."
  })
  output$plot5 <- renderPlot({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    netheat(mn1)
    
  }, height = 600)
  
  output$plot6 <- renderPlot({
    
    dataset = data()
    mn1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataset, sm = "MD")
    netheat(mn1, random = TRUE)
    
  }, height = 600)
  }


###########################################################
###########################################################
###########################################################

netModBiUI <- function(id){
  ns <- NS(id)
  navlistPanel(
    tabPanel("Upload File (network MA with binary outcomes)",
             sidebarLayout(
               mainPanel(
                 fileInput(ns('file'), 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 tags$br(),
                 checkboxInput(ns('header'), 'Header', TRUE)
               ),
               mainPanel(
                 strong(tableOutput(ns('contents')))
               )
             )
    ),
    tabPanel('Data', DT::dataTableOutput(ns('mytable'))),
    'Estimate Overall Effect',
    tabPanel('Summary', verbatimTextOutput(ns('summary'))),
    tabPanel('Network Plot', plotOutput(ns('plot1'))),
    tabPanel('Additinoal Network Plot', plotOutput(ns('plot2'))),
    tabPanel('Forest Plot', plotOutput(ns('plot3')), plotOutput(ns('plot4'))),
    'Heterogeneity Statistic',
    tabPanel('Summary for Fixed Effect Model',verbatimTextOutput(ns('summary2'))),
    tabPanel('Decomposition', verbatimTextOutput(ns('decomposition'))),
    tabPanel('Summary for Random Effect Model', verbatimTextOutput(ns('summary3'))),
    'The Net Heat Plot',
    tabPanel('Introduction', verbatimTextOutput(ns('introduction'))),
    tabPanel('Plot for Fixed Effect Model', verbatimTextOutput(ns('explanation')), plotOutput(ns('plot5'))),
    tabPanel('Plot for Random Effect Model',plotOutput(ns('plot6')))
  )
}
