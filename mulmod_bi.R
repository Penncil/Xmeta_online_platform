#############################################
# multivariate meta-analysis with binary outcome
#############################################


mulModBi <- function(input,output,session){
  
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
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    print(mc1, digits = 2)
    
  })
  
  output$plot1 <- renderPlot({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    forest <- forest(mc1, xlab =
                       "Difference in mean response (intervention - control)
                     units: maximum % fall in FEV1",
                     xlim = c(-50, 10), xlab.pos = -20, smlab.pos = -20)
    
    
  }, height=700)
  
  output$sens_fix <- renderPlot({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    fix <- metacum(mc1, pooled = 'fixed')
    forest(fix)
  })
  
  output$sens_random <- renderPlot({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    random <- metacum(mc1, pooled = 'random')
    forest(random)
  })
  
  
  output$beggtest <- renderPrint({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    begg <- metabias(mc1, method = "rank")
    print(begg)
    
  })
  
  output$eggerstest <- renderPrint({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    egger <- metabias(mc1, method = 'linreg', plotit = TRUE)
    print(egger)
    
  })  
  
  output$eggersplot <- renderPlot({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    reg <- lm(I(mc1$TE/mc1$seTE) ~ I(1/mc1$seTE))
    radial(mc1)
    abline(reg)
    
  }, height = 600) 
  
  output$TStest <- renderPrint({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    TS <- metabias(mc1, method = "mm")
    print(TS)
    
  })
  
  output$FunnelPlot <- renderPlot({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    
    funnel(mc1,level=0.95,contour=c(0.9, 0.95, 0.99), shade=c("darkgray", "gray", "lightgray"))
    legend(-30, 1.25,c("0.1 > p > 0.05","0.05 > p > 0.01", "< 0.01"),fill=c("darkgray", "gray", "lightgray"),bty="n")
    
  }, height = 700)
  
  output$pvalue_egger <- renderPrint({
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    egger <- metabias(mc1, method = 'linreg', plotit = TRUE)
    a <- round(egger$p.value,digits = 2)
    cat(paste("P-value for Eggers Test is", a))
  })
  
  output$pvalue_begg <- renderPrint({
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    begg <- metabias(mc1, method = "rank")
    b <- round(begg$p.value, digits = 2)
    cat(paste("P-value for Begg Test is", b))
  })
  
  output$pvalue_TS <- renderPrint({
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    TS <- metabias(mc1, method = "mm")
    c <- round(TS$p.value, digits = 2)
    cat(paste("P-value for Thompson and Sharp Test is", c))
  })
  
  
  
  output$trimfill <- renderPrint({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    tf1 <- trimfill(mc1)
    print(tf1, digits = 2, comb.fixed = TRUE);
    
  })
  
  output$TFfunnel <- renderPlot({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    tf1 <- trimfill(mc1)
    funnel(tf1)
    
  }, height = 600)
  
  output$copasSum <- renderPrint({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    print(summary(copas(mc1)), digits = 2)
    
  })
  
  
  output$regression <- renderPlot({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    reg <- limitmeta(mc1)
    funnel(reg,level=0.95,contour=c(0.9, 0.95, 0.99), shade=c("darkgray", "gray", "lightgray"))
    legend(-30, 1.25,c("0.1 > p > 0.05","0.05 > p > 0.01", "< 0.01"),fill=c("darkgray", "gray", "lightgray"),bty="n")
  }, height = 600)
  
  output$regSum <- renderPrint({
    
    dataset = data()
    mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,data = dataset,studlab = paste(author, year))
    reg <- limitmeta(mc1)
    print(summary(reg), digits = 2)
    
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      my.params <- list(n = data())
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = my.params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

###########################################################
###########################################################
###########################################################
mulModBiUI <- function(id){
  ns <- NS(id)
  navlistPanel(
    tabPanel("Upload File (multivariate MA with binary outcomes)",
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
                 strong(textOutput(ns('contents')))
               )
             )
    ),
    tabPanel('Data', DT::dataTableOutput(ns('mytable'))),
    'Estimate Overall Effect',
    tabPanel('Summary', verbatimTextOutput(ns('summary'))),
    tabPanel('Forest Plot', plotOutput(ns('plot1'))),
    tabPanel('Sensitivity Analysis',plotOutput(ns('sens_fix')),plotOutput(ns('sens_random'))),
    'Test for Small Study Effect',
    tabPanel('Test for Publication Bias',verbatimTextOutput(ns('pvalue_egger')),verbatimTextOutput(ns("pvalue_begg")),verbatimTextOutput(ns('pvalue_TS')),plotOutput(ns('FunnelPlot'))),
    tabPanel('Eggers Test', verbatimTextOutput(ns('eggerstest')), plotOutput(ns('eggersplot'))),
    tabPanel('Begg Test',verbatimTextOutput(ns('beggtest'))),
    tabPanel('Thompson and Sharp Test', verbatimTextOutput(ns('TStest'))),
    'Adjusted Overall Effect',
    tabPanel('Trim-and-Fill Method', verbatimTextOutput(ns('trimfill')), plotOutput(ns('TFfunnel'))),
    tabPanel('Copas Selection Model',verbatimTextOutput(ns('copasSum'))),
    tabPanel('Regression', verbatimTextOutput(ns('regSum')), plotOutput(ns('regression'))),
    'Download Report',
    tabPanel('Meta-analysis Report in PDF', downloadButton(ns("report"), "download PDF report"))
  )
}
