#############################################
# univariate meta-analysis with binary outcome
#############################################


uniModBi <- function(input,output,session){
  
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
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    print(mb1, digits = 2)
    
  })
  
  output$plot1 <- renderPlot({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    forest <- forest(mb1)
    
    
  }, height=600)
  
  output$sens_fix <- renderPlot({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    fix <- metacum(mb1, pooled = 'fixed')
    forest(fix)
  })
  
  output$sens_random <- renderPlot({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    random <- metacum(mb1, pooled = 'random')
    forest(random)
  })
  
  
  output$beggtest <- renderPrint({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    begg <- metabias(mb1, method = "rank")
    print(begg)
    
  })
  
  output$eggerstest <- renderPrint({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    egger <- metabias(mb1, method = 'linreg', plotit = TRUE)
    print(egger)
    
  })  
  
  output$eggersplot <- renderPlot({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    reg <- lm(I(mb1$TE/mb1$seTE) ~ I(1/mb1$seTE))
    radial(mb1)
    abline(reg)
    
  }, height = 600) 
  
  output$TStest <- renderPrint({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    TS <- metabias(mb1, method = "mm")
    print(TS)
    
  })
  
  output$FunnelPlot <- renderPlot({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    
    funnel(mb1,level=0.95,contour=c(0.9, 0.95, 0.99), shade=c("darkgray", "gray", "lightgray"))
    legend(-30, 1.25,c("0.1 > p > 0.05","0.05 > p > 0.01", "< 0.01"),fill=c("darkgray", "gray", "lightgray"),bty="n")
    
  }, height = 700)
  
  output$pvalue_egger <- renderPrint({
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    egger <- metabias(mb1, method = 'linreg', plotit = TRUE)
    a <- round(egger$p.value,digits = 2)
    cat(paste("P-value for Eggers Test is", a))
  })
  
  output$pvalue_begg <- renderPrint({
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    begg <- metabias(mb1, method = "rank")
    b <- round(begg$p.value, digits = 2)
    cat(paste("P-value for Begg Test is", b))
  })
  
  output$pvalue_TS <- renderPrint({
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    TS <- metabias(mb1, method = "mm")
    c <- round(TS$p.value, digits = 2)
    cat(paste("P-value for Thompson and Sharp Test is", c))
  })
  
  
  
  output$trimfill <- renderPrint({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    tf1 <- trimfill(mb1)
    print(tf1, digits = 2, comb.fixed = TRUE);
    
  })
  
  output$TFfunnel <- renderPlot({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    tf1 <- trimfill(mb1)
    funnel(tf1,level=0.95,contour=c(0.9, 0.95, 0.99), shade=c("darkgray", "gray", "lightgray"))
    legend(-30, 1.25,c("0.1 > p > 0.05","0.05 > p > 0.01", "< 0.01"),fill=c("darkgray", "gray", "lightgray"),bty="n")
    
  }, height = 600)
  
  output$copasSum <- renderPrint({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    print(summary(copas(mb1)), digits = 2)
    
  })
  
  
  output$regression <- renderPlot({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    reg <- limitmeta(mb1)
    funnel(reg,level=0.95,contour=c(0.9, 0.95, 0.99), shade=c("darkgray", "gray", "lightgray"))
    legend(-30, 1.25,c("0.1 > p > 0.05","0.05 > p > 0.01", "< 0.01"),fill=c("darkgray", "gray", "lightgray"),bty="n")
  }, height = 600)
  
  output$regSum <- renderPrint({
    
    dataset = data()
    mb1 <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study)
    reg <- limitmeta(mb1)
    print(summary(reg), digits = 2)
    
  })
  
  output$sub_group <- renderPlot({
    
    dataset = data()
    if (dim(dataset) == 6) {
      mb_s <- metabin(Ee, Ne, Ec, Nc, sm = "OR", method = "I", data = dataset,studlab =study,byvar = dataset[,6])
       forest(mb_s)
    }
  }, height=700, width = 780)
  
  output$sub_group_error <- renderText({
    
    dataset = data()
    if (dim(dataset)[2] != 6) {
      "No subgroup defined. Please check you dataset and add subgroup column (6th)"
    }
  })
  
  output$meta_reg <- renderPlot({
    
    dataset = data()
    if (dim(dataset)[2] != 5) {
      mb1 <- metabin(Ee, Ne, Ec, Nc, data = dataset,studlab = study)
      mb1_reg <- metareg(mb1, dataset[,6])
      bubble(mb1_reg)
    }
  })
  
  output$meta_reg_error <- renderText({
    
    ### ???
    dataset = data()
    if (dim(dataset)[2] == 5) {
      "Please double check if you have the regression covariate in your dataset."
    }
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_NMA_bi.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_uni_bi.Rmd")
      file.copy("report_uni_bi.Rmd", tempReport, overwrite = TRUE)
      
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
uniModBiUI <- function(id){
  ns <- NS(id)
  navlistPanel(
    tabPanel("Upload File (UMA with binary outcomes)",
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
    tabPanel('Cumulative Meta-analysis',plotOutput(ns('sens_fix')),plotOutput(ns('sens_random'))),
    'Heterogeneity Detection',
    tabPanel('Subgroup Analysis', verbatimTextOutput(ns('sub_group_error')), plotOutput(ns('sub_group'))),
    tabPanel('Meta-regression', verbatimTextOutput(ns('meta_reg_error')), plotOutput(ns('meta_reg'))),
    'Test for Small Study Effect',
    tabPanel('Test for Publication Bias',verbatimTextOutput(ns('pvalue_egger')),verbatimTextOutput(ns("pvalue_begg")),verbatimTextOutput(ns('pvalue_TS')),plotOutput(ns('FunnelPlot'))),
    tabPanel('Eggers Test', verbatimTextOutput(ns('eggerstest')), plotOutput(ns('eggersplot'))),
    tabPanel('Begg Test',verbatimTextOutput(ns('beggtest'))),
    tabPanel('Thompson and Sharp Test', verbatimTextOutput(ns('TStest'))),
    'Adjusted PB Effect',
    tabPanel('Trim-and-Fill Method', verbatimTextOutput(ns('trimfill')), plotOutput(ns('TFfunnel'))),
    tabPanel('Copas Selection Model',verbatimTextOutput(ns('copasSum'))),
    tabPanel('Regression', verbatimTextOutput(ns('regSum')), plotOutput(ns('regression'))),
    'Download Report',
    tabPanel('Meta-analysis Report in PDF', downloadButton(ns("report"), "download PDF report"))
  )
}
