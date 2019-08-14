## app.R ##
library(shinydashboard)
library(metafor)
library(metasens)
library(datasets)

task_ui <- function(i){
  navlistPanel(
    tabPanel("Upload File",
             sidebarLayout(
               mainPanel(
                 fileInput('file', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE)
               ),
               mainPanel(
                 strong(textOutput('contents'))
               )
             )
    ),
    tabPanel('Data', DT::dataTableOutput('mytable')),
    'Estiamte Overall Effect',
    tabPanel('Summary', verbatimTextOutput('summary')),
    tabPanel('Forest Plot', plotOutput('plot1')),
    tabPanel('Sensitivity Analysis',plotOutput('sens_fix'),plotOutput('sens_random')),
    'Test for Small Study Effect',
    tabPanel('Test for Publication Bias',verbatimTextOutput('pvalue_egger'),verbatimTextOutput("pvalue_begg"),verbatimTextOutput('pvalue_TS'),plotOutput('FunnelPlot')),
    tabPanel('Eggers Test', verbatimTextOutput('eggerstest'), plotOutput('eggersplot')),
    tabPanel('Begg Test',verbatimTextOutput('beggtest')),
    tabPanel('Thompson and Sharp Test', verbatimTextOutput('TStest')),
    'Adjusted Overall Effect',
    tabPanel('Trim-and-Fill Method', verbatimTextOutput('trimfill'), plotOutput('TFfunnel')),
    tabPanel('Copas Selection Model',verbatimTextOutput('copasSum')),
    tabPanel('Regression', verbatimTextOutput('regSum'), plotOutput('regression')),
    'Download Report',
    tabPanel('Meta-analysis Report in PDF', downloadButton("report", "download PDF report"))
  )
}

task_server <- function(input,output){
  data <- reactive({ 
    req(input$'file') 
    print(input)
    inFile <- input$'file'
    
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

ui <- 
dashboardPage(
  dashboardHeader(title = "Xmeta Online Analysis"
                  ),
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Tools", tabName = "demo",icon = icon("list-alt")),
      menuItem("Introduction",tabName = 'intro',icon = icon("home")),
      menuItem('dashboard',icon=icon("dashboard"),startExpanded = FALSE,
               menuSubItem("Univariate MA dashboard", tabName = "Uni_dashboard", icon = icon("list-alt")),
               menuSubItem("Network MA dashborad", tabName = "Net_dashboard", icon = icon("list-alt"))
               ),
      menuItem("Information", icon=icon("info-circle"), tabName="info")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              h3(strong("This Online Analysis platform allows you to use our online meta-analysis tool to get statistical results from your data.")),
              br(),
              br(),
              h3(p("Choose, Click & Get started!"))
        
      ),
      
      # Second tab content
      tabItem(tabName = "Uni_dashboard",
              tabBox(
                side = "left", height = "600px",width = "1000px",
                selected = "Task 1",
                tabPanel("Task 1",task_ui(1))
                # tabPanel("Task 2",task_ui(2))
              )
      ),
      # third tab content
      tabItem(tabName = "Net_dashboard",
              fluidRow(
                column(width = 5,
                       fileInput("in_file_2", "Input file:",
                                 accept=c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
                       
                       ),
                column(width = 5,
                       actionButton("umeta","See details",onclick ="window.open('http://54.205.93.148:3838/example1/')")
                      )
              )
              )
      )
    )
  )
UI <- bootstrapPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "md5.js"),
      tags$script(type="text/javascript", src = "passwdInputBinding.js")
    )
  ),
  
  ## Login module;
  div(class = "login",
      uiOutput("uiLogin"),
      textOutput("pass")
  ),htmlOutput("page")
  
  
)



# server1 <- function(input, output) {
#   Logged = FALSE;
#   
#   PASSWORD <- data.frame(Brukernavn = "withr", Passord = "e10adc3949ba59abbe56e057f20f883e")
#   source("C://Users//38084//Desktop//Xmeta//www//Login.R",  local = TRUE)
#   
#   observe({
#     if (USER$Logged == TRUE) {
#       output$page = ui
#       task_server(input,output)
#     }
#   })
#   
# 
# 
# }

shinyApp(UI, server1)
