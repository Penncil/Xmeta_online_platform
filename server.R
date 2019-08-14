library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(metafor)
library(metasens)
library(datasets)
library(shiny)
library(netmeta)
library(knitr)
library(datasets)
library(DT)
library(RMySQL)


## univariate meta-analysis
# continuous outcomes
source('unimod.R')
# binary outcomes
source('unimod_bi.R')

## multivariate meta-analysis
# continuous outcomes
source('mulmod.R')
# binary outcomes
source('mulmod_bi.R')

## network meta-analysis
# continuous outcomes
source('netmod.R')
# binary outcomes
source('netmod_bi.R')

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  
  print(all_cons)
  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  print(paste(length(all_cons), " connections killed."))
  
}






# passwdInput <- function(inputId, label) {
#   tagList(
#     tags$label(label),
#     tags$input(id = inputId, type="password", value="")
#   )
# }

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


server <- function(input, output, session) {
  Logged = FALSE;
  sessionid <- "OQGYIrpOvV3KnOpBSPgOhqGxz2dE5A9IpKhP6Dy2kd7xIQhLjwYzskn9mIhRAVHo" ;
  PASSWORD <- data.frame(Brukernavn = "withr", Passord = "e10adc3949ba59abbe56e057f20f883e")
  source("www/Login.R",  local = TRUE)
  #output$Header <- tagList(ui_DashHeader)
  
  status <- reactiveVal(value = NULL)
  # check if a cookie is present and matching our super random sessionid
  hideTab(inputId = "Loginout",target ="Sign Up")

  
  # output$gt =NULL;
  # output$gt = renderUI(tabPanel("Task 6"))
  observe({
    if (USER$Logged == FALSE) {
      
      hideTab(inputId = "unicon",target ="Task 2")
      hideTab(inputId = "unicon",target ="Task 3")
      hideTab(inputId = "unicon",target ="Task 4")
      hideTab(inputId = "unicon",target ="Task 5")
      
      hideTab(inputId = "unibinary",target ="Task 2")
      hideTab(inputId = "unibinary",target ="Task 3")
      hideTab(inputId = "unibinary",target ="Task 4")
      hideTab(inputId = "unibinary",target ="Task 5")
      
      hideTab(inputId = "mulcon",target ="Task 2")
      hideTab(inputId = "mulcon",target ="Task 3")
      hideTab(inputId = "mulcon",target ="Task 4")
      hideTab(inputId = "mulcon",target ="Task 5")
      
      hideTab(inputId = "mulbinary",target ="Task 2")
      hideTab(inputId = "mulbinary",target ="Task 3")
      hideTab(inputId = "mulbinary",target ="Task 4")
      hideTab(inputId = "mulbinary",target ="Task 5")
      
      hideTab(inputId = "netcon",target ="Task 2")
      hideTab(inputId = "netcon",target ="Task 3")
      hideTab(inputId = "netcon",target ="Task 4")
      hideTab(inputId = "netcon",target ="Task 5")
      
      hideTab(inputId = "netbinary",target ="Task 2")
      hideTab(inputId = "netbinary",target ="Task 3")
      hideTab(inputId = "netbinary",target ="Task 4")
      hideTab(inputId = "netbinary",target ="Task 5")
      
      showTab(inputId = "Loginout",target ="Log in")

      hideTab(inputId = "Loginout",target ="Log Out")
      
      callModule(uniMod, 'first')
      
      # binary
      callModule(uniModBi, 'first_bi')
      
      ###############################################
      #################### MMA ######################
      ###############################################
      # continuous
      callModule(mulMod, 'mul_first')
      
      # binary
      callModule(mulModBi, 'mul_first_bi')      
            
      ###############################################
      #################### NMA ######################
      ###############################################
      # continuous
      callModule(netMod, 'n_first')
      
      # binary
      callModule(netModBi, 'n_first_bi')
    }
    if (USER$Logged == TRUE) {
      
      showTab(inputId = "unicon",target ="Task 2")
      showTab(inputId = "unicon",target ="Task 3")
      showTab(inputId = "unicon",target ="Task 4")
      showTab(inputId = "unicon",target ="Task 5")
      
      showTab(inputId = "unibinary",target ="Task 2")
      showTab(inputId = "unibinary",target ="Task 3")
      showTab(inputId = "unibinary",target ="Task 4")
      showTab(inputId = "unibinary",target ="Task 5")
      
      showTab(inputId = "mulcon",target ="Task 2")
      showTab(inputId = "mulcon",target ="Task 3")
      showTab(inputId = "mulcon",target ="Task 4")
      showTab(inputId = "mulcon",target ="Task 5")
      
      showTab(inputId = "mulbinary",target ="Task 2")
      showTab(inputId = "mulbinary",target ="Task 3")
      showTab(inputId = "mulbinary",target ="Task 4")
      showTab(inputId = "mulbinary",target ="Task 5")
      
      showTab(inputId = "netcon",target ="Task 2")
      showTab(inputId = "netcon",target ="Task 3")
      showTab(inputId = "netcon",target ="Task 4")
      showTab(inputId = "netcon",target ="Task 5")
      
      showTab(inputId = "netbinary",target ="Task 2")
      showTab(inputId = "netbinary",target ="Task 3")
      showTab(inputId = "netbinary",target ="Task 4")
      showTab(inputId = "netbinary",target ="Task 5")
      
      showTab(inputId = "Loginout",target ="Log Out")
      hideTab(inputId = "Loginout",target ="Log in")

      
      callModule(uniMod, 'first')
      callModule(uniMod, 'second')
      callModule(uniMod, 'third')
      callModule(uniMod, 'fourth')
      callModule(uniMod, 'fifth')
      # binary
      callModule(uniModBi, 'first_bi')
      callModule(uniModBi, 'second_bi')
      callModule(uniModBi, 'third_bi')
      callModule(uniModBi, 'fourth_bi')
      callModule(uniModBi, 'fifth_bi')

      ###############################################
      #################### MMA ######################
      ###############################################
      # continuous
      callModule(mulMod, 'mul_first')
      callModule(mulMod, 'mul_second')
      callModule(mulMod, 'mul_third')
      callModule(mulMod, 'mul_fourth')
      callModule(mulMod, 'mul_fifth')
      # binary
      callModule(mulModBi, 'mul_first_bi')
      callModule(mulModBi, 'mul_second_bi')
      callModule(mulModBi, 'mul_third_bi')
      callModule(mulModBi, 'mul_fourth_bi')
      callModule(mulModBi, 'mul_fifth_bi')


      ###############################################
      #################### NMA ######################
      ###############################################
      # continuous
      callModule(netMod, 'n_first')
      callModule(netMod, 'n_second')
      callModule(netMod, 'n_third')
      callModule(netMod, 'n_fourth')
      callModule(netMod, 'n_fifth')
      # binary
      callModule(netModBi, 'n_first_bi')
      callModule(netModBi, 'n_second_bi')
      callModule(netModBi, 'n_third_bi')
      callModule(netModBi, 'n_fourth_bi')
      callModule(netModBi, 'n_fifth_bi')
    }
    

  })
  
  observeEvent(input$Logout, {
    status('out')
    js$rmcookie()
    USER$Logged =FALSE
    print("must be first")
    output$uiLogin <- renderUI({
      if (USER$Logged == FALSE) {
        wellPanel(
          textInput("userName", "User Name:"),
          passwdInput("passwd", "Pass word:"),
          br(),
          actionButton("Login", "Log in"),
          h2("Don't have a acconut? Join in!"),
          actionButton("Signup", "Sign up")


        )
      }
    })
    
  })
  observe({
    js$getcookie()
    print(paste0(input$jscookie," get"))
    if (!is.null(input$jscookie) && input$jscookie == sessionid) {
      status(paste0('in with sessionid ', input$jscookie))
      if(isolate(USER$Logged == FALSE)){
        print("error1")
        isolate(USER$Logged == TRUE)
      }
    }
    else {
      status('out')
      js$rmcookie()
      USER$Logged =FALSE
      output$uiLogin <- renderUI({
        if (USER$Logged == FALSE) {
          wellPanel(
            textInput("userName", "User Name:"),
            passwdInput("passwd", "Pass word:"),
            br(),
            actionButton("Login", "Log in"),
            h2("Don't have a acconut? Join in!"),
            actionButton("Signup", "Sign up")


          )
        }
      })
    }
    
    
  })
  
  output$SucLogin <- renderUI({
    })
  
  
  
 
  
  observeEvent(input$Signup, {
    output$uiLogin <- renderUI({
      if (USER$Logged == FALSE) {
        # wellPanel(
        #   h2("Create your own profile"),
        #   textInput("userName1", "User Name:"),
        #   passwdInput("passwd1", "Pass word:"),
        #   br(),
        #   actionButton("Signupcommit", "Sign up"),
        #   actionButton("BacktoLogin","Back to Log in Page"),
        #   verbatimTextOutput('Status_Sign')
        #
        # )

      }
    })
    showTab(inputId = "Loginout",target ="Sign Up")
    hideTab(inputId = "Loginout",target ="Log in")
  })
  observeEvent(input$BacktoLogin, {
    output$uiLogin <- renderUI({
      if (USER$Logged == FALSE) {
        # wellPanel(
        #   textInput("userName", "User Name:"),
        #   passwdInput("passwd", "Pass word:"),
        #   br(),
        #   actionButton("Login", "Log in"),
        #   h2("Don't have a acconut? Join in!"),
        #   actionButton("Signup", "Sign up")
        #
        # )

      }
    })
    showTab(inputId = "Loginout",target ="Log in",select = TRUE)
    hideTab(inputId = "Loginout",target ="Sign Up")


  })
  observeEvent(input$Signupcommit, {
    con <- dbConnect(MySQL(),host="127.0.0.1",port=3307,dbname="test_xmeta",user="root",password="9611dgp")
    Signup_Uname = input$username1
    Signup_Pwd = input$passwd1
    sql_Sel <- paste0("select idUser_Test from user_test where idUser_Test = '",Sigup_Uname,"'")
    SignQ <- dbGetQuery(con,sql_Sel)
    if(nrow(SignQ)>0){
      output$Status_Sign = renderText({
        "The Username has been used! Try again!"
      })
    }
    else if(nrow(SignQ)==0){
      sql_insert <- paste0("insert into  user_test values ('",Signup_Uname,"',md5('",Signup_Pwd,"'),md5('",Signup_Uname,"'))")
      output$uiLogin <- renderUI({
        if (USER$Logged == FALSE) {
          wellPanel(
            textInput("userName", "User Name:"),
            passwdInput("passwd", "Pass word:"),
            br(),
            actionButton("Login", "Log in"),
            h2("Don't have a acconut? Join in!"),
            actionButton("Signup", "Sign up")


          )
        }
      })
      showTab(inputId = "Loginout",target ="Log in",select = TRUE)
      hideTab(inputId = "Loginout",target ="Sign Up")

    }
    dbDisconnect(con)

  })
  
  output$inpass <- renderText({
    paste0("You are Logged ",status())
  })
  
  #===========================================================
  ###############################
  #### jessie changed here, 1229#
  ################################
  login_modal <- modalDialog(
    title = h3("Login"), size = "s", easyClose = T, footer = NULL,
    textInput("login_username", label = "Username"),
    passwordInput("login_password", label = "Password"),
    textOutput("login_error"), 
    br(),
    fluidRow(
      actionButton("login_submit", label = "Submit"), 
      align = "center"
    )
  )
  
  sign_up_modal <- modalDialog(
    title = h3("Sign Up"), size = "s", easyClose = T, footer = NULL,
    textInput("sign_up_username", label = "Username"),
    passwordInput("sign_up_password", label = "Password"),
    passwordInput("sign_up_verify", label = "Verify Password"),
    textOutput("sign_up_error"),
    br(),
    fluidRow(
      actionButton("sign_up_submit", label = "Submit"),
      align = "center"
    )
  )
  
  remove_account_modal <- modalDialog(
    title = h3("Remove Account"), size = "s", easyClose = T, footer = NULL,
    "Are you sure?",
    br(),
    "Click anywhere to cancel",
    br(), br(),
    fluidRow(
      actionButton("remove_account_confirm", label = "Confirm"),
      align = "center"
    )
  )
  # login -------------------------------------------------------------------
  observeEvent(input$login, {
    output$login_error <- renderText(NULL)
    showModal(login_modal)
  })
  
  observeEvent(input$login_submit, {
    req(input$login_username != "")
    req(input$login_password != "")
    
    validation <- withProgress(
      value = 0.5,
      message = "Please wait",
      expr = validate_login(
        username = input$login_username, 
        password = input$login_password
      )
    )
    
    if (validation$result == F) {
      output$login_error <- renderText("Wrong username and/or password")
      
    } else {
      active_user(
        list(
          username = input$login_username,
          last_login = validation$last_login
        )
      )
      removeModal()
    }
  })
  
  # sign up -----------------------------------------------------------------
  observeEvent(input$signup, {
    output$sign_up_error <- renderText(NULL)
    showModal(sign_up_modal)
  })
  
  observeEvent(input$sign_up_submit, {
    req(input$sign_up_username != "")
    req(input$sign_up_password != "")
    req(input$sign_up_verify != "")
    
    validation <- validate_sign_up(
      username = input$sign_up_username,
      password = input$sign_up_password,
      verify = input$sign_up_verify
    )
    
    if (validation$result == F) {
      output$sign_up_error <- renderText(validation$message)
      
    } else {
      active_user(
        list(
          username = input$sign_up_username, 
          last_login = validation$last_login
        )
      )
      removeModal()
    }
  })
  
}

shinyServer(server)