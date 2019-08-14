#### Log in module ###
library(shinyjs)
library(RMySQL)
USER <- reactiveValues(Logged = Logged,sessionid =sessionid)


# passwdInput <- function(inputId, label) {
#   tagList(
#     tags$label(label),
#     tags$input(id = inputId, type="password", value="")
#   )
# }

# output$uiLogin <- renderUI({
#   if (USER$Logged == FALSE) {
#     wellPanel(
#       textInput("userName", "User Name:"),
#       passwdInput("passwd", "Pass word:"),
#       br(),
#       actionButton("Login", "Log in"),
#       h2("Don't have a acconut? Join in!"),
#       actionButton("Signup", "Sign up")
#       
#       
#     )
#   }
# })

observeEvent(input$Login,{
  output$pass <- renderText({
    if (USER$Logged == FALSE) {
      Username <- reactive({input$userName})
      Password <- reactive({input$passwd})

      print(Password)
      print(Username)
      print(Username())
      print(Password())
      # print(PASSWORD$Passord)
      # print(PASSWORD$Brukernavn)
      updateTextInput(session,"userName", value = "")
      updateTextInput(session,"passwd", value = "")

      # print((input$userName))
      # print((input$Login))
      # print((input$passwd))
      con_login = dbConnect(MySQL(),host="127.0.0.1",port=3307,dbname="test_xmeta",user="root",password="9611dgp")
      
      sql_Log <- paste0("select idUser_Test from user_test where idUser_Test = '",Username(),"' and Passwd ='",Password(),"'")
      LogQ <- dbGetQuery(con_login,sql_Log)
      dbDisconnect(con_login)
      # reset("userName")
      # reset("passwd")
      print(1)
      print(Username())
      print(Password())
      if(nrow(LogQ)>0){
        sessionID <- isolate(USER$sessionid)
        
        js$setcookie(sessionID)
        #js$getcookie()
        #print(input$jscookie)
        USER$Logged <- TRUE
        # Username =""
        # Password =""
      }
      else if(nrow(LogQ)==0){
        "User name or password failed!"
        
      }
      LogQ = 0 
      # Id.username <- which(PASSWORD$Brukernavn == Username)
      # Id.password <- which(PASSWORD$Passord    == Password)
      # if (length(Id.username) > 0 & length(Id.password) > 0) {
      #   if (Id.username == Id.password) {
      #     
      #     sessionID <- isolate(USER$sessionid)
      # 
      #     js$setcookie(sessionID)
      #     USER$Logged <- TRUE
      #     
      #   } 
      # } else  {
      #   "User name or password failed!"
      # }
    }
    
  })
  
  
  
})


# output$pass <- renderText({  
#   if (USER$Logged == FALSE) {
#     print(input$Login)
#     if (!is.null(input$Login)) {
#       if (input$Login > 0) {
#         Username <- isolate(input$userName)
#         Password <- isolate(input$passwd)
#         # print(Password)
#         # print(Username)
#         # print(PASSWORD$Passord)
#         # print(PASSWORD$Brukernavn)
#         # updateTextInput( session,"userName", value = "")     
#         # updateTextInput(session,"passwd", value = "") 
#         reset("userName")
#         reset("passwd")
#         print((input$userName))
#         print((input$Login))
#         print((input$passwd))
#         con_login = dbConnect(MySQL(),host="127.0.0.1",port=3307,dbname="test_xmeta",user="root",password="9611dgp")
#         
#         sql_Log <- paste0("select idUser_Test from user_test where idUser_Test = '",Username,"' and Passwd ='",Password,"'")
#         LogQ <- dbGetQuery(con_login,sql_Log)
#         dbDisconnect(con_login)
#         if(nrow(LogQ)>0){
#           sessionID <- isolate(USER$sessionid)
#           
#           js$setcookie(sessionID)
#           js$getcookie()
#           print(input$jscookie)
#           USER$Logged <- TRUE
#           Username =""
#           Password =""
#         }
#         else if(nrow(LogQ)==0){
#           "User name or password failed!"
# 
#         }
#         LogQ = 0 
#         # Id.username <- which(PASSWORD$Brukernavn == Username)
#         # Id.password <- which(PASSWORD$Passord    == Password)
#         # if (length(Id.username) > 0 & length(Id.password) > 0) {
#         #   if (Id.username == Id.password) {
#         #     
#         #     sessionID <- isolate(USER$sessionid)
#         # 
#         #     js$setcookie(sessionID)
#         #     USER$Logged <- TRUE
#         #     
#         #   } 
#         # } else  {
#         #   "User name or password failed!"
#         # }
#         
#       } 
#     }
#   }
# })
