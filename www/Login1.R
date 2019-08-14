#### Log in module ###
USER <- reactiveValues(Logged = Logged,sessionid =sessionid)

passwdInput <- function(inputId, label) {
  tagList(
    tags$label(label),
    tags$input(id = inputId, type="password", value="")
  )
}

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

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if (input$Login > 0) {
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        print(Password)
        print(Username)
        print(PASSWORD$Passord)
        print(PASSWORD$Brukernavn)
        Id.username <- which(PASSWORD$Brukernavn == Username)
        Id.password <- which(PASSWORD$Passord    == Password)
        if (length(Id.username) > 0 & length(Id.password) > 0) {
          if (Id.username == Id.password) {
            sessionID <- isolate(USER$sessionid)
            
            js$setcookie(sessionID)
            
            
            USER$Logged <- TRUE
            
          } 
        } else  {
          "User name or password failed!"
        }
      } 
    }
  }
})
