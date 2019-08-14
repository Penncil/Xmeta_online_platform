library(shiny)
library(DT)
library(metasens)
library(metafor)
library(netmeta)
library(knitr)
library(datasets)

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

Logged = FALSE;

PASSWORD <- data.frame(Brukernavn = "withr", Passord = "e10adc3949ba59abbe56e057f20f883e")
server <- shinyServer(function(input, output, session) {
  
  
  source("www/Login.R",  local = TRUE)
  
  observe({
    if (USER$Logged == TRUE) {
      # source("tour.R", local=TRUE)
      print
      ###############################################
      #################### UMA ######################
      ###############################################
      # continuous
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
  
  
  

  
})



