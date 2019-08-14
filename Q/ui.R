library(shinydashboard)
library(shinyBS)
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

bootstrapPage(
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
  )
  
)

faq <- source("faq.R", local=TRUE)[[1]]

dashboardPage(
  dashboardHeader(title = "XMETA Online Analysis",
                  tags$li(class="dropdown",
                          tags$a(href="https://www.xmeta.wiki/", target="_blank",
                                 "X-META HOMEPAGE",align = "right", style="padding: 15px; margin: 0px;")
                  )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction",tabName = 'intro',icon = icon("home")),
      menuItem("Tutorials", tabName = 'tutorials',icon = icon("graduation-cap")),
      menuItem('dashboard',icon=icon("dashboard"),
               menuItem("Univariate MA dashboard", tabName = "Uni_dashboard", icon = icon("list-alt"),
                        menuSubItem("Continuous outcomes", tabName = "uni_continuous"),
                        menuSubItem("Binary outcomes",tabName = "uni_binary"),
                        menuSubItem('Survival outcomes', tabName = 'uni_sur')),
               menuItem("Multivariate MA dashborad", tabName = "Mul_dashboard", icon = icon("list-alt"),
                        menuSubItem("Continuous outcomes", tabName = "mul_continuous"),
                        menuSubItem("Binary outcomes",tabName = "mul_binary"),
                        menuSubItem('Survival outcomes', tabName = 'mul_sur')),
               menuItem("Network MA dashborad", tabName = "Net_dashboard", icon = icon("list-alt"),
                        menuSubItem("Continuous outcomes", tabName = "net_continuous"),
                        menuSubItem("Binary outcomes",tabName = "net_binary"),
                        menuSubItem('Survival outcomes', tabName = 'net_sur'))
      ),
      menuItem("Information", icon=icon("info-circle"), tabName="info")
      # actionButton("help", "Take tour", style="margin: 10px 15px 10px 15px; width: 200px",
      #              class="btn-flat action-button btn-block", icon=icon("question-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "intro",
              h2(strong("Welcome to X-META Online Analysis Platform")),
              br(),
              br(),
              h4(p("This platform is an interactive toolbox for meta-analysis. 
                    It allows you to use meta-analysis methods written in R programming 
                   language to get statistical results from your data. Click on either of the tabs on the side bar to access the corresponding Online Analysis platform or
                   keep scrolling to learn more about how to use the tool and get started."),style="text-align: justify;"),
              br(),
              h3("Here is the pipeline diagram for the platform:"),
              HTML('
                   <div style="clear: left;"><img src="http://oi63.tinypic.com/w2dijo.jpg"
                   width="80%" height="80%" /></div>'
              )
      ),
      
      # Second tab content
      tabItem(tabName = "tutorials",
              h3(strong("How to use our ONLINE ANALYSIS platform?")),
              br(),
              h4(strong("I: Using Our Sample Data")),
              h4("1. Before uploading your own data, you can download our sample datasets to explore the functions of our platform."),
              br(),
              h4("UMA:"),
              h4("Univariate Meta-analysis with Continuous Outcomes: ",a("sample dataset", 
                                                href="https://drive.google.com/open?id=1zIOVrWe9fD-QCsLGA2bF025AHkjxxflT",
                                                target="_blank")),
              h4("Univariate Meta-analysis with Binary Outcomes: ",a("sample dataset", 
                                                href="https://drive.google.com/open?id=1VJyyTfozRs_kfLqT_CXueV-hWd6EQ2A0",
                                                target="_blank")),
              h4("Univariate Meta-analysis with Survival Outcomes: ",a("coming soon")),
              
              br(),
              h4("MMA:"),
              h4("Multivariate Meta-analysis with Continuous Outcomes: ",a("coming soon")),
              h4("Multivariate Meta-analysis with Binary Outcomes: ",a("coming soon")),
              h4("Multivariate Meta-analysis with Survival Outcomes: ",a("coming soon")),
              
              br(),
              h4("NMA:"),
              h4("Network Meta-analysis with Continuous Outcomes: ",a("sample dataset",
                                             href="https://drive.google.com/file/d/1FdFbjO5VZWevICLpKWhPLWe0N6VvOT09/view",
                                             target="_blank")),
              h4("Network Meta-analysis with Binary Outcomes: ",a("coming soon")),
              h4("Network Meta-analysis with Survival Outcomes: ",a("coming soon")),
              
              br(),
              h4("2. Once you have the CSV file on your laptop, choose the corresponding method of meta-analysis you would like to conduct.
                 In this example, we selected univariate meta-analysis and reached the following website:"),
              HTML('
                   <div style="clear: left;"><img src="https://static.wixstatic.com/media/81a271_fba0e970fc82444aa53b8acfb93c8ead~mv2_d_2582_1468_s_2.png/v1/fill/w_1494,h_852,al_c,usm_0.66_1.00_0.01/81a271_fba0e970fc82444aa53b8acfb93c8ead~mv2_d_2582_1468_s_2.png"
                   width="70%" height="70%" /></div>'
              ),
              h4("3. Click the 'Browse' button and keep 'Header' checked. Upload 'dataset_umeta.csv'.
                 Once the file is uploaded, the system will automatically display the following screen:"),
              HTML('
                   <div style="clear: left;"><img src="https://static.wixstatic.com/media/81a271_e2d4938bca5140d595930d4e46448e30~mv2.png/v1/fill/w_1494,h_378,al_c,usm_0.66_1.00_0.01/81a271_e2d4938bca5140d595930d4e46448e30~mv2.png"
                   width="70%" height="70%" /></div>'
              ),
              h4("4. On the left side bar, you will see the outputs of the Online Analysis, 
                 including various plots and other results. You can also download the analysis report in PDF."),
              HTML('
                   <div style="clear: left;"><img src="http://oi68.tinypic.com/23jqw0i.jpg"
                   width="30%" height="30%" /></div>'
              ),
              h4("5. On the panel, you can choose to download the analysis report in PDF."),
              HTML('
                   <div style="clear: left;"><img src="http://oi64.tinypic.com/wbe13d.jpg"
                   width="30%" height="30%" /></div>'
              ),
              br(),
              h4(strong("II: Get Started: Using Your Own Data")),
              h4("1. Our online platform requires a fixed format of Excel files. Results cannot be presented if a file with different formatting is uploaded.
                 Please download the following templates and fill them in your own data:"),
              br(),
              h4("UMA:"),
              h4("Univariate Meta-analysis with Continuous Outcomes: ",a("sample template", 
                                                href="https://drive.google.com/file/d/19QGLbmTPeQatOqNVi7-xcr6x6eGmh7s6/view",
                                                target="_blank")),
              h4("Univariate Meta-analysis with Binary Outcomes: ",a("coming soon")),
              h4("Univariate Meta-analysis with Survival Outcomes: ",a("coming soon")),
              
              br(),
              h4("MMA:"),
              h4("Multivariate Meta-analysis with Continuous Outcomes: ",a("coming soon")),
              h4("Multivariate Meta-analysis with Binary Outcomes: ",a("coming soon")),
              h4("Multivariate Meta-analysis with Survival Outcomes: ",a("coming soon")),
              
              br(),
              h4("NMA:"),
              h4("Network Meta-analysis with Continuous Outcomes: ",a("sample template", 
                                             href="https://drive.google.com/file/d/1SHI7ZHfm47QmAcnovuhCfyin6NMaK9L-/view",
                                             target="_blank")),
              h4("Network Meta-analysis with Binary Outcomes: ",a("coming soon")),
              h4("Network Meta-analysis with Survival Outcomes: ",a("coming soon")),
              
              br(),
              h4("2. Repeat step 2,3,4 from the Tutorial above.")
              
              ),
      
      ###############################################
      #################### UMA ######################
      ###############################################
      # continuous outcomes
      tabItem(tabName = "uni_continuous",
              tabBox(
                side = "left", height = "600px",width = "1000px",
                selected = "Task 1",
                tabPanel("Task 1", uniModUI("first")),
                tabPanel("Task 2", uniModUI("second")),
                tabPanel("Task 3", uniModUI("third")),
                tabPanel("Task 4", uniModUI("fourth")),
                tabPanel("Task 5", uniModUI("fifth"))
              )
      ),
      
      # binary outcomes
      tabItem(tabName = "uni_binary",
              tabBox(
                side = "left", height = "600px",width = "1000px",
                selected = "Task 1",
                tabPanel("Task 1", uniModBiUI("first_bi")),
                tabPanel("Task 2", uniModBiUI("second_bi")),
                tabPanel("Task 3", uniModBiUI("third_bi")),
                tabPanel("Task 4", uniModBiUI("fourth_bi")),
                tabPanel("Task 5", uniModBiUI("fifth_bi"))
              )
      ),
      
      # survival outcomes
      tabItem(tabName = "uni_sur",
              h2("Coming soon!")
      ),
      
      ###############################################
      #################### MMA ######################
      ###############################################
      # continuous outcomes
      tabItem(tabName = "mul_continuous",
              tabBox(
                side = "left", height = "600px",width = "1000px",
                selected = "Task 1",
                tabPanel("Task 1", mulModUI("mul_first")),
                tabPanel("Task 2", mulModUI("mul_second")),
                tabPanel("Task 3", mulModUI("mul_third")),
                tabPanel("Task 4", mulModUI("mul_fourth")),
                tabPanel("Task 5", mulModUI("mul_fifth"))
              )
      ),
      
      # binary outcomes
      tabItem(tabName = "mul_binary",
              tabBox(
                side = "left", height = "600px",width = "1000px",
                selected = "Task 1",
                tabPanel("Task 1", mulModBiUI("mul_first_bi")),
                tabPanel("Task 2", mulModBiUI("mul_second_bi")),
                tabPanel("Task 3", mulModBiUI("mul_third_bi")),
                tabPanel("Task 4", mulModBiUI("mul_fourth_bi")),
                tabPanel("Task 5", mulModBiUI("mul_fifth_bi"))
              )
      ),
      
      # survival outcomes
      tabItem(tabName = "mul_sur",
                h2("Coming soon!")
      ),
      
      ###############################################
      #################### NMA ######################
      ###############################################
      # continuous outcomes
      tabItem(tabName = "net_continuous",
              tabBox(
                side = "left", height = "600px",width = "1000px",
                selected = "Task 1",
                tabPanel("Task 1", netModUI("n_first")),
                tabPanel("Task 2", netModUI("n_second")),
                tabPanel("Task 3", netModUI("n_third")),
                tabPanel("Task 4", netModUI("n_fourth")),
                tabPanel("Task 5", netModUI("n_fifth"))
              )
      ),
      
      # binary outcomes
      tabItem(tabName = "net_binary",
              tabBox(
                side = "left", height = "600px",width = "1000px",
                selected = "Task 1",
                tabPanel("Task 1", netModBiUI("n_first_bi")),
                tabPanel("Task 2", netModBiUI("n_second_bi")),
                tabPanel("Task 3", netModBiUI("n_third_bi")),
                tabPanel("Task 4", netModBiUI("n_fourth_bi")),
                tabPanel("Task 5", netModBiUI("n_fifth_bi"))
              )
      ),
      
      # survival outcomes
      tabItem(tabName = "net_sur",
                h2("Coming soon!")
      ),
      
      # Fourth tab content
      tabItem(tabName = "info",
              h2("About this application"),
              p("This online analysis platform works with different available methods and a variety of formats of data, 
                enabling users to quickly obtain the meta-analysis results without writing any code.", style="text-align:justify"),
              h2("Frequently asked questions"),
              faq,
              h2("Contact information"),
              HTML('
                   <div style="clear: left;"><img src="http://oi66.tinypic.com/2m3nrpc.jpg"
                   alt="" style="float: left; margin-right:5px" width="12%" height="12%" /></div>
                   <p>Yong Chen<br/>
                   Assistant Professor<br/>
                  Department of Biostatistics<br/>
                  The Perelman School of Medicine, University of Pennsylvania<br/>
                   <a href="https://dbe.med.upenn.edu/biostat-research/YongChen" target="_blank">Website.io</a>
                   </p>'
                  ),
              p("For questions about this application, please email ychen123@mail.med.upenn.edu")
              )
    )
  )
)


