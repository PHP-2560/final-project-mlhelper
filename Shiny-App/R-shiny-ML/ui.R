library(shiny)
library(htmltools)
shinyUI(
  pageWithSidebar(
    
    
    # Header:
    headerPanel("Machine Learning Helper"),
    
    
    # Input in sidepanel:
    sidebarPanel(
    ## useless  
      tags$head(
        tags$style(type="text/css", "select { max-width: 200px; }"),
        tags$style(type="text/css", ".span4 { max-width:200px; }"),
        tags$style(type="text/css", ".well { max-width: 300px; }")
      ),
      
      # Upload data:
      fileInput("file", "Upload csv data-file:"),
      
      #Using action buttoms to load sample dataset
      #change the color of the buttom to contrast with previous blank
      actionButton("myLoader", "Load test dataset",  
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      #add block between each part
      hr(),
      
      # Variable selection:
      #Independent Numeric variable selection:
      #htmlOutput("varselect_num"),
      
      #Independent Categorical variable selection:
      #htmlOutput("varselect_cat"),
      
      #Dependent variable selection:
      
      htmlOutput("response"),
      
      #Because next part is the download file part, so we add a line to block between variable selection and 
      #file download
      
      hr(),
    
      width=3),
    
    # Main:
    mainPanel(
      titlePanel("Choose an Algorithmn"),
    
      tags$head(
        
        tags$style(type='text/css', 
                   ".nav-tabs {font-size: 14px} ")), 
      tabsetPanel(type = "tabs", 
                  tabPanel("LogTransformation", br(),
                           sliderInput("fp_set",label = "Adjusted Fisher-Pearson Coefficient of Skewness",0,1,0.5),
                           p(em("A number,usually between 0 and 1, it the critical value of adjusted Fisher-Pearson coefficient of skewness, when the adjusted Fisher-Pearson coefficient of skewness of the data response is larger than this value, we think the distribution of response is skewed and will do the log-transformation")),
                           br(),
                           plotOutput("LogTransformation", width = "100%", height = "580px")
                           ), 
                  tabPanel("PCA", br(),
                           sliderInput("pov_set", label = "Critical value of proportion",0,1,0.5),
                           p(em("Critical value of proportion of variance that Principal Components must have, this value will decide how many principle components we will have after PCA, its value need to be in [0,1], largert it is, more principle components we will have.")),
                           br(),
                           verbatimTextOutput("pca_matrix")), 
                  tabPanel("Ridge",  plotOutput("mse_ridge", height = "580px"),br(),
                           p("The smallest MSE that Ridge could achieve was: "),
                           verbatimTextOutput("ridge_mse_num")),
                  tabPanel("LASSO",  plotOutput("mse_lasso", height = "580px"),br(),
                           p("The smallest MSE that LASSO could achieve was: "),
                           verbatimTextOutput("lasso_mse_num")),
                  tabPanel("Elastic Net",  plotOutput("mse_elnet", height = "580px"),br(),
                           p("The smallest MSE that Elastic Net could achieve was: "),
                           verbatimTextOutput("elnet_mse_num")),
                  tabPanel("Bagging",  plotOutput("bag_mse", height = "580px"),br(),
                           p("The smallest MSE that Bagging could achieve was: "),
                           verbatimTextOutput("bag_mse_num")),
                  tabPanel("Random Forest",  plotOutput("rf_mse", height = "580px"),br(),
                           p("The smallest MSE that Random Forest could achieve was: "),
                           verbatimTextOutput("rf_mse_num")),
                  tabPanel("Help", htmlOutput("inc"))
      )
    )
  ))

