library(shiny)
library(MASS)
library(car)
library(ggplot2)
library(glmnet)
library(dplyr)
library(randomForest)
source("LogTrans.R")
source("pca_dim_redc.R")
source("data_clean.R")

shinyServer(function(input, output) {
  ##Argument names:
  #Using the header names from the data 
  ArgNames <- reactive({
    Names <- names(formals("read.csv")[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  
  ### Data import:
  Dataset <- reactive({
    
    # User has not uploaded a file yet
    if (is.null(input$file) && input$myLoader==0) {
      return(data.frame())
    }
    
    #loading test dataset
    if (input$myLoader && is.null(input$file)){
      # the test data set is a dataframe called "Boston" in "MASS" package, all variables are numeric
      return(Boston)
    }
    
    #loading csv. when data has been uploaded
    
    args <- grep(paste0("^","read.csv","__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^","read.csv","__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.frame(do.call("read.csv",c(list(input$file$datapath),argList)))
    ## clean the data set. 
    #data_clean will remove non-numeric variables, remove outliers, remove observations with NA and Inf value.
    # see more details of data_clean() function, look at our package "MLhelper"
    Dataset<-data_clean(Dataset)
    return(Dataset)
  })
  
  # Select response:
  output$response <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    selectInput("response","Select Response:",
                names(Dataset()), names(Dataset()))
  })
  ## Building the Ridge Model
  model.ridge<-reactive(
    {
      if(is.null(input$response)) return(NULL)
      # make the resposnse and explantory variables as matrix
      y=as.matrix(Dataset()[,which(colnames(Dataset())==input$response)])
      x=as.matrix(Dataset()[,-which(colnames(Dataset())==input$response)])
      lambdas=10^seq(3,-1,by=-0.1)
      ridge.fit=cv.glmnet(x,y,alpha = 0, lambda=lambdas)
      return(ridge.fit)
    }
  )
  
  
  ## Building the LASSO Model
  model.lasso<-reactive(
    {
      if(is.null(input$response)) return(NULL)
      y=as.matrix(Dataset()[,which(colnames(Dataset())==input$response)])
      x=as.matrix(Dataset()[,-which(colnames(Dataset())==input$response)])
      lambdas=10^seq(3,-1,by=-0.1)
      lasso.fit=cv.glmnet(x,y,alpha = 1, lambda=lambdas)
      return(lasso.fit)
    }
  )
  
  ## Building the Elastic Net Model
  model.elnet<-reactive(
    {
      if(is.null(input$response)) return(NULL)
      y=as.matrix(Dataset()[,which(colnames(Dataset())==input$response)])
      x=as.matrix(Dataset()[,-which(colnames(Dataset())==input$response)])
      lambdas=10^seq(3,-1,by=-0.1)
      elnet.fit=cv.glmnet(x,y,alpha = 0.5, lambda=lambdas)
      return(elnet.fit)
    }
  )
  
  ##Building the Bagging Model
  model.bag<-reactive(
    {
      y=Dataset()[ ,which(colnames(Dataset())==input$response)]
      x=Dataset()[ ,-which(colnames(Dataset())==input$response)]
      bag.fit=randomForest(x,y, mtry=ncol(Dataset()),importance =TRUE)
      return(bag.fit)
    }
  )
  
  ##Building the Random Forest Model
  model.rf<-reactive(
    {
      y=Dataset()[ ,which(colnames(Dataset())==input$response)]
      x=Dataset()[ ,-which(colnames(Dataset())==input$response)]
      rf.fit=randomForest(x,y, mtry=ncol(Dataset())%/%3,importance =TRUE)
      return(rf.fit)
    }
  )

  ## LogTransformation
  output$LogTransformation <- renderPlot({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    par(mfrow=c(2,1))
    hist(Dataset()[,which(names(Dataset())==input$response)],breaks = 10,main="Response Dist of Original Data",xlab=input$response)
    #LogTrans is a function of package we built called "MLhelper". It does the Log transformation of resposne variable.
    #see more detailed information of LogTrans, look at "MLhelper" packages.
    LogData=LogTrans(Dataset(),fp=as.numeric(input$fp_set),res_name=input$response)
    hist(LogData[,which(names(LogData)==input$response)],breaks = 10,main="Response Dist of LogTransformed Data",xlab=input$response)
  })
  
  
  ## PCA: Principal Component Analysis
  output$pca_matrix <- renderPrint({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    #summary(model())
    #LogTrans is a function of package we built called "MLhelper". It does Principal component analysis of dataset
    #see more detailed information of pca_dim_redc, look at "MLhelper" packages.
    pca_dim_redc(Dataset(),pov_critical = as.numeric(input$pov_set),res_name = input$response)
  })
  
  
  #Ridge
  
  ## mse_ridge is a ridge regression model
  output$mse_ridge <- renderPlot({
    if (is.null(model.ridge())) return(NULL)
    par(mfrow=c(1,1))
    plot(model.ridge())
  })  
  
  ## ridge_mse_num gives the smallest mse that ridge regression could reach
  output$ridge_mse_num=renderPrint(
    {
      min(model.ridge()$cvm)
      
    }
  )
  
  
  ##LASSO 
  ## mse_lasso is a LASSO regression model
  output$mse_lasso <- renderPlot({
    if (is.null(model.lasso())) return(NULL)
    par(mfrow=c(1,1))
    plot(model.lasso())
  })
  
  # lasso_mse_num is the possible smallest mse that the model "mse_lasso" could reach.
  output$lasso_mse_num=renderPrint(
    {
      min(model.lasso()$cvm)
    }
  )
  
  ##Elastic Net
  # mse_elnet is Elastic net regresson model
  output$mse_elnet <- renderPlot({
    if (is.null(model.elnet())) return(NULL)
    par(mfrow=c(1,1))
    plot(model.elnet())
  })
  
  # elnet_mse_num is the possible smallest mse that the model "mse_elnet" model could reach.
  output$elnet_mse_num=renderPrint(
    {
      min(model.elnet()$cvm)
    }
  )
  
  ##Bagging
  # bag_mse is a Bagging regression model
  set.seed(1)
  output$bag_mse<-renderPlot(
    {
      if (is.null(model.bag())) return(NULL)
      plot(model.bag())
    }
  )
  
  # bag_mse_num is the possible smallest mse that the model "mse_elnet" model could reach.
  output$bag_mse_num=renderPrint(
    {
      min(model.bag()$mse)
      
    }
  )
  
  ##Random Forest 
  #rf_mse is a  Random Forest regression model
  output$rf_mse<-renderPlot(
    {
      if (is.null(model.rf())) return(NULL)
      plot(model.rf())
    }
  )
  
  #rf_mse_num is the smallest mse that rf_mse model can reach 
  output$rf_mse_num=renderPrint(
    {
      min(model.rf()$mse)
      
    }
  )

  
  ## Linking to Help documentation
  getPage<-function() {
    return(includeHTML("help_document.html"))
  }
  output$inc<-renderUI({getPage()})
})
