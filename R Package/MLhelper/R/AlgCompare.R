AlgCompare=function(df)
{
  library(glmnet)
  library(randomForest)
  set.seed(1)
  ncol=dim(df)[2]
  give_response=function() ## ask user to input a varaible name as response
  {
    name=readline(prompt = "Enter the name of response: ")
    return(name)
  }
  res_name=give_response()
  vnames=colnames(df)
  while (res_name %in% vnames==F)
  {
    res_name=readline(prompt = "Invalid input, please enter the name of response: ")
  }
  response=as.matrix(df[,which(vnames==res_name)])
  predictor=NA
  for (i in 1:ncol)
  {
    if (vnames[i]!=res_name)
    {
      predictor=cbind(predictor,df[,i])
    }

  }
  # store the preditcors' names
  pnames=c()
  for (i in 1:ncol)
  {
    if (vnames[i]!=res_name)
    {
      pnames=append(pnames,vnames[i])
    }

  }
  predictor=as.matrix(predictor[,-1])
  ## then random select 4/5 of the varaibles of the data
  train.index=sample(dim(df)[1]%/%5*4)
  x.train=predictor[train.index,]
  y.train=response[train.index,]
  x.test=predictor[-train.index,]
  y.test=response[-train.index]
  ## then we use lasso
  fit.lasso=cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, family="gaussian")
  lasso.yhat=predict(fit.lasso, s=fit.lasso$lambda.1se, newx=x.test)
  lasso.mse=mean((y.test - lasso.yhat)^2)
  # then we use ridge
  fit.ridge=cv.glmnet(x.train, y.train, family="gaussian", alpha=0)
  ridge.yhat=predict(fit.ridge, s=fit.ridge$lambda.1se, newx=x.test)
  ridge.mse=mean((y.test - ridge.yhat)^2)
  # then we use Elastic Net
  fit.elnet=glmnet(x.train, y.train, family="gaussian", alpha=.5)
  elnet.yhat=predict(fit.elnet,s=fit.ridge$lambda.1se, newx=x.test)
  elnet.mse=mean((y.test - elnet.yhat)^2)
  # random forest
  pred=df[,-which(vnames==res_name)]
  res=df[,which(vnames==res_name)]
  train.index=sample(dim(df)[1]%/%5*4)
  pred.train=pred[train.index,]
  res.train=res[train.index]
  pred.test=pred[-train.index,]
  res.test=res[-train.index]
  rf.df=randomForest(x=pred.train,y=res.train, mtry=length(pnames)%/%3,importance =TRUE)
  yhat.rf=predict(rf.df,newdata=pred.test)
  rf.mse=mean((yhat.rf-y.test)^2)
  # bagging
  bag.df=randomForest(x=pred.train,y=res.train, mtry=length(pnames),importance =TRUE)
  yhat.bag=predict(bag.df,newdata=pred.test)
  bag.mse=mean((yhat.bag-y.test)^2)
  mse=c(ridge.mse,lasso.mse,elnet.mse,rf.mse,bag.mse)
  mse=as.data.frame(mse)
  algos=c("Ridge","LASSO","Elnet","Random Forest","Bagging")
  rownames(mse)=algos
  return(mse)
}


