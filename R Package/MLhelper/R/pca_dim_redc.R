pca_dim_redc=function(df,pov_critical){
  ##Prompt user to input the name of the response
  give_response=function(){
    name=readline(prompt = "Enter the name of response: ")
    return(name)
  }
  ##Get the response and predictors in the data frame
  res_name=give_response()
  cols=colnames(df)
  response=df[,which(cols==res_name)]
  predictor=df[,which(cols!=res_name)]

  ##Use the Principal Components Analysis.
  pca=prcomp(predictor,scale=TRUE)

  ##Calculate the Proportion of variance of PCn.
  pov=pca$sdev^2/sum(pca$sdev^2)

  ##Calculate the Accumulative Proportion of variance of PCn.
  acc_pov=rep(NA,length(pov))
  for (i in 1:length(pov)){
    acc_pov[i]=sum(pov[1:i])
  }

  ##Get the number of Principal Components that contain at least user-defined proportion of variance.
  i=1
  while (acc_pov[i]<=pov_critical){
    i=i+1
  }

  ##Constructing the Principal Components by its number obtained above.
  new_predictor=matrix(nrow=nrow(df),ncol = i)
  new_colnames=paste("PC",1:i,sep=" ")
  new_colnames=append(new_colnames,res_name)
  for(j in 1:i){
    new_predictor[,j]=as.matrix(predictor)%*%as.matrix(pca$rotation[,j])
  }

  new_df=cbind(new_predictor,response)
  colnames(new_df)=new_colnames
  return(new_df)
}

