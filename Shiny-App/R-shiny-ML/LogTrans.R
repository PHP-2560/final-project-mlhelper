
LogTrans=function(df,fp,res_name){

  cols=colnames(df)
  response=df[,which(cols==res_name)]

  ##Use the adjusted Fisher-Pearson coefficient of skewness to detremin whether the data is skew.
  n=length(response)
  mu=mean(response)
  s=sd(response)
  adj_fp=(sqrt(n*(n-1))/(n-2))*sum((response-mu)^3)/(n*s^3)

  ##If the reponse's adjusted Fisher-Pearson coefficient of skewness is larger than the user-difined critical value, we do the log-transformation, and we don't do anything if not.
  if (adj_fp>fp){
    df[,which(cols==res_name)]=log(response)
  }
  return(df)
}
