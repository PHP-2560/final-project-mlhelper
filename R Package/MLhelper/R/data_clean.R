## data cleainig
##(1) remove NA and Inf
##(2) remove outliers
data_clean=function(object)
{
  remove_special=function(object)
  {
    is.special=function(y)
    {
      if (is.numeric(y)) !is.finite(y) else is.na(y)
    }
    ## If an element is a special value, then the corresponding position of the index matrix
    index=sapply(object, is.special)
    object=object[which(apply(index, 1, sum)==0),]
    object
  }

  ## remove outliers for numeric variable
  remove_outliers=function(object)
  {
    ncol=dim(object)[2]
    vnames=colnames(object) # store all variable names
    for (i in 1:ncol)
    {
      if (is.numeric(object[,vnames[i]])) #for each varaible in the data frame, check if it is a nuermic variable
      {

         outliers=boxplot.stats(object[,vnames[i]], coef = 2)$out # find the outliers
         if (length(outliers)!=0)
         {
          object=object[-which(object[vnames[i]]==outliers),]  # remove the correspoding row of each outliers
         }
      }
    }
    object
  }

  object=remove_special(object)
  object=remove_outliers(object)
  object
}


