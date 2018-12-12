\name{data_clean}
\alias{data_clean}
\title{Data Clean
}
\description{
This function will clean the data frame. It requires a 'data.frame' as input and return a cleanned dataframe.

}
\usage{
data_clean(object)
}
\arguments{
  \item{object}{
  a data frame, where each row is an observation and each column is a predictor or response, there should be only 1 response and it's numeric.
}
}
\details{
This function will clean the data frame. It requires a 'data.frame' as
input. First, it will omit the NA and Inf observations. Then it will
remove the outliers. The cleaned version of dataframe will be returned.
}
\value{

}
\references{

}
\author{
This designed was raised by the discussion of Mingxuan Bo and Chengqian Sun (2018) and was coded by Chengqian Sun(2018)
}
\note{

}

\seealso{

}
\examples{
   people
     Name  Age Weight
1    Adam   12    179
2     Bob   15  99999
3   Chuck  999    123
4 Charlie   20    123
5    Dick   30    123
6    Evan   45    341
7     Eve   NA    198
8   Frank   76     13
9  George 9800     NA

data_clean(people)
     Name Age Weight
1    Adam  12    179
4 Charlie  20    123
5    Dick  30    123
6    Evan  45    341
8   Frank  76     13
