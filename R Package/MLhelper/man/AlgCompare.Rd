\name{AlgCompare}
\alias{AlgCompare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
4 Machine Learning Algorithms Comparision
}
\description{
This function takes in a data frame and returns a table of MSE of prediction on testing data computed by 5 machine learning algorithms: Rideg, lasso, Elmnet, Bagging and Random Forest.
}
\usage{
AlgCompare(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame, where each row is an observation and each column is a predictor or response, there should be only 1 response and it's numeric.
}
}
\details{
This function has one arguement: df-a numeric data frame and promote user to give the name of response. This function then randomly selects 1/5 of observations as testing data and the rest observation as training data, it builds models with training data on 5 methods: Rideg, lasso, Elmnet, Bagging and Random Forest. Then this function calculates the mse of 5 different models and report the mse to user. Note that the format of input data of 5 machine learning methods are different, and our AlgCompare() will make appropriate adjustment on data format.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
This designed was raised by the discussion of Mingxuan Bo and Chengqian Sun (2018) and was coded by Chengqian Sun (2018)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
> AlgCompare(mtcars)
Enter the name of response: mpg
                    mse
Ridge          6.752640
LASSO          4.968938
Elnet         32.427934
Random Forest  8.164507
Bagging       11.995100
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
