\name{pca_dim_redc}
\alias{pca_dim_redc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dimension Reduction with Principle Components Analysis
}
\description{
This function use the Principle Components Analysis Algorithm to reduce the dimension of original predictors, helpful to avoid over-fitting problem
}
\usage{
pca_dim_redc(df,pov_critical)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame, where each row is an observation and each column is a predictor or response, there should be only 1 response and it's numeric.
}
\item{pov_critical}{
Critical value of proportion of variance that Principle Components must have, this value will decide how many principle components we will have after PCA, its value need to be in [0,1], largert it is, more principle components we will have.
}
}
\details{
This pca_dim_redc() function will take in 2 arguements, df---data frame we work on & pov_critical---critical value of proportion of variance deciding how many principle components we will have. There is a Principle Components Analysis towards the data inside our pca_dim_redc() function, and it will give out a transformed data frame of principle components and the response
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
This designed was raised by the discussion of Mingxuan Bo and Chengqian Sun (2018) and was coded by Mingxuan Bo(2018)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
> pca_dim_redc(mtcars,0.95)
Enter the name of response: mpg
           PC 1       PC 2      PC 3       PC 4      PC 5      PC 6  mpg
 [1,] 100.62577 -15.609333 -27.88612  -39.33519 107.82134  58.08916 21.0
 [2,] 100.59617 -15.313938 -28.21022  -39.43981 107.66508  58.36321 21.0
 [3,]  71.62771 -11.748460 -25.38631  -26.78821  82.41514  41.59282 22.8
 [4,] 138.70355  -6.275859 -26.29193  -65.38619 141.84053  94.23576 21.4
 [5,] 204.07758 -18.515888 -35.46481  -88.50994 210.33208 127.43142 18.7
 .... several rows omitted
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
