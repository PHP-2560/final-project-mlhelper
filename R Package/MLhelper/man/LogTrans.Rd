\name{LogTrans}
\alias{LogTrans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log-Transformation of Response in a data frame
}
\description{
LogTrans takes in a data frame, set a user-defined variable as response and do the log-transformation to the response by inspecting the skewness of the distribution of response.
}
\usage{
LogTrans(df,fp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
a data frame, where each row is an observation and each column is a predictor or response, there should be only 1 response and it's numeric.
}
  \item{fp}{
  a number,usually between 0 and 1, it the critical value of adjusted Fisher-Pearson coefficient of skewness, when the adjusted Fisher-Pearson coefficient of skewness of the data response is larger than this value, we think the distribution of response is skewed and will do the log-transformation
  }
  \item{...}{
  additional arguments to be passed to the low level regression fitting functions (see below).
  }
}
\details{
This LogTrans() function takes 2 arguement which are df (a data frame) and fp(the critical of adjusted Fisher-Pearson coefficient of skewness). When this function is called, the user will be asked for the col name of response and the untion will atumatically select the response from the data frame, then it will decide whether the data is so skewed that need a log-transformation, and finally give out a data frame after processed.
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
This designed was raised by the discussion of Mingxuan Bo and Chengqian Sun (2018) and was coded by Mingxuan Bo(2018)
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
> LogTrans(df=mtcars,fp=0.2)
Enter the name of response: mpg
                         mpg cyl  disp  hp drat    wt  qsec vs am gear carb
Mazda RX4           3.044522   6 160.0 110 3.90 2.620 16.46  0  1    4    4
Mazda RX4 Wag       3.044522   6 160.0 110 3.90 2.875 17.02  0  1    4    4
Datsun 710          3.126761   4 108.0  93 3.85 2.320 18.61  1  1    4    1
Hornet 4 Drive      3.063391   6 258.0 110 3.08 3.215 19.44  1  0    3    1
Hornet Sportabout   2.928524   8 360.0 175 3.15 3.440 17.02  0  0    3    2
Valiant             2.895912   6 225.0 105 2.76 3.460 20.22  1  0    3    1
.... several rows omitted
}

