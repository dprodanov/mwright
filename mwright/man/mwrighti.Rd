\name{mwrighti}
\alias{mwrighti}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Integral of the Mainardi Wright function
%%  ~~function to do ... ~~
}
\description{
General probability distribution function.
Arises in fractional diffusion.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mwrighti(z, a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{range variable
%%     ~~Describe \code{z} here~~
}
  \item{a}{exponent
%%     ~~Describe \code{a} here~~
}
}
\details{
Special case:
mwrighti(x,1/2)=1/2+erf(x/2)/2
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Luchko Y, Algorithms for evaluation of the Wright function for the real arguments' values. Fract. Calc. Appl. Anal. 11 : 57-75, 2008
%% ~put references to the literature/web site here ~
}
\author{
Dimiter Prodanov
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (z, a)
{
    result <- 0
    b <- 1
    ifelse(z < 0, b <- 0.5, b <- (-0.5))
    z <- abs(z)
    a <- (-a)
    ker1 <- function(x) {
        ifelse(x <= 1e-08, 0, exp(-z * x * cos(pi * a) - 1/x^(1/a)) *
            sin(z * x * sin(pi * a))/x)
    }
    result <- 0.5 - b * integrate(ker1, lower = 0, upper = Inf)$value/(a *
        pi)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~wright }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mainardi }% __ONLY ONE__ keyword per line
