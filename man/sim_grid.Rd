\name{sim_grid}
\alias{sim_grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sim_grid
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function will create the simulation grid. The simulation will iterate over
effects sizes and absolute correlations with the outcome (rho) and see how the
treatment effect and relevant p-value changes
}
\usage{
ps_object=NULL, stop.method=NULL, data=NULL, weights=NULL,
                     treatment=NULL, outcome=NULL, covariates=NULL,
                     es_grid = NULL, # user can specify a grid to simulate.
                     rho_grid = NULL,
                     n_reps = 101, estimand = "ATE"

sim_grid(ps.object, stop.method, data, weights, treatment, outcome, covariates,
es_grid, rho_grid, n_reps, estimand)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  %%     ~~Describe \code{x} here~~
  A dataframe containing the data
}
  \item{ps_object}{
  %%     ~~Describe \code{x} here~~
  A ps object exported from TWANG
}
  \item{stop.method}{
  %%     ~~Describe \code{x} here~~
  If the user specifies ps_object, which stop.method should be used to export the weights
}
  \item{outcome}{
  %%     ~~Describe \code{x} here~~
  A vector that represents the outcome.
}
  \item{weights}{
  A vector of propensity score weights
}
  \item{treatment}{
  A vector for the treatment indicator (must be 0s and 1s)
}
  \item{covariates}{
  vector of column names representing the covariates
}
  \item{es_grid}{
  Not required. A grid of effect sizes to simulate over
}
  \item{rho_grid}{
  Note required. A grid of correlations to simulate over this rho relates the correaltion between
  the effect size.
}
  \item{estimand}{
  "ATE"" or "ATT""
}
  \item{n_reps}{
  Number of repetitions to simulate over
}
}
\details{
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
%% ~put references to the literature/web site here ~
}
\author{
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
