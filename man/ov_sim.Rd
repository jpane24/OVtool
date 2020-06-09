\name{ov_sim}
\alias{ov_sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ov_sim
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function will create the simulation grid. The simulation will iterate over
effects sizes and absolute correlations with the outcome (rho) and see how the
treatment effect and relevant p-value changes
}
\usage{
ov_sim(model_results, weight_covariates, es_grid = seq(-.4, .4, by = 0.05),
rho_grid = seq(0, .4, by = 0.05), n_reps = 50)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model_results}{
  object returned from outcome_model
}
  \item{weight_covariates}{
  vector of column names representing the covariates you used to create your propensity score weights
}
  \item{es_grid}{
  Not required. A grid of effect sizes to simulate over
}
  \item{rho_grid}{
  Not required. A grid of correlations to simulate over; rho relates the correlation to the effect size.
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
ov_sim returns a list containing the following components:
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{p_val}{matrix of pvalues for each grid point}
\item{trt_effect}{matrix of effect sizes for each grid point}
\item{es_grid}{vector of the effect size grid}
\item{rho_grid}{vector of the rho grid}
\item{cov}{vector of covariates used to estimate propensity score weights}
\item{data}{the initial data frame containing data with new weights}
\item{tx}{column name in data representing the treatment indicator}
\item{y}{column name in data representing the outcome}
\item{estimand}{estimand used}
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
