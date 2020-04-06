\name{outcome_model}
\alias{outcome_model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{outcome_model
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function will run the outcomes model for your analysis. Upon completeion,
use the model object returned from this function and call ov_simgrid to check
the sensitivity of your findings.
}
\usage{
outcome_model(ps_object = NULL, stop.method, data, weights, treatment, outcome, model_covariates, estimand = "ATE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ps_object}{
  %%     ~~Describe \code{x} here~~
  A ps object exported from TWANG
}
  \item{stop.method}{
  %%     ~~Describe \code{x} here~~
  If the user specifies ps_object, which stop.method should be used to export the weights
}
  \item{data}{
  %%     ~~Describe \code{x} here~~
  A dataframe containing the data
}
  \item{weights}{
  A vector of propensity score weights
}
  \item{treatment}{
  Column name for the treatment indicator
}
  \item{outcome}{
  %%     ~~Describe \code{x} here~~
  Column name for the outcome
}
  \item{model_covariates}{
%%     ~~Describe \code{model_covariates} here~~
  vector of column names representing the covariates in your final outcomes model
}
  \item{estimand}{
  "ATE"" or "ATT""
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
function (ps_object = NULL, stop.method, data, weights, treatment,
    outcome, model_covariates, estimand = "ATE")
{
    set.seed(24)
    if (class(ps_object) != "ps") {
        if (missing(data) | missing(weights) | missing(treatment) |
            missing(outcome) | missing(model_covariates)) {
            stop("Please supply either a ps class object, stop.method, and relevant column names for:\n      outcome, and covariates OR the data and relevant column names for: weights, treatment, outcome, & covariates")
        }
        else {
            data$w_orig = weights
            y = outcome
            tx = treatment
            if (typeof(model_covariates) == "language") {
                cov = all.vars(model_covariates)
            }
            else {
                cov = model_covariates
            }
            design_u <- survey::svydesign(ids = ~1, weights = ~w_orig,
                data = data)
        }
    }
    else {
        if (is.null(stop.method) | is.null(outcome) | is.null(model_covariates)) {
            stop("Please supply a stop.method to generate the weights (e.g. \"ks.max/")
        }
        y = outcome
        tx = ps_object$treat.var
        if (typeof(model_covariates) == "language") {
            cov = all.vars(model_covariates)
        }
        else {
            cov = model_covariates
        }
        data = ps_object$data
        data$w_orig = twang::get.weights(ps_object, stop.method = stop.method,
            estimand = estimand)
        design_u <- survey::svydesign(ids = ~1, weights = ~w_orig,
            data = data)
    }
    formula = formula(paste0("scale(", y, ") ~ ", tx, " + ",
        glue::glue_collapse(cov, sep = " + ")))
    if (length(unique(data[, "y"])) <= 1) {
        stop("No variation in outcome.")
    }
    else {
        model_results = survey::svyglm(formula, design = design_u)
    }
    print(summary(model_results))
    return(list(ps_object, stop.method, data, weights, tx, y,
        model_covariates, estimand, design_u, outcome_mod_results))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
