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
outcome_model(ps_object = NULL, stop.method=NULL, data, weights=NULL, treatment,
outcome, model_covariates, estimand = "ATE")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ps_object}{
  %%     ~~Describe \code{x} here~~
  A ps object exported from TWANG
}
  \item{stop.method}{
  %%     ~~Describe \code{x} here~~
  If the user specifies ps_object, stop.method should be used to export the weights (e.g "ks.max")
}
  \item{data}{
  %%     ~~Describe \code{x} here~~
  A data frame containing the data
}
  \item{weights}{
  A vector of propensity score weights
}
  \item{treatment}{
  A column name in data for the treatment indicator
}
  \item{outcome}{
  %%     ~~Describe \code{x} here~~
  A column name in data indicating the outcome vector
}
  \item{model_covariates}{
%%     ~~Describe \code{model_covariates} here~~
  A vector of column names representing the covariates in your final outcome's model
}
  \item{estimand}{
  "ATE" or "ATT"
}
}
\value{
%%  ~Describe the value returned
outcome_model returns a list containing the following components:
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{ps_object}{The ps_object from TWANG specified in the function call. If ignored, this component will be NULL}
\item{stop.method}{The stop method, if applicable, specified in the function call}
\item{data}{the updated data frame}
\item{weights}{a vector of weights}
\item{tx}{a character name in data indicating the treatment indicator}
\item{y}{a character name in data indicating the outcome}
\item{outcome_mod_fmla}{the final outcome model formula}
\item{estimand}{The estimand specified in the function call}
\item{mod_results}{an object of class "svyglm"}

}
\references{
Lumley T (2020). “survey: analysis of complex survey samples.” R package version 4.0.
}

