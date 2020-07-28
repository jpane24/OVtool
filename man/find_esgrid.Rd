\name{find_esgrid}
\alias{find_esgrid}
\title{find_esgrid
}
\description{
Finds a reasonable effect size grid to simulate over.
}
\usage{
find_esgrid(my_data, my_cov, treatment, outcome, my_estimand)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{my_data}{
%%     ~~Describe \code{x} here~~
Data
}
  \item{my_cov}{
%%     ~~Describe \code{x} here~~
vector of covariates
}
  \item{treatment}{
%%     ~~Describe \code{x} here~~
column name of treatment indicator in my_data
}
  \item{outcome}{
%%     ~~Describe \code{x} here~~
column name of outcome in my_data
}
  \item{my_estimand}{
%%     ~~Describe \code{x} here~~
Relevant estimand ("ATE" or "ATT")
}
}
\value{
%%  ~Describe the value returned
a data frame with three columns, "Cor_Outcome", "es", and "cov". "Cor_Outcome" represents rho grid values, "ES" represents the range of grid values to represent the association between the unobserved confounder and the treatment indicator on the effect size scale, and "cov" is a vector of all the covariates used in the propensity score model
}

