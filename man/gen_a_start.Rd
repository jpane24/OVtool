\name{gen_a_start}
\alias{gen_a_start}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gen_a_start
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function is a wrapper to ov_simgrid. It generates the a. a is used to control the strength of the relationshp between the unobserved counfounder, U, and the treatment indicator
}
\usage{
gen_a_start(y, tx, es, rho, b1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{x} here~~
A vector that represents the outcome.
}
  \item{tx}{
A vector for the treatment indicator (must be 0s and 1s).
}
  \item{es}{
An effect size value to simulate over.
}
  \item{rho}{
A rho (correlation) value to simulate over.
}
  \item{b1}{
additional parameter needed; occasionally assumed away unknowingly.
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
gen_a_start returns a list containing the following components:
\item{n1}{scalar representing sample size of treatment group (treat == 1)}
\item{ve1}{1 - b1^2 multiplied by the variance of Ystar1}
\item{b1}{bounded parameter for treatment group (treat == 1) that it with b0 are selected to set the correlation of the omitted variable and the outcome equal to rho}
\item{ystar1}{inverse of the standard normal distribution cumulative distribution function of the outcome for the treatment group sample (treat == 1)}
\item{es}{}
\item{pi}{proportion of population that is in the treatment group (treat == 1)}
\item{n0}{scalar represnting sample size of control group (treat == 0)}
\item{ve0}{1 - b0^2 multiplied by the variance of Ystar0}
\item{b0}{bounded parameter for control group (treat == 0) that it with b1 are selected to set the correlation of the omitted variable and the outcome equal to rho}
\item{ystar0}{inverse of the standard normal distribution cumulative distribution function of the outcome for the control group sample (treat == 0)}
\item{n}{scalar representing the total sample size}
\item{ind}{vector of positions in data that represent treatment group (treat == 1)}
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
