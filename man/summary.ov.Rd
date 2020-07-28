\name{summary.ov}
\alias{summary.ov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{summary.ov
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.ov(OVtool_results, model_results, sig_level = 0.05, progress = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{OVtool_results}{
The object returned from OVtool::ov_simgrid()
}
  \item{model_results}{
The object returned from OVtool::outcome_model()
}
  \item{sig_level}{
The alpha level with default 0.05
}
  \item{progress}{
  Whether or not the function progress should print to screen. The default value is TRUE. If the user does not want the output to print to screen, they should set to FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function will print a recommendation for reporting the sensitivity analyses.
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
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
