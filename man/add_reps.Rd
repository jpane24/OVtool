\name{add_reps}
\alias{add_reps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{add_reps
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add_reps(OVtool_results, model_results, more_reps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{OVtool_results}{
The object returned from OVtool::ov_simgrid()
}
  \item{model_results}{
The object returned from OVtool::outcome_model()
}
  \item{more_reps}{
The number of additional repetitions the user wants to simulate the unobserved confounder
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
add_reps returns an updated object returned from OVtool::ov_simgrid()
%%  ~Describe the value returned
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
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
