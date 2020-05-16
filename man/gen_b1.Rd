\name{gen_b1}
\alias{gen_b1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{gen_b1
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gen_b1(y, tx, es, rho)
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
A rho (correlation) value to simulate over.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
gen_b1 returns a vector containing the following two components:
\item{b1low}{lower b1 limit for each es and rho combination}
\item{b1high}{upper b1 limit for each es and rho combination}
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
