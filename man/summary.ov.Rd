\name{summary.ov}
\alias{summary.ov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{summary.ov
%%  ~~function to do ... ~~
}
\description{
Produces summary information that contains a recommendation for reporting the sensitivity analyses
}
\usage{
\method{summary}{ov}(object, model_results, sig_level = 0.05, progress = TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
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
  \item{...}{
Additional arguments.
}
}
\value{
This function will print a recommendation for reporting the sensitivity analyses.
}

