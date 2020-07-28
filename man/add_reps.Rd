\name{add_reps}
\alias{add_reps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{add_reps
%%  ~~function to do ... ~~
}
\description{
This function will run additional simulations of the unobserved confounder and add the results to the object returned from the previous call to OVtool::ov_sim
}
\usage{
add_reps(OVtool_results, model_results, more_reps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{OVtool_results}{
The object returned from OVtool::ov_sim()
}
  \item{model_results}{
The object returned from OVtool::outcome_model()
}
  \item{more_reps}{
The number of additional repetitions the user wants to simulate the unobserved confounder
}
}

\value{
add_reps returns an updated object returned from OVtool::ov_sim()
}

