\name{plot.ov}
\alias{plot.ov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot
%%  ~~function to do ... ~~
}
\description{
Plots the user specified graphic(s)
}
\usage{
\method{plot}{ov}(ov_sims, col="color", print_graphic="1", p_contours = c(0.01, 0.05, 0.1), \dots)
}

\arguments{
  \item{ov_sims}{
Object returned from the call to ov_sim
}
  \item{col}{
If user wants color or black and white. Specify color with "color" or black and white "bw"
}
  \item{print_graphic}{
%%     ~~Describe \code{x} here~~
Takes values "1", "2", or "3", depending what graphics the user wants
}
  \item{p_contours}{
%%     ~~Describe \code{x} here~~
P-value countours to plot. The default plots: 0.01, 0.05, and 0.1. We only recommend changing this if the raw effect p-value is very close to one of these values. Do not specify more than four p-value contours.
}
\item{...}{
Additional arguments.
}
}

\value{
This function will print the plot to screen that the use specifies with print_graphic.
}
