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
\method{plot}{ov}(x, col="color", print_graphic="1", \dots)
}

\arguments{
  \item{x}{
Object returned from sim_grid
}
  \item{col}{
If user wants color or black and white. Specify color with "color" or black and white "bw"
}
  \item{print_graphic}{
%%     ~~Describe \code{x} here~~
Takes values "1", "2", or "3", depending what graphics the user wants
}
\item{...}{
Additional arguments.
}
}

\value{
This function will print the plot to screen that the use specifies with print_graphic.
}
