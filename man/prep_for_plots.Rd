\name{prep_for_plots}
\alias{prep_for_plots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{prep_for_plots
%%  ~~function to do ... ~~
}
\description{
Data preparation for producing the graphics and summary results.
}
\usage{
prep_for_plots(r1, p_contours)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r1}{
%%     ~~Describe \code{x} here~~
An object returned from sim_grid
}
  \item{p_contours}{
%%     ~~Describe \code{x} here~~
P-value countours to plot. The default plots: 0.01, 0.05, and 0.1. We only recommend changing this if the raw effect p-value is very close to one of these values. Do not specify more than four p-value contours.
}
}
\value{
%%  ~Describe the value returned
prep_for_plots returns a list containing the following components:
\item{r1}{a list with the components returned from ov_simgrid}
\item{r1_df}{a data frame with components used to create the contour graphic}
\item{obs_cors}{a data frame with components used to plot the observed covariates on plot_graphic = "2" and plot_graphic = "3"}
\item{text_high}{a character noting the covariates whose absolute correlation with the outcome is greater than the grid allows}
\item{text_high_es}{a character noting the covariates with effect sizes greater than the maximum the plot will allow}
\item{pvals}{a vector of p-value thresholds to be plotted on the graphics}
\item{pval_lines}{a vector of line types to represent pvals}
\item{raw}{a character with the raw effect and pvalue from the outcome model}

}
