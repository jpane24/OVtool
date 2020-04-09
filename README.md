OVtool - Omitted Variable tool
================





**Note: This is a work in progress**

# Introduction

The <ins>O</ins>mitted <ins>V</ins>ariable tool (`OVtool`) package was
designed to assess the sensitivity of research findings to omitted
variables when estimating causal effects using propensity score (PS)
weighting. This package includes graphics and summary results that will
enable a researcher to quantify the impact an omitted variable would
have on their results. Burgette et al. (in preparation) describe the
methodology behind the primary function in this package, `ov_simgrid()`.
This document presents syntax for the implementation of the
`ov_simgrid()` function and provides an example of how to interpret the
packages’ graphical output.

This package is useful in a wide range of applications where researchers
want to analyze how sensitive their research findings are to unobserved
confounders that were not included in their propensity score and outcome
models. It will estimate the potential impact of the unobserved
counfounders on both the estimated treatment or exposure effects as well
as on the statistical significance of an analysis.

# Example: Synthetic Data

This package is demonstrated using a random subset of the Global
Appraisal of Individual Needs biopsychosocial assessment instrument
(GAIN) (Dennis, Titus et al. 2003) from sites that adminstered two
different types of substance use disorder treatments (treatment “A” and
treatment “B”). The Center for Substance Abuse Treatment (CSAT) funded
the sites that administered these two SUD treatments. This dataset
consists of 4,000 adolescents. The main goal of this analysis is to
understand the effect Treatment A and Treatment B, indicated by `treat`,
have on mental health outcomes.

In this synthetic dataset, there are 2,000 adolescents in each treatment
group. Within this dataset there are variables on substance use disorder
and mental health outcomes. For this tutorial we are particularly
interested in the mental health outcome, `eps7p_3`, emotional problem
scale (eps) recorded at three months. Higher values of eps indicate more
emotional problems. Substance abuse researchers are particularly
interested in whether or not treatment A reduces emotional problems more
than treatment B. `eps7p_3` ranges from zero to one, where higher values
of EPS indicate more emotional problems. See (Dennis, 2003) for more
details on this scale.

Past research <ins style="color:red">(cite?)</ins> has indicated there
are many influential confounders when analyzing adolescents’ emotional
problems, some included in this synthetic dataset. These variables were
measured at baseline: emotional problem scale, adjusted days abstinent
(any in past 90) (`ada_0`), substance frequency scale (`sfs8p_0`),
substance abuse treatment index (`sati_0`), in recovery (`recov_0`),
traumatic stress scale (`tss_0`), and internal mental distress scale
(`imds_0`). 

We begin by loading the development version of the package from
[GitHub](https://github.com/) with:

``` r
install_github("jpane24/OVtool")
library(OVtool)
set.seed(24)
```

We can load the synthetic dataset and print to screen the first six
observations by running the following two commands:

``` r
data(sud) 
# head(sud)
sud$treat = ifelse(sud$treat == "A", 1, 0)
table(sud$treat)
```

    ## 
    ##    0    1 
    ## 2000 2000

The relevant variables in this analysis are:

  - **Treatment indicator** `treat`: indicates treatment type where 1 is
    Treatment “A” and 0 is Treatment “B”

  - **Outcome of interest** `eps7p_3`: emotional problem scale at
    3-months

  - `eps7p_0`: emotional problem scale at baseline

  - `sfs8p_0`: substance frequency scale 8-item version at baseline

  - `sati_0`: substance abuse treatment index at baseline

  - `ada_0`: adjusted days abstinent at baseline

  - `recov_0`: indicates whether the adolescent was in recovery at
    baseline, where 1 is in recovery and 0 is not in recovery

  - `tss_0`: traumatic stress scale at baseline

The package will accept propensity score weights or, if not provided by
the user, will calculate propensity score weights using  Ridgeway et al.
(2014). In the next section, we will show how our method works with the
average treatment effect (ATE) using a continuous outcome. The OVtool
also handles binary outcomes and weights that were estimated using the
average treatment effect on the treated (ATT) estimand.
