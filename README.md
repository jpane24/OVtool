OVtool
================





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
treatment B. `eps7p_3` ranges from zero to one, where higher values of
EPS indicate more emotional problems. See (Dennis, 2003) for more
details on this scale.

Past research <ins style="color:red">(cite?)</ins> has indicated there
are many influential confounders when analyzing adolescents’ emotional
problems, some included in this synthetic dataset. These variables were
measured at baseline: emotional problem scale, adjusted days abstinent
(any in past 90) (`ada_0`), substance frequency scale (`sfs8p_0`),
substance abuse treatment index (`sati_0`), in recovery (`recov_0`),
traumatic stress scale (`tss_0`), and internal mental distress scale
(`imds_0`). 

We begin by loading the package and data:

``` r
install_github("jpane24/OVtool")
library(OVtool)
set.seed(24)
```

``` r
data(sud)
head(sud)
```

    ##   treat tss_0     tss_3     tss_6    sfs8p_0   sfs8p_3   sfs8p_6   eps7p_0
    ## 1     A     0 0.0000000 9.0000000  1.1111111  7.301587  0.000000  4.761905
    ## 2     A     0 0.0000000 0.0000000  0.4166667 18.333333  8.611111 21.746032
    ## 3     A     4 2.0929730 0.3283035  0.0000000  3.194444 26.666667 41.587302
    ## 4     A     0 5.6843082 0.0000000 36.5277778 29.305556 20.833333 38.888889
    ## 5     A     2 0.6815128 1.3258402  0.5555556  2.174038  0.000000 14.285714
    ## 6     A     0 0.0000000 0.0000000  0.0000000  0.000000  0.000000 20.634921
    ##     eps7p_3   eps7p_6   ias5p_0 dss9_0 mhtrt_0   sati_0 sp_sm_0   sp_sm_3
    ## 1  8.253968 69.523810  6.666667      1       1 1.111111       1  4.451943
    ## 2  2.380952  4.761905  6.666667      3       2 0.000000       0  4.000000
    ## 3 23.846689 15.873016 10.444444      6       1 0.000000       0 11.000000
    ## 4 53.968254 27.301587  0.000000      4       2 0.000000       4  3.512196
    ## 5  9.523810 13.915008 16.444444      2       1 0.000000       2  1.000000
    ## 6 16.666667  0.000000  0.000000      1       0 0.000000       0  0.000000
    ##   sp_sm_6 gvs ers21_0    nproc ada_0 ada_3    ada_6 recov_0 recov_3 recov_6
    ## 1       0   0      29 28.46232    80    76 89.00000       0       0       1
    ## 2       1   3      37 10.82736    88    55 39.18048       1       0       0
    ## 3       0  10      30 10.00000    84    85  0.00000       1       0       1
    ## 4       4   6      33 20.00000    17    29  0.00000       0       0       0
    ## 5       0   3      39 47.00000    74     0  0.00000       0       1       0
    ## 6       0   0      26 42.52685    90    90 90.00000       1       1       1
    ##   subsgrps_n sncnt engage
    ## 1          2    15      1
    ## 2          2    NA     NA
    ## 3          1     3      0
    ## 4          1     9      0
    ## 5          1    11      1
    ## 6          2    28      1
