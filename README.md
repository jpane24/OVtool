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
# get EPS on proper scale according to GAIN
sud$eps7p_0 = sud$eps7p_0/100
sud$eps7p_3 = sud$eps7p_3/100
sud$eps7p_6 = sud$eps7p_6/100
```

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

  - `mhtrt_0`: mental health treatment in the past 90 days at baseline

  - `dss9_0`: depressive symptom scale at baseline

In the next section, we will show how our method works with the average
treatment effect (ATE) using a continuous outcome. The OVtool also
handles binary outcomes and weights that were estimated using the
average treatment effect on the treated (ATT) estimand.

## Continous Outcome: Average Treatment Effect (ATE)

The `OVtool` can either take a vector of weights estimated using any
method or a ps object produced by `TWANG`. We begin walking through the
OVtool by estimating weights using `ps()` from the `TWANG` package prior
to running our outcome model using `OVtool::outcome_model()`. The
snippet of code belows walks through an example:

``` r
## Create Formula
my_formula = as.formula(treat ~ eps7p_0 + sfs8p_0 + sati_0 + ada_0 + recov_0 + 
                          tss_0 + mhtrt_0 + dss9_0)

## Get weights
sud = data.frame(sud)
ps.twang <- twang::ps(my_formula, data = sud, estimand = 'ATE', booster = "gbm",
                      stop.method = "ks.max", verbose=F, ks.exact = T)

# Check Balance
twang::bal.table(ps.twang); # summary(ps.twang)
```

    ## $unw
    ##          tx.mn  tx.sd  ct.mn  ct.sd std.eff.sz   stat     p    ks ks.pval
    ## eps7p_0  0.248  0.189  0.213  0.188      0.182  5.765 0.000 0.091   0.000
    ## sfs8p_0 12.077 13.768 10.707 12.218      0.105  3.331 0.001 0.062   0.001
    ## sati_0   7.449 20.788  2.748 12.169      0.273  8.728 0.000 0.101   0.000
    ## ada_0   49.841 33.660 53.710 32.162     -0.117 -3.718 0.000 0.060   0.002
    ## recov_0  0.250  0.433  0.244  0.429      0.015  0.477 0.634 0.006   1.000
    ## tss_0    2.096  3.379  1.857  3.113      0.074  2.329 0.020 0.034   0.185
    ## mhtrt_0  0.306  0.514  0.240  0.469      0.134  4.241 0.000 0.058   0.002
    ## dss9_0   2.644  2.548  2.502  2.402      0.057  1.812 0.070 0.034   0.198
    ## 
    ## $ks.max.ATE
    ##          tx.mn  tx.sd  ct.mn  ct.sd std.eff.sz   stat     p    ks ks.pval
    ## eps7p_0  0.231  0.186  0.226  0.187      0.024  0.756 0.450 0.020   0.821
    ## sfs8p_0 11.366 13.011 11.243 12.838      0.009  0.295 0.768 0.013   0.996
    ## sati_0   5.212 17.393  4.325 15.727      0.052  1.546 0.122 0.021   0.798
    ## ada_0   52.009 32.877 52.056 32.882     -0.001 -0.044 0.965 0.016   0.961
    ## recov_0  0.254  0.435  0.244  0.430      0.023  0.711 0.477 0.010   1.000
    ## tss_0    1.962  3.250  1.967  3.212     -0.001 -0.046 0.963 0.013   0.998
    ## mhtrt_0  0.278  0.493  0.261  0.485      0.035  1.079 0.280 0.017   0.936
    ## dss9_0   2.542  2.477  2.559  2.439     -0.007 -0.208 0.835 0.014   0.987

The output produced by the code snippet above shows that TWANG does a
reasonable job of balancing. There are additional diagnostics we could
check to ensure we have good balance but we move on without diving in
further because the purpose of this tutorial is to showcase `OVtool`.
The next step is to estimate the treatment effect and analyze the
sensitivity of those results using `OVtool`. We first present how a
researcher would produce results for their outcomes model
(`survey::svyglm()`). There are two options the researcher can take to
input the relevant information to get their outcome results using
`OVtool::outcome_model()`.

  - Input a `ps.object` from `TWANG` and a `stop.method` (e.g.
    `"ks.max"`) or
  - Input a vector of `weights`, a data frame containing the `data`
    used, and the column name representing the treatment indicator,
    `treatment`.

The analyst must also provide a column name representing the outcome and
a vector of covariates to be included in the final outcome model.

``` r
# Get weights (not needed if user inserts a ps object in OVTool)
sud$w_twang = ps.twang$w$ks.max.ATE

# Run Models -- first standardize outcome
sud$eps7p_3_std = sud$eps7p_3/sd(sud$eps7p_3) 

# Use OVtool::outcome_model() to run outcomes model
results = OVtool::outcome_model(ps_object = NULL, # could use ps.twang
                                stop.method = NULL, # could use ks.max
                                data = sud,
                                weights = sud$w_twang, 
                                treatment = "treat",
                                outcome = "eps7p_3",
                                model_covariates = c("eps7p_0", "sfs8p_0", 
                                                     "sati_0", "ada_0",
                                                     "recov_0", "tss_0",
                                                     "mhtrt_0", "dss9_0"),
                                estimand = "ATE")
summary(results$mod_results)
```

    ## 
    ## Call:
    ## svyglm(formula = formula, design = design_u)
    ## 
    ## Survey design:
    ## survey::svydesign(ids = ~1, weights = ~w_orig, data = data)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.5764849  0.0642017  -8.979  < 2e-16 ***
    ## treat        0.0217005  0.0270779   0.801   0.4229    
    ## eps7p_0      1.6871797  0.1174425  14.366  < 2e-16 ***
    ## sfs8p_0     -0.0010474  0.0020032  -0.523   0.6011    
    ## sati_0       0.0023012  0.0011757   1.957   0.0504 .  
    ## ada_0       -0.0010416  0.0007373  -1.413   0.1578    
    ## recov_0     -0.0732305  0.0317215  -2.309   0.0210 *  
    ## tss_0        0.0383000  0.0068819   5.565 2.79e-08 ***
    ## mhtrt_0      0.1442749  0.0337082   4.280 1.91e-05 ***
    ## dss9_0       0.0509168  0.0081181   6.272 3.94e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.6861048)
    ## 
    ## Number of Fisher Scoring iterations: 2

The outcome model results show an adjusted treatment effect estimate
that accounts for some of the confounding between youth the two
treatment programs. From the results, we can see that the effect size is
0.02 (p \< 0.427). At this stage, researchers should begin to ask
themselves if this effect is real and how sensitive it is. Our tool is
used to help answer these sort of logical next step questions. The next
snippet of code presents the main function in `OVtool`: `ov_simgrid`.
This function requires results from `OVtool::outcome_model` plus
additional parameters including:

  - `weight_covariates`: a vector of column names representing the
    covariates used to produce the analysts propensity score weights
    (these may or may not be the same as the list of covariates used for
    the outcome model)

  - `es_grid`: a vector on an effect size scale representing the
    association between an unobserved confounder (omitted variable) and
    the treatment indicator

  - `rho_grid`: a vector of correlations to simulate over. These
    correlations represent the correlation between the omitted variable
    and the outcome

  - `n_reps`: the number of repetitions at each grid point. The package
    defaults to 101.

The grid, as shown by the x-axis and y-axis in Figure 1 presents the
effect size and rho, respectively. We define rho in this setting as the
absolute correlation a covariate has with the outcome of interest.
Please see Burgette et al. (2020), for additional details.

``` r
# Run OVtool (with weights/not a ps object)
ovtool_results_twang = OVtool::ov_simgrid(model_results=results, 
                                          weight_covariates=c("eps7p_0", "sfs8p_0", 
                                                              "sati_0", "ada_0",
                                                              "recov_0", "tss_0",
                                                              "mhtrt_0", "dss9_0"),
                                          es_grid = NULL,
                                          rho_grid = NULL,
                                          n_reps=2)
```

    ## Warning in OVtool::ov_simgrid(model_results = results, weight_covariates =
    ## c("eps7p_0", : Ties in the outcome variable `y` may be problematic.

    ## [1] "14% Done!"
    ## [1] "29% Done!"
    ## [1] "43% Done!"
    ## [1] "57% Done!"
    ## [1] "71% Done!"
    ## [1] "86% Done!"
    ## [1] "100% Done!"

``` r
# plot(ovtool_results_twang, print_all = "1")
# plot(ovtool_results_twang, print_all = "2")
# plot(ovtool_results_twang, print_all = "3")
```
