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
                          tss_0 +  mhtrt_0 + dss9_0)

## Get weights
sud = data.frame(sud)
ps.twang <- twang::ps(my_formula, data = sud, estimand = 'ATE', booster = "gbm",
                      stop.method = "ks.max", verbose=F, ks.exact = T)

# Check Balance
twang::bal.table(ps.twang); # summary(ps.twang)
```

    ## $unw
    ##          tx.mn  tx.sd  ct.mn  ct.sd std.eff.sz   stat     p    ks ks.pval
    ## eps7p_0 24.760 18.852 21.325 18.841      0.182  5.765 0.000 0.091   0.000
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
    ## eps7p_0 23.067 18.646 22.603 18.709      0.025  0.765 0.444 0.020   0.826
    ## sfs8p_0 11.367 13.004 11.241 12.836      0.010  0.300 0.764 0.013   0.996
    ## sati_0   5.213 17.396  4.327 15.739      0.052  1.542 0.123 0.021   0.799
    ## ada_0   52.004 32.877 52.064 32.880     -0.002 -0.056 0.955 0.016   0.964
    ## recov_0  0.254  0.435  0.244  0.429      0.023  0.708 0.479 0.010   1.000
    ## tss_0    1.964  3.251  1.967  3.212     -0.001 -0.034 0.973 0.013   0.998
    ## mhtrt_0  0.278  0.493  0.261  0.485      0.034  1.047 0.295 0.017   0.941
    ## dss9_0   2.544  2.477  2.558  2.438     -0.006 -0.176 0.860 0.014   0.988

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

  - Input a `ps.object` from `TWANG`, a `stop.method` (e.g. `"ks.max"`)
    or
  - Input a vector of `weights`, a data frame containing the `data`
    used, and the column name representing the treatment indicator.

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
                                outcome = "eps7p_3_std",
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
    ## design_u <- survey::svydesign(ids=~1, weights=~w_orig, data=data)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.574651   0.064473  -8.913  < 2e-16 ***
    ## treat        0.021525   0.027113   0.794   0.4273    
    ## eps7p_0      0.016879   0.001176  14.356  < 2e-16 ***
    ## sfs8p_0     -0.001114   0.002012  -0.554   0.5799    
    ## sati_0       0.002270   0.001177   1.928   0.0539 .  
    ## ada_0       -0.001056   0.000739  -1.428   0.1532    
    ## recov_0     -0.073422   0.031700  -2.316   0.0206 *  
    ## tss_0        0.038360   0.006881   5.575 2.64e-08 ***
    ## mhtrt_0      0.143324   0.033906   4.227 2.42e-05 ***
    ## dss9_0       0.050808   0.008117   6.259 4.27e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.6863185)
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

  - a vector of column names representing the covariates used to produce
    the analysts propensity score weights (these may or may not be the
    same as the list of covariates used for the outcome model)

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
