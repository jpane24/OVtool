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
    ## eps7p_0  0.003  0.002  0.002  0.002      0.187  5.937 0.000 0.103   0.000
    ## sfs8p_0 11.253 13.134 10.571 12.162      0.054  1.703 0.089 0.045   0.032
    ## sati_0   8.233 22.128  2.145 10.658      0.345 11.088 0.000 0.121   0.000
    ## ada_0   48.748 33.400 54.236 32.454     -0.166 -5.271 0.000 0.081   0.000
    ## recov_0  0.246  0.431  0.240  0.427      0.015  0.479 0.632 0.006   1.000
    ## tss_0    2.277  3.525  1.924  3.115      0.106  3.365 0.001 0.043   0.050
    ## mhtrt_0  0.290  0.513  0.256  0.484      0.069  2.188 0.029 0.028   0.413
    ## dss9_0   2.750  2.604  2.638  2.492      0.044  1.390 0.165 0.023   0.666
    ## 
    ## $ks.max.ATE
    ##          tx.mn  tx.sd  ct.mn  ct.sd std.eff.sz   stat     p    ks ks.pval
    ## eps7p_0  0.002  0.002  0.002  0.002      0.034  1.052 0.293 0.023   0.701
    ## sfs8p_0 10.819 12.602 10.666 12.314      0.012  0.382 0.703 0.012   0.999
    ## sati_0   5.291 17.829  4.085 15.436      0.068  1.942 0.052 0.024   0.652
    ## ada_0   51.609 32.900 52.565 32.838     -0.029 -0.887 0.375 0.019   0.861
    ## recov_0  0.246  0.431  0.240  0.427      0.016  0.478 0.633 0.007   1.000
    ## tss_0    2.094  3.345  2.018  3.238      0.023  0.705 0.481 0.014   0.988
    ## mhtrt_0  0.272  0.502  0.274  0.500     -0.005 -0.157 0.876 0.004   1.000
    ## dss9_0   2.677  2.549  2.681  2.527     -0.002 -0.052 0.959 0.008   1.000

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
    ## (Intercept) -7.090e-01  6.498e-02 -10.911  < 2e-16 ***
    ## treat        7.900e-02  2.752e-02   2.871  0.00411 ** 
    ## eps7p_0      1.634e+02  1.182e+01  13.830  < 2e-16 ***
    ## sfs8p_0      2.359e-03  2.032e-03   1.161  0.24587    
    ## sati_0       1.669e-03  1.231e-03   1.356  0.17515    
    ## ada_0       -4.595e-05  7.589e-04  -0.061  0.95172    
    ## recov_0     -6.924e-02  3.144e-02  -2.202  0.02771 *  
    ## tss_0        3.113e-02  6.876e-03   4.528 6.14e-06 ***
    ## mhtrt_0      2.697e-01  3.553e-02   7.592 3.89e-14 ***
    ## dss9_0       4.895e-02  7.849e-03   6.237 4.93e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.6887949)
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
                                          rho_grid = seq(0, 0.4, by = 0.05),
                                          n_reps=10)
```

    ## Warning in OVtool::ov_simgrid(model_results = results, weight_covariates =
    ## c("eps7p_0", : Ties in the outcome variable `y` may be problematic.

    ## [1] "11% Done!"
    ## [1] "22% Done!"
    ## [1] "33% Done!"
    ## [1] "44% Done!"
    ## [1] "56% Done!"
    ## [1] "67% Done!"
    ## [1] "78% Done!"
    ## [1] "89% Done!"
    ## [1] "100% Done!"

``` r
# plot(ovtool_results_twang, print_all = "1")
# plot(ovtool_results_twang, print_all = "2")
# plot(ovtool_results_twang, print_all = "3")
```
