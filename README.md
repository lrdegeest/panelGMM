# panelGMM

General Method of Moments (GMM) estimators for panel data with lagged and differenced instruments in R. 

The function `panelGMM` estimates one-step and two-step GMM-IV linear models for panel data. Panel-robust standard errors allowing for heteroskedasticity and correlation over time are calculated using the algorithm laid out in Chapter 22 of Cameron and Trivedi (2005). 

The package also provides the following convenience functions to:

* rename variables so that independent variables are prepended with "x_" and dependent variables are prepended with "y_" (`renameVars`);
* create $n$-lagged variables in batches (`makeLAGS`);
* and create $n$-differenced variables in batches (`makeDIFFS`).

## Installation

```R
library("devtools")
install_github("lrdegeest/panelGMM")
```

## Demo

Recreate the base-case two-step GMM-IV model in Table 22.2 of Cameron and Trivedi (2005). The authors estimate wages of a panel of 532 workers from 1981 - 1988. 

```R
> #0. data
> load("hours_wages.RData")
> #1. rename variables
> hours_wages_gmm <- renameVars(hours_wages, "lnhr", c("lnwg", "kids", "age", "agesq", "disab"))
> #2. first difference (independent & dependent variables)
> hours_wages_gmm <- makeDIFFS(hours_wages_gmm, id, 1)
> #3. lag 1 (independent variables)
> hours_wages_gmm <- makeLAGS(hours_wages_gmm, id, 1, y = FALSE)
> #4. lag 2 (independent variables)
> hours_wages_gmm <- makeLAGS(hours_wages_gmm, id, 2, y = FALSE)
> #5. estimate two-step GMM
> model <- d1_y_lnhr ~ 0 + d1_x_lnwg + d1_x_kids + d1_x_age + d1_x_agesq + d1_x_disab | 0 + l1_x_kids + l1_x_age + l1_x_agesq + l1_x_disab + l2_x_kids + l2_x_age + l2_x_agesq + l2_x_disab + l2_x_lnwg
> estimation <- panelGMM(model, time = year, nlags = 2, twostep = T, data = hours_wages_gmm)
> # view results
> estimation$summary
$Call
panelGMM(formula = model, time = year, nlags = 2, twostep = T, 
    data = hours_wages_gmm)

$Model
d1_y_lnhr ~ 0 + d1_x_lnwg + d1_x_kids + d1_x_age + d1_x_agesq + 
    d1_x_disab | 0 + l1_x_kids + l1_x_age + l1_x_agesq + l1_x_disab + 
    l2_x_kids + l2_x_age + l2_x_agesq + l2_x_disab + l2_x_lnwg

$Results
           Estimated.coefficient Standard.error p.value
d1_x_lnwg                 0.5468         0.3276  0.0951
d1_x_kids                -0.0449         0.0271  0.0981
d1_x_age                  0.0275         0.0130  0.0340
d1_x_agesq               -0.0004         0.0002  0.0243
d1_x_disab               -0.0468         0.0624  0.4526

$RMSE
[1] 0.307

$OIR
[1] "5.45 (p = 0.244)"
```

## References

Cameron, A. Colin, and Pravin K. Trivedi. Microeconometrics: methods and applications. Cambridge university press, 2005.
