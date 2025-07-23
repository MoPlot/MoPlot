
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MoPlot <img src="man/figures/logo.png" align="right" height="135" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

MoPlot is an R package designed to visualize linear models that include
categorical predictors. It enhances the interpretability of complex
model outputs through clear and intuitive graphical representations,
with a particular focus on the contrasts used for categorical variables.
By highlighting these contrasts, MoPlot helps users better understand
the specific comparisons underlying model estimates.

## Installation

You can install the development version of MoPlot like so:

``` r
devtools::install_github("Moplot/Moplot")
```

## 1. MoPlotOneWay

`MoPlotOneWay()` is a function for visualizing the results of a linear
model with one categorical predictor and one numeric dependent variable.
It takes as input a model created with `lm()` and produces a plot that
highlights group means with their associated uncertainty using bars, and
visually emphasizes significant contrasts through colored lines.
Optionally, the raw data points can be displayed in the plot. A
secondary panel shows the model coefficients, including standardized
beta values and effect sizes such as Cohen’s *d*, allowing for a clear
and informative summary of the model’s findings. `MoPlotOneWay()`
function supports a variety of contrast coding schemes for categorical
predictors, including treatment coding (dummy coding), sum to zero
coding, Helmert coding, reverse Helmert coding, polynomial coding,
sliding difference coding, and customized coding.

### 1.1 Default Treatment coding

Treatment coding, or dummy coding is a common way to represent
categorical variables in regression models. It transforms a categorical
predictor with k levels into k − 1 binary (0/1) variables, each
comparing one level to a reference (baseline) category. The following
example uses the classic iris dataset with treatment contrasts and shows
the plot without raw data points or coefficient panel, using a
significance level of 0.05:

``` r
library(MoPlot)

# Using Iris data
data(iris)
iris$Species <- factor(iris$Species)
contrasts(iris$Species) <- contr.treatment(3)

# Fit the model
model <- lm(Sepal.Length ~ Species, data = iris)

# MoPlotting
MoPlotOneWay(model, data = FALSE, coef = FALSE, siglevel = 0.05)
#> This graphical representation illustrates the linear model, where
#>  Species is the categorical predictor (represented on the x-axis with its levels),
#>  and Sepal.Length is the numerical dependent variable (plotted on the y-axis).
#>  The contrast type applied is treatment, which compares each group mean against
#>  the baseline mean of setosa (the first level of the predictor).
#>  Blue dots indicate the expected values (means) for each group, while the error bars
#>  represent the uncertainty associated with these estimates.
#>  The dashed purple line marks the baseline expected value.
#>  Green lines highlight significant contrasts, with a significance threshold of 0.05 for the first type of error.
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Here is the same plot, but including raw data points and the coefficient
panel, also with a 0.05 significance level:

``` r
# MoPlotting
MoPlotOneWay(model, data = TRUE, coef = TRUE, siglevel = 0.05)
#> This graphical representation illustrates the linear model, where
#>  Species is the categorical predictor (represented on the x-axis with its levels),
#>  and Sepal.Length is the numerical dependent variable (plotted on the y-axis).
#>  The contrast type applied is treatment, which compares each group mean against
#>  the baseline mean of setosa (the first level of the predictor).
#>  Blue dots indicate the expected values (means) for each group, while the error bars
#>  represent the uncertainty associated with these estimates.
#>  The dashed purple line marks the baseline expected value.
#>  Green lines highlight significant contrasts, with a significance threshold of 0.05 for the first type of error.
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### 1.2 Sum coding

Sum coding is another way to represent categorical variables in
regression models. Unlike treatment coding, sum coding compares each
level to the overall mean rather than a specific reference group. The
following example uses again the iris dataset with sum contrasts,
showing the plot without raw data points or coefficient panel at a
significance level of 0.1

``` r
# Using Sum coding
contrasts(iris$Species) <- contr.sum(3)

# Fit the model
model <- lm(Sepal.Length ~ Species, data = iris)

# MoPlotting
MoPlotOneWay(model, data = FALSE, coef = FALSE, siglevel = 0.1)
#> This graphical representation depicts the linear model, with
#>  Species as the categorical predictor (levels shown on the x-axis),
#>  and Sepal.Length as the numerical dependent variable (values displayed on the y-axis).
#>  The contrast type is sum, where each group mean is compared to
#>  the intercept, which serves as the baseline.
#>  Blue dots represent the expected values (means) for each group, and error bars capture
#>  the uncertainty associated with these estimates.
#>  The dashed purple line marks the intercept expected value.
#>  The green error bars highlight significant contrasts, with 0.1 as the threshold for the first type of error.
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Here is the same plot including raw data points and the coefficient
panel:

``` r
# MoPlotting
MoPlotOneWay(model, data = TRUE, coef = TRUE, siglevel = 0.1)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### 1.3 Helmert coding

Helmert coding is a contrast coding scheme where each level of a
categorical variable is compared to the mean of previous levels. The
following example uses a generated dataset (using `DatasetOne()`) with
Helmert contrasts. It first shows a plot without raw data points or
coefficient panel at a significance level of 0.05:

``` r
# Using a custom function to generate data with four groups
DT <- DatasetOne(n_sample = 20, means = c(120, 135, 115, 120), sds = 20)
DT$Ind <- as.factor(DT$Ind)
contrasts(DT$Ind) <- contr.helmert(4)

# Fit the model
model <- lm(Dep ~ Ind, data = DT)

# MoPlotting sum
MoPlotOneWay(model, data = FALSE, coef = FALSE, siglevel = 0.05)
#> This graphical representation depicts the linear model, with
#>  Ind as the categorical predictor (levels are represented on the x-axis),
#>  and Dep as the numerical dependent variable (values are shown on the y-axis).
#>  The applied contrast type is helmert. In this type of contrast, each group mean is compared to
#>  the cumulative mean of all previous groups.
#>  Blue dots represent the expected values (means) for the cumulative means,
#>  while orange dots represent the expected values (means) for each individual group.
#>  Error bars indicate the uncertainty associated with each expected value.
#>  The dashed purple line marks the baseline expected value.
#>  Green lines highlight significant contrasts, with 0.05 serving as the threshold for the first type of error.
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Here is the same plot including raw data points and the coefficient
panel, using a 0.2 significance level:

``` r
# MoPlotting sum
MoPlotOneWay(model, data = TRUE, coef = TRUE, siglevel = 0.2)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## 2. MoPlotTwoWay

`MoPlotTwoWay()` is a function for visualizing the results of a linear
model with two categorical predictors including their interaction, and
one numeric dependent variable. It takes as input a model created with
`lm()` and produces a plot that highlights marginal means with their
associated uncertainty using error bars. Significant interaction
contrasts are visually emphasized using colored lines. Optionally, a
secondary panel displays the model coefficients, including standardized
beta values and effect sizes such as Cohen’s *d*, providing a clear and
informative summary of the model’s findings.

### 2.1 Helmert coding x Sum Coding

This example shows how to use `MoPlotTwoWay()` with a linear model where
the factors (with interaction) are encoded using **Helmert** and **sum**
contrasts. We first show a plot without coefficient panel at a
significance level of 0.05:

``` r
# Simulating a two way dataset
n <- 200
dt1 <- expand.grid(Ind1 = factor(c("A", "B", "C")), Ind2 = factor(c("X", "Y")))
dt1 <- dt1[rep(1:nrow(dt1), each = n / nrow(dt1)), ]
dt1$y <- with(dt1, 3 + ifelse(Ind1 == "B", 2, ifelse(Ind1 == "C", -1, 0)) +
    ifelse(Ind2 == "Y", 1, 0) + ifelse(Ind1 == "C" & Ind2 == "Y", 2, 0) +
    rnorm(nrow(dt1), 0, 1))

# Applying Helmert and Sum contrasts
contrasts(dt1$Ind1) <- contr.helmert(3)
contrasts(dt1$Ind2) <- contr.sum(2)

# Model fit
modello <- lm(y ~ Ind1 * Ind2, data = dt1)

# Moplotting
MoPlotTwoWay(modello, coef = F, focus = F, sig.level = 0.05)
#> Variabili con contrasti ortogonali: Ind1
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

    #> This graphical representation illustrates the linear model, where
    #>  Ind1, Ind2 are the categorical predictors (represented on the x-axis and with color),
    #>  and y is the numerical dependent variable (plotted on the y-axis).
    #>  The contrast type applied is helmert, sum. 
    #>  Blue dots indicate the expected values (means), while the error bars
    #>  represent the uncertainty associated with these estimates.
    #>  Green color highlights significant interactions, with a significance threshold of 0.05 for the first type of error.

We then show a plot with the coefficient panel at a significance level
of 0.05:

``` r
# Moplotting
MoPlotTwoWay(modello, coef = T, focus = F, sig.level = 0.05)
#> Variabili con contrasti ortogonali: Ind1
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

    #> This graphical representation illustrates the linear model, where
    #>  Ind1, Ind2 are the categorical predictors (represented on the x-axis and with color),
    #>  and y is the numerical dependent variable (plotted on the y-axis).
    #>  The contrast type applied is helmert, sum. 
    #>  Blue dots indicate the expected values (means), while the error bars
    #>  represent the uncertainty associated with these estimates.
    #>  Green color highlights significant interactions, with a significance threshold of 0.05 for the first type of error.

### 2.2 Custom Coding

This example demonstrates how `MoPlotTwoWay()` can handle custom
contrast matrices, allowing for flexible hypothesis testing in factorial
designs. In this case, `Ind1` uses sum coding while `Ind2` uses a
user-defined custom contrast matrix to explore non-standard comparisons
between group levels.

We visualize single contrasts one by one using the option `Focus`.

``` r
# Generating a dataset
n <- 240
dt1 <- expand.grid(Ind1 = factor(c("A", "B", "C")),
                    Ind2 = factor(c("W", "X", "Y", "Z")))
dt1 <- dt1[rep(1:nrow(dt1), each = n / nrow(dt1)), ]
dt1$y <- with(dt1,5 +ifelse(Ind1 == "B", 2, ifelse(Ind1 == "C", -1, 0)) +
                ifelse(Ind2 == "X", 1, ifelse(Ind2 == "Y", 2, 
                                              ifelse(Ind2 == "Z", -2, 0))) +
                 ifelse(Ind1 == "C" & Ind2 == "Y", 3, 0) +
                 rnorm(nrow(dt1), 0, 1))

# Applying Sum and Custom contrasts
contrasts(dt1$Ind1) <- contr.sum(3)
custom_Ind2 <- matrix(c(1,  1, -1, -1, # W,X vs Y,Z
                      1, -1,  0,  0,   # arbitrary 
                      0,  0,  1, -1),  # arbitrary 
                    ncol = 3)
rownames(custom_Ind2) <- c("W", "X", "Y", "Z")
colnames(custom_Ind2) <- c("WX_vs_YZ", "W_vs_X", "Y_vs_Z")
contrasts(dt1$Ind2) <- custom_Ind2

# Model fitting
model_custom <- lm(y ~ Ind2 * Ind1, data = dt1)

# MoPlotting
MoPlotTwoWay(model_custom, focus=T)
#> Variabili con contrasti ortogonali: Ind2
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" /><img src="man/figures/README-unnamed-chunk-10-3.png" width="100%" /><img src="man/figures/README-unnamed-chunk-10-4.png" width="100%" /><img src="man/figures/README-unnamed-chunk-10-5.png" width="100%" /><img src="man/figures/README-unnamed-chunk-10-6.png" width="100%" />

    #> This graphical representation illustrates the linear model, where
    #>  Ind2, Ind1 are the categorical predictors (represented on the x-axis and with color),
    #>  and y is the numerical dependent variable (plotted on the y-axis).
    #>  The contrast type applied is personalizzato, sum. 
    #>  Blue dots indicate the expected values (means), while the error bars
    #>  represent the uncertainty associated with these estimates.
    #>  Green color highlights significant interactions, with a significance threshold of 0.05 for the first type of error.
