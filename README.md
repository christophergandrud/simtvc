simtvc is depricated. It's functions have been moved to [simPH](https://github.com/christophergandrud/simPH).
=====

simtvc
======

### Christopher Gandrud

### Version 0.04

### Note: **simtvc** is in beta. Please report any bugs at: <https://github.com/christophergandrud/simtvc/issues>.

---


An R package for simulating and graphing non-proportional hazards (relative hazards, first differences, and hazard ratios) from [Cox Proportional Hazard (PH) models](http://en.wikipedia.org/wiki/Proportional_hazards_models). It can also simulate and graph stratified non-proportional hazard rates from Cox models.

The package includes four functions:

- `tvc`: a function for creating time interactions. Currently supports `'linear'`, natural `'log'`, and exponentiation (`'power'`).

- `coxsimtvc`: a function for simulating time-varying hazards (relative hazards, first differences, and hazard ratios) from a Cox PH model estimated using `coxph` from the [survival](http://cran.r-project.org/web/packages/survival/index.html) package. For more information see this [blog post](http://christophergandrud.blogspot.kr/2012/10/graphing-non-proportional-hazards-in-r.html). If `strata = TRUE` the function will calculate time-varying hazard ratios for multiple strata estimated from a stratified Cox PH model.

- `ggtvc`: uses [ggplot2](http://ggplot2.org/) to graph the simulated relative hazards, first differences, hazard ratios or stratified hazard rates.

- `ggfitStrata`: a function to plot fitted stratified survival curves estimated from `survfit` using **ggplot2**. This function builds on the **survival** package's `plot.survfit` command. One major advantage is the ability to split the survival curves into multiple plots and arrange them in a grid. This makes it easier to examine many strata at once. Otherwise they can be very bunched up.

## Installation

Use the [devtools](https://github.com/hadley/devtools) command `install_github` to install **simtvc** in R. Here is the exact code for installing the most recent stable version:

```r
devtools::install_github("simtvc", "christophergandrud", ref = "v0.04")
```

## Sources

For more information about simulating parameter estimates to make interpretation of results easier see:

Licht, Amanda A. 2011. [“Change Comes with Time: Substantive Interpretation of Nonproportional Hazards in Event History Analysis.”](http://pan.oxfordjournals.org/content/19/2/227.abstract) Political Analysis 19: 227–43.

King, Gary, Michael Tomz, and Jason Wittenberg. 2000. [“Making the Most of Statistical Analyses: Improving Interpretation and Presentation.”](http://www.jstor.org/stable/2669316) American Journal of Political Science 44(2): 347–61.

For more information about stratified Cox PH models (and frailties, which I am working to incorporate in future versions) see:

Box-Steffensmeier, Janet M, and Suzanna De Boef. 2006. [“Repeated Events Survival Models: the Conditional Frailty Model.”](http://onlinelibrary.wiley.com/doi/10.1002/sim.2434/abstract;jsessionid=28218243DD3D6E01A3D10EEE75D96675.d01t02) Statistics in Medicine 25(20): 3518–33.

For an example of how non-proportional hazard results were often presented before `simtvc` see (some of the problems I encountered in this paper were a major part of why I'm developing this package): 

Gandrud, Christopher. 2012. [“The Diffusion of Financial Supervisory Governance Ideas.”](http://www.tandfonline.com/doi/full/10.1080/09692290.2012.727362) Review of International Political Economy.


### Future Plans
This package is in the **early stages** of development. I intend to expand the quantities of interest that can be simulated and graphed for Cox PH models. I am also currently working on functions that can simulate and graph hazard ratios estimated from [Fine and Gray competing risks models](http://www.jstor.org/stable/2670170). 

I am also working on a way to graph hazard ratios with frailties. 
