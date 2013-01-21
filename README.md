simtvc
======

## Christopher Gandrud


An R package for simulating and graphing non-proportional hazards ratios from [Cox Proportional Hazard (PH) models](http://en.wikipedia.org/wiki/Proportional_hazards_models).

Currently it can simulate and graph hazard ratios *one* variable at a time. Also, only natural log time interactions are allowed.


## Installation

Use the [devtools](https://github.com/hadley/devtools) command `install_github` to install `simtvc` in R. Here is the exact code:

```r
devtools::install_github("simtvc", "christophergandrud")
```

### Future Plans
This package is in the **early stages** of development. I intend to expand the quantities of interest that can be simulated and graphed for Cox PH models. I am also currently working on functions that can simulate and graph hazard ratios estimated from [Fine and Gray competing risks models](http://www.jstor.org/stable/2670170).   