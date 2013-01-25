simtvc
======

## Christopher Gandrud


An R package for simulating and graphing non-proportional hazards ratios from [Cox Proportional Hazard (PH) models](http://en.wikipedia.org/wiki/Proportional_hazards_models).

For more information see this [blog post](http://christophergandrud.blogspot.kr/2012/10/graphing-non-proportional-hazards-in-r.html).

Currently **simtvc** can simulate and graph hazard ratios for *one* variable at a time.


## Installation

Use the [devtools](https://github.com/hadley/devtools) command `install_github` to install **simtvc** in R. Here is the exact code for installing version 0.01.1:

```r
devtools::install_github("simtvc", "christophergandrud", ref = "v0.01.1")
```

### Future Plans
This package is in the **early stages** of development. I intend to expand the quantities of interest that can be simulated and graphed for Cox PH models. I am also currently working on functions that can simulate and graph hazard ratios estimated from [Fine and Gray competing risks models](http://www.jstor.org/stable/2670170).   