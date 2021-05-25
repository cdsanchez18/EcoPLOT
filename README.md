---
output: 
  html_document: 
    keep_md: yes 
---
# EcoPLOT

[EcoPLOT](https://github.com/cdsanchez18/EcoPLOT) 
is an interactive web-application for the analysis of multivariate-biogeochemical datasets. EcoPLOT provides an interactive user interface to enable the exploration of environmental, plant-phenotypic, and microbiome datasets. 

### Launching EcoPLOT

EcoPLOT can be run on local devices. Simply clone the app folder from [EcoPLOT's Github](https://github.com/cdsanchez18/EcoPLOT). 
NOTE: Running EcoPLOT locally requires the most [up to date version of R](https://cran.r-project.org/) to be installed on your local device. This will also require a stable internet connection. 

The following code can be used to launch EcoPLOT. 


```r
install.packages("shiny") 
shiny::runGitHub("EcoPLOT","CDSanchez18")
```

Upon startup, EcoPLOT will download, install, and load any required packages that are missing from the system's package library. 

## The Data
Example microbiome data is provided by [Dr. Esther Singer](https://www.researchgate.net/profile/Esther-Singer). Example environmental and plant phenotypic datasets have been simulated for the purpose of demonstration. 
