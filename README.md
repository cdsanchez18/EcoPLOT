# EcoPLOT

[EcoPLOT](https://github.com/cdsanchez18/EcoPLOT) 
is an interactive web-application for the analysis of multivariate-biogeochemical datasets. EcoPLOT provides an interactive user interface to enable the exploration of environmental, plant-phenotypic, and microbiome datasets. 

### Launching EcoPLOT

EcoPLOT can be run on local devices. Simply clone the app repository from [EcoPLOT's Github](https://github.com/cdsanchez18/EcoPLOT). To run the app, open the *app.r* file and select *run app* from top bar. 
NOTE: Running EcoPLOT locally requires the most [up to date version of R](https://cran.r-project.org/) to be installed on your local device as well as a number of R pacakges. This will also require a stable internet connection. 

The following code will launch EcoPLOT on your local system. After installing the first two required pacakges (shiny, devtools), EcoPLOT will automatically download, install, and load any additional required packages that are not currently installed on the user's package library upon launch.

```{r, eval=FALSE}
install.packages(c("shiny", "devtools"))
library(shiny)
library(devtools)
shiny::runGitHub("EcoPLOT","CDSanchez18", ref = "master")
```



## The Data
Example microbiome data is provided by [Dr. Esther Singer](https://orcid.org/0000-0002-3126-2199). Example environmental and plant phenotypic datasets have been simulated for the purpose of demonstration.  

