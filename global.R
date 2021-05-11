##list required packages and install via the proper function if mising
#Cran
required.packages <- c("shiny", "readxl", "treeio", "colourpicker", "ggtree", "shinyjs",
                       "ggplot2", "vegan", "tidyr", "lubridate", "dplyr", "reshape2", "stringr", "shinythemes", #"shinycssloaders",
                       "RColorBrewer", "DT", "htmltools", "dataPreparation", "iRF", "doParallel", "BiocGenerics", "plotly", "randomForest")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

#Biocmanager
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
required.packages.biocmanager <- c("DESeq2", "ggtree", "treeio", "phyloseq")
new.packages.biocmanager <- required.packages.biocmanager[!(required.packages.biocmanager %in% installed.packages()[,"Package"])]
if(length(new.packages.biocmanager)) BiocManager::install(new.packages.biocmanager, dependencies = TRUE)

#Devtools
if(!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")
required.packages.devtools <- c("kalimu/shinyEventLogger", "CDSanchez18/EcoPLOT","jbisanz/qiime2R")
required.packages.devtools2 <- c("shinyEventLogger", "EcoPLOT", "qiime2R")
new.packages.devtools <- required.packages.devtools[!(required.packages.devtools %in% installed.packages()[,"Package"])]
if(length(new.packages.devtools)) devtools::install_github(new.packages.devtools, dependencies = TRUE)

#load packages once installed
for(pkg in c(required.packages, required.packages.biocmanager, required.packages.devtools2)){
  library(pkg, character.only = TRUE)
}