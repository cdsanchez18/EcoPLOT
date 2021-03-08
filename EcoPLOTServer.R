


set_logging(js_console = FALSE,
            file = FALSE)#paste(paste(format(Sys.Date(), format = "%Y%m%d"), 
                               #format(Sys.time(), format = "%H%M"), sep = "_"), "EcoPLOT.log", sep = ""))


EcoPLOTServer <- shinyServer(function(input,output,session){
  options(shiny.maxRequestSize=100*1024^4,
          shiny.usecairo=FALSE)
  ##Log Data
  set_logging_session()
      #Phenotype
        source("modules/Phenotype/datasetLOG.R", local = TRUE)
        source("modules/Phenotype/filterLOG.R", local = TRUE)
        source("modules/Phenotype/parametricLOG.R", local = TRUE)
        source("modules/Phenotype/nonparametricLOG.R", local = TRUE)
      #Geochemistry
        source("modules/Geochemistry/datasetLOG.R", local = TRUE)
        source("modules/Geochemistry/filterLOG.R", local = TRUE)
        source("modules/Geochemistry/parametricLOG.R", local = TRUE)
        source("modules/Geochemistry/nonparametricLOG.R", local = TRUE)
      #Amplicon
        source("modules/Amplicon/datasetLOG.R", local = TRUE)
        source("modules/Amplicon/filterLOG.R", local = TRUE)
      #iRF
        source("modules/iRF/iRFLOG.R", local = TRUE)
  
  ##Beginning of app section
  
  ##Plant Phenotypic Data Server
  source("modules/Phenotype/upload.R", local = TRUE)
  source("modules/Phenotype/filter.R", local = TRUE)
  source("modules/Phenotype/plot.R", local = TRUE)
  source("modules/Phenotype/parametric.R", local = TRUE)
  source("modules/Phenotype/nonparametric.R", local = TRUE)
  source("modules/Phenotype/misc.R", local = TRUE)
  
  ##Geochemistry
  source("modules/Geochemistry/upload.R", local = TRUE)
  source("modules/Geochemistry/filter.R", local = TRUE)
  source("modules/Geochemistry/plot.R", local = TRUE)
  source("modules/Geochemistry/parametric.R", local = TRUE)
  source("modules/Geochemistry/nonparametric.R", local = TRUE)
  source("modules/Geochemistry/soilindexes.R", local = TRUE)
  source("modules/Geochemistry/misc.R", local = TRUE)
  
  
  ###Amplicon Data
  source("modules/Amplicon/upload.R", local = TRUE)
  source("modules/Amplicon/summaries.R", local = TRUE)
  source("modules/Amplicon/phylotree.R", local = TRUE)
  source("modules/Amplicon/barplot.R", local = TRUE)
  source("modules/Amplicon/alphadiv.R", local = TRUE)
  source("modules/Amplicon/betadiv.R", local = TRUE)
  source("modules/Amplicon/heatmap.R", local = TRUE)
  source("modules/Amplicon/filter.R", local = TRUE)
  source("modules/Amplicon/diffabundance.R", local = TRUE)
  source("modules/Amplicon/misc.R", local = TRUE)
  
  ###IRF 
  source("modules/IRF/dataformatting.R", local = TRUE)
  source("modules/IRF/misc.R", local = TRUE)
  
})



