


set_logging(js_console = FALSE,
            file = FALSE)#paste(paste(format(Sys.Date(), format = "%Y%m%d"), 
                               #format(Sys.time(), format = "%H%M"), sep = "_"), "EcoPLOT.log", sep = ""))


EcoPLOTServer <- shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=100*1024^4,
          shiny.usecairo=FALSE)
  ##Log Data
  set_logging_session()
      #Phenotype
        source("modules/Phenotype/Logs/datasetLOG.R", local = TRUE)
        source("modules/Phenotype/Logs/filterLOG.R", local = TRUE)
        source("modules/Phenotype/Logs/parametricLOG.R", local = TRUE)
        source("modules/Phenotype/Logs/nonparametricLOG.R", local = TRUE)
      #Geochemistry
        source("modules/Geochemistry/Logs/datasetLOG.R", local = TRUE)
        source("modules/Geochemistry/Logs/filterLOG.R", local = TRUE)
        source("modules/Geochemistry/Logs/parametricLOG.R", local = TRUE)
        source("modules/Geochemistry/Logs/nonparametricLOG.R", local = TRUE)
      #Amplicon
        source("modules/Amplicon/Logs/datasetLOG.R", local = TRUE)
        source("modules/Amplicon/Logs/filterLOG.R", local = TRUE)
      #iRF
        source("modules/iRF/Logs/iRFLOG.R", local = TRUE)
  
  ##Beginning of app section
  
  ##Plant Phenotypic Data Server
  source("modules/Phenotype/Sections/upload.R", local = TRUE)
  source("modules/Phenotype/Sections/filter.R", local = TRUE)
  source("modules/Phenotype/Sections/plot.R", local = TRUE)
  source("modules/Phenotype/Sections/parametric.R", local = TRUE)
  source("modules/Phenotype/Sections/nonparametric.R", local = TRUE)
  source("modules/Phenotype/Sections/misc.R", local = TRUE)
  
  ##Geochemistry
  source("modules/Geochemistry/Sections/upload.R", local = TRUE)
  source("modules/Geochemistry/Sections/filter.R", local = TRUE)
  source("modules/Geochemistry/Sections/plot.R", local = TRUE)
  source("modules/Geochemistry/Sections/parametric.R", local = TRUE)
  source("modules/Geochemistry/Sections/nonparametric.R", local = TRUE)
  source("modules/Geochemistry/Sections/soilindexes.R", local = TRUE)
  source("modules/Geochemistry/Sections/misc.R", local = TRUE)
  
  
  ###Amplicon Data
  source("modules/Amplicon/Sections/upload.R", local = TRUE)
  source("modules/Amplicon/Sections/summaries.R", local = TRUE)
  source("modules/Amplicon/Sections/phylotree.R", local = TRUE)
  source("modules/Amplicon/Sections/barplot.R", local = TRUE)
  source("modules/Amplicon/Sections/alphadiv.R", local = TRUE)
  source("modules/Amplicon/Sections/betadiv.R", local = TRUE)
  source("modules/Amplicon/Sections/heatmap.R", local = TRUE)
  source("modules/Amplicon/Sections/filter.R", local = TRUE)
  source("modules/Amplicon/Sections/diffabundance.R", local = TRUE)
  source("modules/Amplicon/Sections/misc.R", local = TRUE)
  
  ###IRF 
  source("modules/IRF/Sections/dataformatting.R", local = TRUE)
  source("modules/IRF/Sections/misc.R", local = TRUE)
  
})



