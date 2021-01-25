source("modules/EcoPLOTuploadUI.R", local = TRUE)
source("modules/Geochemistry/EcoPLOTGeochemUI.R", local = TRUE)
source("modules/Phenotype/EcoPLOTPlantUI.R", local = TRUE)
source("modules/Amplicon/EcoPLOTAmpliconUI.R", local = TRUE)
source("modules/IRF/EcoPLOT_IRF.R", local = TRUE)

EcoPLOTUI <- shinyUI(
  navbarPage(title = "EcoPLOT", theme = shinythemes::shinytheme("yeti"),
             EcoPLOTuploadUI, 
             EcoPLOTGeochemUI,
             EcoPLOTPlantUI,
             EcoPLOTAmpliconUI,
             EcoPLOT_IRF)
)