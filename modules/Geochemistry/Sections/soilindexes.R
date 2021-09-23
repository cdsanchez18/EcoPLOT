
##outputs unique to geochemistry tab
output$environmenttable <- renderDataTable({
  req(environmentdata$table)
  ##check implementation of functions created in nov2020_functions
  cmatch <- c("totalc", "total_c", "totalcarbon", "total_carbon", "toc", "tc")
  nmatch <- c("totalnitrogen","nitrogen","nitrate","nitrite")
  pmatch <- c("phosphorus", "totalphosphorus", "total_p", "tp")
  env_names <- names(environmentdata$use)
  pmatches <- grep(paste(pmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  nmatches <- grep(paste(nmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  cmatches <- grep(paste(cmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  environmentdata$use %>% select(all_of(nmatches), all_of(pmatches), all_of(cmatches))
})
output$CNP <- renderText({
  req(environmentdata$table)
  cmatch <- c("totalc", "total_c", "totalcarbon", "total_carbon", "toc", "tc")
  nmatch <- c("totalnitrogen","nitrogen","nitrate","nitrite", "total_nitrogen", "total_n")
  pmatch <- c("phosphorus", "totalphosphorus", "total_p", "tp")
  env_names <- names(environmentdata$use)
  pmatches <- grep(paste(pmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  nmatches <- grep(paste(nmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  cmatches <- grep(paste(cmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  environmentdata$use %>% select(all_of(nmatches), all_of(pmatches), all_of(cmatches))
  
  minimum <- min(c(environmentdata$use[, cmatches], environmentdata$use[, nmatches], environmentdata$use[,pmatches]))
  A <- ceiling(mean(environmentdata$use[, cmatches])/minimum)
  B <- ceiling(mean(environmentdata$use[, nmatches])/minimum)
  C <- ceiling(mean(environmentdata$use[, pmatches])/minimum)
  
  paste(A, B, C, sep = ":")
})
output$NPK <- renderText({
  req(environmentdata$table)
  cmatch <- c("totalc", "total_c", "totalcarbon", "total_carbon", "toc", "tc")
  nmatch <- c("totalnitrogen","nitrogen","nitrate","nitrite", "total_nitrogen", "total_n")
  kmatch <- c("potassium", "totalpotassium", "total_k", "tk", "total_potassium")
  env_names <- names(environmentdata$use)
  kmatches <- grep(paste(kmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  nmatches <- grep(paste(nmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  cmatches <- grep(paste(cmatch,collapse="|"), 
                   env_names, value= FALSE, ignore.case = TRUE)
  environmentdata$use %>% select(all_of(nmatches), all_of(kmatches), all_of(cmatches))
  
  minimum <- min(c(environmentdata$use[, cmatches], environmentdata$use[, nmatches], environmentdata$use[,kmatches]))
  A <- ceiling(mean(environmentdata$use[, cmatches])/minimum)
  B <- ceiling(mean(environmentdata$use[, nmatches])/minimum)
  C <- ceiling(mean(environmentdata$use[, kmatches])/minimum)
  
  paste(A, B, C, sep = ":")
})
output$ssc <- renderTable({
  req(environmentdata$table)
  env_names <- names(environmentdata$use)
  sand <- grep(paste("sand",collapse="|"), 
               env_names, value= FALSE, ignore.case = TRUE)
  silt <- grep(paste("silt",collapse="|"), 
               env_names, value= FALSE, ignore.case = TRUE)
  clay <- grep(paste("clay",collapse="|"), 
               env_names, value= FALSE, ignore.case = TRUE)
  
  sand_avg <- mean(environmentdata$use[, sand])
  silt_avg <- mean(environmentdata$use[, silt])
  clay_avg <- mean(environmentdata$use[, clay])
  ssc <- data.frame(name = c("Sand", "Silt", "Clay"), 
                    grain_size = c(sand_avg, silt_avg, clay_avg))
  ssc
})

output$texturetriangle <- renderUI({
  includeHTML("www/sand_silt_clay_Test_indexNOV16.html")
})



output$soilindexuioutput <- renderUI({
  #req(environmentdata$table)
  validate(
    need(!is.null(environmentdata$table), "Please Upload a Dataset")
  )
  
  output <- tagList(
  fluidRow(
    column(width = 12,
           splitLayout(dataTableOutput("environmenttable")))),
  fluidRow(
    column(width = 6,
           h5("This is your C to N to P Ratio:"),
           wellPanel(textOutput("CNP"))),
    column(width = 6,
           h5("This is your N to P to K Ratio:"),
           wellPanel(textOutput("NPK")))),
  fluidRow(
    column(width = 6, offset = 5,
           h5("Sand, Silt, Clay Particle Sizes"),
           splitLayout(tableOutput("ssc"))))
  )
  return(output)
})





