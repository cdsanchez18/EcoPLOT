#Record filtering options that are applied
observeEvent(input$phyloseqfilter, {
  if(is.null(phyloseqobj()))return(NULL)
  if(!is.null(av(input$filter_rank)) && !is.null(av(input$filter_taxa))){
    log_event("Filtered to Taxonomic Rank:", input$filter_rank, ", with specific taxa selections of:", input$filter_taxa,
              name = "Amplicon Data", type = "", status = "")
  }
  if(!is.null(av(input$filter_sample))){
    log_event("Filtered to only include ASV's from the following samples:", input$filter_sample,
              name = "Amplicon Data", type = "", status = "")
  }
  if(!is.null(av(input$filter_sample1)) && samplecounter() >= 1){
    log_event("Filtered", input$filter_sample1, "to only include", input$filter_sample_selection1,
              name = "Amplicon Data", type = "", status = "")
    if(!is.null(av(input$filter_sample2)) && samplecounter() >= 2){
      log_event("Filtered", input$filter_sample2, "to only include", input$filter_sample_selection2,
                name = "Amplicon Data", type = "", status = "")
      if(!is.null(av(input$filter_sample3)) && samplecounter() >= 3){
        log_event("Filtered", input$filter_sample3, "to only include", input$filter_sample_selection3,
                  name = "Amplicon Data", type = "", status = "")
        if(!is.null(av(input$filter_sample4)) && samplecounter() >= 4){
          log_event("Filtered", input$filter_sample4, "to only include", input$filter_sample_selection4,
                    name = "Amplicon Data", type = "", status = "")
          if(!is.null(av(input$filter_sample5)) && samplecounter() >= 5){
            log_event("Filtered", input$filter_sample5, "to only include", input$filter_sample_selection5,
                      name = "Amplicon Data", type = "", status = "")
          }
        }
      }
    }
  }
  if(input$phyloseqmincount > 0){
    log_event("Removed all ASV's with less than", input$phyloseqmincount, "reads",
              name = "Amplicon Data", type = "", status = "")
  }
  if(input$phyloseqminpresence > 0 && input$phyloseqminpresencenumber ==0){
    log_event("Removed ASV's present in less than", input$phyloseqminpresence, "% of samples",
              name = "Amplicon Data", type = "", status = "")
  }
  if(input$phyloseqminpresence ==0 && input$phyloseqminpresencenumber >0){
    log_event("Removed ASV's present in less than", input$phyloseqminpresencenumber, "samples",
              name = "Amplicon Data", type = "", status = "")
  }
})