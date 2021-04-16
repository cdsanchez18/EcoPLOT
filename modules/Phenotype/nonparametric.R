
#non parametric test options
output$phenotypenonparametricUI <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    selectInput("phenotypenonparametrictesttype", "Select Non-Parametric Test",
                choices = c("NULL",
                            "One Sample Wilcoxon Rank-Sum" = "1WRS",
                            "Two-Sample Wilcoxon Rank-Sum (Mann-Whitney U Test)" = "2SW",
                            #"Wilcoxon Sign-Rank (Unpaired Samples)" = "UWSR",
                            "Wilcoxon Sign-Rank (Paired Samples)" = "PWSR",
                            "Kruskal Wallis" = "kw"),
                selected = "NULL")
    ,
    conditionalPanel(condition = "input.phenotypenonparametrictesttype == '1WRS' || 
                     input.phenotypenonparametrictesttype == '2SW' || input.phenotypenonparametrictesttype == 'UWSR' ||
                     input.phenotypenonparametrictesttype == 'PWSR' || input.phenotypenonparametrictesttype == 'kw'",
                     selectInput("phenotypenonparametricvar1", "Select Continuous Variable:",
                                 choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.numeric))),
                                 selected = "NULL"))
    ,
    # conditionalPanel(condition = "input.phenotypenonparametrictesttype == 'UWSR' || input.phenotypenonparametrictesttype == 'kw'",
    #                  selectInput("phenotypenonparametricvar2", "Select Continuous Variable:",
    #                              choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.numeric))),
    #                              selected = "NULL"))
    # ,
    conditionalPanel(condition = "input.phenotypenonparametrictesttype == '2SW' || input.phenotypenonparametrictesttype == 'PWSR' ||
                       input.environmentnonparametrictesttype == 'kw'",
                     selectInput("phenotypenonparametricvar3", "Select Grouping Variable:",
                                 choices = c("NULL", names(dplyr::select_if(phenotypedata$table1, is.character)),
                                             names(dplyr::select_if(phenotypedata$use, is.factor))),
                                 selected = "NULL"))
    ,
    conditionalPanel(condition = "input.phenotypenonparametrictesttype == '1WRS' ||
                     input.phenotypenonparametrictesttype == '2SW' || input.phenotypenonparametrictesttype == 'UWSR' ||
                     input.phenotypenonparametrictesttype == 'PWSR'",
                     numericInput("phenotypenonparametrictestmu", "True Value of the Mean (or Difference in Mean if 2 Sample T-Test)",
                                  value = 0),
                     radioButtons("phenotypenonparametrictesthypothesis", "Select Alternative Hypothesis",
                                  choices = c("Two Sided" = "two.sided",
                                              "Less" = "less",
                                              "Greater" = "greater"),
                                  selected = "two.sided"))
  )
  return(output)
})
observeEvent(input$phenotypeactionbutton, {
  req(phenotypedata$table)
  updateSelectInput(session, "phenotypenonparametricvar3", "Select Grouping Variable:",
                    choices = c("NULL", names(dplyr::select_if(phenotypedata$table1, is.character)),
                                names(dplyr::select_if(phenotypedata$table1, is.factor))),
                    selected = "NULL")
})

#non parametric tests
phenotypenonparametricwork1 <- reactive({
  req(phenotypedata$table)
  if(!is.null(av(input$phenotypenonparametrictesttype))){
  if(input$phenotypenonparametrictesttype == "1WRS"){
    if(!is.null(av(input$phenotypenonparametricvar1))){
      tryCatch(wilcox.test(phenotypedata$use[[input$phenotypenonparametricvar1]], alternative = input$phenotypenonparametrictesthypothesis,
                  mu = input$phenotypenonparametrictestmu),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }else{
      NULL
    }
  }else if(input$phenotypenonparametrictesttype == "2SW"){
    if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar3))){
      tryCatch(wilcox.test(phenotypedata$use[[input$phenotypenonparametricvar1]] ~ phenotypedata$use[[input$phenotypenonparametricvar3]], alternative = input$phenotypenonparametrictesthypothesis,
                  mu = input$phenotypenonparametrictestmu),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }else{
      NULL
    }
  # }else if(input$phenotypenonparametrictesttype == "UWSR"){
  #   if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar3))){
  #     tryCatch(wilcox.test(phenotypedata$use[[input$phenotypenonparametricvar1]] ~ phenotypedata$use[[input$phenotypenonparametricvar3]], alternative = input$phenotypenonparametrictesthypothesis,
  #                 mu = input$phenotypenonparametrictestmu, paired = TRUE),
  #              error = function(cond){
  #                message("Error")
  #                return(NULL)
  #              },
  #              warning = function(cond){
  #                message("Warning")
  #                return(NULL)
  #              })
  #   }else{
  #     NULL
  #   }
  }else if(input$phenotypenonparametrictesttype == "PWSR"){
    if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar3))){
      tryCatch(wilcox.test(phenotypedata$use[[input$phenotypenonparametricvar1]] ~ phenotypedata$use[[input$phenotypenonparametricvar3]], alternative = input$phenotypenonparametrictesthypothesis,
                  mu = input$phenotypenonparametrictestmu, paired = TRUE),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }else{
      NULL
    }
  }else if(input$phenotypenonparametrictesttype == "kw"){
    if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar3))){
      tryCatch(kruskal.test(phenotypedata$use[[input$phenotypenonparametricvar1]] ~ phenotypedata$use[[input$phenotypenonparametricvar3]]),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }
  }
  }
})
observe({
  req(phenotypedata$table)
  if(!is.null(av(input$phenotypenonparametricvar1))){
    if(input$phenotypenonparametrictesttype == "2SW" || input$phenotypenonparametrictesttype == "PWSR"){
      if(!is.null(input$phenotypenonparametricvar3)){
        if(length(unique(phenotypedata$use[[input$phenotypenonparametricvar3]])) > 2){
          showNotification("Your Categorical Variable can only have 2 levels to perform this test",
                           type = "warning")
        }
      }
    }
  }else{
    NULL
  }
})
output$phenotypenonparametricwork <- renderPrint({
  req(phenotypedata$table)
  if(!is.null(phenotypenonparametricwork1())){
  phenotypenonparametricwork1()
  }else {
    NULL
  }
})
output$phenotypenonparametricMain <- renderUI({
  validate(
    need(!is.null(phenotypedata$table), "Please Upload a Dataset")
  )
  output <- tagList(
    verbatimTextOutput("phenotypenonparametricwork")
  )
})