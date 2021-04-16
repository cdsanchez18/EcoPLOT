#non parametric test options
output$environmentnonparametricUI <- renderUI({
  req(environmentdata$table)
  isolate({
    output <- tagList(
      selectInput("environmentnonparametrictesttype", "Select Non-Parametric Test",
                  choices = c("NULL",
                              "One Sample Wilcoxon Rank-Sum" = "1WRS",
                              "Two-Sample Wilcoxon Rank-Sum (Mann-Whitney U Test)" = "2SW",
                              #"Wilcoxon Sign-Rank (Unpaired Samples)" = "UWSR",
                              "Wilcoxon Sign-Rank (Paired Samples)" = "PWSR",
                              "Kruskal Wallis" = "kw"),
                  selected = "NULL")
      ,
      conditionalPanel(condition = "input.environmentnonparametrictesttype == '1WRS' || 
                     input.environmentnonparametrictesttype == '2SW' || input.environmentnonparametrictesttype == 'UWSR' ||
                     input.environmentnonparametrictesttype == 'PWSR' || input.environmentnonparametrictesttype == 'kw'",
                       selectInput("environmentnonparametricvar1", "Select Continuous Variable:",
                                   choices = c("NULL", names(dplyr::select_if(environmentdata$use, is.numeric))),
                                   selected = "NULL"))
      ,
     # conditionalPanel(condition = "input.environmentnonparametrictesttype == 'UWSR' || input.environmentnonparametrictesttype == 'kw'",
     #                   selectInput("environmentnonparametricvar2", "Select Continuous Variable:",
     #                              choices = c("NULL", names(dplyr::select_if(environmentdata$use, is.numeric))),
     #                               selected = "NULL"))
     # ,
      conditionalPanel(condition = "input.environmentnonparametrictesttype == '2SW' || input.environmentnonparametrictesttype == 'PWSR' ||
                       input.environmentnonparametrictesttype == 'kw'",
                       selectInput("environmentnonparametricvar3", "Select Grouping Variable:",
                                   choices = c("NULL", names(dplyr::select_if(environmentdata$use, is.character)),
                                               names(dplyr::select_if(environmentdata$use, is.factor))),
                                   selected = "NULL"))
      ,
      conditionalPanel(condition = "input.environmentnonparametrictesttype == '1WRS' ||
                     input.environmentnonparametrictesttype == '2SW' || input.environmentnonparametrictesttype == 'UWSR' ||
                     input.environmentnonparametrictesttype == 'PWSR'",
                       numericInput("environmentnonparametrictestmu", "True Value of the Mean (or Difference in Mean if 2 Sample T-Test)",
                                    value = 0),
                       radioButtons("environmentnonparametrictesthypothesis", "Select Alternative Hypothesis",
                                    choices = c("Two Sided" = "two.sided",
                                                "Less" = "less",
                                                "Greater" = "greater"),
                                    selected = "two.sided"))
    )
  })
  return(output)
})
observeEvent(input$environmentactionbutton, {
  req(environmentdata$table)
  updateSelectInput(session, "environmentnonparametricvar3", "Select Grouping Variable:",
                    choices = c("NULL", names(dplyr::select_if(environmentdata$table1, is.character)),
                                names(dplyr::select_if(environmentdata$table1, is.factor))),
                    selected = "NULL")
})
#non parametric tests
environmentnonparametricwork1 <- reactive({
  req(environmentdata$table)
  if(!is.null(av(input$environmentnonparametrictesttype))){
  if(input$environmentnonparametrictesttype == "1WRS"){
    if(!is.null(av(input$environmentnonparametricvar1))){
      tryCatch(wilcox.test(environmentdata$use[[input$environmentnonparametricvar1]], alternative = input$environmentnonparametrictesthypothesis,
                  mu = input$environmentnonparametrictestmu),
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
  }else if(input$environmentnonparametrictesttype == "2SW"){
    if(!is.null(av(input$environmentnonparametricvar1)) && !is.null(av(input$environmentnonparametricvar3))){
      tryCatch(wilcox.test(environmentdata$use[[input$environmentnonparametricvar1]] ~ environmentdata$use[[input$environmentnonparametricvar3]], alternative = input$environmentnonparametrictesthypothesis,
                  mu = input$environmentnonparametrictestmu),
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
  }else if(input$environmentnonparametrictesttype == "PWSR"){
    if(!is.null(av(input$environmentnonparametricvar1)) && !is.null(av(input$environmentnonparametricvar3))){
      tryCatch(wilcox.test(environmentdata$use[[input$environmentnonparametricvar1]] ~ environmentdata$use[[input$environmentnonparametricvar3]], alternative = input$environmentnonparametrictesthypothesis,
                  mu = input$environmentnonparametrictestmu, paired = TRUE),
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
  }else if(input$environmentnonparametrictesttype == "kw"){
    if(!is.null(av(input$environmentnonparametricvar1)) && !is.null(av(input$environmentnonparametricvar3))){
      tryCatch(kruskal.test(environmentdata$use[[input$environmentnonparametricvar1]] ~ environmentdata$use[[input$environmentnonparametricvar3]]),
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
  req(environmentdata$table)
  if(!is.null(av(input$environmentnonparametricvar1))){
    if(input$environmentnonparametrictesttype == "2SW" || input$environmentnonparametrictesttype == "PWSR"){
      if(!is.null(input$environmentnonparametricvar3)){
        if(length(unique(environmentdata$use[[input$environmentnonparametricvar3]])) > 2){
          showNotification("Your Categorical Variable can only have 2 levels to perform this test",
                           type = "warning")
        }
      }
    }
  }else{
    NULL
  }
})
output$environmentnonparametricwork <- renderPrint({
  req(environmentdata$table)
  if(!is.null(environmentnonparametricwork1())){
  environmentnonparametricwork1()
  }else {
    return(
      NULL)
  }
})
output$environmentnonparametricMain <- renderUI({
  validate(
    need(!is.null(environmentdata$table), "Please Upload a Dataset")
  )
  output <- tagList(
    verbatimTextOutput("environmentnonparametricwork")
  )
})