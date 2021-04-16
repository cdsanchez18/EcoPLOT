#parametric test options
output$environmentparametricUI <- renderUI({
  req(environmentdata$table)
  isolate({
    output <- tagList(
      selectInput("environmentparametrictesttype", "Select Parametric Test",
                  choices = list(
                    "Tests" = c("NULL",
                                "One Sample T-Test" = "1ttest",
                                "Two Sample T-Test" = "2ttest",
                                "Paired T-Test" = "pttest",
                                "One-Way ANOVA" = "1anova",
                                "Two-Way ANOVA" = "2anova"),
                    "Post-Hoc Tests" = c("Tukey" = "tukeyhsd")),
                  selected = "NULL")
      ,
      conditionalPanel(condition = "input.environmentparametrictesttype == '1ttest' ||
                     input.environmentparametrictesttype == '2ttest' || input.environmentparametrictesttype == 'pttest' ||
                     input.environmentparametrictesttype == '1anova' || input.environmentparametrictesttype == '2anova' || input.environmentparametrictesttype == 'tukeyhsd'",
                       selectInput("environmentparametricvar1", "Select Continuous Variable:",
                                   choices = c("NULL", names(dplyr::select_if(environmentdata$use, is.numeric))),
                                   selected = "NULL"))
      ,
      conditionalPanel(condition = "input.environmentparametrictesttype == '2ttest' || input.environmentparametrictesttype == 'pttest' ||
                     input.environmentparametrictesttype == '1anova' || input.environmentparametrictesttype == '2anova' || input.environmentparametrictesttype == 'tukeyhsd'",
                       selectInput("environmentparametricvar2", "Select Categorical Variable:",
                                   choices = c("NULL", names(dplyr::select_if(environmentdata$table, is.character)),
                                               names(dplyr::select_if(environmentdata$use, is.factor))),
                                   selected = "NULL"))
      ,
      # conditionalPanel(condition = "input.environmentparametrictesttype == 'pttest'",
      #                  selectInput("environmentparametricvar3", "Select Continuous Variable:",
      #                              choices = c("NULL", names(dplyr::select_if(environmentdata$use, is.numeric))),
      #                              selected = "NULL"))
      # ,
      conditionalPanel(condition = "input.environmentparametrictesttype == '2anova'",
                       selectInput("environmentparametricvar4", "Select Categorical Variable:",
                                   choices = c("NULL", names(dplyr::select_if(environmentdata$use, is.character)),
                                               names(dplyr::select_if(environmentdata$use, is.factor))),
                                   selected = "NULL"))
      ,
      conditionalPanel(condition = "input.environmentparametrictesttype == '1ttest' ||
                     input.environmentparametrictesttype == '2ttest' || input.environmentparametrictesttype == 'pttest'",
                       numericInput("environmenttestmu", "True Value of the Mean (or Difference in Mean if 2 Sample T-Test)",
                                    value = 0),
                       radioButtons("environmentttesthypothesis", "Select Alternative Hypothesis",
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
  updateSelectInput(session, "environmentparametricvar2", "Select Categorical Variable:",
                    choices = c("NULL", names(dplyr::select_if(environmentdata$table1, is.character)),
                                names(dplyr::select_if(environmentdata$use, is.factor))),
                    selected = "NULL")
})
observeEvent(input$environmentactionbutton, {
  req(environmentdata$table)
  updateSelectInput(session, "environmentparametricvar4", "Select Categorical Variable:",
                    choices = c("NULL", names(dplyr::select_if(environmentdata$table1, is.character)),
                                names(dplyr::select_if(environmentdata$use, is.factor))),
                    selected = "NULL")
})
#parametric tests
environmentparametricwork1 <- reactive({
  req(environmentdata$table)
  if(!is.null(av(input$environmentparametrictesttype))){
  if(input$environmentparametrictesttype == "1ttest"){
    if(!is.null(av(input$environmentparametricvar1))){
      tryCatch(t.test(environmentdata$use[[input$environmentparametricvar1]], alternative = input$environmentttesthypothesis,
             mu = input$environmenttestmu),
             error = function(cond){
               message("Error")
               return(NULL)
             },
             warning = function(cond){
               message("Warning")
               return(NULL)
             })
    }else {
      NULL
    }
  }else if(input$environmentparametrictesttype == "2ttest"){
    if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2))){
      tryCatch(t.test(environmentdata$use[[input$environmentparametricvar1]] ~ environmentdata$use[[input$environmentparametricvar2]],
             alternative = input$environmentttesthypothesis, mu = input$environmenttestmu),
             error = function(cond){
               message("Error")
               return(NULL)
             },
             warning = function(cond){
               message("Warning")
               return(NULL)
             })
    }else {
      NULL
    }
  }else if(input$environmentparametrictesttype == "pttest"){
    if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2))){
      tryCatch(t.test(environmentdata$use[[input$environmentparametricvar1]] ~ environmentdata$use[[input$environmentparametricvar2]],
                      alternative = input$environmentttesthypothesis, mu = input$environmenttestmu, 
                      paired = TRUE, data = environmentdata$use),
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
  }else if(input$environmentparametrictesttype == "1anova"){
    if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2))){
      tryCatch(summary(aov(environmentdata$use[[input$environmentparametricvar1]] ~ environmentdata$use[[input$environmentparametricvar2]], data = environmentdata$use)),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }
  }else if(input$environmentparametrictesttype == "2anova"){
    if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2)) && !is.null(av(input$environmentparametricvar4))){
      tryCatch(summary(aov(environmentdata$use[[input$environmentparametricvar1]] ~ environmentdata$use[[input$environmentparametricvar2]] + environmentdata$use[[input$environmentparametricvar4]],
                  data = environmentdata$use)),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }
  }else if(input$environmentparametrictesttype == "tukeyhsd"){
    if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2))){
      tryCatch(TukeyHSD(aov(environmentdata$use[[input$environmentparametricvar1]] ~ environmentdata$use[[input$environmentparametricvar2]], data = environmentdata$use)),
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
  if(!is.null(av(input$environmentparametricvar1))){
    if(input$environmentparametrictesttype == "2ttest" || input$environmentparametrictesttype == "pttest"){
      if(!is.null(input$environmentparametricvar2)){
        if(length(unique(environmentdata$use[[input$environmentparametricvar2]])) > 2){
          showNotification("Your Categorical Variable can only have 2 levels to perform a this test",
                         type = "warning")
        }
      }
    }
  }else{
    NULL
  }
})
output$environmentparametricwork <- renderPrint({
  req(environmentdata$table)
  if(!is.null(environmentparametricwork1())){
  environmentparametricwork1()
  } else {
    NULL
  }
})
output$environmentparametricMain <- renderUI({
  validate(
    need(!is.null(environmentdata$table), "Please Upload a Dataset")
  )
  output <- tagList(
    verbatimTextOutput("environmentparametricwork")
  )
})