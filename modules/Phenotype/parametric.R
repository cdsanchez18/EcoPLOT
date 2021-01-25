#parametric test options
output$phenotypeparametricUI <- renderUI({
  req(phenotypedata$table)
  output <- tagList(
    selectInput("phenotypeparametrictesttype", "Select Parametric Test",
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
    conditionalPanel(condition = "input.phenotypeparametrictesttype == '1ttest' ||
                     input.phenotypeparametrictesttype == '2ttest' || input.phenotypeparametrictesttype == 'pttest' ||
                     input.phenotypeparametrictesttype == '1anova' || input.phenotypeparametrictesttype == '2anova' || input.phenotypeparametrictesttype == 'tukeyhsd'",
                     selectInput("phenotypeparametricvar1", "Select Continuous Variable:",
                                 choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.numeric))),
                                 selected = "NULL"))
    ,
    conditionalPanel(condition = "input.phenotypeparametrictesttype == '2ttest' || input.phenotypeparametrictesttype == 'pttest' ||
                     input.phenotypeparametrictesttype == '1anova' || input.phenotypeparametrictesttype == '2anova' || input.phenotypeparametrictesttype == 'tukeyhsd'",
                     selectInput("phenotypeparametricvar2", "Select Categorical Variable:",
                                 choices = c("NULL", names(dplyr::select_if(phenotypedata$table1, is.character)),
                                             names(dplyr::select_if(phenotypedata$use, is.factor))),
                                 selected = "NULL"))
    ,
    #conditionalPanel(condition = "input.phenotypeparametrictesttype == 'pttest'",
    #                 selectInput("phenotypeparametricvar3", "Select Continuous Variable:",
    #                             choices = c("NULL", names(dplyr::select_if(phenotypedata$use, is.numeric))),
    #                             selected = "NULL"))
    #,
    conditionalPanel(condition = "input.phenotypeparametrictesttype == '2anova'",
                     selectInput("phenotypeparametricvar4", "Select Categorical Variable:",
                                 choices = c("NULL", names(dplyr::select_if(phenotypedata$table1, is.character)),
                                             names(dplyr::select_if(phenotypedata$use, is.factor))),
                                 selected = "NULL"))
    ,
    conditionalPanel(condition = "input.phenotypeparametrictesttype == '1ttest' ||
                     input.phenotypeparametrictesttype == '2ttest' || input.phenotypeparametrictesttype == 'pttest'",
                     numericInput("phenotypetestmu", "True Value of the Mean (or Difference in Mean if 2 Sample T-Test)",
                                  value = 0),
                     radioButtons("phenotypettesthypothesis", "Select Alternative Hypothesis",
                                  choices = c("Two Sided" = "two.sided",
                                              "Less" = "less",
                                              "Greater" = "greater"),
                                  selected = "two.sided"))
  )
  return(output)
})
observeEvent(input$phenotypeactionbutton, {
  req(phenotypedata$table)
  updateSelectInput(session, "phenotypeparametricvar2", "Select Categorical Variable:",
                    choices = c("NULL", names(dplyr::select_if(phenotypedata$table1, is.character)),
                                names(dplyr::select_if(phenotypedata$table1, is.factor))),
                    selected = "NULL")
})
observeEvent(input$phenotypeactionbutton, {
  req(phenotypedata$table)
  updateSelectInput(session, "phenotypeparametricvar4", "Select Categorical Variable:",
                    choices = c("NULL", names(dplyr::select_if(phenotypedata$table1, is.character)),
                                names(dplyr::select_if(phenotypedata$table1, is.factor))),
                    selected = "NULL")
})
#parametric tests
phenotypeparametricwork1 <- reactive({
  req(phenotypedata$table)
  if(!is.null(av(input$phenotypeparametrictesttype))){
  if(input$phenotypeparametrictesttype == "1ttest"){
    if(!is.null(av(input$phenotypeparametricvar1))){
      tryCatch(t.test(phenotypedata$use[[input$phenotypeparametricvar1]], alternative = input$phenotypettesthypothesis,
             mu = input$phenotypetestmu),
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
  }else if(input$phenotypeparametrictesttype == "2ttest"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
      tryCatch(t.test(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]],
             alternative = input$phenotypettesthypothesis, mu = input$phenotypetestmu),
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
  }else if(input$phenotypeparametrictesttype == "pttest"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
      tryCatch(t.test(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]],
             alternative = input$phenotypettesthypothesis, paired = TRUE, mu = input$phenotypetestmu),
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
  }else if(input$phenotypeparametrictesttype == "1anova"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
      tryCatch(summary(aov(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]], data = phenotypedata$use)),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }
  }else if(input$phenotypeparametrictesttype == "2anova"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2)) && !is.null(av(input$phenotypeparametricvar4))){
      tryCatch(summary(aov(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]] + phenotypedata$use[[input$phenotypeparametricvar4]],
                  data = phenotypedata$use)),
               error = function(cond){
                 message("Error")
                 return(NULL)
               },
               warning = function(cond){
                 message("Warning")
                 return(NULL)
               })
    }
  }else if(input$phenotypeparametrictesttype == "tukeyhsd"){
    if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
      tryCatch(TukeyHSD(aov(phenotypedata$use[[input$phenotypeparametricvar1]] ~ phenotypedata$use[[input$phenotypeparametricvar2]], data = phenotypedata$use)),
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
  if(!is.null(av(input$phenotypeparametricvar1))){
    if(input$phenotypeparametrictesttype == "2ttest" || input$phenotypeparametrictesttype == "pttest"){
      if(!is.null(input$phenotypeparametricvar2)){
        if(length(unique(phenotypedata$use[[input$phenotypeparametricvar2]])) > 2){
          showNotification("Your Categorical Variable can only have 2 levels to perform this test",
                           type = "warning")
        }
      }
    }
  }else{
    NULL
  }
})
output$phenotypeparametricwork <- renderPrint({
  req(phenotypedata$table)
  if(!is.null(phenotypeparametricwork1())){
  phenotypeparametricwork1()
  } else {
    NULL
  }
})
output$phenotypeparametricMain <- renderUI({
  output <- tagList(
    verbatimTextOutput("phenotypeparametricwork")
  )
})