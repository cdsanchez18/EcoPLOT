#store results of plant parametric tests
observe({
  req(phenotypedata$table)
  req(input$phenotypeparametrictesttype)
  if(!is.null(av(input$phenotypeparametrictesttype))){
    if(input$phenotypeparametrictesttype == "1ttest"){
      if(!is.null(av(input$phenotypeparametricvar1))){
        log_event(input$phenotypeparametrictesttype, input$phenotypeparametricvar1, phenotypeparametricwork1(),
                  name = "One Way T-Test", type = "Parametric Test", status = "")
      } 
    }else if(input$phenotypeparametrictesttype == "2ttest"){
      if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
        log_event(input$phenotypeparametrictesttype, input$phenotypeparametricvar1, input$phenotypeparametricvar2, phenotypeparametricwork1(),
                  name = "Two Way T-Test", type = "Parametric Test", status = "")
      }
    }else if(input$phenotypeparametrictesttype == "pttest"){
      if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar3))){
        log_event(input$phenotypeparametrictesttype, input$phenotypeparametricvar1, input$phenotypeparametricvar3, phenotypeparametricwork1(),
                  name = "Paired T-Test", type = "Parametric Test", status = "")
      }
    }else if(input$phenotypeparametrictesttype == "1anova"){
      if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
        log_event(input$phenotypeparametrictesttype, input$phenotypeparametricvar1, input$phenotypeparametricvar2, phenotypeparametricwork1(),
                  name = "One Way ANOVA", type = "Parametric Test", status = "")
      }
    }else if(input$phenotypeparametrictesttype == "2anova"){
      if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2)) && !is.null(av(input$phenotypeparametricvar4))){
        log_event(input$phenotypeparametrictesttype, input$phenotypeparametricvar1, input$phenotypeparametricvar2, input$phenotypeparametricvar4, phenotypeparametricwork1(),
                  name = "Two Way ANOVA", type = "Parametric Test", status = "")
      }
    }else if(input$phenotypeparametrictesttype == "tukeyhsd"){
      if(!is.null(av(input$phenotypeparametricvar1)) && !is.null(av(input$phenotypeparametricvar2))){
        log_event(input$phenotypeparametrictesttype, input$phenotypeparametricvar1, input$phenotypeparametricvar2, phenotypeparametricwork1(),
                  name = "Tukey HSD", type = "Parametric Test", status = "")
      }
    }
  }
})