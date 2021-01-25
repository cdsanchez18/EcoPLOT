#store results of plant parametric tests
observe({
  req(environmentdata$table)
  req(input$environmentparametrictesttype)
  if(!is.null(av(input$environmentparametrictesttype))){
    if(input$environmentparametrictesttype == "1ttest"){
      if(!is.null(av(input$environmentparametricvar1))){
        log_event(input$environmentparametrictesttype, input$environmentparametricvar1, environmentparametricwork1(),
                  name = "One Way T-Test", type = "Parametric Test", status = "")
      } 
    }else if(input$environmentparametrictesttype == "2ttest"){
      if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2))){
        log_event(input$environmentparametrictesttype, input$environmentparametricvar1, input$environmentparametricvar2, environmentparametricwork1(),
                  name = "Two Way T-Test", type = "Parametric Test", status = "")
      }
    }else if(input$environmentparametrictesttype == "pttest"){
      if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar3))){
        log_event(input$environmentparametrictesttype, input$environmentparametricvar1, input$environmentparametricvar3, environmentparametricwork1(),
                  name = "Paired T-Test", type = "Parametric Test", status = "")
      }
    }else if(input$environmentparametrictesttype == "1anova"){
      if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2))){
        log_event(input$environmentparametrictesttype, input$environmentparametricvar1, input$environmentparametricvar2, environmentparametricwork1(),
                  name = "One Way ANOVA", type = "Parametric Test", status = "")
      }
    }else if(input$environmentparametrictesttype == "2anova"){
      if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2)) && !is.null(av(input$phenotypeparametricvar4))){
        log_event(input$environmentparametrictesttype, input$environmentparametricvar1, input$environmentparametricvar2, input$environmentparametricvar4, environmentparametricwork1(),
                  name = "Two Way ANOVA", type = "Parametric Test", status = "")
      }
    }else if(input$environmentparametrictesttype == "tukeyhsd"){
      if(!is.null(av(input$environmentparametricvar1)) && !is.null(av(input$environmentparametricvar2))){
        log_event(input$environmentparametrictesttype, input$environmentparametricvar1, input$environmentparametricvar2, environmentparametricwork1(),
                  name = "Tukey HSD", type = "Parametric Test", status = "")
      }
    }
  }
})