#store results of plant non-parametric tests
observe({
  req(phenotypedata$table)
  req(input$phenotypenonparametrictesttype)
  if(!is.null(av(input$phenotypenonparametrictesttype))){
    if(input$phenotypenonparametrictesttype == "1WRS"){
      if(!is.null(av(input$phenotypenonparametricvar1))){
        log_event(input$phenotypenonparametricvar1, phenotypenonparametricwork1(),
                  name = "One Sample Wilcoxon Rank-Sum", type = "Non Parametric Test", status = "")
      } 
    }else if(input$phenotypenonparametrictesttype == "2SW"){
      if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar2))){
        log_event(input$phenotypenonparametricvar1, input$phenotypenonparametricvar2, phenotypenonparametricwork1(),
                  name = "Two Sample Wilcoxon Rank-Sum", type = "Non Parametric Test", status = "")
      }
    }else if(input$phenotypenonparametrictesttype == "UWSR"){
      if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar2))){
        log_event(input$phenotypenonparametricvar1, input$phenotypenonparametricvar2, phenotypenonparametricwork1(),
                  name = "Wilcoxon Sign-Rank", type = "Non Parametric Test", status = "")
      }
    }else if(input$phenotypenonparametrictesttype == "PWSR"){
      if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar3))){
        log_event(input$phenotypenonparametricvar1, input$phenotypenonparametricvar3, phenotypenonparametricwork1(),
                  name = "Paired Wilcxon Sign-Rank", type = "Non Parametric Test", status = "")
      }
    }else if(input$phenotypenonparametrictesttype == "kw"){
      if(!is.null(av(input$phenotypenonparametricvar1)) && !is.null(av(input$phenotypenonparametricvar2))){
        log_event(input$phenotypenonparametricvar1, input$phenotypenonparametricvar2, phenotypenonparametricwork1(),
                  name = "Kruskal Wallis", type = "Non Parametric Test", status = "")
      }
    }
  }
})