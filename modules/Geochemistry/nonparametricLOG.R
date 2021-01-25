#store results of plant non-parametric tests
observe({
  req(environmentdata$table)
  req(input$environmentnonparametrictesttype)
  if(!is.null(av(input$environmentnonparametrictesttype))){
    if(input$environmentnonparametrictesttype == "1WRS"){
      if(!is.null(av(input$environmentnonparametricvar1))){
        log_event(input$environmentnonparametricvar1, environmentnonparametricwork1(),
                  name = "One Sample Wilcoxon Rank-Sum", type = "Non Parametric Test", status = "")
      } 
    }else if(input$environmentnonparametrictesttype == "2SW"){
      if(!is.null(av(input$environmentnonparametricvar1)) && !is.null(av(input$environmentnonparametricvar2))){
        log_event(input$environmentnonparametricvar1, input$environmentnonparametricvar2, environmentnonparametricwork1(),
                  name = "Two Sample Wilcoxon Rank-Sum", type = "Non Parametric Test", status = "")
      }
    }else if(input$environmentnonparametrictesttype == "UWSR"){
      if(!is.null(av(input$environmentnonparametricvar1)) && !is.null(av(input$environmentnonparametricvar2))){
        log_event(input$environmentnonparametricvar1, input$environmentnonparametricvar2, environmentnonparametricwork1(),
                  name = "Wilcoxon Sign-Rank", type = "Non Parametric Test", status = "")
      }
    }else if(input$environmentnonparametrictesttype == "PWSR"){
      if(!is.null(av(input$environmentnonparametricvar1)) && !is.null(av(input$environmentnonparametricvar3))){
        log_event(input$environmentnonparametricvar1, input$environmentnonparametricvar3, environmentnonparametricwork1(),
                  name = "Paired Wilcxon Sign-Rank", type = "Non Parametric Test", status = "")
      }
    }else if(input$environmentnonparametrictesttype == "kw"){
      if(!is.null(av(input$environmentnonparametricvar1)) && !is.null(av(input$environmentnonparametricvar2))){
        log_event(input$environmentnonparametricvar1, input$environmentnonparametricvar2, environmentnonparametricwork1(),
                  name = "Kruskal Wallis", type = "Non Parametric Test", status = "")
      }
    }
  }
})