### Add log for iRF 

observeEvent(input$parseIRF,
             log_event("You have set seed at", input$IRFsetseed, name = "IRF Output Var", type = "iRF", status = NULL)
             )
observeEvent(input$parseIRF,
             log_event("You have chosen to put", input$IRFpercentage, "percent of your data into your test dataset,
                       and", (100-input$IRFpercentage), "percent into your training dataset.", name = "IRF Output Var", type = "iRF", status = NULL)
)
observeEvent(input$parseIRF,
    log_event("You have selected", input$IRFyvar, "as your output variable", name = "IRF Output Var", type = "iRF", status = NULL)
)
observeEvent(input$parseIRF,
    log_event("You have chosen to exclude", input$IRFexclude, "from your analysis.", name = "IRF Exclude", type = "iRF", status = NULL)
)
observeEvent(input$performIRF,
             log_event("iRF was run using the following parameters: Depth = ", paste0(input$IRFdepth, ","), "nchild = ", paste0(input$IRFnchild, ","),
                       "ntree =", paste0(input$IRFntree, ","), "nbootstrap =", input$IRFnbootstrap,
                       name = "IRF Result", type = "iRF", status = NULL))
observeEvent(input$performIRF,
             if(input$IRFinteractions == FALSE){
               log_event("iRF did not look for interactions between sample variables", name = "IRF Result", type = "iRF", status = NULL)
             }else if(input$IRFinteractions == TRUE){
                 log_event("iRF looked for interactions between sample variables", name = "IRF Result", type = "iRF", status = NULL)
               }
)
