observeEvent(input[["IRF"]], {
  if(input[["IRF"]] == 1 || input[["IRF"]] == 6){
    hideElement(selector = "#IRFsidebar")
    removeCssClass("IRF1", "col-sm-8")
    addCssClass("IRF1", "col-sm-12")
  }else {
    showElement(selector = "#IRFsidebar")
    removeCssClass("IRF1", "col-sm-12")
    addCssClass("IRF1", "col-sm-8")
  }
})
