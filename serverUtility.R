colorTreeTip = function(tree,metadata,var) {
  t<-tree %<+% metadata + geom_tippoint(aes_string(color=var),size=5, alpha=0.35) + theme(legend.position="right")

  if(var %in% c("Country")){
    t<-t + scale_color_manual(values=countryCol)
  }
  
  t
}



# Radio button customiazation code taken from: http://stackoverflow.com/questions/35783446/can-you-have-an-image-as-a-radiobutton-choice-in-shiny
radioButtons_withHTML <- function (inputId, label, choices, selected = NULL, inline = FALSE, 
                                   width = NULL) 
{
  choices <- shiny:::choicesWithNames(choices)
  selected <- if (is.null(selected)) 
    choices[[1]]
  else {
    shiny:::validateSelected(selected, choices, inputId)
  }
  if (length(selected) > 1) 
    stop("The 'selected' argument must be of length 1")
  options <- generateOptions_withHTML(inputId, choices, selected, inline, 
                                      type = "radio")
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) 
    divClass <- paste(divClass, "shiny-input-container-inline")
  tags$div(id = inputId, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), class = divClass, 
    shiny:::controlLabel(inputId, label), options)
}

generateOptions_withHTML <- function (inputId, choices, selected, inline, type = "checkbox") 
{
  options <- mapply(choices, names(choices), FUN = function(value, 
                                                            name) {
    inputTag <- tags$input(type = type, name = inputId, value = value)
    if (value %in% selected) 
      inputTag$attribs$checked <- "checked"
    if (inline) {
      tags$label(class = paste0(type, "-inline"), inputTag, 
                 tags$span(HTML(name)))
    }
    else {
      tags$div(class = type, tags$label(inputTag, tags$span(HTML(name))))
    }
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(class = "shiny-options-group", options)
}