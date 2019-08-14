
# calback convenience functions
stepEquals <- function(i) paste0("this._currentStep==", i-1, collapse=" || ")

dv <- function(x, quote=TRUE){
  x <- paste0("a[data-value=\"", x, "\"]")
  if(quote) x <- paste0("'", x, "'")
  x
}

rmClass <- function(x) paste0(paste0(
  "$(", dv(x), ").removeClass('active');", collapse="\n"), "\n")

goClass <- function(x){
  if(length(x) > 1) stop("Only add and trigger one class at a time.")
  paste0("$(", dv(x), ").addClass('active');\n$(", dv(x), ").trigger('click');\n")
}

stepcb <- function(condition, action){
  paste0("if (", condition, ") {", paste0(action, collapse="\n"), "}")
}


# tour steps
tour.text <- c(
  "00"="Xmeta is a."
)

tour.pos <- c("right", "left", rep("bottom", 5), rep("left", 3), rep("top", 5), "left", 
              rep("left", 4), rep("top", 3), "left",
              rep("left", 10),
              "right")

tour.element <- c(
  "#tabs"
)

steps <- reactive({
  data.frame(element=tour.element, intro=tour.text, position=tour.pos)
})

# begin tour on button click
observeEvent(input$help, {
  not.db.overview <- c('intro')
  
  tour.options <- list(steps=steps(), 
                       "showBullets"="false", "showProgress"="true", "showStepNumbers"="false")
  tour.events <- list(
    
    "onchange"=I(paste0(
      stepcb(stepEquals(c(1:16)), c(rmClass(not.db.overview), goClass("intro"))),
      collapse="\n"))
  )
  introjs(session, options=tour.options, events=tour.events)
})

