
#----------------------------------------------------------------- About UI ----
about_ui = function(id){
  ns = shiny::NS(id)
  shiny::fluidRow(
    shiny::includeMarkdown("./man/about.md")
  )
}

#----------------------------------------------------------- About server ----

about_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

