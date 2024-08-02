
##################################################################### Start ####

#------------------------------------------------------------ Help Start UI ----
help_start_ui = function(id){
  ns = shiny::NS(id)
  shiny::includeMarkdown("./man/input_formats.md")
}

#-------------------------------------------------------- Help Start server ----

help_start_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

############################################################## Single omics ####

#----------------------------------------------------- Help Single omics UI ----
help_single_omics_ui = function(id){
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Visualization tab",
      shiny::includeMarkdown("./man/single_omics/visualization.md")
    ),
    shiny::tabPanel(
      title = "Functional analysis tab",
      shiny::includeMarkdown("./man/single_omics/functional_analysis.md")
    )
  )

}

#------------------------------------------------- Help Single omics server ----

help_single_omics_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

############################################################### Multi omics ####

#------------------------------------------------------ Help Multi omics UI ----
help_multi_omics_ui = function(id){
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "MOFA - Multi-Omics Factor Analysis",
      shiny::includeMarkdown("./man/multi_omics/mofa.md")
    ),
    shiny::tabPanel(
      title = "SNF - Similarity Network Fusion",
      shiny::includeMarkdown("./man/multi_omics/snf.md")
    )
  )
}

#-------------------------------------------------- Help Multi omics server ----

help_multi_omics_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}


