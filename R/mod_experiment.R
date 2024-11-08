#------------------------------------------------------------ Experiment UI ----

experiment_ui = function(id) {
  ns = shiny::NS(id)

  shiny::uiOutput(
    outputId = ns('omics_ui')
  )
}

#-------------------------------------------------------- Experiment server ----

experiment_server = function(id, type, module_controler, isoda_version) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      single_omics_server(
        id = id,
        ns = ns,
        input = input,
        output = output,
        session = session,
        module_controler = module_controler,
        omics_type = type,
        isoda_version = isoda_version)
    }
  )
}




