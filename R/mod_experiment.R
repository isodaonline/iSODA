#------------------------------------------------------------ Experiment UI ----

experiment_ui = function(id) {
  ns = shiny::NS(id)

  shiny::uiOutput(
    outputId = ns('omics_ui')
  )
}

#-------------------------------------------------------- Experiment server ----

experiment_server = function(id, type, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      if (type == 'Lipidomics') {
        lipidomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      } else if (type == 'Proteomics') {
        proteomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      } else if (type == 'Transcriptomics') {
        proteomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      } else if (type == 'Genomics') {
        proteomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      } else if (type == 'Metabolomics') {
        proteomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      }
    }
  )
}




