
#----------------------------------------------------- Main single-omics UI ----
main_single_omics_ui = function(id){
  ns = shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 8,
      render_create_single_omics(ns)
    )
  )
}

#------------------------------------------------- Main single-omics server ----

main_single_omics_server = function(id, main_input, main_output, main_session, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns

      
      # Create experiments
      shiny::observeEvent(input$add_exp,{
        exp_name = input$exp_name
        
        if (exp_name %in% unname(unlist(module_controler$exp_names))) {
          print_t('ERROR: experiment already exists.')
          return()
        }
        
        if (exp_name == '') {
          exp_name = experiment_switch(input$exp_type)
          counter = 1
          while (paste0(exp_name, '_', counter) %in% unname(unlist(module_controler$exp_names))) {
            counter = counter + 1
          }
          exp_name = paste0(exp_name, '_', counter)
        }
        
        if (!grepl("^[a-zA-Z0-9_]+$", exp_name)) {
          print_t('ERROR: only alphanumeric and underscores accepted')
          return()
        }
        
        slot  = names(module_controler$slot_taken)[!sapply(module_controler$slot_taken, base::isTRUE)][1]
        exp_type = input$exp_type
        main_output[[slot]] = shiny::renderUI({
          bs4Dash::bs4SidebarMenuItem(
            text = exp_name,
            tabName = slot,
            icon = shiny::icon("circle")
          )
        })
        
        module_controler$slot_taken[[slot]] = TRUE
        module_controler$exp_names[[slot]] = exp_name
        module_controler$exp_types[[slot]] = exp_type
        module_controler$exp_r6[[slot]] = r6_switch(exp_type = exp_type, name = exp_name, id = paste0('mod_', slot) ,slot = slot)
        
        if (sum(sapply(module_controler$slot_taken, base::isTRUE)) >= 6) {
          shinyjs::disable("add_exp")
        }
        
        print_t(paste0('Added ', input$exp_name, ' (', exp_type, ')'))
        
        
        shiny::updateTextInput(
          inputId = 'exp_name',
          value = character(0)
        )
        
      })
      
      # Change default name when switching experiment
      shiny::observeEvent(input$exp_type,{
        if (input$exp_type == 'Proteomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'prot_1'
          )
        } else if (input$exp_type == 'Transcriptomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'trns_1'
          )
        } else if (input$exp_type == 'Lipidomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'lips_1'
          )
        } else if (input$exp_type == 'Genomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'geno_1'
          )
        } else if (input$exp_type == 'Metabolomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'meta_1'
          )
        }
        
      })
    }
  )
}

