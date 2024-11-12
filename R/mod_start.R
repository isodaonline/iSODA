
#----------------------------------------------------------------- Start UI ----
start_ui = function(id){
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h1('iSODA - integrated Simple Omics Data Analysis')
      ),
      shiny::column(
        width = 1,
      ),
      shiny::column(
        width = 5,
        shiny::h3('Example datasets'),
      )

    ),
    shiny::fluidRow(
      shiny::column(
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        width = 6,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectInput(
              inputId = ns('exp_type'),
              label = 'Omics type',
              choices = c('Lipidomics', 'Metabolomics', 'Proteomics', 'Transcriptomics', 'Genomics'),
              width = '100%'
            )
          ),
          shiny::column(
            width = 6,
            shiny::textInput(
              inputId = ns('exp_name'),
              label = 'Name',
              placeholder = 'lips_1',
              width = '100%'
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyWidgets::actionBttn(
              inputId = ns('add_exp'),
              label = "Add Omics",
              style = "material-flat",
              color = 'success',
              block = T,
              icon = icon("check")
            )
          )
        )
        # shiny::fluidRow(
        #   shiny::fileInput(
        #     inputId = ns('input_mofa_file'),
        #     label = "MOFA file",
        #     width = "100%"
        #   )
        # ),
        # shiny::fluidRow(
        #   shiny::column(
        #     width = 12,
        #     shiny::downloadButton(
        #       outputId = ns("misoda_file_download"),
        #       label = "Download misoda file",
        #       style ="color: #fff; background-color: #00A86B; border-color: #00A86B; width:100%;"
        #     )
        #   )
        # )
      ),
      shiny::column(
        width = 1
      ),
      shiny::column(
        width = 5,
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::h5('CellMiner multi-omics dataset'),
        shiny::fluidRow(
          shiny::downloadButton(
            outputId = ns("dl_cellminer_data"),
            label = "CellMiner.zip",
            style = "width:100%;"
          )
        ),
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::h5('LTP-KO lipidomics dataset'),
        shiny::fluidRow(
          shiny::downloadButton(
            outputId = ns("dl_ltp_ko_data"),
            label = "LTP_KO.zip",
            style = "width:100%;"
          )
        ),
      )
    )
  )
}

#------------------------------------------------------------- Start server ----

start_server = function(id, main_input, main_output, main_session, module_controler) {
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

        created_modules = unname(unlist(module_controler$exp_names))
        created_modules = created_modules[!is.na(created_modules)]
        shiny::updateSelectInput(
          inputId = 'del_exp',
          choices = created_modules
        )

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
      
      # MOFA file input
      session$userData[[id]]$input_mofa_file = shiny::observeEvent(input$input_mofa_file, {
        file = input$input_mofa_file$datapath
        # module_controler$mofa_exp = base::readRDS(file)
        truffles = base::readRDS(file)
        print('Experiment types:')
        for (exp in names(truffles$exp_types)) {
          if (!is.null(truffles$exp_types[[exp]])) {
            print(truffles$exp_types[[exp]])
          }
        }
        
        print('Experiment names:')
        for (nm in names(truffles$exp_names)) {
          if (!is.null(truffles$exp_names[[nm]])) {
            print(truffles$exp_names[[nm]])
          }
        }
        
        print('Experiment slots:')
        for (slot in names(truffles$slot_taken)) {
          print(truffles$slot_taken[[slot]])
        }
        
        print('Experiment loaded:')
        for (loaded in names(truffles$module_loaded)) {
          print(truffles$module_loaded[[loaded]])
        }
        
        print('LOADED MOFA EXP')
      })
      
      # Download miSODA file misoda_file_download
      output$misoda_file_download = shiny::downloadHandler(
        
        filename = function() {
          timestamped_name("multiomics.misoda")
        },
        
        content = function(file) {
          # User feedback start
          print_tm(m = "Global", in_print = "Saving .misoda file locally")
          shinyjs::disable("misoda_file_download")
          waiter::waiter_show(
            id = ns("misoda_file_download"),
            html = spin_circle(),
            color = "#00A86B"
          )
          
          content = module_controler
          base::saveRDS(object = content,
                        file = file)
          
          # User feedback end
          waiter::waiter_hide(
            id = ns("misoda_file_download")
          )
          shinyjs::enable("misoda_file_download")
        }
      )

      # Download CellMiner data
      output$dl_cellminer_data = downloadHandler(
        filename = function(){"CellMiner.zip"},
        content = function(file) {
          file.copy("./tests/datasets/CellMiner.zip", file)
        },
        contentType = "application/zip"
      )
      # Download LTP-KO data
      output$dl_ltp_ko_data = downloadHandler(
        filename = function(){"LTP_KO.zip"},
        content = function(file) {
          file.copy("./tests/datasets/LTP_KO.zip", file)
        },
        contentType = "application/zip"
      )
    }
  )
}

