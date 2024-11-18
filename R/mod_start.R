
#------------------------------------------------------------- Save/Load UI ----
save_load_ui = function(id){
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 8,
        shiny::h1('iSODA - integrated Simple Omics Data Analysis'),
        shiny::hr(style = "border-top: 1px solid #7d7d7d;")
      ),
      shiny::column(
        width = 1,
      ),
      shiny::column(
        width = 3,
        shiny::h3('Example datasets'),
        shiny::hr(style = "border-top: 1px solid #7d7d7d;")
      )

    ),
    shiny::fluidRow(
      shiny::column(
        width = 1,
        shiny::br(),
        shiny::fluidRow(
          shinyWidgets::awesomeRadio(
            inputId = ns("start_method"),
            label = NULL, 
            choices = c("Load", "Save"),
            selected = "Load",
            status = "warning"
          )
        )
      ),
      shiny::column(
        width = 7,
        
        # Load miSODA file
        bs4Dash::box(
          id = ns('box_load_misoda_file'),
          title = 'Load a .miSODA file',
          width = 12,
          collapsible = F,
          solidHeader = T,
          status = "gray",
          render_load_misoda_file(ns)
        ),
        
        # Save miSODA file
        bs4Dash::box(
          id = ns('box_save_misoda_file'),
          title = 'Save multi-omics data',
          width = 12,
          collapsible = F,
          solidHeader = T,
          status = "gray",
          render_save_misoda_file(ns)
        )
        
      ),
      shiny::column(
        width = 1
      ),
      shiny::column(
        width = 3,
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
        )
      )
    )
  )
}

#------------------------------------------------------------- Start server ----

save_load_server = function(id, main_input, main_output, main_session, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      
      # Floating reactives 
      misoda_storage_uuid = shiny::reactiveVal("")
      input_misoda = shiny::reactiveValues(
        data = NULL
      )
      
      # Observe radiobutton method   
      shiny::observeEvent(input$start_method, {
        if (input$start_method == "Load") {
          if (input$box_load_misoda_file$collapsed) {
            bs4Dash::updateBox(
              id = 'box_load_misoda_file',
              action = "toggle",
              session = session
            )
          }
          if (!input$box_save_misoda_file$collapsed) {
            bs4Dash::updateBox(
              id = 'box_save_misoda_file',
              action = "toggle",
              session = session
            )
          }
        } else if (input$start_method == "Save") {
          if (!input$box_load_misoda_file$collapsed) {
            bs4Dash::updateBox(
              id = 'box_load_misoda_file',
              action = "toggle",
              session = session
            )
          }
          if (input$box_save_misoda_file$collapsed) {
            bs4Dash::updateBox(
              id = 'box_save_misoda_file',
              action = "toggle",
              session = session
            )
          }
        }
      })
      
      # .miSODA file input
      shiny::observeEvent(input$input_misoda_file, {
        
        print_tm(m = "Global", in_print = "Previewing data")
        
        file = input$input_misoda_file$datapath
        
        input_misoda$data = base::readRDS(file)
        
        file_summary = c()
        for (exp in names(input_misoda$data$module_loaded)) {
          if (input_misoda$data$module_loaded[[exp]]) {
            file_summary = c(file_summary, paste0(input_misoda$data$exp_types[[exp]], ': ', input_misoda$data$exp_names[[exp]]))
          }
        }
        file_summary = shiny::HTML(paste(file_summary, collapse = '<br>'))
        
        output$misoda_name = shiny::renderText(input_misoda$data$name)
        output$misoda_user = shiny::renderText(input_misoda$data$user)
        output$misoda_comments = shiny::renderText(input_misoda$data$comment)
        output$misoda_summary = shiny::renderText(file_summary)
        
        print_tm(m = "Global", in_print = "Preview ready")
      })
      
      # .miSODA UUID input
      shiny::observeEvent(input$input_misoda_uuid, {
        
        if (input$input_misoda_uuid == "") {return()}
        
        print_tm(m = "Global", in_print = "Previewing data")
        
        base::withCallingHandlers({
          
          file_name = paste0("./isoda_files/", input$input_misoda_uuid, ".misoda")
          
          if (!base::file.exists(file_name)) {
            base::stop("UUID not found")
          }
          input_misoda$data = base::readRDS(file_name)
          
          file_summary = c()
          for (exp in names(input_misoda$data$module_loaded)) {
            if (input_misoda$data$module_loaded[[exp]]) {
              file_summary = c(file_summary, paste0(input_misoda$data$exp_types[[exp]], ': ', input_misoda$data$exp_names[[exp]]))
            }
          }
          file_summary = shiny::HTML(paste(file_summary, collapse = '<br>'))
          
          output$misoda_name = shiny::renderText(input_misoda$data$name)
          output$misoda_user = shiny::renderText(input_misoda$data$user)
          output$misoda_comments = shiny::renderText(input_misoda$data$comment)
          output$misoda_summary = shiny::renderText(file_summary)
          
          print_tm(m = "Global", in_print = "Preview ready")
          
        },warning = function(w){
          print_tmw(m, paste0("Warning: " , w))
        },error=function(e){
          print_tme(m, paste0("Error:" , e))
        })
        
      })
      
      # Load .miSODA file
      shiny::observeEvent(input$load_misoda_file, {
        
        print_tm(m = "Global", in_print = "Loading data")
        shinyjs::disable("load_misoda_file")
        waiter::waiter_show(
          id = "load_misoda_file",
          html = spin_3k(),
          color = NULL
        )

        # Load exp_1
        if (!is.null(input_misoda$data$exp_names[["exp_1"]])) {
          main_output[["exp_1"]] = shiny::renderUI({
            bs4Dash::bs4SidebarMenuItem(
              text = input_misoda$data$exp_names[["exp_1"]],
              tabName = "exp_1",
              icon = shiny::icon("circle")
            )
          })
          module_controler$slot_taken[["exp_1"]] = TRUE
          module_controler$exp_names[["exp_1"]] = input_misoda$data$exp_names[["exp_1"]]
          module_controler$exp_types[["exp_1"]] = input_misoda$data$exp_types[["exp_1"]]
          module_controler$exp_r6[["exp_1"]] = input_misoda$data$exp_r6[["exp_1"]]
          module_controler$exp_preloaded[["exp_1"]] = input_misoda$data$exp_preloaded[["exp_1"]]
        }


        # Load exp_2
        if (!is.null(input_misoda$data$exp_names[["exp_2"]])) {
          main_output[["exp_2"]] = shiny::renderUI({
            bs4Dash::bs4SidebarMenuItem(
              text = input_misoda$data$exp_names[["exp_2"]],
              tabName = "exp_2",
              icon = shiny::icon("circle")
            )
          })
          module_controler$slot_taken[["exp_2"]] = TRUE
          module_controler$exp_names[["exp_2"]] = input_misoda$data$exp_names[["exp_2"]]
          module_controler$exp_types[["exp_2"]] = input_misoda$data$exp_types[["exp_2"]]
          module_controler$exp_r6[["exp_2"]] = input_misoda$data$exp_r6[["exp_2"]]
          module_controler$exp_preloaded[["exp_2"]] = input_misoda$data$exp_preloaded[["exp_2"]]
        }


        # Load exp_3
        if (!is.null(input_misoda$data$exp_names[["exp_3"]])) {
          main_output[["exp_3"]] = shiny::renderUI({
            bs4Dash::bs4SidebarMenuItem(
              text = input_misoda$data$exp_names[["exp_3"]],
              tabName = "exp_3",
              icon = shiny::icon("circle")
            )
          })
          module_controler$slot_taken[["exp_3"]] = TRUE
          module_controler$exp_names[["exp_3"]] = input_misoda$data$exp_names[["exp_3"]]
          module_controler$exp_types[["exp_3"]] = input_misoda$data$exp_types[["exp_3"]]
          module_controler$exp_r6[["exp_3"]] = input_misoda$data$exp_r6[["exp_3"]]
          module_controler$exp_preloaded[["exp_3"]] = input_misoda$data$exp_preloaded[["exp_3"]]
        }


        # Load exp_4
        if (!is.null(input_misoda$data$exp_names[["exp_4"]])) {
          main_output[["exp_4"]] = shiny::renderUI({
            bs4Dash::bs4SidebarMenuItem(
              text = input_misoda$data$exp_names[["exp_4"]],
              tabName = "exp_4",
              icon = shiny::icon("circle")
            )
          })
          module_controler$slot_taken[["exp_4"]] = TRUE
          module_controler$exp_names[["exp_4"]] = input_misoda$data$exp_names[["exp_4"]]
          module_controler$exp_types[["exp_4"]] = input_misoda$data$exp_types[["exp_4"]]
          module_controler$exp_r6[["exp_4"]] = input_misoda$data$exp_r6[["exp_4"]]
          module_controler$exp_preloaded[["exp_4"]] = input_misoda$data$exp_preloaded[["exp_4"]]
        }


        # Load exp_5
        if (!is.null(input_misoda$data$exp_names[["exp_5"]])) {
          main_output[["exp_5"]] = shiny::renderUI({
            bs4Dash::bs4SidebarMenuItem(
              text = input_misoda$data$exp_names[["exp_5"]],
              tabName = "exp_5",
              icon = shiny::icon("circle")
            )
          })
          module_controler$slot_taken[["exp_5"]] = TRUE
          module_controler$exp_names[["exp_5"]] = input_misoda$data$exp_names[["exp_5"]]
          module_controler$exp_types[["exp_5"]] = input_misoda$data$exp_types[["exp_5"]]
          module_controler$exp_r6[["exp_5"]] = input_misoda$data$exp_r6[["exp_5"]]
          module_controler$exp_preloaded[["exp_5"]] = input_misoda$data$exp_preloaded[["exp_5"]]
        }


        # Load exp_6
        if (!is.null(input_misoda$data$exp_names[["exp_6"]])) {
          main_output[["exp_6"]] = shiny::renderUI({
            bs4Dash::bs4SidebarMenuItem(
              text = input_misoda$data$exp_names[["exp_6"]],
              tabName = "exp_6",
              icon = shiny::icon("circle")
            )
          })
          module_controler$slot_taken[["exp_6"]] = TRUE
          module_controler$exp_names[["exp_6"]] = input_misoda$data$exp_names[["exp_6"]]
          module_controler$exp_types[["exp_6"]] = input_misoda$data$exp_types[["exp_6"]]
          module_controler$exp_r6[["exp_6"]] = input_misoda$data$exp_r6[["exp_6"]]
          module_controler$exp_preloaded[["exp_6"]] = input_misoda$data$exp_preloaded[["exp_6"]]
        }

        # MOFA data
        module_controler$mofa_exp = input_misoda$data$mofa_exp

        # SNF data
        module_controler$snf_exp = input_misoda$data$snf_exp

        if (sum(sapply(module_controler$slot_taken, base::isTRUE)) >= 6) {
          shinyjs::disable("add_exp")
        }
        
        waiter::waiter_hide(
          id = "load_misoda_file"
        )
        print_tm(m = "Global", in_print = "Data loaded")
      })
      
      # Download miSODA file
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
          
          
          module_controler$name = input$misoda_file_name
          module_controler$user = input$misoda_file_owner
          module_controler$comment = input$misoda_file_comment
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
      
      # Store miSODA UUID 
      session$userData[[id]]$misoda_file_store = shiny::observeEvent(input$misoda_file_store, {
        
        # User feedback start
        print_tm(m = "Global", in_print = "Saving .misoda on the server")
        shinyjs::disable("misoda_file_store")
        waiter::waiter_show(
          id = ns("misoda_file_store"),
          html = spin_circle(),
          color = "#00A86B"
        )
        
        # Process
        uuid_key = uuid::UUIDgenerate(output = "string")
        file_name = paste0('./isoda_files/', uuid_key, '.misoda')
        
        module_controler$name = input$misoda_file_name
        module_controler$user = input$misoda_file_owner
        module_controler$comment = input$misoda_file_comment
        
        base::saveRDS(object = module_controler,
                      file = file_name)
        print_tm(m = "Global", in_print = paste0("File saved under UUID ", uuid_key))
        output$misoda_uuid = shiny::renderText(
          uuid_key
        )
        misoda_storage_uuid(uuid_key)
        
        # User feedback end
        waiter::waiter_hide(
          id = ns("misoda_file_store")
        )
        shinyjs::enable("misoda_file_store")
        
      })
      
      # Copy uuid to clipboard 
      output$misoda_uuid_clip = shiny::renderUI({
        rclipboard::rclipButton(
          inputId = ns("misoda_uuid_clip_button"),
          label = NULL,
          clipText = misoda_storage_uuid(), 
          icon = icon("copy"),
          options = list(delay = list(show = 800, hide = 100), trigger = "hover"),
          width = "100%",
          style = "height: 45px;"
        )
      })
      
      # Feedback to the copy to clipboard
      session$userData[[id]]$misoda_uuid_clip_button = shiny::observeEvent(input$misoda_uuid_clip_button, {
        print_tm(m = "Global", in_print = "Copied to clipboard")
      })

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

