#------------------------------------------------------ Single omics server ----

single_omics_server = function(id, ns, input, output, session, module_controler, omics_type, isoda_version) {
  # Extract some values and update the module controler
  r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
  m = r6$name
  slot = r6$slot
  #---------------------------------------------- RENDER GLOBAL SKELETON UI ----
  
  # Render skeleton UI
  output$omics_ui = shiny::renderUI({
    bs4Dash::tabsetPanel(
      id = ns('skeleton_ui'),
      type = "pills",
      
      shiny::tabPanel(
        title = "Data",
        id = ns('data_supertab'),
        
        bs4Dash::tabsetPanel(
          id = ns('data_tab'),
          type = "tabs",
          
          shiny::tabPanel(
            title = "Data upload",
            shiny::uiOutput(
              outputId = ns('data_upload_ui')
            )
          ),
          shiny::tabPanel(
            title = "Samples",
            shiny::uiOutput(
              outputId = ns('sample_filtering_ui')
            )
          ),
          shiny::tabPanel(
            title = "Measurements",
            shiny::uiOutput(
              outputId = ns('up_data_ui')
            )
          ),
          shiny::tabPanel(
            title = "Features",
            shiny::uiOutput(
              outputId = ns('up_feature_metadata_ui')
            )
          )
        )
        
      ),
      shiny::tabPanel(
        title = "Interactive visualization",
        shiny::uiOutput(
          outputId = ns('visualize_data_ui')
        )
      ),
      shiny::tabPanel(
        title = "Functional analysis",
        id = ns('functional_analysis_ui'),
        
        bs4Dash::tabsetPanel(
          id = ns('data_tab'),
          type = "tabs",
          
          shiny::tabPanel(
            title = "Functional comparison",
            shiny::uiOutput(
              outputId = ns('functional_comparison_ui')
            )
          ),
          shiny::tabPanel(
            title = "Enrichment",
            shiny::uiOutput(
              outputId = ns('enrichment_ui')
            )
          ),
          shiny::tabPanel(
            title = "Over-representation",
            shiny::uiOutput(
              outputId = ns('over_representation_ui')
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "Save results",
        shiny::uiOutput(
          outputId = ns('save_results_ui')
        )
      )
    )
  })
  
  #-------------------------------------------------------- DATA UPLOAD TAB ----
  
  # Render the upload UI
  output$data_upload_ui = shiny::renderUI({
    shiny::fluidRow(
      
      # Upload choice
      shiny::column(
        width = 1,
        shiny::br(),
        shiny::fluidRow(
          bsplus::bs_embed_tooltip(
            shinyWidgets::awesomeRadio(
              inputId = ns("upload_method"),
              label = NULL, 
              choices = c("File upload", "iSODA file", "iSODA UUID"),
              selected = "File upload",
              status = "warning"
            ),
            title = tooltip_data$data_upload$upload_method,
            placement = "top")
        ),
        shiny::fluidRow(
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns('load_single_omics'),
              label = "Load",
              icon = icon("play"),
              style ="color: #fff; background-color: #00A86B; border-color: #00A86B",
              width = '100%'
            ),
            title = tooltip_data$data_upload$load_single_omics,
            placement = "top")
        )
      ),
      
      # Upload boxes
      shiny::column(
        width = 11,
        
        # Data files upload
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              id = ns('box_file_upload'),
              title = "File upload",
              width = 12,
              collapsible = F,
              solidHeader = T,
              status = "gray",
              render_upload_user_files(ns)
            )
          )
        ),
        
        # R6 object upload
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              id = ns('box_omics_file_upload'),
              title = "Omics file upload",
              width = 12,
              collapsible = F,
              solidHeader = T,
              status = "gray",
              render_upload_omics_file(ns)
            )
          )
        ),
        
        # Shared UUID 
        shiny::fluidRow(
          shiny::column(
            width = 12,
            bs4Dash::box(
              id = ns('box_omics_uuid'),
              title = "Omics UUID",
              width = 12,
              collapsible = F,
              solidHeader = T,
              status = "gray",
              render_omics_uuid(ns)
            )
          )
        )
      
      )
    )
  })
  
  # Observe radiobutton upload 
  session$userData[[id]]$upload_method = shiny::observeEvent(input$upload_method, {
    if (input$upload_method == "File upload") {
      if (input$box_file_upload$collapsed) {
        bs4Dash::updateBox(
          id = 'box_file_upload',
          action = "toggle",
          session = session
        )
      }
      if (!input$box_omics_file_upload$collapsed) {
        bs4Dash::updateBox(
          id = 'box_omics_file_upload',
          action = "toggle",
          session = session
        )
      }
      if (!input$box_omics_uuid$collapsed) {
        bs4Dash::updateBox(
          id = 'box_omics_uuid',
          action = "toggle",
          session = session
        )
      }
    } else if (input$upload_method == "iSODA file") {
      if (!input$box_file_upload$collapsed) {
        bs4Dash::updateBox(
          id = 'box_file_upload',
          action = "toggle",
          session = session
        )
      }
      if (input$box_omics_file_upload$collapsed) {
        bs4Dash::updateBox(
          id = 'box_omics_file_upload',
          action = "toggle",
          session = session
        )
      }
      if (!input$box_omics_uuid$collapsed) {
        bs4Dash::updateBox(
          id = 'box_omics_uuid',
          action = "toggle",
          session = session
        )
      }
    } else if (input$upload_method == "iSODA UUID") {
      if (!input$box_file_upload$collapsed) {
        bs4Dash::updateBox(
          id = 'box_file_upload',
          action = "toggle",
          session = session
        )
      }
      if (!input$box_omics_file_upload$collapsed) {
        bs4Dash::updateBox(
          id = 'box_omics_file_upload',
          action = "toggle",
          session = session
        )
      }
      if (input$box_omics_uuid$collapsed) {
        bs4Dash::updateBox(
          id = 'box_omics_uuid',
          action = "toggle",
          session = session
        )
      }
    }
  })
  
  # Observe sample annotations upload
  session$userData[[id]]$sample_annotations_upload = shiny::observeEvent(input$sample_annotations_upload, {
    
    imp_meta = soda_read_table(
      file_path = input$sample_annotations_upload$datapath,
      transpose = base::ifelse(input$sample_annotations_format == "Long", T, F)
    )
    shiny::updateSelectizeInput(
      inputId = "sample_annotations_id_col",
      session = session,
      choices = colnames(imp_meta)
    )
  })
  
  # Observe feature annotations upload
  session$userData[[id]]$feature_annotations_upload = shiny::observeEvent(input$feature_annotations_upload, {
    
    imp_feat = soda_read_table(
      file_path = input$feature_annotations_upload$datapath,
      transpose = base::ifelse(input$feature_annotations_format == "Wide", T, F)
    )
    
    shiny::updateSelectizeInput(
      inputId = "feature_annotations_id_col",
      session = session,
      choices = colnames(imp_feat)
    )
  })
  
  # Observe create experiment
  session$userData[[id]]$load_single_omics = shiny::observeEvent(input$load_single_omics, {
    
    print_tm(m = m, in_print = "Loading data")
    shinyjs::disable("load_single_omics")
    waiter::waiter_show(
      id = "load_single_omics",
      html = spin_3k(),
      color = NULL
    )
    
    base::withCallingHandlers({
      # Create the R6 object
      if (input$upload_method == "File upload") {
        r6 = initialize_omics(
          name = m,
          type = omics_type,
          version = isoda_version,
          meta_file = input$sample_annotations_upload$datapath,
          data_file = input$measurement_upload$datapath,
          feat_file = input$feature_annotations_upload$datapath,
          meta_file_format = input$sample_annotations_format,
          data_file_format = input$measurement_format,
          feat_file_format = input$feature_annotations_format,
          param_file = './R/params/params_lipidomics.R',
          id_col_meta = input$sample_annotations_id_col,
          id_col_data = 1,
          id_col_feat = input$feature_annotations_id_col
        )
      } else if (input$upload_method == "iSODA file") {
        r6 = base::readRDS(
          file = input$omics_file_upload$datapath
        )
      } else if (input$upload_method == "iSODA UUID") {
        isoda_file = paste0("./isoda_files/", input$omics_uuid_code, '.isoda')
        if (base::file.exists(isoda_file)) {
          r6 = base::readRDS(file = isoda_file)
        } else {
          base::stop(paste0("No stored omics under ", input$omics_uuid_code))
        }
        

      }
      
      # Check on omics type
      if (r6$type != omics_type) {
        base::stop(paste0("Incorrect .isoda omics type: ", r6$type, ", expected ", omics_type))
      }

      # Check on file version
      if (r6$version != isoda_version) {
        base::warning(paste0("Uploaded a legacy .isoda file (v", r6$version, "), unexpected errors might occur"))
      }
      
      # Store the R6
      module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]] = r6
      
      # Render sample filtering UI
      output$sample_filtering_ui = shiny::renderUI({
        render_sample_filtering(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      # Render measurement filtering UI
      output$up_data_ui = shiny::renderUI({
        render_measurement_filtering(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      # Render feature filtering UI
      output$up_feature_metadata_ui = shiny::renderUI({
        render_feature_filtering(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      # Render visualization UI
      output$visualize_data_ui = shiny::renderUI({
        render_visualization_tab(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      # Render functional_comparison UI
      output$functional_comparison_ui = shiny::renderUI({
        render_functional_comparison_tab(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      # Render enrichment UI
      output$enrichment_ui = shiny::renderUI({
        render_enrichment_tab(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      # Render over_representation UI
      output$over_representation_ui = shiny::renderUI({
        render_over_representation_tab(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      # Render download UI
      output$save_results_ui = shiny::renderUI({
        render_save_results_tab(
          ns = ns,
          r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
        )
      })
      
      print_tm(m = m, in_print = "Data successfully loaded")
      
    },warning = function(w){
      print_tmw(m, paste0("Warning: " , w))
    },error=function(e){
      waiter::waiter_hide(
        id = "load_single_omics"
      )
      shinyjs::enable("load_single_omics")
      print_tme(m, paste0("Error:" , e))
    })
    
    waiter::waiter_hide(
      id = "load_single_omics"
    )
    
    
  })

  #------------------------------------------------------------ SAMPLES TAB ----
  events_sample_filtering(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
  )
  #-------------------------------------------------------- MEASUREMENT TAB ----
  events_measurement_filtering(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
  )
  #------------------------------------------------------------ FEATURE TAB ----
  events_feature_filtering(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
  )
  #------------------------------------------------------ VISUALIZATION TAB ----
  events_visualization_tab(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]],
    module_controler = module_controler
  )
  #---------------------------------------------- FUNCTIONAL COMPARISON TAB ----
  events_functional_comparison_tab(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]],
    ns = ns
  )
  #--------------------------------------------------------- ENRICHMENT TAB ----
  events_enrichment_tab(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]],
    module_controler = module_controler
  )
  #------------------------------------------------ OVER-REPRESENTATION TAB ----
  events_over_representation_tab(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]],
    module_controler = module_controler
  )
  #----------------------------------------------------------- DOWNLOAD TAB ----
  events_save_results_tab(
    input = input,
    output = output,
    session = session,
    id = id,
    r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]],
    ns = ns
  )
  #-------------------------------------------------------------------- END ----
}
  