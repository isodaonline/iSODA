#------------------------------------------------------ Main menu functions ----
render_create_single_omics = function(ns) {
  shiny::fluidRow(
    shiny::column(
      width = 12,
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
      shinyWidgets::actionBttn(
        inputId = ns('add_exp'),
        label = "Add Omics",
        style = "material-flat",
        color = 'success',
        block = T,
        icon = icon("check")
      )
    ),
  )
}
render_load_misoda_file = function(ns) {
  shiny::fluidRow(
    shiny::column(
      width = 12,
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::fileInput(
            inputId = ns('input_misoda_file'),
            label = "miSODA file",
            accept = ".misoda",
            width = "100%"
          )
        ),
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = ns('input_misoda_uuid'),
            label = "miSODA UUID",
            width = "100%",
            placeholder = "miSODA UUID key produced via the app"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::span('Name'),
          shiny::tags$pre(
            shiny::textOutput(
              outputId = ns('misoda_name')
            ),
            style = "background-color: #D3D3D3; padding: 10px; border: 1px solid #ddd; border-radius: 5px; height: 45px; overflow-x: auto; white-space: nowrap;"
          ),
          shiny::span('Summary'),
          shiny::tags$pre(
            shiny::htmlOutput(
              outputId = ns('misoda_summary')
            ),
            style = "background-color: #D3D3D3; padding: 10px; border: 1px solid #ddd; border-radius: 5px; height: 150px; overflow-x: auto; white-space: nowrap;"
          )
        ),
        shiny::column(
          width = 6,
          shiny::span('User'),
          shiny::tags$pre(
            shiny::textOutput(
              outputId = ns('misoda_user')
            ),
            style = "background-color: #D3D3D3; padding: 10px; border: 1px solid #ddd; border-radius: 5px; height: 45px; overflow-x: auto; white-space: nowrap;"
          ),
          shiny::span('Comments'),
          shiny::tags$pre(
            shiny::textOutput(
              outputId = ns('misoda_comments')
            ),
            style = "background-color: #D3D3D3; padding: 10px; border: 1px solid #ddd; border-radius: 5px; height: 150px; overflow-x: auto; white-space: nowrap;"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::actionButton(
            inputId = ns("load_misoda_file"),
            label = "Load miSODA",
            icon = icon("play"),
            width = "100%",
            style ="color: #fff; background-color: #00A86B; border-color: #00A86B"
          )
        )
      )
    )
  )
}
render_save_misoda_file = function(ns) {
  shiny::fluidRow(
    rclipboard::rclipboardSetup(),
    shiny::column(
      width = 6,
      shiny::h3('Download'),
      shiny::span('Download your single- and multi-omics data as a .misoda file locally on your computer. This file can then be loaded back into iSODA and shared with collaborators to explore your data'),
      shiny::downloadButton(
        outputId = ns("misoda_file_download"),
        label = "Download misoda file",
        style ="color: #fff; background-color: #00A86B; border-color: #00A86B; width:100%;"
      ),
      shiny::fluidRow(
        shiny::br()
      ),
      shiny::h3('Store'),
      shiny::span('Store your multi-omics data on the server. You will be provided a key to access it again and to share it with collaborators'),
      shiny::actionButton(
        inputId = ns('misoda_file_store'),
        label = "Generate UUID",
        icon = NULL,
        style ="color: #fff; background-color: #00A86B; border-color: #00A86B",
        width = "100%"
      ),
      shiny::br(),
      shiny::span('Copy and store this key to resume work and share with collaborators:'),
      shiny::fluidRow(
        shiny::column(
          width = 10,
          shiny::tags$pre(
            shiny::textOutput(
              outputId = ns('misoda_uuid')
            ),
            style = "background-color: #D3D3D3; padding: 10px; border: 1px solid #ddd; border-radius: 5px; height: 45px; overflow-x: auto; white-space: nowrap;"
          )
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput(outputId = ns("misoda_uuid_clip")),
        )
      )
    ),
    shiny::column(
      width = 1
    ),
    shiny::column(
      width = 5,
      shiny::h3('Identify your data'),
      shiny::span("Optional descriptors for your .misoda file to make it more identifiable"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = ns("misoda_file_name"),
            label = "Name",
            value = "",
            width = "100%",
            placeholder = "Experiment name"
          )
        ),
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = ns("misoda_file_owner"),
            label = "User",
            value = "",
            width = "100%",
            placeholder = "User producing the file"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::textAreaInput(
            inputId = ns('misoda_file_comment'),
            label = "Comment",
            placeholder = "Comments helping identify the experiment",
            width = "100%",
            height = "200px"
          )
        )
      )
    )
  )
}
#---------------------------------------------------- Data upload functions ----
render_upload_user_files = function(ns) {
  shiny::fluidRow(
    shiny::column(
      width = 4,
      bs4Dash::box(
        id = ns("box_sample_annotations"),
        title = 'Sample annotations',
        width = 12,
        collapsible = F,
        status = "warning",
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 9,
              bsplus::bs_embed_tooltip(
                shiny::fileInput(
                  inputId = ns('sample_annotations_upload'),
                  label = NULL,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx"),
                  placeholder = 'Sample annotations file',
                  width = '100%'
                ),
                title = tooltip_data$single_omics$file_meta,
                placement = "top")
            ),
            shiny::column(
              width = 3,
              bsplus::bs_embed_tooltip(
                shinyWidgets::awesomeRadio(
                  inputId = ns("sample_annotations_format"),
                  label = NULL,
                  choices = c("Wide", "Long"),
                  selected = "Wide",
                  status = "warning"
                ),
                title = tooltip_data$single_omics$meta_file_format,
                placement = "top")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              bsplus::bs_embed_tooltip(
                shiny::selectizeInput(
                  inputId = ns("sample_annotations_id_col"),
                  label = "Sample IDs",
                  choices = NULL,
                  multiple = FALSE,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$select_id_meta,
                placement = "top")
            )
          )
        )
      )
    ),
    shiny::column(
      width = 4,
      bs4Dash::box(
        id = ns("box_measurement_data"),
        title = 'Measurement data',
        width = 12,
        collapsible = F,
        status = "warning",
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 9,
              bsplus::bs_embed_tooltip(
                shiny::fileInput(
                  inputId = ns('measurement_upload'),
                  label = NULL,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx"),
                  placeholder = 'Measurements file',
                  width = '100%'
                ),
                title = tooltip_data$single_omics$file_data,
                placement = "top")

            ),
            shiny::column(
              width = 3,
              bsplus::bs_embed_tooltip(
                shinyWidgets::awesomeRadio(
                  inputId = ns("measurement_format"),
                  label = NULL,
                  choices = c("Wide", "Long"),
                  selected = "Wide",
                  status = "warning"
                ),
                title = tooltip_data$single_omics$data_file_format,
                placement = "top")
            )
          ),
          shiny::fluidRow(
            shiny::span("First column must be sample IDs")
          )
        )
      )
    ),
    shiny::column(
      width = 4,
      bs4Dash::box(
        id = ns("box_feature_annotations"),
        title = 'Feature annotations (Optional)',
        width = 12,
        collapsible = F,
        status = "warning",
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 9,
              bsplus::bs_embed_tooltip(
                shiny::fileInput(
                  inputId = ns('feature_annotations_upload'),
                  label = NULL,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx"),
                  placeholder = 'Feature annotations file',
                  width = '100%'
                ),
                title = tooltip_data$single_omics$file_feat,
                placement = "top")
            ),
            shiny::column(
              width = 3,
              bsplus::bs_embed_tooltip(
                shinyWidgets::awesomeRadio(
                  inputId = ns("feature_annotations_format"),
                  label = NULL,
                  choices = c("Wide", "Long"),
                  selected = "Long",
                  status = "warning"
                ),
                title = tooltip_data$single_omics$feat_file_format,
                placement = "top")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              bsplus::bs_embed_tooltip(
                shiny::selectizeInput(
                  inputId = ns("feature_annotations_id_col"),
                  label = "Feature ID column",
                  choices = NULL,
                  multiple = FALSE,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$select_id_feat,
                placement = "top")
            )
          )
        )
      )
    )
  )
}
render_upload_omics_file = function(ns) {
  bsplus::bs_embed_tooltip(
    shiny::fileInput(
      inputId = ns('omics_file_upload'),
      label = NULL,
      accept = c(".isoda"),
      placeholder = 'Exported iSODA .isoda file',
      width = '100%'
    ),
    title = tooltip_data$single_omics$omics_file,
    placement = "top")
}
render_omics_uuid = function(ns) {
  bsplus::bs_embed_tooltip(
    shiny::textInput(
      inputId = ns('omics_uuid_code'),
      label = NULL,
      placeholder = "single-omics experiment uuid",
      width = "100%"
    ),
    title = tooltip_data$single_omics$omics_uuid,
    placement = "top")
}
#---------------------------------------------------- Samples tab functions ----
render_sample_filtering = function(ns, r6) {

  # Get type column type_col
  if (is.na(r6$indices$type_column)) {
    type_col = colnames(r6$tables$indexed_meta)[1]
  } else {
    type_col = r6$indices$type_column
  }

  # Get group column
  if (is.na(r6$indices$group_column)) {
    group_col = colnames(r6$tables$indexed_meta)[2]
  } else {
    group_col = r6$indices$group_column
  }

  # Get batch column
  if (is.na(r6$indices$batch_column)) {
    batch_col = grep(pattern = "batch",
                     x = colnames(r6$tables$indexed_meta),
                     ignore.case = TRUE)
    if (length(batch_col) == 0) {
      batch_col = "None"
    } else {
      batch_col = colnames(r6$tables$indexed_meta)[batch_col]
    }
  } else {
    batch_col = r6$indices$batch_column
  }


  shiny::fluidRow(
    shiny::column(
      width = 8,
      #### Preview table ----
      bs4Dash::box(
        id = ns("box_sample_filtering_table_preview"),
        title = "Table preview",
        width = 12,
        collapsible = T,
        maximizable = T,
        DT::dataTableOutput(
          outputId = ns("metadata_preview_table")
        ),style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
      ),
      #### Preview plots ----
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shinyWidgets::progressBar(
            id = ns('progbar_sample_counts'),
            value = nrow(r6$tables$raw_meta),
            total = nrow(r6$tables$indexed_meta),
            title = 'Sample count'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          plotly::plotlyOutput(
            outputId = ns('sample_type_plot'),
            width = '100%',
            height = '400px'
          )
        ),
        shiny::column(
          width = 6,
          plotly::plotlyOutput(
            outputId = ns('sample_group_plot'),
            width = '100%',
            height = '400px'
          )
        )
      )
      #### end ----
    ),
    shiny::column(
      width = 4,
      #### Table preview start ----
      shiny::fluidRow(
        shiny::h4('Preview table')
      ),
      shiny::fluidRow(
        shiny::column(
          width = 10,
          bsplus::bs_embed_tooltip(
            shiny::selectInput(
              inputId = ns('sample_annotations_preview_table'),
              label = NULL,
              choices = c('Indexed metadata table', 'Raw metadata table'),
              selected = 'Raw metadata table',
              width = '100%'
            ),
            title = tooltip_data$single_omics$select_meta_table,
            placement = "top")
        ),
        shiny::column(
          width = 2,
          bsplus::bs_embed_tooltip(
            shiny::downloadButton(
              outputId = ns("sample_annotations_download"),
              label = NULL,
              style = "width:100%;"
            ),
            title = tooltip_data$single_omics$download_metatable,
            placement = "top")
        )
      ),
      shiny::fluidRow(
        bsplus::bs_embed_tooltip(
          shinyWidgets::materialSwitch(
            inputId = ns('head_sample_annoations'),
            value = F,
            label = "Partial display",
            status = "danger"
          ),
          title = tooltip_data$single_omics$head_sample_annoations,
          placement = "top")
      ),
      #### Column selection start ----
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::h4("Column selection"),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns("sample_annotations_type_col"),
              label = "Type column",
              choices = colnames(r6$tables$indexed_meta),
              selected = type_col,
              multiple = FALSE,
              width = '100%'
            ),
            title = tooltip_data$single_omics$sample_annotations_type_col,
            placement = "top")
        ),
        shiny::column(
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns("sample_annotations_group_col"),
              label = "Group column",
              choices = colnames(r6$tables$indexed_meta),
              selected = group_col,
              multiple = FALSE,
              width = '100%'
            ),
            title = tooltip_data$single_omics$sample_annotations_group_col,
            placement = "top")
        ),
        shiny::column(
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns("sample_annotations_batch_col"),
              label = "Batch column",
              choices = c("None", colnames(r6$tables$indexed_meta)),
              selected = batch_col,
              multiple = FALSE,
              width = '100%'
            ),
            title = tooltip_data$single_omics$sample_annotations_batch_col,
            placement = "top")
        )
      ),
      #### Text patterns start ----
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::h4("Text patterns"),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::textInput(
              inputId = ns("sample_annotations_blank_pattern"),
              label = "Blanks substring",
              value = "blank",
              width = '100%'
            ),
            title = tooltip_data$single_omics$sample_annotations_blank_pattern,
            placement = "top")
        ),
        shiny::column(
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::textInput(
              inputId = ns("sample_annotations_qc_pattern"),
              label = "QCs substring",
              value = "quality",
              width = '100%'
            ),
            title = tooltip_data$single_omics$sample_annotations_qc_pattern,
            placement = "top")
        ),
        shiny::column(
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::textInput(
              inputId = ns("sample_annotations_pool_pattern"),
              label = "Pools substring",
              value = "pool",
              width = '100%'
            ),
            title = tooltip_data$single_omics$sample_annotations_pool_pattern,
            placement = "top")
        )
      ),
      #### Sample filtering start ----
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::h4("Sample filtering"),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h5("Non-sample selection"),

          bsplus::bs_embed_tooltip(
            shinyWidgets::checkboxGroupButtons(
              inputId = ns('non_samples_selection'),
              label = NULL,
              choices = c("Blanks", "QCs", "Pools"),
              selected = c("Blanks", "QCs", "Pools"),
              direction = "horizontal",
              status = "default",
              justified = TRUE,
              width = '100%',
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),
            title = tooltip_data$single_omics$non_samples_selection,
            placement = "top"),

          # Manual sample exclusion (selection from rows in the filtered metadata table)
          shiny::h5("Manual sample selection"),
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns("selection_manual"),
              choices = rownames(r6$tables$indexed_meta),
              label = NULL,
              multiple = T,
              width = "100%"
            ),
            title = tooltip_data$single_omics$selection_manual,
            placement = "top"),

          # Exclusion based on a metadata column value
          shiny::h5("Metadata selection"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              # Metadata column selection
              bsplus::bs_embed_tooltip(
                shiny::selectInput(
                  inputId = ns("exclusion_meta_col"),
                  choices = colnames(r6$tables$indexed_meta),
                  label = "Column",
                  multiple = F,
                  width = "100%"),
                title = tooltip_data$single_omics$exclusion_meta_col,
                placement = "top"),

              # Value in the metadata column
              bsplus::bs_embed_tooltip(
                shiny::selectizeInput(
                  inputId = ns("exclusion_meta_val"),
                  choices = NULL,
                  label = "Value",
                  multiple = T,
                  width = "100%"),
                title = tooltip_data$single_omics$exclusion_meta_val,
                placement = "top")
            ),
            shiny::column(
              width = 6,
              # Rows to exclude
              bsplus::bs_embed_tooltip(
                shiny::selectizeInput(
                  inputId = ns("exclusion_meta_row"),
                  choices = NULL,
                  label = "Samples",
                  multiple = T,
                  width = "100%"),
                title = tooltip_data$single_omics$exclusion_meta_row,
                placement = "top")
            )
          ),

          # Action buttons to apply filters, clear filters or reset filtered metadata
          shiny::fluidRow(
            shiny::column(
              width = 3,
              bsplus::bs_embed_tooltip(
                shiny::actionButton(
                  inputId = ns("selection_drop"),
                  label = "Drop",
                  width = "100%"
                ),
                title = tooltip_data$single_omics$selection_drop,
                placement = "top")
            ),
            shiny::column(
              width = 3,
              bsplus::bs_embed_tooltip(
                shiny::actionButton(
                  inputId = ns("selection_keep"),
                  label = "Keep",
                  width = "100%"
                ),
                title = tooltip_data$single_omics$selection_keep,
                placement = "top")
            ),
            shiny::column(
              width = 3,
              bsplus::bs_embed_tooltip(
                shiny::actionButton(
                  inputId = ns("clear_filters"),
                  label = "Clear filters",
                  width = "100%"
                ),
                title = tooltip_data$single_omics$clear_filters,
                placement = "top")
            ),
            shiny::column(
              width = 3,
              bsplus::bs_embed_tooltip(
                shiny::actionButton(
                  inputId = ns("reset_meta"),
                  label = "Reset table",
                  width = "100%"
                ),
                title = tooltip_data$single_omics$reset_meta,
                placement = "top")
            )
          )
        )
      )
      #### ----
    )
  )
}
events_sample_filtering = function(input, output, session, id, r6, reactive_triggers) {

  # Floating reactives
  samples_exclude = shiny::reactiveVal()

  # Table preview
  session$userData[[id]]$sample_annotations_preview_table = shiny::observeEvent(c(
    input$sample_annotations_preview_table,
    input$head_sample_annoations,
    reactive_triggers$meta_table_preview
  ), {
    shiny::req(input$sample_annotations_preview_table)
    print_t('meta table preview')

    # Head preview table
    preview_table = r6$table_switch_local(input$sample_annotations_preview_table)
    if (input$head_sample_annoations) {
      preview_table = preview_table[1:min(c(nrow(preview_table), 50)),
                                    1:min(c(ncol(preview_table), 100))]
    }

    output$metadata_preview_table = renderDataTable({
      DT::datatable(
        data = preview_table,
        options = list(paging = TRUE))
    })

    # Update the progress bar
    shinyWidgets::updateProgressBar(
      session = session,
      id = 'progbar_sample_counts',
      value = nrow(r6$table_switch_local(input$sample_annotations_preview_table)),
      total = nrow(r6$tables$indexed_meta),
      title = 'Sample count'
    )

    # Trigger the preview plots
    reactive_triggers$meta_plots = reactive_triggers$meta_plots + 1

  })

  # Reactive detection if dropping or keeping samples
  session$userData[[id]]$selection_drop = shiny::observeEvent(input$selection_drop, {
    samples_exclude(T)})
  session$userData[[id]]$selection_keep = shiny::observeEvent(input$selection_keep, {
    samples_exclude(F)})

  # Manual sample selection
  session$userData[[id]]$exclusion_meta_col = shiny::observeEvent(input$exclusion_meta_col, {
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exclusion_meta_val",
      choices = r6$tables$raw_meta[,input$exclusion_meta_col],
      selected = NULL
    )
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exclusion_meta_row",
      selected = character(0)
    )
  })
  session$userData[[id]]$exclusion_meta_val = shiny::observeEvent(input$exclusion_meta_val, {
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exclusion_meta_row",
      choices = rownames(r6$tables$raw_meta)[r6$tables$raw_meta[,input$exclusion_meta_col] %in% input$exclusion_meta_val],
      selected = rownames(r6$tables$raw_meta)[r6$tables$raw_meta[,input$exclusion_meta_col] %in% input$exclusion_meta_val]
    )
  })

  # Drop / Keep samples
  session$userData[[id]]$create_raw_meta = shiny::observeEvent(c(
    input$selection_drop,
    input$selection_keep
  ),{
    if (is.null(samples_exclude())) {return()}

    # Exclude samples accordingly
    r6$exclude_samples(
      selection = unique(c(input$selection_manual, input$exclusion_meta_row)),
      drop = samples_exclude()
    )

    r6$non_sample_exclusion(
      select_blanks = base::ifelse("Blanks" %in% input$non_samples_selection, T, F),
      select_qcs = base::ifelse("QCs" %in% input$non_samples_selection, T, F),
      select_pools = base::ifelse("Pools" %in% input$non_samples_selection, T, F),
      exclude = samples_exclude())

    # Set the raw metadata table
    r6$set_raw_meta()
    # r6$set_raw_data()
    # r6$derive_data_tables()
    
    # Set manual sample selection to null
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selection_manual",
      selected = character(0)
    )

    # Set metadata column selection to null
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exclusion_meta_val",
      selected = character(0)
    )
    
    # Set metadata sample selection to null
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exclusion_meta_row",
      selected = character(0)
    )

    # Reset the non sample filtering
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "non_samples_selection",
      selected = character(0)
    )

    # Update triggers
    reactive_triggers$meta_table_preview = reactive_triggers$meta_table_preview + 1
    reactive_triggers$set_raw_data = reactive_triggers$set_raw_data + 1 
    reactive_triggers$meta_plots = reactive_triggers$meta_plots + 1 

  })

  # Clear filters
  session$userData[[id]]$clear_filters = shiny::observeEvent(input$clear_filters, {
    # Non-sample selection
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "non_samples_selection",
      selected = c("Blanks", "QCs", "Pools")
    )

    # Manual sample selection
    shiny::updateSelectizeInput(
      session = session,
      inputId = "selection_manual",
      selected = character(0)
    )

    # Samples
    shiny::updateSelectizeInput(
      session = session,
      inputId = "exclusion_meta_row",
      selected = character(0)
    )

  })

  # Observe reset meta
  session$userData[[id]]$reset_meta = shiny::observeEvent(input$reset_meta, {
    r6$reset_sample_exclusion()
    r6$set_raw_meta()

    # Reset the non sample filtering
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "non_samples_selection",
      selected = c("Blanks", "QCs", "Pools")
    )

    # Update triggers
    reactive_triggers$meta_table_preview = reactive_triggers$meta_table_preview + 1
    reactive_triggers$set_raw_data = reactive_triggers$set_raw_data + 1 
    reactive_triggers$meta_plots = reactive_triggers$meta_plots + 1

  })

  # Observe sample indices
  session$userData[[id]]$set_sample_indices = shiny::observeEvent(
    c(input$sample_annotations_type_col,
      input$sample_annotations_group_col,
      input$sample_annotations_batch_col,
      input$sample_annotations_blank_pattern,
      input$sample_annotations_qc_pattern,
      input$sample_annotations_pool_pattern), {

        # Set columns
        r6$set_type_column(type_column = input$sample_annotations_type_col)
        r6$set_group_column(group_column = input$sample_annotations_group_col)
        r6$set_batch_column(batch_column = input$sample_annotations_batch_col)

        # Set indices
        r6$set_blank_indices(blank_pattern = input$sample_annotations_blank_pattern)
        r6$set_qc_indices(qc_pattern = input$sample_annotations_qc_pattern)
        r6$set_pool_indices(pool_pattern = input$sample_annotations_pool_pattern)
        r6$set_sample_indices()

        # Update triggers
        reactive_triggers$meta_plots = reactive_triggers$meta_plots + 1
      })

  # Observe preview plots
  session$userData[[id]]$render_sample_type_plot = shiny::observeEvent(
    reactive_triggers$meta_plots, {

      if (is.null(input$sample_annotations_type_col)) {return()}
      
      try_method(
        r6 = r6,
        method_name = "plot_sample_type_distribution",
        input_table = r6$table_switch_local(input$sample_annotations_preview_table))

      output$sample_type_plot = plotly::renderPlotly({
        r6$plots$sample_type_distribution
      })

      try_method(
        r6 = r6,
        method_name = "plot_sample_group_distribution",
        input_table = r6$table_switch_local(input$sample_annotations_preview_table))

      output$sample_group_plot = plotly::renderPlotly({
        r6$plots$sample_group_distribution
      })

    })

  # Download the samples table
  output$sample_annotations_download = shiny::downloadHandler(
    filename = function() {
      timestamped_name(paste0(stringr::str_replace_all(input$sample_annotations_preview_table, " ", "_"), ".csv"))
    },
    content = function(file) {
      content = r6$table_switch_local(input$sample_annotations_preview_table)
      write.csv(content, file)
    }
  )

}
#----------------------------------------------- Measurements tab functions ----
render_measurement_filtering = function(ns, r6) {

  if (r6$type == "Lipidomics") {
    table_previews = c('Indexed data table',
                       'Raw data table',
                       'Class normalized table',
                       'Total normalized table',
                       'Z-scored table',
                       'Z-scored class normalized table',
                       'Z-scored total normalized table',
                       'Class table',
                       'Class table z-scored',
                       'Class table total normalized',
                       'Class table z-scored total normalized')
  } else {
    table_previews = c('Indexed data table',
                       'Raw data table',
                       'Total normalized table',
                       'Z-scored table',
                       'Z-scored total normalized table')
  }


  shiny::fluidRow(
    shiny::column(
      width = 8,
      #### Preview table ----
      bs4Dash::box(
        id = ns("box_measurement_table_preview"),
        title = "Table preview",
        width = 12,
        collapsible = T,
        maximizable = T,
        DT::dataTableOutput(
          outputId = ns("data_preview_table")
        ),style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
      ),
      #### Preview plots ----
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shinyWidgets::progressBar(
            id = ns('progbar_sample_counts_data'),
            value = nrow(r6$tables$raw_data),
            total = nrow(r6$tables$indexed_data),
            title = 'Sample count'
          )
        ),
        shiny::column(
          width = 6,
          shinyWidgets::progressBar(
            id = ns('progbar_measurement_counts'),
            value = ncol(r6$tables$indexed_data),
            total = ncol(r6$tables$indexed_data),
            title = 'Feature count'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          plotly::plotlyOutput(
            outputId = ns('missingness_donut'),
            width = '100%',
            height = '400px'
          )
        ),
        shiny::column(
          width = 3,
          plotly::plotlyOutput(
            outputId = ns('sample_missingness'),
            width = '100%',
            height = '400px'
          )
        ),
        shiny::column(
          width = 3,
          plotly::plotlyOutput(
            outputId = ns('feature_missingness'),
            width = '100%',
            height = '400px'
          )
        )
      )
    ),
    shiny::column(
      width = 4,
      #### Table preview start ----
      shiny::fluidRow(
        shiny::h4('Preview table')
      ),
      shiny::fluidRow(
        shiny::column(
          width = 10,
          bsplus::bs_embed_tooltip(
            shiny::selectInput(
              inputId = ns('measurement_data_preview_table'),
              label = NULL,
              choices = table_previews,
              selected = 'Raw data table',
              width = '100%'
            ),
            title = tooltip_data$single_omics$select_data_table,
            placement = "top")
        ),
        shiny::column(
          width = 2,
          bsplus::bs_embed_tooltip(
            shiny::downloadButton(
              outputId = ns("measurement_download"),
              label = NULL,
              style = "width:100%;"
            ),
            title = tooltip_data$single_omics$download_datatable,
            placement = "top")
        )
      ),
      shiny::fluidRow(
        bsplus::bs_embed_tooltip(
          shinyWidgets::materialSwitch(
            inputId = ns('head_measurement_data'),
            value = T,
            label = "Partial display",
            status = "danger"
          ),
          title = tooltip_data$single_omics$head_measurement_data,
          placement = "top")
      ),
      #### Pre-analysis start ----
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::h4('Pre-analysis')
      ),
      bsplus::bs_embed_tooltip(
        shiny::selectInput(
          inputId = ns('operation_order'),
          label = "Operation order",
          choices = c("Imputation", "Batch correction", "Filtering"),
          selected = r6$params$measurement_filter$operation_order,
          multiple = T,
          width = "100%"
        ),
        title = tooltip_data$single_omics$operation_order,
        placement = "top"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::selectInput(
              inputId = ns('batch_effect_correction'),
              label = "Batch effect correction",
              choices = c('None', 'No controls', 'Pool', 'QC'),
              selected = r6$params$measurement_filter$batch_effect_correction,
              multiple = FALSE,
              width = '100%'
            ),
            title = tooltip_data$single_omics$batch_effect_correction,
            placement = "top")
        ),
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns("na_imputation"),
              choices = c('None', 'minimum', 'mean', 'median', 'maximum'),
              selected = r6$params$measurement_filter$imputation_method,
              label = 'Imputation method',
              multiple = F,
              width = "100%"),
            title = tooltip_data$single_omics$na_imputation,
            placement = "top")
        )
      ),
      shiny::br(),
      shiny::strong("Filtering"),
      shiny::fluidRow(
        shiny::column( # Blank multiplier
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::numericInput(
              inputId = ns("blank_multiplier"),
              label = 'Blank multiplier',
              value = r6$params$measurement_filter$blank_multiplier,
              min = 0,
              step = 0.5,
              width = "100%"),
            title = tooltip_data$single_omics$blank_multiplier,
            placement = "top")
        ),
        shiny::column( # Sample threshold
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::sliderInput(
              inputId = ns("sample_threshold"),
              label = "Sample threshold",
              value = r6$params$measurement_filter$sample_threshold,
              min = 0,
              max = 1,
              step = 0.05,
              width = "100%"),
            title = tooltip_data$single_omics$sample_threshold,
            placement = "top")
        ),
        shiny::column( # Group threshold
          width = 4,
          bsplus::bs_embed_tooltip(
            shiny::sliderInput(
              inputId = ns("group_threshold"),
              label = "Group threshold",
              value = r6$params$measurement_filter$group_threshold,
              min = 0,
              max = 1,
              step = 0.05,
              width = "100%"),
            title = tooltip_data$single_omics$group_threshold,
            placement = "top")
        )
      ),
      #### Normalization start ----
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::h4('Normalization')
      ),
      shiny::column(
        width = 12,
        bsplus::bs_embed_tooltip(
          shiny::selectizeInput(inputId = ns("normalise_to_col"),
                                label = "Normalise to column",
                                choices = c("None", colnames(r6$tables$indexed_feat)),
                                selected = r6$params$measurement_filter$norm_col,
                                width = "100%"),
          title = tooltip_data$single_omics$normalise_to_col,
          placement = "top")
      )
      #### end ----
    )
  )
}
events_measurement_filtering = function(input, output, session, id, r6, reactive_triggers) {

  # Table preview
  session$userData[[id]]$measurement_data_preview_table = shiny::observeEvent(
    c(input$measurement_data_preview_table,
      input$head_measurement_data,
      reactive_triggers$data_table_preview,

      # Sample annotation widgets
      input$selection_drop,
      input$selection_keep,
      input$reset_meta,

      # Feature annotation widgets
      input$drop_cols,
      input$keep_cols,
      input$reset_feat_table),
    {
      shiny::req(input$measurement_data_preview_table)
      print_t("data table preview")
      # Head preview table
      preview_table = r6$table_switch_local(input$measurement_data_preview_table)

      if (input$head_measurement_data) {
        preview_table = preview_table[1:min(c(nrow(preview_table), 50)),
                                      1:min(c(ncol(preview_table), 100))]
      }

      # Update rendered table
      output$data_preview_table = renderDataTable({
        DT::datatable(
          data = preview_table,
          options = list(paging = TRUE))
      })

      # Update the progress bar
      shinyWidgets::updateProgressBar(
        session = session,
        id = 'progbar_measurement_counts',
        value = ncol(r6$table_switch_local(input$measurement_data_preview_table)),
        total = ncol(r6$tables$indexed_data),
        title = 'Feature count'
      )
      
      # Update the progress bar
      shinyWidgets::updateProgressBar(
        session = session,
        id = 'progbar_sample_counts_data',
        value = nrow(r6$table_switch_local(input$measurement_data_preview_table)),
        total = nrow(r6$tables$indexed_data),
        title = 'Sample count'
      )
      
      # Update triggers
      reactive_triggers$data_plots = reactive_triggers$data_plots + 1

    })

  # Trigger signal filtering
  session$userData[[id]]$measurement_filtering = shiny::observeEvent(
    c(input$operation_order,
      input$batch_effect_correction,
      input$na_imputation,
      input$blank_multiplier,
      input$sample_threshold,
      input$group_threshold,
      input$normalise_to_col,
      reactive_triggers$set_raw_data), {
        shiny::req(input$operation_order)
        
        base::withCallingHandlers({
          # Data filtering
          r6$param_measurement_filter(
            operation_order = input$operation_order,
            batch_effect_correction = input$batch_effect_correction,
            imputation_method = input$na_imputation,
            blank_multiplier = input$blank_multiplier,
            sample_threshold = input$sample_threshold,
            group_threshold = input$group_threshold,
            norm_col = input$normalise_to_col)
          r6$set_raw_data()
          r6$set_raw_feat()
          r6$derive_data_tables()

          # Trigger the preview table
          reactive_triggers$data_table_preview = reactive_triggers$data_table_preview + 1
          reactive_triggers$feat_table_preview = reactive_triggers$feat_table_preview + 1
          reactive_triggers$data_plots = reactive_triggers$data_plots + 1

        },warning = function(w){
          print_tmw(r6$name, paste0("Warning: " , w))
        },error=function(e){
          print_tme(r6$name, paste0("Error:" , e))
        })
      })
  
  # Observe preview plots
  session$userData[[id]]$render_data_preview_plots = shiny::observeEvent(
    reactive_triggers$data_plots, {

      shiny::req(input$measurement_data_preview_table)
      
      # if (is.null(r6$tables$raw_data)) {return()}


      try_method(
        r6 = r6,
        method_name = "plot_missing_donut",
        input_table = r6$table_switch_local(input$measurement_data_preview_table))

      output$missingness_donut = plotly::renderPlotly({
        r6$plots$missing_donut
      })

    })

  # Download the measurements table
  output$measurement_download = shiny::downloadHandler(
    filename = function() {
      timestamped_name(paste0(stringr::str_replace_all(input$measurement_data_preview_table, " ", "_"), ".csv"))
    },
    content = function(file) {
      content = r6$table_switch_local(input$measurement_data_preview_table)
      write.csv(content, file)
    }
  )

}
#--------------------------------------------------- Features tab functions ----
render_feature_filtering = function(ns, r6) {

  if (r6$type %in% c('Proteomics', 'Transcriptomics', 'Genomics')) {
    feature_id_type_tags = shiny::tagList(
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::column(
        width = 12,
        shiny::selectInput(
          inputId = ns('feature_id_type'),
          label = 'Feature ID type',
          choices = c('UNIPROT', 'SYMBOL', 'ENTREZID'),
          selected = 'SYMBOL',
          multiple = F,
          width = '100%'
        )
      )
    )
  } else {
    feature_id_type_tags = shiny::tagList()
  }

  shiny::fluidRow(
    shiny::column(
      width = 8,
      #### Preview table ----
      bs4Dash::box(
        id = ns("box_feature_table_preview"),
        title = "Table preview",
        width = 12,
        collapsible = T,
        maximizable = T,
        DT::dataTableOutput(
          outputId = ns("feat_preview_table")
        ),style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
      ),
      #### Preview plots ----
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shinyWidgets::progressBar(
            id = ns('progbar_feature_counts'),
            value = nrow(r6$tables$indexed_feat),
            total = nrow(r6$tables$indexed_feat),
            title = 'Feature count'
          )
        )
      ),
      shiny::h4('plot placeholder')
    ),
    shiny::column(
      width = 4,
      #### Table preview start ----
      shiny::fluidRow(
        shiny::h4('Preview table')
      ),
      shiny::fluidRow(
        shiny::column(
          width = 10,
          bsplus::bs_embed_tooltip(
            shiny::selectInput(
              inputId = ns('feature_annotations_preview_table'),
              label = NULL,
              choices = c('Indexed feature table', 'Raw feature table'),
              selected = 'Raw feature table',
              width = '100%'
            ),
            title = tooltip_data$single_omics$feat_table_select,
            placement = "top")
        ),
        shiny::column(
          width = 2,
          bsplus::bs_embed_tooltip(
            shiny::downloadButton(
              outputId = ns("feat_download"),
              label = NULL,
              style = "width:100%;"
            ),
            title = tooltip_data$single_omics$download_feature_table,
            placement = "top")
        )
      ),
      shiny::fluidRow(
        bsplus::bs_embed_tooltip(
          shinyWidgets::materialSwitch(
            inputId = ns('head_feature_annotations'),
            value = F,
            label = "Partial display",
            status = "danger"
          ),
          title = tooltip_data$single_omics$head_feature_annotations,
          placement = "top")
      ),
      #### Feature ID type if relevant
      feature_id_type_tags,
      #### Manual filtering start ----
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::h4('Manual filtering')
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns('feature_col_selection'),
              label = "Feature column",
              choices = colnames(r6$tables$raw_feat),
              multiple = F,
              width = "100%"
            ),
            title = tooltip_data$single_omics$feature_col_selection,
            placement = "top"),
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns("feature_value"),
              label = "Value",
              choices = NULL,
              multiple = TRUE,
              width = "100%"),
            title = tooltip_data$single_omics$feature_value,
            placement = "top")
        ),
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns("feature_selection"),
              label = "Features",
              choices = NULL,
              multiple = TRUE,
              width = "100%"),
            title = tooltip_data$single_omics$feature_selection,
            placement = "top")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns("drop_cols"),
              label =  "Drop",
              width = "100%"),
            title = tooltip_data$single_omics$drop_cols,
            placement = "top")

        ),
        shiny::column(
          width = 3,
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns("keep_cols"),
              label =  "Keep",
              width = "100%"),
            title = tooltip_data$single_omics$keep_cols,
            placement = "top")

        ),
        shiny::column(
          width = 3,
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns("clear_data_filters"),
              label =  "Clear filters",
              width = "100%"),
            title = tooltip_data$single_omics$clear_data_filters,
            placement = "top")

        ),
        shiny::column(
          width = 3,
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns("reset_feat_table"),
              label =  "Reset",
              width = "100%"),
            title = tooltip_data$single_omics$reset_feat_table,
            placement = "top")

        )
      ),
      #### Sparse annotations start ----
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::h4('Sparse annotations'),
        shiny::tags$i(
          class = "fa fa-question-circle",
          style = "color: lightblue;",
          title = tooltip_data$single_omics$help_sparse_annotations
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::selectizeInput(
              inputId = ns('sparse_col_selection'),
              label = "Feature column",
              choices = colnames(r6$tables$raw_feat),
              selected = colnames(r6$tables$raw_feat)[1],
              width = "100%"
            ),
            title = tooltip_data$single_omics$sparse_feature_column,
            placement = "top")
        ),
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::selectInput(
              inputId = ns('sparse_delimiter'),
              label = "Sparse delimiter",
              choices = c('|', ':', '/', ';'),
              selected = '|',
              width = "100%"
            ),
            title = tooltip_data$single_omics$sparse_delimiter,
            placement = "top")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns('add_sparse_single'),
              label = "Add sparse table",
              width = "100%"
            ),
            title = tooltip_data$single_omics$add_sparse_single,
            placement = "top")
        )
      ),
      shiny::fluidRow(
        shiny::br()
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns('add_sparse_all'),
              label = "Parse all columns",
              width = "100%"
            ),
            title = tooltip_data$single_omics$add_sparse_all,
            placement = "top")
        ),
        shiny::column(
          width = 6,
          bsplus::bs_embed_tooltip(
            shiny::actionButton(
              inputId = ns('reset_sparse_tables'),
              label = "Reset tables",
              width = "100%"
            ),
            title = tooltip_data$single_omics$reset_sparse_tables,
            placement = "top")
        )
      )
      #### end ----
    )
  )
}
events_feature_filtering = function(input, output, session, id, r6, reactive_triggers) {

  # Table preview
  session$userData[[id]]$feature_annotations_preview_table = shiny::observeEvent(
    c(input$feature_annotations_preview_table,
      input$head_feature_annotations,
      reactive_triggers$feat_table_preview
    ),
    {
      shiny::req(input$feature_annotations_preview_table)
      print_t('feat table preview')
      # Head preview table
      preview_table = r6$table_switch_local(input$feature_annotations_preview_table)
      if (input$head_feature_annotations) {
        preview_table = preview_table[1:min(c(nrow(preview_table), 50)),
                                      1:min(c(ncol(preview_table), 100))]
      }

      # Update rendered table
      output$feat_preview_table = renderDataTable({
        DT::datatable(
          data = preview_table,
          options = list(paging = TRUE))
      })

      # Update the progress bar
      shinyWidgets::updateProgressBar(
        session = session,
        id = 'progbar_feature_counts',
        value = nrow(r6$table_switch_local(input$feature_annotations_preview_table)),
        total = nrow(r6$tables$indexed_feat),
        title = 'Feature count'
      )
    })

  # Reactive detection if dropping or keeping features
  features_exclude = shiny::reactiveVal()
  session$userData[[id]]$drop_cols = shiny::observeEvent(input$drop_cols, {
    features_exclude(T)})
  session$userData[[id]]$keep_cols = shiny::observeEvent(input$keep_cols, {
    features_exclude(F)})

  # Feature ID type selection feature_id_type
  session$userData[[id]]$feature_id_type = shiny::observeEvent(input$feature_id_type, {
    r6$indices$feature_id_type = input$feature_id_type
  })

  # Manual filtering for features
  session$userData[[id]]$feature_col_selection = shiny::observeEvent(input$feature_col_selection, {
    shiny::req(input$feature_col_selection)
    shiny::updateSelectizeInput(
      session = session,
      inputId = "feature_value",
      choices = r6$tables$raw_feat[,input$feature_col_selection],
      selected = NULL
    )
    shiny::updateSelectizeInput(
      session = session,
      inputId = "feature_selection",
      selected = character(0)
    )
  })
  session$userData[[id]]$feature_value = shiny::observeEvent(input$feature_value, {
    shiny::updateSelectizeInput(
      session = session,
      inputId = "feature_selection",
      choices = rownames(r6$tables$raw_feat)[r6$tables$raw_feat[,input$feature_col_selection] %in% input$feature_value],
      selected = rownames(r6$tables$raw_feat)[r6$tables$raw_feat[,input$feature_col_selection] %in% input$feature_value]
    )
  })

  # Create the raw feat
  session$userData[[id]]$create_raw_feat = shiny::observeEvent(c(
    features_exclude()
  ),{
    if (is.null(features_exclude())) {return()}

    # Exclude samples accordingly
    r6$exclude_features(selection = input$feature_selection,
                        drop = features_exclude())

    # Set the raw feature table
    r6$set_raw_feat()
    r6$set_raw_data()
    r6$derive_data_tables()

    # Set manual sample selection to null
    shiny::updateSelectizeInput(
      session = session,
      inputId = "feature_selection",
      selected = character(0)
    )

    # Set metadata sample selection to null
    shiny::updateSelectizeInput(
      session = session,
      inputId = "feature_value",
      selected = character(0)
    )

    # Trigger the preview table update
    reactive_triggers$feat_table_preview = reactive_triggers$feat_table_preview + 1
    reactive_triggers$data_table_preview = reactive_triggers$data_table_preview + 1

    # Reset the reactive
    features_exclude(NULL)

  })

  # Clear filters
  session$userData[[id]]$clear_data_filters = shiny::observeEvent(input$clear_data_filters, {

    # Manual sample selection
    shiny::updateSelectizeInput(
      session = session,
      inputId = "feature_selection",
      selected = character(0)
    )

    # Samples
    shiny::updateSelectizeInput(
      session = session,
      inputId = "feature_value",
      selected = character(0)
    )

  })

  # Reset feature table
  session$userData[[id]]$reset_feat_table = shiny::observeEvent(input$reset_feat_table, {
    print_tm(m = r6$name, in_print = "Resetting feature table")
    r6$reset_feature_exclusion()
    r6$set_raw_data()
    r6$set_raw_feat()
    r6$derive_data_tables()

    # Trigger the preview table update
    reactive_triggers$feat_table_preview = reactive_triggers$feat_table_preview + 1
    reactive_triggers$data_table_preview = reactive_triggers$data_table_preview + 1
  })

  # Add single sparse table
  session$userData[[id]]$add_sparse_single = shiny::observeEvent(input$add_sparse_single, {
    shinyjs::disable("add_sparse_single")
    base::withCallingHandlers({
      print_tm(m = r6$name, in_print = paste0("Generating sparse table from ", input$sparse_col_selection))
      r6$add_sparse_feat(sep = input$sparse_delimiter,
                         column_name = input$sparse_col_selection)
      print_tm(m = r6$name, in_print = "Sparse table successfully generated ")
    },warning = function(w){
      print_tmw(r6$name, paste0("Warning: " , w))
    },error=function(e){
      shinyjs::enable("add_sparse_single")
      print_tme(r6$name, paste0("Error:" , e))
    })
    shinyjs::enable("add_sparse_single")
  })


  # Add all sparse tables
  session$userData[[id]]$add_sparse_all = shiny::observeEvent(input$add_sparse_all, {
    shinyjs::disable("add_sparse_all")
    base::withCallingHandlers({
      r6$add_all_sparse_feat(sep = input$sparse_delimiter)
      print_tm(m = r6$name, in_print = paste0("Sparse tables generated for columns", paste(names(r6$tables$sparse_feat), collapse = ", ")))
    },warning = function(w){
      print_tmw(r6$name, paste0("Warning: " , w))
    },error=function(e){
      shinyjs::enable("add_sparse_all")
      print_tme(r6$name, paste0("Error:" , e))
    })
    shinyjs::enable("add_sparse_all")
  })

  # Reset all sparse tables
  session$userData[[id]]$reset_sparse_tables = shiny::observeEvent(input$reset_sparse_tables, {
    print_tm(m = r6$name, in_print = "Clearing sparse tables")
    r6$reset_sparse_feat()
  })


  # Download the features table
  output$feat_download = shiny::downloadHandler(
    filename = function() {
      timestamped_name(paste0(stringr::str_replace_all(input$feature_annotations_preview_table, " ", "_"), ".csv"))
    },
    content = function(file) {
      content = r6$table_switch_local(input$feature_annotations_preview_table)
      write.csv(content, file)
    }
  )

}
#---------------------------------------------- Visualization tab functions ----
render_visualization_tab = function(ns, r6) {
  # Get omics type for the plot selection
  if (r6$type == "Lipidomics") {
    plot_list = lipidomics_plot_list()
  } else {
    plot_list = generic_plot_list()
  }

  # UI structure
  shiny::tagList(
    shiny::fluidRow(
      # First column with the table input and preview of the raw data
      shiny::column(
        width = 11,
        shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                           label = NULL,
                                           status = "default",
                                           choices = plot_list,
                                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                           size = "sm",
                                           justified = TRUE
        )
      ),
      shiny::column(
        width = 1,
        shinyWidgets::actionBttn(inputId = ns("clear_plots"),
                                 label = "Clear",
                                 style = "material-flat",
                                 color = "danger",
                                 block = T,
                                 icon = icon("x"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::uiOutput(
          outputId = ns("plotbox_field")
        )
      )
    )
  )
}
events_visualization_tab = function(input, output, session, id, r6, module_controler) {


  # Initialise dimensions object
  dimensions_obj = shiny::reactiveValues(
    x_box = module_controler$dims$x_box,
    y_box = module_controler$dims$y_box,
    x_plot = module_controler$dims$x_plot,
    y_plot = module_controler$dims$y_plot,
    x_plot_full = module_controler$dims$x_plot_full,
    y_plot_full = module_controler$dims$y_plot_full,
    xpx_total = shinybrowser::get_width(),
    ypx_total = shinybrowser::get_height(),
    xbs = 12,
    xpx = shinybrowser::get_width(),
    ypx = shinybrowser::get_height()
  )

  color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = 'Spectral'))(40)
  # Plotting events
  dendrogram_events(r6, dimensions_obj, color_palette, input, output, session)
  class_distribution_events(r6, dimensions_obj, color_palette, input, output, session)
  class_comparison_events(r6, dimensions_obj, color_palette, input, output, session)
  volcano_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
  samples_correlation_events(r6, dimensions_obj, color_palette, input, output, session)
  feature_correlation_events(r6, dimensions_obj, color_palette, input, output, session)
  pca_events(r6, dimensions_obj, color_palette, input, output, session)
  fa_analysis_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  fa_comp_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  double_bonds_plot_events(r6, dimensions_obj, color_palette, input, output, session)

  session$userData[[id]]$showPlots = shiny::observeEvent(input$showPlots,{

    # Update x dimensions in px and bs, and y in px
    if (length(input$showPlots) < 2) {
      dimensions_obj$xbs = 12
      dimensions_obj$xpx = shinybrowser::get_width()
      dimensions_obj$ypx = shinybrowser::get_height()
    } else if (length(input$showPlots) == 2) {
      dimensions_obj$xbs  = 6
      dimensions_obj$xpx = shinybrowser::get_width()/2
      dimensions_obj$ypx = shinybrowser::get_height()
    } else {
      dimensions_obj$xbs  = 6
      dimensions_obj$xpx = shinybrowser::get_width()/2
      dimensions_obj$ypx = shinybrowser::get_height()/2.2
    }

    # Display plot boxes
    if (length(input$showPlots) == 1) {
      plot_one_lips(r6 = r6,
                    dimensions_obj = dimensions_obj,
                    selection_list = input$showPlots,
                    input = input,
                    output = output,
                    session = session)

    } else if (length(input$showPlots) == 2) {
      plot_two_lips(r6 = r6,
                    dimensions_obj = dimensions_obj,
                    selection_list = input$showPlots,
                    input = input,
                    output = output,
                    session = session)

    } else if (length(input$showPlots) == 3) {
      plot_three_lips(r6 = r6,
                      dimensions_obj = dimensions_obj,
                      selection_list = input$showPlots,
                      input = input,
                      output = output,
                      session = session)

    } else if (length(input$showPlots) >= 4) {
      plot_four_lips(r6 = r6,
                     dimensions_obj = dimensions_obj,
                     selection_list = input$showPlots,
                     input = input,
                     output = output,
                     session = session)

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "showPlots",
        disabledChoices = setdiff(unname(lipidomics_plot_list()), input$showPlots)
      )

    }

    if ((length(input$showPlots) > 1) & (length(input$showPlots) < 4)) {
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "showPlots",
        disabledChoices = NULL
      )
    } else if (length(input$showPlots) == 1) {
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "showPlots",
        disabledChoices = input$showPlots
      )
    }

  })

  session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
    print_tm(r6$name, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "showPlots",
      disabled = FALSE,
      selected = character(0))
    output$plotbox_field = shiny::renderUI(
      NULL
    )
  })

}
#-------------------------------------- Functional comparison tab functions ----
render_functional_comparison_tab = function(ns, r6) {

  # Detect omics type and the appropriate feature sets
  if (r6$type %in% c('Proteomics', 'Transcriptomics', 'Genomics')) {
    feature_sets = unique(c(
      'Gene ontology (ALL)',
      'Gene ontology (BP)',
      'Gene ontology (MF)',
      'Gene ontology (CC)',
      colnames(r6$tables$raw_feat)))
  } else {
    feature_sets = colnames(r6$tables$raw_feat)
  }

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::h4('Sample selection'),
        bsplus::bs_embed_tooltip(
          shiny::selectInput(
            inputId = ns('gseaprep_table_select'),
            label = 'Select table',
            choices = c('Raw data table', 'Total normalized table', 'Z-scored table', 'Z-scored total normalized table'),
            selected = 'Total normalized table',
            width = '90%'
          ),
          title = tooltip_data$single_omics$gseaprep_table_select,
          placement = "top"),
        bsplus::bs_embed_tooltip(
          shiny::selectInput(
            inputId = ns('gseaprep_group_col'),
            label = 'Group column',
            choices = colnames(r6$tables$raw_meta),
            width = '90%'
          ),
          title = tooltip_data$single_omics$gseaprep_group_col,
          placement = "top"),
        bsplus::bs_embed_tooltip(
          shiny::selectInput(
            inputId = ns('gseaprep_groups'),
            label = 'Select two groups',
            choices = NULL,
            width = '90%',
            multiple = T
          ),
          title = tooltip_data$single_omics$gseaprep_groups,
          placement = "top")

      ),
      shiny::column(
        width = 8,
        shiny::h4('Feature selection'),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            bsplus::bs_embed_tooltip(
              shiny::selectInput(
                inputId = ns('gseaprep_test'),
                label = 'Statistical test',
                choices = c('Wilcoxon', 't-Test'),
                selected = 't-Test',
                width = '100%'
              ),
              title = tooltip_data$single_omics$gseaprep_test,
              placement = "top")
          ),
          shiny::column(
            width = 4,
            bsplus::bs_embed_tooltip(
              shiny::selectInput(
                inputId = ns('gseaprep_adjustment'),
                label = 'Adjustment',
                choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                selected = 'BH',
                width = '100%'
              ),
              title = tooltip_data$single_omics$gseaprep_adjustment,
              placement = "top")
          ),
          shiny::column(
            width = 4,
            bsplus::bs_embed_tooltip(
              shiny::selectInput(
                inputId = ns('gseaprep_method'),
                label = 'FC calculation',
                choices = c('median', 'mean'),
                selected = 'mean',
                width = '100%'
              ),
              title = tooltip_data$single_omics$gseaprep_method,
              placement = "top")
          )
        ),
        bsplus::bs_embed_tooltip(
          shinyWidgets::radioGroupButtons(
            inputId = ns('fa_feature_selection'),
            label = NULL,
            choices = c('Statistical selection (ORA only)', 'User selection'),
            status = "info",
            justified = TRUE
          ),
          title = tooltip_data$single_omics$fa_feature_selection,
          placement = "top"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            bsplus::bs_embed_tooltip(
              shiny::numericInput(
                inputId = ns('gseaprep_pval'),
                label = "p-value cut-off",
                value = 0.05,
                min = 0,
                max = 0.99,
                step = 0.01,
                width = '100%'
              ),
              title = tooltip_data$single_omics$gseaprep_pval,
              placement = "top"),
            bsplus::bs_embed_tooltip(
              shiny::numericInput(
                inputId = ns('or_fc_threshold'),
                label = 'Fold change cut-off',
                value = 2,
                min = 0,
                step = 0.05,
                width = '100%'
              ),
              title = tooltip_data$single_omics$or_fc_threshold,
              placement = "top")
          ),
          shiny::column(
            width = 6,
            bsplus::bs_embed_tooltip(
              shiny::selectInput(
                inputId = ns('fa_feature_col'),
                label = "Feature annotations column",
                choices = colnames(r6$tables$raw_feat),
                width = '100%'
              ),
              title = tooltip_data$single_omics$fa_feature_col,
              placement = "top"),
            bsplus::bs_embed_tooltip(
              shiny::selectizeInput(
                inputId = ns("fa_feature_values"),
                label = "Group(s) to keep",
                choices = NULL,
                multiple = TRUE,
                width = '100%'
              ),
              title = tooltip_data$single_omics$fa_feature_values,
              placement = "top")
          )
        )
      )
    ),
    shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h4('Enrichment analysis'),

        shiny::actionButton(
          inputId = ns('run_gsea'),
          label = "Run EA",
          icon = icon("play"),
          style ="color: #fff; background-color: #00A86B; border-color: #00A86B",
          width = '100%'
        ),

        shiny::fluidRow(
          shiny::br()
        ),

        bs4Dash::box(
          id = ns('gsea_params_box'),
          title = 'Parameters',
          width = 12,
          shiny::column(
            width = 12,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                bsplus::bs_embed_tooltip(
                  shiny::selectInput(
                    inputId = ns('gsea_go'),
                    label = 'Feature sets',
                    choices = feature_sets,
                    selected = NULL,
                    width = '100%'
                  ),
                  title = tooltip_data$single_omics$gsea_go,
                  placement = "top")
              ),
              shiny::column(
                width = 6,
                bsplus::bs_embed_tooltip(
                  shiny::selectInput(
                    inputId = ns('gsea_adjustment'),
                    label = 'Adjustment (feature sets)',
                    choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                    selected = "BH",
                    width = '100%'
                  ),
                  title = tooltip_data$single_omics$gsea_adjustment,
                  placement = "top"
                )
              )
            ),

            shiny::fluidRow(
              shiny::column(
                width = 6,
                bsplus::bs_embed_tooltip(
                  shiny::numericInput(
                    inputId = ns('ea_min_size'),
                    label = 'Min. feature set size',
                    value = 3,
                    min = 1,
                    max = NA,
                    step = 5,
                    width = '100%'
                  ),
                  title = tooltip_data$single_omics$ea_min_size,
                  placement = "top"
                )
              ),
              shiny::column(
                width = 6,
                bsplus::bs_embed_tooltip(
                  shiny::numericInput(
                    inputId = ns('ea_max_size'),
                    label = 'Max. feature set size',
                    value = 800,
                    min = 1,
                    max = NA,
                    step = 50,
                    width = '100%'
                  ),
                  title = tooltip_data$single_omics$ea_max_size,
                  placement = "top"
                )
              )
            ),
            bsplus::bs_embed_tooltip(
              shiny::sliderInput(
                inputId = ns('gsea_pval'),
                label = 'p-value cutoff (feature sets)',
                min = 0.01,
                max = 0.9,
                value = 0.05,
                step = 0.01,
                width = '100%'
              ),
              title = tooltip_data$single_omics$gsea_pval,
              placement = "top"
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              bsplus::bs_embed_tooltip(
                shiny::numericInput(
                  inputId = ns('ea_seed'),
                  label = "Seed",
                  value = 1,
                  min = 1,
                  step = 1,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$ea_seed,
                placement = "top"
              )
            )
          ),
          collapsible = T,
          collapsed  = T,
          maximizable = F,
          headerBorder = T
        ),

        shiny::fluidRow(
          shiny::br()
        ),

        shiny::fluidRow(
          shiny::column(
            width = 12,
            bsplus::bs_embed_tooltip(
              shiny::downloadButton(
                outputId = ns("download_ea_table"),
                label = "EA table",
                style = "width:100%;"
              ),
              title = tooltip_data$single_omics$download_ea_table,
              placement = "top"
            )
          )
        )

      ),
      shiny::column(
        width = 6,
        shiny::h4('Over representation analysis'),

        shiny::actionButton(
          inputId = ns('run_ora'),
          label = "Run ORA",
          icon = icon("play"),
          style ="color: #fff; background-color: #00A86B; border-color: #00A86B",
          width = '100%'
        ),

        shiny::fluidRow(
          shiny::br()
        ),

        bs4Dash::box(
          id = ns('gsea_params_box'),
          title = 'Parameters',
          width = 12,

          shiny::fluidRow(
            shiny::column(
              width = 6,
              bsplus::bs_embed_tooltip(
                shiny::selectInput(
                  inputId = ns('or_go_ont'),
                  label = 'Feature sets',
                  choices = feature_sets,
                  selected = NULL,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$or_go_ont,
                placement = "top"
              )
            ),
            shiny::column(
              width = 6,
              bsplus::bs_embed_tooltip(
                shiny::selectInput(
                  inputId = ns('or_pval_adjustment'),
                  label = 'Adjustment (feature sets)',
                  choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                  selected = "BH",
                  width = '100%'
                ),
                title = tooltip_data$single_omics$or_pval_adjustment,
                placement = "top"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              bsplus::bs_embed_tooltip(
                shiny::numericInput(
                  inputId = ns('ora_min_gssize'),
                  label = 'Min. feature set size',
                  value = 10,
                  min = 1,
                  max = NA,
                  step = 5,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$ora_min_gssize,
                placement = "top"
              )
            ),
            shiny::column(
              width = 6,
              bsplus::bs_embed_tooltip(
                shiny::numericInput(
                  inputId = ns('ora_max_gssize'),
                  label = 'Max. feature set size',
                  value = 500,
                  min = 1,
                  max = NA,
                  step = 50,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$ora_max_gssize,
                placement = "top"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              bsplus::bs_embed_tooltip(
                shiny::sliderInput(
                  inputId = ns('or_pval_cutoff'),
                  label = 'p-value cutoff (feature sets)',
                  min = 0.01,
                  max = 0.9,
                  value = 0.05,
                  step = 0.01,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$or_pval_cutoff,
                placement = "top"
              )
            ),
            shiny::column(
              width = 6,
              bsplus::bs_embed_tooltip(
                shiny::sliderInput(
                  inputId = ns('or_qval_cutoff'),
                  label = 'q-value cutoff (feature sets)',
                  min = 0.01,
                  max = 0.9,
                  value = 0.05,
                  step = 0.01,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$or_qval_cutoff,
                placement = "top"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              bsplus::bs_embed_tooltip(
                shiny::numericInput(
                  inputId = ns('ora_seed'),
                  label = "Seed",
                  value = 1,
                  min = 1,
                  step = 1,
                  width = '100%'
                ),
                title = tooltip_data$single_omics$ora_seed,
                placement = "top"
              )
            )
          ),
          collapsible = T,
          collapsed  = T,
          maximizable = F,
          headerBorder = T
        ),

        shiny::fluidRow(
          shiny::br()
        ),

        shiny::fluidRow(
          shiny::column(
            width = 12,
            bsplus::bs_embed_tooltip(
              shiny::downloadButton(
                outputId = ns("download_ora_table"),
                label = "ORA table",
                style = "width:100%;"
              ),
              title = tooltip_data$single_omics$download_ora_table,
              placement = "top"
            )
          )
        )

      )
    )
  )

}
events_functional_comparison_tab = function(input, output, session, id, r6, ns) {

  # Floating reactives
  reactive_ea_table = shiny::reactiveVal(NULL)
  reactive_ora_table = shiny::reactiveVal(NULL)

  # GSEA groups
  session$userData[[id]]$gsea_groups = shiny::observeEvent(input$gseaprep_group_col,{
    shiny::req(input$gseaprep_group_col)
    shiny::updateSelectInput(
      inputId = 'gseaprep_groups',
      choices = unique(r6$tables$raw_meta[,input$gseaprep_group_col]),
      selected = unique(r6$tables$raw_meta[,input$gseaprep_group_col])[c(1,2)]
    )
  })

  # Update groups to keep
  session$userData[[id]]$fa_feature_col_detect = shiny::observeEvent(input$fa_feature_col, {
    shiny::req(input$fa_feature_col)
    shiny::updateSelectizeInput(
      inputId = "fa_feature_values",
      choices = unique(r6$tables$raw_feat[,input$fa_feature_col])
    )
  })

  # Radio button detect
  session$userData[[id]]$fa_feature_selection_detect = shiny::observeEvent(input$fa_feature_selection, {
    if (input$fa_feature_selection == 'Statistical selection (ORA only)') {
      shinyjs::disable("fa_feature_col")
      shinyjs::disable("fa_feature_values")
      shinyjs::enable("or_fc_threshold")
      shinyjs::enable("gseaprep_pval")

    } else if (input$fa_feature_selection == 'User selection') {
      shinyjs::enable("fa_feature_col")
      shinyjs::enable("fa_feature_values")
      shinyjs::disable("or_fc_threshold")
      shinyjs::disable("gseaprep_pval")
    }
  })

  # Run EA button
  session$userData[[id]]$run_gsea = shiny::observeEvent(input$run_gsea, {
    shiny::req(length(input$gseaprep_groups) == 2)
    print_tm(r6$name, "EA started")
    shinyjs::disable("run_gsea")
    waiter::waiter_show(
      id = ns("run_gsea"),
      html = spin_circle(),
      color = "#00A86B"
    )

    if (input$fa_feature_selection == 'Statistical selection (ORA only)') {
      input_table = r6$table_switch_local(input$gseaprep_table_select)
      selected_features = NULL
    } else if (input$fa_feature_selection == 'User selection') {
      if (length(input$fa_feature_values) == 0) {
        print_tme(m, 'User selection filtering selected, but not feature groups selected.')
        waiter::waiter_hide(
          id = ns("run_gsea")
        )
        shinyjs::enable("run_gsea")
        return()
      }
      input_table = r6$table_switch_local(input$gseaprep_table_select)
      selected_features = rownames(r6$tables$raw_feat)[which(r6$tables$raw_feat[, input$fa_feature_col] %in% input$fa_feature_values)]
      input_table = input_table[,selected_features]
    }

    base::tryCatch({
      r6$get_ea_feature_table(data_table = input_table,
                              group_col = input$gseaprep_group_col,
                              group_1 = input$gseaprep_groups[1],
                              group_2 = input$gseaprep_groups[2],
                              fc_function = input$gseaprep_method,
                              statistical_test = input$gseaprep_test,
                              adjustment_method = input$gseaprep_adjustment)

      if (input$gsea_go %in% c('Gene ontology (ALL)', 'Gene ontology (BP)', 'Gene ontology (MF)', 'Gene ontology (CC)')) {
        ont = gene_ontology_switch(input$gsea_go)
        custom_col = NULL
      } else if(input$gsea_go == "") {
        ont = NULL
        custom_col = NULL
      } else {
        ont = NULL
        custom_col = input$gsea_go
      }

      r6$get_ea_object(custom_col = custom_col,
                       selected_features = selected_features,
                       ont = ont,
                       minGSSize = input$ea_min_size,
                       maxGSSize = input$ea_max_size,
                       terms_p_value_cutoff = input$gsea_pval,
                       terms_pAdjustMethod = input$gsea_adjustment,
                       seed = input$ea_seed)

      reactive_ea_table(r6$tables$ea_table)

      results = nrow(r6$tables$ea_object)
      if (results == 0) {
        print_tm(r6$name, "EA failed: no term enriched under specific pvalueCutoff")
      } else {
        print_tm(r6$name, paste0("EA successful: ", results, ' terms.'))
      }
    },error=function(e){
      print_tm(r6$name, paste0('EA failed: ', e))
    },finally={}
    )
    waiter::waiter_hide(
      id = ns("run_gsea")
    )
    shinyjs::enable("run_gsea")
  })

  # Run ORA button
  session$userData[[id]]$run_ora = shiny::observeEvent(input$run_ora, {
    shiny::req(length(input$gseaprep_groups) == 2)
    print_tm(r6$name, "ORA started")
    shinyjs::disable("run_ora")
    waiter::waiter_show(
      id = ns("run_ora"),
      html = spin_circle(),
      color = "#00A86B"
    )

    if (input$fa_feature_selection == 'Statistical selection (ORA only)') {
      input_table = r6$table_switch_local(input$gseaprep_table_select)
      selected_features = NULL
    } else if (input$fa_feature_selection == 'User selection') {
      if (length(input$fa_feature_values) == 0) {
        print_tme(r6$name, 'User selection filtering selected, but not feature groups selected.')
        waiter::waiter_hide(
          id = ns("run_ora")
        )
        shinyjs::enable("run_ora")
        return()
      }
      input_table = r6$table_switch_local(input$gseaprep_table_select)
      selected_features = rownames(r6$tables$raw_feat)[which(r6$tables$raw_feat[, input$fa_feature_col] %in% input$fa_feature_values)]
    }

    base::tryCatch({
      r6$get_ora_feature_table(data_table = input_table,
                               group_col = input$gseaprep_group_col,
                               group_1 = input$gseaprep_groups[1],
                               group_2 = input$gseaprep_groups[2],
                               fc_function = input$gseaprep_method,
                               statistical_test = input$gseaprep_test,
                               adjustment_method = input$gseaprep_adjustment)


      if (input$or_go_ont %in% c('Gene ontology (ALL)', 'Gene ontology (BP)', 'Gene ontology (MF)', 'Gene ontology (CC)')) {
        ont = gene_ontology_switch(input$or_go_ont)
        custom_col = NULL
      } else if (input$or_go_ont == "") {
        ont = NULL
        custom_col = NULL
      } else {
        ont = NULL
        custom_col = input$or_go_ont
      }

      r6$get_ora_object(custom_col = custom_col,
                        selected_features = selected_features,
                        pval_cutoff_features = input$gseaprep_pval,
                        padjust_features = input$gseaprep_adjustment,
                        pval_cutoff = input$or_pval_cutoff,
                        pAdjustMethod = input$or_pval_adjustment,
                        fc_threshold = input$or_fc_threshold,
                        ont = ont,
                        qval_cutoff = input$or_qval_cutoff,
                        minGSSize = input$ora_min_gssize,
                        maxGSSize  = input$ora_max_gssize,
                        seed = input$ora_seed)

      reactive_ora_table(r6$tables$ora_table)

      if (!is.null(r6$tables$ora_object)) {
        results = nrow(r6$tables$ora_object)
        if (results == 0) {
          print_tm(r6$name, 'WARNING: no over-representation under selected parameters')
        } else {
          print_tm(r6$name, paste0('Over-representation successful: ', results, ' terms'))
        }
        print_tm(r6$name, "ORA finished")
      } else {
        print_tm(r6$name, 'No over represented features, returning.')
      }

    },error=function(e){
      print_tm(r6$name, paste0('ORA failed: ', e))
    },finally={}
    )


    waiter::waiter_hide(
      id = ns("run_ora")
    )
    shinyjs::enable("run_ora")

  })

  # Download the EA table
  output$download_ea_table = shiny::downloadHandler(
    filename = function() {
      timestamped_name("ea_table.tsv")
    },
    content = function(file) {
      content = reactive_ea_table()
      utils::write.table(
        x = content,
        file = file,
        na = "",
        sep = "\t")
    }
  )

  # Download the ORA table
  output$download_ora_table = shiny::downloadHandler(
    filename = function() {
      timestamped_name("ora_table.tsv")
    },
    content = function(file) {
      content = reactive_ora_table()
      utils::write.table(
        x = content,
        file = file,
        na = "",
        sep = "\t")
    }
  )

}
#------------------------------------------------- Enrichment tab functions ----
render_enrichment_tab = function(ns, r6) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 11,
        shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_gsea"),
                                           label = NULL,
                                           status = "default",
                                           choices = gsea_plot_list(),
                                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                           size = "normal",
                                           justified = TRUE)
      ),
      shiny::column(
        width = 1,
        shinyWidgets::actionBttn(inputId = ns("clear_plots_gsea"),
                                 label = "Clear",
                                 style = "material-flat",
                                 color = "danger",
                                 block = T,
                                 icon = icon("x"))
      )
    ),
    shiny::uiOutput(
      outputId = ns("gsea_plotbox_field")
    )
  )
}
events_enrichment_tab = function(input, output, session, id, r6, module_controler) {

  # Initialise dimensions object
  dimensions_obj_gsea = shiny::reactiveValues(
    x_box = module_controler$dims$x_box,
    y_box = module_controler$dims$y_box,
    x_plot = module_controler$dims$x_plot,
    y_plot = module_controler$dims$y_plot,
    x_plot_full = module_controler$dims$x_plot_full,
    y_plot_full = module_controler$dims$y_plot_full,
    xpx_total = shinybrowser::get_width(),
    ypx_total = shinybrowser::get_height(),
    xbs = 12,
    xpx = shinybrowser::get_width(),
    ypx = shinybrowser::get_height()
  )

  # Plot selection
  ea_dot_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)
  ea_ridge_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)
  ea_cnet_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)
  ea_emap_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)

  # Plot selection
  session$userData[[id]]$show_plots_gsea = shiny::observeEvent(input$show_plots_gsea, {
    shiny::req(r6$tables$ea_object)

    # Update x dimensions in px and bs, and y in px
    if (length(input$show_plots_gsea) < 2) {
      dimensions_obj_gsea$xbs = 12
      dimensions_obj_gsea$xpx = shinybrowser::get_width()
      dimensions_obj_gsea$ypx = shinybrowser::get_height()
    } else if (length(input$show_plots_gsea) == 2) {
      dimensions_obj_gsea$xbs  = 6
      dimensions_obj_gsea$xpx = shinybrowser::get_width()/2
      dimensions_obj_gsea$ypx = shinybrowser::get_height()
    } else {
      dimensions_obj_gsea$xbs  = 6
      dimensions_obj_gsea$xpx = shinybrowser::get_width()/2
      dimensions_obj_gsea$ypx = shinybrowser::get_height()/2.2
    }

    # Plots selected: 1 to 4
    if (length(input$show_plots_gsea) == 1) {
      plot_one_prot_gsea(r6 = r6,
                         dimensions_obj = dimensions_obj_gsea,
                         selection_list = input$show_plots_gsea,
                         input = input,
                         output = output,
                         session = session)
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_gsea",
        disabledChoices = input$show_plots_gsea
      )

    } else if (length(input$show_plots_gsea) == 2) {
      plot_two_prot_gsea(r6 = r6,
                         dimensions_obj = dimensions_obj_gsea,
                         selection_list = input$show_plots_gsea,
                         input = input,
                         output = output,
                         session = session)

    } else if (length(input$show_plots_gsea) == 3) {
      plot_three_prot_gsea(r6 = r6,
                           dimensions_obj = dimensions_obj_gsea,
                           selection_list = input$show_plots_gsea,
                           input = input,
                           output = output,
                           session = session)

    } else if (length(input$show_plots_gsea) >= 4) {
      plot_four_prot_gsea(r6 = r6,
                          dimensions_obj = dimensions_obj_gsea,
                          selection_list = input$show_plots_gsea,
                          input = input,
                          output = output,
                          session = session)

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_gsea",
        disabledChoices = setdiff(unname(gsea_plot_list()), input$show_plots_gsea)
      )

    }
    if ((length(input$show_plots_gsea) > 1) & (length(input$show_plots_gsea) < 4)) {
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_gsea",
        disabledChoices = NULL
      )
    }
  })

  # Clear plots
  session$userData[[id]]$clear_plots_gsea = shiny::observeEvent(input$clear_plots_gsea, {
    print_tm(r6$name, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "show_plots_gsea",
      disabled = FALSE,
      selected = character(0))
    output$gsea_plotbox_field = shiny::renderUI(
      NULL
    )
  })

}
#---------------------------------------- Over-representation tab functions ----
render_over_representation_tab = function(ns, r6) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 11,
        shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_or"),
                                           label = NULL,
                                           status = "default",
                                           choices = or_plot_list(),
                                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                           size = "normal",
                                           justified = TRUE)
      ),
      shiny::column(
        width = 1,
        shinyWidgets::actionBttn(inputId = ns("clear_plots_or"),
                                 label = "Clear",
                                 style = "material-flat",
                                 color = "danger",
                                 block = T,
                                 icon = icon("x"))
      )
    ),
    shiny::uiOutput(
      outputId = ns("or_plotbox_field")
    )
  )
}
events_over_representation_tab = function(input, output, session, id, r6, module_controler) {
  # Initialise dimensions object
  dimensions_obj_or = shiny::reactiveValues(
    x_box = module_controler$dims$x_box,
    y_box = module_controler$dims$y_box,
    x_plot = module_controler$dims$x_plot,
    y_plot = module_controler$dims$y_plot,
    x_plot_full = module_controler$dims$x_plot_full,
    y_plot_full = module_controler$dims$y_plot_full,
    xpx_total = shinybrowser::get_width(),
    ypx_total = shinybrowser::get_height(),
    xbs = 12,
    xpx = shinybrowser::get_width(),
    ypx = shinybrowser::get_height()
  )

  # Plot selection
  ora_dot_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)
  ora_bar_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)
  ora_cnet_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)
  ora_emap_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)

  # Plot selection
  session$userData[[id]]$show_plots_or = shiny::observeEvent(input$show_plots_or, {
    shiny::req(r6$tables$ora_object)

    # Update x dimensions in px and bs, and y in px
    if (length(input$show_plots_or) < 2) {
      dimensions_obj_or$xbs = 12
      dimensions_obj_or$xpx = shinybrowser::get_width()
      dimensions_obj_or$ypx = shinybrowser::get_height()
    } else if (length(input$show_plots_or) == 2) {
      dimensions_obj_or$xbs  = 6
      dimensions_obj_or$xpx = shinybrowser::get_width()/2
      dimensions_obj_or$ypx = shinybrowser::get_height()
    } else {
      dimensions_obj_or$xbs  = 6
      dimensions_obj_or$xpx = shinybrowser::get_width()/2
      dimensions_obj_or$ypx = shinybrowser::get_height()/2.2
    }

    # Plots selected: 1 to 4
    if (length(input$show_plots_or) == 1) {
      plot_one_prot_or(r6 = r6,
                       dimensions_obj = dimensions_obj_or,
                       selection_list = input$show_plots_or,
                       input = input,
                       output = output,
                       session = session)
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_or",
        disabledChoices = input$show_plots_or
      )

    } else if (length(input$show_plots_or) == 2) {
      plot_two_prot_or(r6 = r6,
                       dimensions_obj = dimensions_obj_or,
                       selection_list = input$show_plots_or,
                       input = input,
                       output = output,
                       session = session)

    } else if (length(input$show_plots_or) == 3) {
      plot_three_prot_or(r6 = r6,
                         dimensions_obj = dimensions_obj_or,
                         selection_list = input$show_plots_or,
                         input = input,
                         output = output,
                         session = session)

    } else if (length(input$show_plots_or) >= 4) {
      plot_four_prot_or(r6 = r6,
                        dimensions_obj = dimensions_obj_or,
                        selection_list = input$show_plots_or,
                        input = input,
                        output = output,
                        session = session)

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_or",
        disabledChoices = setdiff(unname(or_plot_list()), input$show_plots_or)
      )

    }
    if ((length(input$show_plots_or) > 1) & (length(input$show_plots_or) < 4)) {
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_or",
        disabledChoices = NULL
      )
    }
  })


  session$userData[[id]]$clear_plots_or = shiny::observeEvent(input$clear_plots_or, {
    print_tm(r6$name, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "show_plots_or",
      disabled = FALSE,
      selected = character(0))
    output$or_plotbox_field = shiny::renderUI(
      NULL
    )
  })
}
#--------------------------------------------------- Download tab functions ----
render_save_results_tab = function(ns, r6) {
  shiny::tagList(
    rclipboard::rclipboardSetup(),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h3('Download'),
            shiny::span("Download your experiment as an .isoda file locally on your computer. This file can then be loaded back into iSODA and shared with collaborators to explore your data"),
            shiny::downloadButton(
              outputId = ns("isoda_file_download"),
              label = "Download isoda file",
              style ="color: #fff; background-color: #00A86B; border-color: #00A86B; width:100%;"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::br(),
            shiny::h3('Store on server'),
            shiny::span("Store your data on the server. You will be provided a key to access it again and to share it with collaborators"),
            shiny::actionButton(
              inputId = ns('isoda_file_store'),
              label = "Generate UUID",
              icon = NULL,
              style ="color: #fff; background-color: #00A86B; border-color: #00A86B",
              width = "100%"
            ),
            shiny::br(),
            shiny::span('Copy and store this key to resume work and share with collaborators:'),
            shiny::fluidRow(
              shiny::column(
                width = 11,
                shiny::tags$pre(
                  shiny::textOutput(
                    outputId = ns('isoda_uuid')
                  ),
                  style = "background-color: #D3D3D3; padding: 10px; border: 1px solid #ddd; border-radius: 5px; height: 45px; overflow-x: auto; white-space: nowrap;"
                )
              ),
              shiny::column(
                width = 1,
                shiny::uiOutput(outputId = ns("uuid_clip")),
              )
            )

          )
        )
      ),
      shiny::column(
        width = 1
      ),
      shiny::column(
        width = 5,
        shiny::h3('Identify your data'),
        shiny::span("Optional descriptors for your .isoda file to make it more identifiable"),
        shiny::fluidRow(
          shiny::textInput(
            inputId = ns("isoda_file_owner"),
            label = "User",
            value = "",
            width = "100%",
            placeholder = "User producing the file"
          )
        ),
        shiny::fluidRow(
          shiny::textAreaInput(
            inputId = ns('isode_file_comment'),
            label = "Comment",
            placeholder = "Comments helping identify the experiment",
            width = "100%",
            height = "250px"
          )
        )
      )
    )
  )
}
events_save_results_tab = function(input, output, session, id, r6, ns) {
  # Floating reactives
  storage_uuid = shiny::reactiveVal("")

  # Download isoda file locally
  output$isoda_file_download = shiny::downloadHandler(

    filename = function() {
      timestamped_name(paste0(r6$name, ".isoda"))
    },

    content = function(file) {
      # User feedback start
      print_tm(m = r6$name, in_print = "Saving .isoda file locally")
      shinyjs::disable("isoda_file_download")
      waiter::waiter_show(
        id = ns("isoda_file_download"),
        html = spin_circle(),
        color = "#00A86B"
      )

      # Process
      if (input$isoda_file_owner != "") {
        r6$owner = input$isoda_file_owner
      }

      if (input$isode_file_comment != "") {
        r6$comment = input$isode_file_comment
      }
      content = r6
      base::saveRDS(object = content,
                    file = file)

      # User feedback end
      waiter::waiter_hide(
        id = ns("isoda_file_download")
      )
      shinyjs::enable("isoda_file_download")
    }
  )

  # Store to server
  session$userData[[id]]$isoda_file_store = shiny::observeEvent(input$isoda_file_store, {

    # User feedback start
    print_tm(m = r6$name, in_print = "Saving .isoda on the server")
    shinyjs::disable("isoda_file_store")
    waiter::waiter_show(
      id = ns("isoda_file_store"),
      html = spin_circle(),
      color = "#00A86B"
    )

    # Process
    uuid_key = uuid::UUIDgenerate(output = "string")
    file_name = paste0('./isoda_files/', uuid_key, '.isoda')
    base::saveRDS(object = r6,
                  file = file_name)
    print_tm(m = r6$name, in_print = paste0("File saved under UUID ", uuid_key))
    output$isoda_uuid = shiny::renderText(
      uuid_key
    )
    storage_uuid(uuid_key)

    # User feedback end
    waiter::waiter_hide(
      id = ns("isoda_file_store")
    )
    shinyjs::enable("isoda_file_store")

  })

  # Copy uuid to clipboard
  output$uuid_clip = shiny::renderUI({
    rclipboard::rclipButton(
      inputId = ns("uuid_clip_button"),
      label = NULL,
      clipText = storage_uuid(),
      icon = icon("copy"),
      options = list(delay = list(show = 800, hide = 100), trigger = "hover"),
      width = "100%",
      style = "height: 45px;"
    )
  })

  # Feedback to the copy to clipboard
  session$userData[[id]]$uuid_clip_button = shiny::observeEvent(input$uuid_clip_button, {
    print_tm(m = r6$name, in_print = "Copied to clipboard")
  })

}
