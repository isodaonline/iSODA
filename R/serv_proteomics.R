#-------------------------------------------------------- Proteomics server ----

proteomics_server = function(id, ns, input, output, session, module_controler) {

  # Extract some values and update the module controler
  r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
  m = r6$name
  slot = r6$slot
  #---------------------------------------------- Metadata upload rendering ----

  # Render skeleton UI
  output$omics_ui = shiny::renderUI({
    bs4Dash::tabsetPanel(
      id = ns('skeleton_ui'),
      type = "pills",

      shiny::tabPanel(
        title = "Data",

        bs4Dash::tabsetPanel(
          id = ns('data_tab'),
          type = "tabs",

          shiny::tabPanel(
            title = "Sample annotations",
            shiny::uiOutput(
              outputId = ns('up_sample_metadata_ui')
            )
          ),
          shiny::tabPanel(
            title = "Measurement data",
            shiny::uiOutput(
              outputId = ns('up_data_ui')
            )
          ),
          shiny::tabPanel(
            title = "Feature annotations",
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
        shiny::uiOutput(
          outputId = ns('functional_analysis_ui')
        )
      )
    )
  })

  #---------------------------------------------- Metadata upload rendering ----

  output$up_sample_metadata_ui = shiny::renderUI({
    shiny::fluidRow(

      # First column with the table input and preview of the raw data
      shiny::column(
        width = 8,

        shiny::br(),


        shiny::fluidRow(
          # Data upload
          shiny::column(
            width = 6,
            shiny::fileInput(
              inputId = ns("file_meta"),
              label = NULL,
              multiple = F,
              accept = c(".csv", ".tsv", ".txt", ".xlsx"),
              width = "100%")
          ),
          # Table select
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = ns('select_meta_table'),
              label = NULL,
              choices = c('Imported metadata table'),
              selected = 'Imported metadata table',
              width = '100%'
            )
          ),
          # Download metadata button
          shiny::column(
            width = 3,
            shiny::downloadButton(
              outputId = ns("download_metatable"),
              label = "Download",
              style = "width:100%;"
            )
          )
        ),

        # Table preview box
        bs4Dash::box(
          id = ns('table_box_meta'),
          title = 'Table preview (switch table above)',
          width = 12,
          DT::dataTableOutput(ns("metadata_preview_table")),style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
          collapsible = T,
          collapsed  = T,
          maximizable = T,
          headerBorder = T
        ),

        # Summary box
        bs4Dash::box(
          id = ns('summary_box_meta'),
          title = 'Data summary',
          width = 12,
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinyWidgets::progressBar(
                  id = ns("row_count_bar_meta"),
                  title = "Row count",
                  value = 100,
                  total = 100,
                  unit_mark = "%"
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::plotOutput(
                  outputId = ns('group_distribution_preview'),
                  height = '300px'
                )
              ),
              shiny::column(
                width = 6,
                shiny::plotOutput(
                  outputId = ns('type_distribution_preview'),
                  height = '300px'
                )
              )
            )
          ),
          collapsible = T,
          collapsed  = T,
          maximizable = F,
          headerBorder = T
        )

      ),

      # Second column for data curation
      shiny::column(
        width = 4,
        shiny::tags$h3("Select columns"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            # Select ID column
            shiny::selectInput(
              inputId = ns("select_id_meta"),
              choices = NULL,
              label = "Sample IDs",
              multiple = F,
              width = "100%"),

            # Select group column
            shiny::selectInput(
              inputId = ns("select_group_col"),
              choices = NULL,
              label = "Group column",
              multiple = F,
              width = "100%"),

          ),
          shiny::column(
            width = 6,
            # Select sample type column
            shiny::selectInput(
              inputId = ns("select_type_col"),
              choices = NULL,
              label = "Type column",
              multiple = F,
              width = "100%"),

            # Select batch column
            shiny::selectInput(
              inputId = ns("select_batch_col"),
              choices = NULL,
              label = "Batch column",
              multiple = F,
              width = "100%"),

            shiny::span(textOutput(outputId = ns("found_batches")))
          )
        ),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),

        # Section for regex text patterns
        shiny::h3("Text patterns"),

        shiny::fluidRow(
          shiny::column(
            width = 4,
            # Select blank battern text for regex
            shiny::textInput(inputId = ns("blank_pattern"), label = "Blanks:", value = "blank", width = "100%")
          ),

          shiny::column(
            width = 4,
            # Select QC battern text for regex
            shiny::textInput(inputId = ns("qc_pattern"), label = "QCs:", value = "quality", width = "100%")
          ),

          shiny::column(
            width = 4,
            # Select pool battern text for regex
            shiny::textInput(inputId = ns("pool_pattern"), label = "Pools:", value = "pool", width = "100%")
          )
        ),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),

        # Section for sample filtering
        shiny::h3("Sample filtering"),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h5("Non-sample selection"),

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

            # Manual sample exclusion (selection from rows in the filtered metadata table)
            shiny::h5("Manual sample selection"),
            shiny::selectizeInput(
              inputId = ns("selection_manual"), choices = NULL, label = NULL, multiple = T, width = "100%"
            ),

            # Exclusion based on a metadata column value
            shiny::h5("Metadata selection"),
            shiny::h6("Select samples based on metadata values"),

            shiny::fluidRow(
              shiny::column(
                width = 6,
                # Metadata column selection
                shiny::selectInput(inputId = ns("exclusion_meta_col"), choices = NULL, label = "Column", multiple = F, width = "100%")
              ),
              shiny::column(
                width = 6,
                # Value in the metadata column
                shiny::selectizeInput(inputId = ns("exclusion_meta_val"), choices = NULL, label = "Value", multiple = T, width = "100%")

              )
            ),

            # Rows to exclude
            shiny::selectizeInput(inputId = ns("exclusion_meta_row"), choices = NULL, label = "Samples", multiple = T, width = "100%"),

            # Action buttons to apply filters, clear filters or reset filtered metadata
            shiny::fluidRow(
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("selection_drop"),
                  label = "Drop",
                  width = "100%"
                )
              ),
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("selection_keep"),
                  label = "Keep",
                  width = "100%"
                )
              ),
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("clear_filters"),
                  label = "Clear filters",
                  width = "100%"
                )
              ),
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("reset_meta"),
                  label = "Reset table",
                  width = "100%"
                )
              )
            )
          )
        )
      )
    )
  })


  #------------------------------------------------- Metadata upload server ----

  # Upload metadata
  session$userData[[id]]$upload_meta = shiny::observeEvent(input$file_meta, {
    file_path = input$file_meta$datapath
    data_table = soda_read_table(file_path = file_path)
    if (ncol(data_table) > 70) {
      print_tm(m, 'ERROR: uploaded file has more than 70 columns, unlikely to be a metadata file')
      return()
    }
    r6$tables$imp_meta = data_table
    # Preview table
    output$metadata_preview_table = renderDataTable({
      DT::datatable(data_table, options = list(paging = TRUE))
    })

    if (input$table_box_meta$collapsed) {
      bs4Dash::updateBox(id = 'table_box_meta', action = 'toggle')
    }
    if (input$summary_box_meta$collapsed) {
      bs4Dash::updateBox(id = 'summary_box_meta', action = 'toggle')
    }

    # Update select inputs
    shiny::updateSelectInput(
      inputId = 'select_id_meta',
      choices = colnames(r6$tables$imp_meta),
      selected = colnames(r6$tables$imp_meta)[1]
    )
    shiny::updateSelectInput(
      inputId = 'select_group_col',
      choices = colnames(r6$tables$imp_meta),
      selected = colnames(r6$tables$imp_meta)[3]
    )
    shiny::updateSelectInput(
      inputId = 'select_type_col',
      choices = colnames(r6$tables$imp_meta),
      selected = colnames(r6$tables$imp_meta)[2]
    )

    # Batch regex & default
    default_batch = grep(pattern = "batch",
                         x = colnames(r6$tables$imp_meta),
                         ignore.case = TRUE)
    if (length(default_batch) == 0) {
      default_batch = "None"
    } else {
      default_batch = colnames(r6$tables$imp_meta)[default_batch]
    }

    shiny::updateSelectInput(
      inputId = 'select_batch_col',
      choices = c('None', colnames(r6$tables$imp_meta)),
      selected = default_batch
    )
    shinyjs::disable("file_meta")
  })

  # Preview all / subset switch
  session$userData[[id]]$select_meta_table = shiny::observeEvent(input$select_meta_table, {
    shiny::req(r6$tables$imp_meta)
    if (r6$preloaded_data) {return()}
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    output$metadata_preview_table = renderDataTable({
      DT::datatable(data_table, options = list(paging = TRUE))
    })

  })

  # Get ID
  session$userData[[id]]$id_select_meta = shiny::observeEvent(input$select_id_meta, {
    shiny::req(r6$tables$imp_meta)
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Setting ID column')
    if (length(r6$tables$imp_meta[,input$select_id_meta]) == length(unique(r6$tables$imp_meta[,input$select_id_meta]))) {
      r6$set_indexed_meta(id_col = input$select_id_meta)
      r6$set_raw_meta()
      update_sample_filters(input = input, session = session, r6 = r6)

      # Update metadata column input
      shiny::updateSelectInput(
        session = session,
        inputId = "exclusion_meta_col",
        choices = colnames(r6$tables$raw_meta)
      )

      shiny::updateSelectInput(
        inputId = 'select_meta_table',
        choices = c('Imported metadata table', 'Raw metadata table'),
        selected = 'Raw metadata table'
      )
    } else {
      print_tm(m, 'ERROR: Non-unique IDs in ID column')
      r6$tables$raw_meta = NULL
      shiny::updateSelectInput(
        inputId = 'select_meta_table',
        choices = c('Imported metadata table'),
        selected = 'Imported metadata table'
      )
    }
  })

  # Group col selection
  session$userData[[id]]$select_group_col = shiny::observeEvent(c(
    input$select_group_col,
    input$selection_drop,
    input$selection_keep,
    input$reset_meta),
    {

      shiny::req(r6$tables$indexed_meta)

      meta_table = table_switch(table_name = input$select_meta_table, r6 = r6)
      if (input$select_meta_table == 'Imported metadata table') {
        rownames(meta_table) = meta_table[,input$select_id_meta]
      }

      r6$set_group_column(group_column = input$select_group_col)

      groups = unique_na_rm(r6$tables$indexed_meta[, input$select_group_col])
      freq = data.frame(table(base::factor(na.omit(meta_table[, input$select_group_col]), levels = groups)))
      names(freq) = c("value", "count")

      output$group_distribution_preview = shiny::renderPlot(
        ggplot2::ggplot(data = freq, aes(x = value, y = count)) +
          geom_bar(stat = "identity", fill="blue")+
          geom_text(aes(label=count), vjust=0.5, hjust = -0.5, size=6)+
          xlab(NULL) +
          ylab(NULL) +
          ylim(0,max(freq$count)+10) +
          theme_minimal() +
          coord_flip() +
          labs(title = 'Groups distribution')+
          theme(
            plot.title = element_text(size=17, hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15)
          )
      )
    })

  # Batch col selection
  session$userData[[id]]$select_batch_col = shiny::observeEvent(input$select_batch_col, {
    shiny::req(r6$tables$indexed_meta)
    r6$set_batch_column(batch_column = input$select_batch_col)
  })

  # Type col selection
  session$userData[[id]]$select_type_col = shiny::observeEvent(c(
    input$select_type_col,
    input$blank_pattern,
    input$qc_pattern,
    input$pool_pattern),
    {
      shiny::req(c(
        r6$tables$indexed_meta,
        input$select_type_col,
        input$blank_pattern,
        input$qc_pattern,
        input$pool_pattern))

      if (input$select_type_col == "") {return()}

      r6$set_type_column(type_column = input$select_type_col)
      r6$set_blank_indices(blank_pattern = input$blank_pattern)
      r6$set_qc_indices(qc_pattern = input$qc_pattern)
      r6$set_pool_indices(pool_pattern = input$pool_pattern)
      r6$set_sample_indices()

    })



  # Updating the type plots
  session$userData[[id]]$select_type_col = shiny::observeEvent(c(
    input$select_type_col,
    input$blank_pattern,
    input$qc_pattern,
    input$pool_pattern,
    input$select_id_meta,
    input$select_meta_table,
    input$selection_drop,
    input$selection_keep,
    input$reset_meta),
    {
      shiny::req(r6$tables$raw_meta)

      meta_table = table_switch(table_name = input$select_meta_table, r6 = r6)
      if (input$select_meta_table == 'Imported metadata table') {
        rownames(meta_table) = meta_table[,input$select_id_meta]
      }

      index_blanks = rownames(meta_table)[rownames(meta_table) %in% r6$indices$index_blanks]
      index_qcs = rownames(meta_table)[rownames(meta_table) %in% r6$indices$index_qcs]
      index_pools = rownames(meta_table)[rownames(meta_table) %in% r6$indices$index_pools]

      meta_table[, 'type_distribution'] = 'Samples'
      meta_table[index_blanks, 'type_distribution'] = 'Blanks'
      meta_table[index_qcs, 'type_distribution'] = 'QCs'
      meta_table[index_pools, 'type_distribution'] = 'Pools'

      freq_table = data.frame(table(base::factor(meta_table[, 'type_distribution'], levels = c('Pools', 'QCs', 'Blanks', 'Samples'))))
      names(freq_table) = c("value", "count")

      output$type_distribution_preview = shiny::renderPlot(
        ggplot2::ggplot(data = freq_table, aes(x = value, y = count)) +
          geom_bar(stat = "identity", fill="blue")+
          geom_text(aes(label=count), vjust=-0.5, hjust = 0.5, size=6)+
          xlab(NULL) +
          ylab(NULL) +
          ylim(0,max(freq_table$count)+10) +
          theme_minimal() +
          labs(title = 'Type distribution')+
          theme(
            plot.title = element_text(size=17, hjust = 0.5),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15)
          )
      )
    })



  # Update the metadata value once a metadata column is selected
  session$userData[[id]]$exclusion_meta_col = shiny::observeEvent(c(input$exclusion_meta_col),{
    if (r6$preloaded_data) {return()}
    shiny::updateSelectInput(
      session = session,
      inputId = "exclusion_meta_val",
      choices = unique(r6$tables$raw_meta[,input$exclusion_meta_col]),
      selected = character(0)
    )
  })


  # Update the rows to filter once a metadata value is selected
  session$userData[[id]]$exclusion_meta_val = shiny::observeEvent(c(input$exclusion_meta_val),{
    if (r6$preloaded_data) {return()}
    if (!is.null(input$exclusion_meta_val)) {
      bool_vector = c()
      for (value in input$exclusion_meta_val) {
        bool_vector[[length(bool_vector) + 1]] = r6$tables$raw_meta[,input$exclusion_meta_col] == value
      }
      bool_vector = Reduce("|", bool_vector)

      shiny::updateSelectizeInput(
        session = session,
        inputId = "exclusion_meta_row",
        choices = rownames(r6$tables$raw_meta)[bool_vector],
        selected = rownames(r6$tables$raw_meta)[bool_vector]
      )
    }
  })

  # Clear button
  session$userData[[id]]$clear_filters = shiny::observeEvent(input$clear_filters, {
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Clearing metadata filters')
    reset_sample_filters(input = input, session = session, r6 = r6)
  })

  # Reset button
  session$userData[[id]]$reset_meta = shiny::observeEvent(input$reset_meta, {
    print_tm(m, 'Reseting metadata table')
    r6$reset_sample_exclusion()
    r6$set_raw_meta()
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Drop button
  session$userData[[id]]$selection_drop = shiny::observeEvent(input$selection_drop, {
    print_tm(m, 'Dropping selected samples')

    r6$exclude_samples(manual_selection = c(input$exclusion_meta_row, input$selection_manual),
                       select_blanks = base::ifelse("Blanks" %in% input$non_samples_selection, T, F),
                       select_qcs = base::ifelse("QCs" %in% input$non_samples_selection, T, F),
                       select_pools = base::ifelse("Pools" %in% input$non_samples_selection, T, F),
                       exclude = TRUE)

    r6$set_raw_meta()

    reset_sample_filters(input = input, session = session, r6 = r6)
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Keep button
  session$userData[[id]]$selection_keep = shiny::observeEvent(input$selection_keep, {
    print_tm(m, 'Keeping selected samples')

    r6$exclude_samples(manual_selection = c(input$exclusion_meta_row, input$selection_manual),
                       select_blanks = base::ifelse("Blanks" %in% input$non_samples_selection, T, F),
                       select_qcs = base::ifelse("QCs" %in% input$non_samples_selection, T, F),
                       select_pools = base::ifelse("Pools" %in% input$non_samples_selection, T, F),
                       exclude = F)

    r6$set_raw_meta()

    reset_sample_filters(input = input, session = session, r6 = r6)
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Row count progress bar
  session$userData[[id]]$row_count_bar_meta = shiny::observeEvent(c(input$selection_keep, input$selection_drop, input$reset_meta, input$select_meta_table), {
    if (r6$preloaded_data) {return()}
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    shinyWidgets::updateProgressBar(
      session = session,
      id = "row_count_bar_meta",
      value = nrow(data_table),
      total = nrow(r6$tables$imp_meta)
    )
  })

  # Download meta table
  dl_meta_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_metatable = shiny::observeEvent(c(input$select_meta_table, input$reset_meta, input$selection_keep, input$selection_drop) , {
    shiny::req(r6$tables$raw_meta)
    if (r6$preloaded_data) {return()}
    dl_meta_table$name = timestamped_name(paste0(stringr::str_replace_all(input$select_meta_table, " ", "_"), ".csv"))
    dl_meta_table$table = table_switch(input$select_meta_table, r6)
  })

  output$download_metatable = shiny::downloadHandler(
    filename = shiny::reactive(dl_meta_table$name),
    content = function(file_name) {
      write.csv(dl_meta_table$table, file_name, na = "")
    }
  )


  #-------------------------------------------------- Data upload rendering ----
  output$up_data_ui = shiny::renderUI({
    shiny::fluidRow(
      # First column with the table input and preview of the raw data
      shiny::column(
        width = 8,

        shiny::br(),


        shiny::fluidRow(
          # Data upload
          shiny::column(
            width = 6,
            shiny::fileInput(
              inputId = ns("file_data"),
              label = NULL,
              multiple = F,
              accept = c(".csv", ".tsv", ".txt", ".xlsx"),
              width = "100%")
          ),
          # Table select
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = ns('select_data_table'),
              label = NULL,
              choices = c('Imported data table'),
              selected = 'Imported data table',
              width = '100%'
            )
          ),
          # Download button
          shiny::column(
            width = 3,
            shiny::downloadButton(
              outputId = ns("download_datatable"),
              label = "Download",
              style = "width:100%;"
            )
          )
        ),
        # Table preview box
        bs4Dash::box(
          id = ns('table_box_data'),
          title = 'Table preview (switch table above)',
          width = 12,
          DT::dataTableOutput(ns("data_preview_table")),style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
          collapsible = T,
          collapsed  = T,
          maximizable = T,
          headerBorder = T
        ),

        # Summary box
        bs4Dash::box(
          id = ns('summary_box_data'),
          title = 'Data summary',
          width = 12,
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinyWidgets::progressBar(
                  id = ns("row_count_bar_data"),
                  title = "Row count",
                  value = 100,
                  total = 100,
                  unit_mark = "%"
                )
              ),
              shiny::column(
                width = 6,
                shinyWidgets::progressBar(
                  id = ns("col_count_bar"),
                  title = "Column count",
                  value = 100,
                  total = 100,
                  unit_mark = "%"
                )
              )
            )
          ),
          collapsible = T,
          collapsed  = T,
          maximizable = F,
          headerBorder = T
        )
      ),
      shiny::column(
        width = 4,
        shiny::tags$h3("Select columns"),
        # Select ID column
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectInput(inputId = ns("select_id_data"), choices = NULL, label = "Sample IDs", multiple = F, width = "100%")
          ),
          shiny::column(
            width = 6,
            shiny::selectInput(
              inputId = ns('select_feature_type'),
              label = 'Feature ID type',
              choices = c('UNIPROT', 'SYMBOL', 'ENTREZID'),
              selected = 'SYMBOL',
              multiple = F
            )
          )
        ),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::selectInput(
          inputId = ns('operation_order'),
          label = "Pre-analysis Selection and Order",
          choices = c("Imputation", "Batch correction", "Filtering"),
          selected = c("Imputation", "Batch correction", "Filtering"),
          multiple = T,
          width = "100%"
        ),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::fluidRow(
          shiny::selectInput(
            inputId = ns('batch_effect_correction'),
            label = "Batch effect correction",
            choices = c('None', 'No controls', 'Pool', 'QC'),
            selected = "None",
            multiple = FALSE,
            width = '100%'
          )
        ),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::fluidRow(
          shiny::selectizeInput(inputId = ns("na_imputation"),
                                choices = c('None', 'minimum', 'mean', 'median', 'max'),
                                selected = "None",
                                label = 'Imputation method',
                                multiple = F,
                                width = "100%")
        ),

        # Blank and group filtering
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h6("Filtering")
          )
        ),

        shiny::fluidRow(
          shiny::fluidRow(
            shiny::numericInput(
              inputId = ns("blank_multiplier"),
              label = 'Blank multiplier',
              value = 2,
              min = 0,
              step = 0.5,
              width = "100%")
          ),
        ),

        shiny::fluidRow(
          # Sample threshold
          shiny::column(
            width = 6,
            shiny::sliderInput(
              inputId = ns("sample_threshold"),
              label = "Sample threshold",
              value = 0.8,
              min = 0,
              max = 1,
              step = 0.05,
              width = "100%")
          ),
          # Group threshold
          shiny::column(
            width = 6,
            shiny::sliderInput(
              inputId = ns("group_threshold"),
              label = "Group threshold",
              value = 0.8,
              min = 0,
              max = 1,
              step = 0.05,
              width = "100%")
          )
        ),

        # Normalisation
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::selectizeInput(inputId = ns("normalise_to_col"),
                              label = "Normalise to column",
                              choices = "None",
                              width = "100%"),

        # Manual filtering
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::h6("Manual filtering"),
        shiny::fluidRow(
          shiny::selectizeInput(
            inputId = ns('feature_col_selection'),
            label = "Column selection",
            choices = NULL,
            multiple = F,
            width = "100%"
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectizeInput(inputId = ns("class_selection"), label = "Select classes", choices = NULL, multiple = TRUE, width = "100%")
          ),
          shiny::column(
            width = 6,
            shiny::selectizeInput(inputId = ns("manual_selection"), label = "Select feature", choices = NULL, multiple = TRUE, width = "100%")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("drop_cols"), label =  "Drop", width = "100%")
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("keep_cols"), label =  "Keep", width = "100%")
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("clear_data_filters"), label =  "Clear filters", width = "100%")
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("reset_data_table"), label =  "Reset", width = "100%")
          )
        )
      )
    )
  })

  #----------------------------------------------------- Data upload server ----

  # Upload metadata
  session$userData[[id]]$upload_data = shiny::observeEvent(input$file_data, {
    file_path = input$file_data$datapath
    data_table = soda_read_table(file_path = file_path)
    r6$tables$imp_data = data_table
    # Preview table
    output$data_preview_table = renderDataTable({
      DT::datatable(utils::head(data_table[,1:100]), options = list(paging = TRUE, pageLength = 25))
    })

    if (input$table_box_data$collapsed) {
      bs4Dash::updateBox(id = 'table_box_data', action = 'toggle')
    }
    if (input$summary_box_data$collapsed) {
      bs4Dash::updateBox(id = 'summary_box_data', action = 'toggle')
    }
    # if (input$feat_table_preview_box$collapsed) {
    #   bs4Dash::updateBox(id = 'feat_table_preview_box', action = 'toggle')
    # }



    # Update select inputs
    shiny::updateSelectInput(
      inputId = 'select_id_data',
      choices = colnames(r6$tables$imp_data),
      selected = colnames(r6$tables$imp_data)[1]
    )

    # Update normalise to column
    shiny::updateSelectInput(
      inputId = 'normalise_to_col',
      choices = c("None", colnames(r6$tables$raw_meta)),
      selected = "None"
    )

    shinyjs::disable("file_data")
  })

  # Preview all / subset switch
  session$userData[[id]]$select_data_table = shiny::observeEvent(input$select_data_table, {
    shiny::req(r6$tables$imp_data)

    data_table = table_switch(table_name = input$select_data_table, r6 = r6)

    if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {


      shinyWidgets::updateProgressBar(
        session = session,
        id = "col_count_bar",
        value = ncol(data_table),
        total = ncol(r6$tables$imp_data)
      )

      shinyWidgets::updateProgressBar(
        session = session,
        id = "row_count_bar_data",
        value = nrow(data_table),
        total = nrow(r6$tables$imp_data)
      )
    }

    output$data_preview_table = renderDataTable({
      DT::datatable(utils::head(data_table[,1:100]), options = list(paging = TRUE, pageLength = 25))
    })

  })


  # Get ID
  session$userData[[id]]$id_select_data = shiny::observeEvent(input$select_id_data, {
    shiny::req(r6$tables$imp_data)
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Setting ID column')
    if (length(r6$tables$imp_data[,input$select_id_data]) == length(unique(r6$tables$imp_data[,input$select_id_data]))) {

      try_method(r6 = r6,
                 method_name = "set_indexed_data",
                 id_col = input$select_id_data)

      try_method(r6 = r6,
                 method_name = "set_raw_data",
                 operation_order = input$operation_order,
                 blank_multiplier = input$blank_multiplier,
                 sample_threshold = input$sample_threshold,
                 group_threshold = input$group_threshold,
                 imputation_method = input$na_imputation,
                 batch_effect_correction = input$batch_effect_correction,
                 norm_col = input$normalise_to_col)

      r6$derive_data_tables()

      shiny::updateSelectInput(
        inputId = 'select_data_table',
        choices = c('Imported data table', 'Raw data table', 'Blank table', 'Total normalized table', 'Z-scored table', 'Z-scored total normalized table'),
        selected = 'Raw data table'
      )

      shiny::updateSelectInput(
        inputId = 'gseaprep_table_select',
        choices = c('Raw data table', 'Total normalized table', 'Z-scored table', 'Z-scored total normalized table'),
        selected = 'Total normalized table'
      )

      shiny::updateSelectInput(
        inputId = 'gseaprep_group_col',
        choices = colnames(r6$tables$raw_meta),
        selected = input$select_group_col
      )


    } else {
      print_tm(m, 'ERROR: Non-unique IDs in ID column')
      r6$tables$raw_meta = NULL
      shiny::updateSelectInput(
        inputId = 'select_meta_table',
        choices = c('Imported metadata table'),
        selected = 'Imported metadata table'
      )
    }
  })

  # Feature selection
  session$userData[[id]]$manual_feature_selection = shiny::observeEvent(input$feature_col_selection, {
    shiny::req(input$feature_col_selection)
    # Update class selection
    shiny::updateSelectizeInput(
      session = session,
      inputId = "class_selection",
      choices = unique(r6$tables$feature_table[, input$feature_col_selection]),
      selected = character(0)
    )

  })


  # Feature filters
  session$userData[[id]]$row_col_data = shiny::observeEvent(
    c(input$operation_order,
      input$batch_effect_correction,
      input$na_imputation,
      input$imputation_min_values,
      input$blank_multiplier,
      input$sample_threshold,
      input$group_threshold,
      input$normalise_to_col,
      input$reset_data_table,
      input$selection_drop,
      input$selection_keep,
      input$reset_meta), {

        shiny::req(r6$tables$raw_data)
        if (r6$preloaded_data) {return()}
        print_tm(m, 'Updating data tables')
        try_method(r6 = r6,
                   method_name = "set_raw_data",
                   operation_order = input$operation_order,
                   blank_multiplier = input$blank_multiplier,
                   sample_threshold = input$sample_threshold,
                   group_threshold = input$group_threshold,
                   imputation_method = input$na_imputation,
                   batch_effect_correction = input$batch_effect_correction,
                   norm_col = input$normalise_to_col)

        r6$derive_data_tables()

        # Update class selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "class_selection",
          selected = character(0)
        )

        # Update manual selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "manual_selection",
          choices = colnames(r6$tables$raw_data),
          selected = character(0)
        )

        # Update column selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "feature_col_selection",
          choices = colnames(r6$tables$feature_table),
          selected = character(0)
        )

        # Update column selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "feature_col_selection",
          choices = colnames(r6$tables$feature_table),
          selected = colnames(r6$tables$feature_table)[1]
        )

        # Update manual selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "manual_selection",
          choices = colnames(r6$tables$raw_data),
          selected = character(0)
        )

        data_table = table_switch(table_name = input$select_data_table, r6 = r6)

        if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {


          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = ncol(data_table),
            total = ncol(r6$tables$imp_data)
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "row_count_bar_data",
            value = nrow(data_table),
            total = nrow(r6$tables$imp_data)
          )
        }


        output$data_preview_table = renderDataTable({
          DT::datatable(utils::head(data_table[,1:100]), options = list(paging = TRUE, pageLength = 25))
        })
      })

  # Drop features
  session$userData[[id]]$feature_drop = shiny::observeEvent(input$drop_cols,{
    shiny::req(r6$tables$feature_table)
    print_tm(m, 'Dropping features')
    selected_species = rownames(r6$tables$feature_table)[which(r6$tables$feature_table[,input$feature_col_selection] %in% input$class_selection)]
    selected_species = unique(c(selected_species, input$manual_selection))
    r6$tables$raw_data = drop_cols(data_table = r6$tables$raw_data, cols = selected_species)
    r6$indices$excluded_cols = c(r6$indices$excluded_cols, selected_species)

    r6$derive_data_tables()

    # Update class selection
    shiny::updateSelectizeInput(
      session = session,
      inputId = "class_selection",
      selected = character(0)
    )

    # Update manual selection
    shiny::updateSelectizeInput(
      session = session,
      inputId = "manual_selection",
      choices = colnames(r6$tables$raw_data),
      selected = character(0)
    )

    if (input$select_data_table == 'Raw data table') {
      shinyWidgets::updateProgressBar(
        session = session,
        id = "col_count_bar",
        value = ncol(r6$tables$raw_data),
        total = ncol(r6$tables$imp_data)
      )
      shinyWidgets::updateProgressBar(
        session = session,
        id = "row_count_bar_data",
        value = nrow(r6$tables$raw_data),
        total = nrow(r6$tables$imp_data)
      )
    }
  })

  # Reset feature filter
  session$userData[[id]]$reset_data_table = shiny::observeEvent(input$reset_data_table,{
    r6$indices$excluded_cols = NULL
  })

  # Reset filters
  session$userData[[id]]$clear_data_filters = shiny::observeEvent(input$clear_data_filters,{
    # Reset class selection
    shiny::updateSelectizeInput(
      session = session,
      inputId = "class_selection",
      selected = character(0)
    )

    # Reset manual selection
    shiny::updateSelectizeInput(
      session = session,
      inputId = "manual_selection",
      selected = character(0)
    )
  })

  #--------------------------------------------- Feature metadata rendering ----
  output$up_feature_metadata_ui = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 8,
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::fileInput(
                inputId = ns("feat_add"),
                label = NULL,
                multiple = F,
                accept = c(".csv", ".tsv", ".txt", ".xlsx"),
                width = "100%"
              )
            ),
            shiny::column(
              width = 3,
              shiny::textInput(
                inputId = ns('feat_name_add'),
                label = NULL,
                width = '100%',
                placeholder = 'ex: feat_1'
              )
            ),
            shiny::column(
              width = 3,
              shiny::selectInput(
                inputId = ns('feat_table_select'),
                label = NULL,
                choices = c('Imported feature table', 'Feature table'),
                width = '100%'
              )
            ),
            shiny::column(
              width = 3,
              shiny::downloadButton(
                outputId = ns("download_feature_table"),
                label = "Download",
                style = "width:100%;"
              )
            )
          ),
          shiny::fluidRow(
            bs4Dash::box(
              id = ns('feat_table_preview_box'),
              title = 'Table preview',
              width = 12,
              DT::dataTableOutput(ns("feature_table_preview_table")),
              style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
              collapsible = T,
              collapsed  = T,
              maximizable = T,
              headerBorder = T
            )
          )
        ),
        shiny::column(
          width = 4,
          shiny::h3('Remove feature table'),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(inputId = ns('feat_name_del'),
                                 label = NULL,
                                 choices = names(r6$tables$external_feature_tables),
                                 selected = NULL,
                                 width = '100%')
            ),
            shiny::column(
              width = 6,
              shiny::actionButton(inputId = ns('feat_del'),
                                  label = 'Remove',
                                  width = '100%')
            )
          )
        )
      )
    )
  })

  #------------------------------------------------ Feature metadata server ----
  # Manage Feature tables
  session$userData[[id]]$feat_add = shiny::observeEvent(input$feat_add, {
    if (input$feat_name_add == "") {
      counter = 1
      name = paste0("feat_", counter)
      while (name %in% names(r6$tables$external_feature_tables)) {
        counter = counter + 1
        name = paste0("feat_", counter)
      }
    } else {
      name = input$feat_name_add
    }
    r6$add_feature_table(name = name,
                         feature_file = input$feat_add$datapath)

    shiny::updateSelectInput(
      inputId = 'feat_name_del',
      choices = names(r6$tables$external_feature_tables)
    )
    r6$derive_data_tables()

    if (input$feat_table_preview_box$collapsed) {
      bs4Dash::updateBox(id = 'feat_table_preview_box', action = 'toggle')
    }

    shiny::updateSelectInput(
      inputId = 'feat_table_select',
      selected = 'Feature table'
    )

  })

  session$userData[[id]]$feat_go_ont = shiny::observeEvent(input$feat_go_ont, {
    shiny::updateTextInput(
      inputId = 'go_name_add',
      placeholder = paste0('ex: ', input$feat_go_ont)
    )
  })

  session$userData[[id]]$add_go_table = shiny::observeEvent(input$add_go_table, {
    shinyjs::disable("add_go_table")
    table_name = input$go_name_add
    if (table_name == '') {
      counter = 1
      while (paste0(input$feat_go_ont, counter) %in% names(r6$tables$external_enrichment_tables)) {
        counter = counter + 1
      }
      table_name = paste0(input$feat_go_ont, counter)
    }
    r6$add_go_data(name = table_name,
                   feature_names = rownames(r6$tables$imp_feature_table),
                   keyType = input$select_feature_type,
                   ont = input$feat_go_ont,
                   pvalueCutoff = as.numeric(input$feat_go_ont_cutoff))
    shiny::updateSelectInput(
      inputId = 'go_remove_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )

    shiny::updateSelectInput(
      inputId = 'annotations_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )


    r6$derive_data_tables()
    shinyjs::enable("add_go_table")
  })

  session$userData[[id]]$feat_del = shiny::observeEvent(input$feat_del, {
    r6$del_feature_table(name = input$feat_name_del)
    r6$derive_data_tables()

    names_left = names(r6$tables$external_feature_tables)
    if (is.null(names_left)) {
      names_left = character(0)
    }
    shiny::updateSelectInput(
      inputId = 'feat_name_del',
      choices = names_left
    )
  })

  # Preview all / subset switch
  session$userData[[id]]$enrichment_upload_button = shiny::observeEvent(input$enrichment_upload_button, {
    print('upload enrichment table')
    shinyjs::disable("enrichment_upload_button")

    table_name = input$enrich_name_add
    if (table_name == '') {
      counter = 1
      while (paste0('ENR', counter) %in% names(r6$tables$external_enrichment_tables)) {
        counter = counter + 1
      }
      table_name = paste0('ENR', counter)
    }


    if (!is.null(input$go_association_table$datapath)) {
      association_table = soda_read_table(input$go_association_table$datapath, first_column_as_index = T)
    } else {
      association_table = NULL
    }

    if (!is.null(input$go_terms_table$datapath)) {
      terms_table = soda_read_table(input$go_terms_table$datapath, first_column_as_index = T)
    } else {
      terms_table = NULL
    }

    r6$upload_enrichment_data(name = table_name,
                              association_table = association_table,
                              terms_table = terms_table,
                              sep = '|')

    shiny::updateSelectInput(
      inputId = 'go_remove_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )

    shiny::updateSelectInput(
      inputId = 'annotations_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )

    shinyjs::enable("enrichment_upload_button")

  })


  # Preview all / subset switch
  session$userData[[id]]$feat_table_select = shiny::observeEvent(input$feat_table_select, {
    shiny::req(r6$tables$imp_data)

    data_table = table_switch(table_name = input$feat_table_select, r6 = r6)

    output$feature_table_preview_table = renderDataTable({
      DT::datatable(data_table, options = list(paging = TRUE, pageLength = 25))
    })

  })

  # Preview all / subset switch
  session$userData[[id]]$annotations_table_select = shiny::observeEvent(input$annotations_table_select, {
    shiny::req(r6$tables$imp_data)

    association_table = r6$tables$external_enrichment_tables[[input$annotations_table_select]]$association_table
    terms_table = r6$tables$external_enrichment_tables[[input$annotations_table_select]]$terms_table

    output$association_table_preview_table = renderDataTable({
      DT::datatable(association_table, options = list(paging = TRUE, pageLength = 25))
    })

    output$terms_table_preview_table = renderDataTable({
      DT::datatable(terms_table, options = list(paging = TRUE, pageLength = 25))
    })

  })

  # Download associations table
  dl_feature_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_feature_table = shiny::observeEvent(c(input$feat_table_select) , {
    dl_feature_table$name = timestamped_name("feature_table.csv")
    dl_feature_table$table = table_switch(table_name = input$feat_table_select,
                                          r6 = r6)
  })

  output$download_feature_table = shiny::downloadHandler(
    filename = shiny::reactive(dl_feature_table$name),
    content = function(file_name) {
      write.csv(dl_feature_table$table, file_name, na = "")
    }
  )

  # Download terms table
  dl_terms_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_terms_table = shiny::observeEvent(c(input$annotations_table_select) , {
    dl_terms_table$name = timestamped_name(paste0(input$annotations_table_select, "_terms.csv"))
    dl_terms_table$table = r6$tables$external_enrichment_tables[[input$annotations_table_select]]$terms_table
  })

  output$download_terms_table = shiny::downloadHandler(
    filename = shiny::reactive(dl_terms_table$name),
    content = function(file_name) {
      write.csv(dl_terms_table$table, file_name, na = "")
    }
  )

  # Remove annotations table
  session$userData[[id]]$remove_annotations_table = shiny::observeEvent(c(input$remove_annotations_table) , {
    print(input$annotations_table_select)
    r6$tables$external_enrichment_tables[[input$annotations_table_select]] = NULL
    if (length(r6$tables$external_enrichment_tables[[input$annotations_table_select]]) > 0) {
      shiny::updateSelectInput(
        inputId = 'annotations_table_select',
        choices = names(r6$tables$external_feature_tables)
      )
    } else {
      shiny::updateSelectInput(
        inputId = 'annotations_table_select',
        choices = character(0)
      )

      # association_table_preview_table

    }

  })


  # Download data table
  dl_data_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_datatable = shiny::observeEvent(c(input$select_data_table) , {
    shiny::req(r6$tables$raw_data)
    dl_data_table$name = timestamped_name(paste0(stringr::str_replace_all(input$select_data_table, " ", "_"), ".csv"))
    dl_data_table$table = table_switch(input$select_data_table, r6)
  })

  output$download_datatable = shiny::downloadHandler(
    filename = shiny::reactive(dl_data_table$name),
    content = function(file_name) {
      write.csv(dl_data_table$table, file_name, na = "")
    }
  )




  #------------------------------------------ Functional analysis rendering ----
  output$functional_analysis_ui = shiny::renderUI({

    bs4Dash::tabsetPanel(
      id = ns('functional_analysis_subui'),
      type = "tabs",
      shiny::tabPanel(
        title = "Functional comparison",

        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::h4('Sample selection'),
              shiny::selectInput(
                inputId = ns('gseaprep_table_select'),
                label = 'Select table',
                choices = NULL,
                width = '90%'
              ),
              shiny::selectInput(
                inputId = ns('gseaprep_group_col'),
                label = 'Group column',
                choices = NULL,
                width = '90%'
              ),
              shiny::selectInput(
                inputId = ns('gseaprep_groups'),
                label = 'Select two groups',
                choices = NULL,
                width = '90%',
                multiple = T
              )
            ),
            shiny::column(
              width = 8,
              shiny::h4('Feature selection'),
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::selectInput(
                    inputId = ns('gseaprep_test'),
                    label = 'Statistical test',
                    choices = c('Wilcoxon', 't-Test'),
                    selected = 't-Test',
                    width = '100%'
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::selectInput(
                    inputId = ns('gseaprep_adjustment'),
                    label = 'Adjustment',
                    choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                    selected = 'BH',
                    width = '100%'
                  )
                ),
                shiny::column(
                  width = 4,
                  shiny::selectInput(
                    inputId = ns('gseaprep_method'),
                    label = 'FC calculation',
                    choices = c('median', 'mean'),
                    selected = 'mean',
                    width = '100%'
                  )
                )
              ),
              shinyWidgets::radioGroupButtons(
                inputId = ns('fa_feature_selection'),
                label = NULL,
                choices = c('Statistical selection (ORA only, EA: all features)', 'User selection'),
                status = "info",
                justified = TRUE
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::numericInput(
                    inputId = ns('gseaprep_pval'),
                    label = "ORA p-value cut-off",
                    value = 0.05,
                    min = 0,
                    max = 0.99,
                    step = 0.01,
                    width = '100%'
                  ),
                  shiny::numericInput(
                    inputId = ns('or_fc_threshold'),
                    label = 'ORA fold change cut-off',
                    value = 2,
                    min = 0,
                    step = 0.05,
                    width = '100%'
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::selectInput(
                    inputId = ns('fa_feature_col'),
                    label = "Feature annotations column",
                    choices = NULL,
                    width = '100%'
                  ),
                  shiny::selectizeInput(
                    inputId = ns("fa_feature_values"),
                    label = "Group(s) to keep",
                    choices = NULL,
                    multiple = TRUE,
                    width = '100%'
                  )
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
                      shiny::selectInput(
                        inputId = ns('gsea_go'),
                        label = 'Feature sets',
                        choices = r6$hardcoded_settings$enrichment_analysis$terms,
                        selected = NULL,
                        width = '100%'
                      )
                    ),
                    shiny::column(
                      width = 6,
                      shiny::selectInput(
                        inputId = ns('gsea_adjustment'),
                        label = 'Adjustment (feature sets)',
                        choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                        selected = "BH",
                        width = '100%'
                      )
                    )
                  ),

                  shiny::fluidRow(
                    shiny::column(
                      width = 6,
                      shiny::numericInput(
                        inputId = ns('ea_min_size'),
                        label = 'Min. feature set size',
                        value = 3,
                        min = 1,
                        max = NA,
                        step = 5,
                        width = '100%'
                      )
                    ),
                    shiny::column(
                      width = 6,
                      shiny::numericInput(
                        inputId = ns('ea_max_size'),
                        label = 'Max. feature set size',
                        value = 800,
                        min = 1,
                        max = NA,
                        step = 50,
                        width = '100%'
                      )
                    )
                  ),

                  shiny::sliderInput(
                    inputId = ns('gsea_pval'),
                    label = 'p-value cutoff (feature sets)',
                    min = 0.01,
                    max = 0.9,
                    value = 0.05,
                    step = 0.01,
                    width = '100%'
                  )
                ),

                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    shiny::numericInput(
                      inputId = ns('ea_seed'),
                      label = "Seed",
                      value = 1,
                      min = 1,
                      step = 1,
                      width = '100%'
                    )
                  )
                ),

                collapsible = T,
                collapsed  = T,
                maximizable = F,
                headerBorder = T
              ),

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
                    shiny::selectInput(
                      inputId = ns('or_go_ont'),
                      label = 'Feature sets',
                      choices = r6$hardcoded_settings$over_representation_analysis$terms,
                      selected = NULL,
                      width = '100%'
                    )
                  ),
                  shiny::column(
                    width = 6,
                    shiny::selectInput(
                      inputId = ns('or_pval_adjustment'),
                      label = 'Adjustment (feature sets)',
                      choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                      selected = "BH",
                      width = '100%'
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      inputId = ns('ora_min_gssize'),
                      label = 'Min. feature set size',
                      value = 10,
                      min = 1,
                      max = NA,
                      step = 5,
                      width = '100%'
                    )
                  ),
                  shiny::column(
                    width = 6,
                    shiny::numericInput(
                      inputId = ns('ora_max_gssize'),
                      label = 'Max. feature set size',
                      value = 500,
                      min = 1,
                      max = NA,
                      step = 50,
                      width = '100%'
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::sliderInput(
                      inputId = ns('or_pval_cutoff'),
                      label = 'p-value cutoff (feature sets)',
                      min = 0.01,
                      max = 0.9,
                      value = 0.05,
                      step = 0.01,
                      width = '100%'
                    )
                  ),
                  shiny::column(
                    width = 6,
                    shiny::sliderInput(
                      inputId = ns('or_qval_cutoff'),
                      label = 'q-value cutoff (feature sets)',
                      min = 0.01,
                      max = 0.9,
                      value = 0.05,
                      step = 0.01,
                      width = '100%'
                    )
                  )
                ),

                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    shiny::numericInput(
                      inputId = ns('ora_seed'),
                      label = "Seed",
                      value = 1,
                      min = 1,
                      step = 1,
                      width = '100%'
                    )
                  )
                ),

                collapsible = T,
                collapsed  = T,
                maximizable = F,
                headerBorder = T
              ),



            )
          )
        )
      ),

      shiny::tabPanel(
        title = "Enrichment",
        shiny::uiOutput(
          outputId = ns('geneset_enrichment_ui')
        )
      ),
      shiny::tabPanel(
        title = "Over-representation",
        shiny::uiOutput(
          outputId = ns('over_representation_ui')
        )
      )
    )



  })
  #--------------------------------------------- Functional analysis server ----

  # GSEA groups
  session$userData[[id]]$gsea_groups = shiny::observeEvent(input$gseaprep_group_col,{
    shiny::req(input$gseaprep_group_col)
    shiny::updateSelectInput(
      inputId = 'gseaprep_groups',
      choices = unique(r6$tables$raw_meta[,input$gseaprep_group_col]),
      selected = unique(r6$tables$raw_meta[,input$gseaprep_group_col])[c(1,2)]
    )
  })

  # Entering FA tab observe
  shiny::observe({
    shiny::req(input$skeleton_ui)
    if (input$skeleton_ui == "Functional analysis") {

      shiny::updateSelectInput(
        inputId = 'gseaprep_table_select',
        choices = c('Raw data table', 'Total normalized table', 'Z-scored table', 'Z-scored total normalized table'),
        selected = 'Total normalized table'
      )

      shiny::updateSelectInput(
        inputId = 'gseaprep_group_col',
        choices = colnames(r6$tables$raw_meta),
        selected = input$select_group_col
      )

      shiny::updateSelectInput(
        inputId = 'fa_feature_col',
        choices = colnames(r6$tables$feature_table)
      )

      shiny::updateSelectInput(
        inputId = 'gsea_go',
        choices = unique(c(r6$hardcoded_settings$enrichment_analysis$terms, colnames(r6$tables$feature_table)))
      )

      shiny::updateSelectInput(
        inputId = 'or_go_ont',
        choices = unique(c(r6$hardcoded_settings$over_representation_analysis$terms, colnames(r6$tables$feature_table)))
      )

    }
  })

  # Entering Feature tab observe
  shiny::observe({
    shiny::req(input$skeleton_ui)
    if (input$skeleton_ui == "Data") {
      shiny::updateSelectizeInput(
        inputId = 'feature_col_selection',
        choices = colnames(r6$tables$feature_table)
      )
    }
  })


  # Radio button detect
  session$userData[[id]]$fa_feature_selection_detect = shiny::observeEvent(input$fa_feature_selection, {
    if (input$fa_feature_selection == 'Statistical selection (ORA only, EA: all features)') {
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

  # Update groups to keep
  session$userData[[id]]$fa_feature_col_detect = shiny::observeEvent(input$fa_feature_col, {
    shiny::req(input$fa_feature_col)
    shiny::updateSelectizeInput(
      inputId = "fa_feature_values",
      choices = unique(r6$tables$feature_table[,input$fa_feature_col])
    )
  })

  #----------------------------------------------- Visualize data rendering ----

  output$visualize_data_ui = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        # First column with the table input and preview of the raw data
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                             label = NULL,
                                             status = "default",
                                             choices = proteomics_plot_list(),
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
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
  })






  #-------------------------------------------------- Visualize data server ----

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
  volcano_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
  samples_correlation_events(r6, dimensions_obj, color_palette, input, output, session)
  feature_correlation_events(r6, dimensions_obj, color_palette, input, output, session)
  pca_events(r6, dimensions_obj, color_palette, input, output, session)


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
      plot_one_prot(r6 = r6,
                    dimensions_obj = dimensions_obj,
                    selection_list = input$showPlots,
                    input = input,
                    output = output,
                    session = session)
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "showPlots",
        disabledChoices = input$showPlots
      )

    } else if (length(input$showPlots) == 2) {
      plot_two_prot(r6 = r6,
                    dimensions_obj = dimensions_obj,
                    selection_list = input$showPlots,
                    input = input,
                    output = output,
                    session = session)

    } else if (length(input$showPlots) == 3) {
      plot_three_prot(r6 = r6,
                      dimensions_obj = dimensions_obj,
                      selection_list = input$showPlots,
                      input = input,
                      output = output,
                      session = session)

    } else if (length(input$showPlots) >= 4) {
      plot_four_prot(r6 = r6,
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
    }

  })


  session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
    print_tm(m, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "showPlots",
      disabled = FALSE,
      selected = character(0))
    output$plotbox_field = shiny::renderUI(
      NULL
    )
  })


  #------------------------------------------ Enrichment analysis rendering ----

  output$geneset_enrichment_ui = shiny::renderUI({
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
  })

  #--------------------------------------------- Enrichment analysis server ----

  session$userData[[id]]$select_feature_type = shiny::observeEvent(input$select_feature_type, {
    if (r6$preloaded_data) {return()}
    print_tm(m, paste0('FA: feature ID type set to ', input$select_feature_type))
    r6$indices$feature_id_type = input$select_feature_type
  })


  session$userData[[id]]$run_gsea = shiny::observeEvent(input$run_gsea, {
    shiny::req(length(input$gseaprep_groups) == 2)
    print_tm(m, "EA started")
    shinyjs::disable("run_gsea")
    waiter::waiter_show(
      id = ns("run_gsea"),
      html = spin_circle(),
      color = "#00A86B"
    )

    if (input$fa_feature_selection == 'Statistical selection (ORA only, EA: all features)') {
      input_table = table_switch(input$gseaprep_table_select, r6)
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
      input_table = table_switch(input$gseaprep_table_select, r6)
      selected_features = rownames(r6$tables$feature_table)[which(r6$tables$feature_table[, input$fa_feature_col] %in% input$fa_feature_values)]
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


      results = nrow(r6$tables$ea_object)
      if (results == 0) {
        print_tm(m, "EA failed: no term enriched under specific pvalueCutoff")
      } else {
        print_tm(m, paste0("EA successful: ", results, ' terms.'))
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

  #------------------------------------ Over representation analysis server ----

  session$userData[[id]]$run_ora = shiny::observeEvent(input$run_ora, {
    shiny::req(length(input$gseaprep_groups) == 2)
    print_tm(m, "ORA started")
    shinyjs::disable("run_ora")
    waiter::waiter_show(
      id = ns("run_ora"),
      html = spin_circle(),
      color = "#00A86B"
    )

    if (input$fa_feature_selection == 'Statistical selection (ORA only, EA: all features)') {
      input_table = table_switch(input$gseaprep_table_select, r6)
      selected_features = NULL
    } else if (input$fa_feature_selection == 'User selection') {
      if (length(input$fa_feature_values) == 0) {
        print_tme(m, 'User selection filtering selected, but not feature groups selected.')
        waiter::waiter_hide(
          id = ns("run_ora")
        )
        shinyjs::enable("run_ora")
        return()
      }
      input_table = table_switch(input$gseaprep_table_select, r6)
      selected_features = rownames(r6$tables$feature_table)[which(r6$tables$feature_table[, input$fa_feature_col] %in% input$fa_feature_values)]
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

      if (!is.null(r6$tables$ora_object)) {
        results = nrow(r6$tables$ora_object)
        if (results == 0) {
          print_tm(m, 'WARNING: no over-representation under selected parameters')
        } else {
          print_tm(m, paste0('Over-representation successful: ', results, ' terms'))
        }
        print_tm(m, "ORA finished")
      } else {
        print_tm(m, 'No over represented features, returning.')
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


  session$userData[[id]]$clear_plots_gsea = shiny::observeEvent(input$clear_plots_gsea, {
    print_tm(m, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "show_plots_gsea",
      disabled = FALSE,
      selected = character(0))
    output$gsea_plotbox_field = shiny::renderUI(
      NULL
    )
  })



  #------------------------------------------ Over-representation rendering ----

  output$over_representation_ui = shiny::renderUI({
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
  })

  #--------------------------------------------- Over-representation server ----

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
    print_tm(m, "Clearing plots")
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
