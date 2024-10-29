#------------------------------------------------------------------ SNF UI ----
snf_ui = function(id) {
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",

    #------------------------------------------------------------ Setup tab ----
    shiny::tabPanel(
      title = "Setup",
      shiny::uiOutput(
        outputId = ns('snf_upload')
      )
    ),
    shiny::tabPanel(
      title = "Visualization",
      shiny::uiOutput(
        outputId = ns('snf_visualization')
      )
    )
  )

}

#--------------------------------------------------------------- SNF server ----
snf_server = function(id, r6, module_controler, main_input) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      
      output$snf_upload = shiny::renderUI({
        shiny::tagList(shiny::fluidRow(
          shiny::column(
            width=12,
            
            # Modules table
            shiny::br(),
            bs4Dash::box(
              id = ns('module_table_box'),
              title = 'Available data',
              width = 12,
              DT::dataTableOutput(outputId = ns("modules_table"))
              
            ),
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 3),
          shiny::column(
            width = 6,
            shiny::actionButton(
              inputId = ns('run_snf'),
              label = "Run SNF",
              icon = icon("play"),
              style ="color: #fff; background-color: #00A86B; border-color: #00A86B",
              width = '100%'
              )
            ),
          shiny::column(width = 3)
          )
        )
      })
      output$snf_visualization = shiny::renderUI({
        shiny::tagList(
          shiny::fluidRow(
            shiny::column(
              width = 11,
              shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_snf"),
                                                 label = NULL,
                                                 status = "default",
                                                 choices = get_snf_plot_list(),
                                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                                 size = "normal",
                                                 justified = TRUE)
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
          shiny::uiOutput(
            outputId = ns("plotbox_field")
          )
        )
      })
      
      input_modules = shiny::reactiveValues(
        table = NULL,
        uuids = NULL,
        idx_list = NULL
      )

      # Omics modules loaded (in table)
      session$userData[[id]]$snf_observe = shiny::observe({

        shiny::req(main_input$main_sidebar)
        if (main_input$main_sidebar == "snf_tab") {

          run = NULL
          exp_name = NULL
          exp_type = NULL
          sample_count = NULL
          table_select_list = NULL
          uuid_list = NULL
          idx_list = NULL

          for (i in 1:length(module_controler$exp_r6)){
            if (!is.null(module_controler$exp_r6[[i]]$tables$raw_data)) {

              name = module_controler$exp_r6[[i]]$name
              uuid = generate_id()
              run = c(run,
                      base::sprintf(
                        '<input id="%s-modSelected-%s" type="checkbox" class="shiny-input-checkbox" checked/>',
                        id, uuid
                      )
              )
              exp_name = c(exp_name, name)
              exp_type = c(exp_type,  module_controler$exp_r6[[i]]$type)
              sample_count = c(sample_count, nrow(module_controler$exp_r6[[i]]$tables$raw_meta))
              table_select_list = c(table_select_list,
                                    base::sprintf(
                                      '<select id="%s-modTable-%s" class="shiny-input-select"><option value="Raw data table">Raw data table</option><option value="Total normalized table">Total normalized table</option><option value="Z-scored table" selected>Z-scored table</option><option value="Z-scored total normalized table">Z-scored total normalized table</option></select>',
                                      id, uuid
                                    )
              )
              uuid_list = c(uuid_list, uuid)
              idx_list = c(idx_list, i)
            }
          }


          m = base::data.frame(
            "Run" = run,
            "Name" = exp_name,
            "Type" = exp_type,
            "Samples" = sample_count,
            "Table" = table_select_list
          )

          input_modules$table = m
          input_modules$uuids = uuid_list
          input_modules$idx_list = idx_list

          output$modules_table = DT::renderDataTable(
            m, escape = FALSE, selection = 'none', server = FALSE,
            options = list(dom = 't', paging = FALSE, ordering = FALSE), rownames= FALSE,
            callback = JS("table.rows().every(function() {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-checkbox');
          $this.addClass('shiny-input-select');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
          )
        }
      })

      # Start SNF
      session$userData[[id]]$run_snf = shiny::observeEvent(input$run_snf, {

        # Disable button while running
        shinyjs::disable("run_snf")
        waiter::waiter_show(
          id = ns("run_snf"),
          html = spin_circle(),
          color = "#00A86B"
        )

        # Check on experiment counts
        if (length(input_modules$idx_list) < 2) {
          waiter::waiter_hide(id = ns("run_snf"))
          shinyjs::enable("run_snf")
          print_tme(m = id, e = 'Upload more than one experiment.')
        }
        print_tm(r6$name, "SNF: starting...")

        r6$tables$metadata = NULL
        r6$tables$omics_tables = list()

        m = input_modules$table
        uuids = input_modules$uuids
        idx_list = input_modules$idx_list

        base::withCallingHandlers({
          for (i in base::seq_len(nrow(m))) {
            uuid = uuids[i]
            idx = idx_list[i]
            if (input[[paste0("modSelected-", uuid)]]) {
              exp_name = module_controler$exp_r6[[idx]]$name
              exp_type = module_controler$exp_r6[[idx]]$type
              exp_table_name = input[[paste0("modTable-", uuid)]]
              exp_table = module_controler$exp_r6[[idx]]$table_switch_local(exp_table_name)
              exp_meta = module_controler$exp_r6[[idx]]$tables$raw_meta
              r6$add_data(name = exp_name,
                          data_table = exp_table)
              r6$add_meta(name = exp_name,
                          meta_table = exp_meta)
              print_tm(r6$name, paste0('Added ',exp_name, ' (', exp_type, ')'))
            }
          }
          r6$clean_datasets()

        },warning = function(w){
          print_tmw(r6$name, w)
        }, error=function(e){
          waiter::waiter_hide(id = ns("run_snf"))
          shinyjs::enable("run_snf")
          print_tme(r6$name, e)
        }
        )
        # Check if only a single dataset was selected
        if (length(r6$tables$omics_tables) < 2) {
          waiter::waiter_hide(id = ns("run_snf"))
          shinyjs::enable("run_snf")
          print_tme(m = id, e = 'Select more than one experiment.')
        }

        print_tm(r6$name, "SNF: ready.")

        # Enable button
        waiter::waiter_hide(id = ns("run_snf"))
        shinyjs::enable("run_snf")
      })



      # #----------------------------------------------------------- Plotting ----

      # Initialise dimensions object
      dimensions_obj = shiny::reactiveValues()
      shiny::observe({
        dimensions_obj$x_box = module_controler$dims$x_box
        dimensions_obj$y_box = module_controler$dims$y_box
        dimensions_obj$x_plot = module_controler$dims$x_plot
        dimensions_obj$y_plot = module_controler$dims$y_plot
        dimensions_obj$x_plot_full = module_controler$dims$x_plot_full
        dimensions_obj$y_plot_full = module_controler$dims$y_plot_full
        dimensions_obj$xpx_total = shinybrowser::get_width()
        dimensions_obj$ypx_total = shinybrowser::get_height()
        dimensions_obj$xbs = 12
        dimensions_obj$xpx = shinybrowser::get_width()
        dimensions_obj$ypx = shinybrowser::get_height()
      })


      color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = 'Spectral'))(40)
      # # Plotting events
      sample_clustering_1_events(r6, dimensions_obj, color_palette, input, output, session)
      sample_clustering_2_events(r6, dimensions_obj, color_palette, input, output, session)
      similarity_network_1_events(r6, dimensions_obj, color_palette, input, output, session)
      similarity_network_2_events(r6, dimensions_obj, color_palette, input, output, session)
      fusion_heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
      similarity_network_fusion_events(r6, dimensions_obj, color_palette, input, output, session)


      # Plot selection
      session$userData[[id]]$show_plots_snf = shiny::observeEvent(input$show_plots_snf, {

        # Update x dimensions in px and bs, and y in px
        if (length(input$show_plots_snf) < 2) {
          dimensions_obj$xbs = 12
          dimensions_obj$xpx = shinybrowser::get_width()
          dimensions_obj$ypx = shinybrowser::get_height()
        } else if (length(input$show_plots_snf) == 2) {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()
        } else {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()/2.2
        }

        # Plots selected: 1 to 4
        if (length(input$show_plots_snf) == 1) {
          snf_plot_one(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_snf,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_snf) == 2) {
          snf_plot_two(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_snf,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_snf) == 3) {
          snf_plot_three(r6 = r6,
                          dimensions_obj = dimensions_obj,
                          selection_list = input$show_plots_snf,
                          input = input,
                          output = output,
                          session = session)

        } else if (length(input$show_plots_snf) >= 4) {
          snf_plot_four(r6 = r6,
                         dimensions_obj = dimensions_obj,
                         selection_list = input$show_plots_snf,
                         input = input,
                         output = output,
                         session = session)

          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_snf",
            disabledChoices = setdiff(unname(get_snf_plot_list()), input$show_plots_snf)
          )
        }

        if ((length(input$show_plots_snf) > 1) & (length(input$show_plots_snf) < 4)) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_snf",
            disabledChoices = NULL
          )
        } else if (length(input$show_plots_snf) == 1) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_snf",
            disabledChoices = input$show_plots_snf
          )
        }
      })

      session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
        print_tm(r6$name, "Clearing plots")
        shinyWidgets::updateCheckboxGroupButtons(
          session = session,
          inputId = "show_plots_snf",
          disabled = FALSE,
          selected = character(0))
        output$plotbox_field = shiny::renderUI(
          NULL
        )
      })


    }
  )
}
