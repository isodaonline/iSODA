#------------------------------------------------------------------ MOFA UI ----
mofa_ui = function(id) {
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",

    #------------------------------------------------------------ Setup tab ----
    shiny::tabPanel(
      title = "Setup",
      shiny::fluidRow(
        shiny::column(
          width=12,

          # Modules table
          shiny::br(),
          bs4Dash::box(
            id = ns('module_table_box'),
            title = 'Available data',
            width = 12,
            DT::dataTableOutput(outputId = ns("modules_table"))

          )
        )
      ),
      bs4Dash::box(
        id = ns('mofa_options_box'),
        title = 'Options',
        width = 12,
        collapsed = T,
        shiny::fluidRow(

          shiny::column(
            width = 4,
            shiny::h3("Data options"),

            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinyWidgets::prettySwitch(inputId = ns("data_scale_views"),
                                           label = "Scale views",
                                           value = FALSE,
                                           fill = TRUE, status = "primary")
              ),
              shiny::column(
                width = 6,
                shinyWidgets::prettySwitch(inputId = ns("data_scale_groups"),
                                           label = "Scale groups",
                                           value = FALSE,
                                           fill = TRUE, status = "primary")
              ),
              shiny::numericInput(
                inputId = ns('mofa_seed'),
                label = 'Seed',
                value = 1,
                min = 1,
                step = 1,
                width = '80%'
              )
            )
          ),
          shiny::column(
            width = 4,
            shiny::h3("Model options"),

            shiny::fluidRow(
              shiny::numericInput(
                inputId = ns("model_num_factors"),
                label = "Number of factors",
                value = 6,
                min = 1,
                width = "100%"
              ),
              shiny::column(
                width = 6,
                shinyWidgets::prettySwitch(inputId = ns("model_spikeslab_factors"),
                                           label = "Spikeslab factors",
                                           value = FALSE,
                                           fill = TRUE, status = "primary"),
                shinyWidgets::prettySwitch(inputId = ns("model_ard_factors"),
                                           label = "ARD factors",
                                           value = FALSE,
                                           fill = TRUE, status = "primary")
              ),
              shiny::column(
                width = 6,
                shinyWidgets::prettySwitch(inputId = ns("model_spikeslab_weights"),
                                           label = "Spikeslab weights",
                                           value = TRUE,
                                           fill = TRUE, status = "primary"),
                shinyWidgets::prettySwitch(inputId = ns("model_ard_weights"),
                                           label = "ARD weights",
                                           value = TRUE,
                                           fill = TRUE, status = "primary")
              )
            )
          ),
          shiny::column(
            width = 4,
            shiny::h3("Training options"),

            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::numericInput(inputId = ns("training_iterations"),
                                    label = "Max iterations",
                                    value = 1000,
                                    width = "100%"),
                shiny::numericInput(inputId = ns("training_start_elbo"),
                                    label = "startELBO",
                                    value = 1,
                                    width = "100%"),
                shinyWidgets::prettySwitch(inputId = ns("training_stochastic"),
                                           label = "Stochastic",
                                           value = FALSE,
                                           fill = TRUE, status = "primary")
              ),
              shiny::column(
                width = 6,
                shiny::selectInput(inputId = ns("training_convergence_mode"),
                                   label = "Convergence mode",
                                   choices = c("fast", "medium", "slow"),
                                   selected = "fast",
                                   width = "100%"),
                shiny::numericInput(inputId = ns("training_freq_elbo"),
                                    label = "freqELBO",
                                    value = 5,
                                    width = "100%")
              )
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::br()
        ),
        shiny::column(width = 3),
        shiny::column(
          width = 6,
          shiny::actionButton(
            inputId = ns('run_mofa'),
            label = "Run MOFA",
            icon = icon("play"),
            style ="color: #fff; background-color: #00A86B; border-color: #00A86B",
            width = '100%'
          )
        ),
        shiny::column(width = 3)
      )
    ),
    shiny::tabPanel(
      title = "Visualization",
      shiny::fluidRow(
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_mofa"),
                                             label = NULL,
                                             status = "default",
                                             choices = get_mofa_plot_list(),
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
  )

}


#-------------------------------------------------------------- MOFA server ----
mofa_server = function(id, r6, module_controler, main_input) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns


      input_modules = shiny::reactiveValues(
        table = NULL,
        uuids = NULL,
        idx_list = NULL
      )


      # Omics modules loaded (in table)
      session$userData[[id]]$snf_observe = shiny::observe({

        shiny::req(main_input$main_sidebar)
        if (main_input$main_sidebar == "mofa_tab") {

          run = NULL
          exp_name = NULL
          exp_type = NULL
          sample_count = NULL
          table_select_list = NULL
          likelihood_list = NULL
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
              likelihood_list = c(likelihood_list,
                                    base::sprintf(
                                      '<select id="%s-modLikelihood-%s" class="shiny-input-select"><option value="gaussian" selected>gaussian</option><option value="poisson">poisson</option><option value="bernoulli">bernoulli</option></select>',
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
            "Table" = table_select_list,
            "Likelihood" = likelihood_list
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

      # Start MOFA
      session$userData[[id]]$run_mofa = shiny::observeEvent(input$run_mofa, {

        # Disable button while running
        shinyjs::disable("run_mofa")
        waiter::waiter_show(
          id = ns("run_mofa"),
          html = spin_circle(),
          color = "#00A86B"
        )

        # Check on experiment counts
        if (length(input_modules$idx_list) < 2) {
          waiter::waiter_hide(id = ns("run_mofa"))
          shinyjs::enable("run_mofa")
          print_tme(m = id, e = 'Upload more than one experiment.')
        }
        print_tm(r6$name, "MOFA: starting...")

        r6$tables$metadata = NULL
        r6$tables$omics_tables = list()

        m = input_modules$table
        uuids = input_modules$uuids
        idx_list = input_modules$idx_list
        likelihoods = list()

        base::withCallingHandlers({
          for (i in base::seq_len(nrow(m))) {
            uuid = uuids[i]
            idx = idx_list[i]

            if (input[[paste0("modSelected-", uuid)]]) {
              exp_name = module_controler$exp_r6[[idx]]$name
              exp_type = module_controler$exp_r6[[idx]]$type
              exp_table_name = input[[paste0("modTable-", uuid)]]
              likelihoods[[exp_name]] = input[[paste0("modLikelihood-", uuid)]]
              data_table = module_controler$exp_r6[[idx]]$table_switch_local(exp_table_name)
              sample_meta = module_controler$exp_r6[[idx]]$tables$raw_meta
              feature_meta = module_controler$exp_r6[[idx]]$tables$feature_table
              r6$add_data(name = exp_name,
                          data_table = data_table)
              r6$add_sample_meta(name = exp_name,
                                 sample_meta = sample_meta)
              r6$add_feature_data(name = exp_name,
                                  feature_data = feature_meta)
              print_tm(r6$name, paste0('Added ',exp_name, ' (', exp_type, ')'))
            }
          }
          r6$clean_datasets()

        },warning = function(w){
          print_tmw(r6$name, w)
        }, error=function(e){
          waiter::waiter_hide(id = ns("run_mofa"))
          shinyjs::enable("run_mofa")
          print_tme(r6$name, e)
        }
        )
        # Check if only a single dataset was selected
        if (length(r6$tables$omics_tables) < 2) {
          waiter::waiter_hide(id = ns("run_mofa"))
          shinyjs::enable("run_mofa")
          print_tme(m = id, e = 'Select more than one experiment.')
        }
        base::withCallingHandlers({
          r6$create_mofa_object()
          print_tm(r6$name, "MOFA: training model...")
          r6$prepare_mofa(scale_views = input$data_scale_views,
                          scale_groups = input$data_scale_groups,
                          likelihoods = likelihoods,
                          num_factors = as.numeric(input$model_num_factors),
                          spikeslab_factors = input$model_spikeslab_factors,
                          spikeslab_weights = input$model_spikeslab_weights,
                          ard_factors = input$model_ard_factors,
                          ard_weights = input$model_ard_weights,
                          maxiter = as.numeric(input$training_iterations),
                          convergence_mode = input$training_convergence_mode,
                          startELBO = as.numeric(input$training_start_elbo),
                          freqELBO = as.numeric(input$training_freq_elbo),
                          stochastic = input$training_stochastic)
          r6$train_model(mofa_object = r6$mofa_objects$pretrained,
                         outfile = base::file.path("./models", timestamped_name("model.hdf5")),
                         save_data = T,
                         seed = input$mofa_seed)
          r6$add_metadata_to_mofa()
        },warning = function(w){
          print_tmw(r6$name, w)
        }, error=function(e){
          waiter::waiter_hide(id = ns("run_mofa"))
          shinyjs::enable("run_mofa")
          print_tme(r6$name, e)
        })


        print_tm(r6$name, "MOFA: model ready.")

        # Enable button
        waiter::waiter_hide(
          id = ns("run_mofa")
        )
        shinyjs::enable("run_mofa")
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
      explained_variance_events(r6, dimensions_obj, color_palette, input, output, session)
      factor_plot_events(r6, dimensions_obj, color_palette, input, output, session)
      combined_factors_plot_events(r6, dimensions_obj, color_palette, input, output, session)
      feature_weights_events(r6, dimensions_obj, color_palette, input, output, session)
      feature_top_weights_events(r6, dimensions_obj, color_palette, input, output, session)
      mofa_heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
      scatter_plot_events(r6, dimensions_obj, r6_settings, input, output, session)


      # Plot selection
      session$userData[[id]]$show_plots_mofa = shiny::observeEvent(input$show_plots_mofa, {

        # Update x dimensions in px and bs, and y in px
        if (length(input$show_plots_mofa) < 2) {
          dimensions_obj$xbs = 12
          dimensions_obj$xpx = shinybrowser::get_width()
          dimensions_obj$ypx = shinybrowser::get_height()
        } else if (length(input$show_plots_mofa) == 2) {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()
        } else {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()/2.2
        }

        # Plots selected: 1 to 4
        if (length(input$show_plots_mofa) == 1) {
          mofa_plot_one(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_mofa,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_mofa) == 2) {
          mofa_plot_two(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_mofa,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_mofa) == 3) {
          mofa_plot_three(r6 = r6,
                          dimensions_obj = dimensions_obj,
                          selection_list = input$show_plots_mofa,
                          input = input,
                          output = output,
                          session = session)

        } else if (length(input$show_plots_mofa) >= 4) {
          mofa_plot_four(r6 = r6,
                         dimensions_obj = dimensions_obj,
                         selection_list = input$show_plots_mofa,
                         input = input,
                         output = output,
                         session = session)

          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_mofa",
            disabledChoices = setdiff(unname(get_mofa_plot_list()), input$show_plots_mofa)
          )

        }



        if ((length(input$show_plots_mofa) > 1) & (length(input$show_plots_mofa) < 4)) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_mofa",
            disabledChoices = NULL
          )
        } else if (length(input$show_plots_mofa) == 1) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_mofa",
            disabledChoices = input$show_plots_mofa
          )
        }
      })

      session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
        print_tm(r6$name, "Clearing plots")
        shinyWidgets::updateCheckboxGroupButtons(
          session = session,
          inputId = "show_plots_mofa",
          disabled = FALSE,
          selected = character(0))
        output$plotbox_field = shiny::renderUI(
          NULL
        )
      })

    }
  )
}


