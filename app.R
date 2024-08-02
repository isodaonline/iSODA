# shiny app
library(shiny)
library(shinyjs)
library(bs4Dash)
library(shinyWidgets)
library(shinybrowser)
library(shinymanager)
library(spsComps)
library(waiter)

# Plotting
library(ggplot2)
library(gridExtra)
library(plotly)
library(visNetwork)
library(heatmaply)
library(ggpubr)
library(ggupset)
library(networkD3)
library(igraph)
library(ellipse)

# text
library(stringr)
library(stringi)
library(markdown)

# Tables
library(DT)
library(readxl)
library(Matrix)

# Colors
library(grDevices)
library(RColorBrewer)
library(viridisLite)

# Statistics
library(stats)
library(glmnet)
library(pcaMethods)
library(scales)

# Omics
library(org.Hs.eg.db)
library(ggtree) # devtools::install_github("YuLab-SMU/ggtree")
library(clusterProfiler)
library(enrichplot)
library(ggridges)
library(MOFA2)
library(basilisk)
library(SNFtool)
library(sva)

# General
library(reshape2)
library(dplyr)
library(xfun)
library(R6)

# New
library(ggdendro)

# Use basilisk
# reticulate::use_condaenv(condaenv = 'mofa_1')

#------------------------------------------------------------- Setup header ----
header_ui = function() {

  # Get data from the description file
  desc = read.delim("DESCRIPTION", header = FALSE)

  # Extract and capitalise name
  name = stringr::str_split(desc[1,1], ":")[[1]][2]
  name = trimws(name)

  # Extract version
  version = gsub("[^0-9.-]", "", desc[3,1])
  header = paste(name, "|", version, sep = " ")
  bs4Dash::dashboardHeader(title = header)
}

#------------------------------------------------------------ Setup sidebar ----

sidebar_ui = function() {
  bs4Dash::bs4DashSidebar(
    bs4Dash::bs4SidebarMenu(
      id = "main_sidebar",

      # Single-omics menu
      bs4Dash::bs4SidebarMenuItem(
        text = "Single-omics",
        tabName = "start",
        icon = shiny::icon("list")
        ),

      bs4Dash::sidebarMenuOutput("exp_1"),
      bs4Dash::sidebarMenuOutput("exp_2"),
      bs4Dash::sidebarMenuOutput("exp_3"),
      bs4Dash::sidebarMenuOutput("exp_4"),
      bs4Dash::sidebarMenuOutput("exp_5"),
      bs4Dash::sidebarMenuOutput("exp_6"),

      bs4Dash::bs4SidebarMenuItem(
        text = "Multi-omics",
        tabName = "multi_omics",
        icon = shiny::icon("list"),
        bs4Dash::bs4SidebarMenuSubItem(
          text = "MOFA",
          tabName = "mofa_tab",
          icon = shiny::icon("circle")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          text = "SNF",
          tabName = "snf_tab",
          icon = shiny::icon("circle")
        )
      ),

      bs4Dash::bs4SidebarMenuItem(
        text = "Help",
        tabName = "help",
        icon = shiny::icon("question"),
        bs4Dash::bs4SidebarMenuSubItem(
          text = "Logs",
          tabName = "logs_tab"
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          text = "About",
          tabName = "about"
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          text = 'Input files',
          tabName = 'help_start',
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          text = 'Single omics',
          tabName = 'help_single_omics',
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          text = 'Multi-omics',
          tabName = 'help_multi_omics',
        )
      )
    )
  )
}


#--------------------------------------------------------------- Setup body ----
body_ui = function() {
  bs4Dash::dashboardBody(

    # Detect UI functions
    shinyjs::useShinyjs(),
    shinybrowser::detect(),
    waiter::autoWaiter(html = spin_3k(), color = "rgba(255, 255, 255, 0)"),

    bs4Dash::tabItems(

      # Start page
      bs4Dash::tabItem(
        tabName = "start",
        start_ui(id = 'mod_start')
      ),

      bs4Dash::tabItem(
        tabName = "exp_1",
        experiment_ui(id = 'mod_exp_1')
      ),

      bs4Dash::tabItem(
        tabName = "exp_2",
        experiment_ui(id = 'mod_exp_2')
      ),

      bs4Dash::tabItem(
        tabName = "exp_3",
        experiment_ui(id = 'mod_exp_3')
      ),

      bs4Dash::tabItem(
        tabName = "exp_4",
        experiment_ui(id = 'mod_exp_4')
      ),

      bs4Dash::tabItem(
        tabName = "exp_5",
        experiment_ui(id = 'mod_exp_5')
      ),

      bs4Dash::tabItem(
        tabName = "exp_6",
        experiment_ui(id = 'mod_exp_6')
      ),

      bs4Dash::tabItem(
        tabName = "mofa_tab",
        mofa_ui(id = "mofa")
      ),

      bs4Dash::tabItem(
        tabName = "snf_tab",
        snf_ui(id = "snf")
      ),

      bs4Dash::tabItem(
        tabName = "logs_tab",
        logs_ui(id = "logs")
      ),

      bs4Dash::tabItem(
        tabName = "about",
        about_ui(id = 'mod_about')
      ),

      bs4Dash::tabItem(
        tabName = "help_start",
        help_start_ui(id = 'mod_help_start')
      ),

      bs4Dash::tabItem(
        tabName = "help_single_omics",
        help_single_omics_ui(id = 'mod_help_single_omics')
      ),

      bs4Dash::tabItem(
        tabName = "help_multi_omics",
        help_multi_omics_ui(id = 'mod_help_multi_omics')
      )

    )
  )
}

#----------------------------------------------------------------------- UI ----
header = header_ui()
sidebar = sidebar_ui()
body = body_ui()

ui = bs4Dash::dashboardPage(header, sidebar, body)

#------------------------------------------------------------------- Server ----

server = function(input, output, session) {

  # Create logfile
  log_file <<- paste0("./logs/", get_day_time_code(), ".log")
  if (!base::file.exists("./logs/")) {
    base::dir.create("./logs/")
  }
  if (!base::file.exists("./models/")) {
    base::dir.create("./models/")
  }

  options(shiny.maxRequestSize=300*1024^2)

  module_controler = shiny::reactiveValues(

    slot_taken = list(
      'exp_1' = FALSE,
      'exp_2' = FALSE,
      'exp_3' = FALSE,
      'exp_4' = FALSE,
      'exp_5' = FALSE,
      'exp_6' = FALSE
    ),

    module_loaded = list(
      'exp_1' = FALSE,
      'exp_2' = FALSE,
      'exp_3' = FALSE,
      'exp_4' = FALSE,
      'exp_5' = FALSE,
      'exp_6' = FALSE
    ),

    exp_types = list(
      'exp_1' = NULL,
      'exp_2' = NULL,
      'exp_3' = NULL,
      'exp_4' = NULL,
      'exp_5' = NULL,
      'exp_6' = NULL
    ),

    exp_names = list(
      'exp_1' = NULL,
      'exp_2' = NULL,
      'exp_3' = NULL,
      'exp_4' = NULL,
      'exp_5' = NULL,
      'exp_6' = NULL
    ),

    exp_r6 = list(
      'exp_1' = NULL,
      'exp_2' = NULL,
      'exp_3' = NULL,
      'exp_4' = NULL,
      'exp_5' = NULL,
      'exp_6' = NULL
    ),

    dims = list(
      x_box = 0.9,
      y_box = 0.75,
      x_plot = 0.8,
      y_plot = 0.70,
      x_plot_full = 0.95,
      y_plot_full = 0.91,
      xpx_total = NULL,
      ypx_total = NULL
    )
  )

  mofa_exp = Mofa_class$new(
    name = "mofa_1"
  )

  snf_exp = Snf_class$new(
    name = "snf_1"
  )



  start_server(id = 'mod_start', main_input = input, main_output = output, main_session = session, module_controler = module_controler)
  about_server(id = 'mod_about', main_output = output)
  logs_server(id = "logs", main_input = input, main_output = output)
  help_start_server(id = 'mod_help_start', main_output = output)
  help_single_omics_server(id = 'mod_help_single_omics', main_output = output)
  help_multi_omics_server(id = 'mod_help_multi_omics', main_output = output)


  # Single omics modules
  shiny::observe({
    set_1 = names(which(module_controler$slot_taken == TRUE))
    set_2 = names(which(module_controler$module_loaded == TRUE))
    slot = base::setdiff(set_1, set_2)
    if (length(slot) > 0) {
      slot = slot[1]
      exp_type = module_controler$exp_types[[slot]]
      module_controler$module_loaded[[slot]] = TRUE
      experiment_server(id = paste0(c('mod', slot), collapse = '_'),
                        type = exp_type,
                        module_controler = module_controler)
    }
  })

  # MOFA module
  mofa_server("mofa", r6 = mofa_exp, module_controler = module_controler, main_input = input)

  # SNF module
  snf_server("snf", r6 = snf_exp, module_controler = module_controler, main_input = input)

  # Example datasets
  shiny::observeEvent(input[['mod_start-add_lipidomics_ex']],{
    if (!file.exists('./examples/multiomics/lipidomics.csv') | !file.exists('./examples/multiomics/lipidomics_metadata.csv')) {
      print('example file missing')
      return()
    }
    print('Loading example lipidomics')
    shinyjs::disable('mod_start-add_lipidomics_ex')
    for (slot in names(module_controler$slot_taken)){
      if (!module_controler$slot_taken[[slot]]) {
        module_controler$module_loaded[[slot]] = T
        module_controler$slot_taken[[slot]] = T
        module_controler$exp_types[[slot]] = 'Lipidomics'
        module_controler$exp_names[[slot]] = 'lips_example'
        module_controler$exp_r6[[slot]] = example_lipidomics(name = 'lips_example',
                                                             id = paste0(c('mod', slot), collapse = '_'),
                                                             slot = slot)
        output[[slot]] = bs4Dash::renderMenu({
          bs4Dash::sidebarMenu(
            bs4Dash::menuItem(text = 'lips_example',
                              tabName = slot,
                              icon = icon('l'))
          )
        })
        experiment_server(id = paste0(c('mod', slot), collapse = '_'),
                          type = 'Lipidomics',
                          module_controler = module_controler)
        break
      }
    }
  })

  shiny::observeEvent(input[['mod_start-add_proteomics_ex']],{
    if (!file.exists('./examples/multiomics/proteomics_2.tsv') | !file.exists('./examples/multiomics/metadata.csv')) {
      print('example file missing')
      return()
    }
    print('Loading example proteomics')

    shinyjs::disable('mod_start-add_proteomics_ex')

    for (slot in names(module_controler$slot_taken)){
      if (!module_controler$slot_taken[[slot]]) {
        module_controler$module_loaded[[slot]] = T
        module_controler$slot_taken[[slot]] = T
        module_controler$exp_types[[slot]] = 'Proteomics'
        module_controler$exp_names[[slot]] = 'prot_example'
        module_controler$exp_r6[[slot]] = example_proteomics(name = 'prot_example',
                                                             id = paste0(c('mod', slot), collapse = '_'),
                                                             slot = slot)
        output[[slot]] = bs4Dash::renderMenu({
          bs4Dash::sidebarMenu(
            bs4Dash::menuItem(text = 'prot_example',
                              tabName = slot,
                              icon = icon('p'))
          )
        })
        experiment_server(id = paste0(c('mod', slot), collapse = '_'),
                          type = 'Proteomics',
                          module_controler = module_controler)
        break
      }
    }
  })

  shiny::observeEvent(input[['mod_start-add_transcriptomics_ex']],{
    if (!file.exists('./examples/multiomics/transcriptomics_2_genename_test.tsv') | !file.exists('./examples/multiomics/metadata.csv')) {
      print('example file missing')
      return()
    }
    print('Loading example transcriptomics')

    shinyjs::disable('mod_start-add_transcriptomics_ex')

    for (slot in names(module_controler$slot_taken)){
      if (!module_controler$slot_taken[[slot]]) {
        module_controler$module_loaded[[slot]] = T
        module_controler$slot_taken[[slot]] = T
        module_controler$exp_types[[slot]] = 'Transcriptomics'
        module_controler$exp_names[[slot]] = 'trns_example'
        module_controler$exp_r6[[slot]] = example_transcriptomics(name = 'trns_example',
                                                                  id = paste0(c('mod', slot), collapse = '_'),
                                                                  slot = slot)
        output[[slot]] = bs4Dash::renderMenu({
          bs4Dash::sidebarMenu(
            bs4Dash::menuItem(text = 'trns_example',
                              tabName = slot,
                              icon = icon('t'))
          )
        })
        experiment_server(id = paste0(c('mod', slot), collapse = '_'),
                          type = 'Transcriptomics',
                          module_controler = module_controler)
        break
      }
    }
  })

}

#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
