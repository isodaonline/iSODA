
#------------------------------------------------------------------ Logs UI ----
logs_ui = function(id){
  ns = shiny::NS(id)
  shiny::fluidPage(
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h2('iSODA current session log file'),
          shiny::hr(style = "border-top: 2px solid #000000;")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shiny::h4('Log file:'),
          shiny::uiOutput(ns('log_file_name')),
          shiny::downloadButton(
            outputId = ns("downloadLog"),
            label = "Download",
            icon = shiny::icon("download")
            )
        ),
        shiny::column(
          width = 10,
          shiny::uiOutput(ns('log_output'))
        )
      )
    )
  )
}

#-------------------------------------------------------------- Logs server ----

logs_server = function(id, main_input, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Autorefresh the log
      shiny::observe({
        shiny::req(main_input$main_sidebar)
        if (main_input$main_sidebar == "logs_tab") {
          if (!base::file.exists(log_file)) {
            base::writeLines(text = "START", log_file)
          }
          log_data = readLines(log_file)
          log_data = stringi::stri_split(str = log_data, regex = '\\n')
          log_data = base::rev(log_data)
          log_data = lapply(log_data, shiny::br)
          output$log_file_name = shiny::renderUI({
            log_file
          })
          output$log_output = shiny::renderUI({
            log_data
          })
        }
      })

      # Download the logfile
      output$downloadLog = shiny::downloadHandler(
        filename = function() {
          base::basename(log_file)
        },
        content = function(file) {
          base::file.copy(log_file, file)
        }
      )

    }
  )
}



