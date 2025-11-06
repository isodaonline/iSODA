
##################################################################### Start ####

#------------------------------------------------------------ Help Start UI ----
help_start_ui = function(id){
  ns = shiny::NS(id)
  #shiny::includeMarkdown("./man/input_formats.md")
  shiny::fluidPage(
    shiny::h3("Input formats"),
    shiny::p("Supported formats are coma-separated files (CSV, CSV2), tab-separated files (TSV, TXT) and Excel files (XLSX)."),
    
    shiny::tags$hr(width = "75%"),
    shiny::h3("Sample annotations"),
    shiny::p("Contains all metadata pertaining to each sample, values can be of any type. There is no mandatory naming for the columns, but there should be", shiny::tags$b("minimum"), " three of them containing:"),
    shiny::tags$ul(
      shiny::tags$li(shiny::tags$b("Sample ID"), " (mandatory): unique identifiers (same in all the tables). They ", shiny::tags$b("need"), " to match the sample IDs in the measurement data file."),
      shiny::tags$li(shiny::tags$b("Sample type"), " (mandatory): specifies which rows are blanks, QC, pools and actual samples."),
      shiny::tags$li(shiny::tags$b("Group type"), " (mandatory): specifies which groups each sample belongs to."),
      shiny::tags$li(shiny::tags$b("Batch"), " (optional): batch number for each row (cannot contain NAs). If not supplied, all samples will be assumed to be from the same batch. ")
    ),
    shiny::p("There are no rules for naming the mandatory columns. The ", shiny::tags$b("Sample IDs"), " column you can select when the meta data file is uploaded. iSODA tries to recognize the columns: ", shiny::tags$b("Sample type"), "
    and ", shiny::tags$b("Group type"), " by their name."),
    shiny::h5("Download templates"),
    shiny::p("Two templates for meta data are available. An empty one and one filled with some dummy data."),
    shiny::downloadButton(
      outputId = ns("help_template_empty"),
      label = "Empty template",
      style = "width:20%;"
    ),
    shiny::downloadButton(
      outputId = ns("help_template_data"),
      label = "Template with data",
      style = "width:20%;"
    ),
    
    shiny::tags$hr(width = "75%"),
    shiny::h3("Measurement data"),
    shiny::p("Contains samples as rows and features as columns. The first column should be the sample IDs (same as in the sample annotations table), the rest are the feature names, values are only numerical or missing. Currently supported feature names are the following:"),
    shiny::tags$ul(
      shiny::tags$li("Shorthand lipid IDs (Lipidomics)"),
      shiny::tags$li("Symbol (Proteomics, Transcriptomics, Genomics)"),
      shiny::tags$li("Entrez ID (Proteomics, Transcriptomics, Genomics)"),
      shiny::tags$li("Uniprot (Proteomics, Transcriptomics, Genomics)")
    ),
    
    shiny::tags$hr(width = "75%"),
    shiny::h3("Feature annotations (optional)"),
    shiny::p("This table is formatted with features as rows and annotations as columns. Feature annotations can be 
             simple – one value per cell – or complex: multiple values per cell, separated by a pipe (“|”) character. 
             Complex annotations typically include ontologies, like classifications, pathways or gene ontologies and can be used in 
             certain plots like the volcano plot, or for functional analysis.")
  )
}

#-------------------------------------------------------- Help Start server ----

help_start_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$help_template_empty <- shiny::downloadHandler(
        filename = function() {
          "template_metadata_empty.xlsx"
        },
        content = function(file) {
          file.copy("./www/templates/Template_metadata_empty.xlsx", file)
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
      
      output$help_template_data <- shiny::downloadHandler(
        filename = function() {
          "template_metadata.xlsx"
        },
        content = function(file) {
          file.copy("./www/templates/Template_metadata.xlsx", file)
        },
        contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    }
  )
}

############################################################## Single omics ####

#----------------------------------------------------- Help Single omics UI ----
help_single_omics_ui = function(id){
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Visualization tab",
      shiny::includeMarkdown("./man/single_omics/visualization.md")
    ),
    shiny::tabPanel(
      title = "Functional analysis tab",
      shiny::includeMarkdown("./man/single_omics/functional_analysis.md")
    )
  )

}

#------------------------------------------------- Help Single omics server ----

help_single_omics_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

############################################################### Multi omics ####

#------------------------------------------------------ Help Multi omics UI ----
help_multi_omics_ui = function(id){
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "MOFA - Multi-Omics Factor Analysis",
      shiny::includeMarkdown("./man/multi_omics/mofa.md")
    ),
    shiny::tabPanel(
      title = "SNF - Similarity Network Fusion",
      shiny::includeMarkdown("./man/multi_omics/snf.md")
    )
  )
}

#-------------------------------------------------- Help Multi omics server ----

help_multi_omics_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}


