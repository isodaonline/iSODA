#------------------------------------------------------- Sample clustering 1 ----
sample_clustering_1_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Sample clustering 1: generating plot.")

  if (input$sample_clustering_1_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_sample_clustering_1(width = width,
                              height = height)
}
sample_clustering_1_spawn = function(r6, format, output) {
  print_tm(r6$name, "Sample clustering 1: spawning plot.")
  output$sample_clustering_1_plot = plotly::renderPlotly({
    r6$plots$sample_clustering_1
    plotly::config(r6$plots$sample_clustering_1, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_module_name(r6, 'sample_clustering_1'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


sample_clustering_1_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "sample_clustering_1",
                 label = "Sample clustering 1",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


sample_clustering_1_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$sample_clustering_1_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('sample_clustering_1_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$sample_clustering_1$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('sample_clustering_1_data_table'),
        label = 'Omics dataset',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$sample_clustering_1$data_table,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('sample_clustering_1_vertical_annotations'),
        label = 'Vertical annotations',
        choices = c(colnames(r6$tables$sample_metadata), 'K clusters'),
        selected = r6$params$sample_clustering_1$vertical_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('sample_clustering_1_horizontal_annotations'),
        label = 'Horizontal annotations',
        choices = c(colnames(r6$tables$sample_metadata), 'K clusters'),
        selected = r6$params$sample_clustering_1$horizontal_annotations,
        multiple = T,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns('sample_clustering_1_K_nearest_neighbors'),
        label = 'K-nearest neighbors',
        value = r6$params$sample_clustering_1$K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('sample_clustering_1_sigma'),
        label = 'Sigma (variance)',
        value = r6$params$sample_clustering_1$sigma,
        step = 0.1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("sample_clustering_1_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$sample_clustering_1$distance_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns('sample_clustering_1_K_clusters'),
        label = 'K clusters',
        value = r6$params$sample_clustering_1$K_clusters,
        step = 1,
        width = '100%'
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('sample_clustering_1_color_palette'),
        label = "Color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$sample_clustering_1$color_palette,
        multiple = FALSE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('sample_clustering_1_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$sample_clustering_1$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::textInput(
        inputId = ns('sample_clustering_1_z_max'),
        label = 'Max. z value',
        value = r6$params$sample_clustering_1$z_max,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('sample_clustering_1_z_min'),
        label = 'Min. z value',
        value = r6$params$sample_clustering_1$z_min,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("sample_clustering_1_title_font_size"),
        label = 'Title font size',
        value = r6$params$sample_clustering_1$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("sample_clustering_1_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$sample_clustering_1$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("sample_clustering_1_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$sample_clustering_1$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("sample_clustering_1_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$sample_clustering_1$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("sample_clustering_1_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}


sample_clustering_1_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$sample_clustering_1_sidebar)
  monitor_refresh = shiny::reactive(input$sample_clustering_1_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Sample clustering 1",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = sample_clustering_1_generate,
             spawn_function = sample_clustering_1_spawn,
             img_format = input$sample_clustering_1_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$sample_clustering_1_data_table,
      input$sample_clustering_1_K_nearest_neighbors,
      input$sample_clustering_1_sigma,
      input$sample_clustering_1_distance_method,
      input$sample_clustering_1_K_clusters,
      input$sample_clustering_1_z_max,
      input$sample_clustering_1_z_min,
      input$sample_clustering_1_vertical_annotations,
      input$sample_clustering_1_horizontal_annotations,
      input$sample_clustering_1_color_palette,
      input$sample_clustering_1_reverse_palette,
      input$sample_clustering_1_title_font_size,
      input$sample_clustering_1_y_tick_font_size,
      input$sample_clustering_1_x_tick_font_size,
      input$sample_clustering_1_img_format), {


        r6$param_sample_clustering_1(auto_refresh = input$sample_clustering_1_auto_refresh,
                                     data_table = input$sample_clustering_1_data_table,
                                     K_nearest_neighbors = input$sample_clustering_1_K_nearest_neighbors,
                                     sigma = input$sample_clustering_1_sigma,
                                     distance_method = input$sample_clustering_1_distance_method,
                                     K_clusters = input$sample_clustering_1_K_clusters,
                                     z_max = input$sample_clustering_1_z_max,
                                     z_min = input$sample_clustering_1_z_min,
                                     vertical_annotations = input$sample_clustering_1_vertical_annotations,
                                     horizontal_annotations = input$sample_clustering_1_horizontal_annotations,
                                     color_palette = input$sample_clustering_1_color_palette,
                                     reverse_palette = input$sample_clustering_1_reverse_palette,
                                     title_font_size = input$sample_clustering_1_title_font_size,
                                     y_tick_font_size = input$sample_clustering_1_y_tick_font_size,
                                     x_tick_font_size = input$sample_clustering_1_x_tick_font_size,
                                     img_format = input$sample_clustering_1_img_format)

        if (!input$sample_clustering_1_auto_refresh) {
          r6$params$sample_clustering_1$auto_refresh = input$sample_clustering_1_auto_refresh
          return()
        }

        try_plot(prefix = "Sample clustering 1",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = sample_clustering_1_generate,
                 spawn_function = sample_clustering_1_spawn,
                 img_format = input$sample_clustering_1_img_format,
                 input = input,
                 output = output,
                 session = session)

  })

  # Download associated table
  output$sample_clustering_1_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("sample_clustering_1_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$sample_clustering_1, file_name)
    }
  )

  # Expanded boxes
  sample_clustering_1_proxy = plotly::plotlyProxy(outputId = "sample_clustering_1_plot",
                                                 session = session)

  shiny::observeEvent(input$sample_clustering_1_plotbox,{
    if (input$sample_clustering_1_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = sample_clustering_1_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = sample_clustering_1_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#------------------------------------------------------- Sample clustering 2 ----
sample_clustering_2_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Sample clustering 2: generating plot.")

  if (input$sample_clustering_2_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_sample_clustering_2(width = width,
                              height = height)
}
sample_clustering_2_spawn = function(r6, format, output) {
  print_tm(r6$name, "Sample clustering 2: spawning plot.")
  output$sample_clustering_2_plot = plotly::renderPlotly({
    r6$plots$sample_clustering_2
    plotly::config(r6$plots$sample_clustering_2, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_module_name(r6, 'sample_clustering_2'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


sample_clustering_2_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "sample_clustering_2",
                 label = "Sample clustering 2",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


sample_clustering_2_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$sample_clustering_2_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('sample_clustering_2_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$sample_clustering_2$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('sample_clustering_2_data_table'),
        label = 'Omics dataset',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$sample_clustering_2$data_table,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('sample_clustering_2_vertical_annotations'),
        label = 'Vertical annotations',
        choices = c(colnames(r6$tables$sample_metadata), 'K clusters'),
        selected = r6$params$sample_clustering_2$vertical_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('sample_clustering_2_horizontal_annotations'),
        label = 'Horizontal annotations',
        choices = c(colnames(r6$tables$sample_metadata), 'K clusters'),
        selected = r6$params$sample_clustering_2$horizontal_annotations,
        multiple = T,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns('sample_clustering_2_K_nearest_neighbors'),
        label = 'K-nearest neighbors',
        value = r6$params$sample_clustering_2$K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('sample_clustering_2_sigma'),
        label = 'Sigma (variance)',
        value = r6$params$sample_clustering_2$sigma,
        step = 0.1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("sample_clustering_2_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$sample_clustering_2$distance_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns('sample_clustering_2_K_clusters'),
        label = 'K clusters',
        value = r6$params$sample_clustering_2$K_clusters,
        step = 1,
        width = '100%'
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('sample_clustering_2_color_palette'),
        label = "Color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$sample_clustering_2$color_palette,
        multiple = FALSE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('sample_clustering_2_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$sample_clustering_2$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::textInput(
        inputId = ns('sample_clustering_2_z_max'),
        label = 'Max. z value',
        value = r6$params$sample_clustering_2$z_max,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('sample_clustering_2_z_min'),
        label = 'Min. z value',
        value = r6$params$sample_clustering_2$z_min,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("sample_clustering_2_title_font_size"),
        label = 'Title font size',
        value = r6$params$sample_clustering_2$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("sample_clustering_2_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$sample_clustering_2$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("sample_clustering_2_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$sample_clustering_2$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("sample_clustering_2_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$sample_clustering_2$img_format,
        width = "100%"
        ),
      shiny::downloadButton(
        outputId = ns("sample_clustering_2_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


sample_clustering_2_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$sample_clustering_2_sidebar)
  monitor_refresh = shiny::reactive(input$sample_clustering_2_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Sample clustering 2",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = sample_clustering_2_generate,
             spawn_function = sample_clustering_2_spawn,
             img_format = input$sample_clustering_2_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$sample_clustering_2_data_table,
      input$sample_clustering_2_K_nearest_neighbors,
      input$sample_clustering_2_sigma,
      input$sample_clustering_2_distance_method,
      input$sample_clustering_2_K_clusters,
      input$sample_clustering_2_z_max,
      input$sample_clustering_2_z_min,
      input$sample_clustering_2_vertical_annotations,
      input$sample_clustering_2_horizontal_annotations,
      input$sample_clustering_2_color_palette,
      input$sample_clustering_2_reverse_palette,
      input$sample_clustering_2_title_font_size,
      input$sample_clustering_2_y_tick_font_size,
      input$sample_clustering_2_x_tick_font_size,
      input$sample_clustering_2_img_format),{


        r6$param_sample_clustering_2(auto_refresh = input$sample_clustering_2_auto_refresh,
                                     data_table = input$sample_clustering_2_data_table,
                                     K_nearest_neighbors = input$sample_clustering_2_K_nearest_neighbors,
                                     sigma = input$sample_clustering_2_sigma,
                                     distance_method = input$sample_clustering_2_distance_method,
                                     K_clusters = input$sample_clustering_2_K_clusters,
                                     z_max = input$sample_clustering_2_z_max,
                                     z_min = input$sample_clustering_2_z_min,
                                     vertical_annotations = input$sample_clustering_2_vertical_annotations,
                                     horizontal_annotations = input$sample_clustering_2_horizontal_annotations,
                                     color_palette = input$sample_clustering_2_color_palette,
                                     reverse_palette = input$sample_clustering_2_reverse_palette,
                                     title_font_size = input$sample_clustering_2_title_font_size,
                                     y_tick_font_size = input$sample_clustering_2_y_tick_font_size,
                                     x_tick_font_size = input$sample_clustering_2_x_tick_font_size,
                                     img_format = input$sample_clustering_2_img_format)

        if (!input$sample_clustering_2_auto_refresh) {
          r6$params$sample_clustering_2$auto_refresh = input$sample_clustering_2_auto_refresh
          return()
        }

        try_plot(prefix = "Sample clustering 2",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = sample_clustering_2_generate,
                 spawn_function = sample_clustering_2_spawn,
                 img_format = input$sample_clustering_2_img_format,
                 input = input,
                 output = output,
                 session = session)

  })

  # Download associated table
  output$sample_clustering_2_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("sample_clustering_2_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$sample_clustering_2, file_name)
    }
  )

  # Expanded boxes
  sample_clustering_2_proxy = plotly::plotlyProxy(outputId = "sample_clustering_2_plot",
                                                 session = session)

  shiny::observeEvent(input$sample_clustering_2_plotbox,{
    if (input$sample_clustering_2_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = sample_clustering_2_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = sample_clustering_2_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}




#----------------------------------------------------------- Fusion heatmap ----
fusion_heatmap_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Fusion heatmap: generating plot.")

  if (input$fusion_heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_fusion_heatmap(width = width,
                         height = height)
}
fusion_heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "Fusion heatmap: spawning plot.")
  output$fusion_heatmap_plot = plotly::renderPlotly({
    r6$plots$fusion_heatmap
    plotly::config(r6$plots$fusion_heatmap, toImageButtonOptions = list(format= format,
                                                                                   filename= timestamped_module_name(r6, 'fusion_heatmap'),
                                                                                   height= NULL,
                                                                                   width= NULL,
                                                                                   scale= 1))
  })
}


fusion_heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "fusion_heatmap",
                 label = "Fusion heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


fusion_heatmap_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$fusion_heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('fusion_heatmap_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$fusion_heatmap$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('fusion_heatmap_omics_list'),
        label = 'Omics datasets',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$fusion_heatmap$omics_list,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('fusion_heatmap_vertical_annotations'),
        label = 'Vertical annotations',
        choices = c('K clusters', colnames(r6$tables$sample_metadata)),
        selected = r6$params$fusion_heatmap$vertical_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('fusion_heatmap_horizontal_annotations'),
        label = 'Horizontal annotations',
        choices = c('K clusters', colnames(r6$tables$sample_metadata)),
        selected = r6$params$fusion_heatmap$horizontal_annotations,
        multiple = T,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns('fusion_heatmap_K_nearest_neighbors'),
        label = 'K-nearest neighbors',
        value = r6$params$fusion_heatmap$K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('fusion_heatmap_sigma'),
        label = 'Sigma (variance)',
        value = r6$params$fusion_heatmap$sigma,
        step = 0.1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("fusion_heatmap_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$fusion_heatmap$distance_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns('fusion_heatmap_K_clusters'),
        label = 'K clusters',
        value = r6$params$fusion_heatmap$K_clusters,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('fusion_heatmap_SNF_K_nearest_neighbors'),
        label = 'K-nearest neighbors (SNF)',
        value = r6$params$fusion_heatmap$SNF_K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('fusion_heatmap_T_iterations'),
        label = 'T iterations',
        value = r6$params$fusion_heatmap$T_iterations,
        step = 1,
        width = '100%'
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('fusion_heatmap_color_palette'),
        label = "Color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$fusion_heatmap$color_palette,
        multiple = FALSE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('fusion_heatmap_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$fusion_heatmap$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::textInput(
        inputId = ns('fusion_heatmap_z_max'),
        label = 'Max. z value',
        value = r6$params$fusion_heatmap$z_max,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('fusion_heatmap_z_min'),
        label = 'Min. z value',
        value = r6$params$fusion_heatmap$z_min,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fusion_heatmap_title_font_size"),
        label = 'Title font size',
        value = r6$params$fusion_heatmap$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fusion_heatmap_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$fusion_heatmap$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fusion_heatmap_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$fusion_heatmap$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("fusion_heatmap_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$fusion_heatmap$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("fusion_heatmap_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


fusion_heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$fusion_heatmap_sidebar)
  monitor_refresh = shiny::reactive(input$fusion_heatmap_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Fusion heatmap",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = fusion_heatmap_generate,
             spawn_function = fusion_heatmap_spawn,
             img_format = input$fusion_heatmap_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(c(input$fusion_heatmap_K_nearest_neighbors,
                        input$fusion_heatmap_sigma,
                        input$fusion_heatmap_distance_method,
                        input$fusion_heatmap_SNF_K_nearest_neighbors,
                        input$fusion_heatmap_omics_list,
                        input$fusion_heatmap_K_clusters,
                        input$fusion_heatmap_T_iterations,
                        input$fusion_heatmap_z_max,
                        input$fusion_heatmap_z_min,
                        input$fusion_heatmap_vertical_annotations,
                        input$fusion_heatmap_horizontal_annotations,
                        input$fusion_heatmap_color_palette,
                        input$fusion_heatmap_reverse_palette,
                        input$fusion_heatmap_title_font_size,
                        input$fusion_heatmap_y_tick_font_size,
                        input$fusion_heatmap_x_tick_font_size,
                        input$fusion_heatmap_img_format), {

    r6$param_fusion_heatmap(auto_refresh = input$fusion_heatmap_auto_refresh,
                            K_nearest_neighbors = input$fusion_heatmap_K_nearest_neighbors,
                            sigma = input$fusion_heatmap_sigma,
                            distance_method = input$fusion_heatmap_distance_method,
                            SNF_K_nearest_neighbors = input$fusion_heatmap_SNF_K_nearest_neighbors,
                            omics_list = input$fusion_heatmap_omics_list,
                            K_clusters = input$fusion_heatmap_K_clusters,
                            T_iterations = input$fusion_heatmap_T_iterations,
                            z_max = input$fusion_heatmap_z_max,
                            z_min = input$fusion_heatmap_z_min,
                            vertical_annotations = input$fusion_heatmap_vertical_annotations,
                            horizontal_annotations = input$fusion_heatmap_horizontal_annotations,
                            color_palette = input$fusion_heatmap_color_palette,
                            reverse_palette = input$fusion_heatmap_reverse_palette,
                            title_font_size = input$fusion_heatmap_title_font_size,
                            y_tick_font_size = input$fusion_heatmap_y_tick_font_size,
                            x_tick_font_size = input$fusion_heatmap_x_tick_font_size,
                            img_format = input$fusion_heatmap_img_format)

    if (!input$fusion_heatmap_auto_refresh) {
      r6$params$fusion_heatmap$auto_refresh = input$fusion_heatmap_auto_refresh
      return()
    }

    try_plot(prefix = "Fusion heatmap",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = fusion_heatmap_generate,
             spawn_function = fusion_heatmap_spawn,
             img_format = input$fusion_heatmap_img_format,
             input = input,
             output = output,
             session = session)

  })

  # Download associated table
  output$fusion_heatmap_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("fusion_heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$fusion_heatmap, file_name)
    }
  )

  # Expanded boxes
  fusion_heatmap_proxy = plotly::plotlyProxy(outputId = "fusion_heatmap_plot",
                                                        session = session)

  shiny::observeEvent(input$fusion_heatmap_plotbox,{
    if (input$fusion_heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = fusion_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = fusion_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}

#----------------------------------------------------- Similarity Network 1 ----
similarity_network_1_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Similarity Network 1: generating plot.")

  if (input$similarity_network_1_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_similarity_network_1(width = width,
                               height = height)
}

similarity_network_1_spawn = function(r6, format, output) {
  print_tm(r6$name, "Similarity Network 1: spawning plot.")
  output$similarity_network_1_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$similarity_network_1
  )
}

similarity_network_1_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "similarity_network_1",
                 label = "Similarity Network 1",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


similarity_network_1_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$similarity_network_1_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_1_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$similarity_network_1$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('similarity_network_1_data_table'),
        label = 'Omics dataset',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$similarity_network_1$data_table,
        multiple = F,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns('similarity_network_1_K_nearest_neighbors'),
        label = 'K-nearest neighbors',
        value = r6$params$similarity_network_1$K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_1_sigma'),
        label = 'Sigma (variance)',
        value = r6$params$similarity_network_1$sigma,
        step = 0.1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("similarity_network_1_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$similarity_network_1$distance_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_1_K_clusters'),
        label = 'K clusters',
        value = r6$params$similarity_network_1$K_clusters,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns('similarity_network_1_top_edges'),
        label = 'Top edges (%)',
        value = r6$params$similarity_network_1$top_edges,
        min = 0,
        max = 100,
        step = 1,
        width = '100%'
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('similarity_network_1_group_colors'),
        label = 'Group colors',
        choices = c('K clusters', colnames(r6$tables$sample_metadata)),
        selected = r6$params$similarity_network_1$group_colors,
        multiple = F,
        width = '100%'
      ),
      shiny::selectizeInput(
        inputId = ns('similarity_network_1_node_color_palette'),
        label = "Node color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$similarity_network_1$node_color_palette,
        multiple = FALSE
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_1_edge_magnifier'),
        label = 'Edge magnifier',
        value = r6$params$similarity_network_1$edge_magnifier,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_1_node_opacity'),
        label = 'Node opacity',
        value = r6$params$similarity_network_1$node_opacity,
        min = 0.1,
        max = 1,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_1_label_font_size'),
        label = 'Label font size',
        value = r6$params$similarity_network_1$label_font_size,
        min = 1,
        max = 300,
        step = 1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_1_legend'),
        label = 'Show legend',
        value = r6$params$similarity_network_1$legend,
        right = TRUE,
        status = "primary"
      ),
      ## Network settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_1_static_network'),
        label = 'Static network',
        value = r6$params$similarity_network_1$static_network,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("similarity_network_1_solver"),
        label = "Solver",
        choices = c('barnesHut', 'repulsion'),
        selected = r6$params$similarity_network_1$solver,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_1_gravitationalConstant"),
        label = "Gravitational constant (BarnesHut)",
        value = r6$params$similarity_network_1$gravitationalConstant,
        min = NA,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_1_nodeDistance"),
        label = "Node distance (Repulsion)",
        value = r6$params$similarity_network_1$nodeDistance,
        min = 1,
        max = NA,
        step = 10,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_1_centralGravity"),
        label = "Central gravity",
        value = r6$params$similarity_network_1$centralGravity,
        min = NA,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_1_springLength"),
        label = "Spring length",
        value = r6$params$similarity_network_1$springLength,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_1_springConstant"),
        label = "Spring constant",
        value = r6$params$similarity_network_1$springConstant,
        min = NA,
        max = NA,
        step = 0.01,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::downloadButton(
        outputId = ns("similarity_network_1_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


similarity_network_1_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$similarity_network_1_sidebar)
  monitor_refresh = shiny::reactive(input$similarity_network_1_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Similarity Network 1",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = similarity_network_1_generate,
             spawn_function = similarity_network_1_spawn,
             img_format = input$similarity_network_1_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$similarity_network_1_data_table,
      input$similarity_network_1_group_colors,
      input$similarity_network_1_node_color_palette,
      input$similarity_network_1_K_nearest_neighbors,
      input$similarity_network_1_sigma,
      input$similarity_network_1_distance_method,
      input$similarity_network_1_K_clusters,
      input$similarity_network_1_top_edges,
      input$similarity_network_1_edge_magnifier,
      input$similarity_network_1_node_opacity,
      input$similarity_network_1_label_font_size,
      input$similarity_network_1_legend,
      input$similarity_network_1_static_network,
      input$similarity_network_1_solver,
      input$similarity_network_1_gravitationalConstant,
      input$similarity_network_1_nodeDistance,
      input$similarity_network_1_centralGravity,
      input$similarity_network_1_springLength,
      input$similarity_network_1_springConstant

      ), {

        r6$param_similarity_network_1(auto_refresh = input$similarity_network_1_auto_refresh,
                                      data_table = input$similarity_network_1_data_table,
                                      group_colors = input$similarity_network_1_group_colors,
                                      node_color_palette = input$similarity_network_1_node_color_palette,
                                      K_nearest_neighbors = input$similarity_network_1_K_nearest_neighbors,
                                      sigma = input$similarity_network_1_sigma,
                                      distance_method = input$similarity_network_1_distance_method,
                                      K_clusters = input$similarity_network_1_K_clusters,
                                      top_edges = input$similarity_network_1_top_edges,
                                      edge_magnifier = input$similarity_network_1_edge_magnifier,
                                      node_opacity = input$similarity_network_1_node_opacity,
                                      label_font_size = input$similarity_network_1_label_font_size,
                                      legend = input$similarity_network_1_legend,
                                      static_network = input$similarity_network_1_static_network,
                                      solver = input$similarity_network_1_solver,
                                      gravitationalConstant = input$similarity_network_1_gravitationalConstant,
                                      nodeDistance = input$similarity_network_1_nodeDistance,
                                      centralGravity = input$similarity_network_1_centralGravity,
                                      springLength = input$similarity_network_1_springLength,
                                      springConstant = input$similarity_network_1_springConstant
                                      )

        if (!input$similarity_network_1_auto_refresh) {
          r6$params$similarity_network_1$auto_refresh = input$similarity_network_1_auto_refresh
          return()
        }

        try_plot(prefix = "Similarity Network 1",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = similarity_network_1_generate,
                 spawn_function = similarity_network_1_spawn,
                 img_format = input$similarity_network_1_img_format,
                 input = input,
                 output = output,
                 session = session)


      })

  # Download associated table
  output$similarity_network_1_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("similarity_network_1_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$similarity_network_1, file_name)
    }
  )

  # Expanded boxes
  similarity_network_1_proxy = plotly::plotlyProxy(outputId = "similarity_network_1_plot",
                                                   session = session)

  # shiny::observeEvent(input$similarity_network_1_plotbox,{
  #   if (input$similarity_network_1_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = similarity_network_1_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = similarity_network_1_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}

#----------------------------------------------------- Similarity Network 2 ----
similarity_network_2_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Similarity Network 2: generating plot.")

  if (input$similarity_network_2_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_similarity_network_2(width = width,
                               height = height)
}
similarity_network_2_spawn = function(r6, format, output) {
  print_tm(r6$name, "Similarity Network 2: spawning plot.")
  output$similarity_network_2_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$similarity_network_2
  )
}


similarity_network_2_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "similarity_network_2",
                 label = "Similarity Network 2",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


similarity_network_2_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$similarity_network_2_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_2_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$similarity_network_2$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      # Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('similarity_network_2_data_table'),
        label = 'Omics dataset',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$similarity_network_2$data_table,
        multiple = F,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns('similarity_network_2_K_nearest_neighbors'),
        label = 'K-nearest neighbors',
        value = r6$params$similarity_network_2$K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_2_sigma'),
        label = 'Sigma (variance)',
        value = r6$params$similarity_network_2$sigma,
        step = 0.1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("similarity_network_2_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$similarity_network_2$distance_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_2_K_clusters'),
        label = 'K clusters',
        value = r6$params$similarity_network_2$K_clusters,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns('similarity_network_2_top_edges'),
        label = 'Top edges (%)',
        value = r6$params$similarity_network_2$top_edges,
        min = 0,
        max = 100,
        step = 1,
        width = '100%'
      ),
      # Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('similarity_network_2_group_colors'),
        label = 'Group colors',
        choices = c('K clusters', colnames(r6$tables$sample_metadata)),
        selected = r6$params$similarity_network_2$group_colors,
        multiple = F,
        width = '100%'
      ),
      shiny::selectizeInput(
        inputId = ns('similarity_network_2_node_color_palette'),
        label = "Node color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$similarity_network_2$node_color_palette,
        multiple = FALSE
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_2_edge_magnifier'),
        label = 'Edge magnifier',
        value = r6$params$similarity_network_2$edge_magnifier,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_2_node_opacity'),
        label = 'Node opacity',
        value = r6$params$similarity_network_2$node_opacity,
        min = 0.1,
        max = 1,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_2_label_font_size'),
        label = 'Label font size',
        value = r6$params$similarity_network_2$label_font_size,
        min = 1,
        max = 300,
        step = 1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_2_legend'),
        label = 'Show legend',
        value = r6$params$similarity_network_2$legend,
        right = TRUE,
        status = "primary"
      ),
      ## Network settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_2_static_network'),
        label = 'Static network',
        value = r6$params$similarity_network_2$static_network,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("similarity_network_2_solver"),
        label = "Solver",
        choices = c('barnesHut', 'repulsion'),
        selected = r6$params$similarity_network_2$solver,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_2_gravitationalConstant"),
        label = "Gravitational constant (BarnesHut)",
        value = r6$params$similarity_network_2$gravitationalConstant,
        min = NA,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_2_nodeDistance"),
        label = "Node distance (Repulsion)",
        value = r6$params$similarity_network_2$nodeDistance,
        min = 1,
        max = NA,
        step = 10,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_2_centralGravity"),
        label = "Central gravity",
        value = r6$params$similarity_network_2$centralGravity,
        min = NA,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_2_springLength"),
        label = "Spring length",
        value = r6$params$similarity_network_2$springLength,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_2_springConstant"),
        label = "Spring constant",
        value = r6$params$similarity_network_2$springConstant,
        min = NA,
        max = NA,
        step = 0.01,
        width = '100%'
      ),
      # Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::downloadButton(
        outputId = ns("similarity_network_2_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


similarity_network_2_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$similarity_network_2_sidebar)
  monitor_refresh = shiny::reactive(input$similarity_network_2_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Similarity Network 2",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = similarity_network_2_generate,
             spawn_function = similarity_network_2_spawn,
             img_format = input$similarity_network_2_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(c(
    input$similarity_network_2_data_table,
    input$similarity_network_2_group_colors,
    input$similarity_network_2_node_color_palette,
    input$similarity_network_2_K_nearest_neighbors,
    input$similarity_network_2_sigma,
    input$similarity_network_2_distance_method,
    input$similarity_network_2_K_clusters,
    input$similarity_network_2_top_edges,
    input$similarity_network_2_edge_magnifier,
    input$similarity_network_2_node_opacity,
    input$similarity_network_2_label_font_size,
    input$similarity_network_2_legend,
    input$similarity_network_2_static_network,
    input$similarity_network_2_solver,
    input$similarity_network_2_gravitationalConstant,
    input$similarity_network_2_nodeDistance,
    input$similarity_network_2_centralGravity,
    input$similarity_network_2_springLength,
    input$similarity_network_2_springConstant

    ), {


      r6$param_similarity_network_2(auto_refresh = input$similarity_network_2_auto_refresh,
                                    data_table = input$similarity_network_2_data_table,
                                    group_colors = input$similarity_network_2_group_colors,
                                    node_color_palette = input$similarity_network_2_node_color_palette,
                                    K_nearest_neighbors = input$similarity_network_2_K_nearest_neighbors,
                                    sigma = input$similarity_network_2_sigma,
                                    distance_method = input$similarity_network_2_distance_method,
                                    K_clusters = input$similarity_network_2_K_clusters,
                                    top_edges = input$similarity_network_2_top_edges,
                                    edge_magnifier = input$similarity_network_2_edge_magnifier,
                                    node_opacity = input$similarity_network_2_node_opacity,
                                    label_font_size = input$similarity_network_2_label_font_size,
                                    legend = input$similarity_network_2_legend,
                                    static_network = input$similarity_network_2_static_network,
                                    solver = input$similarity_network_2_solver,
                                    gravitationalConstant = input$similarity_network_2_gravitationalConstant,
                                    nodeDistance = input$similarity_network_2_nodeDistance,
                                    centralGravity = input$similarity_network_2_centralGravity,
                                    springLength = input$similarity_network_2_springLength,
                                    springConstant = input$similarity_network_2_springConstant
                                    )

      if (!input$similarity_network_2_auto_refresh) {
        r6$params$similarity_network_2$auto_refresh = input$similarity_network_2_auto_refresh
        return()
      }

      try_plot(prefix = "Similarity Network 2",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = similarity_network_2_generate,
               spawn_function = similarity_network_2_spawn,
               img_format = input$similarity_network_2_img_format,
               input = input,
               output = output,
               session = session)

    })

  # Download associated table
  output$similarity_network_2_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("similarity_network_2_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$similarity_network_2, file_name)
    }
  )

  # Expanded boxes
  similarity_network_2_proxy = plotly::plotlyProxy(outputId = "similarity_network_2_plot",
                                                   session = session)

  # shiny::observeEvent(input$similarity_network_2_plotbox,{
  #   if (input$similarity_network_2_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = similarity_network_2_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = similarity_network_2_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}

#------------------------------------------------ Similarity Network Fusion ----
similarity_network_fusion_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Similarity Network Fusion: generating plot.")

  if (input$similarity_network_fusion_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_similarity_network_fusion(width = width,
                                    height = height)
}
similarity_network_fusion_spawn = function(r6, format, output) {
  print_tm(r6$name, "Similarity Network Fusion: spawning plot.")
  output$similarity_network_fusion_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$similarity_network_fusion
  )
}

similarity_network_fusion_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "similarity_network_fusion",
                 label = "Similarity Network Fusion",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


similarity_network_fusion_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$similarity_network_fusion_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_fusion_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$similarity_network_fusion$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('similarity_network_fusion_omics_list'),
        label = 'Omics datasets',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$similarity_network_fusion$data_table,
        multiple = T,
        width = '100%'
      ),
      # Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_K_nearest_neighbors'),
        label = 'K-nearest neighbors',
        value = r6$params$similarity_network_fusion$K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_sigma'),
        label = 'Sigma (variance)',
        value = r6$params$similarity_network_fusion$sigma,
        step = 0.1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("similarity_network_fusion_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$similarity_network_fusion$distance_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_K_clusters'),
        label = 'K clusters',
        value = r6$params$similarity_network_fusion$K_clusters,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_SNF_K_nearest_neighbors'),
        label = 'K-nearest neighbors (SNF)',
        value = r6$params$similarity_network_fusion$SNF_K_nearest_neighbors,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_T_iterations'),
        label = 'T iterations',
        value = r6$params$similarity_network_fusion$T_iterations,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns('similarity_network_fusion_top_edges'),
        label = 'Top edges (%)',
        value = r6$params$similarity_network_fusion$top_edges,
        min = 0,
        max = 100,
        step = 1,
        width = '100%'
      ),
      # Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('similarity_network_fusion_group_colors'),
        label = 'Group colors',
        choices = c('K clusters', colnames(r6$tables$sample_metadata)),
        selected = r6$params$similarity_network_fusion$group_colors,
        multiple = F,
        width = '100%'
      ),
      shiny::selectizeInput(
        inputId = ns('similarity_network_fusion_node_color_palette'),
        label = "Node color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$similarity_network_fusion$node_color_palette,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns('similarity_network_fusion_edge_color_palette'),
        label = "Edge color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$similarity_network_fusion$edge_color_palette,
        multiple = FALSE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_fusion_reverse_edge_colors'),
        label = 'Reverse edge colors',
        value = r6$params$similarity_network_fusion$reverse_edge_colors,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_edge_magnifier'),
        label = 'Edge magnifier',
        value = r6$params$similarity_network_fusion$edge_magnifier,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_node_opacity'),
        label = 'Node opacity',
        value = r6$params$similarity_network_fusion$node_opacity,
        min = 0.1,
        max = 1,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('similarity_network_fusion_label_font_size'),
        label = 'Label font size',
        value = r6$params$similarity_network_fusion$label_font_size,
        min = 1,
        max = 300,
        step = 1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_fusion_legend'),
        label = 'Show legend',
        value = r6$params$similarity_network_fusion$legend,
        right = TRUE,
        status = "primary"
      ),
      ## Network settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('similarity_network_fusion_static_network'),
        label = 'Static network',
        value = r6$params$similarity_network_fusion$static_network,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("similarity_network_fusion_solver"),
        label = "Solver",
        choices = c('barnesHut', 'repulsion'),
        selected = r6$params$similarity_network_fusion$solver,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_fusion_gravitationalConstant"),
        label = "Gravitational constant (BarnesHut)",
        value = r6$params$similarity_network_fusion$gravitationalConstant,
        min = NA,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_fusion_nodeDistance"),
        label = "Node distance (Repulsion)",
        value = r6$params$similarity_network_fusion$nodeDistance,
        min = 1,
        max = NA,
        step = 10,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_fusion_centralGravity"),
        label = "Central gravity",
        value = r6$params$similarity_network_fusion$centralGravity,
        min = NA,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_fusion_springLength"),
        label = "Spring length",
        value = r6$params$similarity_network_fusion$springLength,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("similarity_network_fusion_springConstant"),
        label = "Spring constant",
        value = r6$params$similarity_network_fusion$springConstant,
        min = NA,
        max = NA,
        step = 0.01,
        width = '100%'
      ),
      # Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::downloadButton(
        outputId = ns("similarity_network_fusion_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}


similarity_network_fusion_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$similarity_network_fusion_sidebar)
  monitor_refresh = shiny::reactive(input$similarity_network_fusion_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Similarity Network Fusion",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = similarity_network_fusion_generate,
             spawn_function = similarity_network_fusion_spawn,
             img_format = input$similarity_network_fusion_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(c(
    input$similarity_network_fusion_omics_list,
    input$similarity_network_fusion_group_colors,
    input$similarity_network_fusion_node_color_palette,
    input$similarity_network_fusion_edge_color_palette,
    input$similarity_network_fusion_reverse_edge_colors,
    input$similarity_network_fusion_edge_magnifier,
    input$similarity_network_fusion_node_opacity,
    input$similarity_network_fusion_label_font_size,
    input$similarity_network_fusion_K_nearest_neighbors,
    input$similarity_network_fusion_sigma,
    input$similarity_network_fusion_distance_method,
    input$similarity_network_fusion_SNF_K_nearest_neighbors,
    input$similarity_network_fusion_K_clusters,
    input$similarity_network_fusion_T_iterations,
    input$similarity_network_fusion_top_edges,
    input$similarity_network_fusion_legend,
    input$similarity_network_fusion_static_network,
    input$similarity_network_fusion_solver,
    input$similarity_network_fusion_gravitationalConstant,
    input$similarity_network_fusion_nodeDistance,
    input$similarity_network_fusion_centralGravity,
    input$similarity_network_fusion_springLength,
    input$similarity_network_fusion_springConstant
  ), {


    r6$param_similarity_network_fusion(auto_refresh = input$similarity_network_fusion_auto_refresh,
                                       group_colors = input$similarity_network_fusion_group_colors,
                                       node_color_palette = input$similarity_network_fusion_node_color_palette,
                                       edge_color_palette = input$similarity_network_fusion_edge_color_palette,
                                       reverse_edge_colors = input$similarity_network_fusion_reverse_edge_colors,
                                       edge_magnifier= input$similarity_network_fusion_edge_magnifier,
                                       node_opacity = input$similarity_network_fusion_node_opacity,
                                       label_font_size = input$similarity_network_fusion_label_font_size,
                                       K_nearest_neighbors = input$similarity_network_fusion_K_nearest_neighbors,
                                       sigma = input$similarity_network_fusion_sigma,
                                       distance_method = input$similarity_network_fusion_distance_method,
                                       SNF_K_nearest_neighbors = input$similarity_network_fusion_SNF_K_nearest_neighbors,
                                       omics_list = input$similarity_network_fusion_omics_list,
                                       K_clusters = input$similarity_network_fusion_K_clusters,
                                       T_iterations = input$similarity_network_fusion_T_iterations,
                                       top_edges = input$similarity_network_fusion_top_edges,
                                       legend = input$similarity_network_fusion_legend,
                                       static_network = input$similarity_network_fusion_static_network,
                                       solver = input$similarity_network_fusion_solver,
                                       gravitationalConstant = input$similarity_network_fusion_gravitationalConstant,
                                       nodeDistance = input$similarity_network_fusion_nodeDistance,
                                       centralGravity = input$similarity_network_fusion_centralGravity,
                                       springLength = input$similarity_network_fusion_springLength,
                                       springConstant = input$similarity_network_fusion_springConstant
                                       )

    if (!input$similarity_network_fusion_auto_refresh) {
      r6$params$similarity_network_fusion$auto_refresh = input$similarity_network_fusion_auto_refresh
      return()
    }

    try_plot(prefix = "Similarity Network Fusion",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = similarity_network_fusion_generate,
             spawn_function = similarity_network_fusion_spawn,
             img_format = input$similarity_network_fusion_img_format,
             input = input,
             output = output,
             session = session)

  })

  # Download associated table
  output$similarity_network_fusion_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("similarity_network_fusion_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$similarity_network_fusion, file_name)
    }
  )

  # Expanded boxes
  similarity_network_fusion_proxy = plotly::plotlyProxy(outputId = "similarity_network_fusion_plot",
                                                        session = session)

  # shiny::observeEvent(input$similarity_network_fusion_plotbox,{
  #   if (input$similarity_network_fusion_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = similarity_network_fusion_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = similarity_network_fusion_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}
