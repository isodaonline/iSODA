#-------------------------------------------------------------- EA Dot plot ----
ea_dot_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "EA dot plot: generating plot.")
  if (input$ea_dot_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_ea_dot_plot(width = width,
                      height = height)
}

ea_dot_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "EA dot plot: spawning plot.")

  output$ea_dot_plot_plot = plotly::renderPlotly({
    r6$plots$ea_dot_plot
    plotly::config(r6$plots$ea_dot_plot, toImageButtonOptions = list(format= format,
                                                                     filename= timestamped_module_name(r6, 'ea_dot_plot'),
                                                                     height= NULL,
                                                                     width= NULL,
                                                                     scale= 1))
  })
}

ea_dot_plot_ui = function(dimensions_obj, session) {
  get_plotly_box(id = "ea_dot_plot",
                 label = "EA dot plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ea_dot_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ea_dot_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('ea_dot_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ea_dot_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ea_dot_plot_x"),
        label = "x-axis",
        choices = c("GeneRatio", "GeneCount", "setSize"),
        selected = r6$params$ea_dot_plot$x,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_dot_plot_y"),
        label = "y-axis",
        choices = c("ID", "Description"),
        selected = r6$params$ea_dot_plot$y,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_dot_plot_color"),
        label = "Coloring",
        choices = c("pvalue", "p.adjust"),
        selected = r6$params$ea_dot_plot$color,
        multiple = F,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_dot_plot_show_categories"),
        label = "Show categories",
        value = r6$params$ea_dot_plot$show_categories,
        min = 1,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_dot_plot_size"),
        label = "Size",
        choices = c("GeneRatio", "GeneCount", "setSize"),
        selected = r6$params$ea_dot_plot$size,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_dot_plot_order_by"),
        label = "Order by",
        choices = c("GeneRatio", "GeneCount", "setSize"),
        selected = r6$params$ea_dot_plot$order_by,
        multiple = F,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_dot_plot_reverse_order'),
        label = 'Reverse order',
        value = r6$params$ea_dot_plot$reverse_order,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("ea_dot_plot_mode"),
        label = "Mode",
        choices = c("Both", "Activated", "Suppressed"),
        selected = r6$params$ea_dot_plot$mode,
        multiple = F,
        width = '100%'
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ea_dot_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ea_dot_plot$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_dot_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$ea_dot_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('ea_dot_plot_size_ref'),
        label = "Size ref",
        value = r6$params$ea_dot_plot$size_ref,
        min = 1,
        step = 0.25,
        width = "100%"
      ),
      shiny::sliderInput(
        inputId = ns("ea_dot_plot_marker_opacity"),
        label = 'Marker opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$ea_dot_plot$marker_opacity,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_dot_plot_yaxis_word_split"),
        label = 'yaxis split',
        min = 0,
        max = 5,
        value = r6$params$ea_dot_plot$yaxis_word_split,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_dot_plot_title_size"),
        label = 'Title size',
        min = 0,
        max = 50,
        value = r6$params$ea_dot_plot$title_size,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_dot_plot_xlabel_size"),
        label = 'x-label size',
        min = 0,
        max = 50,
        value = r6$params$ea_dot_plot$xlabel_size,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_dot_plot_xtick_size"),
        label = 'x-tick size',
        min = 0,
        max = 50,
        value = r6$params$ea_dot_plot$xtick_size,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_dot_plot_ytick_size"),
        label = 'y-tick size',
        min = 0,
        max = 50,
        value = r6$params$ea_dot_plot$ytick_size,
        step = 1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_dot_plot_show_legend'),
        label = 'Show legend',
        value = r6$params$ea_dot_plot$show_legend,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("ea_dot_plot_legend_size"),
        label = 'Legend font size',
        min = 1,
        max = 50,
        value = r6$params$ea_dot_plot$legend_size,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ea_dot_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ea_dot_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("download_ea_dot_plot_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

ea_dot_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ea_dot_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ea_dot_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "EA dot plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ea_dot_plot_generate,
             spawn_function = ea_dot_plot_spawn,
             img_format = input$ea_dot_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ea_dot_plot_x,
    input$ea_dot_plot_y,
    input$ea_dot_plot_color,
    input$ea_dot_plot_show_categories,
    input$ea_dot_plot_size,
    input$ea_dot_plot_order_by,
    input$ea_dot_plot_reverse_order,
    input$ea_dot_plot_mode,
    input$ea_dot_plot_color_palette,
    input$ea_dot_plot_reverse_palette,
    input$ea_dot_plot_show_legend,
    input$ea_dot_plot_legend_size,
    input$ea_dot_plot_marker_opacity,
    input$ea_dot_plot_size_ref,
    input$ea_dot_plot_yaxis_word_split,
    input$ea_dot_plot_title_size,
    input$ea_dot_plot_xlabel_size,
    input$ea_dot_plot_xtick_size,
    input$ea_dot_plot_ytick_size,
    input$ea_dot_plot_img_format),{

      # Update params
      r6$param_ea_dot_plot(
        auto_refresh = input$ea_dot_plot_auto_refresh,
        x = input$ea_dot_plot_x,
        y = input$ea_dot_plot_y,
        color = input$ea_dot_plot_color,
        show_categories = input$ea_dot_plot_show_categories,
        size = input$ea_dot_plot_size,
        order_by = input$ea_dot_plot_order_by,
        reverse_order = input$ea_dot_plot_reverse_order,
        mode = input$ea_dot_plot_mode,
        color_palette = input$ea_dot_plot_color_palette,
        reverse_palette = input$ea_dot_plot_reverse_palette,
        show_legend = input$ea_dot_plot_show_legend,
        legend_size = input$ea_dot_plot_legend_size,
        size_ref = input$ea_dot_plot_size_ref,
        marker_opacity = input$ea_dot_plot_marker_opacity,
        yaxis_word_split = input$ea_dot_plot_yaxis_word_split,
        title_size = input$ea_dot_plot_title_size,
        xlabel_size = input$ea_dot_plot_xlabel_size,
        xtick_size = input$ea_dot_plot_xtick_size,
        ytick_size = input$ea_dot_plot_ytick_size,
        img_format = input$ea_dot_plot_img_format)

      if (!input$ea_dot_plot_auto_refresh) {
        r6$params$ea_dot_plot$auto_refresh = input$ea_dot_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "EA dot plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ea_dot_plot_generate,
               spawn_function = ea_dot_plot_spawn,
               img_format = input$ea_dot_plot_img_format,
               input = input,
               output = output,
               session = session)


    })


    # Download associated tables
    output$download_ea_dot_plot_table = shiny::downloadHandler(
      filename = function(){timestamped_name("ea_dot_plot_table.csv")},
      content = function(file_name){
        write.csv(r6$tables$ea_dot_plot, file_name)
      }
    )

  # Expanded boxes
  ea_dot_plot_proxy = plotly::plotlyProxy(outputId = "ea_dot_plot_plot",
                                       session = session)

  shiny::observeEvent(input$ea_dot_plot_plotbox,{
    if (input$ea_dot_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = ea_dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = ea_dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#------------------------------------------------------------- EA CNET plot ----
ea_cnet_plot_generate = function(r6, dimensions_obj, input) {

  print_tm(r6$name, "EA CNET plot: generating plot.")

  if (input$ea_cnet_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_ea_cnet_plot(width = width,
                       height = height)
}

ea_cnet_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "EA CNET plot: spawning plot.")

  output$ea_cnet_plot_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$ea_cnet_plot,
  )
}

ea_cnet_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "ea_cnet_plot",
                 label = "EA CNET plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ea_cnet_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ea_cnet_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('ea_cnet_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ea_cnet_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns("ea_cnet_plot_show_category"),
        label = "Show category",
        value = r6$params$ea_cnet_plot$show_category,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_cnet_plot_displayed_labels"),
        label = "Displayed labels",
        choices = c('ID and Description', 'ID', 'Description'),
        selected = r6$params$ea_cnet_plot$displayed_labels,
        multiple = FALSE,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ea_cnet_plot_set_node_annotations"),
        label = "Set node annotations",
        choices = c('None', 'enrichmentScore', 'NES', 'pvalue', 'p.adjust'),
        selected = r6$params$ea_cnet_plot$set_node_annotations,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_cnet_plot_feature_node_annotations"),
        label = "Feature node annotations",
        choices = c('None', colnames(r6$tables$ea_feature_table)),
        selected = r6$params$ea_cnet_plot$feature_node_annotations,
        multiple = FALSE,
        width = '100%'
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ea_cnet_plot_set_node_color_palette'),
        label = 'Set color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ea_cnet_plot$set_node_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_cnet_plot_reverse_set_palette'),
        label = 'Reverse set palette',
        value = r6$params$ea_cnet_plot$reverse_set_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('ea_cnet_plot_feature_node_color_palette'),
        label = 'Feature color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ea_cnet_plot$feature_node_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_cnet_plot_reverse_feature_palette'),
        label = 'Reverse feature palette',
        value = r6$params$ea_cnet_plot$reverse_feature_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('ea_cnet_plot_label_font_size'),
        label = 'Label font size',
        value = r6$params$ea_cnet_plot$label_font_size,
        min = 1,
        max = 300,
        step = 1,
        width = '100%'
      ),
      ## Network settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_cnet_plot_static_network'),
        label = 'Static network',
        value = r6$params$ea_cnet_plot$static_network,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("ea_cnet_plot_solver"),
        label = "Solver",
        choices = c('barnesHut', 'repulsion'),
        selected = r6$params$ea_cnet_plot$solver,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_cnet_plot_gravitationalConstant"),
        label = "Gravitational constant (BarnesHut)",
        value = r6$params$ea_cnet_plot$gravitationalConstant,
        min = NA,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_cnet_plot_nodeDistance"),
        label = "Node distance (Repulsion)",
        value = r6$params$ea_cnet_plot$nodeDistance,
        min = 1,
        max = NA,
        step = 10,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_cnet_plot_centralGravity"),
        label = "Central gravity",
        value = r6$params$ea_cnet_plot$centralGravity,
        min = NA,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_cnet_plot_springLength"),
        label = "Spring length",
        value = r6$params$ea_cnet_plot$springLength,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_cnet_plot_springConstant"),
        label = "Spring constant",
        value = r6$params$ea_cnet_plot$springConstant,
        min = NA,
        max = NA,
        step = 0.01,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ea_cnet_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ea_cnet_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("download_ea_cnet_plot_node_table"),
        label = "Download node table",
        style = "width:100%;"
      ),
      shiny::downloadButton(
        outputId = ns("download_ea_cnet_plot_edge_table"),
        label = "Download edge table",
        style = "width:100%;"
      )
    )
  })
}

ea_cnet_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ea_cnet_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ea_cnet_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "EA CNET plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ea_cnet_plot_generate,
             spawn_function = ea_cnet_plot_spawn,
             img_format = input$ea_cnet_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ea_cnet_plot_show_category,
    input$ea_cnet_plot_displayed_labels,
    input$ea_cnet_plot_set_node_annotations,
    input$ea_cnet_plot_feature_node_annotations,
    input$ea_cnet_plot_set_node_color_palette,
    input$ea_cnet_plot_reverse_set_palette,
    input$ea_cnet_plot_feature_node_color_palette,
    input$ea_cnet_plot_reverse_feature_palette,
    input$ea_cnet_plot_label_font_size,
    input$ea_cnet_plot_static_network,
    input$ea_cnet_plot_solver,
    input$ea_cnet_plot_gravitationalConstant,
    input$ea_cnet_plot_nodeDistance,
    input$ea_cnet_plot_centralGravity,
    input$ea_cnet_plot_springLength,
    input$ea_cnet_plot_springConstant,
    input$ea_cnet_plot_img_format),{

      # Update params
      r6$param_ea_cnet_plot(
        auto_refresh = input$ea_cnet_plot_auto_refresh,
        show_category = input$ea_cnet_plot_show_category,
        displayed_labels = input$ea_cnet_plot_displayed_labels,
        set_node_annotations = input$ea_cnet_plot_set_node_annotations,
        feature_node_annotations = input$ea_cnet_plot_feature_node_annotations,
        set_node_color_palette = input$ea_cnet_plot_set_node_color_palette,
        reverse_set_palette = input$ea_cnet_plot_reverse_set_palette,
        feature_node_color_palette = input$ea_cnet_plot_feature_node_color_palette,
        reverse_feature_palette = input$ea_cnet_plot_reverse_feature_palette,
        label_font_size = input$ea_cnet_plot_label_font_size,
        static_network = input$ea_cnet_plot_static_network,
        solver = input$ea_cnet_plot_solver,
        gravitationalConstant = input$ea_cnet_plot_gravitationalConstant,
        nodeDistance = input$ea_cnet_plot_nodeDistance,
        centralGravity = input$ea_cnet_plot_centralGravity,
        springLength = input$ea_cnet_plot_springLength,
        springConstant = input$ea_cnet_plot_springConstant,
        img_format = input$ea_cnet_plot_img_format)

      if (!input$ea_cnet_plot_auto_refresh) {
        r6$params$ea_cnet_plot$auto_refresh = input$ea_cnet_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "EA CNET plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ea_cnet_plot_generate,
               spawn_function = ea_cnet_plot_spawn,
               img_format = input$ea_cnet_plot_img_format,
               input = input,
               output = output,
               session = session)


    })


    # Download associated tables
    output$download_ea_cnet_plot_node_table = shiny::downloadHandler(
      filename = function(){timestamped_name("ea_cnet_plot_node_table.csv")},
      content = function(file_name){
        write.csv(r6$tables$ea_cnet_plot$node_table, file_name)
      }
    )
    output$download_ea_cnet_plot_edge_table = shiny::downloadHandler(
      filename = function(){timestamped_name("ea_cnet_plot_edge_table.csv")},
      content = function(file_name){
        write.csv(r6$tables$ea_cnet_plot$edge_table, file_name)
      }
    )

  # Expanded boxes
  ea_cnet_plot_proxy = visNetwork::visNetworkProxy(shinyId = "ea_cnet_plot_plot",
                                                session = session)
  shiny::observeEvent(input$ea_cnet_plot_plotbox,{
    if (input$ea_cnet_plot_plotbox$maximized) {
      visNetwork::visOptions(graph = ea_cnet_plot_proxy,
                             width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                             height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full)
    } else {
      visNetwork::visOptions(graph = ea_cnet_plot_proxy,
                             width = dimensions_obj$xpx * dimensions_obj$x_plot,
                             height = dimensions_obj$ypx * dimensions_obj$y_plot)
    }
  })

}

#------------------------------------------------------------ EA Ridge plot ----
ea_ridge_plot_generate = function(r6, dimensions_obj, input) {

  if (input$ea_ridge_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  print_tm(r6$name, "EA Ridge plot: generating plot.")
  r6$plot_ea_ridge_plot(width = width,
                        height = height)
}

ea_ridge_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "EA Ridge plot: spawning plot.")
  output$ea_ridge_plot_plot = plotly::renderPlotly({
    r6$plots$ea_ridge_plot
    plotly::config(r6$plots$ea_ridge_plot, toImageButtonOptions = list(format= format,
                                                                   filename= timestamped_module_name(r6, 'ea_ridge_plot'),
                                                                   height= NULL,
                                                                   width= NULL,
                                                                   scale= 1))
  })
}

ea_ridge_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "ea_ridge_plot",
                 label = "EA Ridge plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ea_ridge_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ea_ridge_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('ea_ridge_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ea_ridge_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_show_category"),
        label = "Show category",
        value = r6$params$ea_ridge_plot$show_category,
        min = 1,
        max = NA,
        step = 1
      ),
      shiny::selectInput(
        inputId = ns("ea_ridge_plot_fill"),
        label = "Fill",
        choices = c("pvalue", "p.adjust", "enrichmentScore", "NES"),
        selected = r6$params$ea_ridge_plot$fill,
        width = "100%"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_ridge_plot_core_enrichment'),
        label = 'Core enrichment',
        value = r6$params$ea_ridge_plot$core_enrichment,
        right = TRUE,
        status = "primary"
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ea_ridge_plot_displayed_label"),
        label = "Displayed label",
        choices = c("ID", "Description", "None"),
        selected = r6$params$ea_ridge_plot$displayed_label,
        width = "100%"
      ),
      shiny::selectInput(
        inputId = ns("ea_ridge_plot_orderBy"),
        label = "Order by",
        choices = c("pvalue", "p.adjust", "enrichmentScore", "NES"),
        selected = r6$params$ea_ridge_plot$orderBy,
        width = "100%"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_ridge_plot_decreasing'),
        label = 'Reverse order',
        value = r6$params$ea_ridge_plot$decreasing,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ea_ridge_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ea_ridge_plot$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_ridge_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$ea_ridge_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$ea_ridge_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_yaxis_word_split"),
        label = 'y-axis word split',
        value = r6$params$ea_ridge_plot$yaxis_word_split,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$ea_ridge_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$ea_ridge_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$ea_ridge_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$ea_ridge_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_ridge_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$ea_ridge_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      # Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ea_ridge_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ea_ridge_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("ea_ridge_plot_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

ea_ridge_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ea_ridge_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ea_ridge_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "EA Ridge plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ea_ridge_plot_generate,
             spawn_function = ea_ridge_plot_spawn,
             img_format = input$ea_ridge_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ea_ridge_plot_show_category,
    input$ea_ridge_plot_fill,
    input$ea_ridge_plot_core_enrichment,
    input$ea_ridge_plot_displayed_label,
    input$ea_ridge_plot_orderBy,
    input$ea_ridge_plot_decreasing,
    input$ea_ridge_plot_color_palette,
    input$ea_ridge_plot_reverse_palette,
    input$ea_ridge_plot_title_font_size,
    input$ea_ridge_plot_yaxis_word_split,
    input$ea_ridge_plot_y_label_font_size,
    input$ea_ridge_plot_y_tick_font_size,
    input$ea_ridge_plot_x_label_font_size,
    input$ea_ridge_plot_x_tick_font_size,
    input$ea_ridge_plot_legend_font_size,
    input$ea_ridge_plot_img_format),{

      # Update params
      r6$param_ea_ridge_plot(
        auto_refresh = input$ea_ridge_plot_auto_refresh,
        show_category = input$ea_ridge_plot_show_category,
        fill = input$ea_ridge_plot_fill,
        core_enrichment = input$ea_ridge_plot_core_enrichment,
        displayed_label = input$ea_ridge_plot_displayed_label,
        orderBy = input$ea_ridge_plot_orderBy,
        decreasing = input$ea_ridge_plot_decreasing,
        color_palette = input$ea_ridge_plot_color_palette,
        reverse_palette = input$ea_ridge_plot_reverse_palette,
        title_font_size = input$ea_ridge_plot_title_font_size,
        yaxis_word_split = input$ea_ridge_plot_yaxis_word_split,
        y_label_font_size = input$ea_ridge_plot_y_label_font_size,
        y_tick_font_size = input$ea_ridge_plot_y_tick_font_size,
        x_label_font_size = input$ea_ridge_plot_x_label_font_size,
        x_tick_font_size = input$ea_ridge_plot_x_tick_font_size,
        legend_font_size = input$ea_ridge_plot_legend_font_size,
        img_format = input$ea_ridge_plot_img_format
      )

      if (!input$ea_ridge_plot_auto_refresh) {
        r6$params$ea_ridge_plot$auto_refresh = input$ea_ridge_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "EA Ridge plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ea_ridge_plot_generate,
               spawn_function = ea_ridge_plot_spawn,
               img_format = input$ea_ridge_plot_img_format,
               input = input,
               output = output,
               session = session)


    })


  # Download associated tables
  output$ea_ridge_plot_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ea_ridge_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ea_ridge_plot, file_name)
    }
  )


  # Expanded boxes
  ea_ridge_plot_proxy = plotly::plotlyProxy(outputId = "ea_ridge_plot_plot",
                                            session = session)

  shiny::observeEvent(input$ea_ridge_plot_plotbox,{
    if (input$ea_ridge_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = ea_ridge_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = ea_ridge_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------------- EA EMAP plot ----
ea_emap_plot_generate = function(r6, dimensions_obj, input) {

  print_tm(r6$name, "EA EMAP plot: generating plot.")

  if (input$ea_emap_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_ea_emap_plot(width = width,
                       height = height)
}

ea_emap_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "EA EMAP plot: spawning plot.")

  output$ea_emap_plot_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$ea_emap_plot,
  )
}

ea_emap_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "ea_emap_plot",
                 label = "EA EMAP plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ea_emap_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ea_emap_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('ea_emap_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ea_emap_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns("ea_emap_plot_show_category"),
        label = "Show category",
        value = r6$params$ea_emap_plot$show_category,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_displayed_labels"),
        label = "Displayed labels",
        choices = c('ID and Description', 'ID', 'Description'),
        selected = r6$params$ea_emap_plot$displayed_labels,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_mode"),
        label = "Mode",
        choices = c("Both", "Activated", "Suppressed"),
        selected = r6$params$ea_emap_plot$mode,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_color"),
        label = "Node colors",
        choices = c('GeneRatio', 'GeneCount', 'pvalue', 'p.adjust', 'enrichmentScore', 'NES'), # c('GeneCount', 'enrichmentScore', 'NES', 'pvalue', 'p.adjust'),
        selected = r6$params$ea_emap_plot$color,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_size"),
        label = "Node size",
        choices = c('GeneRatio', 'GeneCount', 'pvalue', 'p.adjust'), # c('GeneCount', 'enrichmentScore', 'NES', 'pvalue', 'p.adjust'),
        selected = r6$params$ea_emap_plot$size,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_edge_color"),
        label = "Edge colors",
        choices = c('None', 'Similarity score'),
        selected = r6$params$ea_emap_plot$edge_color,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_edge_width"),
        label = "Edge width",
        choices = c('None', 'Similarity score'),
        selected = r6$params$ea_emap_plot$edge_width,
        multiple = FALSE,
        width = '100%'
      ),

      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::sliderInput(
        inputId = ns("ea_emap_plot_score_threshold"),
        label = 'Score threshold',
        min = 0.0,
        max = 1.0,
        value = r6$params$ea_emap_plot$score_threshold,
        step = 0.01,
        width = "100%"
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_similarity_score"),
        label = 'Similarity score',
        choices = c('JC', 'Wang', 'Jiang', 'Rel', 'Lin', 'Resnik'),
        selected = r6$params$ea_emap_plot$similarity_score,
        width = "100%"
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ea_emap_plot_node_color_palette'),
        label = 'Node color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ea_emap_plot$node_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_emap_plot_reverse_node_palette'),
        label = 'Reverse node palette',
        value = r6$params$ea_emap_plot$reverse_node_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('ea_emap_plot_edge_color_palette'),
        label = 'Edge color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ea_emap_plot$edge_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_emap_plot_reverse_edge_palette'),
        label = 'Reverse edge palette',
        value = r6$params$ea_emap_plot$reverse_edge_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('ea_emap_plot_node_magnifier'),
        label = 'Node magnifier',
        value = r6$params$ea_emap_plot$node_magnifier,
        min = 0.1,
        max = 10,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('ea_emap_plot_edge_magnifier'),
        label = 'Edge magnifier',
        value = r6$params$ea_emap_plot$edge_magnifier,
        min = 0.1,
        max = 10,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('ea_emap_plot_label_font_size'),
        label = 'Label font size',
        value = r6$params$ea_emap_plot$label_font_size,
        min = 1,
        max = 300,
        step = 1,
        width = '100%'
      ),
      ## Network settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('ea_emap_plot_static_network'),
        label = 'Static network',
        value = r6$params$ea_emap_plot$static_network,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_solver"),
        label = "Solver",
        choices = c('barnesHut', 'repulsion'),
        selected = r6$params$ea_emap_plot$solver,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_emap_plot_gravitationalConstant"),
        label = "Gravitational constant (BarnesHut)",
        value = r6$params$ea_emap_plot$gravitationalConstant,
        min = NA,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_emap_plot_nodeDistance"),
        label = "Node distance (Repulsion)",
        value = r6$params$ea_emap_plot$nodeDistance,
        min = 1,
        max = NA,
        step = 10,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_emap_plot_centralGravity"),
        label = "Central gravity",
        value = r6$params$ea_emap_plot$centralGravity,
        min = NA,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_emap_plot_springLength"),
        label = "Spring length",
        value = r6$params$ea_emap_plot$springLength,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ea_emap_plot_springConstant"),
        label = "Spring constant",
        value = r6$params$ea_emap_plot$springConstant,
        min = NA,
        max = NA,
        step = 0.01,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ea_emap_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ea_emap_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("download_ea_emap_plot_node_table"),
        label = "Download node table",
        style = "width:100%;"
      ),
      shiny::downloadButton(
        outputId = ns("download_ea_emap_plot_edge_table"),
        label = "Download edge table",
        style = "width:100%;"
      )
    )
  })
}

ea_emap_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ea_emap_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ea_emap_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "EA EMAP plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ea_emap_plot_generate,
             spawn_function = ea_emap_plot_spawn,
             img_format = input$ea_emap_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ea_emap_plot_show_category,
    input$ea_emap_plot_displayed_labels,
    input$ea_emap_plot_mode,
    input$ea_emap_plot_color,
    input$ea_emap_plot_size,
    input$ea_emap_plot_edge_color,
    input$ea_emap_plot_edge_width,
    input$ea_emap_plot_score_threshold,
    input$ea_emap_plot_similarity_score,
    input$ea_emap_plot_node_color_palette,
    input$ea_emap_plot_reverse_node_palette,
    input$ea_emap_plot_edge_color_palette,
    input$ea_emap_plot_reverse_edge_palette,
    input$ea_emap_plot_node_magnifier,
    input$ea_emap_plot_edge_magnifier,
    input$ea_emap_plot_label_font_size,
    input$ea_emap_plot_static_network,
    input$ea_emap_plot_solver,
    input$ea_emap_plot_gravitationalConstant,
    input$ea_emap_plot_nodeDistance,
    input$ea_emap_plot_centralGravity,
    input$ea_emap_plot_springLength,
    input$ea_emap_plot_springConstant,
    input$ea_emap_plot_img_format),{

      # Update params
      r6$param_ea_emap_plot(
        auto_refresh = input$ea_emap_plot_auto_refresh,
        show_category = input$ea_emap_plot_show_category,
        displayed_labels = input$ea_emap_plot_displayed_labels,
        mode = input$ea_emap_plot_mode,
        color = input$ea_emap_plot_color,
        size = input$ea_emap_plot_size,
        edge_color = input$ea_emap_plot_edge_color,
        edge_width = input$ea_emap_plot_edge_width,
        score_threshold = input$ea_emap_plot_score_threshold,
        similarity_score = input$ea_emap_plot_similarity_score,
        node_color_palette = input$ea_emap_plot_node_color_palette,
        reverse_node_palette = input$ea_emap_plot_reverse_node_palette,
        edge_color_palette = input$ea_emap_plot_edge_color_palette,
        reverse_edge_palette = input$ea_emap_plot_reverse_edge_palette,
        node_magnifier = input$ea_emap_plot_node_magnifier,
        edge_magnifier = input$ea_emap_plot_edge_magnifier,
        label_font_size = input$ea_emap_plot_label_font_size,
        static_network = input$ea_emap_plot_static_network,
        solver = input$ea_emap_plot_solver,
        gravitationalConstant = input$ea_emap_plot_gravitationalConstant,
        nodeDistance = input$ea_emap_plot_nodeDistance,
        centralGravity = input$ea_emap_plot_centralGravity,
        springLength = input$ea_emap_plot_springLength,
        springConstant = input$ea_emap_plot_springConstant,
        img_format = input$ea_emap_plot_img_format)

      if (!input$ea_emap_plot_auto_refresh) {
        r6$params$ea_emap_plot$auto_refresh = input$ea_emap_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "EA EMAP plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ea_emap_plot_generate,
               spawn_function = ea_emap_plot_spawn,
               img_format = input$ea_emap_plot_img_format,
               input = input,
               output = output,
               session = session)


    })


  # Download associated tables
  output$download_ea_emap_plot_node_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ea_emap_plot_node_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ea_emap_plot$node_table, file_name)
    }
  )
  output$download_ea_emap_plot_edge_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ea_emap_plot_edge_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ea_emap_plot$edge_table, file_name)
    }
  )

  # Expanded boxes
  ea_emap_plot_proxy = visNetwork::visNetworkProxy(shinyId = "ea_emap_plot_plot",
                                                   session = session)
  shiny::observeEvent(input$ea_emap_plot_plotbox,{
    if (input$ea_emap_plot_plotbox$maximized) {
      visNetwork::visOptions(graph = ea_emap_plot_proxy,
                             width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                             height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full)
    } else {
      visNetwork::visOptions(graph = ea_emap_plot_proxy,
                             width = dimensions_obj$xpx * dimensions_obj$x_plot,
                             height = dimensions_obj$ypx * dimensions_obj$y_plot)
    }
  })

}

#------------------------------------------------------------- ORA Dot plot ----
ora_dot_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "ORA dot plot: generating plot.")
  if (input$ora_dot_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_ora_dot_plot(width = width,
                       height = height)
}

ora_dot_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "ORA dot plot: spawning plot.")

  output$ora_dot_plot_plot = plotly::renderPlotly({
    r6$plots$ora_dot_plot
    plotly::config(r6$plots$ora_dot_plot, toImageButtonOptions = list(format= format,
                                                                     filename= timestamped_module_name(r6, 'ora_dot_plot'),
                                                                     height= NULL,
                                                                     width= NULL,
                                                                     scale= 1))
  })
}

ora_dot_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "ora_dot_plot",
                 label = "ORA dot plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ora_dot_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ora_dot_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('ora_dot_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ora_dot_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_dot_plot_x"),
        label = "x-axis",
        choices = c("GeneRatio", "GeneCount", "setSize"),
        selected = r6$params$ora_dot_plot$x,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_dot_plot_y"),
        label = "y-axis",
        choices = c("ID", "Description"),
        selected = r6$params$ora_dot_plot$y,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_dot_plot_color"),
        label = "Coloring",
        choices = c("pvalue", "p.adjust"),
        selected = r6$params$ora_dot_plot$color,
        multiple = F,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_dot_plot_show_categories"),
        label = "Show categories",
        value = r6$params$ora_dot_plot$show_categories,
        min = 1,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_dot_plot_size"),
        label = "Size",
        choices = c("GeneRatio", "GeneCount", "setSize"),
        selected = r6$params$ora_dot_plot$size,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_dot_plot_order_by"),
        label = "Order by",
        choices = c("GeneRatio", "GeneCount", "setSize"),
        selected = r6$params$ora_dot_plot$order_by,
        multiple = F,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_dot_plot_reverse_order'),
        label = 'Reverse order',
        value = r6$params$ora_dot_plot$reverse_order,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ora_dot_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ora_dot_plot$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_dot_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$ora_dot_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('ora_dot_plot_size_ref'),
        label = "Size ref",
        value = r6$params$ora_dot_plot$size_ref,
        min = 1,
        step = 0.25,
        width = "100%"
      ),
      shiny::sliderInput(
        inputId = ns("ora_dot_plot_marker_opacity"),
        label = 'Marker opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$ora_dot_plot$marker_opacity,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_dot_plot_yaxis_word_split"),
        label = 'yaxis split',
        min = 0,
        max = 5,
        value = r6$params$ora_dot_plot$yaxis_word_split,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_dot_plot_title_size"),
        label = 'Title size',
        min = 0,
        max = 50,
        value = r6$params$ora_dot_plot$title_size,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_dot_plot_xlabel_size"),
        label = 'x-label size',
        min = 0,
        max = 50,
        value = r6$params$ora_dot_plot$xlabel_size,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_dot_plot_xtick_size"),
        label = 'x-tick size',
        min = 0,
        max = 50,
        value = r6$params$ora_dot_plot$xtick_size,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_dot_plot_ytick_size"),
        label = 'y-tick size',
        min = 0,
        max = 50,
        value = r6$params$ora_dot_plot$ytick_size,
        step = 1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_dot_plot_show_legend'),
        label = 'Show legend',
        value = r6$params$ora_dot_plot$show_legend,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("ora_dot_plot_legend_size"),
        label = 'Legend font size',
        min = 1,
        max = 50,
        value = r6$params$ora_dot_plot$legend_size,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_dot_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ora_dot_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("download_ora_dot_plot_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

ora_dot_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ora_dot_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ora_dot_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "ORA dot plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ora_dot_plot_generate,
             spawn_function = ora_dot_plot_spawn,
             img_format = input$ora_dot_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ora_dot_plot_x,
    input$ora_dot_plot_y,
    input$ora_dot_plot_color,
    input$ora_dot_plot_show_categories,
    input$ora_dot_plot_size,
    input$ora_dot_plot_order_by,
    input$ora_dot_plot_reverse_order,
    input$ora_dot_plot_color_palette,
    input$ora_dot_plot_reverse_palette,
    input$ora_dot_plot_marker_opacity,
    input$ora_dot_plot_size_ref,
    input$ora_dot_plot_yaxis_word_split,
    input$ora_dot_plot_title_size,
    input$ora_dot_plot_xlabel_size,
    input$ora_dot_plot_xtick_size,
    input$ora_dot_plot_ytick_size,
    input$ora_dot_plot_show_legend,
    input$ora_dot_plot_legend_size,
    input$ora_dot_plot_img_format),{

      # Update params
      r6$param_ora_dot_plot(
        auto_refresh = input$ora_dot_plot_auto_refresh,
        x = input$ora_dot_plot_x,
        y = input$ora_dot_plot_y,
        color = input$ora_dot_plot_color,
        show_categories = input$ora_dot_plot_show_categories,
        size = input$ora_dot_plot_size,
        order_by = input$ora_dot_plot_order_by,
        reverse_order = input$ora_dot_plot_reverse_order,
        color_palette = input$ora_dot_plot_color_palette,
        reverse_palette = input$ora_dot_plot_reverse_palette,
        show_legend = input$ora_dot_plot_show_legend,
        size_ref = input$ora_dot_plot_size_ref,
        marker_opacity = input$ora_dot_plot_marker_opacity,
        yaxis_word_split = input$ora_dot_plot_yaxis_word_split,
        title_size = input$ora_dot_plot_title_size,
        xlabel_size = input$ora_dot_plot_xlabel_size,
        xtick_size = input$ora_dot_plot_xtick_size,
        ytick_size = input$ora_dot_plot_ytick_size,
        legend_size = input$ora_dot_plot_legend_size,
        img_format = input$ora_dot_plot_img_format)

      if (!input$ora_dot_plot_auto_refresh) {
        r6$params$ora_dot_plot$auto_refresh = input$ora_dot_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "ORA dot plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ora_dot_plot_generate,
               spawn_function = ora_dot_plot_spawn,
               img_format = input$ora_dot_plot_img_format,
               input = input,
               output = output,
               session = session)


    })


  # Download associated tables
  output$download_ora_dot_plot_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ora_dot_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ora_dot_plot, file_name)
    }
  )

  # Expanded boxes
  ora_dot_plot_proxy = plotly::plotlyProxy(outputId = "ora_dot_plot_plot",
                                          session = session)

  shiny::observeEvent(input$ora_dot_plot_plotbox,{
    if (input$ora_dot_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = ora_dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = ora_dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------------- ORA Bar plot ----
ora_bar_plot_generate = function(r6, dimensions_obj, input) {

  if (input$ora_bar_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  print_tm(r6$name, "ORA Bar plot: generating plot.")
  r6$plot_ora_bar_plot(width = width,
                      height = height)
}

ora_bar_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "ORA Bar plot: spawning plot.")

  output$ora_bar_plot_plot = plotly::renderPlotly({
    r6$plots$ora_bar_plot
    plotly::config(r6$plots$ora_bar_plot, toImageButtonOptions = list(format= format,
                                                                    filename= timestamped_module_name(r6, 'ora_bar_plot'),
                                                                    height= NULL,
                                                                    width= NULL,
                                                                    scale= 1))
  })
}

ora_bar_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "ora_bar_plot",
                 label = "Bar plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ora_bar_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ora_bar_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(

      shinyWidgets::materialSwitch(
        inputId = ns('ora_bar_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ora_bar_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_bar_plot_x"),
        label = "x-axis",
        choices = c("GeneRatio", "GeneCount"),
        selected = r6$params$ora_bar_plot$x,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_bar_plot_color"),
        label = "Color",
        choices = c("pvalue", "p.adjust"),
        selected = r6$params$ora_bar_plot$color,
        multiple = F,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_show_category"),
        label = "Show category",
        value = r6$params$ora_bar_plot$show_category,
        min = 1,
        step = 1,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_bar_plot_displayed_label"),
        label = "Displayed label",
        choices = c("ID", "Description", "None"),
        selected = r6$params$ora_bar_plot$displayed_label,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_bar_plot_order_by"),
        label = "Order by",
        choices = c("GeneRatio", "GeneCount", "pvalue", "p.adjust"),
        selected = r6$params$ora_bar_plot$order_by,
        multiple = F,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_bar_plot_order_decreasing'),
        label = 'Order decreasing',
        value = r6$params$ora_bar_plot$order_decreasing,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ora_bar_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ora_bar_plot$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_bar_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$ora_bar_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$ora_bar_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_yaxis_word_split"),
        label = 'y-axis word split',
        value = r6$params$ora_bar_plot$yaxis_word_split,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$ora_bar_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$ora_bar_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$ora_bar_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$ora_bar_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_bar_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$ora_bar_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_bar_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ora_bar_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("download_ora_bar_plot_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

ora_bar_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ora_bar_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ora_bar_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "ORA Bar plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ora_bar_plot_generate,
             spawn_function = ora_bar_plot_spawn,
             img_format = input$ora_bar_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ora_bar_plot_x,
    input$ora_bar_plot_color,
    input$ora_bar_plot_show_category,
    input$ora_bar_plot_displayed_label,
    input$ora_bar_plot_order_by,
    input$ora_bar_plot_order_decreasing,
    input$ora_bar_plot_color_palette,
    input$ora_bar_plot_reverse_palette,
    input$ora_bar_plot_title_font_size,
    input$ora_bar_plot_yaxis_word_split,
    input$ora_bar_plot_y_label_font_size,
    input$ora_bar_plot_y_tick_font_size,
    input$ora_bar_plot_x_label_font_size,
    input$ora_bar_plot_x_tick_font_size,
    input$ora_bar_plot_legend_font_size,
    input$ora_bar_plot_img_format),{

      # Update params
      r6$param_ora_bar_plot(
        auto_refresh = input$ora_bar_plot_auto_refresh,
        x = input$ora_bar_plot_x,
        color = input$ora_bar_plot_color,
        show_category = input$ora_bar_plot_show_category,
        displayed_label = input$ora_bar_plot_displayed_label,
        order_by = input$ora_bar_plot_order_by,
        order_decreasing = input$ora_bar_plot_order_decreasing,
        color_palette = input$ora_bar_plot_color_palette,
        reverse_palette = input$ora_bar_plot_reverse_palette,
        title_font_size = input$ora_bar_plot_title_font_size,
        yaxis_word_split = input$ora_bar_plot_yaxis_word_split,
        y_label_font_size = input$ora_bar_plot_y_label_font_size,
        y_tick_font_size = input$ora_bar_plot_y_tick_font_size,
        x_label_font_size = input$ora_bar_plot_x_label_font_size,
        x_tick_font_size = input$ora_bar_plot_x_tick_font_size,
        legend_font_size = input$ora_bar_plot_legend_font_size,
        input$ora_bar_plot_img_format
      )

      if (!input$ora_bar_plot_auto_refresh) {
        r6$params$ora_bar_plot$auto_refresh = input$ora_bar_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "ORA Bar plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ora_bar_plot_generate,
               spawn_function = ora_bar_plot_spawn,
               img_format = input$ora_bar_plot_img_format,
               input = input,
               output = output,
               session = session)

    })


    # Download associated tables
    output$download_ora_bar_plot_table = shiny::downloadHandler(
      filename = function(){timestamped_name("ora_bar_plot_table.csv")},
      content = function(file_name){
        write.csv(r6$tables$ora_bar_plot, file_name)
      }
    )

  # Expanded boxes
  ora_bar_plot_proxy = plotly::plotlyProxy(outputId = "ora_bar_plot_plot",
                                          session = session)

  shiny::observeEvent(input$ora_bar_plot_plotbox,{
    if (input$ora_bar_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = ora_bar_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = ora_bar_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#------------------------------------------------------------ ORA CNET plot ----
ora_cnet_plot_generate = function(r6, dimensions_obj, input) {

  print_tm(r6$name, "ORA CNET plot: generating plot.")

  if (input$ora_cnet_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_ora_cnet_plot(width = width,
                       height = height)
}

ora_cnet_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "ORA CNET plot: spawning plot.")

  output$ora_cnet_plot_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$ora_cnet_plot,
  )
}

ora_cnet_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "ora_cnet_plot",
                 label = "ORA CNET plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ora_cnet_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ora_cnet_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('ora_cnet_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ora_cnet_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns("ora_cnet_plot_show_category"),
        label = "Show category",
        value = r6$params$ora_cnet_plot$show_category,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_cnet_plot_displayed_labels"),
        label = "Displayed labels",
        choices = c('ID and Description', 'ID', 'Description'),
        selected = r6$params$ora_cnet_plot$displayed_labels,
        multiple = FALSE,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_cnet_plot_set_node_annotations"),
        label = "Set node annotations",
        choices = c('None', 'GeneRatio', 'GeneCount', 'pvalue', 'p.adjust'),
        selected = r6$params$ora_cnet_plot$set_node_annotations,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_cnet_plot_feature_node_annotations"),
        label = "Feature node annotations",
        choices = c('None', colnames(r6$tables$ea_feature_table)),
        selected = r6$params$ora_cnet_plot$feature_node_annotations,
        multiple = FALSE,
        width = '100%'
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ora_cnet_plot_set_node_color_palette'),
        label = 'Set color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ora_cnet_plot$set_node_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_cnet_plot_reverse_set_palette'),
        label = 'Reverse set palette',
        value = r6$params$ora_cnet_plot$reverse_set_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('ora_cnet_plot_feature_node_color_palette'),
        label = 'Feature color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ora_cnet_plot$feature_node_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_cnet_plot_reverse_feature_palette'),
        label = 'Reverse feature palette',
        value = r6$params$ora_cnet_plot$reverse_feature_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('ora_cnet_plot_label_font_size'),
        label = 'Label font size',
        value = r6$params$ora_cnet_plot$label_font_size,
        min = 1,
        max = 300,
        step = 1,
        width = '100%'
      ),
      ## Network settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_cnet_plot_static_network'),
        label = 'Static network',
        value = r6$params$ora_cnet_plot$static_network,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("ora_cnet_plot_solver"),
        label = "Solver",
        choices = c('barnesHut', 'repulsion'),
        selected = r6$params$ora_cnet_plot$solver,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_cnet_plot_gravitationalConstant"),
        label = "Gravitational constant (BarnesHut)",
        value = r6$params$ora_cnet_plot$gravitationalConstant,
        min = NA,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_cnet_plot_nodeDistance"),
        label = "Node distance (Repulsion)",
        value = r6$params$ora_cnet_plot$nodeDistance,
        min = 1,
        max = NA,
        step = 10,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_cnet_plot_centralGravity"),
        label = "Central gravity",
        value = r6$params$ora_cnet_plot$centralGravity,
        min = NA,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_cnet_plot_springLength"),
        label = "Spring length",
        value = r6$params$ora_cnet_plot$springLength,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_cnet_plot_springConstant"),
        label = "Spring constant",
        value = r6$params$ora_cnet_plot$springConstant,
        min = NA,
        max = NA,
        step = 0.01,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_cnet_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ora_cnet_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("download_ora_cnet_plot_node_table"),
        label = "Download node table",
        style = "width:100%;"
      ),
      shiny::downloadButton(
        outputId = ns("download_ora_cnet_plot_edge_table"),
        label = "Download edge table",
        style = "width:100%;"
      )
    )
  })
}

ora_cnet_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ora_cnet_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ora_cnet_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "ORA CNET plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ora_cnet_plot_generate,
             spawn_function = ora_cnet_plot_spawn,
             img_format = input$ora_cnet_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ora_cnet_plot_show_category,
    input$ora_cnet_plot_displayed_labels,
    input$ora_cnet_plot_set_node_annotations,
    input$ora_cnet_plot_feature_node_annotations,
    input$ora_cnet_plot_set_node_color_palette,
    input$ora_cnet_plot_reverse_set_palette,
    input$ora_cnet_plot_feature_node_color_palette,
    input$ora_cnet_plot_reverse_feature_palette,
    input$ora_cnet_plot_label_font_size,
    input$ora_cnet_plot_static_network,
    input$ora_cnet_plot_solver,
    input$ora_cnet_plot_gravitationalConstant,
    input$ora_cnet_plot_nodeDistance,
    input$ora_cnet_plot_centralGravity,
    input$ora_cnet_plot_springLength,
    input$ora_cnet_plot_springConstant,
    input$ora_cnet_plot_img_format),{

      # Update params
      r6$param_ora_cnet_plot(
        auto_refresh = input$ora_cnet_plot_auto_refresh,
        show_category = input$ora_cnet_plot_show_category,
        displayed_labels = input$ora_cnet_plot_displayed_labels,
        set_node_annotations = input$ora_cnet_plot_set_node_annotations,
        feature_node_annotations = input$ora_cnet_plot_feature_node_annotations,
        set_node_color_palette = input$ora_cnet_plot_set_node_color_palette,
        reverse_set_palette = input$ora_cnet_plot_reverse_set_palette,
        feature_node_color_palette = input$ora_cnet_plot_feature_node_color_palette,
        reverse_feature_palette = input$ora_cnet_plot_reverse_feature_palette,
        label_font_size = input$ora_cnet_plot_label_font_size,
        static_network = input$ora_cnet_plot_static_network,
        solver = input$ora_cnet_plot_solver,
        gravitationalConstant = input$ora_cnet_plot_gravitationalConstant,
        nodeDistance = input$ora_cnet_plot_nodeDistance,
        centralGravity = input$ora_cnet_plot_centralGravity,
        springLength = input$ora_cnet_plot_springLength,
        springConstant = input$ora_cnet_plot_springConstant,
        img_format = input$ora_cnet_plot_img_format)

      if (!input$ora_cnet_plot_auto_refresh) {
        r6$params$ora_cnet_plot$auto_refresh = input$ora_cnet_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "ORA CNET plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ora_cnet_plot_generate,
               spawn_function = ora_cnet_plot_spawn,
               img_format = input$ora_cnet_plot_img_format,
               input = input,
               output = output,
               session = session)


    })


  # Download associated tables
  output$download_ora_cnet_plot_node_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ora_cnet_plot_node_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ora_cnet_plot$node_table, file_name)
    }
  )
  output$download_ora_cnet_plot_edge_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ora_cnet_plot_edge_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ora_cnet_plot$edge_table, file_name)
    }
  )

  # Expanded boxes
  ora_cnet_plot_proxy = visNetwork::visNetworkProxy(shinyId = "ora_cnet_plot_plot",
                                                   session = session)
  shiny::observeEvent(input$ora_cnet_plot_plotbox,{
    if (input$ora_cnet_plot_plotbox$maximized) {
      visNetwork::visOptions(graph = ora_cnet_plot_proxy,
                             width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                             height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full)
    } else {
      visNetwork::visOptions(graph = ora_cnet_plot_proxy,
                             width = dimensions_obj$xpx * dimensions_obj$x_plot,
                             height = dimensions_obj$ypx * dimensions_obj$y_plot)
    }
  })

}



#------------------------------------------------------------ ORA EMAP plot ----
ora_emap_plot_generate = function(r6, dimensions_obj, input) {

  print_tm(r6$name, "ORA EMAP plot: generating plot.")

  if (input$ora_emap_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_ora_emap_plot(width = width,
                       height = height)
}

ora_emap_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "ORA EMAP plot: spawning plot.")

  output$ora_emap_plot_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$ora_emap_plot,
  )
}

ora_emap_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "ora_emap_plot",
                 label = "ORA EMAP plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


ora_emap_plot_server = function(r6, output, session) {

  ns = session$ns

  output$ora_emap_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('ora_emap_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$ora_emap_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::numericInput(
        inputId = ns("ora_emap_plot_show_category"),
        label = "Show category",
        value = r6$params$ora_emap_plot$show_category,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_displayed_labels"),
        label = "Displayed labels",
        choices = c('ID and Description', 'ID', 'Description'),
        selected = r6$params$ora_emap_plot$displayed_labels,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_color"),
        label = "Node colors",
        choices = c('GeneCount', 'enrichmentScore', 'NES', 'pvalue', 'p.adjust'),
        selected = r6$params$ora_emap_plot$color,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_size"),
        label = "Node size",
        choices = c('GeneCount', 'enrichmentScore', 'NES', 'pvalue', 'p.adjust'),
        selected = r6$params$ora_emap_plot$size,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_edge_color"),
        label = "Edge colors",
        choices = c('None', 'Similarity score'),
        selected = r6$params$ora_emap_plot$edge_color,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_edge_width"),
        label = "Edge width",
        choices = c('None', 'Similarity score'),
        selected = r6$params$ora_emap_plot$edge_width,
        multiple = FALSE,
        width = '100%'
      ),

      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::sliderInput(
        inputId = ns("ora_emap_plot_score_threshold"),
        label = 'Score threshold',
        min = 0.0,
        max = 1.0,
        value = r6$params$ora_emap_plot$score_threshold,
        step = 0.01,
        width = "100%"
      ),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_similarity_score"),
        label = 'Similarity score',
        choices = c('JC', 'Wang', 'Jiang', 'Rel', 'Lin', 'Resnik'),
        selected = r6$params$ora_emap_plot$similarity_score,
        width = "100%"
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('ora_emap_plot_node_color_palette'),
        label = 'Node color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ora_emap_plot$node_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_emap_plot_reverse_node_palette'),
        label = 'Reverse node palette',
        value = r6$params$ora_emap_plot$reverse_node_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('ora_emap_plot_edge_color_palette'),
        label = 'Edge color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$ora_emap_plot$edge_color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_emap_plot_reverse_edge_palette'),
        label = 'Reverse edge palette',
        value = r6$params$ora_emap_plot$reverse_edge_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('ora_emap_plot_node_magnifier'),
        label = 'Node magnifier',
        value = r6$params$ora_emap_plot$node_magnifier,
        min = 0.1,
        max = 10,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('ora_emap_plot_edge_magnifier'),
        label = 'Edge magnifier',
        value = r6$params$ora_emap_plot$edge_magnifier,
        min = 0.1,
        max = 10,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('ora_emap_plot_label_font_size'),
        label = 'Label font size',
        value = r6$params$ora_emap_plot$label_font_size,
        min = 1,
        max = 300,
        step = 1,
        width = '100%'
      ),
      ## Network settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('ora_emap_plot_static_network'),
        label = 'Static network',
        value = r6$params$ora_emap_plot$static_network,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_solver"),
        label = "Solver",
        choices = c('barnesHut', 'repulsion'),
        selected = r6$params$ora_emap_plot$solver,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_emap_plot_gravitationalConstant"),
        label = "Gravitational constant (BarnesHut)",
        value = r6$params$ora_emap_plot$gravitationalConstant,
        min = NA,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_emap_plot_nodeDistance"),
        label = "Node distance (Repulsion)",
        value = r6$params$ora_emap_plot$nodeDistance,
        min = 1,
        max = NA,
        step = 10,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_emap_plot_centralGravity"),
        label = "Central gravity",
        value = r6$params$ora_emap_plot$centralGravity,
        min = NA,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_emap_plot_springLength"),
        label = "Spring length",
        value = r6$params$ora_emap_plot$springLength,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("ora_emap_plot_springConstant"),
        label = "Spring constant",
        value = r6$params$ora_emap_plot$springConstant,
        min = NA,
        max = NA,
        step = 0.01,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("ora_emap_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ora_emap_plot$img_format,
        width = "100%"
      ),
      shiny::downloadButton(
        outputId = ns("download_ora_emap_plot_node_table"),
        label = "Download node table",
        style = "width:100%;"
      ),
      shiny::downloadButton(
        outputId = ns("download_ora_emap_plot_edge_table"),
        label = "Download edge table",
        style = "width:100%;"
      )
    )
  })
}

ora_emap_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$ora_emap_plot_sidebar)
  monitor_refresh = shiny::reactive(input$ora_emap_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "ORA EMAP plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = ora_emap_plot_generate,
             spawn_function = ora_emap_plot_spawn,
             img_format = input$ora_emap_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  shiny::observeEvent(c(
    input$ora_emap_plot_show_category,
    input$ora_emap_plot_displayed_labels,
    input$ora_emap_plot_color,
    input$ora_emap_plot_size,
    input$ora_emap_plot_edge_color,
    input$ora_emap_plot_edge_width,
    input$ora_emap_plot_score_threshold,
    input$ora_emap_plot_similarity_score,
    input$ora_emap_plot_node_color_palette,
    input$ora_emap_plot_reverse_node_palette,
    input$ora_emap_plot_edge_color_palette,
    input$ora_emap_plot_reverse_edge_palette,
    input$ora_emap_plot_node_magnifier,
    input$ora_emap_plot_edge_magnifier,
    input$ora_emap_plot_label_font_size,
    input$ora_emap_plot_static_network,
    input$ora_emap_plot_solver,
    input$ora_emap_plot_gravitationalConstant,
    input$ora_emap_plot_nodeDistance,
    input$ora_emap_plot_centralGravity,
    input$ora_emap_plot_springLength,
    input$ora_emap_plot_springConstant,
    input$ora_emap_plot_img_format),{

      # Update params
      r6$param_ora_emap_plot(
        auto_refresh = input$ora_emap_plot_auto_refresh,
        show_category = input$ora_emap_plot_show_category,
        displayed_labels = input$ora_emap_plot_displayed_labels,
        color = input$ora_emap_plot_color,
        size = input$ora_emap_plot_size,
        edge_color = input$ora_emap_plot_edge_color,
        edge_width = input$ora_emap_plot_edge_width,
        score_threshold = input$ora_emap_plot_score_threshold,
        similarity_score = input$ora_emap_plot_similarity_score,
        node_color_palette = input$ora_emap_plot_node_color_palette,
        reverse_node_palette = input$ora_emap_plot_reverse_node_palette,
        edge_color_palette = input$ora_emap_plot_edge_color_palette,
        reverse_edge_palette = input$ora_emap_plot_reverse_edge_palette,
        node_magnifier = input$ora_emap_plot_node_magnifier,
        edge_magnifier = input$ora_emap_plot_edge_magnifier,
        label_font_size = input$ora_emap_plot_label_font_size,
        static_network = input$ora_emap_plot_static_network,
        solver = input$ora_emap_plot_solver,
        gravitationalConstant = input$ora_emap_plot_gravitationalConstant,
        nodeDistance = input$ora_emap_plot_nodeDistance,
        centralGravity = input$ora_emap_plot_centralGravity,
        springLength = input$ora_emap_plot_springLength,
        springConstant = input$ora_emap_plot_springConstant,
        img_format = input$ora_emap_plot_img_format)

      if (!input$ora_emap_plot_auto_refresh) {
        r6$params$ora_emap_plot$auto_refresh = input$ora_emap_plot_auto_refresh
        return()
      }

      # Produce the plot
      try_plot(prefix = "ORA EMAP plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = ora_emap_plot_generate,
               spawn_function = ora_emap_plot_spawn,
               img_format = input$ora_emap_plot_img_format,
               input = input,
               output = output,
               session = session)


    })


  # Download associated tables
  output$download_ora_emap_plot_node_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ora_emap_plot_node_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ora_emap_plot$node_table, file_name)
    }
  )
  output$download_ora_emap_plot_edge_table = shiny::downloadHandler(
    filename = function(){timestamped_name("ora_emap_plot_edge_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$ora_emap_plot$edge_table, file_name)
    }
  )

  # Expanded boxes
  ora_emap_plot_proxy = visNetwork::visNetworkProxy(shinyId = "ora_emap_plot_plot",
                                                   session = session)
  shiny::observeEvent(input$ora_emap_plot_plotbox,{
    if (input$ora_emap_plot_plotbox$maximized) {
      visNetwork::visOptions(graph = ora_emap_plot_proxy,
                             width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                             height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full)
    } else {
      visNetwork::visOptions(graph = ora_emap_plot_proxy,
                             width = dimensions_obj$xpx * dimensions_obj$x_plot,
                             height = dimensions_obj$ypx * dimensions_obj$y_plot)
    }
  })

}
