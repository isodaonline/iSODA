#------------------------------------------------------- Dendrogram ----

dendrogram_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Dendrogram: generating plot.")

  if (input$dendrogram_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_dendrogram(width = width,
                     height = height)
}


dendrogram_spawn = function(r6, format, output) {
  print_tm(r6$name, "Dendrogram: spawning plot.")
  output$dendrogram_plot = plotly::renderPlotly({
    r6$plots$dendrogram
    plotly::config(r6$plots$dendrogram, toImageButtonOptions = list(format= format,
                                                                    filename= timestamped_module_name(r6, 'dendrogram'),
                                                                    height= NULL,
                                                                    width= NULL,
                                                                    scale= 1))
  })
}


dendrogram_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "dendrogram",
                 label = "Dendrogram",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


dendrogram_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$dendrogram_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('dendrogram_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$dendrogram$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("dendrogram_dataset"),
        label = "Select table",
        choices = r6$hardcoded_settings$dendrogram$datasets,
        selected = r6$params$dendrogram$dataset,
        width = '100%'
      ),
      shiny::selectizeInput(
        inputId = ns("dendrogram_annotations"),
        label = "Select sample annotation(s)",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$dendrogram$annotations,
        multiple = TRUE,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("dendrogram_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$dendrogram$distance_method,
        multiple = F
      ),

      shiny::numericInput(
        inputId = ns("dendrogram_p"),
        label = "Minkowski distance power",
        value = r6$params$dendrogram$p,
        width = '100%'
      ),

      shiny::selectInput(
        inputId = ns("dendrogram_clustering_method"),
        label = "Clustering method",
        choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
        selected = r6$params$dendrogram$clustering_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns('dendrogram_k_clusters'),
        label = "K clusters",
        value = r6$params$dendrogram$k_clusters,
        width = '100%'
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('dendrogram_color_palette'),
        label = "Color palette(s)",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$dendrogram$color_palette,
        multiple = TRUE,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("dendrogram_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$dendrogram$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("dendrogram_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$dendrogram$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("dendrogram_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$dendrogram$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("dendrogram_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$dendrogram$img_format,
        width = "100%"),
      shiny::actionButton(
        inputId = ns('dendrogram_push_clusters'),
        label = 'Push clusters to meta',
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("download_dendrogram_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}

dendrogram_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$dendrogram_sidebar)
  monitor_refresh = shiny::reactive(input$dendrogram_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$dendrogram$update) {
      try_plot(prefix = "Dendrogram",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = dendrogram_generate,
               spawn_function = dendrogram_spawn,
               img_format = input$dendrogram_img_format,
               toggle_function = "toggle_dendrogram",
               input = input,
               output = output,
               session = session)
    }
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$dendrogram_dataset,
      input$dendrogram_annotations,
      input$dendrogram_distance_method,
      input$dendrogram_p,
      input$dendrogram_clustering_method,
      input$dendrogram_k_clusters,
      input$dendrogram_color_palette,
      input$dendrogram_y_label_font_size,
      input$dendrogram_y_tick_font_size,
      input$dendrogram_x_tick_font_size,
      input$dendrogram_img_format),
    {

      r6$param_dendrogram(auto_refresh = input$dendrogram_auto_refresh,
                          dataset = input$dendrogram_dataset,
                          annotations = input$dendrogram_annotations,
                          distance_method = input$dendrogram_distance_method,
                          p = input$dendrogram_p,
                          clustering_method = input$dendrogram_clustering_method,
                          k_clusters = input$dendrogram_k_clusters,
                          color_palette = input$dendrogram_color_palette,
                          y_label_font_size = input$dendrogram_y_label_font_size,
                          y_tick_font_size = input$dendrogram_y_tick_font_size,
                          x_tick_font_size = input$dendrogram_x_tick_font_size,
                          img_format = input$dendrogram_img_format)

      if (!input$dendrogram_auto_refresh) {
        r6$params$dendrogram$auto_refresh = input$dendrogram_auto_refresh
        return()
      }

      if (r6$params$dendrogram$update) {
        try_plot(prefix = "Dendrogram",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = dendrogram_generate,
                 spawn_function = dendrogram_spawn,
                 img_format = input$dendrogram_img_format,
                 toggle_function = "toggle_dendrogram",
                 input = input,
                 output = output,
                 session = session)
      }


    })

  # Push k clusters to meta
  shiny::observeEvent(input$dendrogram_push_clusters,{
    if ('k_clusters' %in% colnames(r6$tables$dendrogram)){
      k_clusters = r6$tables$dendrogram[rownames(r6$tables$raw_meta), 'k_clusters']
      r6$tables$raw_meta$dendro_clusters = k_clusters
      print_tm(r6$name, "Dendrogram: pushed clusters to the metadata")
    } else {
      print_tm(r6$name, "Dendrogram: no clusters to be pushed")
    }
  })

  # Download associated table
  output$download_dendrogram_table = shiny::downloadHandler(
    filename = function(){timestamped_name("dendrogram_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$dendrogram, file_name)
    }
  )

  # Expanded boxes
  dendrogram_proxy = plotly::plotlyProxy(outputId = "dendrogram_plot",
                                         session = session)

  shiny::observeEvent(input$dendrogram_plotbox,{
    if (input$dendrogram_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = dendrogram_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = dendrogram_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------- Class distribution ----

class_distribution_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Class distribution: generating plot.")

  if (input$class_distribution_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_class_distribution(width = width,
                             height = height)
}


class_distribution_spawn = function(r6, format, output) {
  print_tm(r6$name, "Class distribution: spawning plot.")
  output$class_distribution_plot = plotly::renderPlotly({
    r6$plots$class_distribution
    plotly::config(r6$plots$class_distribution, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_module_name(r6, 'class_distribution'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


class_distribution_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_distribution",
                 label = "Class distribution",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


class_distribution_server = function(r6, output, session) {
  ns = session$ns

  # Generate UI
  output$class_distribution_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('class_distribution_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$class_distribution$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_distribution_dataset"),
        label = "Select table",
        choices = c('Class table', 'Class table total normalized'),
        selected = r6$params$class_distribution$dataset
      ),
      shiny::selectInput(
        inputId = ns("class_distribution_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$class_distribution$group_col
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('class_distribution_color_palette'),
        label = "Color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$class_distribution$color_palette,
        multiple = FALSE
      ),
      shiny::numericInput(
        inputId = ns("class_distribution_title_font_size"),
        label = 'Title font size',
        value = r6$params$class_distribution$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_distribution_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$class_distribution$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_distribution_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$class_distribution$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_distribution_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$class_distribution$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_distribution_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$class_distribution$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_distribution_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$class_distribution$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_distribution_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$class_distribution$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_class_distribution_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}

class_distribution_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$class_distribution_sidebar)
  monitor_refresh = shiny::reactive(input$class_distribution_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$class_distribution$update) {
      try_plot(prefix = "Class distribution",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = class_distribution_generate,
               spawn_function = class_distribution_spawn,
               img_format = input$class_distribution_img_format,
               toggle_function = "toggle_class_distribution",
               input = input,
               output = output,
               session = session)
    }

  })

  # Generate the plot
  shiny::observeEvent(
    c(input$class_distribution_dataset,
      input$class_distribution_metacol,
      input$class_distribution_color_palette,
      input$class_distribution_title_font_size,
      input$class_distribution_y_label_font_size,
      input$class_distribution_y_tick_font_size,
      input$class_distribution_x_label_font_size,
      input$class_distribution_x_tick_font_size,
      input$class_distribution_legend_font_size,
      input$class_distribution_img_format), {

        r6$param_class_distribution(auto_refresh = input$class_distribution_auto_refresh,
                                    dataset = input$class_distribution_dataset,
                                    group_col = input$class_distribution_metacol,
                                    color_palette = input$class_distribution_color_palette,
                                    title_font_size = input$class_distribution_title_font_size,
                                    y_label_font_size = input$class_distribution_y_label_font_size,
                                    y_tick_font_size = input$class_distribution_y_tick_font_size,
                                    x_label_font_size = input$class_distribution_x_label_font_size,
                                    x_tick_font_size = input$class_distribution_x_tick_font_size,
                                    legend_font_size = input$class_distribution_legend_font_size,
                                    img_format = input$class_distribution_img_format)

        if (!input$class_distribution_auto_refresh) {
          return()
        }

        if (r6$params$class_distribution$update) {
          try_plot(prefix = "Class distribution",
                   r6 = r6,
                   dimensions_obj = dimensions_obj,
                   gen_function = class_distribution_generate,
                   spawn_function = class_distribution_spawn,
                   img_format = input$class_distribution_img_format,
                   toggle_function = "toggle_class_distribution",
                   input = input,
                   output = output,
                   session = session)

        }

  })

  # Download associated table
  output$download_class_distribution_table = shiny::downloadHandler(
    filename = function(){timestamped_name("class_distribution_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$class_distribution_table, file_name)
    }
  )

  # Expanded boxes
  class_distribution_proxy = plotly::plotlyProxy(outputId = "class_distribution_plot",
                                                 session = session)

  shiny::observeEvent(input$class_distribution_plotbox,{
    if (input$class_distribution_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#--------------------------------------------------------- Class comparison ----

class_comparison_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Class comparison: generating plot.")

  if (input$class_comparison_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_class_comparison(width = width,
                           height = height)
}
class_comparison_spawn = function(r6, format, output) {
  print_tm(r6$name, "Class comparison: spawning plot.")
  output$class_comparison_plot = plotly::renderPlotly({
    r6$plots$class_comparison
    plotly::config(r6$plots$class_comparison, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_module_name(r6, 'class_comparison'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}
class_comparison_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_comparison",
                 label = "Class comparison",
                 dimensions_obj = dimensions_obj,
                 session = session)

}
class_comparison_server = function(r6, output, session) {

  ns = session$ns

  output$class_comparison_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('class_comparison_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$class_comparison$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_comparison_dataset"),
        label = "Select table",
        choices = c('Class table', 'Class table total normalized'),
        selected = r6$params$class_comparison$dataset
      ),
      shiny::selectInput(
        inputId = ns("class_comparison_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$class_comparison$group_col
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('class_comparison_color_palette'),
        label = "Color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$class_comparison$color_palette,
        multiple = FALSE
      ),
      shiny::numericInput(
        inputId = ns("class_comparison_title_font_size"),
        label = 'Title font size',
        value = r6$params$class_comparison$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_comparison_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$class_comparison$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_comparison_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$class_comparison$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_comparison_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$class_comparison$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("class_comparison_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$class_comparison$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_comparison_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$class_comparison$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_class_comparison_table"),
        label = "Download unavailable for now",
        style = "width:100%;"
      )
    )
  })
}
class_comparison_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$class_comparison_sidebar)
  monitor_refresh = shiny::reactive(input$class_comparison_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$class_comparison$update) {
      try_plot(prefix = "Class comparison",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = class_comparison_generate,
               spawn_function = class_comparison_spawn,
               img_format = input$class_comparison_img_format,
               toggle_function = "toggle_class_comparison",
               input = input,
               output = output,
               session = session)
    }
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$class_comparison_dataset,
      input$class_comparison_metacol,
      input$class_comparison_color_palette,
      input$class_comparison_title_font_size,
      input$class_comparison_y_label_font_size,
      input$class_comparison_y_tick_font_size,
      input$class_comparison_x_tick_font_size,
      input$class_comparison_legend_font_size,
      input$class_comparison_img_format
      ),
    {

    r6$param_class_comparison(auto_refresh = input$class_comparison_auto_refresh,
                              dataset = input$class_comparison_dataset,
                              group_col = input$class_comparison_metacol,
                              color_palette = input$class_comparison_color_palette,
                              title_font_size = input$class_comparison_title_font_size,
                              y_label_font_size = input$class_comparison_y_label_font_size,
                              y_tick_font_size = input$class_comparison_y_tick_font_size,
                              x_tick_font_size = input$class_comparison_x_tick_font_size,
                              legend_font_size = input$class_comparison_legend_font_size,
                              img_format = input$class_comparison_img_format)

      if (!input$class_comparison_auto_refresh) {
        return()
      }

      if (r6$params$class_comparison$update) {
        try_plot(prefix = "Class comparison",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = class_comparison_generate,
                 spawn_function = class_comparison_spawn,
                 img_format = input$class_comparison_img_format,
                 toggle_function = "toggle_class_comparison",
                 input = input,
                 output = output,
                 session = session)
      }

  })


  # # Download associated table
  # output$download_class_comparison_table = shiny::downloadHandler(
  #   filename = function(){"class_comparison_table.csv"},
  #   content = function(file_name){
  #     write.csv(r6$tables$class_distribution_table, file_name)
  #   }
  # )


  # Expanded boxes
  class_comparison_proxy = plotly::plotlyProxy(outputId = "class_comparison_plot",
                                               session = session)

  shiny::observeEvent(input$class_comparison_plotbox,{
    if (input$class_comparison_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------------- Volcano plot ----

volcano_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Volcano plot: generating plot.")

  if (input$volcano_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_volcano_table()

  r6$plot_volcano(width = width,
                  height = height)

}

volcano_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Volcano plot: spawning plot.")
  output$volcano_plot_plot = plotly::renderPlotly({
    r6$plots$volcano_plot
    plotly::config(r6$plots$volcano_plot, toImageButtonOptions = list(format= format,
                                                                      filename= timestamped_module_name(r6, 'volcano_plot'),
                                                                      height= NULL,
                                                                      width= NULL,
                                                                      scale= 1))
  })
}

volcano_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "volcano_plot",
                 label = "Volcano plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}

volcano_plot_server = function(r6, output, session) {

  ns = session$ns

  # Set UI
  output$volcano_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('volcano_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$volcano_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("volcano_plot_data_table"),
        label = "Select data table",
        choices = r6$hardcoded_settings$volcano_plot$datasets,
        selected = r6$params$volcano_plot_comparison$data_table
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$volcano_plot_comparison$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$raw_meta[,r6$params$volcano_plot_comparison$group_col]),
        selected = c(r6$params$volcano_plot_comparison$group_1, r6$params$volcano_plot_comparison$group_2),
        multiple = TRUE
      ),

      shiny::selectizeInput(
        inputId = ns('volcano_plot_feature_metadata'),
        label = "Feature annotations",
        choices = c('None', colnames(r6$tables$raw_feat)),
        selected = r6$params$volcano_plot$feature_metadata,
        multiple = FALSE
      ),
      
      bsplus::bs_embed_tooltip(
        shiny::selectizeInput(
          inputId = ns('volcano_plot_sparse_feat'),
          label = "Sparse annotations",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        title = tooltip_data$single_omics$volcano_plot_sparse_feat,
        placement = "top"
      ),
      
      shinyWidgets::prettySwitch(
        inputId = ns('volcano_plot_keep_significant'),
        label = 'Keep only significant data',
        value = r6$params$volcano_plot$keep_significant
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_displayed_plot"),
        label = 'Displayed plot',
        choices = c('main', 'all', 'left', 'right', 'top'),
        selected = r6$params$volcano_plot$displayed_plot,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_function"),
        label = "FC function",
        choices = c("median", "mean"),
        selected = r6$params$volcano_plot_comparison$fc_function,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_statistical_test"),
        label = "Statistical test",
        choices = c("Wilcoxon", "t-Test"),
        selected = r6$params$volcano_plot_comparison$statistical_test,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_adjustment_method"),
        label = "Adjustement method",
        choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
        selected = r6$params$volcano_plot_comparison$adjustment_method,
        multiple = FALSE
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_p_val_threshold"),
        label = 'p-value threshold',
        value = r6$params$volcano_plot$p_val_threshold,
        step = 0.01,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_fc_threshold"),
        label = 'FC threshold',
        value = r6$params$volcano_plot$fc_threshold,
        step = 0.1,
        width = '100%'
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('volcano_plot_color_palette'),
        label = "Feature metadata colors",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$volcano_plot$color_palette,
        multiple = FALSE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('volcano_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$volcano_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_marker_size"),
        label = 'Marker size',
        value = r6$params$volcano_plot$marker_size,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("volcano_plot_opacity"),
        label = 'Opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$volcano_plot$opacity,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$volcano_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$volcano_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$volcano_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$volcano_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$volcano_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("volcano_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$volcano_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("volcano_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$volcano_plot$img_format,
        width = "100%"),
      shiny::actionButton(
        inputId = ns('volcano_plot_push_expression'),
        label = 'Push expression to meta',
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("download_volcano_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

volcano_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$volcano_plot_sidebar)
  monitor_refresh = shiny::reactive(input$volcano_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$volcano_plot$update) {
      try_plot(prefix = "Volcano plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = volcano_plot_generate,
               spawn_function = volcano_plot_spawn,
               img_format = input$volcano_plot_img_format,
               toggle_function = "toggle_volcano_plot",
               input = input,
               output = output,
               session = session)
    }

  })

  # auto-update selected groups
  shiny::observeEvent(input$volcano_plot_metacol,{
    shiny::updateSelectizeInput(
      inputId = "volcano_plot_metagroup",
      session = session,
      choices = unique(r6$tables$raw_meta[,input$volcano_plot_metacol]),
      selected = unique(r6$tables$raw_meta[,input$volcano_plot_metacol])[c(1,2)]
    )
  })

  shiny::observeEvent(input$volcano_plot_feature_metadata, {
    if (input$volcano_plot_feature_metadata %in% names(r6$tables$sparse_feat)) {
      shiny::updateSelectizeInput(
        inputId = "volcano_plot_sparse_feat",
        session = session,
        choices = r6$tables$sparse_feat[[input$volcano_plot_feature_metadata]]$terms_list,
        selected = character(0)
      )
    } else {
      shiny::updateSelectizeInput(
        inputId = "volcano_plot_sparse_feat",
        session = session,
        choices = NULL,
        selected = character(0)
      )
    }
  })


  shiny::observeEvent(
    c(shiny::req(length(input$volcano_plot_metagroup) == 2),
      input$volcano_plot_data_table,
      input$volcano_plot_metagroup,
      input$volcano_plot_feature_metadata,
      input$volcano_plot_sparse_feat,
      input$volcano_plot_keep_significant,
      input$volcano_plot_displayed_plot,
      input$volcano_plot_function,
      input$volcano_plot_statistical_test,
      input$volcano_plot_adjustment_method,
      input$volcano_plot_p_val_threshold,
      input$volcano_plot_fc_threshold,
      input$volcano_plot_color_palette,
      input$volcano_plot_reverse_palette,
      input$volcano_plot_marker_size,
      input$volcano_plot_opacity,
      input$volcano_plot_title_font_size,
      input$volcano_plot_y_label_font_size,
      input$volcano_plot_y_tick_font_size,
      input$volcano_plot_x_label_font_size,
      input$volcano_plot_x_tick_font_size,
      input$volcano_plot_legend_font_size,
      input$volcano_plot_img_format
    ),{

      # Is the column multivalue?
      if (input$volcano_plot_feature_metadata %in% names(r6$tables$sparse_feat)) {
        if (length(input$volcano_plot_sparse_feat) > 0) {
          feature_metadata = match_go_terms(terms_list = input$volcano_plot_sparse_feat,
                                            sparse_table = r6$tables$sparse_feat[[input$volcano_plot_feature_metadata]]$sparse_matrix)
        } else {
          return()
        }
      } else {
        feature_metadata = input$volcano_plot_feature_metadata
      }

      r6$param_volcano_plot_comparison(data_table = input$volcano_plot_data_table,
                                       group_col = input$volcano_plot_metacol,
                                       group_1 = input$volcano_plot_metagroup[1],
                                       group_2 = input$volcano_plot_metagroup[2],
                                       fc_function = input$volcano_plot_function,
                                       statistical_test = input$volcano_plot_statistical_test,
                                       adjustment_method = input$volcano_plot_adjustment_method)

      r6$param_volcano_plot(auto_refresh = input$volcano_plot_auto_refresh,
                            feature_metadata = feature_metadata,
                            keep_significant = input$volcano_plot_keep_significant,
                            displayed_plot = input$volcano_plot_displayed_plot,
                            p_val_threshold = input$volcano_plot_p_val_threshold,
                            fc_threshold = input$volcano_plot_fc_threshold,
                            marker_size = input$volcano_plot_marker_size,
                            opacity = input$volcano_plot_opacity,
                            title_font_size = input$volcano_plot_title_font_size,
                            y_label_font_size = input$volcano_plot_y_label_font_size,
                            y_tick_font_size = input$volcano_plot_y_tick_font_size,
                            x_label_font_size = input$volcano_plot_x_label_font_size,
                            x_tick_font_size = input$volcano_plot_x_tick_font_size,
                            legend_font_size = input$volcano_plot_legend_font_size,
                            color_palette = input$volcano_plot_color_palette,
                            reverse_palette = input$volcano_plot_reverse_palette,
                            img_format = input$volcano_plot_img_format)

      if (!input$volcano_plot_auto_refresh) {
        r6$params$volcano_plot$auto_refresh = input$volcano_plot_auto_refresh
        return()
      }

      if (r6$params$volcano_plot$update) {
        try_plot(prefix = "Volcano plot",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = volcano_plot_generate,
                 spawn_function = volcano_plot_spawn,
                 img_format = input$volcano_plot_img_format,
                 toggle_function = "toggle_volcano_plot",
                 input = input,
                 output = output,
                 session = session)
      }


    })

  # Push expression to meta
  shiny::observeEvent(input$volcano_plot_push_expression,{
    r6$push_volcano_to_meta()
    print_tm(r6$name, "Volcano plot: pushed expression to the metadata")

  })

  # Export volcano table
  output$download_volcano_table = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_plot.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_plot, file_name)
    }
  )

  output$download_volcano_subtable = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table_selection.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table_slice, file_name)
    }
  )

  # Expanded boxes
  volcano_plot_proxy = plotly::plotlyProxy(outputId = "volcano_plot_plot",
                                           session = session)

  shiny::observeEvent(input$volcano_plot_plotbox,{
    if (input$volcano_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#------------------------------------------------------------------ Heatmap ----
heatmap_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Heatmap: generating plot.")

  if (input$heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_heatmap(width = width,
                  height = height)
}

heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "Heatmap: spawning plot.")
  output$heatmap_plot = plotly::renderPlotly({
    r6$plots$heatmap
    plotly::config(r6$plots$heatmap, toImageButtonOptions = list(format= format,
                                                                 filename= timestamped_module_name(r6, 'heatmap'),
                                                                 height= NULL,
                                                                 width= NULL,
                                                                 scale= 1))
  })
}


heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "heatmap",
                 label = "Heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


heatmap_server = function(r6, output, session) {

  ns = session$ns

  output$heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('heatmap_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$heatmap$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("heatmap_dataset"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$heatmap$datasets,
        selected = r6$params$heatmap$dataset
      ),
      shiny::selectizeInput(
        inputId = ns("heatmap_map_rows"),
        label = "Map sample data",
        multiple = TRUE,
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$heatmap$map_sample_data
      ),
      shiny::selectizeInput(
        inputId = ns("heatmap_map_cols"),
        label = "Map feature data",
        multiple = TRUE,
        choices = colnames(r6$tables$raw_feat)[!(colnames(r6$tables$raw_feat) %in% names(r6$tables$sparse_feat))],
        selected = r6$params$heatmap$map_feature_data
      ),
      bsplus::bs_embed_tooltip(
        shiny::selectizeInput(
          inputId = ns("heatmap_sparse_table"),
          label = "Sparse table",
          multiple = F,
          choices = c('None', names(r6$tables$sparse_feat)),
          selected = r6$params$heatmap$sparse_table
        ),
        title = tooltip_data$single_omics$heatmap_sparse_table,
        placement = "top"
      ),
      bsplus::bs_embed_tooltip(
        shiny::selectizeInput(
          inputId = ns("heatmap_sparse_feat"),
          label = "Sparse annotations",
          multiple = T,
          choices = NULL,
          selected = r6$params$heatmap$sparse_features
        ),
        title = tooltip_data$single_omics$heatmap_sparse_feat,
        placement = "top"
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),

      shiny::selectInput(
        inputId = ns("heatmap_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$heatmap$distance_method,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns("heatmap_clustering_method"),
        label = "Clustering method",
        choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
        selected = r6$params$heatmap$clustering_method,
        multiple = F
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('heatmap_center'),
        label = 'Center scale to 0',
        value = r6$params$heatmap$center,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('heatmap_impute_median'),
        label = 'Impute to median',
        value = r6$params$heatmap$impute_median,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('heatmap_apply_clustering'),
        label = 'Apply clustering',
        value = r6$params$heatmap$apply_clustering,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('heatmap_k_clusters_samples'),
        label = 'K clusters samples',
        value = r6$params$heatmap$k_clusters_samples,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('heatmap_k_clusters_features'),
        label = 'K clusters features',
        value = r6$params$heatmap$k_clusters_features,
        min = 1,
        max = NA,
        step = 1,
        width = '100%'
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h3("Discriminant analysis")
        ),
        shiny::column(
          width = 6,

          shinyWidgets::materialSwitch(
            inputId = ns('heatmap_apply_da'),
            label = 'Apply',
            value = r6$params$heatmap$apply_da,
            right = TRUE,
            status = "primary"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectizeInput(inputId = ns("heatmap_group_col_da"),
                                label = "Group column",
                                choices = colnames(r6$tables$raw_meta),
                                selected = r6$params$heatmap$group_column_da,
                                multiple = FALSE,
                                width = "100%")
        ),
        shiny::column(
          width = 6,
          shiny::sliderInput(inputId = ns("heatmap_alpha_da"),
                             label = "Alpha",
                             min = 0,
                             max = 0.99,
                             value = r6$params$heatmap$alpha_da,
                             step = 0.01,
                             width = "100%")
        )
      ),

      shiny::numericInput(
        inputId = ns("heatmap_seed_da"),
        label = 'DA seed',
        value = r6$params$heatmap$seed_da,
        step= 1,
        width = '100%'
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('heatmap_colors_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$heatmap$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('heatmap_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$heatmap$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("heatmap_title_font_size"),
        label = 'Title font size',
        value = r6$params$heatmap$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("heatmap_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$heatmap$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("heatmap_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$heatmap$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("heatmap_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$heatmap$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("heatmap_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$heatmap$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("heatmap_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$heatmap$img_format,
        width = "100%"),
      shiny::actionButton(
        inputId = ns('heatmap_push_sample_clusters'),
        label = 'Push sample clusters to meta',
        width = '100%'
      ),
      shiny::actionButton(
        inputId = ns('heatmap_push_feature_clusters'),
        label = 'Push feature clusters to meta',
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("download_heatmap_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$heatmap_sidebar)
  monitor_refresh = shiny::reactive(input$heatmap_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$heatmap$update) {
      try_plot(prefix = "Heatmap",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = heatmap_generate,
               spawn_function = heatmap_spawn,
               img_format = input$heatmap_img_format,
               toggle_function = "toggle_heatmap",
               input = input,
               output = output,
               session = session)
    }
  })

  shiny::observeEvent(input$heatmap_sparse_table, {
    if (input$heatmap_sparse_table %in% names(r6$tables$sparse_feat)) {
      shiny::updateSelectizeInput(
        inputId = "heatmap_sparse_feat",
        session = session,
        choices = r6$tables$sparse_feat[[input$heatmap_sparse_table]]$terms_list,
        selected = character(0)
      )
    } else {
      shiny::updateSelectizeInput(
        inputId = "heatmap_sparse_feat",
        session = session,
        choices = NULL,
        selected = character(0)
      )
    }
  })

  shiny::observeEvent(
    c(input$heatmap_distance_method,
      input$heatmap_clustering_method,
      input$heatmap_dataset,
      input$heatmap_center,
      input$heatmap_impute_median,
      input$heatmap_apply_clustering,
      input$heatmap_k_clusters_samples,
      input$heatmap_k_clusters_features,
      input$heatmap_map_rows,
      input$heatmap_map_cols,
      input$heatmap_sparse_feat,
      input$heatmap_group_col_da,
      input$heatmap_apply_da,
      input$heatmap_alpha_da,
      input$heatmap_seed_da,
      input$heatmap_colors_palette,
      input$heatmap_reverse_palette,
      input$heatmap_title_font_size,
      input$heatmap_y_label_font_size,
      input$heatmap_y_tick_font_size,
      input$heatmap_x_label_font_size,
      input$heatmap_x_tick_font_size,
      input$heatmap_img_format
    ),{

      # Feature annotation terms
      if (length(input$heatmap_sparse_feat) > 0) {
        sparse_features = list()
        sparse_features[[input$heatmap_sparse_table]] = match_go_terms(terms_list = input$heatmap_sparse_feat,
                                                                          sparse_table = r6$tables$sparse_feat[[input$heatmap_sparse_table]]$sparse_matrix)
      } else {
        sparse_features = NULL
      }

      r6$param_heatmap(auto_refresh = input$heatmap_auto_refresh,
                       dataset = input$heatmap_dataset,
                       distance_method = input$heatmap_distance_method,
                       clustering_method = input$heatmap_clustering_method,
                       impute_median = input$heatmap_impute_median,
                       center = input$heatmap_center,
                       apply_clustering = input$heatmap_apply_clustering,
                       k_clusters_samples = input$heatmap_k_clusters_samples,
                       k_clusters_features = input$heatmap_k_clusters_features,
                       map_sample_data = input$heatmap_map_rows,
                       map_feature_data = input$heatmap_map_cols,
                       sparse_features = sparse_features,
                       sparse_table = input$heatmap_sparse_table,
                       group_column_da = input$heatmap_group_col_da,
                       apply_da = input$heatmap_apply_da,
                       alpha_da = input$heatmap_alpha_da,
                       seed_da = input$heatmap_seed_da,
                       color_palette = input$heatmap_colors_palette,
                       reverse_palette = input$heatmap_reverse_palette,
                       title_font_size = input$heatmap_title_font_size,
                       y_label_font_size = input$heatmap_y_label_font_size,
                       y_tick_font_size = input$heatmap_y_tick_font_size,
                       x_label_font_size = input$heatmap_x_label_font_size,
                       x_tick_font_size = input$heatmap_x_tick_font_size,
                       img_format = input$heatmap_img_format)

      if (!input$heatmap_auto_refresh) {
        r6$params$heatmap$auto_refresh = input$heatmap_auto_refresh
        return()
      }

      if (r6$params$heatmap$update) {
        try_plot(prefix = "Heatmap",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = heatmap_generate,
                 spawn_function = heatmap_spawn,
                 img_format = input$heatmap_img_format,
                 toggle_function = "toggle_heatmap",
                 input = input,
                 output = output,
                 session = session)
      }

    }
  )

  # Push k clusters features to meta
  shiny::observeEvent(input$heatmap_push_sample_clusters,{
    if ('k_clusters_heatmap' %in% colnames(r6$tables$heatmap$sample_clusters)){
      r6$tables$raw_meta$k_clusters_heatmap = "None"
      r6$tables$raw_meta[rownames(r6$tables$heatmap$sample_clusters), 'k_clusters_heatmap'] = r6$tables$heatmap$sample_clusters$k_clusters_heatmap
      print_tm(r6$name, "Heatmap: pushed feature clusters to the metadata")
    } else {
      print_tm(r6$name, "Heatmap: no clusters to be pushed")
    }
  })

  # Push k clusters features to meta
  shiny::observeEvent(input$heatmap_push_feature_clusters,{
    if ('k_clusters_heatmap' %in% colnames(r6$tables$heatmap$feature_clusters)){
      r6$tables$raw_feat$k_clusters_heatmap = "None"
      r6$tables$raw_feat[rownames(r6$tables$heatmap$feature_clusters), 'k_clusters_heatmap'] = r6$tables$heatmap$feature_clusters$k_clusters_heatmap
      print_tm(r6$name, "Heatmap: pushed feature clusters to the metadata")
    } else {
      print_tm(r6$name, "Heatmap: no clusters to be pushed")
    }
  })


  # Download associated table
  output$download_heatmap_table = shiny::downloadHandler(
    filename = function(){timestamped_name("heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$heatmap$data_table, file_name)
    }
  )

  # Expanded boxes
  heatmap_proxy = plotly::plotlyProxy(outputId = "heatmap_plot",
                                      session = session)

  shiny::observeEvent(input$heatmap_plotbox,{
    if (input$heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#------------------------------------------------------ Samples correlation ----
samples_correlation_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Samples correlation: generating plot.")

  if (input$samples_correlation_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_samples_correlation(width = width,
                              height = height)
}

samples_correlation_spawn = function(r6, format, output) {
  print_tm(r6$name, "Samples correlation: spawning plot.")
  output$samples_correlation_plot = plotly::renderPlotly({
    r6$plots$samples_correlation
    plotly::config(r6$plots$samples_correlation, toImageButtonOptions = list(format= format,
                                                                             filename= timestamped_module_name(r6, 'samples_correlation'),
                                                                             height= NULL,
                                                                             width= NULL,
                                                                             scale= 1))
  })
}


samples_correlation_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "samples_correlation",
                 label = "Samples correlation",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


samples_correlation_server = function(r6, output, session) {

  ns = session$ns

  output$samples_correlation_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('samples_correlation_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$samples_correlation$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("samples_correlation_dataset"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$samples_correlation$datasets,
        selected = r6$params$samples_correlation$dataset
      ),
      shiny::selectizeInput(
        inputId = ns("samples_correlation_map_rows"),
        label = "Map data on rows",
        multiple = TRUE,
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$samples_correlation$row_annotations
      ),
      shiny::selectizeInput(
        inputId = ns("samples_correlation_map_cols"),
        label = "Map data on cols",
        multiple = TRUE,
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$samples_correlation$col_annotations
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("samples_correlation_correlation_method"),
        label = "Correlation method",
        choices = c("pearson", "spearman"),
        selected = r6$params$samples_correlation$correlation_method
      ),
      shiny::selectInput(
        inputId = ns("samples_correlation_use"),
        label = "Data completeness",
        choices = c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"),
        selected = r6$params$samples_correlation$use
      ),


      shiny::selectInput(
        inputId = ns("samples_correlation_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$samples_correlation$distance_method,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns("samples_correlation_clustering_method"),
        label = "Clustering method",
        choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
        selected = r6$params$samples_correlation$clustering_method,
        multiple = F
      ),

      shiny::numericInput(
        inputId = ns('samples_correlation_k_clusters'),
        label = "K clusters",
        value = r6$params$samples_correlation$k_clusters,
        step = 1,
        width = '100%'
      ),

      shinyWidgets::materialSwitch(
        inputId = ns('samples_correlation_apply_clustering'),
        label = 'Apply clustering',
        value = r6$params$samples_correlation$apply_clustering,
        right = TRUE,
        status = "primary"
      ),

      shinyWidgets::materialSwitch(
        inputId = ns('samples_correlation_center'),
        label = 'Center',
        value = r6$params$samples_correlation$center,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('samples_correlation_colors_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$samples_correlation$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('samples_correlation_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$samples_correlation$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("samples_correlation_title_font_size"),
        label = 'Title font size',
        value = r6$params$samples_correlation$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("samples_correlation_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$samples_correlation$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("samples_correlation_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$samples_correlation$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("samples_correlation_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$samples_correlation$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("samples_correlation_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$samples_correlation$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("samples_correlation_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$samples_correlation$img_format,
        width = "100%"),
      shiny::actionButton(
        inputId = ns('samples_correlation_push_clusters'),
        label = 'Push clusters to meta',
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("download_samples_correlation_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

samples_correlation_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$samples_correlation_sidebar)
  monitor_refresh = shiny::reactive(input$samples_correlation_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$samples_correlation$update) {
      try_plot(prefix = "Samples correlation",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = samples_correlation_generate,
               spawn_function = samples_correlation_spawn,
               img_format = input$samples_correlation_img_format,
               toggle_function = "toggle_samples_correlation",
               input = input,
               output = output,
               session = session)
    }
  })

  shiny::observeEvent(
    c(input$samples_correlation_dataset,
      input$samples_correlation_correlation_method,
      input$samples_correlation_use,
      input$samples_correlation_distance_method,
      input$samples_correlation_clustering_method,
      input$samples_correlation_k_clusters,
      input$samples_correlation_apply_clustering,
      input$samples_correlation_center,
      input$samples_correlation_map_rows,
      input$samples_correlation_map_cols,
      input$samples_correlation_colors_palette,
      input$samples_correlation_reverse_palette,
      input$samples_correlation_title_font_size,
      input$samples_correlation_y_label_font_size,
      input$samples_correlation_y_tick_font_size,
      input$samples_correlation_x_label_font_size,
      input$samples_correlation_x_tick_font_size,
      input$samples_correlation_legend_font_size,
      input$samples_correlation_img_format
    ),{

      r6$param_samples_correlation(auto_refresh = input$samples_correlation_auto_refresh,
                                   dataset = input$samples_correlation_dataset,
                                   correlation_method = input$samples_correlation_correlation_method,
                                   use = input$samples_correlation_use,
                                   distance_method = input$samples_correlation_distance_method,
                                   clustering_method = input$samples_correlation_clustering_method,
                                   k_clusters = input$samples_correlation_k_clusters,
                                   apply_clustering = input$samples_correlation_apply_clustering,
                                   center = input$samples_correlation_center,
                                   row_annotations = input$samples_correlation_map_rows,
                                   col_annotations = input$samples_correlation_map_cols,
                                   color_palette = input$samples_correlation_colors_palette,
                                   reverse_palette = input$samples_correlation_reverse_palette,
                                   title_font_size = input$samples_correlation_title_font_size,
                                   y_label_font_size = input$samples_correlation_y_label_font_size,
                                   y_tick_font_size = input$samples_correlation_y_tick_font_size,
                                   x_label_font_size = input$samples_correlation_x_label_font_size,
                                   x_tick_font_size = input$samples_correlation_x_tick_font_size,
                                   img_format = input$samples_correlation_img_format)

      if (!input$samples_correlation_auto_refresh) {
        r6$params$samples_correlation$auto_refresh = input$samples_correlation_auto_refresh
        return()
      }

      if (r6$params$samples_correlation$update) {
        try_plot(prefix = "Samples correlation",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = samples_correlation_generate,
                 spawn_function = samples_correlation_spawn,
                 img_format = input$samples_correlation_img_format,
                 toggle_function = "toggle_samples_correlation",
                 input = input,
                 output = output,
                 session = session)
      }

    })

  # Push k clusters to meta
  shiny::observeEvent(input$samples_correlation_push_clusters,{
    if ('k_clusters' %in% colnames(r6$tables$samples_correlation_clusters)){
      k_clusters = r6$tables$samples_correlation_clusters[rownames(r6$tables$raw_meta), 'k_clusters']
      r6$tables$raw_meta$correlation_clusters = k_clusters
      print_tm(r6$name, "Samples correlation: pushed clusters to the metadata")
    } else {
      print_tm(r6$name, "Samples correlation: no clusters to be pushed")
    }
  })

  # Download associated table
  output$download_samples_correlation_table = shiny::downloadHandler(
    filename = function(){timestamped_name("samples_correlation_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$samples_correlation_table, file_name)
    }
  )

  # Expanded boxes
  samples_correlation_proxy = plotly::plotlyProxy(outputId = "samples_correlation_plot",
                                                  session = session)

  shiny::observeEvent(input$samples_correlation_plotbox,{
    if (input$samples_correlation_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = samples_correlation_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = samples_correlation_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#------------------------------------------------------ Feature correlation ----
feature_correlation_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Feature correlation: generating plot.")

  if (input$feature_correlation_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_feature_correlation(width = width,
                              height = height)
}

feature_correlation_spawn = function(r6, format, output) {
  print_tm(r6$name, "Feature correlation: spawning plot.")
  output$feature_correlation_plot = plotly::renderPlotly({
    r6$plots$feature_correlation
    plotly::config(r6$plots$feature_correlation, toImageButtonOptions = list(format= format,
                                                                             filename= timestamped_module_name(r6, 'feature_correlation'),
                                                                             height= NULL,
                                                                             width= NULL,
                                                                             scale= 1))
  })
}


feature_correlation_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "feature_correlation",
                 label = "Feature correlation",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


feature_correlation_server = function(r6, output, session) {

  ns = session$ns

  output$feature_correlation_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('feature_correlation_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$feature_correlation$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      shiny::selectInput(
        inputId = ns("feature_correlation_dataset"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$feature_correlation$datasets,
        selected = r6$params$feature_correlation$dataset
      ),
      shiny::selectInput(
        inputId = ns("feature_correlation_correlation_method"),
        label = "Correlation method",
        choices = c("pearson", "spearman"),
        selected = r6$params$feature_correlation$correlation_method
      ),
      shiny::selectInput(
        inputId = ns("feature_correlation_use"),
        label = "Data completeness",
        choices = c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"),
        selected = r6$params$feature_correlation$use
      ),

      shiny::selectInput(
        inputId = ns("feature_correlation_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$feature_correlation$distance_method,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns("feature_correlation_clustering_method"),
        label = "Clustering method",
        choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
        selected = r6$params$feature_correlation$clustering_method,
        multiple = F
      ),

      shiny::numericInput(
        inputId = ns('feature_correlation_k_clusters'),
        label = "K clusters",
        value = r6$params$feature_correlation$k_clusters,
        step = 1,
        width = '100%'
      ),

      shinyWidgets::materialSwitch(
        inputId = ns('feature_correlation_apply_clustering'),
        label = 'Apply clustering',
        value = r6$params$feature_correlation$apply_clustering,
        right = TRUE,
        status = "primary"
      ),

      shinyWidgets::materialSwitch(
        inputId = ns('feature_correlation_center'),
        label = 'Center',
        value = r6$params$feature_correlation$center,
        right = TRUE,
        status = "primary"
      ),

      shiny::selectizeInput(
        inputId = ns("feature_correlation_map_rows"),
        label = "Map data on rows",
        multiple = TRUE,
        choices = colnames(r6$tables$raw_feat)[!(colnames(r6$tables$raw_feat) %in% names(r6$tables$sparse_feat))],
        selected = r6$params$feature_correlation$row_annotations
      ),
      shiny::selectizeInput(
        inputId = ns("feature_correlation_map_cols"),
        label = "Map data on cols",
        multiple = TRUE,
        choices = colnames(r6$tables$raw_feat)[!(colnames(r6$tables$raw_feat) %in% names(r6$tables$sparse_feat))],
        selected = r6$params$feature_correlation$col_annotations
      ),
      bsplus::bs_embed_tooltip(
        shiny::selectizeInput(
          inputId = ns("feature_correlation_sparse_table"),
          label = "Sparse table",
          multiple = F,
          choices = c('None', names(r6$tables$sparse_feat)),
          selected = r6$params$feature_correlation$sparse_table
        ),
        title = tooltip_data$single_omics$feature_correlation_sparse_table,
        placement = "top"
      ),
      bsplus::bs_embed_tooltip(
        shiny::selectizeInput(
          inputId = ns("feature_correlation_sparse_features"),
          label = "Sparse annotations",
          multiple = T,
          choices = NULL,
          selected = r6$params$feature_correlation$sparse_features
        ),
        title = tooltip_data$single_omics$feature_correlation_sparse_feat,
        placement = "top"
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::h3("Filter features"),
      shiny::sliderInput(inputId = ns("feature_correlation_roh_threshold"),
                         label = "Roh threshold",
                         min = 0,
                         max = 0.99,
                         value = r6$params$feature_correlation$roh_threshold,
                         step = 0.01,
                         width = "100%"),
      shiny::sliderInput(inputId = ns("feature_correlation_top_features"),
                         label = "Top features",
                         min = 50,
                         max = 1000,
                         value = r6$params$feature_correlation$top_features,
                         step = 1,
                         width = "100%"),
      shiny::selectInput(
        inputId = ns('feature_correlation_colors_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$feature_correlation$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('feature_correlation_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$feature_correlation$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("feature_correlation_title_font_size"),
        label = 'Title font size',
        value = r6$params$feature_correlation$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_correlation_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$feature_correlation$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_correlation_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$feature_correlation$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_correlation_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$feature_correlation$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_correlation_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$feature_correlation$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("feature_correlation_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$feature_correlation$img_format,
        width = "100%"),
      shiny::actionButton(
        inputId = ns('feature_correlation_push_clusters'),
        label = 'Push clusters to meta',
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("download_feature_correlation_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

feature_correlation_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$feature_correlation_sidebar)
  monitor_refresh = shiny::reactive(input$feature_correlation_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$feature_correlation$update) {
      try_plot(prefix = "Feature correlation",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = feature_correlation_generate,
               spawn_function = feature_correlation_spawn,
               img_format = input$feature_correlation_img_format,
               toggle_function = "toggle_feature_correlation",
               input = input,
               output = output,
               session = session)
    }
  })

  shiny::observeEvent(input$feature_correlation_sparse_table, {
    if (input$feature_correlation_sparse_table %in% names(r6$tables$sparse_feat)) {
      shiny::updateSelectizeInput(
        inputId = "feature_correlation_sparse_features",
        session = session,
        choices = r6$tables$sparse_feat[[input$feature_correlation_sparse_table]]$terms_list,
        selected = character(0)
      )
    } else {
      shiny::updateSelectizeInput(
        inputId = "feature_correlation_sparse_features",
        session = session,
        choices = NULL,
        selected = character(0)
      )
    }
  })

  shiny::observeEvent(
    c(input$feature_correlation_dataset,
      input$feature_correlation_correlation_method,
      input$feature_correlation_use,
      input$feature_correlation_distance_method,
      input$feature_correlation_clustering_method,
      input$feature_correlation_k_clusters,
      input$feature_correlation_apply_clustering,
      input$feature_correlation_center,
      input$feature_correlation_map_rows,
      input$feature_correlation_map_cols,
      input$feature_correlation_sparse_features,
      input$feature_correlation_roh_threshold,
      input$feature_correlation_top_features,
      input$feature_correlation_colors_palette,
      input$feature_correlation_reverse_palette,
      input$feature_correlation_title_font_size,
      input$feature_correlation_y_label_font_size,
      input$feature_correlation_y_tick_font_size,
      input$feature_correlation_x_label_font_size,
      input$feature_correlation_x_tick_font_size,
      input$feature_correlation_img_format
    ),{

      # Feature annotation terms
      if (length(input$feature_correlation_sparse_features) > 0) {
        sparse_features = list()
        sparse_features[[input$feature_correlation_sparse_table]] = match_go_terms(terms_list = input$feature_correlation_sparse_features,
                                                                                      sparse_table = r6$tables$sparse_feat[[input$feature_correlation_sparse_table]]$sparse_matrix)
      } else {
        sparse_features = NULL
      }

      r6$param_feature_correlation(auto_refresh = input$feature_correlation_auto_refresh,
                                   dataset = input$feature_correlation_dataset,
                                   sparse_table = input$feature_correlation_sparse_table,
                                   sparse_features = sparse_features,
                                   correlation_method = input$feature_correlation_correlation_method,
                                   use = input$feature_correlation_use,
                                   distance_method = input$feature_correlation_distance_method,
                                   clustering_method = input$feature_correlation_clustering_method,
                                   k_clusters = input$feature_correlation_k_clusters,
                                   apply_clustering = input$feature_correlation_apply_clustering,
                                   center = input$feature_correlation_center,
                                   row_annotations = input$feature_correlation_map_rows,
                                   col_annotations = input$feature_correlation_map_cols,
                                   roh_threshold = input$feature_correlation_roh_threshold,
                                   top_features = input$feature_correlation_top_features,
                                   color_palette = input$feature_correlation_colors_palette,
                                   reverse_palette = input$feature_correlation_reverse_palette,
                                   title_font_size = input$feature_correlation_title_font_size,
                                   y_label_font_size = input$feature_correlation_y_label_font_size,
                                   y_tick_font_size = input$feature_correlation_y_tick_font_size,
                                   x_label_font_size = input$feature_correlation_x_label_font_size,
                                   x_tick_font_size = input$feature_correlation_x_tick_font_size,
                                   img_format = input$feature_correlation_img_format)

      if (!input$feature_correlation_auto_refresh) {
        r6$params$feature_correlation$auto_refresh = input$feature_correlation_auto_refresh
        return()
      }

      if (r6$params$feature_correlation$update) {
        try_plot(prefix = "Feature correlation",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = feature_correlation_generate,
                 spawn_function = feature_correlation_spawn,
                 img_format = input$feature_correlation_img_format,
                 toggle_function = "toggle_feature_correlation",
                 input = input,
                 output = output,
                 session = session)
      }

    })

  # Push k clusters to meta
  shiny::observeEvent(input$feature_correlation_push_clusters,{
    if ('k_clusters' %in% colnames(r6$tables$feature_correlation_clusters)){
      r6$tables$raw_feat$correlation_clusters = "None"
      r6$tables$raw_feat[rownames(r6$tables$feature_correlation_clusters), 'correlation_clusters'] = r6$tables$feature_correlation_clusters$k_clusters
      print_tm(r6$name, "Feature correlation: pushed clusters to the metadata")
    } else {
      print_tm(r6$name, "Feature correlation: no clusters to be pushed")
    }
  })


  # Download associated table
  output$download_feature_correlation_table = shiny::downloadHandler(
    filename = function(){timestamped_name("feature_correlation_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$feature_correlation_table, file_name)
    }
  )

  # Expanded boxes
  feature_correlation_proxy = plotly::plotlyProxy(outputId = "feature_correlation_plot",
                                                  session = session)

  shiny::observeEvent(input$feature_correlation_plotbox,{
    if (input$feature_correlation_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = feature_correlation_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = feature_correlation_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#---------------------------------------------------------------------- PCA ----

pca_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "PCA: generating plot.")

  if (input$pca_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }


  r6$plot_pca(width = width,
              height = height)
}

pca_spawn = function(r6, format, output) {
  print_tm(r6$name, "PCA: spawning plot.")
  output$pca_plot = plotly::renderPlotly({
    r6$plots$pca_plot
    plotly::config(r6$plots$pca_plot, toImageButtonOptions = list(format= format,
                                                                  filename= timestamped_module_name(r6, 'pca_plot'),
                                                                  height= NULL,
                                                                  width= NULL,
                                                                  scale= 1))
  })
}

pca_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "pca",
                 label = "PCA",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


pca_server = function(r6, output, session) {

  ns = session$ns

  output$pca_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('pca_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$pca$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("pca_data_table"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$pca$datasets,
        selected = r6$params$pca$data_table
      ),
      shiny::selectInput(
        inputId = ns("pca_sample_groups_col"),
        label = "Sample group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$pca$sample_groups_col
      ),
      shiny::selectInput(
        inputId = ns("pca_feature_group"),
        label = "Feature metadata",
        choices = c('None', colnames(r6$tables$raw_feat)),
        selected = r6$params$pca$feature_groups_col
      ),
      bsplus::bs_embed_tooltip(
        shiny::selectizeInput(
          inputId = ns('pca_sparse_features'),
          label = "Sparse annotations",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        title = tooltip_data$single_omics$pca_sparse_feat,
        placement = "top"
      ),
      shiny::selectInput(
        inputId = ns('pca_displayed_plots'),
        label = 'Displayed plot',
        choices = c('both', 'scores', 'loadings', 'variance'),
        selected = r6$params$pca$displayed_plots,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('pca_impute_median'),
        label = 'Impute to median',
        value = r6$params$pca$impute_median,
        right = TRUE,
        status = "primary"
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h4("Discriminant analysis")
        ),
        shiny::column(
          width = 6,
          shinyWidgets::materialSwitch(
            inputId = ns('pca_apply_da'),
            label = 'Apply',
            value = r6$params$pca$apply_da,
            right = TRUE,
            status = "primary")
        )
      ),
      shiny::selectInput(
        inputId = ns("pca_sample_groups_da"),
        label = "DA sample group",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$pca$sample_groups_da
      ),
      shiny::sliderInput(inputId = ns("pca_alpha_da"),
                         label = "Alpha",
                         min = 0,
                         max = 0.99,
                         value = r6$params$pca$alpha_da,
                         step = 0.01,
                         width = "100%"),

      shiny::numericInput(
        inputId = ns("pca_seed_da"),
        label = 'DA seed',
        value = r6$params$pca$seed_da,
        step= 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('pca_method'),
        label = 'PCA method',
        choices = c('svd', 'nipals', 'rnipals', 'bpca', 'ppca', 'svdImpute', 'llsImputeAll'),
        selected = r6$params$pca$pca_method,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('pca_npcs'),
        label = 'Number of PCs',
        value = r6$params$pca$nPcs,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('pca_displayed_pc_1'),
        label = 'Displayed PC (1)',
        value = r6$params$pca$displayed_pc_1,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('pca_displayed_pc_2'),
        label = 'Displayed PC (2)',
        value = r6$params$pca$displayed_pc_2,
        step = 1,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns('pca_completeObs'),
        label = 'Complete observations',
        value = r6$params$pca$completeObs
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('pca_colors_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'),
        selected = r6$params$pca$colors_palette,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("pca_marker_size"),
        label = 'Marker size',
        value = r6$params$pca$marker_size,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("pca_opacity"),
        label = 'Opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$pca$opacity,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("pca_title_font_size"),
        label = 'Title font size',
        value = r6$params$pca$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("pca_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$pca$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("pca_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$pca$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("pca_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$pca$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("pca_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$pca$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("pca_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$pca$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::selectInput(
          inputId = ns("pca_img_format"),
          label = "Image format",
          choices = c("png", "svg", "jpeg", "webp"),
          selected = r6$params$pca$img_format,
          width = "100%"),
        shiny::downloadButton(
          outputId = ns("download_pca_scores_table"),
          label = "Download scores table",
          style = "width:50%;"
        ),
        shiny::downloadButton(
          outputId = ns("download_pca_loadings_table"),
          label = "Download loadings table",
          style = "width:50%;"
        )
      )

    )
  })
}

pca_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$pca_sidebar)
  monitor_refresh = shiny::reactive(input$pca_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$pca$update) {
      try_plot(prefix = "PCA",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = pca_generate,
               spawn_function = pca_spawn,
               img_format = input$pca_img_format,
               toggle_function = "toggle_pca",
               input = input,
               output = output,
               session = session)
    }
  })

  shiny::observeEvent(input$pca_feature_group, {
    if (input$pca_feature_group %in% names(r6$tables$sparse_feat)) {
      shiny::updateSelectizeInput(
        inputId = "pca_sparse_features",
        session = session,
        choices = r6$tables$sparse_feat[[input$pca_feature_group]]$terms_list,
        selected = character(0)
      )
    } else {
      shiny::updateSelectizeInput(
        inputId = "pca_sparse_features",
        session = session,
        choices = NULL,
        selected = character(0)
      )
    }
  })

  shiny::observeEvent(c(
    input$pca_data_table,
    input$pca_sample_groups_col,
    input$pca_feature_group,
    input$pca_sparse_features,
    input$pca_impute_median,
    input$pca_apply_da,
    input$pca_sample_groups_da,
    input$pca_alpha_da,
    input$pca_seed_da,
    input$pca_method,
    input$pca_npcs,
    input$pca_displayed_pc_1,
    input$pca_displayed_pc_2,
    input$pca_completeObs,
    input$pca_displayed_plots,
    input$pca_colors_palette,
    input$pca_marker_size,
    input$pca_opacity,
    input$pca_title_font_size,
    input$pca_y_label_font_size,
    input$pca_y_tick_font_size,
    input$pca_x_label_font_size,
    input$pca_x_tick_font_size,
    input$pca_legend_font_size,
    input$pca_img_format),{

      # Is the column multivalue?
      if (input$pca_feature_group %in% names(r6$tables$sparse_feat)) {
        if (length(input$pca_sparse_features) > 0) {
          feature_metadata = match_go_terms(terms_list = input$pca_sparse_features,
                                            sparse_table = r6$tables$sparse_feat[[input$pca_feature_group]]$sparse_matrix)
        } else {
          return()
        }
      } else {
        feature_metadata = input$pca_feature_group
      }


    r6$param_pca(auto_refresh = input$pca_auto_refresh,
                 data_table = input$pca_data_table,
                 sample_groups_col = input$pca_sample_groups_col,
                 feature_groups_col = feature_metadata,
                 impute_median = input$pca_impute_median,
                 apply_da = input$pca_apply_da,
                 sample_groups_da = input$pca_sample_groups_da,
                 alpha_da = input$pca_alpha_da,
                 seed_da = input$pca_seed_da,
                 pca_method = input$pca_method,
                 nPcs = input$pca_npcs,
                 displayed_pc_1 = input$pca_displayed_pc_1,
                 displayed_pc_2 = input$pca_displayed_pc_2,
                 completeObs = input$pca_completeObs,
                 displayed_plots = input$pca_displayed_plots,
                 colors_palette = input$pca_colors_palette,
                 marker_size = input$pca_marker_size,
                 opacity = input$pca_opacity,
                 title_font_size = input$pca_title_font_size,
                 y_label_font_size = input$pca_y_label_font_size,
                 y_tick_font_size = input$pca_y_tick_font_size,
                 x_label_font_size = input$pca_x_label_font_size,
                 x_tick_font_size = input$pca_x_tick_font_size,
                 legend_font_size = input$pca_legend_font_size,
                 img_format = input$pca_img_format)

    if (!input$pca_auto_refresh) {
      r6$params$pca$auto_refresh = input$pca_auto_refresh
      return()
    }

    if (r6$params$pca$update) {
      try_plot(prefix = "PCA",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = pca_generate,
               spawn_function = pca_spawn,
               img_format = input$pca_img_format,
               toggle_function = "toggle_pca",
               input = input,
               output = output,
               session = session)
    }

  })


  # Download associated tables
  output$download_pca_scores_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_scores_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_scores_table, file_name)
    }
  )
  output$download_pca_loadings_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_loadings_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_loadings_table, file_name)
    }
  )

  # Expanded boxes
  pca_proxy = plotly::plotlyProxy(outputId = "pca_plot",
                                  session = session)

  shiny::observeEvent(input$pca_plotbox,{
    if (input$pca_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#-------------------------------------------------------- FA analysis index ----
fa_analysis_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Fatty acid analysis index plot: generating plot.")

  if (input$fa_analysis_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_fa_analysis(width = width,
                      height = height)
}

fa_analysis_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Fatty acid analysis index: spawning plot.")

  output$fa_analysis_plot_plot = plotly::renderPlotly({
    r6$plots$fa_analysis_plot
    plotly::config(r6$plots$fa_analysis_plot, toImageButtonOptions = list(format = format,
                                                                          filename = timestamped_name('fa_analysis'),
                                                                          height = NULL,
                                                                          width = NULL,
                                                                          scale = 1))
  })
}

fa_analysis_plot_ui = function(dimensions_obj, session) {
  # add function to show bs4dash with plotting function
  get_plotly_box(id = "fa_analysis_plot",
                 label = "Fatty acid analysis",
                 dimensions_obj = dimensions_obj,
                 session = session)
}

fa_analysis_plot_server = function(r6, output, session) {
  ns = session$ns

  # set some UI
  output$fa_analysis_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('fa_analysis_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$fa_analysis_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("fa_analysis_plot_data_table"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$fa_analysis_plot$datasets,
        selected = r6$params$fa_analysis_plot$data_table
      ),
      shiny::selectInput(
        inputId = ns("fa_analysis_plot_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$fa_analysis_plot$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("fa_analysis_plot_selected_view"),
        label = "Select view",
        choices = c("FA overview per lipid class (A)" = "lipidclass",
                    "Lipid class overview per FA (B)" = "fa"),
        selected = r6$params$fa_analysis_plot$selected_view,
        multiple = FALSE
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectizeInput(
            inputId = ns("fa_analysis_plot_selected_lipidclass"),
            label = "Select lipid class (A)",
            choices = c("All (incl. TG)" = "All",
                        "All (excl. TG)" = "All_noTG",
                        unique(r6$tables$raw_feat[['Lipid class']])[!(unique(r6$tables$raw_feat[['Lipid class']]) %in% c("PA"))]),
            selected = r6$params$fa_analysis_plot$selected_lipidclass,
            multiple = FALSE,
            width = "98%"
          )
        ),
        shiny::column(
          width = 6,
          shiny::selectizeInput(
            inputId = ns("fa_analysis_plot_selected_fa"),
            label = "Select fatty acid (B)",
            choices = get_fa_tails(r6$tables$raw_feat),
            selected = r6$params$fa_analysis_plot$selected_fa,
            multiple = TRUE,
            width = "98%"
          )
        )
      ),
      shiny::span(
        shinyWidgets::materialSwitch(
          inputId = ns("fa_analysis_plot_fa_norm"),
          label = "Fatty acid normalisation",
          status = "success",
          right = TRUE,
          value = r6$params$fa_analysis_plot$fa_norm
        ),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        title = "Normalize the data by the total of the fatty acids."
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('fa_analysis_plot_color_palette'),
        label = "Color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$fa_analysis_plot$color_palette,
        multiple = FALSE
      ),
      shiny::numericInput(
        inputId = ns("fa_analysis_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$fa_analysis_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_analysis_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$fa_analysis_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_analysis_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$fa_analysis_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_analysis_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$fa_analysis_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_analysis_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$fa_analysis_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_analysis_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$fa_analysis_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("fa_analysis_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$fa_analysis_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_fa_analysis_plot_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

fa_analysis_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$fa_analysis_plot_sidebar)
  monitor_refresh = shiny::reactive(input$fa_analysis_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$fa_analysis_plot$update) {
      try_plot(prefix = "FA analysis plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = fa_analysis_plot_generate,
               spawn_function = fa_analysis_plot_spawn,
               img_format = input$fa_analysis_plot_img_format,
               toggle_function = "toggle_fa_analysis_plot",
               input = input,
               output = output,
               session = session)
    }
  })


  # Generate the plot
  shiny::observeEvent(c(
    input$fa_analysis_plot_data_table,
    input$fa_analysis_plot_metacol,
    input$fa_analysis_plot_selected_view,
    input$fa_analysis_plot_selected_lipidclass,
    input$fa_analysis_plot_selected_fa,
    input$fa_analysis_plot_fa_norm,
    input$fa_analysis_plot_color_palette,
    input$fa_analysis_plot_title_font_size,
    input$fa_analysis_plot_y_label_font_size,
    input$fa_analysis_plot_y_tick_font_size,
    input$fa_analysis_plot_x_label_font_size,
    input$fa_analysis_plot_x_tick_font_size,
    input$fa_analysis_plot_legend_font_size,
    input$fa_analysis_plot_img_format), {

      if (input$fa_analysis_plot_selected_view == "fa") {
        shiny::req(input$fa_analysis_plot_selected_fa)
      }

      r6$param_fa_analysis_plot(
        auto_refresh = input$fa_analysis_plot_auto_refresh,
        data_table = input$fa_analysis_plot_data_table,
        group_col = input$fa_analysis_plot_metacol,
        selected_view = input$fa_analysis_plot_selected_view,
        selected_lipidclass = input$fa_analysis_plot_selected_lipidclass,
        selected_fa = input$fa_analysis_plot_selected_fa,
        fa_norm = input$fa_analysis_plot_fa_norm,
        color_palette = input$fa_analysis_plot_color_palette,
        title_font_size = input$fa_analysis_plot_title_font_size,
        y_label_font_size = input$fa_analysis_plot_y_label_font_size,
        y_tick_font_size = input$fa_analysis_plot_y_tick_font_size,
        x_label_font_size = input$fa_analysis_plot_x_label_font_size,
        x_tick_font_size = input$fa_analysis_plot_x_tick_font_size,
        legend_font_size = input$fa_analysis_plot_legend_font_size,
        img_format = input$fa_analysis_plot_img_format)

      if (!input$fa_analysis_plot_auto_refresh) {
        r6$params$fa_analysis_plot$auto_refresh = input$fa_analysis_plot_auto_refresh
        return()
      }

      if (r6$params$fa_analysis_plot$update) {
        try_plot(prefix = "FA analysis plot",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = fa_analysis_plot_generate,
                 spawn_function = fa_analysis_plot_spawn,
                 img_format = input$fa_analysis_plot_img_format,
                 toggle_function = "toggle_fa_analysis_plot",
                 input = input,
                 output = output,
                 session = session)
      }
    })

  # Download associated table
  output$download_fa_analysis_plot_table = shiny::downloadHandler(
    filename = function(){timestamped_name("fa_analysis_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$fa_analysis_plot_table, file_name)
    }
  )

  # Expanded boxes
  fa_analysis_plot_proxy = plotly::plotlyProxy(outputId = "fa_analysis_plot_plot",
                                          session = session)

  shiny::observeEvent(input$fa_analysis_plot_plotbox,{
    if (input$fa_analysis_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = fa_analysis_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = fa_analysis_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}
#---------------------------------------------------------- FA composition  ----
fa_comp_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Fatty acid composition: generating plot.")

  if (input$fa_comp_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_fa_comp(width = width,
                  height = height)
}

fa_comp_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Fatty acid composition: spawning plot.")

  output$fa_comp_plot_plot = plotly::renderPlotly({
    r6$plots$fa_comp_plot
    plotly::config(r6$plots$fa_comp_plot, toImageButtonOptions = list(format = format,
                                                                      filename = timestamped_name('fa_comp'),
                                                                      height = NULL,
                                                                      width = NULL,
                                                                      scale = 1))
  })
}

fa_comp_plot_ui = function(dimensions_obj, session) {
  # add function to show bs4dash with plotting function
  get_plotly_box(id = "fa_comp_plot",
                 label = "Composition analysis",
                 dimensions_obj = dimensions_obj,
                 session = session)
}

fa_comp_plot_server = function(r6, output, session) {
  ns = session$ns

  # set some UI
  output$fa_comp_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('fa_comp_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$fa_comp_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("fa_comp_plot_data_table"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$fa_comp_plot$datasets,
        selected = r6$params$fa_comp_plot$data_table
      ),
      shiny::selectInput(
        inputId = ns("fa_comp_plot_composition"),
        label = "Select composition",
        choices = list(
          "Fatty acid tail" = "fa_tail",
          "Total lipid" = "total_lipid"
        ),
        selected = r6$params$fa_comp_plot$composition,
        multiple = F
      ),
      shiny::selectInput(
        inputId = ns("fa_comp_plot_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$fa_comp_plot$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("fa_comp_plot_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$raw_meta[, r6$params$fa_comp_plot$group_col]),
        selected = c(r6$params$fa_comp_plot$group_1, r6$params$fa_comp_plot$group_2),
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("fa_comp_plot_selected_lipidclass"),
        label = "Select lipid class",
        choices = c("All (excl. PA)" = "All", unique(r6$tables$raw_feat[["Lipid class"]])),
        selected = r6$params$fa_comp_plot$selected_lipidclass,
        multiple = FALSE
      ),
      # Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectizeInput(
        inputId = ns('fa_comp_plot_color_palette'),
        label = "Color palette",
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$fa_comp_plot$color_palette,
        multiple = FALSE
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('fa_comp_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$fa_comp_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("fa_comp_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$fa_comp_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_comp_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$fa_comp_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_comp_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$fa_comp_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_comp_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$fa_comp_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_comp_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$fa_comp_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("fa_comp_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$fa_comp_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("fa_comp_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$fa_comp_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_fa_comp_plot_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

fa_comp_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # auto-update the lipid classes
  shiny::observeEvent(input$fa_comp_plot_composition, {
    if(input$fa_comp_plot_composition == "fa_tail") {
      lipidclass_choices = c("All (excl. PA)" = "All", unique(r6$tables$raw_feat[["Lipid class"]]))
    } else {
      lipidclass_choices = unique(r6$tables$raw_feat[["Lipid class"]])
    }

    if(r6$params$fa_comp_plot$selected_lipidclass == "All") {
      selected_lipidclass = "CE"
    } else {
      selected_lipidclass = r6$params$fa_comp_plot$selected_lipidclass
    }

    shiny::updateSelectizeInput(
      inputId = "fa_comp_plot_selected_lipidclass",
      session = session,
      choices = lipidclass_choices,
      selected = selected_lipidclass,
    )
  })

  # auto-update selected groups
  shiny::observeEvent(input$fa_comp_plot_metacol, {
    shiny::updateSelectizeInput(
      inputId = "fa_comp_plot_metagroup",
      session = session,
      choices = unique(r6$tables$raw_meta[, input$fa_comp_plot_metacol]),
      selected = unique(r6$tables$raw_meta[, input$fa_comp_plot_metacol])[c(1, 2)]
    )
  })

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$fa_comp_plot_sidebar)
  monitor_refresh = shiny::reactive(input$fa_comp_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$fa_comp_plot$update) {
      try_plot(prefix = "FA Comp plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = fa_comp_plot_generate,
               spawn_function = fa_comp_plot_spawn,
               img_format = input$fa_comp_plot_img_format,
               toggle_function = "toggle_fa_comp_plot",
               input = input,
               output = output,
               session = session)
    }
  })

  # Generate the plot
  shiny::observeEvent(c(
    shiny::req(length(input$fa_comp_plot_metagroup) == 2),
    input$fa_comp_plot_data_table,
    input$fa_comp_plot_composition,
    input$fa_comp_plot_selected_lipidclass,
    input$fa_comp_plot_color_palette,
    input$fa_comp_plot_reverse_palette,
    input$fa_comp_plot_title_font_size,
    input$fa_comp_plot_y_label_font_size,
    input$fa_comp_plot_y_tick_font_size,
    input$fa_comp_plot_x_label_font_size,
    input$fa_comp_plot_x_tick_font_size,
    input$fa_comp_plot_legend_font_size,
    input$fa_comp_plot_img_format),
    {

      if(!(input$fa_comp_plot_composition == "total_lipid" & input$fa_comp_plot_selected_lipidclass == "All")) {

        r6$param_fa_comp_plot(
          auto_refresh = input$fa_comp_plot_auto_refresh,
          data_table = input$fa_comp_plot_data_table,
          composition = input$fa_comp_plot_composition,
          group_col = input$fa_comp_plot_metacol,
          group_1 = input$fa_comp_plot_metagroup[1],
          group_2 = input$fa_comp_plot_metagroup[2],
          selected_lipidclass = input$fa_comp_plot_selected_lipidclass,
          color_palette = input$fa_comp_plot_color_palette,
          reverse_palette = input$fa_comp_plot_reverse_palette,
          title_font_size = input$fa_comp_plot_title_font_size,
          y_label_font_size = input$fa_comp_plot_y_label_font_size,
          y_tick_font_size = input$fa_comp_plot_y_tick_font_size,
          x_label_font_size = input$fa_comp_plot_x_label_font_size,
          x_tick_font_size = input$fa_comp_plot_x_tick_font_size,
          legend_font_size = input$fa_comp_plot_legend_font_size,
          img_format = input$fa_comp_plot_img_format
        )

        if (!input$fa_comp_plot_auto_refresh) {
          r6$params$fa_comp_plot$auto_refresh = input$fa_comp_plot_auto_refresh
          return()
        }


        if (r6$params$fa_comp_plot$update) {
          try_plot(prefix = "FA Comp plot",
                   r6 = r6,
                   dimensions_obj = dimensions_obj,
                   gen_function = fa_comp_plot_generate,
                   spawn_function = fa_comp_plot_spawn,
                   img_format = input$fa_comp_plot_img_format,
                   toggle_function = "toggle_fa_comp_plot",
                   input = input,
                   output = output,
                   session = session)
        }

      }
    })

  # Download associated table
  output$download_fa_comp_plot_table = shiny::downloadHandler(
    filename = function(){timestamped_name("fa_composition_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$fa_comp_table, file_name)
    }
  )

  # Expanded boxes
  fa_comp_plot_proxy = plotly::plotlyProxy(outputId = "fa_comp_plot_plot",
                                      session = session)

  shiny::observeEvent(input$fa_comp_plot_plotbox,{
    if (input$fa_comp_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = fa_comp_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = fa_comp_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}

#-------------------------------------------------------- Double bonds plot ----
double_bonds_plot_generate = function(r6, dimensions_obj, input, session) {
  print_tm(r6$name, "Double bonds plot: generating plot.")
  if (input$double_bonds_plot_plotbox$maximized) {
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_double_bonds_table()
  r6$plot_double_bonds_plot(width = width,
                            height = height)


}

double_bonds_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Double bonds plot: spawning plot.")
  output$double_bonds_plot_plot = plotly::renderPlotly({
    r6$plots$double_bonds_plot
    plotly::config(r6$plots$double_bonds_plot, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_module_name(r6, 'double_bonds_plot'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}



double_bonds_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "double_bonds_plot",
                 label = "Double bonds plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


double_bonds_plot_server = function(r6, output, session) {

  ns = session$ns

  output$double_bonds_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('double_bonds_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$double_bonds_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("double_bonds_plot_dataset"),
        label = "Select data table",
        choices = r6$hardcoded_settings$double_bonds_plot$datasets,
        selected = r6$params$double_bonds_comparison$data_table,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('double_bonds_plot_group_col'),
        label = "Group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$double_bonds_comparison$group_col,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('double_bonds_plot_groups'),
        label = "Select two groups",
        # choices = unique(r6$tables$raw_meta[,input$double_bonds_plot_group_col]),
        choices = NULL,
        selected = c(r6$params$double_bonds_comparison$group_1, r6$params$double_bonds_comparison$group_2),
        multiple = TRUE,
        width = '100%'
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_plot_selected_carbon_chain"),
        label = "Carbon count",
        choices = c('Carbon count (chain 1)', 'Carbon count (chain 2)', 'Carbon count (sum)'),
        selected = r6$params$double_bonds_plot$selected_carbon_chain,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_plot_selected_unsat"),
        label = "Double bonds count",
        choices = c('Double bonds (chain 1)', 'Double bonds (chain 2)', 'Double bonds (sum)'),
        selected = r6$params$double_bonds_plot$selected_unsat,
        multiple = FALSE,
        width = '100%'
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_plot_selected_lipid_class"),
        label = "Lipid class",
        choices = sort(unique(r6$tables$raw_feat$`Lipid class`)),
        selected = r6$params$double_bonds_plot$selected_lipid_class,
        multiple = FALSE,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('double_bonds_plot_fc_function'),
        label = 'Fold change calculation',
        choices = c('mean', 'median'),
        selected = r6$params$double_bonds_comparison$fc_function,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('double_bonds_plot_statistical_test'),
        label = 'Statistical test',
        choices = c('t-Test', 'Wilcoxon'),
        selected = r6$params$double_bonds_comparison$statistical_test,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('double_bonds_plot_adjustment_method'),
        label = 'Adjustment method',
        choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
        selected = r6$params$double_bonds_comparison$adjustment_method,
        multiple = F,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_min_fc"),
        label = 'Min abs(fold change)',
        value = r6$params$double_bonds_plot$min_fc,
        min = 0,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_max_pval"),
        label = 'Max p-value',
        value = r6$params$double_bonds_plot$max_pval,
        min = 0,
        max = 1,
        step = 0.01,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('double_bonds_plot_remove_missing_pval'),
        label = 'Remove_missing pval',
        value = r6$params$double_bonds_plot$remove_missing_pval,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('double_bonds_plot_remove_infitive_fc'),
        label = 'Remove infitive FC',
        value = r6$params$double_bonds_plot$remove_infitive_fc,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetic settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('double_bonds_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$double_bonds_plot$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('double_bonds_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$double_bonds_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns('double_bonds_plot_marker_size'),
        label = 'Marker size',
        value = r6$params$double_bonds_plot$marker_size,
        min = 0.1,
        max = NA,
        step = 0.1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns('double_bonds_plot_marker_opacity'),
        label = 'Marker opacity',
        value = r6$params$double_bonds_plot$marker_opacity,
        min = 0,
        max = 1,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$double_bonds_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$double_bonds_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$double_bonds_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$double_bonds_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$double_bonds_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("double_bonds_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$double_bonds_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("double_bonds_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$double_bonds_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_double_bonds_plot_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

double_bonds_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$double_bonds_plot_sidebar)
  monitor_refresh = shiny::reactive(input$double_bonds_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    if (r6$params$double_bonds_plot$update) {
      try_plot(prefix = "Double bonds plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = double_bonds_plot_generate,
               spawn_function = double_bonds_plot_spawn,
               img_format = input$double_bonds_plot_img_format,
               toggle_function = "toggle_double_bonds_plot",
               input = input,
               output = output,
               session = session)
    }
  })

  # Group col selection
  shiny::observeEvent(input$double_bonds_plot_group_col,{
    shiny::updateSelectInput(
      inputId = "double_bonds_plot_groups",
      session = session,
      choices = unique(r6$tables$raw_meta[,input$double_bonds_plot_group_col]),
      selected = unique(r6$tables$raw_meta[,input$double_bonds_plot_group_col])[c(1,2)]
    )
  })
  # Double bonds plot
  shiny::observeEvent(
    c(input$double_bonds_plot_dataset,
      input$double_bonds_plot_groups,
      input$double_bonds_plot_selected_carbon_chain,
      input$double_bonds_plot_selected_unsat,
      input$double_bonds_plot_selected_lipid_class,
      input$double_bonds_plot_fc_function,
      input$double_bonds_plot_statistical_test,
      input$double_bonds_plot_adjustment_method,
      input$double_bonds_plot_min_fc,
      input$double_bonds_plot_max_pval,
      input$double_bonds_plot_remove_missing_pval,
      input$double_bonds_plot_remove_infitive_fc,
      input$double_bonds_plot_color_palette,
      input$double_bonds_plot_reverse_palette,
      input$double_bonds_plot_marker_size,
      input$double_bonds_plot_marker_opacity,
      input$double_bonds_plot_title_font_size,
      input$double_bonds_plot_y_label_font_size,
      input$double_bonds_plot_y_tick_font_size,
      input$double_bonds_plot_x_label_font_size,
      input$double_bonds_plot_x_tick_font_size,
      input$double_bonds_plot_legend_font_size,
      input$double_bonds_plot_img_format
      ),
    {
    shiny::req(length(input$double_bonds_plot_groups) == 2)

      r6$param_double_bonds_comparison(
        data_table = input$double_bonds_plot_dataset,
        group_col = input$double_bonds_plot_group_col,
        group_1 = input$double_bonds_plot_groups[1],
        group_2 = input$double_bonds_plot_groups[2],
        fc_function = input$double_bonds_plot_fc_function,
        statistical_test = input$double_bonds_plot_statistical_test,
        adjustment_method = input$double_bonds_plot_adjustment_method
      )
      r6$param_double_bonds_plot(
        auto_refresh = input$double_bonds_plot_auto_refresh,
        carbon_selection = input$double_bonds_plot_selected_carbon_chain,
        unsat_selection = input$double_bonds_plot_selected_unsat,
        lipid_class = input$double_bonds_plot_selected_lipid_class,
        min_fc = input$double_bonds_plot_min_fc,
        max_pval = input$double_bonds_plot_max_pval,
        remove_missing_pval = input$double_bonds_plot_remove_missing_pval,
        remove_infitive_fc = input$double_bonds_plot_remove_infitive_fc,
        color_palette = input$double_bonds_plot_color_palette,
        reverse_palette = input$double_bonds_plot_reverse_palette,
        marker_size = input$double_bonds_plot_marker_size,
        marker_opacity = input$double_bonds_plot_marker_opacity,
        title_font_size = input$double_bonds_plot_title_font_size,
        y_label_font_size = input$double_bonds_plot_y_label_font_size,
        y_tick_font_size = input$double_bonds_plot_y_tick_font_size,
        x_label_font_size = input$double_bonds_plot_x_label_font_size,
        x_tick_font_size = input$double_bonds_plot_x_tick_font_size,
        legend_font_size = input$double_bonds_plot_legend_font_size,
        img_format = input$double_bonds_plot_img_format
      )

      if (!input$double_bonds_plot_auto_refresh) {
        r6$params$double_bonds_plot$auto_refresh = input$double_bonds_plot_auto_refresh
        return()
      }

      if (r6$params$double_bonds_plot$update) {
        try_plot(prefix = "Double bonds plot",
                 r6 = r6,
                 dimensions_obj = dimensions_obj,
                 gen_function = double_bonds_plot_generate,
                 spawn_function = double_bonds_plot_spawn,
                 img_format = input$double_bonds_plot_img_format,
                 toggle_function = "toggle_double_bonds_plot",
                 input = input,
                 output = output,
                 session = session)
      }



  })

  # Download associated tables
  output$download_double_bonds_plot_table = shiny::downloadHandler(
    filename = function(){timestamped_name("double_bonds_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$double_bonds_plot, file_name)
    }
  )

  # Expanded boxes
  double_bonds_plot_proxy = plotly::plotlyProxy(outputId = "double_bonds_plot_plot",
                                           session = session)

  shiny::observeEvent(input$double_bonds_plot_plotbox,{
    if (input$double_bonds_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = double_bonds_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = double_bonds_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}