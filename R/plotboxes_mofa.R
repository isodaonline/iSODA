#------------------------------------------------------- Explained variance ----
explained_variance_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Explained variance: generating plot.")

  if (input$explained_variance_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_explained_variance(width = width,
                             height = height)
}
explained_variance_spawn = function(r6, format, output) {
  print_tm(r6$name, "Explained variance: spawning plot.")
  output$explained_variance_plot = plotly::renderPlotly({
    r6$plots$explained_variance
    plotly::config(r6$plots$explained_variance, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_module_name(r6, 'explained_variance'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


explained_variance_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "explained_variance",
                 label = "Explained variance",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


explained_variance_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$explained_variance_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('explained_variance_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$explained_variance$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      shiny::selectInput(
        inputId = ns('explained_variance_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$explained_variance$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('explained_variance_reverse_color_palette'),
        label = 'Reverse palette',
        value = r6$params$explained_variance$reverse_color_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("explained_variance_title_font_size"),
        label = 'Title font size',
        value = r6$params$explained_variance$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("explained_variance_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$explained_variance$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("explained_variance_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$explained_variance$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("explained_variance_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$explained_variance$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("explained_variance_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$explained_variance$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("explained_variance_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$explained_variance$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("explained_variance_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$explained_variance$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("explained_variance_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


explained_variance_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$explained_variance_sidebar)
  monitor_refresh = shiny::reactive(input$explained_variance_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Explained variance",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = explained_variance_generate,
             spawn_function = explained_variance_spawn,
             img_format = input$explained_variance_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$explained_variance_color_palette,
      input$explained_variance_reverse_color_palette,
      input$explained_variance_title_font_size,
      input$explained_variance_y_label_font_size,
      input$explained_variance_y_tick_font_size,
      input$explained_variance_x_label_font_size,
      input$explained_variance_x_tick_font_size,
      input$explained_variance_legend_font_size,
      input$explained_variance_img_format),
    {

    r6$param_explained_variance(auto_refresh = input$explained_variance_auto_refresh,
                                color_palette = input$explained_variance_color_palette,
                                reverse_color_palette = input$explained_variance_reverse_color_palette,
                                title_font_size = input$explained_variance_title_font_size,
                                y_label_font_size = input$explained_variance_y_label_font_size,
                                y_tick_font_size = input$explained_variance_y_tick_font_size,
                                x_label_font_size = input$explained_variance_x_label_font_size,
                                x_tick_font_size = input$explained_variance_x_tick_font_size,
                                legend_font_size = input$explained_variance_legend_font_size,
                                img_format = input$explained_variance_img_format)

      if (!input$explained_variance_auto_refresh) {
        r6$params$explained_variance$auto_refresh = input$explained_variance_auto_refresh
        return()
      }

    try_plot(prefix = "Explained variance",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = explained_variance_generate,
             spawn_function = explained_variance_spawn,
             img_format = input$explained_variance_img_format,
             input = input,
             output = output,
             session = session)

  })

  # Download associated table
  output$explained_variance_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("explained_variance_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$explained_variance, file_name)
    }
  )

  # Expanded boxes
  explained_variance_proxy = plotly::plotlyProxy(outputId = "explained_variance_plot",
                                                 session = session)

  shiny::observeEvent(input$explained_variance_plotbox,{
    if (input$explained_variance_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = explained_variance_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = explained_variance_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#-------------------------------------------------------------- Factor plot ----
factor_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Factor plot: generating plot.")

  if (input$factor_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_factor_plot(width = width,
                      height = height)
}
factor_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Factor plot: spawning plot.")
  output$factor_plot_plot = plotly::renderPlotly({
    r6$plots$factor_plot
    plotly::config(r6$plots$factor_plot, toImageButtonOptions = list(format= format,
                                                                     filename= timestamped_module_name(r6, 'factor_plot'),
                                                                     height= NULL,
                                                                     width= NULL,
                                                                     scale= 1))
  })
}


factor_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "factor_plot",
                 label = "Factor plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


factor_plot_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$factor_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('factor_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$factor_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      shiny::selectInput(
        inputId = ns('factor_plot_factors'),
        label = 'Factors',
        choices = r6$params$factor_list,
        selected = r6$params$factor_plot$factors,
        multiple = T,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('factor_plot_scale'),
        label = 'Scale',
        value = r6$params$factor_plot$scale,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('factor_plot_groups'),
        label = 'Groups',
        choices = colnames(r6$tables$sample_metadata),
        selected = r6$params$factor_plot$groups,
        multiple = F,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('factor_plot_show_missing'),
        label = 'Show missing',
        value = r6$params$factor_plot$show_missing,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('factor_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$factor_plot$color_palette,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("factor_plot_marker_size"),
        label = 'Marker size',
        value = r6$params$factor_plot$marker_size,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("factor_plot_opacity"),
        label = 'Opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$factor_plot$opacity,
        step = 0.1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('factor_plot_add_violin'),
        label = 'Add violin',
        value = r6$params$factor_plot$add_violin,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('factor_plot_show_legend'),
        label = 'Show legend',
        value = r6$params$factor_plot$show_legend,
        right = TRUE,
        status = "primary"
      ),
      shiny::sliderInput(
        inputId = ns("factor_plot_violin_alpha"),
        label = 'Violin alpha',
        min = 0.1,
        max = 1.0,
        value = r6$params$factor_plot$violin_alpha,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("factor_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$factor_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("factor_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$factor_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("factor_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$factor_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("factor_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$factor_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("factor_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$factor_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("factor_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$factor_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("factor_plot_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$factor_plot$img_format,
        width = "100%"
        ),
      shiny::downloadButton(
        outputId = ns("factor_plot_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


factor_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$factor_plot_sidebar)
  monitor_refresh = shiny::reactive(input$factor_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Factor plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = factor_plot_generate,
             spawn_function = factor_plot_spawn,
             img_format = input$factor_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$factor_plot_factors,
      input$factor_plot_scale,
      input$factor_plot_groups,
      input$factor_plot_show_missing,
      input$factor_plot_color_palette,
      input$factor_plot_marker_size,
      input$factor_plot_opacity,
      input$factor_plot_add_violin,
      input$factor_plot_show_legend,
      input$factor_plot_violin_alpha,
      input$factor_plot_title_font_size,
      input$factor_plot_y_label_font_size,
      input$factor_plot_y_tick_font_size,
      input$factor_plot_x_label_font_size,
      input$factor_plot_x_tick_font_size,
      input$factor_plot_legend_font_size,
      input$factor_plot_img_format
      ),
    {

      r6$param_factor_plot(auto_refresh = input$factor_plot_auto_refresh,
                           factors = input$factor_plot_factors,
                           scale = input$factor_plot_scale,
                           groups = input$factor_plot_groups,
                           show_missing = input$factor_plot_show_missing,
                           color_palette = input$factor_plot_color_palette,
                           marker_size = input$factor_plot_marker_size,
                           opacity = input$factor_plot_opacity,
                           add_violin = input$factor_plot_add_violin,
                           show_legend = input$factor_plot_show_legend,
                           violin_alpha = input$factor_plot_violin_alpha,
                           title_font_size = input$factor_plot_title_font_size,
                           y_label_font_size = input$factor_plot_y_label_font_size,
                           y_tick_font_size = input$factor_plot_y_tick_font_size,
                           x_label_font_size = input$factor_plot_x_label_font_size,
                           x_tick_font_size = input$factor_plot_x_tick_font_size,
                           legend_font_size = input$factor_plot_legend_font_size,
                           img_format = input$factor_plot_img_format)

      if (!input$factor_plot_auto_refresh) {
        r6$params$factor_plot$auto_refresh = input$factor_plot_auto_refresh
        return()
      }

      try_plot(prefix = "Factor plot",
               r6 = r6,
               dimensions_obj = dimensions_obj,
               gen_function = factor_plot_generate,
               spawn_function = factor_plot_spawn,
               img_format = input$factor_plot_img_format,
               input = input,
               output = output,
               session = session)

  })

  # Download associated table
  output$factor_plot_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("factor_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$factor_plot, file_name)
    }
  )

  # Expanded boxes
  factor_plot_proxy = plotly::plotlyProxy(outputId = "factor_plot_plot",
                                          session = session)

  shiny::observeEvent(input$factor_plot_plotbox,{
    if (input$factor_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = factor_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = factor_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#---------------------------------------------------- Combined factors plot ----
combined_factors_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Combined factors plot: generating plot.")

  if (input$combined_factors_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_combined_factors_plot(width = width,
                                height = height)
}
combined_factors_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Combined factors plot: spawning plot.")
  output$combined_factors_plot_plot = plotly::renderPlotly({
    r6$plots$combined_factors_plot
    plotly::config(r6$plots$combined_factors_plot, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_module_name(r6, 'combined_factors_plot'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}


combined_factors_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "combined_factors_plot",
                 label = "Combined factors plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


combined_factors_plot_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$combined_factors_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('combined_factors_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$combined_factors_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('combined_factors_plot_factors'),
        label = 'Factors',
        choices = r6$params$factor_list,
        selected = r6$params$combined_factors_plot$factors,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('combined_factors_plot_groups'),
        label = 'Groups',
        choices = colnames(r6$tables$sample_metadata),
        selected = r6$params$combined_factors_plot$groups,
        multiple = F,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('combined_factors_plot_scale'),
        label = 'Scale',
        value = r6$params$combined_factors_plot$scale,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('combined_factors_plot_show_missing'),
        label = 'Show missing',
        value = r6$params$combined_factors_plot$show_missing,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('combined_factors_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$combined_factors_plot$color_palette,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("combined_factors_plot_marker_size"),
        label = 'Marker size',
        value = r6$params$combined_factors_plot$marker_size,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("combined_factors_plot_marker_opacity"),
        label = 'Marker opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$combined_factors_plot$marker_opacity,
        step = 0.1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("combined_factors_plot_area_alpha"),
        label = 'Area alpha',
        min = 0.1,
        max = 1.0,
        value = r6$params$combined_factors_plot$area_alpha,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("combined_factors_plot_title_font_size"),
        label = 'Title font size',
        value = r6$params$combined_factors_plot$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("combined_factors_plot_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$combined_factors_plot$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("combined_factors_plot_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$combined_factors_plot$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("combined_factors_plot_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$combined_factors_plot$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("combined_factors_plot_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$combined_factors_plot$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("combined_factors_plot_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$combined_factors_plot$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("combined_factors_plot_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$combined_factors_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("combined_factors_plot_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


combined_factors_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$combined_factors_plot_sidebar)
  monitor_refresh = shiny::reactive(input$combined_factors_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Combined factors plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = combined_factors_plot_generate,
             spawn_function = combined_factors_plot_spawn,
             img_format = input$combined_factors_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$combined_factors_plot_factors,
      input$combined_factors_plot_groups,
      input$combined_factors_plot_scale,
      input$combined_factors_plot_show_missing,
      input$combined_factors_plot_color_palette,
      input$combined_factors_plot_marker_size,
      input$combined_factors_plot_marker_opacity,
      input$combined_factors_plot_area_alpha,
      input$combined_factors_plot_title_font_size,
      input$combined_factors_plot_y_label_font_size,
      input$combined_factors_plot_y_tick_font_size,
      input$combined_factors_plot_x_label_font_size,
      input$combined_factors_plot_x_tick_font_size,
      input$combined_factors_plot_legend_font_size,
      input$combined_factors_plot_img_format
      ),
    {

    r6$param_combined_factors_plot(auto_refresh = input$combined_factors_plot_auto_refresh,
                                   factors = input$combined_factors_plot_factors,
                                   groups = input$combined_factors_plot_groups,
                                   scale = input$combined_factors_plot_scale,
                                   show_missing = input$combined_factors_plot_show_missing,
                                   color_palette = input$combined_factors_plot_color_palette,
                                   marker_size = input$combined_factors_plot_marker_size,
                                   marker_opacity = input$combined_factors_plot_marker_opacity,
                                   area_alpha = input$combined_factors_plot_area_alpha,
                                   title_font_size = input$combined_factors_plot_title_font_size,
                                   y_label_font_size = input$combined_factors_plot_y_label_font_size,
                                   y_tick_font_size = input$combined_factors_plot_y_tick_font_size,
                                   x_label_font_size = input$combined_factors_plot_x_label_font_size,
                                   x_tick_font_size = input$combined_factors_plot_x_tick_font_size,
                                   legend_font_size = input$combined_factors_plot_legend_font_size,
                                   img_format = input$combined_factors_plot_img_format)

      if (!input$combined_factors_plot_auto_refresh) {
        r6$params$combined_factors_plot$auto_refresh = input$combined_factors_plot_auto_refresh
        return()
      }

    try_plot(prefix = "Combined factors plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = combined_factors_plot_generate,
             spawn_function = combined_factors_plot_spawn,
             img_format = input$combined_factors_plot_img_format,
             input = input,
             output = output,
             session = session)

  })

  # Download associated table
  output$combined_factors_plot_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("combined_factors_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$combined_factors_plot, file_name)
    }
  )

  # Expanded boxes
  combined_factors_plot_proxy = plotly::plotlyProxy(outputId = "combined_factors_plot_plot",
                                               session = session)

  shiny::observeEvent(input$combined_factors_plot_plotbox,{
    if (input$combined_factors_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = combined_factors_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = combined_factors_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}

#---------------------------------------------------------- Feature weights ----
feature_weights_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Feature weights: generating plot.")

  if (input$feature_weights_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_feature_weights(width = width,
                          height = height)
}
feature_weights_spawn = function(r6, format, output) {
  print_tm(r6$name, "Feature weights: spawning plot.")
  output$feature_weights_plot = plotly::renderPlotly({
    r6$plots$feature_weights
    plotly::config(r6$plots$feature_weights, toImageButtonOptions = list(format= format,
                                                                         filename= timestamped_module_name(r6, 'feature_weights'),
                                                                         height= NULL,
                                                                         width= NULL,
                                                                         scale= 1))
  })
}


feature_weights_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "feature_weights",
                 label = "Feature weights",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


feature_weights_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$feature_weights_sidebar_ui = shiny::renderUI({
    shiny::tagList(

      shinyWidgets::materialSwitch(
        inputId = ns('feature_weights_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$feature_weights$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('feature_weights_omics'),
        label = 'Omics',
        choices = r6$params$omics,
        selected = r6$params$feature_weights$omics,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('feature_weights_factors'),
        label = 'Factors',
        choices = r6$params$factor_list,
        selected = r6$params$feature_weights$factors,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('feature_weights_groups'),
        label = 'Groups',
        choices = r6$params$feature_weights$groups_list,
        selected = r6$params$feature_weights$groups,
        multiple = F,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('feature_weights_scale'),
        label = 'Scale',
        value = r6$params$feature_weights$scale,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('feature_weights_abs'),
        label = 'Abs',
        value = r6$params$feature_weights$abs,
        right = TRUE,
        status = "primary"
      ),

      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('feature_weights_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$feature_weights$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('feature_weights_reverse_color_palette'),
        label = 'Reverse color palette',
        value = r6$params$feature_weights$reverse_color_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("feature_weights_marker_size"),
        label = 'Marker size',
        value = r6$params$feature_weights$marker_size,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("feature_weights_marker_opacity"),
        label = 'Marker opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$feature_weights$marker_opacity,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_weights_title_font_size"),
        label = 'Title font size',
        value = r6$params$feature_weights$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_weights_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$feature_weights$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_weights_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$feature_weights$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_weights_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$feature_weights$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_weights_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$feature_weights$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_weights_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$feature_weights$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("feature_weights_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$feature_weights$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("feature_weights_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


feature_weights_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$feature_weights_sidebar)
  monitor_refresh = shiny::reactive(input$feature_weights_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Feature weights",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = feature_weights_generate,
             spawn_function = feature_weights_spawn,
             img_format = input$feature_weights_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Update on omics
  shiny::observeEvent(input$feature_weights_omics, {
    r6$params$feature_weights$groups_list = c('factor_weights', 'None', colnames(r6$tables$feature_tables[[input$feature_weights_omics]]))
    shiny::updateSelectInput(
      inputId = 'feature_weights_groups',
      session = session,
      choices = r6$params$feature_weights$groups_list
    )
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$feature_weights_omics,
      input$feature_weights_factors,
      input$feature_weights_groups,
      input$feature_weights_scale,
      input$feature_weights_abs,
      input$feature_weights_color_palette,
      input$feature_weights_reverse_color_palette,
      input$feature_weights_marker_size,
      input$feature_weights_marker_opacity,
      input$feature_weights_title_font_size,
      input$feature_weights_y_label_font_size,
      input$feature_weights_y_tick_font_size,
      input$feature_weights_x_label_font_size,
      input$feature_weights_x_tick_font_size,
      input$feature_weights_legend_font_size,
      input$feature_weights_img_format
    ),
    {

    r6$param_feature_weights(auto_refresh = input$feature_weights_auto_refresh,
                             omics = input$feature_weights_omics,
                             factors = input$feature_weights_factors,
                             groups = input$feature_weights_groups,
                             scale = input$feature_weights_scale,
                             abs = input$feature_weights_abs,
                             color_palette = input$feature_weights_color_palette,
                             reverse_color_palette = input$feature_weights_reverse_color_palette,
                             marker_size = input$feature_weights_marker_size,
                             marker_opacity = input$feature_weights_marker_opacity,
                             title_font_size = input$feature_weights_title_font_size,
                             y_label_font_size = input$feature_weights_y_label_font_size,
                             y_tick_font_size = input$feature_weights_y_tick_font_size,
                             x_label_font_size = input$feature_weights_x_label_font_size,
                             x_tick_font_size = input$feature_weights_x_tick_font_size,
                             legend_font_size = input$feature_weights_legend_font_size,
                             img_format = input$feature_weights_img_format)

      if (!input$feature_weights_auto_refresh) {
        r6$params$feature_weights$auto_refresh = input$feature_weights_auto_refresh
        return()
      }

    try_plot(prefix = "Feature weights",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = feature_weights_generate,
             spawn_function = feature_weights_spawn,
             img_format = input$feature_weights_img_format,
             input = input,
             output = output,
             session = session)

  })

  # Download associated table
  output$feature_weights_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("feature_weights_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$feature_weights, file_name)
    }
  )

  # Expanded boxes
  feature_weights_proxy = plotly::plotlyProxy(outputId = "feature_weights_plot",
                                              session = session)

  shiny::observeEvent(input$feature_weights_plotbox,{
    if (input$feature_weights_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = feature_weights_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = feature_weights_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#------------------------------------------------------ Feature top weights ----

feature_top_weights_generate = function(r6, dimensions_obj, input) {
  print_t("MOFA Feature top weights: generating plot.")

  if (input$feature_top_weights_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_feature_top_weights(width = width,
                              height = height)

}

feature_top_weights_spawn = function(r6, format, output) {
  print_tm(r6$name, "Feature top weights: spawning plot.")
  output$feature_top_weights_plot = plotly::renderPlotly({
    r6$plots$feature_top_weights
    plotly::config(r6$plots$feature_top_weights, toImageButtonOptions = list(format= format,
                                                                         filename= timestamped_module_name(r6, 'feature_top_weights'),
                                                                         height= NULL,
                                                                         width= NULL,
                                                                         scale= 1))
  })
}



feature_top_weights_ui = function(dimensions_obj, session) {
  get_plotly_box(id = "feature_top_weights",
                 label = "Feature top weights",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


feature_top_weights_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$feature_top_weights_sidebar_ui = shiny::renderUI({
    shiny::tagList(

      shinyWidgets::materialSwitch(
        inputId = ns('feature_top_weights_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$feature_top_weights$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("feature_top_weights_omics"),
        label = "Omics",
        choices = r6$params$omics,
        selected = r6$params$feature_top_weights$omics,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns('feature_top_weights_factors'),
        label = 'Factors',
        choices = r6$params$factor_list,
        selected = r6$params$feature_top_weights$factors,
        multiple = F,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('feature_top_weights_nfeatures'),
        label = 'nFeatures',
        value = r6$params$feature_top_weights$nfeatures,
        min = 0,
        step = 1
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('feature_top_weights_abs'),
        label = 'Abs',
        value = r6$params$feature_top_weights$abs,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('feature_top_weights_scale'),
        label = 'Scale',
        value = r6$params$feature_top_weights$scale,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('feature_top_weights_sign'),
        label = 'Sign',
        choices = c('all', 'positive', 'negative'),
        selected = r6$params$feature_top_weights$sign,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('feature_top_weights_groups'),
        label = 'Groups',
        choices = r6$params$feature_top_weights$groups_list,
        selected = r6$params$feature_top_weights$groups,
        multiple = F,
        width = '100%'
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('feature_top_weights_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$feature_top_weights$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('feature_top_weights_reverse_color_palette'),
        label = 'Reverse color palette',
        value = r6$params$feature_top_weights$reverse_color_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("feature_top_weights_marker_size"),
        label = 'Marker size',
        value = r6$params$feature_top_weights$marker_size,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("feature_top_weights_marker_opacity"),
        label = 'Marker opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$feature_top_weights$marker_opacity,
        step = 0.1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_top_weights_title_font_size"),
        label = 'Title font size',
        value = r6$params$feature_top_weights$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_top_weights_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$feature_top_weights$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_top_weights_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$feature_top_weights$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_top_weights_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$feature_top_weights$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_top_weights_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$feature_top_weights$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("feature_top_weights_legend_font_size"),
        label = 'Legend font size',
        value = r6$params$feature_top_weights$legend_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("feature_top_weights_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$feature_top_weights$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("feature_top_weights_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

feature_top_weights_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$feature_top_weights_sidebar)
  monitor_refresh = shiny::reactive(input$feature_top_weights_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Feature top weights",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = feature_top_weights_generate,
             spawn_function = feature_top_weights_spawn,
             img_format = input$feature_top_weights_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Update on omics
  shiny::observeEvent(input$feature_top_weights_omics, {
    r6$params$feature_top_weights$groups_list = c('sign', colnames(r6$tables$feature_tables[[input$feature_top_weights_omics]]))
    shiny::updateSelectInput(
      inputId = 'feature_top_weights_groups',
      session = session,
      choices = r6$params$feature_top_weights$groups_list
    )
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$feature_top_weights_omics,
      input$feature_top_weights_factors,
      input$feature_top_weights_nfeatures,
      input$feature_top_weights_abs,
      input$feature_top_weights_scale,
      input$feature_top_weights_sign,
      input$feature_top_weights_groups,
      input$feature_top_weights_color_palette,
      input$feature_top_weights_reverse_color_palette,
      input$feature_top_weights_marker_size,
      input$feature_top_weights_marker_opacity,
      input$feature_top_weights_title_font_size,
      input$feature_top_weights_y_label_font_size,
      input$feature_top_weights_y_tick_font_size,
      input$feature_top_weights_x_label_font_size,
      input$feature_top_weights_x_tick_font_size,
      input$feature_top_weights_legend_font_size,
      input$feature_top_weights_img_format
    ),
    {

      r6$param_feature_top_weights(auto_refresh = input$feature_top_weights_auto_refresh,
                                omics = input$feature_top_weights_omics,
                                factors = input$feature_top_weights_factors,
                                nfeatures = input$feature_top_weights_nfeatures,
                                abs = input$feature_top_weights_abs,
                                scale = input$feature_top_weights_scale,
                                sign = input$feature_top_weights_sign,
                                groups = input$feature_top_weights_groups,
                                color_palette = input$feature_top_weights_color_palette,
                                reverse_color_palette = input$feature_top_weights_reverse_color_palette,
                                marker_size = input$feature_top_weights_marker_size,
                                marker_opacity = input$feature_top_weights_marker_opacity,
                                title_font_size = input$feature_top_weights_title_font_size,
                                y_label_font_size = input$feature_top_weights_y_label_font_size,
                                y_tick_font_size = input$feature_top_weights_y_tick_font_size,
                                x_label_font_size = input$feature_top_weights_x_label_font_size,
                                x_tick_font_size = input$feature_top_weights_x_tick_font_size,
                                legend_font_size = input$feature_top_weights_legend_font_size,
                                img_format = input$feature_top_weights_img_format)

      if (!input$feature_top_weights_auto_refresh) {
        r6$params$feature_top_weights$auto_refresh = input$feature_top_weights_auto_refresh
        return()
      }

    try_plot(prefix = "Feature top weights",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = feature_top_weights_generate,
             spawn_function = feature_top_weights_spawn,
             img_format = input$feature_top_weights_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Download associated table
  output$feature_top_weights_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("feature_top_weights_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$feature_top_weights, file_name)
    }
  )

  # Expanded boxes
  feature_top_weights_proxy = plotly::plotlyProxy(outputId = "feature_top_weights_plot",
                                              session = session)

  shiny::observeEvent(input$feature_top_weights_plotbox,{
    if (input$feature_top_weights_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = feature_top_weights_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = feature_top_weights_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------------- MOFA heatmap ----
mofa_heatmap_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "MOFA heatmap: generating plot.")

  if (input$mofa_heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_mofa_heatmap(width = width,
                       height = height)
}
mofa_heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "MOFA heatmap: spawning plot.")
  output$mofa_heatmap_plot = plotly::renderPlotly({
    r6$plots$mofa_heatmap
    plotly::config(r6$plots$mofa_heatmap, toImageButtonOptions = list(format= format,
                                                                      filename= timestamped_module_name(r6, 'mofa_heatmap'),
                                                                      height= NULL,
                                                                      width= NULL,
                                                                      scale= 1))
  })
}


mofa_heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "mofa_heatmap",
                 label = "MOFA heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


mofa_heatmap_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$mofa_heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('mofa_heatmap_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$mofa_heatmap$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('mofa_heatmap_factor'),
        label = 'Factor',
        choices = r6$params$factor_list,
        selected = r6$params$mofa_heatmap$factor,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('mofa_heatmap_omics'),
        label = 'Omics',
        choices = r6$params$omics,
        selected = r6$params$mofa_heatmap$omics,
        multiple = F,
        width = '100%'
      ),

      shiny::numericInput(
        inputId = ns('mofa_heatmap_features'),
        label = 'Features',
        value = r6$params$mofa_heatmap$features,
        min  = 0,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('mofa_heatmap_sample_annotations'),
        label = 'Sample annotations',
        choices = colnames(r6$tables$sample_metadata),
        selected = r6$params$mofa_heatmap$sample_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('mofa_heatmap_feature_annotations'),
        label = 'Feature annotations',
        choices = r6$params$mofa_heatmap$features_list,
        selected = r6$params$mofa_heatmap$feature_annotations,
        multiple = T,
        width = '100%'
      ),
      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shinyWidgets::materialSwitch(
        inputId = ns('mofa_heatmap_imputed'),
        label = 'Imputed',
        value = r6$params$mofa_heatmap$imputed,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('mofa_heatmap_denoise'),
        label = 'Denoise',
        value = r6$params$mofa_heatmap$denoise,
        right = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("mofa_heatmap_distance_method"),
        label = "Distance method",
        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
        selected = r6$params$mofa_heatmap$distance_method,
        multiple = F
      ),

      shiny::numericInput(
        inputId = ns("mofa_heatmap_p_minkowski"),
        label = "Minkowski distance power",
        value = r6$params$mofa_heatmap$p_minkowski,
        width = '100%'
      ),

      shiny::selectInput(
        inputId = ns("mofa_heatmap_clustering_method"),
        label = "Clustering method",
        choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
        selected = r6$params$mofa_heatmap$clustering_method,
        multiple = F
      ),
      shiny::numericInput(
        inputId = ns("mofa_heatmap_k_clusters_samples"),
        label = "Sample clusters",
        value = r6$params$mofa_heatmap$k_clusters_samples,
        min = 1,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("mofa_heatmap_k_clusters_features"),
        label = "Feature clusters",
        value = r6$params$mofa_heatmap$k_clusters_features,
        min = 1,
        step = 1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('mofa_heatmap_center'),
        label = 'Center',
        value = r6$params$mofa_heatmap$center,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('mofa_heatmap_apply_clustering'),
        label = 'Apply clustering',
        value = r6$params$mofa_heatmap$apply_clustering,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('mofa_heatmap_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$mofa_heatmap$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('mofa_heatmap_reverse_color_palette'),
        label = 'Reverse color palette',
        value = r6$params$mofa_heatmap$reverse_color_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("mofa_heatmap_title_font_size"),
        label = 'Title font size',
        value = r6$params$mofa_heatmap$title_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("mofa_heatmap_y_label_font_size"),
        label = 'y-axis label font size',
        value = r6$params$mofa_heatmap$y_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("mofa_heatmap_y_tick_font_size"),
        label = 'y-axis tick font size',
        value = r6$params$mofa_heatmap$y_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("mofa_heatmap_x_label_font_size"),
        label = 'x-axis label font size',
        value = r6$params$mofa_heatmap$x_label_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns("mofa_heatmap_x_tick_font_size"),
        label = 'x-axis tick font size',
        value = r6$params$mofa_heatmap$x_tick_font_size,
        min = 0,
        step = 1,
        width = '100%'
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("mofa_heatmap_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$mofa_heatmap$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("mofa_heatmap_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


mofa_heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$mofa_heatmap_sidebar)
  monitor_refresh = shiny::reactive(input$mofa_heatmap_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "MOFA heatmap",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = mofa_heatmap_generate,
             spawn_function = mofa_heatmap_spawn,
             img_format = input$mofa_heatmap_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Update feature table on omics
  shiny::observeEvent(input$mofa_heatmap_omics, {
    r6$params$mofa_heatmap$features_list = colnames(r6$tables$feature_tables[[input$mofa_heatmap_omics]])
    shiny::updateSelectInput(
      inputId = 'mofa_heatmap_feature_annotations',
      session = session,
      choices = r6$params$mofa_heatmap$features_list
    )
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$mofa_heatmap_factor,
      input$mofa_heatmap_omics,
      input$mofa_heatmap_features,
      input$mofa_heatmap_sample_annotations,
      input$mofa_heatmap_feature_annotations,
      input$mofa_heatmap_imputed,
      input$mofa_heatmap_denoise,
      input$mofa_heatmap_distance_method,
      input$mofa_heatmap_p_minkowski,
      input$mofa_heatmap_clustering_method,
      input$mofa_heatmap_k_clusters_samples,
      input$mofa_heatmap_k_clusters_features,
      input$mofa_heatmap_center,
      input$mofa_heatmap_apply_clustering,
      input$mofa_heatmap_color_palette,
      input$mofa_heatmap_reverse_color_palette,
      input$mofa_heatmap_title_font_size,
      input$mofa_heatmap_y_label_font_size,
      input$mofa_heatmap_y_tick_font_size,
      input$mofa_heatmap_x_label_font_size,
      input$mofa_heatmap_x_tick_font_size,
      input$mofa_heatmap_img_format
      ),
    {

    r6$param_mofa_heatmap(auto_refresh = input$mofa_heatmap_auto_refresh,
                          factor = input$mofa_heatmap_factor,
                          omics = input$mofa_heatmap_omics,
                          features = input$mofa_heatmap_features,
                          sample_annotations = input$mofa_heatmap_sample_annotations,
                          feature_annotations = input$mofa_heatmap_feature_annotations,
                          imputed = input$mofa_heatmap_imputed,
                          denoise = input$mofa_heatmap_denoise,
                          distance_method = input$mofa_heatmap_distance_method,
                          p_minkowski = input$mofa_heatmap_p_minkowski,
                          clustering_method = input$mofa_heatmap_clustering_method,
                          k_clusters_samples = input$mofa_heatmap_k_clusters_samples,
                          k_clusters_features = input$mofa_heatmap_k_clusters_features,
                          center = input$mofa_heatmap_center,
                          apply_clustering = input$mofa_heatmap_apply_clustering,
                          color_palette = input$mofa_heatmap_color_palette,
                          reverse_color_palette = input$mofa_heatmap_reverse_color_palette,
                          title_font_size = input$mofa_heatmap_title_font_size,
                          y_label_font_size = input$mofa_heatmap_y_label_font_size,
                          y_tick_font_size = input$mofa_heatmap_y_tick_font_size,
                          x_label_font_size = input$mofa_heatmap_x_label_font_size,
                          x_tick_font_size = input$mofa_heatmap_x_tick_font_size,
                          img_format = input$mofa_heatmap_img_format)

      if (!input$mofa_heatmap_auto_refresh) {
        r6$params$mofa_heatmap$auto_refresh = input$mofa_heatmap_auto_refresh
        return()
      }

    try_plot(prefix = "MOFA heatmap",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = mofa_heatmap_generate,
             spawn_function = mofa_heatmap_spawn,
             img_format = input$mofa_heatmap_img_format,
             input = input,
             output = output,
             session = session)

  })

  # Download associated table
  output$mofa_heatmap_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("mofa_heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$mofa_heatmap, file_name)
    }
  )

  # Expanded boxes
  mofa_heatmap_proxy = plotly::plotlyProxy(outputId = "mofa_heatmap_plot",
                                           session = session)

  shiny::observeEvent(input$mofa_heatmap_plotbox,{
    if (input$mofa_heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = mofa_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = mofa_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}


#-------------------------------------------------------------- Scatter plot ----
scatter_plot_generate = function(r6, dimensions_obj, input) {
  print_tm(r6$name, "Scatter plot: generating plot.")

  if (input$scatter_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_scatter_plot(width = width,
                       height = height)
}
scatter_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Scatter plot: spawning plot.")
  output$scatter_plot_plot = plotly::renderPlotly({
    r6$plots$scatter_plot
    plotly::config(r6$plots$scatter_plot, toImageButtonOptions = list(format= format,
                                                                     filename= timestamped_module_name(r6, 'scatter_plot'),
                                                                     height= NULL,
                                                                     width= NULL,
                                                                     scale= 1))
  })
}


scatter_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "scatter_plot",
                 label = "Scatter plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


scatter_plot_server = function(r6, output, session) {

  ns = session$ns

  # Generate UI
  output$scatter_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('scatter_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$scatter_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      ## Input settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('scatter_plot_factor'),
        label = 'Factor',
        choices = r6$params$factor_list,
        selected = r6$params$scatter_plot$factor,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('scatter_plot_omics'),
        label = 'Omics',
        choices = r6$params$omics,
        selected = r6$params$scatter_plot$omics,
        multiple = F,
        width = '100%'
      ),
      shiny::numericInput(
        inputId = ns('scatter_plot_features'),
        label = 'Features',
        value = r6$params$scatter_plot$features,
        min  = 0,
        step = 1,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('scatter_plot_sample_annotations'),
        label = 'Sample annotations',
        choices = colnames(r6$tables$sample_metadata),
        selected = r6$params$scatter_plot$sample_annotations,
        multiple = F,
        width = '100%'
      ),

      ## Data settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('scatter_plot_sign'),
        label = 'Sign',
        choices = c('all', 'positive', 'negative'),
        selected = r6$params$scatter_plot$sign,
        multiple = F,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('scatter_plot_add_lm'),
        label = 'Add lm',
        value = r6$params$scatter_plot$add_lm,
        right = TRUE,
        status = "primary"
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('scatter_plot_imputed'),
        label = 'Imputed',
        value = r6$params$scatter_plot$imputed,
        right = TRUE,
        status = "primary"
      ),
      ## Aesthetics settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns('scatter_plot_color_palette'),
        label = 'Color palette',
        choices = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                    'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                    'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                    'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                    'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                    'plotly_1', 'plotly_2', 'ggplot2'),
        selected = r6$params$scatter_plot$color_palette,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('scatter_plot_reverse_palette'),
        label = 'Reverse palette',
        value = r6$params$scatter_plot$reverse_palette,
        right = TRUE,
        status = "primary"
      ),
      shiny::numericInput(
        inputId = ns("scatter_plot_marker_size"),
        label = 'Marker size',
        value = r6$params$scatter_plot$marker_size,
        step = 1,
        width = '100%'
      ),
      shiny::sliderInput(
        inputId = ns("scatter_plot_marker_opacity"),
        label = 'Marker opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$scatter_plot$marker_opacity,
        step = 0.1,
        width = '100%'
      ),
      shinyWidgets::materialSwitch(
        inputId = ns('scatter_plot_show_legend'),
        label = 'Show legend',
        value = r6$params$scatter_plot$show_legend,
        right = TRUE,
        status = "primary"
      ),
      ## Output settings
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("scatter_plot_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$scatter_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("scatter_plot_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


scatter_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Sidebar refresh
  monitor_sidebar = shiny::reactive(input$scatter_plot_sidebar)
  monitor_refresh = shiny::reactive(input$scatter_plot_auto_refresh)
  shiny::observe({
    if (is.null(monitor_sidebar())) {return()}
    if (is.null(monitor_refresh())) {return()}
    if (monitor_sidebar()) {return()}
    if (monitor_refresh()) {return()}
    try_plot(prefix = "Scatter plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = scatter_plot_generate,
             spawn_function = scatter_plot_spawn,
             img_format = input$scatter_plot_img_format,
             input = input,
             output = output,
             session = session)
  })

  # Generate the plot
  shiny::observeEvent(
    c(input$scatter_plot_factor,
      input$scatter_plot_omics,
      input$scatter_plot_features,
      input$scatter_plot_sample_annotations,
      input$scatter_plot_sign,
      input$scatter_plot_add_lm,
      input$scatter_plot_imputed,
      input$scatter_plot_color_palette,
      input$scatter_plot_reverse_palette,
      input$scatter_plot_marker_size,
      input$scatter_plot_marker_opacity,
      input$scatter_plot_show_legend,
      input$scatter_plot_img_format
      ),
    {

      r6$param_scatter_plot(auto_refresh = input$scatter_plot_auto_refresh,
                            factor = input$scatter_plot_factor,
                            omics = input$scatter_plot_omics,
                            features = input$scatter_plot_features,
                            sign = input$scatter_plot_sign,
                            sample_annotations = input$scatter_plot_sample_annotations,
                            add_lm = input$scatter_plot_add_lm,
                            imputed = input$scatter_plot_imputed,
                            color_palette = input$scatter_plot_color_palette,
                            reverse_palette = input$scatter_plot_reverse_palette,
                            marker_size = input$scatter_plot_marker_size,
                            marker_opacity = input$scatter_plot_marker_opacity,
                            show_legend = input$scatter_plot_show_legend,
                            img_format = input$scatter_plot_img_format)

      if (!input$scatter_plot_auto_refresh) {
        r6$params$scatter_plot$auto_refresh = input$scatter_plot_auto_refresh
        return()
      }

    try_plot(prefix = "Scatter plot",
             r6 = r6,
             dimensions_obj = dimensions_obj,
             gen_function = scatter_plot_generate,
             spawn_function = scatter_plot_spawn,
             img_format = input$scatter_plot_img_format,
             input = input,
             output = output,
             session = session)

  })

  # Download associated table
  output$scatter_plot_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("scatter_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$scatter_plot, file_name)
    }
  )

  # Expanded boxes
  scatter_plot_proxy = plotly::plotlyProxy(outputId = "scatter_plot_plot",
                                          session = session)

  shiny::observeEvent(input$scatter_plot_plotbox,{
    if (input$scatter_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = scatter_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = scatter_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}




