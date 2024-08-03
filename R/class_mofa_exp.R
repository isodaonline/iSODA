#---------------------------- Class Mofa_data ---------------------------------
Mofa_class = R6::R6Class(
  "Mofa_exp",
  public = list(

    initialize = function(name = NA){
      self$name = name
    },

    #--------------------------------------------------------------- Global ----
    name = NULL,
    type = NULL,

    #----------------------------------------------------------- Parameters ----
    params = list(

      # Number of views / omics
      omics = NULL,

      # Number of factors
      factor_list = NULL,
      sample_metadata = NULL,

      # Explained variance parameters
      explained_variance = list(
        auto_refresh = F,
        color_palette = 'Blues',
        reverse_color_palette = F,
        title_font_size = 15,
        y_label_font_size = 0,
        y_tick_font_size = 15,
        x_label_font_size = 0,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = 'png',
        update = T
      ),

      # Factor plot parameters
      factor_plot = list(
        auto_refresh = F,
        factors = c(1,2),
        scale = F,
        groups = NULL,
        show_missing = F,
        color_palette = "Spectral",
        marker_size = 10,
        opacity = 1,
        add_violin = T,
        show_legend = T,
        violin_alpha = 0.5,
        title_font_size = 0,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_label_font_size = 20,
        x_tick_font_size = 0,
        legend_font_size = 15,
        img_format = 'png',
        update = T
      ),

      combined_factors_plot = list(
        auto_refresh = F,
        factors = c(1,2),
        groups = NULL,
        scale = FALSE,
        show_missing = FALSE,
        color_palette = "Spectral",
        marker_size = 10,
        marker_opacity = 1,
        area_alpha = 0.5,
        title_font_size = 0,
        y_label_font_size = 17,
        x_label_font_size = 17,
        y_tick_font_size = 15,
        x_tick_font_size = 15,
        legend_font_size = 17,
        img_format = 'png',
        update = T
      ),

      # feature weights plot parameters
      feature_weights = list(
        auto_refresh = F,
        omics = 1,
        factors = 1,
        groups = NULL,
        scale = TRUE,
        abs = FALSE,
        color_palette = 'Spectral',
        reverse_color_palette = T,
        marker_size = 10,
        marker_opacity = 1,
        title_font_size = 17,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_label_font_size = 20,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = 'png',
        groups_list = NULL,
        update = T
      ),

      # Feature top weights parameters
      feature_top_weights = list(
        auto_refresh = F,
        omics = NULL,
        factors = 1,
        nfeatures = 50,
        abs = T,
        scale = T,
        sign = 'all',
        groups = 'sign',
        color_palette = 'Spectral',
        reverse_color_palette = F,
        marker_size = 10,
        marker_opacity = 1,
        title_font_size = 10,
        y_label_font_size = 10,
        y_tick_font_size = 10,
        x_label_font_size = 10,
        x_tick_font_size = 10,
        legend_font_size = 10,
        img_format = 'png',
        groups_list = NULL,
        update = T
      ),

      # MOFA heatmap parameters
      mofa_heatmap = list(
        auto_refresh = F,
        factor = 1,
        omics = NULL,
        features = 50,
        feature_annotations = 'None',
        sample_annotations = 'None',
        imputed = F,
        denoise = F,
        distance_method = 'euclidean',
        clustering_method = 'ward.D2',
        p_minkowski = 2,
        k_clusters_samples = 1,
        k_clusters_features = 1,
        center = F,
        apply_clustering = T,
        color_palette = 'RdYlBu',
        reverse_color_palette = T,
        title_font_size = 20,
        y_label_font_size = 17,
        x_label_font_size = 17,
        x_tick_font_size = 8,
        y_tick_font_size = 8,
        img_format = 'png',
        features_list = NULL,
        update = T
      ),

      # Scatter_plot parameters
      scatter_plot = list(
        auto_refresh = F,
        factor = 1,
        omics = NULL,
        features = 4,
        sign = "all",
        sample_annotations = NULL,
        add_lm = TRUE,
        imputed = FALSE,
        color_palette = 'Turbo',
        reverse_palette = F,
        marker_size = 10,
        marker_opacity = 1,
        show_legend = TRUE,
        img_format = 'png',
        update = T
      )

    ),
    #---------------------------------------------------- Parameter methods ----

    toggle_explained_variance = function() {
      if (self$params$explained_variance$update) {
        self$params$explained_variance$update = F
      } else {
        self$params$explained_variance$update = T
      }
    },

    param_explained_variance = function(auto_refresh, color_palette, reverse_color_palette, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$explained_variance$auto_refresh = auto_refresh
      self$params$explained_variance$color_palette = color_palette
      self$params$explained_variance$reverse_color_palette = reverse_color_palette
      self$params$explained_variance$title_font_size = title_font_size
      self$params$explained_variance$y_label_font_size = y_label_font_size
      self$params$explained_variance$y_tick_font_size = y_tick_font_size
      self$params$explained_variance$x_label_font_size = x_label_font_size
      self$params$explained_variance$x_tick_font_size = x_tick_font_size
      self$params$explained_variance$legend_font_size = legend_font_size
      self$params$explained_variance$img_format = img_format
      self$params$explained_variance$update = T

    },

    toggle_factor_plot = function() {
      if (self$params$factor_plot$update) {
        self$params$factor_plot$update = F
      } else {
        self$params$factor_plot$update = T
      }
    },

    param_factor_plot = function(auto_refresh, factors, scale, groups, show_missing, color_palette, marker_size, opacity, add_violin, show_legend, violin_alpha, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$factor_plot$auto_refresh = auto_refresh
      self$params$factor_plot$factors = factors
      self$params$factor_plot$scale = scale
      self$params$factor_plot$groups = groups
      self$params$factor_plot$show_missing = show_missing
      self$params$factor_plot$color_palette = color_palette
      self$params$factor_plot$marker_size = marker_size
      self$params$factor_plot$opacity = opacity
      self$params$factor_plot$add_violin = add_violin
      self$params$factor_plot$show_legend = show_legend
      self$params$factor_plot$violin_alpha = violin_alpha
      self$params$factor_plot$title_font_size = title_font_size
      self$params$factor_plot$y_label_font_size = y_label_font_size
      self$params$factor_plot$y_tick_font_size = y_tick_font_size
      self$params$factor_plot$x_label_font_size = x_label_font_size
      self$params$factor_plot$x_tick_font_size = x_tick_font_size
      self$params$factor_plot$legend_font_size = legend_font_size
      self$params$factor_plot$img_format = img_format
      self$params$factor_plot$update = T
    },

    toggle_combined_factors_plot = function() {
      if (self$params$combined_factors_plot$update) {
        self$params$combined_factors_plot$update = F
      } else {
        self$params$combined_factors_plot$update = T
      }
    },

    param_combined_factors_plot = function(auto_refresh, factors, groups, scale, show_missing, color_palette, marker_size, marker_opacity, area_alpha, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$combined_factors_plot$auto_refresh = auto_refresh
      self$params$combined_factors_plot$factors = factors
      self$params$combined_factors_plot$groups = groups
      self$params$combined_factors_plot$scale = scale
      self$params$combined_factors_plot$show_missing = show_missing
      self$params$combined_factors_plot$color_palette = color_palette
      self$params$combined_factors_plot$marker_size = marker_size
      self$params$combined_factors_plot$marker_opacity = marker_opacity
      self$params$combined_factors_plot$area_alpha = area_alpha
      self$params$combined_factors_plot$title_font_size = title_font_size
      self$params$combined_factors_plot$y_label_font_size = y_label_font_size
      self$params$combined_factors_plot$y_tick_font_size = y_tick_font_size
      self$params$combined_factors_plot$x_label_font_size = x_label_font_size
      self$params$combined_factors_plot$x_tick_font_size = x_tick_font_size
      self$params$combined_factors_plot$legend_font_size = legend_font_size
      self$params$combined_factors_plot$img_format = img_format
      self$params$combined_factors_plot$update = T
    },

    toggle_feature_weights = function() {
      if (self$params$feature_weights$update) {
        self$params$feature_weights$update = F
      } else {
        self$params$feature_weights$update = T
      }
    },

    param_feature_weights = function(auto_refresh, omics, factors, groups, scale, abs, color_palette, reverse_color_palette, marker_size, marker_opacity, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$feature_weights$auto_refresh = auto_refresh
      self$params$feature_weights$omics = omics
      self$params$feature_weights$factors = factors
      self$params$feature_weights$groups = groups
      self$params$feature_weights$scale = scale
      self$params$feature_weights$abs = abs
      self$params$feature_weights$color_palette = color_palette
      self$params$feature_weights$reverse_color_palette = reverse_color_palette
      self$params$feature_weights$marker_size = marker_size
      self$params$feature_weights$marker_opacity = marker_opacity
      self$params$feature_weights$title_font_size = title_font_size
      self$params$feature_weights$y_label_font_size = y_label_font_size
      self$params$feature_weights$y_tick_font_size = y_tick_font_size
      self$params$feature_weights$x_label_font_size = x_label_font_size
      self$params$feature_weights$x_tick_font_size = x_tick_font_size
      self$params$feature_weights$legend_font_size = legend_font_size
      self$params$feature_weights$img_format = img_format
      self$params$feature_weights$update = T
    },

    toggle_feature_top_weights = function() {
      if (self$params$feature_top_weights$update) {
        self$params$feature_top_weights$update = F
      } else {
        self$params$feature_top_weights$update = T
      }
    },

    param_feature_top_weights = function(auto_refresh, omics, factors, nfeatures, abs, scale, sign, groups, color_palette, reverse_color_palette, marker_size, marker_opacity, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$feature_top_weights$auto_refresh = auto_refresh
      self$params$feature_top_weights$omics = omics
      self$params$feature_top_weights$factors = factors
      self$params$feature_top_weights$nfeatures = nfeatures
      self$params$feature_top_weights$abs = abs
      self$params$feature_top_weights$scale = scale
      self$params$feature_top_weights$sign = sign
      self$params$feature_top_weights$groups = groups
      self$params$feature_top_weights$color_palette = color_palette
      self$params$feature_top_weights$reverse_color_palette = reverse_color_palette
      self$params$feature_top_weights$marker_size = marker_size
      self$params$feature_top_weights$marker_opacity = marker_opacity
      self$params$feature_top_weights$title_font_size = title_font_size
      self$params$feature_top_weights$y_label_font_size = y_label_font_size
      self$params$feature_top_weights$y_tick_font_size = y_tick_font_size
      self$params$feature_top_weights$x_label_font_size = x_label_font_size
      self$params$feature_top_weights$x_tick_font_size = x_tick_font_size
      self$params$feature_top_weights$legend_font_size = legend_font_size
      self$params$feature_top_weights$img_format = img_format
      self$params$feature_top_weights$update = T
    },

    toggle_mofa_heatmap = function() {
      if (self$params$mofa_heatmap$update) {
        self$params$mofa_heatmap$update = F
      } else {
        self$params$mofa_heatmap$update = T
      }
    },

    param_mofa_heatmap = function(auto_refresh, factor, omics, features, feature_annotations, sample_annotations, imputed, denoise, distance_method, clustering_method, p_minkowski, k_clusters_samples, k_clusters_features, center, apply_clustering, color_palette, reverse_color_palette, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, img_format) {
      self$params$mofa_heatmap$auto_refresh = auto_refresh
      self$params$mofa_heatmap$factor = factor
      self$params$mofa_heatmap$omics = omics
      self$params$mofa_heatmap$features = features
      self$params$mofa_heatmap$feature_annotations = feature_annotations
      self$params$mofa_heatmap$sample_annotations = sample_annotations
      self$params$mofa_heatmap$imputed = imputed
      self$params$mofa_heatmap$denoise = denoise
      self$params$mofa_heatmap$distance_method = distance_method
      self$params$mofa_heatmap$clustering_method = clustering_method
      self$params$mofa_heatmap$p_minkowski = p_minkowski
      self$params$mofa_heatmap$k_clusters_samples = k_clusters_samples
      self$params$mofa_heatmap$k_clusters_features = k_clusters_features
      self$params$mofa_heatmap$center = center
      self$params$mofa_heatmap$apply_clustering = apply_clustering
      self$params$mofa_heatmap$color_palette = color_palette
      self$params$mofa_heatmap$reverse_color_palette = reverse_color_palette
      self$params$mofa_heatmap$title_font_size = title_font_size
      self$params$mofa_heatmap$y_label_font_size = y_label_font_size
      self$params$mofa_heatmap$y_tick_font_size = y_tick_font_size
      self$params$mofa_heatmap$x_label_font_size = x_label_font_size
      self$params$mofa_heatmap$x_tick_font_size = x_tick_font_size
      self$params$mofa_heatmap$img_format = img_format
      self$params$mofa_heatmap$update = T
    },

    toggle_scatter_plot = function() {
      if (self$params$scatter_plot$update) {
        self$params$scatter_plot$update = F
      } else {
        self$params$scatter_plot$update = T
      }
    },

    param_scatter_plot = function(auto_refresh, factor, omics, features, sign, sample_annotations, add_lm, imputed, color_palette, reverse_palette, marker_size, marker_opacity, show_legend, img_format) {
      self$params$scatter_plot$auto_refresh = auto_refresh
      self$params$scatter_plot$factor = factor
      self$params$scatter_plot$omics = omics
      self$params$scatter_plot$features = features
      self$params$scatter_plot$sign = sign
      self$params$scatter_plot$sample_annotations = sample_annotations
      self$params$scatter_plot$add_lm = add_lm
      self$params$scatter_plot$imputed = imputed
      self$params$scatter_plot$color_palette = color_palette
      self$params$scatter_plot$reverse_palette = reverse_palette
      self$params$scatter_plot$marker_size = marker_size
      self$params$scatter_plot$marker_opacity = marker_opacity
      self$params$scatter_plot$show_legend = show_legend
      self$params$scatter_plot$img_format = img_format
      self$params$scatter_plot$update = T
    },



    #---------------------------------------------------------------- Plots ----

    plots = list(
      explained_variance = NULL,
      factor_plot = NULL,
      combined_factors_plot = NULL,
      feature_weights = NULL,
      feature_top_weights = NULL,
      mofa_heatmap = NULL,
      scatter_plot = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(
      omics_tables = list(),
      sample_tables = list(),
      feature_tables = list(),
      sample_metadata = NULL,
      explained_variance = NULL,
      combined_factors_plot = NULL,
      feature_weights = NULL,
      feature_top_weights = NULL,
      mofa_heatmap = NULL,
      scatter_plot = NULL
    ),


    #--------------------------------------------------------- MOFA objects ----
    mofa_objects = list(
      pretrained = NULL,
      model = NULL
    ),

    #-------------------------------------------------------- Table methods ----

    add_data = function(name, data_table) {
      colnames(data_table) = paste0(colnames(data_table), '_', name)
      self$tables$omics_tables[[name]] = data_table
    },

    add_sample_meta = function(name, sample_meta) {
      self$tables$sample_tables[[name]] = sample_meta
    },

    add_feature_data = function(name, feature_data) {
      rownames(feature_data) = paste0(rownames(feature_data), '_', name)
      self$tables$feature_tables[[name]] = feature_data
    },

    clean_datasets = function() {
      omics_names = names(self$tables$omics_tables)
      intersected_samples = list()
      sample_counts = c()

      for (name in omics_names) {
        intersected_samples[[name]] = rownames(self$tables$sample_tables[[name]])
        sample_counts = c(sample_counts, nrow(self$tables$sample_tables[[name]]))
      }

      intersected_samples = base::Reduce(intersect, intersected_samples)

      # More than 2 shared samples should be provided
      if (length(intersected_samples) < 3) {
        base::stop('Less than 3 shared samples')
      }

      # Warning for mismatched samples
      if (base::min(sample_counts) != base::max(sample_counts)) {
        base::warning(paste0("Different samples provided: keeping only ", length(intersected_samples)," shared samples."))
      }

      # Filter the tables
      for (name in omics_names) {
        self$tables$omics_tables[[name]] = remove_empty_cols(self$tables$omics_tables[[name]][intersected_samples,])
        self$tables$sample_tables[[name]] = self$tables$sample_tables[[name]][intersected_samples,]
      }

      # Merge the sample annotations
      merged_sample_annotations = self$tables$sample_tables[[1]]
      for (name in omics_names[2:length(omics_names)]) {
        next_table = self$tables$sample_tables[[name]]
        diff = dplyr::setdiff(colnames(next_table), colnames(merged_sample_annotations))
        if (length(diff) > 0) {
          merged_sample_annotations = base::cbind(merged_sample_annotations, next_table[,diff, drop = F])
        }
      }

      self$tables$sample_metadata = merged_sample_annotations

    },

    #-------------------------------------------------------------- Methods ----
    create_mofa_object = function(matrix_list = self$tables$omics_tables,
                                  groups = NULL) {
      if (!is.null(groups)) {
        groups = gsub("/", "-", groups)
      }

      matrix_list = base::lapply(matrix_list, base::t)

      MOFAobject = MOFA2::create_mofa(data = matrix_list,
                                      groups = groups)
      self$params$omics = names(matrix_list)
      self$mofa_objects$pretrained = MOFAobject
    },

    prepare_mofa = function(pretrained = self$mofa_objects$pretrained,
                            scale_views = F,
                            scale_groups = F,
                            center_groups = T,
                            likelihoods = base::rep("gaussian", length(self$params$omics)),
                            num_factors = 15,
                            spikeslab_factors = F,
                            spikeslab_weights = F,
                            ard_factors = F,
                            ard_weights = T,
                            maxiter = 1000,
                            convergence_mode = "fast",
                            startELBO = 1,
                            freqELBO = 5,
                            stochastic = F,
                            weight_views = F) {

      # checks
      if (length(likelihoods) != length(self$params$omics)){
        stop("Not as many likelihoods provided as omics experiments")
      }

      # Retrieve all options
      data_opts = MOFA2::get_default_data_options(pretrained)
      model_opts = MOFA2::get_default_model_options(pretrained)
      train_opts = MOFA2::get_default_training_options(pretrained)

      # Set data options
      data_opts$scale_views = scale_views
      data_opts$scale_groups = scale_groups
      data_opts$center_groups = center_groups

      # Set model options
      model_opts$likelihoods = likelihoods
      model_opts$num_factors = num_factors
      model_opts$spikeslab_factors = spikeslab_factors
      model_opts$spikeslab_weights = spikeslab_weights
      model_opts$ard_factors = ard_factors
      model_opts$ard_weights = ard_weights

      # Set training options
      train_opts$maxiter = maxiter
      train_opts$convergence_mode = convergence_mode
      train_opts$startELBO = startELBO
      train_opts$freqELBO = freqELBO
      train_opts$stochastic = stochastic
      train_opts$weight_views = weight_views

      # Set parameters to the object
      pretrained = MOFA2::prepare_mofa(
        object = pretrained,
        data_options = data_opts,
        model_options = model_opts,
        training_options = train_opts
      )
      self$mofa_objects$pretrained = pretrained
    },

    train_model = function(mofa_object = self$mofa_objects$pretrained,
                           outfile = NULL,
                           save_data = FALSE,
                           seed = 1) {
      # model = MOFA2::run_mofa(object = mofa_object,
      #                         outfile = outfile,
      #                         use_basilisk = F,
      #                         save_data = save_data)
      base::set.seed(seed)
      model = MOFA2::run_mofa(object = mofa_object,
                              outfile = outfile,
                              use_basilisk = T,
                              save_data = save_data)

      factor_list = MOFA2::get_factors(model)
      factor_list = colnames(factor_list$group1)
      factor_list = 1:length(factor_list)

      self$params$factor_list = factor_list

      self$mofa_objects$model = model
    },

    get_views = function(model = self$mofa_objects$pretrained) {
      self$params$views = 1:length(model@data)

    },

    add_metadata_to_mofa = function(model = self$mofa_objects$model,
                                    sample_metadata = self$tables$sample_metadata) {

      sample_metadata['sample'] = rownames(sample_metadata)
      MOFA2::samples_metadata(model) = sample_metadata
      meta_cols = colnames(sample_metadata)

      self$params$sample_metadata = meta_cols
      self$mofa_objects$model = model
    },

    #----------------------------------------------------- Plotting methods ----

    # Plot explained variance
    plot_explained_variance = function(model = self$mofa_objects$model,
                                       color_palette = self$params$explained_variance$color_palette,
                                       reverse_color_palette = self$params$explained_variance$reverse_color_palette,
                                       title_font_size = self$params$explained_variance$title_font_size,
                                       y_label_font_size = self$params$explained_variance$y_label_font_size,
                                       y_tick_font_size = self$params$explained_variance$y_tick_font_size,
                                       x_label_font_size = self$params$explained_variance$x_label_font_size,
                                       x_tick_font_size = self$params$explained_variance$x_tick_font_size,
                                       legend_font_size = self$params$explained_variance$legend_font_size,
                                       width = NULL,
                                       height = NULL) {

      data_table = model@cache$variance_explained$r2_per_factor$group1

      # Sort
      data_table = data_table[base::names(base::sort(base::rowSums(data_table), decreasing = F)),
                              base::names(base::sort(base::colSums(data_table), decreasing = F))]

      plot = plot_mofa_explained_variance(data_table = data_table,
                                          color_palette = color_palette,
                                          reverse_color_palette = reverse_color_palette,
                                          title_font_size = title_font_size,
                                          y_label_font_size = y_label_font_size,
                                          y_tick_font_size = y_tick_font_size,
                                          x_label_font_size = x_label_font_size,
                                          x_tick_font_size = x_tick_font_size,
                                          legend_font_size = legend_font_size,
                                          width = width,
                                          height = height)

      self$plots$explained_variance = plot
      self$tables$explained_variance = data_table
    },

    # Factor plot
    plot_factor_plot = function(model = self$mofa_objects$model,
                                sample_metadata = self$tables$sample_metadata,
                                factors = self$params$factor_plot$factors,
                                scale = self$params$factor_plot$scale,
                                groups = self$params$factor_plot$groups,
                                show_missing = self$params$factor_plot$show_missing,
                                color_palette = self$params$factor_plot$color_palette,
                                marker_size = self$params$factor_plot$marker_size,
                                opacity = self$params$factor_plot$opacity,
                                add_violin = self$params$factor_plot$add_violin,
                                show_legend = self$params$factor_plot$show_legend,
                                violin_alpha = self$params$factor_plot$violin_alpha,
                                title_font_size = self$params$factor_plot$title_font_size,
                                y_label_font_size = self$params$factor_plot$y_label_font_size,
                                y_tick_font_size = self$params$factor_plot$y_tick_font_size,
                                x_label_font_size = self$params$factor_plot$x_label_font_size,
                                x_tick_font_size = self$params$factor_plot$x_tick_font_size,
                                legend_font_size = self$params$factor_plot$legend_font_size,
                                width = NULL,
                                height = NULL) {

      factors = as.numeric(factors)

      output = plot_factor_plot(model = model,
                                sample_metadata = sample_metadata,
                                factors = factors,
                                scale = scale,
                                groups = groups,
                                show_missing = show_missing,
                                color_palette = color_palette,
                                marker_size = marker_size,
                                opacity = opacity,
                                add_violin = add_violin,
                                show_legend = show_legend,
                                violin_alpha = violin_alpha,
                                title_font_size = title_font_size,
                                y_label_font_size = y_label_font_size,
                                y_tick_font_size = y_tick_font_size,
                                x_label_font_size = x_label_font_size,
                                x_tick_font_size = x_tick_font_size,
                                legend_font_size = legend_font_size,
                                width = width,
                                height = height)

      self$plots$factor_plot = output$plot
      self$tables$factor_plot = output$table
    },

    # Combnined factors

    plot_combined_factors_plot = function(model = self$mofa_objects$model,
                                          sample_metadata = self$tables$sample_metadata,
                                          factors = self$params$combined_factors_plot$factors,
                                          groups = self$params$combined_factors_plot$groups,
                                          scale = self$params$combined_factors_plot$scale,
                                          show_missing = self$params$combined_factors_plot$show_missing,
                                          color_palette = self$params$combined_factors_plot$color_palette,
                                          marker_size = self$params$combined_factors_plot$marker_size,
                                          marker_opacity = self$params$combined_factors_plot$marker_opacity,
                                          area_alpha = self$params$combined_factors_plot$area_alpha,
                                          title_font_size = self$params$combined_factors_plot$title_font_size,
                                          y_label_font_size = self$params$combined_factors_plot$y_label_font_size,
                                          y_tick_font_size = self$params$combined_factors_plot$y_tick_font_size,
                                          x_label_font_size = self$params$combined_factors_plot$x_label_font_size,
                                          x_tick_font_size = self$params$combined_factors_plot$x_tick_font_size,
                                          legend_font_size = self$params$combined_factors_plot$legend_font_size,
                                          width = NULL,
                                          height = NULL) {
      factors = as.numeric(factors)

      out = plot_combined_factors_plot(model = model,
                                       factors = factors,
                                       sample_metadata = sample_metadata,
                                       groups = groups,
                                       scale = scale,
                                       show_missing = show_missing,
                                       color_palette = color_palette,
                                       marker_size = marker_size,
                                       marker_opacity = marker_opacity,
                                       area_alpha = area_alpha,
                                       title_font_size = title_font_size,
                                       y_label_font_size = y_label_font_size,
                                       y_tick_font_size = y_tick_font_size,
                                       x_label_font_size = x_label_font_size,
                                       x_tick_font_size = x_tick_font_size,
                                       legend_font_size = legend_font_size,
                                       width = width,
                                       height = height)

      self$plots$combined_factors_plot = out$plot
      self$tables$combined_factors_plot = out$table
    },

    # Feature weights
    plot_feature_weights = function(model = self$mofa_objects$model,
                                    omics = self$params$feature_weights$omics,
                                    factors = self$params$feature_weights$factors,
                                    groups = self$params$feature_weights$groups,
                                    scale = self$params$feature_weights$scale,
                                    abs = self$params$feature_weights$abs,
                                    color_palette = self$params$feature_weights$color_palette,
                                    reverse_color_palette = self$params$feature_weights$reverse_color_palette,
                                    marker_size = self$params$feature_weights$marker_size,
                                    marker_opacity = self$params$feature_weights$marker_opacity,
                                    title_font_size = self$params$feature_weights$title_font_size,
                                    y_label_font_size = self$params$feature_weights$y_label_font_size,
                                    y_tick_font_size = self$params$feature_weights$y_tick_font_size,
                                    x_label_font_size = self$params$feature_weights$x_label_font_size,
                                    x_tick_font_size = self$params$feature_weights$x_tick_font_size,
                                    legend_font_size = self$params$feature_weights$legend_font_size,
                                    width = NULL,
                                    height = NULL) {

      factors = as.numeric(factors)
      feature_metadata = self$tables$feature_tables[[omics]]

      out = plot_feature_weights(model = model,
                                 omics = omics,
                                 feature_metadata = feature_metadata,
                                 factors = factors,
                                 groups = groups,
                                 scale = scale,
                                 abs = abs,
                                 color_palette = color_palette,
                                 reverse_color_palette = reverse_color_palette,
                                 marker_size = marker_size,
                                 marker_opacity = marker_opacity,
                                 title_font_size = title_font_size,
                                 y_label_font_size = y_label_font_size,
                                 y_tick_font_size = y_tick_font_size,
                                 x_label_font_size = x_label_font_size,
                                 x_tick_font_size = x_tick_font_size,
                                 legend_font_size = legend_font_size,
                                 width = width,
                                 height = height)


      self$tables$feature_weights = out$table
      self$plots$feature_weights = out$plot

    },

    # Feature top weights
    plot_feature_top_weights = function(model = self$mofa_objects$model,
                                        omics = self$params$feature_top_weights$omics,
                                        factors = self$params$feature_top_weights$factors,
                                        nfeatures = self$params$feature_top_weights$nfeatures,
                                        abs = self$params$feature_top_weights$abs,
                                        scale = self$params$feature_top_weights$scale,
                                        sign = self$params$feature_top_weights$sign,
                                        groups = self$params$feature_top_weights$groups,
                                        color_palette = self$params$feature_top_weights$color_palette,
                                        reverse_color_palette = self$params$feature_top_weights$reverse_color_palette,
                                        marker_size = self$params$feature_top_weights$marker_size,
                                        marker_opacity = self$params$feature_top_weights$marker_opacity,
                                        title_font_size = self$params$feature_top_weights$title_font_size,
                                        y_label_font_size = self$params$feature_top_weights$y_label_font_size,
                                        y_tick_font_size = self$params$feature_top_weights$y_tick_font_size,
                                        x_label_font_size = self$params$feature_top_weights$x_label_font_size,
                                        x_tick_font_size = self$params$feature_top_weights$x_tick_font_size,
                                        legend_font_size = self$params$feature_top_weights$legend_font_size,
                                        width = NULL,
                                        height = NULL) {

      factors = as.numeric(factors)
      feature_table = self$tables$feature_tables[[omics]]
      rownames(feature_table) = stringr::str_replace(rownames(feature_table), paste0('_', omics), '')

      out = plot_feature_top_weights(model = model,
                                     omics = omics,
                                     feature_table = feature_table,
                                     factors = factors,
                                     nfeatures = nfeatures,
                                     abs = abs,
                                     scale = scale,
                                     sign = sign,
                                     groups = groups,
                                     color_palette = color_palette,
                                     reverse_color_palette = reverse_color_palette,
                                     marker_size = marker_size,
                                     marker_opacity = marker_opacity,
                                     title_font_size = title_font_size,
                                     y_label_font_size = y_label_font_size,
                                     y_tick_font_size = y_tick_font_size,
                                     x_label_font_size = x_label_font_size,
                                     x_tick_font_size = x_tick_font_size,
                                     legend_font_size = legend_font_size,
                                     width = width,
                                     height = height)


      self$plots$feature_top_weights = out$plot
      self$tables$feature_top_weights = out$table
    },

    # MOFA Heatmap
    plot_mofa_heatmap = function(model = self$mofa_objects$model,
                                 factor = self$params$mofa_heatmap$factor,
                                 omics = self$params$mofa_heatmap$omics,
                                 features = self$params$mofa_heatmap$features,
                                 feature_annotations = self$params$mofa_heatmap$feature_annotations,
                                 sample_annotations = self$params$mofa_heatmap$sample_annotations,
                                 imputed = self$params$mofa_heatmap$imputed,
                                 denoise = self$params$mofa_heatmap$denoise,
                                 distance_method = self$params$mofa_heatmap$distance_method,
                                 clustering_method = self$params$mofa_heatmap$clustering_method,
                                 p_minkowski = self$params$mofa_heatmap$p_minkowski,
                                 k_clusters_samples = self$params$mofa_heatmap$k_clusters_samples,
                                 k_clusters_features = self$params$mofa_heatmap$k_clusters_features,
                                 center = self$params$mofa_heatmap$center,
                                 apply_clustering = self$params$mofa_heatmap$apply_clustering,
                                 color_palette = self$params$mofa_heatmap$color_palette,
                                 reverse_color_palette = self$params$mofa_heatmap$reverse_color_palette,
                                 title_font_size = self$params$mofa_heatmap$title_font_size,
                                 y_label_font_size = self$params$mofa_heatmap$y_label_font_size,
                                 y_tick_font_size = self$params$mofa_heatmap$y_tick_font_size,
                                 x_label_font_size = self$params$mofa_heatmap$x_label_font_size,
                                 x_tick_font_size = self$params$mofa_heatmap$x_tick_font_size,
                                 width = NULL,
                                 height = NULL) {


      factor = as.numeric(factor)
      features = as.numeric(features)

      samples_annotation_table = self$tables$sample_tables[[omics]]
      features_annotation_table = self$tables$feature_tables[[omics]]

      out_data = plot_mofa_heatmap(model = self$mofa_objects$model,
                                   omics = omics,
                                   samples_annotation_table = samples_annotation_table,
                                   features_annotation_table = features_annotation_table,
                                   factor = factor,
                                   features = features,
                                   feature_annotations = feature_annotations,
                                   sample_annotations = sample_annotations,
                                   imputed = imputed,
                                   denoise = denoise,
                                   distance_method = distance_method,
                                   clustering_method = clustering_method,
                                   p_minkowski = p_minkowski,
                                   k_clusters_samples = k_clusters_samples,
                                   k_clusters_features = k_clusters_features,
                                   center = center,
                                   apply_clustering = apply_clustering,
                                   color_palette = color_palette,
                                   reverse_color_palette = reverse_color_palette,
                                   title_font_size = title_font_size,
                                   y_label_font_size = y_label_font_size,
                                   y_tick_font_size = y_tick_font_size,
                                   x_label_font_size = x_label_font_size,
                                   x_tick_font_size = x_tick_font_size,
                                   width = width,
                                   height = height)


      self$plots$mofa_heatmap = out_data$plot
      self$tables$mofa_heatmap = out_data$table

    },

    # Scatter plot
    plot_scatter_plot = function(model = self$mofa_objects$model,
                                 factor = self$params$scatter_plot$factor,
                                 omics = self$params$scatter_plot$omics,
                                 features = self$params$scatter_plot$features,
                                 sign = self$params$scatter_plot$sign,
                                 sample_annotations = self$params$scatter_plot$sample_annotations,
                                 add_lm = self$params$scatter_plot$add_lm,
                                 imputed = self$params$scatter_plot$imputed,
                                 color_palette = self$params$scatter_plot$color_palette,
                                 reverse_palette = self$params$scatter_plot$reverse_palette,
                                 marker_size = self$params$scatter_plot$marker_size,
                                 marker_opacity = self$params$scatter_plot$marker_opacity,
                                 show_legend = self$params$scatter_plot$show_legend,
                                 width = NULL,
                                 height = NULL) {

      factor = as.numeric(factor)
      features = as.numeric(features)

      out_data = plot_mofa_scatter_plot(model = model,
                                        factor = factor,
                                        omics = omics,
                                        features = features,
                                        sign = sign,
                                        sample_annotations = sample_annotations,
                                        show_legend = show_legend,
                                        marker_size = marker_size,
                                        marker_opacity = marker_opacity,
                                        add_lm = add_lm,
                                        imputed = imputed,
                                        fixed_axes = F,
                                        color_palette = color_palette,
                                        reverse_palette = reverse_palette,
                                        width = width,
                                        height = height)

      self$plots$scatter_plot = out_data$plot
      self$tables$scatter_plot = out_data$table
    }



  )
)