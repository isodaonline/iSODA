#--------------------------------------------------- Omics experiment class ----
Omics_exp = R6::R6Class(
  "Omics_exp",
  public = list(
    initialize = function(name, type = NA, id = NA, slot = NA, version = NA, param_file = NULL){
      self$name = name
      self$type = type
      self$id = id
      self$slot = slot
      self$version = version

      if (!is.null(param_file)) {
        if (file.exists(param_file)){
          tryCatch({
            params = base::dget(param_file)
            # Regular params
            for (p1 in names(params$params)) {
              if (p1 %in% names(self$params)){
                for (p2 in names(params$params[[p1]])){
                  if (p2 %in% names(self$params[[p1]])){
                    self$params[[p1]][[p2]] = params$params[[p1]][[p2]]
                  }
                }
              }
            }

            # Settings
            for (p1 in names(params$hardcoded_settings)) {
              if (p1 %in% names(self$hardcoded_settings)){
                for (p2 in names(params$hardcoded_settings[[p1]])) {
                  if (p2 %in% names(self$hardcoded_settings[[p1]])) {
                    self$hardcoded_settings[[p1]][[p2]] = params$hardcoded_settings[[p1]][[p2]]
                  }
                }
              }
            }
          },
          error = function(e) {
            message("Error in reading the parameter file, default parameters kept: ", e$message)
          })
        }
      }
    },
    #--------------------------------------------------------------- Global ----
    name = NA,
    id = NA,
    slot = NA,
    type = NA,
    version = NA,
    owner = NA,
    comment = NA,

    #----------------------------------------------------------- Parameters ----
    params = list(
      
      # Feature signal filtering
      measurement_filter = list(
        operation_order = c('Imputation', 'Batch correction', 'Filtering'),
        batch_effect_correction = "None",
        imputation_method = "None",
        blank_multiplier = 2,
        sample_threshold = 0.8,
        group_threshold = 0.8,
        norm_col = "None"
      ),

      # Dendrogram parameters
      dendrogram = list(
        auto_refresh = F,
        dataset = 'Z-scored table',
        annotations = NULL,
        distance_method = "euclidean",
        p = 2,
        clustering_method = "ward.D2",
        k_clusters = 1,
        color_palette = "Set1",
        x_tick_font_size = 0,
        y_label_font_size = 12,
        y_tick_font_size = 15,
        img_format = "png",
        update = T
      ),

      # Class distribution parameters
      class_distribution = list(
        auto_refresh = F,
        dataset = 'Class table total normalized',
        group_col = NULL,
        color_palette = 'Set1',
        title_font_size = 0,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_label_font_size = 0,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = "png",
        update = T
      ),

      # Class comparison parameters
      class_comparison = list(
        auto_refresh = F,
        dataset = 'Class table total normalized',
        group_col = NULL,
        color_palette = 'Set1',
        title_font_size = 16,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = "png",
        update = T
      ),

      volcano_plot_comparison = list(
        data_table = 'Total normalized table',
        group_col = NULL,
        group_1 = NULL,
        group_2 = NULL,
        fc_function = 'mean',
        statistical_test = 't-Test',
        adjustment_method = 'none',
        update = T
      ),

      # Volcano plot parameters self$params$volcano_plot$
      volcano_plot = list(
        auto_refresh = T,
        feature_metadata = 'None',
        keep_significant = F,
        displayed_plot = 'main',
        p_val_threshold = 0.05,
        fc_threshold = 2,
        marker_size = 10,
        opacity = 1,
        color_palette = 'Set1',
        reverse_palette = F,
        title_font_size = 16,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_label_font_size = 20,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = "png",
        update = T
      ),

      # Heatmap parameters self$params$heatmap$
      heatmap = list(
        auto_refresh = F,
        dataset = 'Z-scored total normalized table',
        distance_method = "euclidian",
        clustering_method = "ward.D2",
        impute_min = T,
        center = F,
        apply_clustering = T,
        k_clusters_samples = 1,
        k_clusters_features = 1,
        map_sample_data = NULL,
        map_feature_data = NULL,
        map_feature_terms = NULL,
        multival_cols = 'None',
        group_column_da = NULL,
        apply_da = T,
        alpha_da = 0.8,
        seed_da = 1,
        color_palette = 'RdYlBu',
        reverse_palette = T,
        title_font_size = 0,
        y_label_font_size = 17,
        x_label_font_size = 17,
        x_tick_font_size = 0,
        y_tick_font_size = 0,
        img_format = "png",
        update = T
      ),

      # samples correlation parameters self$params$samples_correlation$
      samples_correlation = list(
        auto_refresh = F,
        dataset = 'Z-scored total normalized table',
        correlation_method = "pearson",
        use = 'pairwise.complete.obs',
        distance_method = "euclidian",
        clustering_method = "ward.D2",
        k_clusters = 1,
        apply_clustering = T,
        center = F,
        row_annotations = 'Group_type',
        col_annotations = 'Group_type',
        color_palette = 'RdYlBu',
        reverse_palette = T,
        title_font_size = 0,
        y_label_font_size = 0,
        y_tick_font_size = 0,
        x_label_font_size = 0,
        x_tick_font_size = 0,
        img_format = "png",
        update = T
      ),

      # feature_correlation parameters self$params$feature_correlation$
      feature_correlation = list(
        auto_refresh = F,
        dataset = 'Z-scored total normalized table',
        multival_cols = 'None',
        map_feature_terms = NULL,
        correlation_method = "pearson",
        use = 'pairwise.complete.obs',
        distance_method = "euclidian",
        clustering_method = "ward.D2",
        k_clusters = 1,
        apply_clustering = T,
        center = F,
        row_annotations = NULL,
        col_annotations = NULL,
        roh_threshold = 0.95,
        top_features = 400,
        color_palette = 'RdYlBu',
        reverse_palette = T,
        title_font_size = 0,
        y_label_font_size = 0,
        y_tick_font_size = 0,
        x_label_font_size = 0,
        x_tick_font_size = 0,
        img_format = "png",
        update = T
      ),

      # PCA parameters self$params$pca$
      pca = list(
        auto_refresh = F,
        data_table = 'Z-scored total normalized table',
        sample_groups_col = NULL,
        feature_groups_col = NULL,
        impute_median = FALSE,
        apply_da = FALSE,
        sample_groups_da = NULL,
        alpha_da = 0.8,
        seed_da = 1,
        pca_method = 'nipals',
        nPcs = 2,
        displayed_pc_1 = 1,
        displayed_pc_2 = 2,
        completeObs = F,
        displayed_plots = 'scores',
        colors_palette = 'Set1',
        marker_size = 10,
        opacity = 1,
        title_font_size = 16,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_label_font_size = 20,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = "png",
        update = T
      ),

      # Fatty acid analysis parameters self$params$fa_analysis_plot$
      fa_analysis_plot = list(
        auto_refresh = F,
        data_table = "Total normalized table",
        feature_meta = NULL,
        sample_meta = "Raw meta table",
        group_col = NULL,
        selected_view = "lipidclass",
        selected_lipidclass = "All",
        fa_norm = FALSE,
        color_palette = "Set1",
        title_font_size = NULL,
        y_label_font_size = NULL,
        y_tick_font_size = NULL,
        x_label_font_size = NULL,
        x_tick_font_size = NULL,
        legend_font_size = NULL,
        img_format = "png",
        update = T
      ),

      # Fatty acid analysis heatmap self$params$fa_comp_plot$
      fa_comp_plot = list(
        auto_refresh = F,
        data_table = "Total normalized table",
        composition = "fa_tail",
        group_col = NULL,
        group_1 = NULL,
        group_2 = NULL,
        selected_lipidclass = "CE",
        color_palette = "Blues",
        reverse_palette = F,
        title_font_size = NULL,
        y_label_font_size = NULL,
        y_tick_font_size = NULL,
        x_label_font_size = NULL,
        x_tick_font_size = NULL,
        legend_font_size = NULL,
        img_format = "png",
        update = T
      ),

      double_bonds_comparison = list(
        data_table = "Total normalized table",
        group_col = NULL,
        group_1 = NULL,
        group_2 = NULL,
        fc_function = 'mean',
        statistical_test = 't-Test',
        adjustment_method = 'none'
      ),

      double_bonds_plot = list(
        auto_refresh = F,
        carbon_selection = "Carbon count (chain 1)",
        unsat_selection = "Double bonds (chain 1)",
        lipid_class = "CE",
        min_fc = 0,
        max_pval = 1,
        remove_missing_pval = T,
        remove_infitive_fc = T,
        color_palette = 'RdBu',
        reverse_palette = T,
        marker_size = 1,
        marker_opacity = 0.5,
        title_font_size = 16,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_label_font_size = 20,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = "png",
        update = T
      ),

      ea_selection = list(
        table = "Total normalized table",
        group_col = NULL,
        group_1 = NULL,
        group_2 = NULL,
        fc_function = "mean",
        statistical_test = "t-Test",
        adjustment_method = "none"
      ),

      ea_process = list(
        custom_col = NULL,
        selected_features = NULL,
        ont = "ALL",
        minGSSize = 3,
        maxGSSize = 800,
        terms_p_value_cutoff = 0.05,
        terms_pAdjustMethod = "none",
        verbose = TRUE,
        OrgDb = "org.Hs.eg.db",
        seed = 1
      ),

      ora_selection = list(
        group_col = NULL,
        group_1 = NULL,
        group_2 = NULL,
        fc_function = "mean",
        statistical_test = "t-Test",
        adjustment_method = "none"
      ),


      ora_process = list(
        custom_col = NULL,
        selected_features = NULL,
        pval_cutoff_features = 0.05,
        padjust_features = "none",
        pval_cutoff = 0.05,
        pAdjustMethod = "none",
        fc_threshold = 2,
        ont = "ALL",
        qval_cutoff = 0.05,
        minGSSize = 10,
        maxGSSize  = 500,
        seed = 1
      ),

      # Dot plot parameters self$params$ea_dot_plot
      ea_dot_plot = list(
        auto_refresh = F,
        x = "GeneRatio",
        y = "ID",
        color = "p.adjust",
        show_categories = 10,
        size = "GeneCount",
        order_by = "GeneRatio",
        reverse_order = F,
        mode = "Both",
        marker_opacity = 0.5,
        color_palette = "RdYlBu",
        reverse_palette = F,
        show_legend = T,
        legend_size = 14,
        size_ref = 3,
        yaxis_word_split = 0,
        title_size = 20,
        xlabel_size = 15,
        xtick_size = 15,
        ytick_size = 15,
        img_format = "png",
        update = T
      ),

      # Dot plot parameters self$params$ora_dot_plot
      ora_dot_plot = list(
        auto_refresh = F,
        x = "GeneRatio",
        y = "ID",
        color = "p.adjust",
        show_categories = 10,
        size = "GeneCount",
        order_by = "GeneRatio",
        reverse_order = F,
        marker_opacity = 0.5,
        color_palette = "RdYlBu",
        reverse_palette = F,
        show_legend = T,
        size_ref = 0.1,
        legend_size = 14,
        yaxis_word_split = 0,
        title_size = 25,
        xlabel_size = 15,
        xtick_size = 15,
        ytick_size = 15,
        img_format = "png",
        update = T
      ),

      # Ridge plot parameters self$params$ridge_plot
      ea_ridge_plot = list(
        auto_refresh = F,
        show_category = 50,
        fill="p.adjust",
        core_enrichment = TRUE,
        color_palette = "Spectral",
        reverse_palette = FALSE,
        displayed_label = "ID",
        orderBy = "NES",
        decreasing = FALSE,
        title_font_size = 0,
        yaxis_word_split = 0,
        y_label_font_size = 0,
        y_tick_font_size = 13,
        x_label_font_size = 18,
        x_tick_font_size = 15,
        legend_font_size = 12,
        img_format = "png",
        update = T
      ),

      # CNET plot parameters self$params$ea_cnet_plot
      ea_cnet_plot = list(
        auto_refresh = F,
        show_category = 10,
        displayed_labels = 'ID' ,
        set_node_annotations = "None",
        feature_node_annotations = "Log2(fold change)",
        set_node_color_palette = "Purples",
        reverse_set_palette = F,
        feature_node_color_palette = "RdYlBu",
        reverse_feature_palette = T,
        label_font_size = 40,
        static_network = T,
        solver = "barnesHut",
        gravitationalConstant = -10000 ,
        nodeDistance = 500,
        centralGravity = 0.3,
        springLength = 800,
        springConstant = 0.01,
        img_format = "png",
        update = T
      ),

      # Over representation CNET plot parameters self$params$ora_cnet_plot
      ora_cnet_plot = list(
        auto_refresh = F,
        show_category = 10,
        displayed_labels = 'ID' ,
        set_node_annotations = "None",
        feature_node_annotations = "Log2(fold change)",
        set_node_color_palette = "Purples",
        reverse_set_palette = F,
        feature_node_color_palette = "RdYlBu",
        reverse_feature_palette = T,
        label_font_size = 40,
        static_network = T,
        solver = "barnesHut",
        gravitationalConstant = -10000 ,
        nodeDistance = 500,
        centralGravity = 0.3,
        springLength = 800,
        springConstant = 0.01,
        img_format = "png",
        update = T
      ),


      # ea emap plot parameters self$params$ea_emap_plot
      ea_emap_plot = list(
        auto_refresh = F,
        show_category = 50,
        color = "p.adjust",
        size = "GeneCount",
        displayed_labels = 'ID and Description',
        mode = "Both",
        score_threshold = 0.2,
        similarity_score = "JC",
        node_color_palette = "RdYlBu",
        reverse_node_palette = F,
        edge_width = "Similarity score",
        edge_color = "Similarity score",
        edge_color_palette = "Blues",
        reverse_edge_palette = F,
        edge_magnifier = 5,
        node_magnifier = 0.2,
        label_font_size = 40,
        static_network = F,
        solver = "repulsion",
        gravitationalConstant = -8000,
        nodeDistance = 100,
        centralGravity = 0.2,
        springLength = 200,
        springConstant = 0.05,
        img_format = "png",
        update = T
      ),

      # ora emap plot parameters self$params$ora_emap_plot
      ora_emap_plot = list(
        auto_refresh = F,
        show_category = 50,
        color = "p.adjust",
        size = "GeneCount",
        displayed_labels = 'ID and Description',
        score_threshold = 0.2,
        similarity_score = "JC",
        node_color_palette = "RdYlBu",
        reverse_node_palette = F,
        edge_width = "Similarity score",
        edge_color = "Similarity score",
        edge_color_palette = "Blues",
        reverse_edge_palette = F,
        edge_magnifier = 5,
        node_magnifier = 2,
        label_font_size = 40,
        static_network = F,
        solver = "repulsion",
        gravitationalConstant = -8000,
        nodeDistance = 100,
        centralGravity = 0.2,
        springLength = 200,
        springConstant = 0.05,
        img_format = "png",
        update = T
      ),

      # Over representation bar plot parameters self$params$ora_bar_plot
      ora_bar_plot = list(
        auto_refresh = F,
        x = "GeneRatio",
        color = "p.adjust",
        show_category = 40,
        displayed_label = "Description",
        order_by = "GeneRatio",
        order_decreasing = F,
        color_palette = 'RdYlBu',
        reverse_palette = F,
        title_font_size = 0,
        yaxis_word_split = 0,
        y_label_font_size = 0,
        y_tick_font_size = 13,
        x_label_font_size = 15,
        x_tick_font_size = 13,
        legend_font_size = 10,
        img_format = "png",
        update = T
      )

    ),

    hardcoded_settings = list(

      dendrogram = list(
        datasets = list(
          'Z-scored table',
          'Z-scored total normalized table'
        )
      ),
      volcano_plot = list(
        datasets = list(
          "Raw data table",
          "Class normalized table",
          "Total normalized table"
        )
      ),
      heatmap = list(
        datasets = list(
          'Z-scored table',
          'Z-scored total normalized table',
          'Class table z-scored'
        )
      ),
      samples_correlation = list(
        datasets = list(
          'Z-scored table',
          'Z-scored total normalized table'
        )
      ),
      feature_correlation = list(
        datasets = list(
          "Raw data table",
          "Total normalized table",
          'Z-scored table',
          'Z-scored total normalized table'
        )
      ),

      pca = list(
        datasets = list(
          'Z-scored table',
          'Z-scored total normalized table'
        )
      ),
      fa_analysis_plot = list(
        datasets = list(
          "Raw data table",
          "Class normalized table",
          "Total normalized table"
        )
      ),
      fa_comp_plot = list(
        datasets = list(
          "Raw data table",
          "Class normalized table",
          "Total normalized table"
        )
      ),
      double_bonds_plot = list(
        datasets = list(
          "Raw data table",
          "Class normalized table",
          "Total normalized table"
        )
      ),

      enrichment_analysis = list(
        terms = c('Gene ontology (ALL)',
                  'Gene ontology (BP)',
                  'Gene ontology (MF)',
                  'Gene ontology (CC)')
      ),
      over_representation_analysis = list(
        terms = c('Gene ontology (ALL)',
                  'Gene ontology (BP)',
                  'Gene ontology (MF)',
                  'Gene ontology (CC)')
      )
    ),



    #--------------------------------------------------------------- Indices ----

    indices = list(
      id_col_meta = NA,
      id_col_data = NA,
      id_col_feat = NA,

      type_column = NA,
      group_column = NA,
      batch_column = NA,

      index_blanks = NULL,
      index_qcs = NULL,
      index_pools = NULL,
      index_samples = NULL,

      excluded_samples = NULL,
      excluded_features = NULL,
      filtered_out_features = NULL,

      feature_id_type = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(

      # Metadata tables
      imp_meta = NULL,
      indexed_meta = NULL,
      raw_meta = NULL,

      # Data tables
      imp_data = NULL,
      indexed_data = NULL,
      raw_data = NULL,
      
      # Feature tables
      imp_feat = NULL,
      indexed_feat = NULL,
      raw_feat = NULL,

      # Non sample tables
      blank_table = NULL,
      qc_table = NULL,
      pool_table = NULL,

      #Feature tables
      feature_table = NULL,
      feature_list = NULL,

      # External feature tables
      external_feature_tables = list(),

      # External feature tables
      external_enrichment_tables = list(),

      # Group summaries
      summary_species_table = NULL,
      summary_class_table = NULL,

      # Normalised
      class_norm_data = NULL,
      total_norm_data = NULL,

      # Z-scored
      z_scored_data = NULL,
      z_scored_class_norm_data = NULL,
      z_scored_total_norm_data = NULL,

      # class tables
      class_table= NULL,
      class_table_z_scored = NULL,
      class_table_total_norm = NULL,
      class_table_z_scored_total_norm = NULL,

      # Plot tables
      dendrogram = NULL,
      class_distribution_table = NULL,
      volcano_table = NULL,
      heatmap = NULL,
      samples_correlation = NULL,
      samples_correlation_clusters = NULL,
      feature_correlation = NULL,
      feature_correlation_clusters = NULL,
      pca_scores_table = NULL,
      pca_loadings_table = NULL,
      dbplot_table = NULL,

      # GSEA & over representation
      ea_feature_table = NULL,
      ea_object = NULL,
      ea_table = NULL,
      
      ora_feature_table = NULL,
      ora_object = NULL,
      ora_table = NULL,
      
      # Functional analysis tables
      ea_dot_plot = NULL,
      ora_dot_plot = NULL,
      ea_ridge_plot = NULL,
      ora_bar_plot = NULL


    ),

    #-------------------------------------------------------------- Local table

    table_switch_local = function(table_name) {
      table_switch(
        table_name = table_name,
        r6 = self
      )
    },

    table_check_convert = function(table) {
      if (length(table) == 1) {
        if (is.character(table)){
          table = self$table_switch_local(table)
        }
      }
      return(table)
    },

    #---------------------------------------------------------------- Plots ----
    plots = list(

      # Preview plots
      sample_type_distribution = NULL,
      sample_group_distribution = NULL,
      
      # Interactive visualization
      dendrogram = NULL,
      class_distribution = NULL,
      class_comparison = NULL,
      volcano_plot = NULL,
      heatmap = NULL,
      samples_correlation = NULL,
      feature_correlation = NULL,
      pca_plot = NULL,
      fa_analysis_plot = NULL,
      fa_comp_plot = NULL,
      double_bond_plot = NULL,

      # Functional analysis plots
      ea_dot_plot = NULL,
      ora_dot_plot = NULL,
      ea_ridge_plot = NULL,

      ridgeplot = NULL,
      ea_emap_plot = NULL,
      cnetplot = NULL,
      ora_emap_plot = NULL,
      or_cnetplot = NULL,
      ora_bar_plot = NULL
    ),

    #---------------------------------------------------- Parameter methods ----

    export_params= function(file_name) {
      parameter_file = list(params = self$params,
                            hardcoded_settings = self$hardcoded_settings)
      base::dput(parameter_file, file = file_name)
    },
    
    param_measurement_filter = function(operation_order, batch_effect_correction, imputation_method, blank_multiplier, sample_threshold, group_threshold, norm_col) {
      self$params$measurement_filter$operation_order = operation_order
      self$params$measurement_filter$batch_effect_correction = batch_effect_correction
      self$params$measurement_filter$imputation_method = imputation_method
      self$params$measurement_filter$blank_multiplier = blank_multiplier
      self$params$measurement_filter$sample_threshold = sample_threshold
      self$params$measurement_filter$group_threshold = group_threshold
      self$params$measurement_filter$norm_col = norm_col
    },
    
    toggle_dendrogram = function() {
      if (self$params$dendrogram$update) {
        self$params$dendrogram$update = F
      } else {
        self$params$dendrogram$update = T
      }
    },

    param_dendrogram = function(auto_refresh, dataset, annotations, distance_method, p, clustering_method, k_clusters, color_palette, x_tick_font_size, y_label_font_size, y_tick_font_size, img_format) {
      self$params$dendrogram$auto_refresh = auto_refresh
      self$params$dendrogram$dataset = dataset
      self$params$dendrogram$annotations = annotations
      self$params$dendrogram$distance_method = distance_method
      self$params$dendrogram$p = p
      self$params$dendrogram$clustering_method = clustering_method
      self$params$dendrogram$k_clusters = k_clusters
      self$params$dendrogram$color_palette = color_palette
      self$params$dendrogram$x_tick_font_size = x_tick_font_size
      self$params$dendrogram$y_label_font_size = y_label_font_size
      self$params$dendrogram$y_tick_font_size = y_tick_font_size
      self$params$dendrogram$img_format = img_format
      self$params$dendrogram$update = T
    },

    toggle_class_distribution = function() {
      if (self$params$class_distribution$update) {
        self$params$class_distribution$update = F
      } else {
        self$params$class_distribution$update = T
      }
    },

    param_class_distribution = function(auto_refresh, dataset, group_col, color_palette, title_font_size, y_label_font_size, y_tick_font_size, x_label_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$class_distribution$auto_refresh = auto_refresh
      self$params$class_distribution$dataset = dataset
      self$params$class_distribution$group_col = group_col
      self$params$class_distribution$color_palette = color_palette
      self$params$class_distribution$title_font_size = title_font_size
      self$params$class_distribution$y_label_font_size = y_label_font_size
      self$params$class_distribution$y_tick_font_size = y_tick_font_size
      self$params$class_distribution$x_label_font_size = x_label_font_size
      self$params$class_distribution$x_tick_font_size = x_tick_font_size
      self$params$class_distribution$legend_font_size = legend_font_size
      self$params$class_distribution$img_format = img_format
      self$params$class_distribution$update = T
    },

    toggle_class_comparison = function() {
      if (self$params$class_comparison$update) {
        self$params$class_comparison$update = F
      } else {
        self$params$class_comparison$update = T
      }
    },

    param_class_comparison = function(auto_refresh, dataset, group_col, color_palette, title_font_size, y_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$class_comparison$auto_refresh = auto_refresh
      self$params$class_comparison$dataset = dataset
      self$params$class_comparison$group_col = group_col
      self$params$class_comparison$color_palette = color_palette
      self$params$class_comparison$title_font_size = title_font_size
      self$params$class_comparison$y_label_font_size = y_label_font_size
      self$params$class_comparison$y_tick_font_size = y_tick_font_size
      self$params$class_comparison$x_tick_font_size = x_tick_font_size
      self$params$class_comparison$legend_font_size = legend_font_size
      self$params$class_comparison$img_format = img_format
      self$params$class_comparison$update = T
    },

    param_volcano_plot_comparison = function(data_table, group_col, group_1, group_2, fc_function, statistical_test, adjustment_method) {
      self$params$volcano_plot_comparison$data_table = data_table
      self$params$volcano_plot_comparison$group_col = group_col
      self$params$volcano_plot_comparison$group_1 = group_1
      self$params$volcano_plot_comparison$group_2 = group_2
      self$params$volcano_plot_comparison$fc_function = fc_function
      self$params$volcano_plot_comparison$statistical_test = statistical_test
      self$params$volcano_plot_comparison$adjustment_method = adjustment_method
    },

    toggle_volcano_plot = function() {
      if (self$params$volcano_plot$update) {
        self$params$volcano_plot$update = F
      } else {
        self$params$volcano_plot$update = T
      }
    },

    param_volcano_plot = function(auto_refresh, feature_metadata, keep_significant, displayed_plot, p_val_threshold, fc_threshold, marker_size, opacity, color_palette, reverse_palette, title_font_size, y_label_font_size, y_tick_font_size, x_label_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$volcano_plot$auto_refresh = auto_refresh
      self$params$volcano_plot$feature_metadata = feature_metadata
      self$params$volcano_plot$keep_significant = keep_significant
      self$params$volcano_plot$displayed_plot = displayed_plot
      self$params$volcano_plot$p_val_threshold = p_val_threshold
      self$params$volcano_plot$fc_threshold = fc_threshold
      self$params$volcano_plot$marker_size = marker_size
      self$params$volcano_plot$opacity = opacity
      self$params$volcano_plot$color_palette = color_palette
      self$params$volcano_plot$reverse_palette = reverse_palette
      self$params$volcano_plot$title_font_size = title_font_size
      self$params$volcano_plot$y_label_font_size = y_label_font_size
      self$params$volcano_plot$y_tick_font_size = y_tick_font_size
      self$params$volcano_plot$x_label_font_size = x_label_font_size
      self$params$volcano_plot$x_tick_font_size = x_tick_font_size
      self$params$volcano_plot$legend_font_size = legend_font_size
      self$params$volcano_plot$img_format = img_format
      self$params$volcano_plot$update = T
    },

    toggle_heatmap = function() {
      if (self$params$heatmap$update) {
        self$params$heatmap$update = F
      } else {
        self$params$heatmap$update = T
      }
    },

    param_heatmap = function(auto_refresh, dataset, distance_method, clustering_method, impute_min, center, apply_clustering, k_clusters_samples, k_clusters_features, map_sample_data, map_feature_data, map_feature_terms, multival_cols, group_column_da, apply_da, alpha_da, seed_da, color_palette, reverse_palette, title_font_size, y_label_font_size, x_label_font_size, x_tick_font_size, y_tick_font_size, img_format) {
      self$params$heatmap$auto_refresh = auto_refresh
      self$params$heatmap$dataset = dataset
      self$params$heatmap$distance_method = distance_method
      self$params$heatmap$clustering_method = clustering_method
      self$params$heatmap$impute_min = impute_min
      self$params$heatmap$center = center
      self$params$heatmap$apply_clustering = apply_clustering
      self$params$heatmap$k_clusters_samples = k_clusters_samples
      self$params$heatmap$k_clusters_features = k_clusters_features
      self$params$heatmap$map_sample_data = map_sample_data
      self$params$heatmap$map_feature_data = map_feature_data
      self$params$heatmap$map_feature_terms = map_feature_terms
      self$params$heatmap$multival_cols = multival_cols
      self$params$heatmap$group_column_da = group_column_da
      self$params$heatmap$apply_da = apply_da
      self$params$heatmap$alpha_da = alpha_da
      self$params$heatmap$seed_da = seed_da
      self$params$heatmap$color_palette = color_palette
      self$params$heatmap$reverse_palette = reverse_palette
      self$params$heatmap$title_font_size = title_font_size
      self$params$heatmap$y_label_font_size = y_label_font_size
      self$params$heatmap$x_label_font_size = x_label_font_size
      self$params$heatmap$x_tick_font_size = x_tick_font_size
      self$params$heatmap$y_tick_font_size = y_tick_font_size
      self$params$heatmap$img_format = img_format
      self$params$heatmap$update = T
    },

    toggle_samples_correlation = function() {
      if (self$params$samples_correlation$update) {
        self$params$samples_correlation$update = F
      } else {
        self$params$samples_correlation$update = T
      }
    },

    param_samples_correlation = function(auto_refresh, dataset, correlation_method, use, distance_method, clustering_method, k_clusters, apply_clustering, center, row_annotations, col_annotations, color_palette, reverse_palette, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, img_format) {
      self$params$samples_correlation$auto_refresh = auto_refresh
      self$params$samples_correlation$dataset = dataset
      self$params$samples_correlation$correlation_method = correlation_method
      self$params$samples_correlation$use = use
      self$params$samples_correlation$distance_method = distance_method
      self$params$samples_correlation$clustering_method = clustering_method
      self$params$samples_correlation$k_clusters = k_clusters
      self$params$samples_correlation$apply_clustering = apply_clustering
      self$params$samples_correlation$center = center
      self$params$samples_correlation$row_annotations = row_annotations
      self$params$samples_correlation$col_annotations = col_annotations
      self$params$samples_correlation$color_palette = color_palette
      self$params$samples_correlation$reverse_palette = reverse_palette
      self$params$samples_correlation$title_font_size = title_font_size
      self$params$samples_correlation$y_label_font_size = y_label_font_size
      self$params$samples_correlation$y_tick_font_size = y_tick_font_size
      self$params$samples_correlation$x_label_font_size = x_label_font_size
      self$params$samples_correlation$x_tick_font_size = x_tick_font_size
      self$params$samples_correlation$img_format = img_format
      self$params$samples_correlation$update = T
    },

    toggle_feature_correlation = function() {
      if (self$params$feature_correlation$update) {
        self$params$feature_correlation$update = F
      } else {
        self$params$feature_correlation$update = T
      }
    },

    param_feature_correlation = function(auto_refresh, dataset, multival_cols, map_feature_terms, correlation_method, use, distance_method, clustering_method, k_clusters, apply_clustering, center, row_annotations, col_annotations, roh_threshold, top_features, color_palette, reverse_palette, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, img_format) {

      self$params$feature_correlation$auto_refresh = auto_refresh
      self$params$feature_correlation$dataset = dataset
      self$params$feature_correlation$multival_cols = multival_cols
      self$params$feature_correlation$map_feature_terms = map_feature_terms
      self$params$feature_correlation$correlation_method = correlation_method
      self$params$feature_correlation$use = use
      self$params$feature_correlation$distance_method = distance_method
      self$params$feature_correlation$clustering_method = clustering_method
      self$params$feature_correlation$k_clusters = k_clusters
      self$params$feature_correlation$apply_clustering = apply_clustering
      self$params$feature_correlation$center = center
      self$params$feature_correlation$row_annotations = row_annotations
      self$params$feature_correlation$col_annotations = col_annotations
      self$params$feature_correlation$roh_threshold = roh_threshold
      self$params$feature_correlation$top_features = top_features
      self$params$feature_correlation$color_palette = color_palette
      self$params$feature_correlation$reverse_palette = reverse_palette
      self$params$feature_correlation$title_font_size = title_font_size
      self$params$feature_correlation$y_label_font_size = y_label_font_size
      self$params$feature_correlation$y_tick_font_size = y_tick_font_size
      self$params$feature_correlation$x_label_font_size = x_label_font_size
      self$params$feature_correlation$x_tick_font_size = x_tick_font_size
      self$params$feature_correlation$img_format = img_format
      self$params$feature_correlation$update = T
    },

    toggle_pca = function() {
      if (self$params$pca$update) {
        self$params$pca$update = F
      } else {
        self$params$pca$update = T
      }
    },

    param_pca = function(auto_refresh, data_table, sample_groups_col, feature_groups_col, impute_median, apply_da, sample_groups_da, alpha_da, seed_da, pca_method, nPcs, displayed_pc_1, displayed_pc_2, completeObs, displayed_plots, colors_palette, marker_size, opacity, title_font_size, y_label_font_size, y_tick_font_size, x_label_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$pca$auto_refresh = auto_refresh
      self$params$pca$data_table = data_table
      self$params$pca$sample_groups_col = sample_groups_col
      self$params$pca$feature_groups_col = feature_groups_col
      self$params$pca$impute_median = impute_median
      self$params$pca$apply_da = apply_da
      self$params$pca$sample_groups_da = sample_groups_da
      self$params$pca$alpha_da = alpha_da
      self$params$pca$seed_da = seed_da
      self$params$pca$pca_method = pca_method
      self$params$pca$nPcs = nPcs
      self$params$pca$displayed_pc_1 = displayed_pc_1
      self$params$pca$displayed_pc_2 = displayed_pc_2
      self$params$pca$completeObs = completeObs
      self$params$pca$displayed_plots = displayed_plots
      self$params$pca$colors_palette = colors_palette
      self$params$pca$marker_size = marker_size
      self$params$pca$opacity = opacity
      self$params$pca$title_font_size = title_font_size
      self$params$pca$y_label_font_size = y_label_font_size
      self$params$pca$y_tick_font_size = y_tick_font_size
      self$params$pca$x_label_font_size = x_label_font_size
      self$params$pca$x_tick_font_size = x_tick_font_size
      self$params$pca$legend_font_size = legend_font_size
      self$params$pca$img_format = img_format
      self$params$pca$update = T

    },

    toggle_fa_analysis_plot = function() {
      if (self$params$fa_analysis_plot$update) {
        self$params$fa_analysis_plot$update = F
      } else {
        self$params$fa_analysis_plot$update = T
      }
    },

    param_fa_analysis_plot = function(auto_refresh,
                                      data_table,
                                      group_col,
                                      selected_view,
                                      selected_lipidclass,
                                      selected_fa,
                                      fa_norm,
                                      color_palette,
                                      title_font_size,
                                      y_label_font_size,
                                      y_tick_font_size,
                                      x_label_font_size,
                                      x_tick_font_size,
                                      legend_font_size,
                                      img_format) {
      self$params$fa_analysis_plot$auto_refresh = auto_refresh
      self$params$fa_analysis_plot$data_table = data_table
      self$params$fa_analysis_plot$group_col = group_col
      self$params$fa_analysis_plot$selected_view = selected_view
      self$params$fa_analysis_plot$selected_lipidclass = selected_lipidclass
      self$params$fa_analysis_plot$selected_fa = selected_fa
      self$params$fa_analysis_plot$fa_norm = fa_norm
      self$params$fa_analysis_plot$color_palette = color_palette
      self$params$fa_analysis_plot$title_font_size = title_font_size
      self$params$fa_analysis_plot$y_label_font_size = y_label_font_size
      self$params$fa_analysis_plot$y_tick_font_size = y_tick_font_size
      self$params$fa_analysis_plot$x_label_font_size = x_label_font_size
      self$params$fa_analysis_plot$x_tick_font_size = x_tick_font_size
      self$params$fa_analysis_plot$legend_font_size = legend_font_size
      self$params$fa_analysis_plot$img_format = img_format
      self$params$fa_analysis_plot$update = T
    },

    toggle_fa_comp_plot = function() {
      if (self$params$fa_comp_plot$update) {
        self$params$fa_comp_plot$update = F
      } else {
        self$params$fa_comp_plot$update = T
      }
    },

    param_fa_comp_plot = function(auto_refresh,
                                  data_table,
                                  composition,
                                  group_col,
                                  group_1,
                                  group_2,
                                  selected_lipidclass,
                                  color_palette,
                                  reverse_palette,
                                  title_font_size,
                                  y_label_font_size,
                                  y_tick_font_size,
                                  x_label_font_size,
                                  x_tick_font_size,
                                  legend_font_size,
                                  img_format) {
      self$params$fa_comp_plot$auto_refresh = auto_refresh
      self$params$fa_comp_plot$data_table = data_table
      self$params$fa_comp_plot$composition = composition
      self$params$fa_comp_plot$group_col = group_col
      self$params$fa_comp_plot$group_1 = group_1
      self$params$fa_comp_plot$group_2 = group_2
      self$params$fa_comp_plot$selected_lipidclass = selected_lipidclass
      self$params$fa_comp_plot$color_palette = color_palette
      self$params$fa_comp_plot$reverse_palette = reverse_palette
      self$params$fa_comp_plot$title_font_size = title_font_size
      self$params$fa_comp_plot$y_label_font_size = y_label_font_size
      self$params$fa_comp_plot$y_tick_font_size = y_tick_font_size
      self$params$fa_comp_plot$x_label_font_size = x_label_font_size
      self$params$fa_comp_plot$x_tick_font_size = x_tick_font_size
      self$params$fa_comp_plot$legend_font_size = legend_font_size
      self$params$fa_comp_plot$img_format = img_format
      self$params$fa_comp_plot$update = T
    },

    param_double_bonds_comparison = function(data_table, group_col, group_1, group_2, fc_function, statistical_test, adjustment_method) {
      self$params$double_bonds_comparison$data_table = data_table
      self$params$double_bonds_comparison$group_col = group_col
      self$params$double_bonds_comparison$group_1 = group_1
      self$params$double_bonds_comparison$group_2 = group_2
      self$params$double_bonds_comparison$fc_function = fc_function
      self$params$double_bonds_comparison$statistical_test = statistical_test
      self$params$double_bonds_comparison$adjustment_method = adjustment_method
    },

    toggle_double_bonds_plot = function() {
      if (self$params$double_bonds_plot$update) {
        self$params$double_bonds_plot$update = F
      } else {
        self$params$double_bonds_plot$update = T
      }
    },

    param_double_bonds_plot = function(auto_refresh, carbon_selection, unsat_selection, lipid_class, min_fc, max_pval, remove_missing_pval, remove_infitive_fc, color_palette, reverse_palette, marker_size, marker_opacity, title_font_size, y_label_font_size, y_tick_font_size, x_label_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$double_bonds_plot$auto_refresh = auto_refresh
      self$params$double_bonds_plot$selected_carbon_chain = carbon_selection
      self$params$double_bonds_plot$selected_unsat = unsat_selection
      self$params$double_bonds_plot$selected_lipid_class = lipid_class
      self$params$double_bonds_plot$min_fc = min_fc
      self$params$double_bonds_plot$max_pval = max_pval
      self$params$double_bonds_plot$remove_missing_pval = remove_missing_pval
      self$params$double_bonds_plot$remove_infitive_fc = remove_infitive_fc
      self$params$double_bonds_plot$color_palette = color_palette
      self$params$double_bonds_plot$reverse_palette = reverse_palette
      self$params$double_bonds_plot$marker_size = marker_size
      self$params$double_bonds_plot$marker_opacity = marker_opacity
      self$params$double_bonds_plot$title_font_size = title_font_size
      self$params$double_bonds_plot$y_label_font_size = y_label_font_size
      self$params$double_bonds_plot$y_tick_font_size = y_tick_font_size
      self$params$double_bonds_plot$x_label_font_size = x_label_font_size
      self$params$double_bonds_plot$x_tick_font_size = x_tick_font_size
      self$params$double_bonds_plot$legend_font_size = legend_font_size
      self$params$double_bonds_plot$img_format = img_format
      self$params$double_bonds_plot$update = T
    },

    param_ea_selection = function(table, group_col, group_1, group_2, fc_function, statistical_test, adjustment_method) {
      self$params$ea_selection$table = table
      self$params$ea_selection$group_col = group_col
      self$params$ea_selection$group_1 = group_1
      self$params$ea_selection$group_2 = group_2
      self$params$ea_selection$fc_function = fc_function
      self$params$ea_selection$statistical_test = statistical_test
      self$params$ea_selection$adjustment_method = adjustment_method
    },

    param_ea_process = function(custom_col, selected_features, ont, minGSSize, maxGSSize, terms_p_value_cutoff, terms_pAdjustMethod, verbose, OrgDb, seed) {
      self$params$ea_process$custom_col = custom_col
      self$params$ea_process$selected_features = selected_features
      self$params$ea_process$ont = ont
      self$params$ea_process$minGSSize = minGSSize
      self$params$ea_process$maxGSSize = maxGSSize
      self$params$ea_process$terms_p_value_cutoff = terms_p_value_cutoff
      self$params$ea_process$terms_pAdjustMethod = terms_pAdjustMethod
      self$params$ea_process$verbose = verbose
      self$params$ea_process$OrgDb = OrgDb
      self$params$ea_process$seed = seed
    },

    param_ora_selection = function(group_col, group_1, group_2, fc_function, statistical_test, adjustment_method) {
      self$params$ora_selection$group_col = group_col
      self$params$ora_selection$group_1 = group_1
      self$params$ora_selection$group_2 = group_2
      self$params$ora_selection$fc_function = fc_function
      self$params$ora_selection$statistical_test = statistical_test
      self$params$ora_selection$adjustment_method = adjustment_method
    },

    param_ora_process = function(custom_col, selected_features, pval_cutoff_features, padjust_features, pval_cutoff, pAdjustMethod, fc_threshold, ont, qval_cutoff, minGSSize, maxGSSize, seed) {
      self$params$ora_process$custom_col = custom_col
      self$params$ora_process$selected_features = selected_features
      self$params$ora_process$pval_cutoff_features = pval_cutoff_features
      self$params$ora_process$padjust_features = padjust_features
      self$params$ora_process$pval_cutoff = pval_cutoff
      self$params$ora_process$pAdjustMethod = pAdjustMethod
      self$params$ora_process$fc_threshold = fc_threshold
      self$params$ora_process$ont = ont
      self$params$ora_process$qval_cutoff = qval_cutoff
      self$params$ora_process$minGSSize = minGSSize
      self$params$ora_process$maxGSSize = maxGSSize
      self$params$ora_process$seed = seed
    },

    toggle_ea_dot_plot = function() {
      if (self$params$ea_dot_plot$update) {
        self$params$ea_dot_plot$update = F
      } else {
        self$params$ea_dot_plot$update = T
      }
    },

    param_ea_dot_plot = function(auto_refresh, x, y, color, show_categories, size, order_by, reverse_order, mode, marker_opacity, color_palette, reverse_palette, show_legend, legend_size, size_ref, yaxis_word_split, title_size, xlabel_size, xtick_size, ytick_size, img_format) {
      self$params$ea_dot_plot$auto_refresh= auto_refresh
      self$params$ea_dot_plot$x = x
      self$params$ea_dot_plot$y = y
      self$params$ea_dot_plot$color = color
      self$params$ea_dot_plot$show_categories = show_categories
      self$params$ea_dot_plot$size = size
      self$params$ea_dot_plot$order_by = order_by
      self$params$ea_dot_plot$reverse_order = reverse_order
      self$params$ea_dot_plot$mode = mode
      self$params$ea_dot_plot$marker_opacity = marker_opacity
      self$params$ea_dot_plot$color_palette = color_palette
      self$params$ea_dot_plot$reverse_palette = reverse_palette
      self$params$ea_dot_plot$show_legend = show_legend
      self$params$ea_dot_plot$legend_size = legend_size
      self$params$ea_dot_plot$size_ref = size_ref
      self$params$ea_dot_plot$yaxis_word_split = yaxis_word_split
      self$params$ea_dot_plot$title_size = title_size
      self$params$ea_dot_plot$xlabel_size = xlabel_size
      self$params$ea_dot_plot$xtick_size = xtick_size
      self$params$ea_dot_plot$ytick_size = ytick_size
      self$params$ea_dot_plot$img_format = img_format
      self$params$ea_dot_plot$update = T
    },

    toggle_ora_dot_plot = function() {
      if (self$params$ora_dot_plot$update) {
        self$params$ora_dot_plot$update = F
      } else {
        self$params$ora_dot_plot$update = T
      }
    },

    param_ora_dot_plot = function(auto_refresh, x, y, color, show_categories, size, order_by, reverse_order, marker_opacity, color_palette, reverse_palette, show_legend, size_ref, legend_size, yaxis_word_split, title_size, xlabel_size, xtick_size, ytick_size, img_format) {
      self$params$ora_dot_plot$auto_refresh= auto_refresh
      self$params$ora_dot_plot$x = x
      self$params$ora_dot_plot$y = y
      self$params$ora_dot_plot$color = color
      self$params$ora_dot_plot$show_categories = show_categories
      self$params$ora_dot_plot$size = size
      self$params$ora_dot_plot$order_by = order_by
      self$params$ora_dot_plot$reverse_order = reverse_order
      self$params$ora_dot_plot$marker_opacity = marker_opacity
      self$params$ora_dot_plot$color_palette = color_palette
      self$params$ora_dot_plot$reverse_palette = reverse_palette
      self$params$ora_dot_plot$show_legend = show_legend
      self$params$ora_dot_plot$size_ref = size_ref
      self$params$ora_dot_plot$legend_size = legend_size
      self$params$ora_dot_plot$yaxis_word_split = yaxis_word_split
      self$params$ora_dot_plot$title_size = title_size
      self$params$ora_dot_plot$xlabel_size = xlabel_size
      self$params$ora_dot_plot$xtick_size = xtick_size
      self$params$ora_dot_plot$ytick_size = ytick_size
      self$params$ora_dot_plot$img_format = img_format
      self$params$ora_dot_plot$update = T
    },

    toggle_ea_ridge_plot = function() {
      if (self$params$ea_ridge_plot$update) {
        self$params$ea_ridge_plot$update = F
      } else {
        self$params$ea_ridge_plot$update = T
      }
    },

    param_ea_ridge_plot = function(auto_refresh, show_category, fill, core_enrichment, color_palette, reverse_palette, displayed_label, orderBy, decreasing, title_font_size, yaxis_word_split, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$ea_ridge_plot$auto_refresh= auto_refresh
      self$params$ea_ridge_plot$show_category = show_category
      self$params$ea_ridge_plot$fill = fill
      self$params$ea_ridge_plot$core_enrichment = core_enrichment
      self$params$ea_ridge_plot$color_palette = color_palette
      self$params$ea_ridge_plot$reverse_palette = reverse_palette
      self$params$ea_ridge_plot$displayed_label = displayed_label
      self$params$ea_ridge_plot$orderBy = orderBy
      self$params$ea_ridge_plot$decreasing = decreasing
      self$params$ea_ridge_plot$title_font_size = title_font_size
      self$params$ea_ridge_plot$yaxis_word_split = yaxis_word_split
      self$params$ea_ridge_plot$y_label_font_size = y_label_font_size
      self$params$ea_ridge_plot$y_tick_font_size = y_tick_font_size
      self$params$ea_ridge_plot$x_label_font_size = x_label_font_size
      self$params$ea_ridge_plot$x_tick_font_size = x_tick_font_size
      self$params$ea_ridge_plot$legend_font_size = legend_font_size
      self$params$ea_ridge_plot$img_format = img_format
      self$params$ea_ridge_plot$update = T
    },

    toggle_ea_cnet_plot = function() {
      if (self$params$ea_cnet_plot$update) {
        self$params$ea_cnet_plot$update = F
      } else {
        self$params$ea_cnet_plot$update = T
      }
    },

    param_ea_cnet_plot = function(auto_refresh, show_category, displayed_labels, set_node_annotations, feature_node_annotations, set_node_color_palette, reverse_set_palette, feature_node_color_palette, reverse_feature_palette, label_font_size, static_network, solver, gravitationalConstant, nodeDistance, centralGravity, springLength, springConstant, img_format) {
      self$params$ea_cnet_plot$auto_refresh= auto_refresh
      self$params$ea_cnet_plot$show_category = show_category
      self$params$ea_cnet_plot$displayed_labels = displayed_labels
      self$params$ea_cnet_plot$set_node_annotations = set_node_annotations
      self$params$ea_cnet_plot$feature_node_annotations = feature_node_annotations
      self$params$ea_cnet_plot$set_node_color_palette = set_node_color_palette
      self$params$ea_cnet_plot$reverse_set_palette = reverse_set_palette
      self$params$ea_cnet_plot$feature_node_color_palette = feature_node_color_palette
      self$params$ea_cnet_plot$reverse_feature_palette = reverse_feature_palette
      self$params$ea_cnet_plot$label_font_size = label_font_size
      self$params$ea_cnet_plot$static_network = static_network
      self$params$ea_cnet_plot$solver = solver
      self$params$ea_cnet_plot$gravitationalConstant = gravitationalConstant
      self$params$ea_cnet_plot$nodeDistance = nodeDistance
      self$params$ea_cnet_plot$centralGravity = centralGravity
      self$params$ea_cnet_plot$springLength = springLength
      self$params$ea_cnet_plot$springConstant = springConstant
      self$params$ea_cnet_plot$img_format = img_format
      self$params$ea_cnet_plot$update = T
    },

    toggle_ora_cnet_plot = function() {
      if (self$params$ora_cnet_plot$update) {
        self$params$ora_cnet_plot$update = F
      } else {
        self$params$ora_cnet_plot$update = T
      }
    },

    param_ora_cnet_plot = function(auto_refresh, show_category, displayed_labels, set_node_annotations, feature_node_annotations, set_node_color_palette, reverse_set_palette, feature_node_color_palette, reverse_feature_palette, label_font_size, static_network, solver, gravitationalConstant, nodeDistance, centralGravity, springLength, springConstant, img_format) {
      self$params$ora_cnet_plot$auto_refresh= auto_refresh
      self$params$ora_cnet_plot$show_category = show_category
      self$params$ora_cnet_plot$displayed_labels = displayed_labels
      self$params$ora_cnet_plot$set_node_annotations = set_node_annotations
      self$params$ora_cnet_plot$feature_node_annotations = feature_node_annotations
      self$params$ora_cnet_plot$set_node_color_palette = set_node_color_palette
      self$params$ora_cnet_plot$reverse_set_palette = reverse_set_palette
      self$params$ora_cnet_plot$feature_node_color_palette = feature_node_color_palette
      self$params$ora_cnet_plot$reverse_feature_palette = reverse_feature_palette
      self$params$ora_cnet_plot$label_font_size = label_font_size
      self$params$ora_cnet_plot$static_network = static_network
      self$params$ora_cnet_plot$solver = solver
      self$params$ora_cnet_plot$gravitationalConstant = gravitationalConstant
      self$params$ora_cnet_plot$nodeDistance = nodeDistance
      self$params$ora_cnet_plot$centralGravity = centralGravity
      self$params$ora_cnet_plot$springLength = springLength
      self$params$ora_cnet_plot$springConstant = springConstant
      self$params$ora_cnet_plot$img_format = img_format
      self$params$ora_cnet_plot$update = T
    },

    toggle_ea_emap_plot = function() {
      if (self$params$ea_emap_plot$update) {
        self$params$ea_emap_plot$update = F
      } else {
        self$params$ea_emap_plot$update = T
      }
    },

    param_ea_emap_plot = function(auto_refresh, show_category, color, size, displayed_labels, mode, score_threshold, similarity_score, node_color_palette, reverse_node_palette, edge_width, edge_color, edge_color_palette, reverse_edge_palette, edge_magnifier, node_magnifier, label_font_size, static_network, solver, gravitationalConstant, nodeDistance, centralGravity, springLength, springConstant, img_format) {
      self$params$ea_emap_plot$auto_refresh= auto_refresh
      self$params$ea_emap_plot$show_category = show_category
      self$params$ea_emap_plot$color = color
      self$params$ea_emap_plot$size = size
      self$params$ea_emap_plot$displayed_labels = displayed_labels
      self$params$ea_emap_plot$mode = mode
      self$params$ea_emap_plot$score_threshold = score_threshold
      self$params$ea_emap_plot$similarity_score = similarity_score
      self$params$ea_emap_plot$node_color_palette = node_color_palette
      self$params$ea_emap_plot$reverse_node_palette = reverse_node_palette
      self$params$ea_emap_plot$edge_width = edge_width
      self$params$ea_emap_plot$edge_color = edge_color
      self$params$ea_emap_plot$edge_color_palette = edge_color_palette
      self$params$ea_emap_plot$reverse_edge_palette = reverse_edge_palette
      self$params$ea_emap_plot$edge_magnifier = edge_magnifier
      self$params$ea_emap_plot$node_magnifier = node_magnifier
      self$params$ea_emap_plot$label_font_size = label_font_size
      self$params$ea_emap_plot$static_network = static_network
      self$params$ea_emap_plot$solver = solver
      self$params$ea_emap_plot$gravitationalConstant = gravitationalConstant
      self$params$ea_emap_plot$nodeDistance = nodeDistance
      self$params$ea_emap_plot$centralGravity = centralGravity
      self$params$ea_emap_plot$springLength = springLength
      self$params$ea_emap_plot$springConstant = springConstant
      self$params$ea_emap_plot$img_format = img_format
      self$params$ea_emap_plot$update = T
    },

    toggle_ora_emap_plot = function() {
      if (self$params$ora_emap_plot$update) {
        self$params$ora_emap_plot$update = F
      } else {
        self$params$ora_emap_plot$update = T
      }
    },

    param_ora_emap_plot = function(auto_refresh, show_category, color, size, displayed_labels, score_threshold, similarity_score, node_color_palette, reverse_node_palette, edge_width, edge_color, edge_color_palette, reverse_edge_palette, edge_magnifier, node_magnifier, label_font_size, static_network, solver, gravitationalConstant, nodeDistance, centralGravity, springLength, springConstant, img_format) {
      self$params$ora_emap_plot$auto_refresh= auto_refresh
      self$params$ora_emap_plot$show_category = show_category
      self$params$ora_emap_plot$color = color
      self$params$ora_emap_plot$size = size
      self$params$ora_emap_plot$displayed_labels = displayed_labels
      self$params$ora_emap_plot$score_threshold = score_threshold
      self$params$ora_emap_plot$similarity_score = similarity_score
      self$params$ora_emap_plot$node_color_palette = node_color_palette
      self$params$ora_emap_plot$reverse_node_palette = reverse_node_palette
      self$params$ora_emap_plot$edge_width = edge_width
      self$params$ora_emap_plot$edge_color = edge_color
      self$params$ora_emap_plot$edge_color_palette = edge_color_palette
      self$params$ora_emap_plot$reverse_edge_palette = reverse_edge_palette
      self$params$ora_emap_plot$edge_magnifier = edge_magnifier
      self$params$ora_emap_plot$node_magnifier = node_magnifier
      self$params$ora_emap_plot$label_font_size = label_font_size
      self$params$ora_emap_plot$static_network = static_network
      self$params$ora_emap_plot$solver = solver
      self$params$ora_emap_plot$gravitationalConstant = gravitationalConstant
      self$params$ora_emap_plot$nodeDistance = nodeDistance
      self$params$ora_emap_plot$centralGravity = centralGravity
      self$params$ora_emap_plot$springLength = springLength
      self$params$ora_emap_plot$springConstant = springConstant
      self$params$ora_emap_plot$img_format = img_format
      self$params$ora_emap_plot$update = T
    },

    toggle_ora_bar_plot = function() {
      if (self$params$ora_bar_plot$update) {
        self$params$ora_bar_plot$update = F
      } else {
        self$params$ora_bar_plot$update = T
      }
    },

    param_ora_bar_plot = function(auto_refresh, x, color, show_category, displayed_label, order_by, order_decreasing, color_palette, reverse_palette, title_font_size, yaxis_word_split, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, img_format) {
      self$params$ora_bar_plot$auto_refresh= auto_refresh
      self$params$ora_bar_plot$x = x
      self$params$ora_bar_plot$color = color
      self$params$ora_bar_plot$show_category = show_category
      self$params$ora_bar_plot$displayed_label = displayed_label
      self$params$ora_bar_plot$order_by = order_by
      self$params$ora_bar_plot$order_decreasing = order_decreasing
      self$params$ora_bar_plot$color_palette = color_palette
      self$params$ora_bar_plot$reverse_palette = reverse_palette
      self$params$ora_bar_plot$title_font_size = title_font_size
      self$params$ora_bar_plot$yaxis_word_split = yaxis_word_split
      self$params$ora_bar_plot$y_label_font_size = y_label_font_size
      self$params$ora_bar_plot$y_tick_font_size = y_tick_font_size
      self$params$ora_bar_plot$x_label_font_size = x_label_font_size
      self$params$ora_bar_plot$x_tick_font_size = x_tick_font_size
      self$params$ora_bar_plot$legend_font_size = legend_font_size
      self$params$ora_bar_plot$img_format = img_format
      self$params$ora_bar_plot$update = T
    },

    #------------------------------------------------------ Indices methods ----

    set_type_column = function(type_column,
                               indexed_meta = self$tables$indexed_meta) {

      if (is.null(indexed_meta)) {
        base::stop('Define an ID column before proceeding')
      }

      if (!(type_column %in% colnames(indexed_meta))) {
        base::stop('Selected type column does not exist')
      }

      if (base::any(base::is.na(indexed_meta[,type_column]))) {
        base::stop('Column for sample types cannot contain missing values')
      }

      self$indices$type_column = type_column

    },

    set_group_column = function(group_column,
                                indexed_meta = self$tables$indexed_meta) {

      if (is.null(indexed_meta)) {
        base::stop('Define an ID column before proceeding')
      }

      if (!(group_column %in% colnames(indexed_meta))) {
        base::stop('Selected group column does not exist')
      }

      if (base::any(base::is.na(indexed_meta[,group_column]))) {
        base::stop('Column for sample groups cannot contain missing values')
      }

      self$indices$group_column = group_column

    },

    set_batch_column = function(batch_column = "None",
                                indexed_meta = self$tables$indexed_meta) {

      if (is.null(indexed_meta)) {
        base::stop('Define an ID column before proceeding')
      }

      if (is.null(batch_column)) {
        batch_column = "isoda_batch"
      }

      if (!(batch_column %in% colnames(indexed_meta))) {

        base::warning('Undefined or unsupplied batch column, proceding without batches')
        self$tables$indexed_meta$isoda_batch = 1
        batch_column = "isoda_batch"

      } else if (base::any(base::is.na(indexed_meta[,batch_column]))) {

        base::stop('Column for batches cannot contain missing values')

      }

      self$indices$batch_column = batch_column

    },

    set_blank_indices = function(blank_pattern = NULL,
                                 indexed_meta = self$tables$indexed_meta,
                                 type_column = self$indices$type_column) {
      if (is.null(blank_pattern)) {
        index_blanks = NULL
      } else {
        index_blanks = grep(pattern = blank_pattern,
                            x = indexed_meta[,type_column],
                            ignore.case = TRUE)
        index_blanks = rownames(indexed_meta)[index_blanks]
        if (length(index_blanks) == 0) {index_blanks = NULL}
      }

      self$indices$index_blanks = index_blanks
    },

    set_qc_indices = function(qc_pattern = NULL,
                              indexed_meta = self$tables$indexed_meta,
                              type_column = self$indices$type_column) {
      if (is.null(qc_pattern)) {
        index_qcs = NULL
      } else {
        index_qcs = grep(pattern = qc_pattern,
                         x = indexed_meta[,type_column],
                         ignore.case = TRUE)
        index_qcs = rownames(indexed_meta)[index_qcs]
        if (length(index_qcs) == 0) {index_qcs = NULL}
      }

      self$indices$index_qcs = index_qcs
    },

    set_pool_indices = function(pool_pattern = NULL,
                                indexed_meta = self$tables$indexed_meta,
                                type_column = self$indices$type_column) {
      if (is.null(pool_pattern)) {
        index_pools = NULL
      } else {
        index_pools = grep(pattern = pool_pattern,
                           x = indexed_meta[,type_column],
                           ignore.case = TRUE)
        index_pools = rownames(indexed_meta)[index_pools]
        if (length(index_pools) == 0) {index_pools = NULL}
      }

      self$indices$index_pools = index_pools
    },

    set_sample_indices = function(indexed_meta = self$tables$indexed_meta,
                                  index_blanks = self$indices$index_blanks,
                                  index_qcs = self$indices$index_qcs,
                                  index_pools = self$indices$index_pools) {
      index_samples = rownames(indexed_meta)[!(rownames(indexed_meta) %in% c(index_blanks, index_qcs, index_pools))]
      self$indices$index_samples = index_samples
    },

    exclude_samples = function(indexed_meta = self$tables$indexed_meta,
                               excluded_samples = self$indices$excluded_samples,
                               selection = NULL,
                               drop = T
                               ) {
      if (!is.null(selection)) {
        remaining_samples = rownames(indexed_meta)[!(rownames(indexed_meta) %in% excluded_samples)]
        selected_samples = selection[selection %in% remaining_samples]
        if (drop) {
          excluded_samples = c(excluded_samples, selected_samples)
        } else {
          excluded_samples = c(excluded_samples,
                               remaining_samples[!(remaining_samples %in% selected_samples)])
        }
        excluded_samples = sort(unique(excluded_samples))
        self$indices$excluded_samples = excluded_samples
      }
    },
    
    non_sample_exclusion = function(indexed_meta = self$tables$indexed_meta,
                                    excluded_samples = self$indices$excluded_samples,
                                    select_blanks = F,
                                    select_qcs = F,
                                    select_pools = F,
                                    index_blanks = self$indices$index_blanks,
                                    index_qcs = self$indices$index_qcs,
                                    index_pools = self$indices$index_pools,
                                    exclude = TRUE) {

      # Filter out from indexed_meta samples that were already excluded
      if (!is.null(excluded_samples)) {
        indexed_meta = indexed_meta[-which(rownames(indexed_meta) %in% excluded_samples),]
      }

      selected_samples = NULL

      # Add blanks
      if (select_blanks) {
        selected_samples = c(selected_samples, index_blanks)
      }

      # Add qcs
      if (select_qcs) {
        selected_samples = c(selected_samples, index_qcs)
      }

      # Add pools
      if (select_pools) {
        selected_samples = c(selected_samples, index_pools)
      }

      if (exclude) {
        excluded_samples = c(excluded_samples, selected_samples)
      } else {
        excluded_samples = c(excluded_samples, rownames(indexed_meta)[!(rownames(indexed_meta) %in% selected_samples)])
      }

      # Unique and sort
      excluded_samples = base::sort(base::unique(excluded_samples))

      self$indices$excluded_samples = excluded_samples
    },

    reset_sample_exclusion = function() {
      self$indices$excluded_samples = NULL
    },

    exclude_features = function(indexed_data = self$tables$indexed_data,
                                excluded_features = self$indices$excluded_features,
                                selection = NULL,
                                drop = T) {

      if (!is.null(selection)) {
        remaining_features = colnames(indexed_data)[!(colnames(indexed_data) %in% excluded_features)]
        selected_features = selection[selection %in% remaining_features]
        if (drop) {
          excluded_features = c(excluded_features, selected_features)
        } else {
          excluded_features = c(excluded_features,
                                remaining_features[!(remaining_features %in% selected_features)])
        }
        excluded_features = sort(unique(excluded_features))
        self$indices$excluded_features = excluded_features
      }
    },

    reset_feature_exclusion = function() {
      self$indices$excluded_features = NULL
    },

    #-------------------------------------------------------- Table methods ----

    import_meta = function(path, input_format = "Wide") {
      if (is.null(path)) {base::stop("Sample annotations file not provided")}
      if (!base::file.exists(path)) {base::stop("Sample annotations file not found")}
      transpose = base::ifelse(input_format == "Long", T, F)
      imp_meta = soda_read_table(file_path = path,
                                 sep = NA,
                                 first_column_as_index = F,
                                 transpose = transpose)
      self$tables$imp_meta = imp_meta
    },

    import_data = function(path, input_format = "Wide") {
      if (is.null(path)) {base::stop("Measurement data file not provided")}
      if (!base::file.exists(path)) {base::stop("Measurement data file not found")}
      transpose = base::ifelse(input_format == "Long", T, F)
      imp_data = soda_read_table(file_path = path,
                                 sep = NA,
                                 first_column_as_index = F,
                                 transpose = transpose)
      
      
      self$tables$imp_data = imp_data
    },
    
    import_feat = function(path, input_format = "Long") {
      if (is.null(path)) {return()}
      if (!base::file.exists(path)) {base::stop("Feature annotations file not found")}
      transpose = base::ifelse(input_format == "Wide", T, F)
      imp_feat = soda_read_table(file_path = path,
                                 sep = NA,
                                 first_column_as_index = F,
                                 transpose = transpose)
      self$tables$imp_feat = imp_feat
    },

    set_indexed_meta = function(id_col = self$indices$id_col_meta,
                                imp_meta = self$tables$imp_meta) {
      
      indexed_meta = get_indexed_table(id_col = id_col,
                                       input_table = imp_meta)
      indexed_meta[is.na(indexed_meta)] = "NA"
      indexed_meta[indexed_meta == ""] = "NA"

      # Store
      self$indices$id_col_meta = id_col
      self$tables$indexed_meta = indexed_meta

    },

    set_indexed_data = function(id_col = self$indices$id_col_data,
                                imp_data = self$tables$imp_data) {

      indexed_df = get_indexed_table(id_col = id_col,
                                     input_table = imp_data)
      
      indexed_data = as.matrix(sapply(indexed_df, as.numeric))
      rownames(indexed_data) = rownames(indexed_df)
      colnames(indexed_data) = colnames(indexed_df)

      # Store
      self$indices$id_col_data = id_col
      self$tables$indexed_data = indexed_data

    },
    
    set_indexed_feat = function(id_col = self$indices$id_col_feat,
                                id_col_data = self$indices$id_col_data,
                                imp_feat = self$tables$imp_feat,
                                indexed_data = self$tables$indexed_data,
                                type = self$type) {
      if (is.null(imp_feat)) {
        if (is.null(indexed_data)) {
          return()
        } else {
          indexed_feat = base::data.frame(base::matrix(data = NA, nrow = ncol(indexed_data), ncol = 0))
          rownames(indexed_feat) = colnames(indexed_data)
          id_col = id_col_data
        }
      } else {
        indexed_feat = get_indexed_table(id_col = id_col,
                                         input_table = imp_feat)
      }
      
      if (type == "Lipidomics") {
        indexed_feat = get_feature_metadata(
          feature_table = indexed_feat)
      }
      
      # Store
      self$indices$id_col_feat = id_col
      self$tables$indexed_feat = indexed_feat
      
    },
    
    check_indexed_table_consistency = function(indexed_data = self$tables$indexed_data,
                                               indexed_meta = self$tables$indexed_meta,
                                               indexed_feat = self$tables$indexed_feat) {
      # Check meta - data
      shared_samples = base::intersect(rownames(indexed_meta), rownames(indexed_data))
      if (length(shared_samples) < 3) {
        base::stop("Less than three shared samples between sample annotations and measurement data tables")
      } else {
        diff_samples = base::setdiff(rownames(indexed_data), rownames(indexed_meta))
        if (length(diff_samples) > 0) {
          base::warning(paste0("Dropping ", length(diff_samples), " samples not shared between tables"))
          indexed_data = indexed_data[shared_samples,]
          indexed_meta = indexed_meta[shared_samples,]
          self$tables$indexed_data = indexed_data
          self$tables$indexed_meta = indexed_meta
        }
      }
      
      # Check data - feat
      shared_features = base::intersect(colnames(indexed_data), rownames(indexed_feat))
      if (length(shared_features) < 3) {
        base::stop("Less than three shared features between measurement data and feature annotations tables")
      } else {
        diff_features = base::setdiff(colnames(indexed_data), rownames(indexed_feat))
        if (length(diff_features) > 0) {
          indexed_data = indexed_data[,shared_features]
          indexed_feat = indexed_feat[shared_features,]
          self$tables$indexed_data = indexed_data
          self$tables$indexed_feat = indexed_feat
        }
      }
    },

    set_raw_meta = function(indexed_meta = self$tables$indexed_meta,
                            excluded_samples = self$indices$excluded_samples){
      
      if (is.null(indexed_meta)) {
        base::stop('Error while setting Raw meta: missing indexed meta')
      }
      
      if (!is.null(excluded_samples)) {
        indexed_meta = indexed_meta[-which(rownames(indexed_meta) %in% excluded_samples), ]
      }

      if (nrow(indexed_meta) == 0) {
        base::stop('Removed all samples, cannot proceed.')
      }

      self$tables$raw_meta = indexed_meta
    },

    set_raw_data = function(indexed_data = self$tables$indexed_data,
                            excluded_samples = self$indices$excluded_samples,
                            excluded_features = self$indices$excluded_features,
                            filtered_out_features = self$indices$filtered_out_features,
                            indexed_meta = self$tables$indexed_meta,
                            norm_col = self$params$measurement_filter$norm_col
    ) {
      
      # Check on indexed data
      if (is.null(indexed_data)) {
        base::stop('Error while setting Raw data: missing indexed data')
      } else {
        raw_data = indexed_data
      }
      
      # Merge excluded_features and filtered_out_features
      excluded_features = c(excluded_features, filtered_out_features)
      
      # Exclude features
      if (!is.null(excluded_features)) {
        if (length(excluded_features) > 0) {
          raw_data = raw_data[, -which(colnames(raw_data) %in% excluded_features)]
        }
      }
      
      # Exclude samples
      if (!is.null(excluded_samples)) {
        if (length(excluded_samples) > 0) {
          raw_data = raw_data[-which(rownames(raw_data) %in% excluded_samples),]
        }
      }
      
      # Normalization
      if (norm_col != "None") {
        indexed_meta = indexed_meta[rownames(raw_data),]
        if (is_num_coercible(indexed_meta[,norm_col]) & !base::any(is.na(indexed_meta[,norm_col]))) {
          raw_data = raw_data/as.numeric(indexed_meta[,norm_col])
        } else {
          base::warning("Normalization skipped, selected column contains either non numeric or missing data.")
        }
      }
      
      self$tables$raw_data = raw_data
    },
    
    
    set_raw_feat = function(indexed_data = self$tables$indexed_data,
                            indexed_feat = self$tables$indexed_feat,
                            excluded_features = self$indices$excluded_features,
                            filtered_out_features = self$indices$filtered_out_features) {

      if (is.null(indexed_data)) {
        base::stop('Error while setting Raw feat: missing indexed data')
      }
      
      # Get the features from the indexed data
      features_data = colnames(indexed_data)
      features_feat = rownames(indexed_feat)
      
      # Merge excluded_features and filtered_out_features
      excluded_features = c(excluded_features, filtered_out_features)
      
      # Remove excluded features
      if (!is.null(excluded_features)) {
        if (length(excluded_features) > 0) {
          features_data = features_data[-which(features_data %in% excluded_features)]
        }
      }

      shared_keys = base::intersect(features_data, features_feat)
      missing_keys = base::setdiff(features_data, features_feat)
      raw_feat = indexed_feat[shared_keys, ]
      
      new_rows = data.frame(matrix(NA, nrow = length(missing_keys), ncol = ncol(raw_feat)))
      rownames(new_rows) = missing_keys
      colnames(new_rows) = colnames(raw_feat)
      
      if (nrow(new_rows) > 0) {
        raw_feat = rbind(raw_feat, new_rows)
      }
      self$tables$raw_feat = raw_feat

    },
    
    reset_raw_meta = function() {
      self$reset_sample_exclusion()
      self$set_raw_meta()
    },
    
    reset_raw_data = function() {
      self$reset_feature_exclusion()
      self$set_raw_data()
    },
    
    reset_raw_feat = function() {
      self$reset_feature_exclusion()
      self$set_raw_feat()
    },
    
    
    measurement_filter = function(indexed_data = self$tables$indexed_data,
                                  indexed_meta = self$tables$indexed_meta,
                                  excluded_samples = self$indices$excluded_samples,
                                  excluded_features = self$indices$excluded_features,
                                  index_blanks = self$indices$index_blanks,
                                  index_qcs = self$indices$index_qcs,
                                  index_pools = self$indices$index_pools,
                                  batch_column = self$indices$batch_column,
                                  group_column = self$indices$group_column,
                                  operation_order = self$params$measurement_filter$operation_order,
                                  blank_multiplier = self$params$measurement_filter$blank_multiplier,
                                  sample_threshold = self$params$measurement_filter$sample_threshold,
                                  group_threshold = self$params$measurement_filter$group_threshold,
                                  imputation_method = self$params$measurement_filter$imputation_method,
                                  batch_effect_correction = self$params$measurement_filter$batch_effect_correction) {
      
      if (is.null(indexed_data)) {
        base::stop('Error while filtering measurement data: missing indexed data')
      } else {
        data_table = indexed_data
      }
      
      # Exclude features
      if (!is.null(excluded_features)) {
        if (length(excluded_features) > 0) {
          data_table = data_table[, -which(colnames(data_table) %in% excluded_features)]
        }
      }
      # Exclude rows  
      if (!is.null(excluded_samples)) {
        if (length(excluded_samples) > 0) {
          data_table = data_table[-which(rownames(data_table) %in% excluded_samples),]
        }
      }
      
      # Remove any columns with either only 0s or NAs that can have survived
      empty_cols = colSums(data_table, na.rm = T)
      empty_cols = which(empty_cols == 0)
      if (length(empty_cols) > 0) {
        empty_cols = names(empty_cols)
        data_table = data_table[,-which(colnames(data_table) %in% empty_cols)]
      }
      
      # Get non-sample tables
      blank_table = indexed_data[index_blanks, colnames(data_table)]
      qc_table = indexed_data[index_qcs, colnames(data_table)]
      pool_table = indexed_data[index_pools, colnames(data_table)]
      
      # Process data_table indexed_meta
      for (func_name in operation_order) {
        if (func_name == "Imputation") {
          if (imputation_method != "None") {
            data_table = impute_na(data_table = data_table,
                                   method = imputation_method)
            blank_table = impute_na(data_table = blank_table,
                                    method = imputation_method)
            qc_table = impute_na(data_table = qc_table,
                                 method = imputation_method)
            pool_table = impute_na(data_table = pool_table,
                                   method = imputation_method)
          }
        } else if (func_name == "Batch correction") {
          if (batch_effect_correction != "None") {
            corrected_data = batch_effect_correction_combat(
              raw_data = data_table,
              batch_effect_correction = batch_effect_correction,
              batch_column = batch_column,
              indexed_meta = indexed_meta,
              blank_table = blank_table,
              qc_table = qc_table,
              pool_table = pool_table)
            data_table = corrected_data$raw_data
            blank_table = corrected_data$blank_table
          }
          
        } else if (func_name == "Filtering") {
          if (nrow(blank_table) > 0) {
            data_table = measurement_filtering(
              raw_data = data_table,
              blank_table = blank_table,
              indexed_meta = indexed_meta,
              batch_column = batch_column,
              group_column = group_column,
              blank_multiplier = blank_multiplier,
              sample_threshold = sample_threshold,
              group_threshold = group_threshold)
          }
        } else {
          base::stop(paste0('Requested process does not exist: ', func_name))
        }
      }

      dropped_features = base::setdiff(colnames(indexed_data), colnames(data_table))
      
      if (length(dropped_features) > 0) {
        if (!is.null(excluded_features)) {
          if (length(excluded_features) > 0) {
            dropped_features = base::setdiff(dropped_features, excluded_features)
          }
        }
      }
      
      self$indices$filtered_out_features = dropped_features
    },
    
    add_go_data = function(name,
                           feature_names,
                           keyType,
                           ont,
                           pvalueCutoff) {
      go_data = annotate_go(feature_names = feature_names,
                            keyType = keyType,
                            ont = ont,
                            pvalueCutoff = pvalueCutoff)

      if (is.null(go_data)) {
        print_tm(self$name, 'No GO enrichment with used parameters.')
        return()
      }

      sparse_table = get_sparse_matrix(features_go_table = go_data$feature_table,
                                       all_go_terms = rownames(go_data$go_table),
                                       sep = '|')

      self$tables$external_enrichment_tables[[name]]$terms_table = go_data$go_table
      self$tables$external_enrichment_tables[[name]]$association_table = go_data$feature_table
      self$tables$external_enrichment_tables[[name]]$sparse_table = sparse_table
    },

    upload_enrichment_data = function(name,
                                      association_table,
                                      terms_table = NULL,
                                      sep = '|') {

      # Create terms table if null
      if (is.null(terms_table)) {
        go_list = vector("list", nrow(association_table))
        # Loop through each row and split the 'go_terms' column by '|'
        for (i in 1:nrow(association_table)) {
          if (is.na(association_table[i,1])) {
            next
          } else {
            go_list[[i]] = strsplit(as.character(association_table[i,1]), sep, fixed = TRUE)[[1]]
          }
        }
        go_list = sort(unique(unlist(go_list)))
        terms_table = data.frame(
          ID = go_list,
          Description = go_list
        )
        rownames(terms_table) = terms_table$ID
        terms_table$ID = NULL
      }
      sparse_matrix = get_sparse_matrix(features_go_table = association_table[1],
                                        all_go_terms = rownames(terms_table),
                                        sep = sep)

      self$tables$external_enrichment_tables[[name]]$terms_table = terms_table
      self$tables$external_enrichment_tables[[name]]$association_table = association_table
      self$tables$external_enrichment_tables[[name]]$sparse_table = sparse_matrix

    },

    del_go_data = function(name) {
      self$tables$external_enrichment_tables[[name]] = NULL
      if (length(names(self$tables$external_enrichment_tables)) == 0) {
        names(self$tables$external_enrichment_tables) = NULL
      }
    },

    update_feature_table = function(sep = "|") {
      if (sep == "|") {
        regex_sep = "\\|"
      } else {
        regex_sep = sep
      }
      feature_table = self$tables$imp_feature_table[colnames(self$tables$raw_data),,drop = F]
      ext_names = names(self$tables$external_feature_tables)
      for (name in ext_names) {
        feature_table = augment_feature_table(feature_table = feature_table,
                                              external_table_name = name,
                                              external_feature_table = self$tables$external_feature_tables[[name]])
      }

      multi_value_annotations = sapply(feature_table, function(column) sum(stringr::str_count(column, regex_sep), na.rm = T))
      multi_value_annotations[is.na(multi_value_annotations)] = 0
      feature_table[feature_table == ""] = NA
      non_missing_counts = sapply(feature_table, function(column) sum(!is.na(column)))
      multi_value_annotations = names(multi_value_annotations)[multi_value_annotations > non_missing_counts]

      out_list = vector('list', length(multi_value_annotations))
      names(out_list) = multi_value_annotations

      for (col in multi_value_annotations) {
        feature_list = vector("list", nrow(feature_table))
        for (i in 1:nrow(feature_table)) {
          if (is.na(feature_table[i,col])) {
            next
          } else {
            feature_list[[i]] = strsplit(as.character(feature_table[i,col]), sep, fixed = TRUE)[[1]]
          }
        }
        feature_list = sort(unique(unlist(feature_list)))
        sparse_matrix = get_sparse_matrix(features_go_table = feature_table[col],
                                          all_go_terms = feature_list,
                                          sep = sep)
        out_list[[col]]$feature_list = feature_list
        out_list[[col]]$sparse_matrix = sparse_matrix
      }

      self$tables$feature_table = feature_table
      self$tables$feature_list = out_list
    },

    # Class normalisation
    normalise_class = function(){
      self$tables$class_norm_data = normalise_lipid_class(self$tables$raw_data)
    },

    # Total or Row normalisation
    normalise_total = function(){
      self$tables$total_norm_data = self$tables$raw_data/rowSums(self$tables$raw_data, na.rm = T)
    },

    # Z-score normalisation
    normalise_z_score = function() {
      self$tables$z_scored_data = z_score_normalisation(data_table = self$tables$raw_data)
    },

    # Class and z-score normalisation
    normalise_class_z_score = function() {
      self$tables$z_scored_class_norm_data = z_score_normalisation(data_table = self$tables$class_norm_data)
    },

    # Total and z-score normalisation
    normalise_total_z_score = function() {
      self$tables$z_scored_total_norm_data = z_score_normalisation(data_table = self$tables$total_norm_data)
    },

    # Class table
    get_class_table = function(){
      self$tables$class_table = get_lipid_class_table(self$tables$raw_data)
    },

    # Class table z-scored
    get_class_table_z_scored = function(){
      self$tables$class_table_z_scored = z_score_normalisation(data_table = self$tables$class_table)
    },

    # Class table total norm
    class_grouping_total_norm = function(){
      self$tables$class_table_total_norm = get_lipid_class_table(self$tables$total_norm_data)
    },

    # Z-score the class table (generated by the class_grouping method)
    normalise_class_table_z_score = function() {
      self$tables$class_table_z_scored_total_norm = z_score_normalisation(data_table = self$tables$class_table_total_norm)
    },

    get_group_summary_species = function() {
      self$tables$summary_species_table = get_group_median_table(data_table = self$tables$raw_data,
                                                                 meta_table = self$tables$raw_meta,
                                                                 group_col = self$indices$group_column)
    },

    get_group_summary_classes = function() {
      self$tables$summary_class_table = get_group_median_table(data_table = self$tables$class_table,
                                                               meta_table = self$tables$raw_meta,
                                                               group_col = self$indices$group_column)
    },

    derive_data_tables = function(params_list = NULL) {
      # Derive tables
      # self$update_feature_table()
      self$normalise_total()
      self$normalise_z_score()
      self$normalise_total_z_score()
      # self$get_group_summary_species()

      if (self$type == "Lipidomics") {
        self$normalise_class()
        self$normalise_class_z_score()
        self$get_class_table()
        self$get_class_table_z_scored()
        self$class_grouping_total_norm()
        self$normalise_class_table_z_score()
        self$get_group_summary_classes()
      }

      self$indices$feature_id_type = "SYMBOL"
      
      # Set plotting parameters

      self$param_dendrogram(auto_refresh = F,
                            dataset = 'Z-scored table',
                            annotations = self$indices$group_column,
                            distance_method = "euclidean",
                            p = 2,
                            clustering_method = "ward.D2",
                            k_clusters = NULL,
                            color_palette = "Set1",
                            x_tick_font_size = 0,
                            y_label_font_size = 12,
                            y_tick_font_size = 15,
                            img_format = "png")

      self$param_class_distribution(auto_refresh = F,
                                    dataset = 'Class table total normalized',
                                    group_col = self$indices$group_column,
                                    color_palette = 'Set1',
                                    title_font_size = 0,
                                    y_label_font_size = 20,
                                    y_tick_font_size = 15,
                                    x_label_font_size = 0,
                                    x_tick_font_size = 15,
                                    legend_font_size = 15,
                                    img_format = "png")

      self$param_class_comparison(auto_refresh = F,
                                  dataset = 'Class table total normalized',
                                  group_col = self$indices$group_column,
                                  color_palette = 'Set1',
                                  title_font_size = 16,
                                  y_label_font_size = 20,
                                  y_tick_font_size = 15,
                                  x_tick_font_size = 15,
                                  legend_font_size = 15,
                                  img_format = "png")

      self$param_pca(auto_refresh = F,
                     data_table = 'Z-scored total normalized table',
                     sample_groups_col = self$indices$group_column,
                     feature_groups_col = NULL,
                     impute_median = F,
                     apply_da = FALSE,
                     sample_groups_da = self$indices$group_column,
                     alpha_da = 0.8,
                     seed_da = 1,
                     pca_method = 'nipals',
                     nPcs = 2,
                     displayed_pc_1 = 1,
                     displayed_pc_2 = 2,
                     completeObs = F,
                     displayed_plots = 'scores',
                     colors_palette = 'Set1',
                     marker_size = 10,
                     opacity = 1,
                     title_font_size = 16,
                     y_label_font_size = 20,
                     y_tick_font_size = 15,
                     x_label_font_size = 20,
                     x_tick_font_size = 15,
                     legend_font_size = 15,
                     img_format = "png")

      self$param_fa_analysis_plot(
        auto_refresh = F,
        data_table = "Total normalized table",
        group_col = self$indices$group_column,
        selected_view = self$params$fa_analysis_plot$selected_view,
        selected_lipidclass = self$params$fa_analysis_plot$selected_lipidclass,
        selected_fa = self$params$fa_analysis_plot$selected_fa,
        fa_norm = self$params$fa_analysis_plot$fa_norm,
        color_palette = 'Set1',
        title_font_size = NULL,
        y_label_font_size = NULL,
        y_tick_font_size = NULL,
        x_label_font_size = NULL,
        x_tick_font_size = NULL,
        legend_font_size = NULL,
        img_format = "png")

      self$param_fa_comp_plot(
        auto_refresh = F,
        data_table = "Total normalized table",
        composition = self$params$fa_comp_plot$composition,
        group_col = self$indices$group_column,
        group_1 = unique(self$tables$raw_meta[, self$indices$group_column])[1],
        group_2 = unique(self$tables$raw_meta[, self$indices$group_column])[2],
        selected_lipidclass = self$params$fa_comp_plot$selected_lipidclass,
        color_palette = "Blues",
        reverse_palette = F,
        title_font_size = NULL,
        y_label_font_size = NULL,
        y_tick_font_size = NULL,
        x_label_font_size = NULL,
        x_tick_font_size = NULL,
        legend_font_size = NULL,
        img_format = "png"
      )

      self$param_volcano_plot_comparison(
        data_table = 'Total normalized table',
        group_col = self$indices$group_column,
        group_1 = unique(self$tables$raw_meta[,self$indices$group_column])[1],
        group_2 = unique(self$tables$raw_meta[,self$indices$group_column])[2],
        fc_function = 'mean',
        statistical_test = 't-Test',
        adjustment_method = 'none')

      self$param_volcano_plot(
        auto_refresh = F,
        feature_metadata = 'None',
        keep_significant = F,
        displayed_plot = 'main',
        p_val_threshold = 0.05,
        fc_threshold = 2,
        marker_size = 10,
        opacity = 1,
        color_palette = 'Set1',
        reverse_palette = F,
        title_font_size = 16,
        y_label_font_size = 20,
        y_tick_font_size = 15,
        x_label_font_size = 20,
        x_tick_font_size = 15,
        legend_font_size = 15,
        img_format = "png")

      self$param_heatmap(auto_refresh = F,
                         dataset = 'Z-scored total normalized table',
                         distance_method = "euclidian",
                         clustering_method = "ward.D2",
                         impute_min = T,
                         center = F,
                         apply_clustering = T,
                         k_clusters_samples = 1,
                         k_clusters_features = 1,
                         map_sample_data = NULL,
                         map_feature_data = NULL,
                         map_feature_terms = NULL,
                         multival_cols = "None",
                         group_column_da = self$indices$group_column,
                         apply_da = T,
                         alpha_da = 0.8,
                         seed_da = 1,
                         color_palette = 'RdYlBu',
                         reverse_palette = T,
                         title_font_size = 0,
                         y_label_font_size = 17,
                         x_label_font_size = 17,
                         x_tick_font_size = 0,
                         y_tick_font_size = 0,
                         img_format = "png")


      self$param_samples_correlation(auto_refresh = F,
                                     dataset = 'Z-scored total normalized table',
                                     correlation_method = "pearson",
                                     use = 'pairwise.complete.obs',
                                     distance_method = "euclidian",
                                     clustering_method = "ward.D2",
                                     k_clusters = 1,
                                     apply_clustering = T,
                                     center = F,
                                     row_annotations = self$indices$group_column,
                                     col_annotations = NULL,
                                     color_palette = 'RdYlBu',
                                     reverse_palette = T,
                                     title_font_size = 0,
                                     y_label_font_size = 0,
                                     y_tick_font_size = 0,
                                     x_label_font_size = 0,
                                     x_tick_font_size = 0,
                                     img_format = "png"
      )

      self$param_feature_correlation(
        auto_refresh = F,
        dataset = 'Z-scored total normalized table',
        multival_cols = 'None',
        map_feature_terms = NULL,
        correlation_method = "pearson",
        use = 'pairwise.complete.obs',
        distance_method = "euclidian",
        clustering_method = "ward.D2",
        k_clusters = 1,
        apply_clustering = T,
        center = F,
        row_annotations = NULL,
        col_annotations = NULL,
        roh_threshold = 0.95,
        top_features = 400,
        color_palette = 'RdYlBu',
        reverse_palette = T,
        title_font_size = 0,
        y_label_font_size = 0,
        y_tick_font_size = 0,
        x_label_font_size = 0,
        x_tick_font_size = 0,
        img_format = "png"
      )



      self$param_double_bonds_comparison(data_table = "Total normalized table",
                                         group_col = self$indices$group_column,
                                         group_1 = unique(self$tables$raw_meta[,self$indices$group_column])[1],
                                         group_2 = unique(self$tables$raw_meta[,self$indices$group_column])[2],
                                         fc_function = 'mean',
                                         statistical_test = 't-Test',
                                         adjustment_method = 'none')

      self$param_double_bonds_plot(auto_refresh = F,
                                   carbon_selection = "Carbon count (chain 1)",
                                   unsat_selection = "Double bonds (chain 1)",
                                   lipid_class = "CE",
                                   min_fc = 0,
                                   max_pval = 1,
                                   remove_missing_pval = T,
                                   remove_infitive_fc = T,
                                   color_palette = 'RdBu',
                                   reverse_palette = T,
                                   marker_size = 1,
                                   marker_opacity = 0.5,
                                   title_font_size = 16,
                                   y_label_font_size = 20,
                                   y_tick_font_size = 15,
                                   x_label_font_size = 20,
                                   x_tick_font_size = 15,
                                   legend_font_size = 15,
                                   img_format = "png")

      self$param_ea_selection(
        table = 'Total normalized table',
        group_col = self$indices$group_column,
        group_1 = unique(self$tables$raw_meta[,self$indices$group_column])[1],
        group_2 = unique(self$tables$raw_meta[,self$indices$group_column])[2],
        fc_function = "mean",
        statistical_test = "t-Test",
        adjustment_method = "none"
      )

      self$param_ora_selection(
        group_col = self$indices$group_column,
        group_1 = unique(self$tables$raw_meta[,self$indices$group_column])[1],
        group_2 = unique(self$tables$raw_meta[,self$indices$group_column])[2],
        fc_function = "mean",
        statistical_test = "t-Test",
        adjustment_method = "none"
      )

      self$param_ea_process(
        custom_col = NULL,
        selected_features = NULL,
        ont = "ALL",
        minGSSize = 3,
        maxGSSize = 800,
        terms_p_value_cutoff = 0.05,
        terms_pAdjustMethod = "none",
        verbose = TRUE,
        OrgDb = "org.Hs.eg.db",
        seed = 1
      )

      self$param_ora_process(
        custom_col = NULL,
        selected_features = NULL,
        pval_cutoff_features = 0.05,
        padjust_features = "none",
        pval_cutoff = 0.05,
        pAdjustMethod = "none",
        fc_threshold = 2,
        ont = "ALL",
        qval_cutoff = 0.05,
        minGSSize = 10,
        maxGSSize  = 500,
        seed = 1
      )

    },

    #--------------------------------------------------- Plot table methods ----

    # Volcano table
    get_volcano_table = function(data_table = self$params$volcano_plot_comparison$data_table,
                                 sample_table = self$tables$raw_meta,
                                 feature_table = self$tables$raw_feat,
                                 group_col = self$params$volcano_plot_comparison$group_col,
                                 group_1 = self$params$volcano_plot_comparison$group_1,
                                 group_2 = self$params$volcano_plot_comparison$group_2,
                                 fc_function = self$params$volcano_plot_comparison$fc_function,
                                 statistical_test = self$params$volcano_plot_comparison$statistical_test,
                                 adjustment_method = self$params$volcano_plot_comparison$adjustment_method) {

      if (class(data_table)[1] == 'character'){
        data_table = self$table_switch_local(table_name = data_table)
      }


      volcano_table = get_comparison_table(data_table = data_table,
                                           sample_table = sample_table,
                                           feature_table = feature_table,
                                           group_col = group_col,
                                           group_1 = group_1,
                                           group_2 = group_2,
                                           fc_function = fc_function,
                                           statistical_test = statistical_test,
                                           adjustment_method = adjustment_method)

      self$tables$volcano_table = volcano_table
    },

    push_volcano_to_meta = function() {
      self$tables$feature_table$volcano_plot_expression = "None"
      self$tables$feature_table[rownames(self$tables$volcano_plot), "volcano_plot_expression"] = self$tables$volcano_plot[, "expression"]
    },

    # Double bond plot table
    get_double_bonds_table = function(data_table = self$params$double_bonds_comparison$data_table,
                                      sample_table = self$tables$raw_meta,
                                      feature_table = self$tables$raw_feat,
                                      group_col = self$params$double_bonds_comparison$group_col,
                                      group_1 = self$params$double_bonds_comparison$group_1,
                                      group_2 = self$params$double_bonds_comparison$group_2,
                                      fc_function = self$params$double_bonds_comparison$fc_function,
                                      statistical_test = self$params$double_bonds_comparison$statistical_test,
                                      adjustment_method = self$params$double_bonds_comparison$adjustment_method) {

      if (class(data_table)[1] == 'character') {
        data_table = self$table_switch_local(table_name = data_table)
      }

      double_bonds_table = get_comparison_table(data_table = data_table,
                                                sample_table = sample_table,
                                                feature_table = feature_table,
                                                group_col = group_col,
                                                group_1 = group_1,
                                                group_2 = group_2,
                                                fc_function = fc_function,
                                                statistical_test = statistical_test,
                                                adjustment_method = adjustment_method)



      self$tables$double_bonds_table = double_bonds_table
    },

    #---------------------------------------------------- GSEA & OR methods ----

    # EA feature table
    get_ea_feature_table = function(data_table = self$tables$total_norm_data,
                                    sample_table = self$tables$raw_meta,
                                    feature_table = self$tables$raw_feat,
                                    group_col = self$params$ea_selection$group_col,
                                    group_1 = self$params$ea_selection$group_1,
                                    group_2 = self$params$ea_selection$group_2,
                                    fc_function = self$params$ea_selection$fc_function,
                                    statistical_test = self$params$ea_selection$statistical_test,
                                    adjustment_method = self$params$ea_selection$adjustment_method) {

      if (class(data_table)[1] == "character") {
        data_table = self$table_switch_local(data_table)
      }

      ea_feature_table = get_comparison_table(data_table = data_table,
                                              sample_table = sample_table,
                                              feature_table = feature_table,
                                              group_col = group_col,
                                              group_1 = group_1,
                                              group_2 = group_2,
                                              fc_function = fc_function,
                                              statistical_test = statistical_test,
                                              adjustment_method = adjustment_method)

      self$tables$ea_feature_table = ea_feature_table
    },

    # ORA feature table
    get_ora_feature_table = function(data_table = self$tables$total_norm_data,
                                     sample_table = self$tables$raw_meta,
                                     feature_table = self$tables$raw_feat,
                                     group_col = self$params$ora_selection$group_col,
                                     group_1 = self$params$ora_selection$group_1,
                                     group_2 = self$params$ora_selection$group_2,
                                     fc_function = self$params$ora_selection$fc_function,
                                     statistical_test = self$params$ora_selection$statistical_test,
                                     adjustment_method = self$params$ora_selection$adjustment_method) {

      if (class(data_table)[1] == "character") {
        data_table = self$table_switch_local(data_table)
      }

      ora_feature_table = get_comparison_table(data_table = data_table,
                                               sample_table = sample_table,
                                               feature_table = feature_table,
                                               group_col = group_col,
                                               group_1 = group_1,
                                               group_2 = group_2,
                                               fc_function = fc_function,
                                               statistical_test = statistical_test,
                                               adjustment_method = adjustment_method)

      self$tables$ora_feature_table = ora_feature_table
    },


    # Get EA object
    get_ea_object = function(ea_feature_table = self$tables$ea_feature_table,
                             feature_table = self$tables$raw_feat,
                             keyType = self$indices$feature_id_type,
                             custom_col = self$params$ea_process$custom_col,
                             selected_features = self$params$ea_process$selected_features,
                             ont = self$params$ea_process$ont,
                             minGSSize = self$params$ea_process$minGSSize,
                             maxGSSize = self$params$ea_process$maxGSSize,
                             terms_p_value_cutoff = self$params$ea_process$terms_p_value_cutoff,
                             terms_pAdjustMethod = self$params$ea_process$terms_pAdjustMethod,
                             verbose = self$params$ea_process$verbose,
                             OrgDb = self$params$ea_process$OrgDb,
                             seed = self$params$ea_process$seed) {

      ea_object = get_ea_object(ea_feature_table = ea_feature_table,
                                custom_col = custom_col,
                                selected_features = selected_features,
                                feature_table = feature_table,
                                keyType = keyType,
                                ont = ont,
                                minGSSize = minGSSize,
                                maxGSSize = maxGSSize,
                                p_value_cutoff = terms_p_value_cutoff,
                                verbose = verbose,
                                OrgDb = OrgDb,
                                pAdjustMethod = terms_pAdjustMethod,
                                seed = seed)

      self$tables$ea_object = ea_object
      self$tables$ea_table = ea_object@result
    },

    # Get ORA object
    get_ora_object = function(ora_feature_table = self$tables$ora_feature_table,
                              custom_col = self$params$ora_process$custom_col,
                              selected_features = self$params$ora_process$selected_features,
                              feature_table = self$tables$raw_feat,
                              pval_cutoff_features = self$params$ora_process$pval_cutoff_features,
                              padjust_features = self$params$ora_process$padjust_features,
                              pval_cutoff = self$params$ora_process$pval_cutoff,
                              pAdjustMethod = self$params$ora_process$pAdjustMethod,
                              fc_threshold = self$params$ora_process$fc_threshold,
                              keyType = self$indices$feature_id_type,
                              ont = self$params$ora_process$ont,
                              qval_cutoff = self$params$ora_process$qval_cutoff,
                              minGSSize = self$params$ora_process$minGSSize,
                              maxGSSize  = self$params$ora_process$maxGSSize,
                              seed = self$params$ora_process$seed) {

      ora_object = get_ora_object(ora_feature_table = ora_feature_table,
                                  custom_col = custom_col,
                                  selected_features = selected_features,
                                  feature_table = feature_table,
                                  pval_cutoff_features = pval_cutoff_features,
                                  padjust_features = padjust_features,
                                  pval_cutoff = pval_cutoff,
                                  pAdjustMethod = pAdjustMethod,
                                  fc_threshold = fc_threshold,
                                  keyType = keyType,
                                  ont = ont,
                                  qval_cutoff = qval_cutoff,
                                  minGSSize = minGSSize,
                                  maxGSSize  = maxGSSize,
                                  seed = seed)

      self$tables$ora_object = ora_object
      self$tables$ora_table = ora_object@result
    },

    #----------------------------------------------------- Plotting methods ----

    # Sample type distribution
    plot_sample_type_distribution = function(
    input_table = self$tables$indexed_meta,
    index_blanks = self$indices$index_blanks,
    index_qcs = self$indices$index_qcs,
    index_pools = self$indices$index_pools,
    index_samples = self$indices$index_samples,
    batch_column = self$indices$batch_column
    ) {
      mixed_samples = unique(c(intersect(index_blanks, index_qcs), intersect(index_qcs, index_pools), intersect(index_blanks, index_pools)))
      batches = sort(unique(input_table[,batch_column]))
      plot_df = list()
      for (batch in batches) {
        batch_rows = rownames(input_table)[input_table[,batch_column] == batch]
        plot_df$Batch = c(plot_df$Batch, batch)
        plot_df$Samples = c(plot_df$Samples, length(base::intersect(batch_rows, index_samples)))
        plot_df$Blanks = c(plot_df$Blanks, length(base::intersect(batch_rows, index_blanks)))
        plot_df$QCs = c(plot_df$QCs, length(base::intersect(batch_rows, index_qcs)))
        plot_df$Pools = c(plot_df$Pools, length(base::intersect(batch_rows, index_pools)))
        plot_df$Mixed = c(plot_df$Mixed, length(base::intersect(batch_rows, mixed_samples)))
      }
      
      plot_df = data.frame(plot_df)
      rownames(plot_df) = plot_df$Batch
      plot_df$Batch = NULL
      
      batch_label = base::ifelse(length(batches) > 1, " & batch", "")
      
      fig = plotly::plot_ly(x = factor(colnames(plot_df), levels = colnames(plot_df)))
      for (i in rownames(plot_df)) {
        fig = plotly::add_trace(
          p = fig,
          y = unname(unlist(plot_df[i,,T])),
          type = 'bar',
          name = i)
      }
      fig = plotly::layout(
        p = fig,
        title = paste0("Sample type", batch_label, " distribution"),
        yaxis = list(title = 'Count'),
        barmode = 'stack',
        bargap = 0.2)
      fig = plotly::layout(
        p = fig,
        dragmode = FALSE,
        bargap = 0.2
      )
      fig = plotly::config(
        p = fig,
        displayModeBar = FALSE,
        scrollZoom = FALSE
      )
      self$plots$sample_type_distribution = fig
    },
    
    # Sample group distribution
    plot_sample_group_distribution = function(
    input_table = self$tables$indexed_meta,
    batch_column = self$indices$batch_column,
    group_column = self$indices$group_column) {
      
      batches = sort(unique(input_table[,batch_column]))
      groups = sort(unique(input_table[,group_column]))
      
      # Check on groups
      if (length(groups) < 2) {base::stop('Less than two groups in selected group column')}
      
      plot_df = list()
      for (batch in batches) {
        batch_rows = rownames(input_table)[input_table[,batch_column] == batch]
        plot_df$Batch = c(plot_df$Batch, batch)
        for (group in groups) {
          index_group = rownames(input_table)[input_table[,group_column] == group]
          plot_df[[group]] = c(plot_df[[group]], length(base::intersect(batch_rows, index_group)))
        }
      }
      
      plot_df = data.frame(plot_df, check.names = F)
      rownames(plot_df) = plot_df$Batch
      plot_df$Batch = NULL
      
      batch_label = base::ifelse(length(batches) > 1, " & batch", "")
      
      fig = plotly::plot_ly(x = factor(colnames(plot_df), levels = colnames(plot_df)))
      for (i in rownames(plot_df)) {
        fig = plotly::add_trace(
          p = fig,
          y = unname(unlist(plot_df[i,,T])),
          type = 'bar',
          name = i)
      }
      fig = plotly::layout(
        p = fig,
        title = paste0("Sample groups (", group_column, ")", batch_label, " distribution"),
        yaxis = list(title = 'Count'),
        barmode = 'stack',
        bargap = 0.2)
      fig = plotly::layout(
        p = fig,
        dragmode = FALSE,
        bargap = 0.2
      )
      fig = plotly::config(
        p = fig,
        displayModeBar = FALSE,
        scrollZoom = FALSE
      )
      self$plots$sample_group_distribution = fig
    },
    
    # Dendrogram
    plot_dendrogram = function(dataset = self$params$dendrogram$dataset,
                               meta_table = self$tables$raw_meta,
                               annotations = self$params$dendrogram$annotations,
                               distance_method = self$params$dendrogram$distance_method,
                               p = self$params$dendrogram$p,
                               clustering_method = self$params$dendrogram$clustering_method,
                               k_clusters = self$params$dendrogram$k_clusters,
                               color_palette = self$params$dendrogram$color_palette,
                               x_tick_font_size = self$params$dendrogram$x_tick_font_size,
                               y_label_font_size = self$params$dendrogram$y_label_font_size,
                               y_tick_font_size = self$params$dendrogram$y_tick_font_size,
                               width = NULL,
                               height = NULL){

      # Checks
      data_table = self$table_check_convert(dataset)

      p = as.numeric(p)

      if (!is.null(k_clusters)) {
        if (is_coercible_to_numeric(k_clusters)) {
          k_clusters = as.numeric(k_clusters)
        } else {
          k_clusters = NULL
        }
      }


      output = plot_dendrogram(data_table = data_table,
                               meta_table = meta_table,
                               annotations = annotations,
                               distance_method = distance_method,
                               p = p,
                               clustering_method = clustering_method,
                               k_clusters = k_clusters,
                               color_palette = color_palette,
                               y_label_font_size = y_label_font_size,
                               y_tick_font_size = y_tick_font_size,
                               x_tick_font_size = x_tick_font_size,
                               width = width,
                               height = height)

      self$plots$dendrogram = output$plot
      self$tables$dendrogram = output$data_table
    },

    # Class distribution
    plot_class_distribution = function(table = self$params$class_distribution$dataset,
                                       meta_table = self$tables$raw_meta,
                                       group_col = self$params$class_distribution$group_col,
                                       color_palette = self$params$class_distribution$color_palette,
                                       title_font_size = self$params$class_distribution$title_font_size,
                                       y_label_font_size = self$params$class_distribution$y_label_font_size,
                                       y_tick_font_size = self$params$class_distribution$y_tick_font_size,
                                       x_label_font_size = self$params$class_distribution$x_label_font_size,
                                       x_tick_font_size = self$params$class_distribution$x_tick_font_size,
                                       legend_font_size = self$params$class_distribution$legend_font_size,
                                       width = NULL,
                                       height = NULL){

      # Get table
      table = self$table_check_convert(table)

      # Process fonts
      xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
      ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
      legend_show = base::ifelse(legend_font_size > 0, T, F)
      x_axis_title = base::ifelse(x_label_font_size > 0, "Lipid class", "")
      y_axis_title = base::ifelse(y_label_font_size > 0, "Concentration", "")
      title = base::ifelse(title_font_size > 0, "Class distribution", "")


      meta_table = meta_table[!is.na(meta_table[,group_col]),]
      group_list = as.character(sort(unique(meta_table[,group_col])))
      meta_table[,group_col] = as.character(meta_table[,group_col])

      # Produce the class x group table
      samp_list = rownames(table)
      class_list = colnames(table)

      plot_table = data.frame(matrix(data = 0.0,
                                     nrow = length(class_list),
                                     ncol = length(group_list)))
      rownames(plot_table) = class_list
      colnames(plot_table) = group_list

      for (c in class_list) {
        for (g in group_list){
          s = rownames(meta_table)[meta_table[,group_col] == g]
          m = mean(as.matrix(table[s, c]))
          plot_table[c,g] = m
        }
      }

      # Store the plot_table
      self$tables$class_distribution_table = plot_table

      # Colors
      color_palette = get_colors(color_count = colors_switch(color_palette), color_palette = color_palette)
      # color_palette = RColorBrewer::brewer.pal(color_count, color_palette)
      color_palette = colorRampPalette(color_palette)(length(group_list))
      color_palette = setNames(color_palette, group_list)

      # Produce the plot
      fig = plotly::plot_ly(colors = unname(color_palette), width = width, height = height)
      for (col in colnames(plot_table)) {

        fig = plotly::add_trace(p = fig,
                                x = rownames(plot_table),
                                y = plot_table[,col],
                                name = col,
                                color = color_palette[col],
                                showlegend = legend_show,
                                type  = "bar")
      }

      fig = plotly::layout(p = fig,

                           title = list(text = title,
                                        # xref = "paper",
                                        font = list(size = title_font_size)),

                           xaxis = list(title = list(text = x_axis_title,
                                                     font = list(size = x_label_font_size)),
                                        showticklabels = xtick_show,
                                        tickfont = list(size = x_tick_font_size)
                                        ),

                           yaxis = list(title = list(text = y_axis_title,
                                                     font = list(size = y_label_font_size)),
                                        showticklabels = ytick_show,
                                        tickfont = list(size = y_tick_font_size)
                           ),

                           legend = list(orientation = 'h',
                                         xanchor = "center",
                                         x = 0.5,
                                         font = list(size = legend_font_size)),
                           plot_bgcolor='rgba(0,0,0,0)',
                           paper_bgcolor='rgba(0,0,0,0)'
                           )

      self$plots$class_distribution = fig
    },

    # Class comparison
    plot_class_comparison = function(data_table = self$params$class_comparison$dataset,
                                     meta_table = self$tables$raw_meta,
                                     group_col = self$params$class_comparison$group_col,
                                     color_palette = self$params$class_comparison$color_palette,
                                     title_font_size = self$params$class_comparison$title_font_size,
                                     y_label_font_size = self$params$class_comparison$y_label_font_size,
                                     y_tick_font_size = self$params$class_comparison$y_tick_font_size,
                                     x_tick_font_size = self$params$class_comparison$x_tick_font_size,
                                     legend_font_size = self$params$class_comparison$legend_font_size,
                                     width = NULL,
                                     height = NULL){
      # Get table
      data_table = self$table_check_convert(data_table)

      # Process fonts
      xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
      ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
      legend_show = base::ifelse(legend_font_size > 0, T, F)
      y_axis_title = base::ifelse(y_label_font_size > 0, "Concentration", "")
      title = base::ifelse(title_font_size > 0, "Class comparison", "")

      # Get sample groups and the list of classes
      meta_table = meta_table[!is.na(meta_table[,group_col]),]
      groups = as.character(sort(unique(meta_table[,group_col])))
      meta_table[,group_col] = as.character(meta_table[,group_col])
      class_list = colnames(data_table)

      plot_dims = calculate_subplot_grid_dimensions(length(class_list))
      x_dim = plot_dims$cols
      y_dim = plot_dims$rows

      # Colors
      colors = get_color_palette(groups = groups,
                                 color_palette = color_palette)

      # Plot list will be the list of subplots
      plot_list = c()

      # Cleared groups is created for the legends
      cleared_groups = c()
      j = 1
      for (c in class_list) {
        subplot = plot_ly(colors = unname(colors),
                          width = width,
                          height = height,
                          hovertemplate = paste("Group: %{x}<br>",
                                                "Value: %{y:.3g}%",
                                                "<extra></extra>"))
        for (g in groups){
          if (g %in% cleared_groups) {
            first_bool = FALSE
          }else{
            first_bool = legend_show
            cleared_groups = c(cleared_groups, g)
          }

          # For each class, each group
          s = rownames(meta_table)[meta_table[, group_col] == g] # Get the samples for the current group
          d = data_table[s, c] # Get the concentrations for all s samples in the current class c
          m = mean(d) # Get the mean concentration for samples s for class c

          # Subplot for the bar chart displaying the mean concentration
          subplot = plotly::add_trace(p = subplot,
                                      x = g,
                                      y = m,
                                      type  = "bar",
                                      name = g,
                                      color = colors[g],
                                      alpha = 1,
                                      legendgroup=g,
                                      showlegend = first_bool)

          # Subplot for boxplots displaying the median and all datapoints
          subplot = plotly::add_trace(p = subplot,
                                      x = g,
                                      y = d,
                                      type  = "box",
                                      boxpoints = "all",
                                      pointpos = 0,
                                      name = g,
                                      color = colors[g],
                                      line = list(color = 'rgb(100,100,100)'),
                                      marker = list(color = 'rgb(100,100,100)'),
                                      alpha = 1,
                                      legendgroup=g,
                                      showlegend = FALSE,
                                      text = s,
                                      hoverinfo = "text")

          subplot = plotly::add_annotations(
            p = subplot,
            text = paste0("<b>", c, "</b>"),
            x = 0.5,
            y = 1,
            yref = "paper",
            xref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = x_tick_font_size)
          )

        }

        subplot = plotly::layout(
          p = subplot,
          xaxis= list(showticklabels = FALSE),
          yaxis = list(showticklabels = xtick_show,
                       tickfont = list(size = y_tick_font_size)
          ),
          shapes = list(
            list(
              type = "rect",
              x0 = 0,
              x1 = 1,
              y0 = 1.,
              y1 = 1.2,
              yref = "paper",
              xref = "paper",
              fillcolor = "#0255e9",
              opacity = 0.4,
              line = list(color = "#0255e9",
                          width = 1,
                          opacity = 0.4)
            )
          )
        )

        plot_list[[j]] = plotly_build(subplot)
        j = j + 1
      }

      fig = subplot(plot_list, nrows = y_dim, margin = 0.035, titleX = TRUE)

      fig = plotly::layout(p = fig,

                           # title = list(text = title,
                           #              font = list(size = title_font_size)),

                           legend = list(orientation = 'h',
                                         xanchor = "center",
                                         x = 0.5,
                                         font = list(size = legend_font_size)),
                           plot_bgcolor='rgba(0,0,0,0)',
                           paper_bgcolor='rgba(0,0,0,0)'
      )




      self$plots$class_comparison = fig
    },






    ## Volcano plot
    plot_volcano = function(data_table = self$tables$volcano_table,
                            adjustment = self$params$volcano_plot_comparison$adjustment,
                            group_1 = self$params$volcano_plot_comparison$group_1,
                            group_2 = self$params$volcano_plot_comparison$group_2,
                            feature_metadata = self$params$volcano_plot$feature_metadata,
                            keep_significant = self$params$volcano_plot$keep_significant,
                            displayed_plot = self$params$volcano_plot$displayed_plot,
                            p_val_threshold = self$params$volcano_plot$p_val_threshold,
                            fc_threshold = self$params$volcano_plot$fc_threshold,
                            marker_size = self$params$volcano_plot$marker_size,
                            opacity = self$params$volcano_plot$opacity,
                            color_palette = self$params$volcano_plot$color_palette,
                            reverse_palette = self$params$volcano_plot$reverse_palette,
                            title_font_size = self$params$volcano_plot$title_font_size,
                            y_label_font_size = self$params$volcano_plot$y_label_font_size,
                            y_tick_font_size = self$params$volcano_plot$y_tick_font_size,
                            x_label_font_size = self$params$volcano_plot$x_label_font_size,
                            x_tick_font_size = self$params$volcano_plot$x_tick_font_size,
                            legend_font_size = self$params$volcano_plot$legend_font_size,
                            width = NULL,
                            height = NULL){

      if (class(data_table)[1] == 'character') {
        data_table = self$table_check_convert(data_table)
      }

      p_val_threshold = as.numeric(p_val_threshold)
      fc_threshold = as.numeric(fc_threshold)
      marker_size = as.numeric(marker_size)
      opacity = as.numeric(opacity)

      if (adjustment == 'none') {
        p_val_label = "p-value"
        log_p_val_label =  "-Log10(p-value)"
        y_label = '-Log10(p-value)'
      }else{
        p_val_label = "p-adjusted"
        log_p_val_label = "-Log10(p-adjusted)"
        y_label = paste0('-Log10(', adjustment, '(p-value))')
      }

      if (keep_significant) {
        data_table = data_table[(data_table$`Log2(fold change)` >= log2(fc_threshold)) | (data_table$`Log2(fold change)` <= -log2(fc_threshold)),]
        data_table = data_table[data_table[,p_val_label] <= p_val_threshold,]
      }

      if (!is.null(feature_metadata)) {
        if (length(feature_metadata) == 1) {
          if (feature_metadata %in% colnames(data_table)) {
            groups = data_table[,feature_metadata]
          } else {
            groups = NULL
          }
        } else if (length(feature_metadata) > 1) {
          groups = feature_metadata[rownames(data_table)]
        } else {
          groups = NULL
        }
      }


      displayed_text = paste0(rownames(data_table),
                              '\n',
                              p_val_label,
                              ': ',
                              format_values(values = data_table[,p_val_label]),
                              '\nFold change: ',
                              format_values(data_table$`Fold change`)
      )


      plot = volcano_main(fc_vals = data_table$`Fold change`,
                          p_vals = data_table[,p_val_label],
                          names = displayed_text,
                          y_label = y_label,
                          left_label = group_1,
                          right_label = group_2,
                          groups = groups,
                          displayed_plot = displayed_plot,
                          color_palette = color_palette,
                          reverse_palette = reverse_palette,
                          p_val_threshold = p_val_threshold,
                          fc_threshold = fc_threshold,
                          marker_size = marker_size,
                          opacity = opacity,
                          title_font_size = title_font_size,
                          y_label_font_size = y_label_font_size,
                          y_tick_font_size = y_tick_font_size,
                          x_label_font_size = x_label_font_size,
                          x_tick_font_size = x_tick_font_size,
                          legend_font_size = legend_font_size)

      volcano_plot_table = data_table[, c('Log2(fold change)', log_p_val_label)]
      volcano_plot_table$expression = NA

      if (!is.null(groups)) {
        if (length(feature_metadata) == 1) {
          volcano_plot_table[,feature_metadata] = groups
        } else if (length(feature_metadata) > 1){
          volcano_plot_table[,'feature_annotations'] = groups
        }

      }


      log_p_val_threshold = -log10(p_val_threshold)
      volcano_plot_table$expression = 'Inconclusive'
      volcano_plot_table$expression[(volcano_plot_table[,log_p_val_label] <= log_p_val_threshold) & (volcano_plot_table$`Log2(fold change)` < log2(fc_threshold)) & (volcano_plot_table$`Log2(fold change)` > -log2(fc_threshold))] = 'Not significant'
      volcano_plot_table$expression[((volcano_plot_table[,log_p_val_label] >= log_p_val_threshold) | (is.na(volcano_plot_table[,log_p_val_label]))) & (volcano_plot_table$`Log2(fold change)` > log2(fc_threshold))] = "Overexpressed"
      volcano_plot_table$expression[((volcano_plot_table[,log_p_val_label] >= log_p_val_threshold) | (is.na(volcano_plot_table[,log_p_val_label]))) & (volcano_plot_table$`Log2(fold change)` < -log2(fc_threshold))] = "Underexpressed"
      rownames(volcano_plot_table) = rownames(data_table)
      volcano_plot_table = volcano_plot_table[!(is.na(volcano_plot_table[,log_p_val_label]) & is.infinite(volcano_plot_table$`Log2(fold change)`)), ]

      self$plots$volcano_plot = plot
      self$tables$volcano_plot = volcano_plot_table
    },

    ## Heatmap plot
    plot_heatmap = function(dataset = self$params$heatmap$dataset,
                            distance_method = self$params$heatmap$distance_method,
                            clustering_method = self$params$heatmap$clustering_method,
                            impute_min = self$params$heatmap$impute_min,
                            center = self$params$heatmap$center,
                            meta_table = self$tables$raw_meta,
                            meta_table_features = self$tables$raw_feat,
                            apply_clustering = self$params$heatmap$apply_clustering,
                            k_clusters_samples = self$params$heatmap$k_clusters_samples,
                            k_clusters_features = self$params$heatmap$k_clusters_features,
                            row_annotations = self$params$heatmap$map_sample_data,
                            col_annotations = self$params$heatmap$map_feature_data,
                            map_feature_terms = self$params$heatmap$map_feature_terms,
                            apply_da = self$params$heatmap$apply_da,
                            group_column_da = self$params$heatmap$group_column_da,
                            alpha_da = self$params$heatmap$alpha_da,
                            seed_da = self$params$heatmap$seed_da,
                            color_palette = self$params$heatmap$color_palette,
                            reverse_palette = self$params$heatmap$reverse_palette,
                            title_font_size = self$params$heatmap$title_font_size,
                            y_label_font_size = self$params$heatmap$y_label_font_size,
                            x_label_font_size = self$params$heatmap$x_label_font_size,
                            x_tick_font_size = self$params$heatmap$x_tick_font_size,
                            y_tick_font_size = self$params$heatmap$y_tick_font_size,
                            width = NULL,
                            height = NULL) {

      data_table = self$table_check_convert(dataset)

      xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
      ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
      x_axis_title = base::ifelse(x_label_font_size > 0, paste0('<span style="font-size: ', x_label_font_size, 'px;">Samples</span>'), "")
      y_axis_title = base::ifelse(y_label_font_size > 0, paste0('<span style="font-size: ', y_label_font_size, 'px;">Features</span>'), "")
      title = base::ifelse(title_font_size > 0, paste0('<span style="font-size: ', title_font_size, 'px;">Heatmap</span>'), "")



      # Impute missing values
      if (impute_min) {
        data_table[is.na(data_table)] = min(data_table, na.rm = TRUE)
      }

      if (apply_da) {
        data_table = apply_discriminant_analysis(data_table = data_table,
                                                 group_list = meta_table[,group_column_da],
                                                 nlambda = 100,
                                                 alpha = alpha_da,
                                                 seed = seed_da)

        meta_table = meta_table[rownames(data_table), , drop = F]
        meta_table_features = meta_table_features[colnames(data_table), , drop = F]

      }

      # Set the clustering
      if (apply_clustering) {
        dendrogram = "both"
        Colv= stats::hclust(d = stats::dist(x = data_table,
                                            method = distance_method),
                            method = clustering_method)
        Rowv = stats::hclust(d = stats::dist(x = t(data_table),
                                             method = distance_method),
                             method = clustering_method)
      } else {
        dendrogram = "none"
        Colv= NULL
        Rowv = NULL
      }


      # Set zmax and zmin
      if (center) {
        if (min(data_table, na.rm = T) < 0) {
          zmax = min(c(max(data_table, na.rm = T), - min(data_table, na.rm = T)))
          zmin = -zmax
          data_table[data_table > zmax] = zmax
          data_table[data_table < zmin] = zmin
        } else {
          zmax = max(data_table, na.rm = T)
          zmin = min(data_table, na.rm = T)
        }
      } else {
        zmax = max(data_table, na.rm = T)
        zmin = min(data_table, na.rm = T)
      }

      # Annotations
      if (!is.null(row_annotations)) {
        row_annotations = meta_table[, row_annotations, drop = FALSE]
      }

      # Reorder the feature metadata according to the data_table order
      meta_table_features = meta_table_features[colnames(data_table), , drop = F]

      if (!is.null(col_annotations)) {
        col_annotations = meta_table_features[, col_annotations, drop = FALSE]
      }

      # Save k clusters
      if (!is.null(Colv)) {
        clusters = stats::cutree(tree = Colv, k = k_clusters_samples)
        clusters = paste0('k', clusters)
        meta_table[,'k_clusters_heatmap'] = clusters
      }
      if (!is.null(Rowv)) {
        clusters = stats::cutree(tree = Rowv, k = k_clusters_features)
        clusters = paste0('k', clusters)
        meta_table_features[,'k_clusters_heatmap'] = clusters
      }

      # Add multivalue annotations
      if (!is.null(map_feature_terms)) {
        if (is.null(col_annotations)) {
          col_annotations = as.data.frame(meta_table_features[, NULL])
        }
        for (name in names(map_feature_terms))
          col_annotations[[name]] = map_feature_terms[[name]][rownames(col_annotations)]
      }

      # Get the color palette
      colors = get_color_palette(groups = 1:10,
                                 color_palette = color_palette,
                                 reverse_color_palette = reverse_palette,
                                 force_scale = F,
                                 force_list = F)

      # Plot the data
      plot = heatmaply::heatmaply(x = t(data_table),
                                  colors = colors,
                                  plot_method = "ggplot", # plotly
                                  # colorbar_len = 0.5,
                                  # colorbar_yanchor = "top",
                                  # colorbar_xpos = 1.15,
                                  # colorbar_ypos = 0.7,
                                  # subplot_widths = c(0.7, 0.05, 0.1),
                                  k_col = k_clusters_samples,
                                  k_row = k_clusters_features,
                                  limits = c(zmin, zmax),
                                  Colv= Colv,
                                  Rowv = Rowv,
                                  width = width,
                                  height = height,
                                  col_side_colors = row_annotations,
                                  row_side_colors = col_annotations,
                                  xlab = x_axis_title,
                                  ylab = y_axis_title,
                                  main = title,
                                  label_names = c("Feature", "Sample", "Value"),
                                  fontsize_row = x_tick_font_size,
                                  fontsize_col = y_tick_font_size,
                                  showticklabels = c(xtick_show, ytick_show),
                                  dendrogram = dendrogram)

      plot = plotly::layout(
        p = plot,
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)'
      )





      meta_table_features = meta_table_features[, 'k_clusters_heatmap', drop = F]
      meta_table = meta_table[, 'k_clusters_heatmap', drop = F]
      self$tables$heatmap = list(data_table = data_table,
                                 feature_clusters = meta_table_features,
                                 sample_clusters = meta_table)
      self$plots$heatmap = plot
    },

    ## Sample correlation plot
    plot_samples_correlation = function(dataset = self$params$samples_correlation$dataset,
                                        meta_table = self$tables$raw_meta,
                                        correlation_method = self$params$samples_correlation$correlation_method,
                                        use = self$params$samples_correlation$use,
                                        distance_method = self$params$samples_correlation$distance_method,
                                        clustering_method = self$params$samples_correlation$clustering_method,
                                        k_clusters = self$params$samples_correlation$k_clusters,
                                        apply_clustering = self$params$samples_correlation$apply_clustering,
                                        center = self$params$samples_correlation$center,
                                        row_annotations = self$params$samples_correlation$row_annotations,
                                        col_annotations = self$params$samples_correlation$col_annotations,
                                        color_palette = self$params$samples_correlation$color_palette,
                                        reverse_palette = self$params$samples_correlation$reverse_palette,
                                        title_font_size = self$params$samples_correlation$title_font_size,
                                        y_label_font_size = self$params$samples_correlation$y_label_font_size,
                                        y_tick_font_size = self$params$samples_correlation$y_tick_font_size,
                                        x_label_font_size = self$params$samples_correlation$x_label_font_size,
                                        x_tick_font_size = self$params$samples_correlation$x_tick_font_size,
                                        width = NULL,
                                        height = NULL) {

      # Checks
      if (!is.null(k_clusters)) {
        if (is_coercible_to_numeric(k_clusters)) {
          k_clusters = as.numeric(k_clusters)
        } else {
          k_clusters = NULL
        }
      }
      if (is.null(k_clusters)) {
        k_clusters = 1
      }

      data_table = self$table_check_convert(dataset)

      xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
      ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
      x_axis_title = base::ifelse(x_label_font_size > 0, paste0('<span style="font-size: ', x_label_font_size, 'px;">Samples</span>'), "")
      y_axis_title = base::ifelse(y_label_font_size > 0, paste0('<span style="font-size: ', y_label_font_size, 'px;">Samples</span>'), "")
      title = base::ifelse(title_font_size > 0, paste0('<span style="font-size: ', title_font_size, 'px;">Sample correlation</span>'), "")

      # R fast version
      # sample_names = rownames(data_table)
      # data_table = faster_cor(data_table = t(data_table),
      #                         method = correlation_method)
      # rownames(data_table) = sample_names
      # colnames(data_table) = sample_names

      # stats version
      data_table = stats::cor(x = t(data_table),
                              use = use,
                              method = correlation_method)

      # Set the clustering
      if (apply_clustering) {
        dendrogram = "both"
        Colv= stats::hclust(d = stats::dist(x = data_table,
                                            method = distance_method),
                            method = clustering_method)
        Rowv = stats::hclust(d = stats::dist(x = data_table,
                                             method = distance_method),
                             method = clustering_method)
      } else {
        dendrogram = "none"
        Colv= NULL
        Rowv = NULL
      }
      diag(data_table) = NA


      # Set zmax and zmin
      if (center) {
        if (min(data_table, na.rm = T) < 0) {
          zmax = min(c(max(data_table, na.rm = T), - min(data_table, na.rm = T)))
          zmin = -zmax
          data_table[data_table > zmax] = zmax
          data_table[data_table < zmin] = zmin
        } else {
          zmax = max(data_table, na.rm = T)
          zmin = min(data_table, na.rm = T)
        }
      } else {
        zmax = max(data_table, na.rm = T)
        zmin = min(data_table, na.rm = T)
      }
      diag(data_table) = zmax


      if (!is.null(Colv)) {
        clusters = stats::cutree(tree = Colv, k = k_clusters)
        clusters = paste0('k', clusters)
        meta_table[,'k_clusters'] = clusters
      }


      # Annotations
      if (!is.null(row_annotations)) {
        if (length(row_annotations) > 1) {
          row_annotations = meta_table[, row_annotations]
          colnames(row_annotations) = stringr::str_replace_all(colnames(row_annotations), "_", " ")
        } else {
          row_names = row_annotations
          row_annotations = as.data.frame(meta_table[, row_annotations],
                                          row.names = rownames(meta_table))
          colnames(row_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      if (!is.null(col_annotations)) {
        if (length(col_annotations) > 1) {
          col_annotations = meta_table[, col_annotations]
          colnames(col_annotations) = stringr::str_replace_all(colnames(col_annotations), "_", " ")
        } else {
          row_names = col_annotations
          col_annotations = as.data.frame(meta_table[, col_annotations],
                                          row.names = rownames(meta_table))
          colnames(col_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      # Save table as heatmap table
      meta_table = meta_table[Colv$order, ]
      self$tables$samples_correlation = data_table
      self$tables$samples_correlation_clusters = meta_table

      # Get the color palette
      color_count = colors_switch(color_palette)
      colors = get_colors(color_count = color_count, color_palette = color_palette)
      if (reverse_palette) {
        colors = base::rev(colors)
      }

      # Plot the data
      plot = heatmaply::heatmaply(x = t(data_table),
                                  colors = colors,
                                  limits = c(zmin, zmax),
                                  Colv = Colv,
                                  Rowv = Rowv,
                                  k_col = k_clusters,
                                  k_row = k_clusters,
                                  # Aesthetics
                                  width = width,
                                  height = height,
                                  col_side_colors = row_annotations,
                                  row_side_colors = col_annotations,
                                  xlab = x_axis_title,
                                  ylab = y_axis_title,
                                  main = title,
                                  fontsize_row = x_tick_font_size,
                                  fontsize_col = y_tick_font_size,
                                  showticklabels = c(xtick_show, ytick_show),
                                  dendrogram = dendrogram)

      plot = plotly::layout(
        p = plot,
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)'
      )

      self$plots$samples_correlation = plot

    },

    ## Feature correlation plot
    plot_feature_correlation = function(dataset = self$params$feature_correlation$dataset,
                                        meta_table = self$tables$raw_feat,
                                        map_feature_terms = self$params$feature_correlation$map_feature_terms,
                                        correlation_method = self$params$feature_correlation$correlation_method,
                                        use = self$params$feature_correlation$use,
                                        distance_method = self$params$feature_correlation$distance_method,
                                        clustering_method = self$params$feature_correlation$clustering_method,
                                        k_clusters = self$params$feature_correlation$k_clusters,
                                        apply_clustering = self$params$feature_correlation$apply_clustering,
                                        center = self$params$feature_correlation$center,
                                        row_annotations = self$params$feature_correlation$row_annotations,
                                        col_annotations = self$params$feature_correlation$col_annotations,
                                        roh_threshold = self$params$feature_correlation$roh_threshold,
                                        top_features = self$params$feature_correlation$top_features,
                                        color_palette = self$params$feature_correlation$color_palette,
                                        reverse_palette = self$params$feature_correlation$reverse_palette,
                                        title_font_size = self$params$feature_correlation$title_font_size,
                                        y_label_font_size = self$params$feature_correlation$y_label_font_size,
                                        y_tick_font_size = self$params$feature_correlation$y_tick_font_size,
                                        x_label_font_size = self$params$feature_correlation$x_label_font_size,
                                        x_tick_font_size = self$params$feature_correlation$x_tick_font_size,
                                        legend_font_size = self$params$feature_correlation$legend_font_size,
                                        width = NULL,
                                        height = NULL) {

      # Checks
      if (!is.null(k_clusters)) {
        if (is_coercible_to_numeric(k_clusters)) {
          k_clusters = as.numeric(k_clusters)
        } else {
          k_clusters = NULL
        }
      }
      if (is.null(k_clusters)) {
        k_clusters = 1
      }

      data_table = self$table_check_convert(dataset)

      xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
      ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
      x_axis_title = base::ifelse(x_label_font_size > 0, paste0('<span style="font-size: ', x_label_font_size, 'px;">Features</span>'), "")
      y_axis_title = base::ifelse(y_label_font_size > 0, paste0('<span style="font-size: ', y_label_font_size, 'px;">Features</span>'), "")
      title = base::ifelse(title_font_size > 0, paste0('<span style="font-size: ', title_font_size, 'px;">Feature correlation</span>'), "")

      # R fast version
      # feature_names = colnames(data_table)
      # data_table = faster_cor(data_table = data_table,
      #                         method=correlation_method)
      # rownames(data_table) = feature_names
      # colnames(data_table) = feature_names

      # stats version
      data_table = stats::cor(x = data_table,
                              use = use,
                              method = correlation_method)


      diag(data_table) = NA
      max_abs_values = apply(data_table, 1, function(x) max(abs(x), na.rm = T))
      roh_filter = unname(which(max_abs_values >= roh_threshold))


      if (length(roh_filter) > top_features) {
        best_hits = names(sort(rowSums(abs(data_table), na.rm = T), decreasing = T)[1:top_features])
        roh_filter = which((rownames(data_table) %in% best_hits))
      }
      data_table = data_table[roh_filter, roh_filter]

      meta_table = meta_table[rownames(data_table), , drop = F]

      # Set zmax and zmin
      if (center) {
        if (min(data_table, na.rm = T) < 0) {
          zmax = min(c(max(data_table, na.rm = T), - min(data_table, na.rm = T)))
          zmin = -zmax
          data_table[data_table > zmax] = zmax
          data_table[data_table < zmin] = zmin
        } else {
          zmax = max(data_table, na.rm = T)
          zmin = min(data_table, na.rm = T)
        }
      } else {
        zmax = max(data_table, na.rm = T)
        zmin = min(data_table, na.rm = T)
      }
      diag(data_table) = zmax


      # Set the clustering
      if (apply_clustering) {
        dendrogram = "both"
        Colv= stats::hclust(d = stats::dist(x = t(data_table),
                                            method = distance_method),
                            method = clustering_method)
        Rowv = stats::hclust(d = stats::dist(x = t(data_table),
                                             method = distance_method),
                             method = clustering_method)
      } else {
        dendrogram = "none"
        Colv= NULL
        Rowv = NULL
      }

      if (!is.null(Colv)) {
        clusters = stats::cutree(tree = Colv, k = k_clusters)
        clusters = paste0('k', clusters)
        meta_table[,'k_clusters'] = clusters
      }

      # Annotations
      if (!is.null(row_annotations)) {
        if (length(row_annotations) > 1) {
          row_annotations = meta_table[, row_annotations]
          colnames(row_annotations) = stringr::str_replace_all(colnames(row_annotations), "_", " ")
        } else {
          row_names = row_annotations
          row_annotations = as.data.frame(meta_table[, row_annotations],
                                          row.names = rownames(meta_table))
          colnames(row_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      if (!is.null(col_annotations)) {
        if (length(col_annotations) > 1) {
          col_annotations = meta_table[, col_annotations]
          colnames(col_annotations) = stringr::str_replace_all(colnames(col_annotations), "_", " ")
        } else {
          row_names = col_annotations
          col_annotations = as.data.frame(meta_table[, col_annotations],
                                          row.names = rownames(meta_table))
          colnames(col_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      # Add multivalue annotations
      if (!is.null(map_feature_terms)) {
        if (is.null(col_annotations)) {
          col_annotations = as.data.frame(meta_table[, NULL])
        }
        for (name in names(map_feature_terms))
          col_annotations[[name]] = map_feature_terms[[name]][rownames(col_annotations)]
      }

      # Save table as heatmap table
      meta_table = meta_table[rownames(meta_table)[Colv$order], , drop = F]

      # Get the color palette
      color_count = colors_switch(color_palette)
      colors = get_colors(color_count = color_count, color_palette = color_palette)
      if (reverse_palette) {
        colors = base::rev(colors)
      }


      # Plot the data
      plot = heatmaply::heatmaply(x = data_table,
                                  colors = colors,
                                  limits = c(zmin, zmax),
                                  Colv = Colv,
                                  Rowv = Rowv,
                                  k_col = k_clusters,
                                  k_row = k_clusters,
                                  width = width,
                                  height = height,
                                  col_side_colors = row_annotations,
                                  row_side_colors = col_annotations,
                                  xlab = x_axis_title,
                                  ylab = y_axis_title,
                                  main = title,
                                  fontsize_row = x_tick_font_size,
                                  fontsize_col = y_tick_font_size,
                                  showticklabels = c(xtick_show, ytick_show),
                                  dendrogram = dendrogram)

      plot = plotly::layout(
        p = plot,
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)'
      )

      self$plots$feature_correlation = plot
      self$tables$feature_correlation = data_table
      self$tables$feature_correlation_clusters = meta_table

    },

    ## PCA scores and loading plots
    plot_pca = function(data_table = self$params$pca$data_table,
                        meta_table = self$tables$raw_meta,
                        feature_table = self$tables$raw_feat,
                        sample_groups_col = self$params$pca$sample_groups_col,
                        feature_groups_col = self$params$pca$feature_groups_col,
                        impute_median = self$params$pca$impute_median,
                        apply_da = self$params$pca$apply_da,
                        sample_groups_da = self$params$pca$sample_groups_da,
                        alpha_da = self$params$pca$alpha_da,
                        seed_da = self$params$pca$seed_da,
                        pca_method = self$params$pca$pca_method,
                        nPcs = self$params$pca$nPcs,
                        displayed_pc_1 = self$params$pca$displayed_pc_1,
                        displayed_pc_2 = self$params$pca$displayed_pc_2,
                        completeObs = self$params$pca$completeObs,
                        displayed_plots = self$params$pca$displayed_plots,
                        colors_palette = self$params$pca$colors_palette,
                        marker_size = self$params$pca$marker_size,
                        opacity = self$params$pca$opacity,
                        title_font_size = self$params$pca$title_font_size,
                        y_label_font_size = self$params$pca$y_label_font_size,
                        y_tick_font_size = self$params$pca$y_tick_font_size,
                        x_label_font_size = self$params$pca$x_label_font_size,
                        x_tick_font_size = self$params$pca$x_tick_font_size,
                        legend_font_size = self$params$pca$legend_font_size,
                        return_data = TRUE,
                        width = NULL,
                        height = NULL) {



      data_table = self$table_check_convert(data_table)
      data_table = remove_empty_cols(table = data_table)

      alpha_da = as.numeric(alpha_da)
      nPcs= as.numeric(nPcs)
      displayed_pc_1 = as.numeric(displayed_pc_1)
      displayed_pc_2 = as.numeric(displayed_pc_2)
      marker_size = as.numeric(marker_size)
      opacity = as.numeric(opacity)

      if (is.character(data_table)) {
        data_table = self$tables[[data_table]]
      }

      sample_groups = meta_table[rownames(data_table),sample_groups_col]
      da_groups = meta_table[rownames(data_table),sample_groups_col]

      if (impute_median) {
        for (col in colnames(data_table)) {
          data_table[is.na(data_table[,col]),col] = stats::median(data_table[,col], na.rm = T)
        }
      }

      if (apply_da) {
        data_table = apply_discriminant_analysis(data_table = data_table,
                                                 group_list = da_groups,
                                                 nlambda = 100,
                                                 alpha = alpha_da,
                                                 seed = seed_da)

        sample_groups = meta_table[rownames(data_table),sample_groups_col]
      }


      if (!is.null(feature_groups_col) & !is.null(feature_table)) {
        if (length(feature_groups_col) == 1) {
          if (feature_groups_col %in% colnames(feature_table)) {
            feature_groups = feature_table[colnames(data_table),feature_groups_col]
            if (length(which(is.na(feature_groups))) < 30) {
              feature_groups[which(is.na(feature_groups))] = colnames(data_table)[which(is.na(feature_groups))]
            } else {
              feature_groups[which(is.na(feature_groups))] = "UNK"
            }
          } else {
            feature_groups = NULL
          }
        } else {
          feature_groups = feature_groups_col[colnames(data_table)]
        }
      } else {
        feature_groups = NULL
      }


      pca_out = pca_main(data_table = data_table,
                         sample_groups = sample_groups,
                         feature_groups = feature_groups,
                         nPcs = nPcs,
                         displayed_pc_1 = displayed_pc_1,
                         displayed_pc_2 = displayed_pc_2,
                         pca_method = pca_method,
                         completeObs = completeObs,
                         displayed_plots = displayed_plots,
                         colors_palette = colors_palette,
                         marker_size = marker_size,
                         opacity = opacity,
                         title_font_size = title_font_size,
                         y_label_font_size = y_label_font_size,
                         y_tick_font_size = y_tick_font_size,
                         x_label_font_size = x_label_font_size,
                         x_tick_font_size = x_tick_font_size,
                         legend_font_size = legend_font_size,
                         return_data = return_data)




      self$tables$pca_scores_table = pca_out$pca_data@scores
      self$tables$pca_loadings_table = pca_out$pca_data@loadings
      self$plots$pca_plot = pca_out$fig

    },

    ## FA analysis
    plot_fa_analysis = function(data_table = self$params$fa_analysis_plot$data_table,
                                feature_table = self$tables$raw_feat,
                                sample_meta = self$tables$raw_meta,
                                group_col = self$params$fa_analysis_plot$group_col,
                                selected_view = self$params$fa_analysis_plot$selected_view,
                                selected_lipidclass = self$params$fa_analysis_plot$selected_lipidclass,
                                selected_fa = self$params$fa_analysis_plot$selected_fa,
                                fa_norm = self$params$fa_analysis_plot$fa_norm,
                                color_palette = self$params$fa_analysis_plot$color_palette,
                                title_font_size = self$params$fa_analysis_plot$title_font_size,
                                y_label_font_size = self$params$fa_analysis_plot$y_label_font_size,
                                y_tick_font_size = self$params$fa_analysis_plot$y_tick_font_size,
                                x_label_font_size = self$params$fa_analysis_plot$x_label_font_size,
                                x_tick_font_size = self$params$fa_analysis_plot$x_tick_font_size,
                                legend_font_size = self$params$fa_analysis_plot$legend_font_size,
                                width = NULL,
                                height = NULL) {

      data_table = self$table_check_convert(data_table)

      ## At the moment this function is using the raw data table
      # do the calculations
      if(selected_view == "lipidclass") {
        res = fa_analysis_calc(data_table = data_table,
                               feature_table = feature_table,
                               sample_meta = sample_meta,
                               selected_lipidclass = selected_lipidclass,
                               fa_norm = fa_norm)
        # column names are fa tail names, rownames sample names
      } else if(selected_view == "fa") {
        res = fa_analysis_rev_calc(data_table = data_table,
                                   feature_table = feature_table,
                                   sample_meta = sample_meta,
                                   selected_fa = selected_fa,
                                   fa_norm = fa_norm)
      }

      # Produce the class x group table
      # add ID's, group's and make long
      res$ID = rownames(res)
      res$group = sample_meta[res$ID, group_col]
      res_long = res |>
        tidyr::pivot_longer(cols = -c(ID, group),
                            names_to = "names",
                            values_to = "value")

      # calculate mean and stdev per group
      plot_table = tapply(as.data.frame(res_long), list(res_long$group, res_long$names), function(x) {
        avg = mean(x[, "value"], na.rm = TRUE)
        stdev = sd(x[, "value"], na.rm = TRUE)

        return(list(avg = avg,
                    stdev = stdev,
                    names = x[1, "names"],
                    group = x[1, "group"]))
      })

      plot_table = do.call(rbind.data.frame, plot_table)

      # Store the plot_table
      self$tables$fa_analysis_table = plot_table

      # group_list = sort(unique(plot_table$group))
      colors = get_color_palette(groups = plot_table$group,
                                  color_palette = color_palette,
                                  reverse_color_palette = T)

      # set the main title for FA overview per lipid class
      if(selected_view == "lipidclass") {
        if(selected_lipidclass == "All") {
          main_title = "All lipid classes (incl. TG)"
        } else if(selected_lipidclass == "All_noTG") {
          main_title = "All lipid classes (excl. TG)"
        } else {
          main_title = paste0("Lipid class: ", selected_lipidclass)
        }
        xlabel = "Fatty acid chain"
      }

      if(fa_norm) {
        ylabel = "%"
      } else {
        ylabel = "Normalized value"
      }

      # set the main title for lipid class overview per fatty acids
      if(selected_view == "fa") {
        main_title = paste0("FA tails: ", paste(selected_fa, collapse = ", "))
        xlabel = "Lipid classes"
      }

      if (length(colors) > 1) {
        legend_label = paste0(group_col, ":")
        legend_tick_font_size = legend_font_size
      } else {
        legend_label = NULL
        legend_font_size = 0
        legend_tick_font_size = 0
      }

      fd = get_plot_font_data(title = main_title,
                              title_font_size = title_font_size,
                              x_label = xlabel,
                              x_label_font_size = x_label_font_size,
                              x_tick_font_size = x_tick_font_size,
                              y_label = ylabel,
                              y_label_font_size = y_label_font_size,
                              y_tick_font_size = y_tick_font_size,
                              legend_label = legend_label,
                              legend_font_size = legend_font_size)

      # plotting
      fig = plotly::plot_ly(colors = unname(colors),
                             width = width,
                             height = height)
      for (grp in unique(plot_table$group)) {
        fig = fig |>
          plotly::add_trace(data = plot_table[plot_table$group == grp, ],
                            x = ~names,
                            y = ~avg,
                            color = colors[grp],
                            type = "bar",
                            name = grp,
                            text = ~stdev,
                            error_y = ~ list(array = stdev,
                                             color = "#000000"),
                            hovertemplate = paste("Fatty acid chain: %{x}<br>",
                                                  "Value: %{y:.3g} +/- %{text:0.3g}<br>",
                                                  paste0("Group: ", grp),
                                                  "<extra></extra>"))
      }
      fig = fig |>
        plotly::layout(annotations =
                         list(x = 1,
                              y = -0.175,
                              text = "NOTE: error bars are standard deviation",
                              showarrow = FALSE,
                              xref = "paper",
                              yref = "paper",
                              xanchor = "right",
                              yanchor = "auto",
                              xshift = 0,
                              yshift = 0,
                              font = list(size = 10)),
                       title = list(text = fd$title,
                                    x = 0,
                                    xanchor = "left",
                                    font = list(size = fd$title_font_size)),
                       legend = list(orientation = 'h',
                                     xanchor = "center",
                                     x = 0.5,
                                     title = list(text = fd$legend_label),
                                     font = list(
                                       size = fd$legend_font_size
                                     )),
                       showlegend = fd$legend_show,
                       xaxis = list(
                         title = list(
                           text = fd$x_label,
                           font = list(size = fd$x_label_font_size)
                             ),
                         showticklabels = fd$x_tick_show,
                         tickfont = list(size = fd$x_tick_font_size)
                         ),
                       yaxis = list(
                         title = list(
                           text = fd$y_label,
                           font = list(size = fd$y_label_font_size)
                         ),
                         showticklabels = fd$y_tick_show,
                         tickfont = list(size = fd$y_tick_font_size)
                       )
        )
      fig

      self$plots$fa_analysis_plot = fig
    },

    # plot Fatty acid composition heatmaps
    plot_fa_comp = function(data_table = self$params$fa_comp_plot$data_table,
                            sample_meta = self$tables$raw_meta,
                            feature_table = self$tables$raw_feat,
                            composition = self$params$fa_comp_plot$composition,
                            group_col = self$params$fa_comp_plot$group_col,
                            group_1 = self$params$fa_comp_plot$group_1,
                            group_2 = self$params$fa_comp_plot$group_2,
                            selected_lipidclass = self$params$fa_comp_plot$selected_lipidclass,
                            color_palette = self$params$fa_comp_plot$color_palette,
                            reverse_palette = self$params$fa_comp_plot$reverse_palette,
                            title_font_size = self$params$fa_comp_plot$title_font_size,
                            y_label_font_size = self$params$fa_comp_plot$y_label_font_size,
                            y_tick_font_size = self$params$fa_comp_plot$y_tick_font_size,
                            x_label_font_size = self$params$fa_comp_plot$x_label_font_size,
                            x_tick_font_size = self$params$fa_comp_plot$x_tick_font_size,
                            legend_font_size = self$params$fa_comp_plot$legend_font_size,
                            width = NULL,
                            height = NULL) {

      data_table = self$table_check_convert(data_table)

      # Get the color palette
      # color_palette = get_color_palette(groups = c(group_1, group_2),
      #                                    color_palette = color_palette)

      ## left side
      # heatmap
      hm_left_data = fa_comp_hm_calc(data_table = data_table,
                                      sample_meta = sample_meta,
                                      composition = composition,
                                      feature_table = feature_table,
                                      group_col = group_col,
                                      selected_group = group_1,
                                      selected_lipidclass = selected_lipidclass)
      # bar left top
      bar_top_left_data = data.frame(x = factor(colnames(hm_left_data),
                                                 levels = sort(as.numeric(colnames(hm_left_data))),
                                                 labels = sort(as.numeric(colnames(hm_left_data)))),
                                      y = colSums(hm_left_data))
      avg_carbon_left = weighted.mean(x = as.numeric(as.character(bar_top_left_data$x)),
                                       w = bar_top_left_data$y)
      # bar left
      bar_left_data = data.frame(x = factor(rownames(hm_left_data),
                                             levels = sort(as.numeric(rownames(hm_left_data)), decreasing = TRUE),
                                             labels = sort(as.numeric(rownames(hm_left_data)), decreasing = TRUE)),
                                  y = rowSums(hm_left_data))
      avg_unsat_left = weighted.mean(x = as.numeric(as.character(bar_left_data$x)),
                                      w = bar_left_data$y)


      ## right side
      # heatmap
      hm_right_data = fa_comp_hm_calc(data_table = data_table,
                                       sample_meta = sample_meta,
                                       composition = composition,
                                       feature_table = feature_table,
                                       group_col = group_col,
                                       selected_group = group_2,
                                       selected_lipidclass = selected_lipidclass)
      # bar right top
      bar_top_right_data = data.frame(x = factor(colnames(hm_right_data),
                                                  levels = sort(as.numeric(colnames(hm_right_data))),
                                                  labels = sort(as.numeric(colnames(hm_right_data)))),
                                       y = colSums(hm_right_data))
      avg_carbon_right = weighted.mean(x = as.numeric(as.character(bar_top_right_data$x)),
                                        w = bar_top_right_data$y)
      # bar right
      bar_right_data = data.frame(x = factor(rownames(hm_right_data),
                                              levels = sort(as.numeric(rownames(hm_right_data)), decreasing = TRUE),
                                              labels = sort(as.numeric(rownames(hm_right_data)), decreasing = TRUE)),
                                   y = rowSums(hm_right_data))
      avg_unsat_right = weighted.mean(x = as.numeric(as.character(bar_right_data$x)),
                                       w = bar_right_data$y)


      # get the min and max value for the heatmap colorbar
      min_value = min(c(min(hm_left_data), min(hm_right_data)))
      max_value = max(c(max(hm_left_data), max(hm_right_data)))

      # Get font data
      fd = get_plot_font_data(title = base::ifelse(selected_lipidclass == "All",
                                             paste0("<b>Lipid class: ", selected_lipidclass, " (excl. PA)</b>"),
                                             paste0("<b>Lipid class: ", selected_lipidclass, "</b>")),
                              title_font_size = title_font_size,
                              x_label = "Number of carbon atoms",
                              x_label_font_size = x_label_font_size,
                              x_tick_font_size = x_tick_font_size,
                              y_label = "Proportion",
                              y_label_font_size = y_label_font_size,
                              y_tick_font_size = y_tick_font_size,
                              legend_label = "Proportion",
                              legend_font_size = legend_font_size)

      ## plots
      # left side
      fig_hm_left = fa_comp_heatmap(data = hm_left_data,
                                    vline = avg_carbon_left,
                                    hline = avg_unsat_left,
                                    composition = composition,
                                    color_limits = c(min_value, max_value),
                                    color_palette = color_palette,
                                    reverse_palette = reverse_palette,
                                    x_label_font_size = fd$x_label_font_size,
                                    x_tick_font_size = fd$x_tick_font_size,
                                    x_tick_show = fd$x_tick_show,
                                    y_label_font_size = fd$y_label_font_size,
                                    y_tick_font_size = fd$y_tick_font_size,
                                    y_tick_show = fd$y_tick_show,
                                    legend_label_font_size = fd$legend_font_size)


      fig_bar_top_left = plotly::plot_ly(
        data = bar_top_left_data,
        x = ~x,
        y = ~y,
        type = "bar",
        showlegend = FALSE,
        color = I("gray"),
        hovertemplate = paste("Number of carbons: %{x}<br>",
                              "Proportion: %{y:.3g}<br>",
                              "<extra></extra>")
      ) |>
        plotly::layout(
          xaxis = list(showticklabels = FALSE,
                       fixedrange = TRUE),
          yaxis = list(
            fixedrange = TRUE,
            title = list(
              text = fd$y_label,
              standoff = 3,
              font = list(
                size = fd$y_label_font_size
              )
            ),
            showticklabels = fd$y_tick_show,
            tickfont = list(size = fd$y_tick_font_size)
          )
        )

      fig_bar_left = plotly::plot_ly(
        data = bar_left_data,
        x = ~y,
        y = ~x,
        type = "bar",
        showlegend = FALSE,
        orientation = "h",
        color = I("gray"),
        hovertemplate = paste("Number of double bonds: %{y}<br>",
                              "Proportion: %{x:.3g}<br>",
                              "<extra></extra>")
      ) |>
        plotly::layout(
          xaxis = list(
            autorange = "reversed",
            fixedrange = TRUE,
            title = list(
              text = fd$y_label,
              standoff = 3,
              font = list(
                size = fd$y_label_font_size
              )
            ),
            showticklabels = fd$y_tick_show,
            tickfont = list(size = fd$y_tick_font_size)
          ),
          yaxis = list(
            showticklabels = FALSE,
            fixedrange = TRUE,
            title = "")
        )

      # right side
      fig_hm_right = fa_comp_heatmap(data = hm_right_data,
                                     vline = avg_carbon_right,
                                     hline = avg_unsat_right,
                                     composition = composition,
                                     color_limits = c(min_value, max_value),
                                     color_palette = color_palette,
                                     reverse_palette = reverse_palette,
                                     x_label_font_size = fd$x_label_font_size,
                                     x_tick_font_size = fd$x_tick_font_size,
                                     x_tick_show = fd$x_tick_show,
                                     y_label_font_size = fd$y_label_font_size,
                                     y_tick_font_size = fd$y_tick_font_size,
                                     y_tick_show = fd$y_tick_show,
                                     legend_label_font_size = fd$legend_font_size,
                                     y_pos_right = TRUE,
                                     showlegend = fd$legend_show)

      fig_bar_top_right = plotly::plot_ly(
        data = bar_top_right_data,
        x = ~x,
        y = ~y,
        type = "bar",
        showlegend = FALSE,
        color = I("gray"),
        hovertemplate = paste("Number of carbons: %{x}<br>",
                              "Proportion: %{y:.3g}<br>",
                              "<extra></extra>")
      )|>
        plotly::layout(
          xaxis = list(
            showticklabels = FALSE,
            fixedrange = TRUE),
          yaxis = list(
            fixedrange = TRUE,
            side = "right",
            title = list(
              text = fd$y_label,
              standoff = 3,
              font = list(
                size = fd$y_label_font_size
              )
            ),
            showticklabels = fd$y_tick_show,
            tickfont = list(size = fd$y_tick_font_size)
          )
        )

      fig_bar_right = plotly::plot_ly(
        data = bar_right_data,
        x = ~y,
        y = ~x,
        type = "bar",
        showlegend = FALSE,
        orientation = "h",
        color = I("gray"),
        hovertemplate = paste("Number of double bonds: %{y}<br>",
                              "Proportion: %{x:.3g}<br>",
                              "<extra></extra>")
      ) |>
        plotly::layout(
          yaxis = list(
            showticklabels = FALSE,
            fixedrange = TRUE,
            title = ""),
          xaxis = list(
            fixedrange = TRUE,
            title = list(
              text = fd$y_label,
              standoff = 3,
              font = list(
                size = fd$y_label_font_size
              )
            ),
            showticklabels = fd$y_tick_show,
            tickfont = list(size = fd$y_tick_font_size)
          )
        )

      # blank plot
      blank = plotly::plot_ly(type = "scatter", mode = "markers")
      blank = plotly::layout(blank,
                              xaxis = list(zeroline = FALSE,
                                           showticklabels = FALSE,
                                           showgrid = FALSE,
                                           fixedrange = TRUE),
                              yaxis = list(zeroline = FALSE,
                                           showticklabels = FALSE,
                                           showgrid = FALSE,
                                           fixedrange = TRUE))

      # set annotation for combined plots
      annotations = list(
        list(
          x = 0.3,
          y = 0.975,
          text = paste0("<b>", group_1, "</b>"),
          font = list(size = 12),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        ),
        list(
          x = 0.7,
          y = 0.975,
          text = paste0("<b>", group_2, "</b>"),
          font = list(size = 12),
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE
        )
      )

      # combine plots
      fig_top = plotly::subplot(list(blank, fig_bar_top_left, fig_bar_top_right, blank),
                                 nrows = 1,
                                 widths = c(0.1, 0.4, 0.4, 0.1),
                                 titleY = TRUE)

      fig_bottom = plotly::subplot(list(fig_bar_left, fig_hm_left, fig_hm_right, fig_bar_right),
                                    nrows = 1,
                                    widths = c(0.1, 0.4, 0.4, 0.1),
                                    titleX = TRUE,
                                    titleY = TRUE)

      fig = plotly::subplot(list(fig_top, fig_bottom),
                             nrows = 2,
                             heights = c(0.2, 0.75),
                             titleX = TRUE,
                             titleY = TRUE) |>
        plotly::layout(title = list(
          text = fd$title,
          font = list(
            size = fd$title_font_size
          )
        ),
        annotations = annotations) |>
        plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "select2d", "lasso2d"))

      self$plots$fa_comp_plot = fig
    },

    ## Double bond plot
    plot_double_bonds_plot = function(data_table = self$tables$double_bonds_table,
                                      adjustment = self$params$double_bonds_comparison$adjustment,
                                      group_1 = self$params$double_bonds_comparison$group_1,
                                      group_2 = self$params$double_bonds_comparison$group_2,
                                      carbon_selection = self$params$double_bonds_plot$selected_carbon_chain,
                                      unsat_selection = self$params$double_bonds_plot$selected_unsat,
                                      lipid_class = self$params$double_bonds_plot$selected_lipid_class,
                                      min_fc = self$params$double_bonds_plot$min_fc,
                                      max_pval = self$params$double_bonds_plot$max_pval,
                                      remove_missing_pval = self$params$double_bonds_plot$remove_missing_pval,
                                      remove_infitive_fc = self$params$double_bonds_plot$remove_infitive_fc,
                                      color_palette = self$params$double_bonds_plot$color_palette,
                                      reverse_palette = self$params$double_bonds_plot$reverse_palette,
                                      marker_size = self$params$double_bonds_plot$marker_size,
                                      marker_opacity = self$params$double_bonds_plot$marker_opacity,
                                      title_font_size = self$params$double_bonds_plot$title_font_size,
                                      y_label_font_size = self$params$double_bonds_plot$y_label_font_size,
                                      y_tick_font_size = self$params$double_bonds_plot$y_tick_font_size,
                                      x_label_font_size = self$params$double_bonds_plot$x_label_font_size,
                                      x_tick_font_size = self$params$double_bonds_plot$x_tick_font_size,
                                      legend_font_size = self$params$double_bonds_plot$legend_font_size,
                                      width = NULL,
                                      height = NULL){

      # Font sizes
      xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
      ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
      legend_show = base::ifelse(legend_font_size > 0, T, F)
      x_axis_title = base::ifelse(x_label_font_size > 0, carbon_selection, "")
      y_axis_title = base::ifelse(y_label_font_size > 0, unsat_selection, "")
      title = base::ifelse(title_font_size > 0, paste0("Comparison in ", lipid_class, " - ", group_1, " (low FC), ", group_2, " (high FC)"), "")

      if (adjustment == 'none') {
        pval_col = "p-value"
      } else {
        pval_col = "p-adjusted"
      }

      if (max_pval <= 0) {
        stop('max p-value must be > 0')
      }

      data_table = data_table[which(data_table["Lipid class"] == lipid_class),]

      x_lims = c(min(data_table[,carbon_selection]) -1, max(data_table[,carbon_selection]) +1)
      y_lims = c(min(data_table[,unsat_selection]) -0.5, max(data_table[,unsat_selection]) +1)


      if (min_fc > 0) {
        min_fc = log2(abs(min_fc))
        min_fc = c(-min_fc, min_fc)
        data_table = data_table[(data_table[,"Log2(fold change)"] <= min_fc[1]) | (data_table[,"Log2(fold change)"] >= min_fc[2]),]
      }

      pval_filter = data_table[,pval_col] <= max_pval
      pval_filter[is.na(pval_filter)] = T
      data_table = data_table[pval_filter, ]


      if (nrow(data_table) == 0) {
        base::stop('No features found within the selected p-value and fold change boundaries.')
      }

      data_table$hover = paste0(
        rownames(data_table),
        '\nFold change: ',
        format_values(values = data_table$`Fold change`),
        '\nLog2(fold change): ',
        format_values(data_table$`Log2(fold change)`),
        '\n',
        pval_col,
        ': ',
        format_values(data_table[,pval_col]),
        '\n',
        carbon_selection,
        ': ',
        format_values(data_table[,carbon_selection]),
        '\n',
        unsat_selection,
        ': ',
        format_values(data_table[,unsat_selection])
      )

      color_scale = get_color_palette(groups = data_table$`Log2(fold change)`,
                                      color_palette = color_palette,
                                      reverse_color_palette = reverse_palette,
                                      force_scale = T,
                                      force_list = F)

      # Impute NA and Inf
      if (remove_missing_pval) {
        data_table = data_table[!is.na(data_table[,pval_col]),]
      } else {
        data_table[is.na(data_table[,pval_col]), pval_col] = min(data_table[,pval_col], na.rm = T)
      }

      if (remove_infitive_fc) {
        data_table = data_table[base::is.finite(data_table$`Log2(fold change)`),]
      } else {
        finite_fc = data_table$`Log2(fold change)`
        finite_fc = finite_fc[base::is.finite(finite_fc)]
        data_table[data_table$`Log2(fold change)` == Inf, "Log2(fold change)"] = max(finite_fc)
        data_table[data_table$`Log2(fold change)` == -Inf, "Log2(fold change)"] = min(finite_fc)
      }


      # Plot
      plot = plotly::plot_ly(width = width, height = height)

      plot = plotly::add_trace(
        p = plot,
        x = data_table[,carbon_selection],
        y = data_table[,unsat_selection],
        type = "scatter",
        mode = "markers",
        size = -log10(data_table[,pval_col]),
        sizes = ~c(5,40),
        marker = list(color = data_table$`Log2(fold change)`,
                      sizemode ='diameter',
                      opacity = marker_opacity,
                      sizeref=1/marker_size,
                      colorscale = color_scale,
                      cmax = max(abs(data_table[, "Log2(fold change)"])),
                      cmin = -max(abs(data_table[, "Log2(fold change)"])),
                      colorbar=list(
                        title='Log2(fold change)',
                        tickfont = list(size = legend_font_size)
                      ),
                      showscale = legend_show,
                      line = list(width = 0,
                                  color = 'black')
        ),
        text = data_table$hover,
        hoverinfo = "text"
      )

      plot = plotly::layout(
        p = plot,

        title = list(text = title,
                     xref = "paper",
                     font = list(size = title_font_size)),

        xaxis = list(title = list(text = x_axis_title,
                                  font = list(size = x_label_font_size)),
                     showticklabels = xtick_show,
                     tickfont = list(size = x_tick_font_size),
                     range = x_lims
        ),
        yaxis = list(title = list(text = y_axis_title,
                                  font = list(size = y_label_font_size)),
                     showticklabels = ytick_show,
                     tickfont = list(size = y_tick_font_size),
                     range = y_lims
        ),
        legend= list(itemsizing='constant'),
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)'
      )

      self$plots$double_bonds_plot = plot
      self$tables$double_bonds_plot = data_table
    },

    plot_ea_dot_plot = function(object = self$tables$ea_object,
                                x = self$params$ea_dot_plot$x,
                                y = self$params$ea_dot_plot$y,
                                color = self$params$ea_dot_plot$color,
                                show_categories = self$params$ea_dot_plot$show_categories,
                                size = self$params$ea_dot_plot$size,
                                order_by = self$params$ea_dot_plot$order_by,
                                reverse_order = self$params$ea_dot_plot$reverse_order,
                                mode = self$params$ea_dot_plot$mode,
                                marker_opacity = self$params$ea_dot_plot$marker_opacity,
                                color_palette = self$params$ea_dot_plot$color_palette,
                                reverse_palette = self$params$ea_dot_plot$reverse_palette,
                                show_legend = self$params$ea_dot_plot$show_legend,
                                legend_size = self$params$ea_dot_plot$legend_size,
                                size_ref = self$params$ea_dot_plot$size_ref,
                                yaxis_word_split = self$params$ea_dot_plot$yaxis_word_split,
                                title_size = self$params$ea_dot_plot$title_size,
                                xlabel_size = self$params$ea_dot_plot$xlabel_size,
                                xtick_size = self$params$ea_dot_plot$xtick_size,
                                ytick_size = self$params$ea_dot_plot$ytick_size,
                                width = NULL,
                                height = NULL){


      out_data = plot_fa_dot_plot(object = object,
                                  x = x,
                                  y = y,
                                  color = color,
                                  show_categories = show_categories,
                                  size = size,
                                  order_by = order_by,
                                  reverse_order = reverse_order,
                                  mode = mode,
                                  marker_opacity = marker_opacity,
                                  color_palette = color_palette,
                                  reverse_palette = reverse_palette,
                                  show_legend = show_legend,
                                  legend_size = legend_size,
                                  size_ref = size_ref,
                                  yaxis_word_split = yaxis_word_split,
                                  title_size = title_size,
                                  xlabel_size = xlabel_size,
                                  xtick_size = xtick_size,
                                  ytick_size = ytick_size,
                                  width = width,
                                  height = height)

      self$plots$ea_dot_plot = out_data$plot
      self$tables$ea_dot_plot = out_data$table
    },

    plot_ora_dot_plot = function(object = self$tables$ora_object,
                                x = self$params$ora_dot_plot$x,
                                y = self$params$ora_dot_plot$y,
                                color = self$params$ora_dot_plot$color,
                                show_categories = self$params$ora_dot_plot$show_categories,
                                size = self$params$ora_dot_plot$size,
                                order_by = self$params$ora_dot_plot$order_by,
                                reverse_order = self$params$ora_dot_plot$reverse_order,
                                marker_opacity = self$params$ora_dot_plot$marker_opacity,
                                color_palette = self$params$ora_dot_plot$color_palette,
                                reverse_palette = self$params$ora_dot_plot$reverse_palette,
                                show_legend = self$params$ora_dot_plot$show_legend,
                                size_ref = self$params$ora_dot_plot$size_ref,
                                legend_size = self$params$ora_dot_plot$legend_size,
                                yaxis_word_split = self$params$ora_dot_plot$yaxis_word_split,
                                title_size = self$params$ora_dot_plot$title_size,
                                xlabel_size = self$params$ora_dot_plot$xlabel_size,
                                xtick_size = self$params$ora_dot_plot$xtick_size,
                                ytick_size = self$params$ora_dot_plot$ytick_size,
                                width = NULL,
                                height = NULL){


      out_data = plot_fa_dot_plot(object = object,
                                  x = x,
                                  y = y,
                                  color = color,
                                  show_categories = show_categories,
                                  size = size,
                                  order_by = order_by,
                                  reverse_order = reverse_order,
                                  mode = "Both",
                                  marker_opacity = marker_opacity,
                                  color_palette = color_palette,
                                  reverse_palette = reverse_palette,
                                  show_legend = show_legend,
                                  size_ref = size_ref,
                                  legend_size = legend_size,
                                  yaxis_word_split = yaxis_word_split,
                                  title_size = title_size,
                                  xlabel_size = xlabel_size,
                                  xtick_size = xtick_size,
                                  ytick_size = ytick_size,
                                  width = width,
                                  height = height)

      self$plots$ora_dot_plot = out_data$plot
      self$tables$ora_dot_plot = out_data$table
    },

    plot_ea_cnet_plot = function(x = self$tables$ea_object,
                                 ea_feature_table = self$tables$ea_feature_table,
                                 show_category = self$params$ea_cnet_plot$show_category,
                                 displayed_labels = self$params$ea_cnet_plot$displayed_labels,
                                 set_node_annotations = self$params$ea_cnet_plot$set_node_annotations,
                                 feature_node_annotations = self$params$ea_cnet_plot$feature_node_annotations,
                                 set_node_color_palette = self$params$ea_cnet_plot$set_node_color_palette,
                                 reverse_set_palette = self$params$ea_cnet_plot$reverse_set_palette,
                                 feature_node_color_palette = self$params$ea_cnet_plot$feature_node_color_palette,
                                 reverse_feature_palette = self$params$ea_cnet_plot$reverse_feature_palette,
                                 label_font_size = self$params$ea_cnet_plot$label_font_size,
                                 static_network = self$params$ea_cnet_plot$static_network,
                                 solver = self$params$ea_cnet_plot$solver,
                                 gravitationalConstant = self$params$ea_cnet_plot$gravitationalConstant,
                                 nodeDistance = self$params$ea_cnet_plot$nodeDistance,
                                 centralGravity = self$params$ea_cnet_plot$centralGravity,
                                 springLength = self$params$ea_cnet_plot$springLength,
                                 springConstant = self$params$ea_cnet_plot$springConstant,
                                 width = NULL,
                                 height = NULL) {

      out_data = plot_fa_cnet_plot(x = x,
                                   prot_list = ea_feature_table,
                                   show_category = show_category,
                                   displayed_labels = displayed_labels,
                                   set_node_annotations = set_node_annotations,
                                   feature_node_annotations = feature_node_annotations,
                                   set_node_color_palette = set_node_color_palette,
                                   reverse_set_palette = reverse_set_palette,
                                   feature_node_color_palette = feature_node_color_palette,
                                   reverse_feature_palette = reverse_feature_palette,
                                   label_font_size = label_font_size,
                                   static_network = static_network,
                                   solver = solver,
                                   gravitationalConstant = gravitationalConstant,
                                   nodeDistance = nodeDistance,
                                   centralGravity = centralGravity,
                                   springLength = springLength,
                                   springConstant = springConstant,
                                   width = width,
                                   height = height)

      self$plots$ea_cnet_plot = out_data$plot
      self$tables$ea_cnet_plot = out_data$table
    },

    plot_ora_cnet_plot = function(x = self$tables$ora_object,
                                  ora_feature_table = self$tables$ora_feature_table,
                                  show_category = self$params$ora_cnet_plot$show_category,
                                  displayed_labels = self$params$ora_cnet_plot$displayed_labels,
                                  set_node_annotations = self$params$ora_cnet_plot$set_node_annotations,
                                  feature_node_annotations = self$params$ora_cnet_plot$feature_node_annotations,
                                  set_node_color_palette = self$params$ora_cnet_plot$set_node_color_palette,
                                  reverse_set_palette = self$params$ora_cnet_plot$reverse_set_palette,
                                  feature_node_color_palette = self$params$ora_cnet_plot$feature_node_color_palette,
                                  reverse_feature_palette = self$params$ora_cnet_plot$reverse_feature_palette,
                                  label_font_size = self$params$ora_cnet_plot$label_font_size,
                                  static_network = self$params$ora_cnet_plot$static_network,
                                  solver = self$params$ora_cnet_plot$solver,
                                  gravitationalConstant = self$params$ora_cnet_plot$gravitationalConstant,
                                  nodeDistance = self$params$ora_cnet_plot$nodeDistance,
                                  centralGravity = self$params$ora_cnet_plot$centralGravity,
                                  springLength = self$params$ora_cnet_plot$springLength,
                                  springConstant = self$params$ora_cnet_plot$springConstant,
                                  width = NULL,
                                  height = NULL) {

      out_data = plot_fa_cnet_plot(x = x,
                                   prot_list = ora_feature_table,
                                   show_category = show_category,
                                   displayed_labels = displayed_labels,
                                   set_node_annotations = set_node_annotations,
                                   feature_node_annotations = feature_node_annotations,
                                   set_node_color_palette = set_node_color_palette,
                                   reverse_set_palette = reverse_set_palette,
                                   feature_node_color_palette = feature_node_color_palette,
                                   reverse_feature_palette = reverse_feature_palette,
                                   label_font_size = label_font_size,
                                   static_network = static_network,
                                   solver = solver,
                                   gravitationalConstant = gravitationalConstant,
                                   nodeDistance = nodeDistance,
                                   centralGravity = centralGravity,
                                   springLength = springLength,
                                   springConstant = springConstant,
                                   width = width,
                                   height = height)

      self$plots$ora_cnet_plot = out_data$plot
      self$tables$ora_cnet_plot = out_data$table
    },

    plot_ea_ridge_plot = function(object = self$tables$ea_object,
                                  show_category = self$params$ea_ridge_plot$show_category,
                                  fill = self$params$ea_ridge_plot$fill,
                                  core_enrichment = self$params$ea_ridge_plot$core_enrichment,
                                  color_palette = self$params$ea_ridge_plot$color_palette,
                                  reverse_palette = self$params$ea_ridge_plot$reverse_palette,
                                  displayed_label = self$params$ea_ridge_plot$displayed_label,
                                  orderBy = self$params$ea_ridge_plot$orderBy,
                                  decreasing = self$params$ea_ridge_plot$decreasing,
                                  title_font_size = self$params$ea_ridge_plot$title_font_size,
                                  yaxis_word_split = self$params$ea_ridge_plot$yaxis_word_split,
                                  y_label_font_size = self$params$ea_ridge_plot$y_label_font_size,
                                  y_tick_font_size = self$params$ea_ridge_plot$y_tick_font_size,
                                  x_label_font_size = self$params$ea_ridge_plot$x_label_font_size,
                                  x_tick_font_size = self$params$ea_ridge_plot$x_tick_font_size,
                                  legend_font_size = self$params$ea_ridge_plot$legend_font_size,
                                  width = NULL,
                                  height = NULL) {

      out_data = plot_fa_ridge_plot(object = object,
                                    show_category = show_category,
                                    fill = fill,
                                    core_enrichment = core_enrichment,
                                    color_palette = color_palette,
                                    reverse_palette = reverse_palette,
                                    displayed_label = displayed_label,
                                    orderBy = orderBy,
                                    decreasing = decreasing,
                                    title_font_size = title_font_size,
                                    yaxis_word_split = yaxis_word_split,
                                    y_label_font_size = y_label_font_size,
                                    y_tick_font_size = y_tick_font_size,
                                    x_label_font_size = x_label_font_size,
                                    x_tick_font_size = x_tick_font_size,
                                    legend_font_size = legend_font_size,
                                    width = width,
                                    height = height)

      self$plots$ea_ridge_plot = out_data$plot
      self$tables$ea_ridge_plot = out_data$table
    },

    plot_ea_emap_plot = function(x = self$tables$ea_object,
                                 show_category = self$params$ea_emap_plot$show_category,
                                 color = self$params$ea_emap_plot$color,
                                 size = self$params$ea_emap_plot$size,
                                 displayed_labels = self$params$ea_emap_plot$displayed_labels,
                                 mode = self$params$ea_emap_plot$mode,
                                 score_threshold = self$params$ea_emap_plot$score_threshold,
                                 similarity_score = self$params$ea_emap_plot$similarity_score,
                                 node_color_palette = self$params$ea_emap_plot$node_color_palette,
                                 reverse_node_palette = self$params$ea_emap_plot$reverse_node_palette,
                                 edge_width = self$params$ea_emap_plot$edge_width,
                                 edge_color = self$params$ea_emap_plot$edge_color,
                                 edge_color_palette = self$params$ea_emap_plot$edge_color_palette,
                                 reverse_edge_palette = self$params$ea_emap_plot$reverse_edge_palette,
                                 edge_magnifier = self$params$ea_emap_plot$edge_magnifier,
                                 node_magnifier = self$params$ea_emap_plot$node_magnifier,
                                 label_font_size = self$params$ea_emap_plot$label_font_size,
                                 static_network = self$params$ea_emap_plot$static_network,
                                 solver = self$params$ea_emap_plot$solver,
                                 gravitationalConstant = self$params$ea_emap_plot$gravitationalConstant,
                                 nodeDistance = self$params$ea_emap_plot$nodeDistance,
                                 centralGravity = self$params$ea_emap_plot$centralGravity,
                                 springLength = self$params$ea_emap_plot$springLength,
                                 springConstant = self$params$ea_emap_plot$springConstant,
                                 width = NULL,
                                 height = NULL) {

      out_data = plot_fa_emap_plot(x = x,
                                   show_category = show_category,
                                   color = color,
                                   size = size,
                                   displayed_labels = displayed_labels,
                                   score_threshold = score_threshold,
                                   similarity_score =similarity_score,
                                   node_color_palette = node_color_palette,
                                   reverse_node_palette = reverse_node_palette,
                                   edge_width = edge_width,
                                   edge_color = edge_color,
                                   edge_color_palette = edge_color_palette,
                                   reverse_edge_palette = reverse_edge_palette,
                                   edge_magnifier = edge_magnifier,
                                   node_magnifier = node_magnifier,
                                   label_font_size = label_font_size,
                                   static_network = static_network,
                                   solver = solver,
                                   gravitationalConstant = gravitationalConstant,
                                   nodeDistance = nodeDistance,
                                   centralGravity = centralGravity,
                                   springLength = springLength,
                                   springConstant = springConstant,
                                   width = width,
                                   height = height)

      self$plots$ea_emap_plot = out_data$plot
      self$tables$ea_emap_plot = out_data$table


    },

    plot_ora_emap_plot = function(x = self$tables$ora_object,
                                 show_category = self$params$ora_emap_plot$show_category,
                                 color = self$params$ora_emap_plot$color,
                                 size = self$params$ora_emap_plot$size,
                                 displayed_labels = self$params$ora_emap_plot$displayed_labels,
                                 score_threshold = self$params$ora_emap_plot$score_threshold,
                                 similarity_score = self$params$ora_emap_plot$similarity_score,
                                 node_color_palette = self$params$ora_emap_plot$node_color_palette,
                                 reverse_node_palette = self$params$ora_emap_plot$reverse_node_palette,
                                 edge_width = self$params$ora_emap_plot$edge_width,
                                 edge_color = self$params$ora_emap_plot$edge_color,
                                 edge_color_palette = self$params$ora_emap_plot$edge_color_palette,
                                 reverse_edge_palette = self$params$ora_emap_plot$reverse_edge_palette,
                                 edge_magnifier = self$params$ora_emap_plot$edge_magnifier,
                                 node_magnifier = self$params$ora_emap_plot$node_magnifier,
                                 label_font_size = self$params$ora_emap_plot$label_font_size,
                                 static_network = self$params$ora_emap_plot$static_network,
                                 solver = self$params$ora_emap_plot$solver,
                                 gravitationalConstant = self$params$ora_emap_plot$gravitationalConstant,
                                 nodeDistance = self$params$ora_emap_plot$nodeDistance,
                                 centralGravity = self$params$ora_emap_plot$centralGravity,
                                 springLength = self$params$ora_emap_plot$springLength,
                                 springConstant = self$params$ora_emap_plot$springConstant,
                                 width = NULL,
                                 height = NULL) {

      out_data = plot_fa_emap_plot(x = x,
                                   show_category = show_category,
                                   color = color,
                                   size = size,
                                   displayed_labels = displayed_labels,
                                   score_threshold = score_threshold,
                                   similarity_score =similarity_score,
                                   node_color_palette = node_color_palette,
                                   reverse_node_palette = reverse_node_palette,
                                   edge_width = edge_width,
                                   edge_color = edge_color,
                                   edge_color_palette = edge_color_palette,
                                   reverse_edge_palette = reverse_edge_palette,
                                   edge_magnifier = edge_magnifier,
                                   node_magnifier = node_magnifier,
                                   label_font_size = label_font_size,
                                   static_network = static_network,
                                   solver = solver,
                                   gravitationalConstant = gravitationalConstant,
                                   nodeDistance = nodeDistance,
                                   centralGravity = centralGravity,
                                   springLength = springLength,
                                   springConstant = springConstant,
                                   width = width,
                                   height = height)

      self$plots$ora_emap_plot = out_data$plot
      self$tables$ora_emap_plot = out_data$table


    },

    plot_ora_bar_plot = function(object = self$tables$ora_object,
                                 x = self$params$ora_bar_plot$x,
                                 color = self$params$ora_bar_plot$color,
                                 show_category = self$params$ora_bar_plot$show_category,
                                 displayed_label = self$params$ora_bar_plot$displayed_label,
                                 order_by = self$params$ora_bar_plot$order_by,
                                 order_decreasing = self$params$ora_bar_plot$order_decreasing,
                                 color_palette = self$params$ora_bar_plot$color_palette,
                                 reverse_palette = self$params$ora_bar_plot$reverse_palette,
                                 title_font_size = self$params$ora_bar_plot$title_font_size,
                                 yaxis_word_split = self$params$ora_bar_plot$yaxis_word_split,
                                 y_label_font_size = self$params$ora_bar_plot$y_label_font_size,
                                 y_tick_font_size = self$params$ora_bar_plot$y_tick_font_size,
                                 x_label_font_size = self$params$ora_bar_plot$x_label_font_size,
                                 x_tick_font_size = self$params$ora_bar_plot$x_tick_font_size,
                                 legend_font_size = self$params$ora_bar_plot$legend_font_size,
                                 width = NULL,
                                 height = NULL) {

      out_data = plot_fa_bar_plot(object = object,
                                  x = x,
                                  color = color,
                                  show_category = show_category,
                                  displayed_label = displayed_label,
                                  order_by = order_by,
                                  order_decreasing = order_decreasing,
                                  color_palette = color_palette,
                                  reverse_palette = reverse_palette,
                                  title_font_size = title_font_size,
                                  yaxis_word_split = yaxis_word_split,
                                  y_label_font_size = y_label_font_size,
                                  y_tick_font_size = y_tick_font_size,
                                  x_label_font_size = x_label_font_size,
                                  x_tick_font_size = x_tick_font_size,
                                  legend_font_size = legend_font_size,
                                  width = width,
                                  height = height)

      self$plots$ora_bar_plot = out_data$plot
      self$tables$ora_bar_plot = out_data$table
    }
    #------------------------------------------------------------------ END ----
  )
)
