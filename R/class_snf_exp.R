#---------------------------- Class Snf_data ---------------------------------
Snf_class = R6::R6Class(
  "Snf_exp",
  public = list(

    initialize = function(name = NA){
      self$name = name
    },

    #--------------------------------------------------------------- Global ----
    name = NULL,
    type = NULL,

    #----------------------------------------------------------- Parameters ----
    params = list(

      sample_clustering_1 = list(
        auto_refresh = F,
        data_table = NULL,
        K_nearest_neighbors = 5,
        sigma = 0.5,
        distance_method = 'euclidean',
        K_clusters = 3,
        z_max = NULL,
        z_min = NULL,
        vertical_annotations = "K clusters",
        horizontal_annotations = NULL,
        color_palette = 'Viridis',
        reverse_palette = T,
        title_font_size = 0,
        x_tick_font_size = 0,
        y_tick_font_size = 0,
        img_format = 'png'
      ),
      sample_clustering_2 = list(
        auto_refresh = F,
        data_table = NULL,
        K_nearest_neighbors = 5,
        sigma = 0.5,
        distance_method = 'euclidean',
        K_clusters = 3,
        z_max = NULL,
        z_min = NULL,
        vertical_annotations = "K clusters",
        horizontal_annotations = NULL,
        color_palette = 'Viridis',
        reverse_palette = T,
        title_font_size = 0,
        x_tick_font_size = 0,
        y_tick_font_size = 0,
        img_format = 'png'
      ),

      fusion_heatmap = list(
        auto_refresh = F,
        K_nearest_neighbors = 5,
        sigma = 0.5,
        distance_method = 'euclidean',
        SNF_K_nearest_neighbors =  5,
        omics_list = NULL,
        K_clusters = 3,
        T_iterations = 5,
        z_max = NULL,
        z_min = NULL,
        vertical_annotations = "K clusters",
        horizontal_annotations = NULL,
        color_palette = 'Viridis',
        reverse_palette = T,
        title_font_size = 0,
        x_tick_font_size = 0,
        y_tick_font_size = 0,
        img_format = 'png'
      ),

      similarity_network_1 = list(
        auto_refresh = F,
        data_table = NULL,
        group_colors = "K clusters",
        node_color_palette = "Spectral",
        node_opacity = 1,
        label_font_size = 40,
        edge_magnifier = 1,
        K_nearest_neighbors = 5,
        sigma = 0.5,
        distance_method = 'euclidean',
        K_clusters = 3,
        legend = TRUE,
        top_edges = 10,
        static_network = F,
        solver = "barnesHut",
        gravitationalConstant = -3000,
        nodeDistance = 200,
        centralGravity = 0.2,
        springLength = 200,
        springConstant = 0.04
      ),

      similarity_network_2 = list(
        auto_refresh = F,
        data_table = NULL,
        group_colors = "K clusters",
        node_color_palette = "Spectral",
        node_opacity = 1,
        label_font_size = 40,
        edge_magnifier = 1,
        K_nearest_neighbors = 5,
        sigma = 0.5,
        distance_method = 'euclidean',
        K_clusters = 3,
        legend = TRUE,
        top_edges = 10,
        static_network = F,
        solver = "barnesHut",
        gravitationalConstant = -3000,
        nodeDistance = 200,
        centralGravity = 0.2,
        springLength = 200,
        springConstant = 0.04
      ),

      similarity_network_fusion = list(
        auto_refresh = F,
        group_colors = "K clusters",
        node_color_palette = "Spectral",
        edge_color_palette = "Greys",
        reverse_edge_colors = F,
        K_nearest_neighbors = 5,
        sigma = 0.5,
        distance_method = 'euclidian',
        SNF_K_nearest_neighbors = 5,
        omics_list = NULL,
        K_clusters = 3,
        T_iterations = 5,
        legend = TRUE,
        top_edges = 10,
        edge_magnifier = 1,
        node_opacity = 1,
        label_font_size = 40,
        static_network = F,
        solver = "barnesHut",
        gravitationalConstant = -3000,
        nodeDistance = 200,
        centralGravity = 0.2,
        springLength = 200,
        springConstant = 0.04
      )

    ),

    #---------------------------------------------------- Parameter methods ----
    param_sample_clustering_1 = function(auto_refresh, data_table, K_nearest_neighbors, sigma, distance_method, K_clusters, z_max, z_min, vertical_annotations, horizontal_annotations, color_palette, reverse_palette, title_font_size, x_tick_font_size, y_tick_font_size, img_format) {
      self$params$sample_clustering_1$auto_refresh = auto_refresh
      self$params$sample_clustering_1$data_table = data_table
      self$params$sample_clustering_1$K_nearest_neighbors = K_nearest_neighbors
      self$params$sample_clustering_1$sigma = sigma
      self$params$sample_clustering_1$distance_method = distance_method
      self$params$sample_clustering_1$K_clusters = K_clusters
      self$params$sample_clustering_1$z_max = z_max
      self$params$sample_clustering_1$z_min = z_min
      self$params$sample_clustering_1$vertical_annotations = vertical_annotations
      self$params$sample_clustering_1$horizontal_annotations = horizontal_annotations
      self$params$sample_clustering_1$color_palette = color_palette
      self$params$sample_clustering_1$reverse_palette = reverse_palette
      self$params$sample_clustering_1$title_font_size = title_font_size
      self$params$sample_clustering_1$x_tick_font_size = x_tick_font_size
      self$params$sample_clustering_1$y_tick_font_size = y_tick_font_size
      self$params$sample_clustering_1$img_format = img_format

    },

    param_sample_clustering_2 = function(auto_refresh, data_table, K_nearest_neighbors, sigma, distance_method, K_clusters, z_max, z_min, vertical_annotations, horizontal_annotations, color_palette, reverse_palette, title_font_size, x_tick_font_size, y_tick_font_size, img_format) {
      self$params$sample_clustering_2$auto_refresh = auto_refresh
      self$params$sample_clustering_2$data_table = data_table
      self$params$sample_clustering_2$K_nearest_neighbors = K_nearest_neighbors
      self$params$sample_clustering_2$sigma = sigma
      self$params$sample_clustering_2$distance_method = distance_method
      self$params$sample_clustering_2$K_clusters = K_clusters
      self$params$sample_clustering_2$z_max = z_max
      self$params$sample_clustering_2$z_min = z_min
      self$params$sample_clustering_2$vertical_annotations = vertical_annotations
      self$params$sample_clustering_2$horizontal_annotations = horizontal_annotations
      self$params$sample_clustering_2$color_palette = color_palette
      self$params$sample_clustering_2$reverse_palette = reverse_palette
      self$params$sample_clustering_2$title_font_size = title_font_size
      self$params$sample_clustering_2$x_tick_font_size = x_tick_font_size
      self$params$sample_clustering_2$y_tick_font_size = y_tick_font_size
      self$params$sample_clustering_2$img_format = img_format

    },

    param_fusion_heatmap = function(auto_refresh, K_nearest_neighbors, sigma, distance_method, SNF_K_nearest_neighbors, omics_list, K_clusters, T_iterations, z_max, z_min, vertical_annotations, horizontal_annotations, color_palette, reverse_palette, title_font_size, x_tick_font_size, y_tick_font_size, img_format) {
      self$params$fusion_heatmap$auto_refresh = auto_refresh
      self$params$fusion_heatmap$K_nearest_neighbors = K_nearest_neighbors
      self$params$fusion_heatmap$sigma = sigma
      self$params$fusion_heatmap$distance_method = distance_method
      self$params$fusion_heatmap$SNF_K_nearest_neighbors = SNF_K_nearest_neighbors
      self$params$fusion_heatmap$omics_list = omics_list
      self$params$fusion_heatmap$K_clusters = K_clusters
      self$params$fusion_heatmap$T_iterations = T_iterations
      self$params$fusion_heatmap$z_max = z_max
      self$params$fusion_heatmap$z_min = z_min
      self$params$fusion_heatmap$vertical_annotations = vertical_annotations
      self$params$fusion_heatmap$horizontal_annotations = horizontal_annotations
      self$params$fusion_heatmap$color_palette = color_palette
      self$params$fusion_heatmap$reverse_palette = reverse_palette
      self$params$fusion_heatmap$title_font_size = title_font_size
      self$params$fusion_heatmap$x_tick_font_size = x_tick_font_size
      self$params$fusion_heatmap$y_tick_font_size = y_tick_font_size
      self$params$fusion_heatmap$img_format = img_format
    },

    param_similarity_network_1 = function(auto_refresh, data_table, group_colors, node_color_palette, node_opacity, label_font_size, edge_magnifier, K_nearest_neighbors, sigma, distance_method, K_clusters, top_edges, legend, static_network, solver, gravitationalConstant, nodeDistance, centralGravity, springLength, springConstant) {
      self$params$similarity_network_1$auto_refresh = auto_refresh
      self$params$similarity_network_1$data_table = data_table
      self$params$similarity_network_1$group_colors = group_colors
      self$params$similarity_network_1$node_color_palette = node_color_palette
      self$params$similarity_network_1$node_opacity = node_opacity
      self$params$similarity_network_1$label_font_size = label_font_size
      self$params$similarity_network_1$edge_magnifier = edge_magnifier
      self$params$similarity_network_1$K_nearest_neighbors = K_nearest_neighbors
      self$params$similarity_network_1$sigma = sigma
      self$params$similarity_network_1$distance_method = distance_method
      self$params$similarity_network_1$K_clusters = K_clusters
      self$params$similarity_network_1$top_edges = top_edges
      self$params$similarity_network_1$legend = legend
      self$params$similarity_network_1$static_network = static_network
      self$params$similarity_network_1$solver = solver
      self$params$similarity_network_1$gravitationalConstant = gravitationalConstant
      self$params$similarity_network_1$nodeDistance = nodeDistance
      self$params$similarity_network_1$centralGravity = centralGravity
      self$params$similarity_network_1$springLength = springLength
      self$params$similarity_network_1$springConstant = springConstant
    },

    param_similarity_network_2 = function(auto_refresh, data_table, group_colors, node_color_palette, node_opacity, label_font_size, edge_magnifier, K_nearest_neighbors, sigma, distance_method, K_clusters, top_edges, legend, static_network, solver, gravitationalConstant, nodeDistance, centralGravity, springLength, springConstant) {
      self$params$similarity_network_2$auto_refresh = auto_refresh
      self$params$similarity_network_2$data_table = data_table
      self$params$similarity_network_2$group_colors = group_colors
      self$params$similarity_network_2$node_color_palette = node_color_palette
      self$params$similarity_network_2$node_opacity = node_opacity
      self$params$similarity_network_2$label_font_size = label_font_size
      self$params$similarity_network_2$edge_magnifier = edge_magnifier
      self$params$similarity_network_2$K_nearest_neighbors = K_nearest_neighbors
      self$params$similarity_network_2$sigma = sigma
      self$params$similarity_network_2$distance_method = distance_method
      self$params$similarity_network_2$K_clusters = K_clusters
      self$params$similarity_network_2$top_edges = top_edges
      self$params$similarity_network_2$legend = legend
      self$params$similarity_network_2$static_network = static_network
      self$params$similarity_network_2$solver = solver
      self$params$similarity_network_2$gravitationalConstant = gravitationalConstant
      self$params$similarity_network_2$nodeDistance = nodeDistance
      self$params$similarity_network_2$centralGravity = centralGravity
      self$params$similarity_network_2$springLength = springLength
      self$params$similarity_network_2$springConstant = springConstant
    },

    param_similarity_network_fusion = function(auto_refresh, group_colors,  node_color_palette, edge_color_palette, reverse_edge_colors, K_nearest_neighbors, sigma, distance_method, SNF_K_nearest_neighbors, omics_list, K_clusters, T_iterations, top_edges, edge_magnifier, node_opacity, label_font_size, legend, static_network, solver, gravitationalConstant, nodeDistance, centralGravity, springLength, springConstant) {
      self$params$similarity_network_fusion$auto_refresh = auto_refresh
      self$params$similarity_network_fusion$group_colors = group_colors
      self$params$similarity_network_fusion$ node_color_palette =  node_color_palette
      self$params$similarity_network_fusion$ edge_color_palette =  edge_color_palette
      self$params$similarity_network_fusion$ reverse_edge_colors =  reverse_edge_colors
      self$params$similarity_network_fusion$K_nearest_neighbors = K_nearest_neighbors
      self$params$similarity_network_fusion$sigma = sigma
      self$params$similarity_network_fusion$distance_method = distance_method
      self$params$similarity_network_fusion$SNF_K_nearest_neighbors = SNF_K_nearest_neighbors
      self$params$similarity_network_fusion$omics_list = omics_list
      self$params$similarity_network_fusion$K_clusters = K_clusters
      self$params$similarity_network_fusion$T_iterations = T_iterations
      self$params$similarity_network_fusion$top_edges = top_edges
      self$params$similarity_network_fusion$edge_magnifier = edge_magnifier
      self$params$similarity_network_fusion$node_opacity = node_opacity
      self$params$similarity_network_fusion$label_font_size = label_font_size
      self$params$similarity_network_fusion$legend = legend
      self$params$similarity_network_fusion$static_network = static_network
      self$params$similarity_network_fusion$solver = solver
      self$params$similarity_network_fusion$gravitationalConstant = gravitationalConstant
      self$params$similarity_network_fusion$nodeDistance = nodeDistance
      self$params$similarity_network_fusion$centralGravity = centralGravity
      self$params$similarity_network_fusion$springLength = springLength
      self$params$similarity_network_fusion$springConstant = springConstant
    },

    #---------------------------------------------------------------- Plots ----
    plots = list(
      sample_clustering_1 = NULL,
      sample_clustering_2 = NULL,
      similarity_network_1 = NULL,
      similarity_network_2 = NULL,
      fusion_heatmap = NULL,
      similarity_network_fusion = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(
      sample_annotation_tables = list(),
      omics_tables = list(),
      sample_metadata = NULL,
      sample_clustering_1 = NULL,
      sample_clustering_2 = NULL,
      similarity_network_1 = NULL,
      similarity_network_2 = NULL,
      fusion_heatmap = NULL,
      similarity_network_fusion = NULL
    ),

    #-------------------------------------------------------- Table methods ----

    add_data = function(name, data_table) {
      self$tables$omics_tables[[name]] = data_table
    },

    add_meta = function(name, meta_table) {
      self$tables$sample_annotation_tables[[name]] = meta_table
    },

    clean_datasets = function() {
      omics_names = names(self$tables$sample_annotation_tables)
      intersected_samples = list()
      sample_counts = c()

      for (name in omics_names) {
        intersected_samples[[name]] = rownames(self$tables$sample_annotation_tables[[name]])
        sample_counts = c(sample_counts, nrow(self$tables$sample_annotation_tables[[name]]))
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
        self$tables$omics_tables[[name]] = self$tables$omics_tables[[name]][intersected_samples,]
        self$tables$sample_annotation_tables[[name]] = self$tables$sample_annotation_tables[[name]][intersected_samples,]
      }

      # Merge the sample annotations
      merged_sample_annotations = self$tables$sample_annotation_tables[[1]]
      for (name in omics_names[2:length(omics_names)]) {
        next_table = self$tables$sample_annotation_tables[[name]]
        diff = dplyr::setdiff(colnames(next_table), colnames(merged_sample_annotations))
        if (length(diff) > 0) {
          merged_sample_annotations = base::cbind(merged_sample_annotations, next_table[,diff, drop = F])
        }
      }

      self$tables$sample_metadata = merged_sample_annotations

    },

    #----------------------------------------------------- Plotting methods ----

    plot_sample_clustering_1 = function(data_table = self$params$sample_clustering_1$data_table,
                                        meta_table = self$tables$sample_metadata,
                                        K_nearest_neighbors = self$params$sample_clustering_1$K_nearest_neighbors,
                                        sigma = self$params$sample_clustering_1$sigma,
                                        distance_method = self$params$sample_clustering_1$distance_method,
                                        K_clusters = self$params$sample_clustering_1$K_clusters,
                                        z_max = self$params$sample_clustering_1$z_max,
                                        z_min = self$params$sample_clustering_1$z_min,
                                        vertical_annotations = self$params$sample_clustering_1$vertical_annotations,
                                        horizontal_annotations = self$params$sample_clustering_1$horizontal_annotations,
                                        color_palette = self$params$sample_clustering_1$color_palette,
                                        reverse_palette = self$params$sample_clustering_1$reverse_palette,
                                        title_font_size = self$params$sample_clustering_1$title_font_size,
                                        x_tick_font_size = self$params$sample_clustering_1$x_tick_font_size,
                                        y_tick_font_size = self$params$sample_clustering_1$y_tick_font_size,
                                        width = NULL,
                                        height = NULL) {

      if (!is.null(z_max)) {
        if (z_max == '') {
          z_max = NULL
        } else if (!is.null(z_max)) {
          z_max = as.numeric(z_max)
        }
      }

      if (!is.null(z_min)) {
        if (z_min == '') {
          z_min = NULL
        } else if (!is.null(z_min)) {
          z_min = as.numeric(z_min)
        }
      }


      if (is.null(data_table)) {
        print_tm(self$name, 'ERROR: no default data table for clusters heatmaps')
        return()
      } else {
        data_table = self$tables$omics_tables[[data_table]]
        if (is.null(data_table)) {
          print_tm(self$name, 'ERROR: undefined data table.')
          return()
        }
      }

      K_nearest_neighbors = as.numeric(K_nearest_neighbors)
      K_clusters = as.numeric(K_clusters)
      sigma = as.numeric(sigma)

      out = sample_clustering(data_table = data_table,
                              meta_table = meta_table,
                              K_nearest_neighbors = as.numeric(K_nearest_neighbors),
                              sigma = as.numeric(sigma),
                              distance_method = distance_method,
                              K_clusters = as.numeric(K_clusters),
                              z_max = z_max,
                              z_min = z_min,
                              vertical_annotations = vertical_annotations,
                              horizontal_annotations = horizontal_annotations,
                              color_palette = color_palette,
                              reverse_palette = reverse_palette,
                              title_font_size = title_font_size,
                              x_tick_font_size = x_tick_font_size,
                              y_tick_font_size = y_tick_font_size,
                              width = width,
                              height = height)

      self$tables$sample_clustering_1 = out$data_table
      self$plots$sample_clustering_1 = out$plot
    },


    plot_sample_clustering_2 = function(data_table = self$params$sample_clustering_2$data_table,
                                        meta_table = self$tables$sample_metadata,
                                        K_nearest_neighbors = self$params$sample_clustering_2$K_nearest_neighbors,
                                        sigma = self$params$sample_clustering_2$sigma,
                                        distance_method = self$params$sample_clustering_2$distance_method,
                                        K_clusters = self$params$sample_clustering_2$K_clusters,
                                        z_max = self$params$sample_clustering_2$z_max,
                                        z_min = self$params$sample_clustering_2$z_min,
                                        vertical_annotations = self$params$sample_clustering_2$vertical_annotations,
                                        horizontal_annotations = self$params$sample_clustering_2$horizontal_annotations,
                                        color_palette = self$params$sample_clustering_2$color_palette,
                                        reverse_palette = self$params$sample_clustering_2$reverse_palette,
                                        title_font_size = self$params$sample_clustering_2$title_font_size,
                                        x_tick_font_size = self$params$sample_clustering_2$x_tick_font_size,
                                        y_tick_font_size = self$params$sample_clustering_2$y_tick_font_size,
                                        width = NULL,
                                        height = NULL) {

      if (z_max == '') {
        z_max = NULL
      } else if (!is.null(z_max)) {
        z_max = as.numeric(z_max)
      }

      if (z_min == '') {
        z_min = NULL
      } else if (!is.null(z_min)) {
        z_min = as.numeric(z_min)
      }

      if (is.null(data_table)) {
        print_tm(self$name, 'ERROR: no default data table for clusters heatmaps')
        return()
      } else {
        data_table = self$tables$omics_tables[[data_table]]
        if (is.null(data_table)) {
          print_tm(self$name, 'ERROR: undefined data table.')
          return()
        }
      }

      K_nearest_neighbors = as.numeric(K_nearest_neighbors)
      K_clusters = as.numeric(K_clusters)
      sigma = as.numeric(sigma)

      out = sample_clustering(data_table = data_table,
                              meta_table = meta_table,
                              K_nearest_neighbors = as.numeric(K_nearest_neighbors),
                              sigma = as.numeric(sigma),
                              distance_method = distance_method,
                              K_clusters = as.numeric(K_clusters),
                              z_max = z_max,
                              z_min = z_min,
                              vertical_annotations = vertical_annotations,
                              horizontal_annotations = horizontal_annotations,
                              color_palette = color_palette,
                              reverse_palette = reverse_palette,
                              title_font_size = title_font_size,
                              x_tick_font_size = x_tick_font_size,
                              y_tick_font_size = y_tick_font_size,
                              width = width,
                              height = height)

      self$tables$sample_clustering_2 = out$data_table
      self$plots$sample_clustering_2 = out$plot
    },


    plot_fusion_heatmap = function(meta_table = self$tables$sample_metadata,
                                   K_nearest_neighbors = self$params$fusion_heatmap$K_nearest_neighbors,
                                   sigma = self$params$fusion_heatmap$sigma,
                                   distance_method = self$params$fusion_heatmap$distance_method,
                                   SNF_K_nearest_neighbors = self$params$fusion_heatmap$SNF_K_nearest_neighbors,
                                   omics_list = self$params$fusion_heatmap$omics_list,
                                   K_clusters = self$params$fusion_heatmap$K_clusters,
                                   T_iterations = self$params$fusion_heatmap$T_iterations,
                                   z_max = self$params$fusion_heatmap$z_max,
                                   z_min = self$params$fusion_heatmap$z_min,
                                   vertical_annotations = self$params$fusion_heatmap$vertical_annotations,
                                   horizontal_annotations = self$params$fusion_heatmap$horizontal_annotations,
                                   color_palette = self$params$fusion_heatmap$color_palette,
                                   reverse_palette = self$params$fusion_heatmap$reverse_palette,
                                   title_font_size = self$params$fusion_heatmap$title_font_size,
                                   x_tick_font_size = self$params$fusion_heatmap$x_tick_font_size,
                                   y_tick_font_size = self$params$fusion_heatmap$y_tick_font_size,
                                   width = NULL,
                                   height = NULL
    ) {

      # Fonts
      xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
      ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
      title = base::ifelse(title_font_size > 0, paste0('<span style="font-size: ', title_font_size, 'px;">Fusion heatmap</span>'), "")


      if (!is.null(z_max)) {
        if (z_max == '') {
          z_max = NULL
        } else if (!is.null(z_max)) {
          z_max = as.numeric(z_max)
        }
      }

      if (!is.null(z_min)) {
        if (z_min == '') {
          z_min = NULL
        } else if (!is.null(z_min)) {
          z_min = as.numeric(z_min)
        }
      }

      if (is.null(omics_list)) {
        omics_list = names(self$tables$omics_tables)
      } else if (length(omics_list) == 1){
        stop("Choose more than one omics for SNF.")
      }
      K_nearest_neighbors = as.numeric(K_nearest_neighbors)
      SNF_K_nearest_neighbors = as.numeric(SNF_K_nearest_neighbors)
      K_clusters = as.numeric(K_clusters)
      sigma = as.numeric(sigma)
      T_iterations = as.numeric(T_iterations)

      matrix_list = list()
      for (w in omics_list){
        affinity_matrix = self$tables$omics_tables[[w]]
        affinity_matrix = base::as.matrix(stats::dist(x = affinity_matrix,
                                                      method = distance_method))
        affinity_matrix = SNFtool::affinityMatrix(affinity_matrix, K = K_nearest_neighbors, sigma = sigma)
        matrix_list[[w]] = affinity_matrix
      }
      data_table = SNFtool::SNF(Wall = matrix_list, K = SNF_K_nearest_neighbors, t = T_iterations)

      clusters = SNFtool::spectralClustering(data_table, K = K_clusters)
      order_by_cluster = base::order(clusters)
      data_table = data_table[order_by_cluster,order_by_cluster]
      base::diag(data_table) = min(data_table, na.rm = T)

      # Annotations
      meta_table = meta_table[order_by_cluster,]
      meta_table[, 'K clusters'] = clusters[order_by_cluster]

      if (!is.null(vertical_annotations)) {
        if (length(vertical_annotations) > 1) {
          vertical_annotations = meta_table[, vertical_annotations]
          colnames(vertical_annotations) = stringr::str_replace_all(colnames(vertical_annotations), "_", " ")
        } else {
          row_names = vertical_annotations
          vertical_annotations = as.data.frame(meta_table[, vertical_annotations],
                                               row.names = rownames(meta_table))
          colnames(vertical_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      if (!is.null(horizontal_annotations)) {
        if (length(horizontal_annotations) > 1) {
          horizontal_annotations = meta_table[, horizontal_annotations]
          colnames(horizontal_annotations) = stringr::str_replace_all(colnames(horizontal_annotations), "_", " ")
        } else {
          row_names = horizontal_annotations
          horizontal_annotations = as.data.frame(meta_table[, horizontal_annotations],
                                                 row.names = rownames(meta_table))
          colnames(horizontal_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      # Use z_max and z_min
      if (!is.null(z_max)) {
        data_table[data_table > z_max] = z_max
      } else {
        z_max = max(data_table, na.rm = T)
      }
      if (!is.null(z_min)) {
        data_table[data_table < z_min] = z_min
      } else {
        z_min = min(data_table, na.rm = T)
      }

      # Get the color palette
      color_count = colors_switch(color_palette)
      colors = get_colors(color_count = color_count, color_palette = color_palette)
      if (reverse_palette) {
        colors = base::rev(colors)
      }

      plot = heatmaply::heatmaply(data_table,
                                  colors = colors,
                                  dendrogram = 'none',
                                  Rowv = F,
                                  Colv = F,
                                  trace = 'none',
                                  col_side_colors = vertical_annotations,
                                  row_side_colors = horizontal_annotations,
                                  main = title,
                                  fontsize_row = x_tick_font_size,
                                  fontsize_col = y_tick_font_size,
                                  showticklabels = c(xtick_show, ytick_show),
      )

      plot = plotly::layout(
        p = plot,
        plot_bgcolor='rgba(0,0,0,0)',
        paper_bgcolor='rgba(0,0,0,0)'
      )

      self$tables$fusion_heatmap = data_table
      self$plots$fusion_heatmap = plot
    },


    plot_similarity_network_1 = function(data_table = self$params$similarity_network_1$data_table,
                                         meta_table = self$tables$sample_metadata,
                                         group_colors = self$params$similarity_network_1$group_colors,
                                         node_color_palette = self$params$similarity_network_1$node_color_palette,
                                         K_nearest_neighbors = self$params$similarity_network_1$K_nearest_neighbors,
                                         sigma = self$params$similarity_network_1$sigma,
                                         distance_method = self$params$similarity_network_1$distance_method,
                                         K_clusters = self$params$similarity_network_1$K_clusters,
                                         legend = self$params$similarity_network_1$legend,
                                         top_edges = self$params$similarity_network_1$top_edges,
                                         edge_magnifier = self$params$similarity_network_1$edge_magnifier,
                                         node_opacity = self$params$similarity_network_1$node_opacity,
                                         label_font_size = self$params$similarity_network_1$label_font_size,
                                         static_network = self$params$similarity_network_1$static_network,
                                         solver = self$params$similarity_network_1$solver,
                                         gravitationalConstant = self$params$similarity_network_1$gravitationalConstant,
                                         nodeDistance = self$params$similarity_network_1$nodeDistance,
                                         centralGravity = self$params$similarity_network_1$centralGravity,
                                         springLength = self$params$similarity_network_1$springLength,
                                         springConstant = self$params$similarity_network_1$springConstant,
                                         width = NULL,
                                         height = NULL) {

      # Checks
      K_nearest_neighbors = numeric_check(value = K_nearest_neighbors, default = 5)
      sigma = numeric_check(value = sigma, default = 0.5)
      K_clusters = numeric_check(value = K_clusters, default = 3)
      top_edges = numeric_check(value = top_edges, default = 10)
      edge_magnifier = numeric_check(value = edge_magnifier, default = 100)
      node_opacity = numeric_check(value = node_opacity, default = 1)

      # top_edges to ratio
      top_edges = top_edges / 100

      if (is.null(data_table)) {
        print_tm(self$name, 'ERROR: no default data table for clusters heatmaps')
        return()
      } else {
        data_table = self$tables$omics_tables[[data_table]]
        if (is.null(data_table)) {
          print_tm(self$name, 'ERROR: undefined data table.')
          return()
        }
      }

      if (group_colors != 'K clusters') {
        group_colors = meta_table[rownames(data_table), group_colors]
      }



      output = plot_similarity_network(data_table = data_table,
                                       group_colors = group_colors,
                                       node_color_palette = node_color_palette,
                                       K_nearest_neighbors = K_nearest_neighbors,
                                       K_clusters = K_clusters,
                                       sigma = sigma,
                                       distance_method = distance_method,
                                       top_edges = top_edges,
                                       node_opacity = node_opacity,
                                       label_font_size = label_font_size,
                                       edge_magnifier = edge_magnifier,
                                       legend = legend,
                                       static_network = static_network,
                                       solver = solver,
                                       gravitationalConstant = gravitationalConstant,
                                       nodeDistance = nodeDistance,
                                       centralGravity = centralGravity,
                                       springLength = springLength,
                                       springConstant = springConstant,
                                       width = width,
                                       height = height)

      self$tables$similarity_network_1$edge_table = output$edge_table
      self$tables$similarity_network_1$node_table = output$node_table
      self$plots$similarity_network_1 = output$plot
    },


    plot_similarity_network_2 = function(data_table = self$params$similarity_network_2$data_table,
                                         meta_table = self$tables$sample_metadata,
                                         group_colors = self$params$similarity_network_2$group_colors,
                                         node_color_palette = self$params$similarity_network_2$node_color_palette,
                                         K_nearest_neighbors = self$params$similarity_network_2$K_nearest_neighbors,
                                         sigma = self$params$similarity_network_2$sigma,
                                         distance_method = self$params$similarity_network_2$distance_method,
                                         K_clusters = self$params$similarity_network_2$K_clusters,
                                         legend = self$params$similarity_network_2$legend,
                                         top_edges = self$params$similarity_network_2$top_edges,
                                         edge_magnifier = self$params$similarity_network_2$edge_magnifier,
                                         node_opacity = self$params$similarity_network_2$node_opacity,
                                         label_font_size = self$params$similarity_network_2$label_font_size,
                                         static_network = self$params$similarity_network_2$static_network,
                                         solver = self$params$similarity_network_2$solver,
                                         gravitationalConstant = self$params$similarity_network_2$gravitationalConstant,
                                         nodeDistance = self$params$similarity_network_2$nodeDistance,
                                         centralGravity = self$params$similarity_network_2$centralGravity,
                                         springLength = self$params$similarity_network_2$springLength,
                                         springConstant = self$params$similarity_network_2$springConstant,
                                         width = NULL,
                                         height = NULL) {

      # Checks
      K_nearest_neighbors = numeric_check(value = K_nearest_neighbors, default = 5)
      sigma = numeric_check(value = sigma, default = 0.5)
      K_clusters = numeric_check(value = K_clusters, default = 3)
      top_edges = numeric_check(value = top_edges, default = 10)
      edge_magnifier = numeric_check(value = edge_magnifier, default = 100)
      node_opacity = numeric_check(value = node_opacity, default = 1)

      # top_edges to ratio
      top_edges = top_edges / 100

      if (is.null(data_table)) {
        print_tm(self$name, 'ERROR: no default data table for clusters heatmaps')
        return()
      } else {
        data_table = self$tables$omics_tables[[data_table]]
        if (is.null(data_table)) {
          print_tm(self$name, 'ERROR: undefined data table.')
          return()
        }
      }

      if (group_colors != 'K clusters') {
        group_colors = meta_table[rownames(data_table), group_colors]
      }


      output = plot_similarity_network(data_table = data_table,
                                       group_colors = group_colors,
                                       node_color_palette = node_color_palette,
                                       K_nearest_neighbors = K_nearest_neighbors,
                                       K_clusters = K_clusters,
                                       sigma = sigma,
                                       distance_method = distance_method,
                                       top_edges = top_edges,
                                       node_opacity = node_opacity,
                                       label_font_size = label_font_size,
                                       edge_magnifier = edge_magnifier,
                                       legend = legend,
                                       static_network = static_network,
                                       solver = solver,
                                       gravitationalConstant = gravitationalConstant,
                                       nodeDistance = nodeDistance,
                                       centralGravity = centralGravity,
                                       springLength = springLength,
                                       springConstant = springConstant,
                                       width = width,
                                       height = height)

      self$tables$similarity_network_2$edge_table = output$edge_table
      self$tables$similarity_network_2$node_table = output$node_table
      self$plots$similarity_network_2 = output$plot
    },

    plot_similarity_network_fusion = function(group_colors = self$params$similarity_network_fusion$group_colors,
                                              node_color_palette = self$params$similarity_network_fusion$node_color_palette,
                                              edge_color_palette = self$params$similarity_network_fusion$edge_color_palette,
                                              reverse_edge_colors = self$params$similarity_network_fusion$reverse_edge_colors,
                                              meta_table = self$tables$sample_metadata,
                                              K_nearest_neighbors = self$params$similarity_network_fusion$K_nearest_neighbors,
                                              sigma = self$params$similarity_network_fusion$sigma,
                                              distance_method = self$params$similarity_network_fusion$distance_method,
                                              SNF_K_nearest_neighbors = self$params$similarity_network_fusion$SNF_K_nearest_neighbors,
                                              omics_list = self$params$similarity_network_fusion$omics_list,
                                              K_clusters = self$params$similarity_network_fusion$K_clusters,
                                              T_iterations = self$params$similarity_network_fusion$T_iterations,
                                              edge_magnifier = self$params$similarity_network_fusion$edge_magnifier,
                                              node_opacity = self$params$similarity_network_fusion$node_opacity,
                                              label_font_size = self$params$similarity_network_fusion$label_font_size,
                                              legend = self$params$similarity_network_fusion$legend,
                                              top_edges = self$params$similarity_network_fusion$top_edges,
                                              static_network = self$params$similarity_network_fusion$static_network,
                                              solver = self$params$similarity_network_fusion$solver,
                                              gravitationalConstant = self$params$similarity_network_fusion$gravitationalConstant,
                                              nodeDistance = self$params$similarity_network_fusion$nodeDistance,
                                              centralGravity = self$params$similarity_network_fusion$centralGravity,
                                              springLength = self$params$similarity_network_fusion$springLength,
                                              springConstant = self$params$similarity_network_fusion$springConstant,
                                              width = NULL,
                                              height = NULL
    ) {

      # Checks
      K_nearest_neighbors = numeric_check(value = K_nearest_neighbors, default = 5)
      sigma = numeric_check(value = sigma, default = 0.5)
      K_clusters = numeric_check(value = K_clusters, default = 3)
      SNF_K_nearest_neighbors = numeric_check(value = SNF_K_nearest_neighbors, default = 5)
      T_iterations = numeric_check(value = T_iterations, default = 3)
      top_edges = numeric_check(value = top_edges, default = 10)
      edge_magnifier = numeric_check(value = edge_magnifier, default = 100)
      node_opacity = numeric_check(value = node_opacity, default = 1)

      # top_edges to ratio
      top_edges = top_edges / 100

      # Get all the single omics
      if (is.null(omics_list)) {
        omics_list = names(self$tables$omics_tables)
      } else if (length(omics_list) == 1){
        stop("Choose more than one omics for SNF.")
      }

      # Retrive the single omics affinity matrices & edges formed for each using the top_edges
      single_omics_edges = list()
      matrix_list = list()
      for (w in omics_list){
        # Get affinity matrix
        affinity_matrix = self$tables$omics_tables[[w]]
        affinity_matrix = base::as.matrix(stats::dist(x = affinity_matrix,
                                                      method = distance_method))
        matrix_list[[w]] = SNFtool::affinityMatrix(affinity_matrix, K = K_nearest_neighbors, sigma = sigma)

        # Get weights and trim with top_edges
        edges = igraph::graph_from_adjacency_matrix(adjmatrix = matrix_list[[w]],
                                                    weighted= TRUE,
                                                    mode="undirected",
                                                    diag=F)
        weights = igraph::E(edges)$weight
        edges = base::as.data.frame(igraph::as_edgelist(edges))
        colnames(edges) = c('from', 'to')
        edges$weights = weights
        for (i in 1:nrow(edges)) { # Sort from/to to have unique ID pairs
          edges[i, c('from', 'to')] = sort(unlist(edges[i, c('from', 'to')][1,], use.names = F))
        }
        edges = edges[order(-edges$weights), ]
        rownames(edges) = NULL
        edges = edges[1:ceiling(nrow(edges) * top_edges),]
        single_omics_edges[[w]] = edges
      }

      # Produced the fused affinity matrix
      data_table = SNFtool::SNF(Wall = matrix_list, K = SNF_K_nearest_neighbors, t = T_iterations)

      # Produce the network edge table and trim it
      edge_table = igraph::graph_from_adjacency_matrix(adjmatrix = data_table,
                                                       weighted= TRUE,
                                                       mode="undirected",
                                                       diag=F)
      weights = igraph::E(edge_table)$weight
      edge_table = base::as.data.frame(igraph::as_edgelist(edge_table))
      colnames(edge_table) = c('from', 'to')
      edge_table$value = weights
      edge_table = edge_table[order(-edge_table$value), ]
      rownames(edge_table) = NULL
      edge_table = edge_table[1:ceiling(nrow(edge_table) * top_edges),]


      # Sort the from/to of the edge_table to create a unique fused_id
      for (i in 1:nrow(edge_table)) {
        edge_table[i, c('from', 'to')] = sort(unlist(edge_table[i, c('from', 'to')][1,], use.names = F))
      }

      # Find out which edges were already present in single omics networks
      fused_id = with(edge_table, paste(from, to, sep="-"))
      for (w in omics_list){
        w_id = with(single_omics_edges[[w]], paste(from, to, sep="-"))
        edge_table[,w] = fused_id %in% w_id
      }


      # Create the node table with coloring
      node_table = data.frame(id = 1:ncol(data_table),
                              label = colnames(data_table))

      if (group_colors == 'K clusters') {
        group_colors = SNFtool::spectralClustering(data_table[node_table$label, node_table$label], K = K_clusters)
      } else {
        group_colors = meta_table[node_table$label, group_colors]
      }
      node_table$group = group_colors

      color_count = colors_switch(node_color_palette)
      node_color_palette = get_colors(color_count = color_count, color_palette = node_color_palette)
      node_color_palette = colorRampPalette(node_color_palette)(length(unique(node_table$group)))
      node_color_palette = setNames(node_color_palette, unique(node_table$group))
      node_table$color = unname(node_color_palette[node_table$group])
      node_table$title = paste0(node_table$label, '<p>', node_table$group)

      # Replace the string IDs in the edge table by numeric IDs
      nodes_dict = node_table$id
      names(nodes_dict) = node_table$label
      edge_table$from = unname(nodes_dict[edge_table$from])
      edge_table$to = unname(nodes_dict[edge_table$to])
      edge_table$shared = unname(rowSums(edge_table[,omics_list]))

      edge_titles = c()
      for (i in 1:nrow(edge_table)) {
        shared = omics_list[base::unlist(edge_table[i, omics_list], use.names = F)]
        weight = paste0("Weight: ", round(edge_table[i, 'value'], 5))
        if (length(shared) > 0) {
          title = paste0('Shared by: ', paste0(omics_list[base::unlist(edge_table[i, omics_list], use.names = F)], collapse = ", "))
        } else {
          title = "Fusion exclusive"
        }
        title = paste0(title, '<p>', weight)
        edge_titles = c(edge_titles, title)
      }
      edge_table$title = edge_titles

      # color edges
      shared_edges = sort(unique(edge_table$shared))
      edge_color_palette = get_colors(color_count = colors_switch(edge_color_palette), color_palette = edge_color_palette)
      edge_color_palette = colorRampPalette(edge_color_palette)(length(shared_edges))
      if (reverse_edge_colors) {
        edge_color_palette = rev(edge_color_palette)
      }
      edge_color_palette = setNames(edge_color_palette, shared_edges)
      edge_table$color = unname(edge_color_palette[as.character(edge_table$shared)])

      # Adapt edge width for visibility
      edge_table$value = edge_table$value * edge_magnifier
      node_table$opacity = node_opacity
      node_table$font.size = label_font_size
      node_table$borderWidth = 0

      # Plot the network
      # plot = visNetwork::visNetwork(node_table, edge_table, height = height, width = width)
      #
      # if (legend) {
      #   for (group in names(node_color_palette)) {
      #     plot = visNetwork::visGroups(graph = plot, groupname = group, shape = "dot", borderWidth = 0, color = unname(node_color_palette[group]))
      #   }
      #   plot = visNetwork::visLegend(graph = plot,
      #                                addEdges = data.frame(label = paste0("Shared by ", names(edge_color_palette)),
      #                                                      color = unname(edge_color_palette))
      #   )
      # }

      plot = visNetwork::visNetwork(node_table, edge_table, width = width, height = height)
      if (static_network) {
        g = igraph::graph_from_data_frame(edge_table, directed = TRUE)

        # Catch singletons if there are any
        node_ids = base::unique(node_table$id)
        graph_nodes = igraph::V(g)$name
        singleton_nodes = setdiff(node_ids, graph_nodes)
        if (length(singleton_nodes) > 0) {
          g = igraph::add_vertices(g, length(singleton_nodes), name=singleton_nodes)
        }

        layout = igraph::layout_with_fr(g)
        nodes = data.frame(id = 1:igraph::vcount(g),
                           label = as.character(1:igraph::vcount(g)),
                           x = layout[,1] * 100,
                           y = -layout[,2] * 100)

        node_table$x = nodes$x
        node_table$y = nodes$y

        plot = visNetwork::visNodes(
          graph = plot,
          fixed = FALSE,
          x = node_table$x,
          y = node_table$y
        )
        plot = visNetwork::visPhysics(graph = plot, enabled = FALSE)
        plot = visNetwork::visOptions(graph = plot, highlightNearest = TRUE, nodesIdSelection = TRUE)

      } else {

        if (solver == "barnesHut") {
          plot = visNetwork::visPhysics(plot,
                                        solver = "barnesHut",
                                        barnesHut = list(gravitationalConstant = gravitationalConstant,
                                                         centralGravity = centralGravity,
                                                         springLength = springLength,
                                                         springConstant = springConstant),
                                        stabilization = list(enabled = TRUE, iterations = 1000))
        } else if (solver == "repulsion") {
          plot = visNetwork::visPhysics(plot,
                                        solver = "repulsion",
                                        repulsion = list(nodeDistance = nodeDistance,
                                                         centralGravity = centralGravity,
                                                         springLength = springLength,
                                                         springConstant = springConstant))
        }
        plot = visNetwork::visOptions(plot,
                                      highlightNearest = TRUE, nodesIdSelection = TRUE)
      }
      if (legend) {
        for (group in names(node_color_palette)) {
          plot = visNetwork::visGroups(graph = plot, groupname = group, shape = "dot", borderWidth = 0, color = unname(node_color_palette[group]))
        }
        plot = visNetwork::visLegend(graph = plot,
                                     addEdges = data.frame(label = paste0("Shared by ", names(edge_color_palette)),
                                                           color = unname(edge_color_palette))
        )
      }

      self$tables$similarity_network_fusion$edge_table = edge_table
      self$tables$similarity_network_fusion$node_table = node_table
      self$plots$similarity_network_fusion = plot

    }

  )
)