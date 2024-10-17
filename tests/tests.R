base::source('./iSODA/R/utils.R')
base::source('./iSODA/R/class_omics_exp.R')
base::source('./iSODA/R/class_mofa_exp.R')
base::source('./iSODA/R/class_snf_exp.R')

#--------------------------------------------- DEBUG TRANSCRIPTOMICS 241010 ----

if (F) {
  name = 'trns_1'
  type = "Transcriptomics"
  meta_file = './test_data/241010_Yassene/trns_meta.csv'
  data_file = './test_data/241010_Yassene/trns_data.csv'
  meta_file_format = "Wide"
  data_file_format = "Wide"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  type_column = 'genotype'
  group_column = 'genotype'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "quality"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = T
  drop_qcs = T
  drop_pools = T
  id_col_data = 'ID'
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
}

if (T) {
  name = 'trns_1'
  type = "Transcriptomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/rna_data.csv'
  meta_file_format = "Wide"
  data_file_format = "Wide"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  type_column = 'Sample_type'
  group_column = 'Group_type'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "quality"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = T
  drop_qcs = T
  drop_pools = T
  id_col_data = 'ID'
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
}

if (T) {
  name = 'trns_1'
  type = "Transcriptomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/t_sample_annotations.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/t_rna_data.csv'
  meta_file_format = "Long"
  data_file_format = "Long"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  type_column = 'Sample_type'
  group_column = 'Group_type'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "quality"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = T
  drop_qcs = T
  drop_pools = T
  id_col_data = 'ID'
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
}
meta_table = soda_read_table(meta_file)
meta_table = t(meta_table)
colnames(meta_table) = meta_table[1,]
meta_table = meta_table[-1,]
write.table(meta_table, file = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/230927_Cellminer_data/cellminer_data/t_sample_annotations.tsv", sep = '\t')

t_meta_table = soda_read_table("/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/230927_Cellminer_data/cellminer_data/t_sample_annotations.tsv",
                               header = F)

t_meta_table = t(t_meta_table)
colnames(t_meta_table) = t_meta_table[1,]
t_meta_table = t_meta_table[-1,]
rownames(t_meta_table) = NULL
t_meta_table = as.data.frame(t_meta_table, check.names = F)

data_table = soda_read_table(data_file)
data_table = t(data_table)
colnames(data_table) = data_table[1,]
data_table = data_table[-1,]
write.table(data_table, file = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/230927_Cellminer_data/cellminer_data/t_rna_data.tsv", sep = '\t')

t_data_table = soda_read_table("/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/230927_Cellminer_data/cellminer_data/t_rna_data.tsv",
                               header = F)
t_data_table = t(t_data_table)
colnames(t_data_table) = t_data_table[1,]
t_data_table = t_data_table[-1,]
rownames(t_data_table) = NULL
t_data_table = as.data.frame(t_data_table, check.names = F)


data_table = get_indexed_table(id_col = "ID",
                  input_table = t_data_table)


numeric_matrix = as.matrix(sapply(data_table, as.numeric))
rownames(numeric_matrix) = rownames(data_table)
colnames(numeric_matrix) = colnames(data_table)


data_table_numeric <- matrix(as.numeric(data_table), nrow = nrow(data_table), ncol = ncol(data_table))
truffles = as.numeric(as.matrix(data_table))
truffles[1,1]

self = example_omics(name = name,
                     type = type,
                     meta_file = meta_file,
                     data_file = data_file,
                     meta_file_format = meta_file_format,
                     data_file_format = data_file_format,
                     param_file = param_file,
                     id_col_meta = id_col_meta,
                     type_column = type_column,
                     group_column = group_column,
                     batch_column = batch_column,
                     blank_pattern = blank_pattern,
                     qc_pattern = qc_pattern,
                     pool_pattern = pool_pattern,
                     excluded_samples = excluded_samples,
                     drop_blanks = drop_blanks,
                     drop_qcs = drop_qcs,
                     drop_pools = drop_pools,
                     id_col_data = id_col_data,
                     blank_multiplier = blank_multiplier,
                     sample_threshold = sample_threshold,
                     group_threshold = group_threshold,
                     excluded_features = excluded_features,
                     imputation_method = imputation_method,
                     batch_effect_correction = batch_effect_correction,
                     operation_order = operation_order,
                     norm_col = norm_col)

self$get_volcano_table()
self$plot_volcano()
self$plots$volcano_plot

base::saveRDS(self, "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/cellminer_iSODArds")

loaded_r6_object <- readRDS("/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/cellminer_iSODArds")

loaded_r6_object$plots$volcano_plot

#--------------------------------------------------------- DEBUG LIPIDOMICS ----

name = 'lips_1'
type = "Lipidomics"
meta_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/241004_Yassene/lips_meta.tsv'
data_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/241004_Yassene/lips_data.tsv'
param_file = './R/params/params_lipidomics.R'
id_col_meta = 'ID'
type_column = 'Sample_type'
group_column = 'Experimental_group'
batch_column = NULL
blank_pattern = "blank"
qc_pattern = "quality"
pool_pattern = "pool"
excluded_samples = NULL
drop_blanks = F
drop_qcs = T
drop_pools = T
id_col_data = 'ID'
blank_multiplier = 2
sample_threshold = 0.8
group_threshold = 0.8
excluded_features = NULL
imputation_method = "minimum"
batch_effect_correction = "None"
operation_order = c("Imputation", "Batch correction", "Filtering")
norm_col = "None"

self = example_omics(name = name,
                     type = type,
                     meta_file = meta_file,
                     data_file = data_file,
                     param_file = param_file,
                     id_col_meta = id_col_meta,
                     type_column = type_column,
                     group_column = group_column,
                     batch_column = batch_column,
                     blank_pattern = blank_pattern,
                     qc_pattern = qc_pattern,
                     pool_pattern = pool_pattern,
                     excluded_samples = excluded_samples,
                     drop_blanks = drop_blanks,
                     drop_qcs = drop_qcs,
                     drop_pools = drop_pools,
                     id_col_data = id_col_data,
                     blank_multiplier = blank_multiplier,
                     sample_threshold = sample_threshold,
                     group_threshold = group_threshold,
                     excluded_features = excluded_features,
                     imputation_method = imputation_method,
                     batch_effect_correction = batch_effect_correction,
                     operation_order = operation_order,
                     norm_col = norm_col)





#--------------------------------------------- RADIAL NETWORKS (SUGGESTION) ----
meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_non_epithelial.tsv'

self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_data.csv',
                          meta = meta,
                          group_column = 'Cancer type',
                          param_file = './R/params/params_gene_based_omics.R')

dataset = self$params$dendrogram$dataset
meta_table = self$tables$raw_meta
annotations = self$params$dendrogram$annotations
distance_method = self$params$dendrogram$distance_method
p = self$params$dendrogram$p
clustering_method = self$params$dendrogram$clustering_method
k_clusters = self$params$dendrogram$k_clusters
color_palette = self$params$dendrogram$color_palette
show_dendrogram_dist = self$params$dendrogram$show_dendrogram_dist
rotate = self$params$dendrogram$rotate
width = NULL
height = NULL


data_table = self$table_check_convert(dataset)

p = as.numeric(p)

if (!is.null(k_clusters)) {
  if (is_coercible_to_numeric(k_clusters)) {
    k_clusters = as.numeric(k_clusters)
  } else {
    k_clusters = NULL
  }
}


# Save and reset rownames
original_rownames = rownames(data_table)
rownames(data_table) = NULL
rownames(meta_table) = NULL

# Filter the metadata table
meta_table = as.data.frame(meta_table[, annotations])
colnames(meta_table) = annotations

# Create the dendrogram
hc = stats::hclust(stats::dist(data_table,
                               method = distance_method,
                               p = p),
                   method = clustering_method)


networkD3::radialNetwork(networkD3::as.radialNetwork(hc))









dataset = self$params$feature_correlation$dataset
meta_table = self$tables$feature_table
map_feature_terms = self$params$feature_correlation$map_feature_terms
correlation_method = self$params$feature_correlation$correlation_method
use = self$params$feature_correlation$use
distance_method = self$params$feature_correlation$distance_method
clustering_method = self$params$feature_correlation$clustering_method
k_clusters = self$params$feature_correlation$k_clusters
apply_clustering = self$params$feature_correlation$apply_clustering
center = self$params$feature_correlation$center
row_annotations = self$params$feature_correlation$row_annotations
col_annotations = self$params$feature_correlation$col_annotations
roh_threshold = self$params$feature_correlation$roh_threshold
top_features = self$params$feature_correlation$top_features
color_palette = self$params$feature_correlation$color_palette
reverse_palette = self$params$feature_correlation$reverse_palette
width = NULL
height = NULL


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

hc = stats::hclust(d = stats::dist(x = t(data_table),
                              method = distance_method),
              method = clustering_method)

dendro = stats::as.dendrogram(hc)

clusters = stats::cutree(tree = hc, k = 10)

radial_plot = networkD3::radialNetwork(networkD3::as.radialNetwork(hc))








assign_clusters_to_dendro <- function(dendro, clusters) {
  # Recursive function to traverse the dendrogram
  traverse_and_assign <- function(node, clusters, leaf_counter) {
    # Base case: if the node is a leaf
    if (is.leaf(node)) {
      # Assign the cluster ID from clusters vector to the leaf node
      attr(node, "members") <- clusters[leaf_counter]
      # Increase the counter for the next leaf
      leaf_counter <<- leaf_counter + 1
    } else {
      # Recursive case: traverse left and right children
      attr(node, "left") <- traverse_and_assign(attr(node, "left"), clusters, leaf_counter)
      attr(node, "right") <- traverse_and_assign(attr(node, "right"), clusters, leaf_counter)
    }
    return(node)
  }

  # Initialize a counter to keep track of the current leaf index
  leaf_counter <- 1

  # Start the recursive traversal
  dendro_modified <- traverse_and_assign(dendro, clusters, leaf_counter)

  return(dendro_modified)
}


dendro_with_clusters <- assign_clusters_to_dendro(dendro, clusters)






















diag(data_table) = NA
max_abs_values = apply(data_table, 1, function(x) max(abs(x), na.rm = T))
roh_filter = unname(which(max_abs_values >= roh_threshold))


if (length(roh_filter) > top_features) {
  best_hits = names(sort(rowSums(abs(data_table), na.rm = T), decreasing = T)[1:top_features])
  roh_filter = which((rownames(data_table) %in% best_hits))
}
data_table = data_table[roh_filter, roh_filter]
meta_table = rowsort_df(rows = rownames(data_table), dataframe = meta_table)

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

meta_table = rowsort_df(rows = Colv$order, dataframe = meta_table)

self$tables$feature_correlation = data_table
self$tables$feature_correlation_clusters = meta_table

# Get the color palette
color_count = colors_switch(color_palette)
colors = get_colors(color_count = color_count, color_palette = color_palette)
if (reverse_palette) {
  colors = base::rev(colors)
}
















#---------------------------------------------------------- PLOTLY VARIANCE ----

# Define function
plot_variance = function(data_table, n_clusters = 3, cutoff = NULL, color_palette = 'Spectral', marker_size = 6, opacity = 1, marker_line_col = NULL, marker_line_width = 0, showlegend = TRUE, plot_only = T, width = NULL, height = NULL) {
  # Get variance
  sorted_variance = apply(data_table, 2, function(x) {stats::var(x = x, na.rm = T)})
  sorted_variance = sort(sorted_variance, decreasing = T)

  # Produce clusters
  kmeans_result = stats::kmeans(sorted_variance, centers = n_clusters)

  # Get the data table
  var_df = data.frame(
    variance = sorted_variance,
    cluster = kmeans_result$cluster,
    rank = 1:length(sorted_variance)
  )

  var_df$hover = paste(rownames(var_df),
                       paste0("Var: ", round(var_df$variance,3)),
                       paste0('Rank: ', var_df$rank),
                       paste0("Cluster: ", var_df$cluster),
                       sep = '\n')

  # Get the color palette
  colors = RColorBrewer::brewer.pal(as.numeric(colors_switch(color_palette)), color_palette)
  colors = grDevices::colorRampPalette(colors)(n_clusters)
  colors = setNames(colors, 1:n_clusters)

  # Create plot
  plot = plotly::plot_ly(width = width,
                         height = height)
  for (i in unique(var_df$cluster)) {
    cluster_data = var_df[var_df$cluster == i,]
    plot = plotly::add_trace(plot,
                             type = "scatter",
                             mode = "markers",
                             x = cluster_data$rank,
                             y = cluster_data$variance,
                             marker = list(size = marker_size,
                                           opacity = opacity,
                                           color = colors[i],
                                           line = list(width = marker_line_width,
                                                       color = marker_line_col)),
                             name = paste0('Cluster ', i),
                             legendgroup = paste0('Cluster ', i),
                             text = cluster_data$hover,
                             hoverinfo = 'text',
                             showlegend = showlegend)
  }

  selected_features = NULL
  if (!is.null(cutoff)) {
    vline = sum(var_df$variance >= cutoff) + 0.5
    plot = plotly::layout(plot,
                          shapes = list(
                            list(
                              type = "line",
                              x0 = vline,
                              x1 = vline,
                              y0 = 0,
                              y1 = max(sorted_variance),
                              line = list(color = "black", width = 1, dash = "dot")
                            )
                          ))
    selected_features = rownames(var_df)[1:vline + 0.5]
  }

  plot = plotly::layout(plot,
                        title = "Feature variance",
                        yaxis = list(title = 'Variance'),
                        xaxis = list(title = 'Feature rank'))

  # Output
  if (plot_only) {
    return(plot)
  } else {
    return(list(
      plot = plot,
      data_table = var_df,
      selected_features = selected_features)
    )
  }
}

#------------------------------------------------------- HVG FILTERING TEST ----

# Proteomics data
prot_table = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/prot_data.csv', first_column_as_index = T)
plot_variance(data_table = prot_table,
              n_clusters = 2,
              marker_size = 10,
              opacity = 1,
              color_palette = 'Paired',
              cutoff = 0.126)
prot_var_results = plot_variance(data_table = prot_table,
                                 n_clusters = 4,
                                 marker_size = 10,
                                 opacity = 1,
                                 color_palette = 'Paired',
                                 cutoff = 0.126,
                                 plot_only = F)
prot_selection = prot_var_results$selected_features
prot_table = prot_table[,prot_selection]
prot_table = cbind(rownames(prot_table), prot_table)
colnames(prot_table)[1] = 'ID'
write.table(x = prot_table,
            file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/prot_data_filtered.tsv',
            row.names = F,
            sep = '\t')


# Transcriptomics data
trns_data = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data.csv', first_column_as_index = T)
plot_variance(data_table = trns_data,
              n_clusters = 3,
              color_palette = 'Paired',
              cutoff = 0.684)
trns_var_results = plot_variance(data_table = trns_data,
                                 n_clusters = 3,
                                 color_palette = 'Paired',
                                 cutoff = 0.684,
                                 plot_only = F)
trns_selection = trns_var_results$selected_features
trns_data = trns_data[,trns_selection]
trns_data = cbind(rownames(trns_data), trns_data)
colnames(trns_data)[1] = 'ID'
write.table(x = trns_data,
            file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data_filtered.tsv',
            row.names = F,
            sep = '\t')


# Genomics data
geno_data = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv', first_column_as_index = T)
plot_variance(data_table = geno_data,
              n_clusters = 4,
              color_palette = 'Paired',
              cutoff = 0.00204)
geno_var_results = plot_variance(data_table = geno_data,
                                 n_clusters = 4,
                                 color_palette = 'Paired',
                                 cutoff = 0.00204,
                                 plot_only = F)
geno_selection = geno_var_results$selected_features[1:10000]
geno_data = geno_data[,geno_selection]
geno_data = cbind(rownames(geno_data), geno_data)
colnames(geno_data)[1] = 'ID'
write.table(x = geno_data,
            file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data_filtered.tsv',
            row.names = F,
            sep = '\t')

#----------------------------------------------- Feature table import tests ----

self = example_omics(name = 'meta_1',
                       type = "Metabolomics",
                       meta_file = 'C:/Users/dolivier/Downloads/sample_annotations_y1.csv',
                       data_file = 'C:/Users/dolivier/Downloads/metabolomics_data.csv',
                       param_file = './R/params/params_gene_based_omics.R',
                       id_col_meta = 'ID',
                       type_column = 'Group',
                       group_column = 'Group',
                       batch_column = 'Batch',
                       blank_pattern = "blank",
                       qc_pattern = "quality",
                       pool_pattern = "pool",
                       excluded_samples = NULL,
                       id_col_data = 'ID',
                       blank_multiplier = 2,
                       sample_threshold = 0.8,
                       group_threshold = 0.8,
                       excluded_features = NULL,
                       imputation_method = "None",
                       batch_effect_correction = "None",
                       operation_order = c("Imputation", "Batch correction"),
                       norm_col = "None")

self$add_feature_table(name = "feat_1",
                         feature_file = 'C:/Users/dolivier/Downloads/feature_annotations_y1.csv')
self$update_feature_table()

feature_table = self$tables$feature_table
rownames(feature_table)


dataset = self$params$feature_correlation$dataset
meta_table = self$tables$feature_table
map_feature_terms = self$params$feature_correlation$map_feature_terms
correlation_method = self$params$feature_correlation$correlation_method
use = self$params$feature_correlation$use
distance_method = self$params$feature_correlation$distance_method
clustering_method = self$params$feature_correlation$clustering_method
k_clusters = self$params$feature_correlation$k_clusters
apply_clustering = self$params$feature_correlation$apply_clustering
center = self$params$feature_correlation$center
row_annotations = self$params$feature_correlation$row_annotations
col_annotations = 'SUPER_PATHWAY'
roh_threshold = self$params$feature_correlation$roh_threshold
top_features = self$params$feature_correlation$top_features
color_palette = self$params$feature_correlation$color_palette
reverse_palette = self$params$feature_correlation$reverse_palette
title_font_size = self$params$feature_correlation$title_font_size
y_label_font_size = self$params$feature_correlation$y_label_font_size
y_tick_font_size = self$params$feature_correlation$y_tick_font_size
x_label_font_size = self$params$feature_correlation$x_label_font_size
x_tick_font_size = self$params$feature_correlation$x_tick_font_size
legend_font_size = self$params$feature_correlation$legend_font_size
width = NULL
height = NULL






prot_1 = example_omics(name = 'prot_1',
                       type = "Proteomics",
                       meta_file = './tests/datasets/CellMiner/sample_annotations.tsv',
                       data_file = './tests/datasets/CellMiner/prot_data.csv',
                       param_file = './R/params/params_gene_based_omics.R',
                       id_col_meta = 'ID',
                       type_column = 'Sample_type',
                       group_column = 'Cancer type',
                       batch_column = 'Batch',
                       blank_pattern = "blank",
                       qc_pattern = "quality",
                       pool_pattern = "pool",
                       excluded_samples = c('CNS:SF-539', 'PR:PC-3', 'PR:DU-145'),
                       id_col_data = 'ID',
                       blank_multiplier = 2,
                       sample_threshold = 0.8,
                       group_threshold = 0.8,
                       excluded_features = NULL,
                       imputation_method = "None",
                       batch_effect_correction = "None",
                       operation_order = c("Imputation", "Batch correction"),
                       norm_col = "None")

prot_1$add_feature_table(name = "feat_1",
                         feature_file = 'C:/Users/dolivier/Desktop/local_work/230704_soda/240325_iSODA/test_data/231023_feature_tables/proteomics_feat_annotation_clean.tsv')
prot_1$update_feature_table()

truffles = prot_1$tables$feature_table

prot_1$get_volcano_table(data_table = prot_1$params$volcano_plot_comparison$data_table,
                         sample_table = prot_1$tables$raw_meta,
                         feature_table = prot_1$tables$feature_table,
                         group_col = prot_1$params$volcano_plot_comparison$group_col,
                         group_1 = 'Leukemia',
                         group_2 = 'Melanoma',
                         fc_function = prot_1$params$volcano_plot_comparison$fc_function,
                         statistical_test = prot_1$params$volcano_plot_comparison$statistical_test,
                         adjustment_method = prot_1$params$volcano_plot_comparison$adjustment_method)
prot_1$plot_volcano()
prot_1$plot_volcano(data_table = self$tables$volcano_table,
                    adjustment = self$params$volcano_plot_comparison$adjustment,
                    group_1 = self$params$volcano_plot_comparison$group_1,
                    group_2 = self$params$volcano_plot_comparison$group_2,
                    feature_metadata = self$params$volcano_plot$feature_metadata,
                    p_val_threshold = self$params$volcano_plot$p_val_threshold,
                    fc_threshold = self$params$volcano_plot$fc_threshold,
                    marker_size = self$params$volcano_plot$marker_size)


volcano_plot_feature_metadata = "Gene Ontology (GO)"
feature_list = prot_1$tables$feature_list[[volcano_plot_feature_metadata]]$feature_list

feature_metadata = match_go_terms(terms_list = feature_list,
                                  sparse_table = prot_1$tables$feature_list[[volcano_plot_feature_metadata]]$sparse_matrix)

truffles = prot_1$tables$feature_table

self = prot_1

data_table = self$tables$volcano_table
adjustment = self$params$volcano_plot_comparison$adjustment
group_1 = self$params$volcano_plot_comparison$group_1
group_2 = self$params$volcano_plot_comparison$group_2
feature_metadata = feature_metadata
keep_significant = self$params$volcano_plot$keep_significant
displayed_plot = self$params$volcano_plot$displayed_plot
p_val_threshold = self$params$volcano_plot$p_val_threshold
fc_threshold = self$params$volcano_plot$fc_threshold
marker_size = self$params$volcano_plot$marker_size
opacity = self$params$volcano_plot$opacity
color_palette = self$params$volcano_plot$color_palette
reverse_palette = self$params$volcano_plot$reverse_palette
title_font_size = self$params$volcano_plot$title_font_size
y_label_font_size = self$params$volcano_plot$y_label_font_size
y_tick_font_size = self$params$volcano_plot$y_tick_font_size
x_label_font_size = self$params$volcano_plot$x_label_font_size
x_tick_font_size = self$params$volcano_plot$x_tick_font_size
legend_font_size = self$params$volcano_plot$legend_font_size
width = NULL
height = NULL


#------------------------------------------------------ MOFA TEST CELLMINER ----

prot_1 = example_omics(name = 'prot_1',
              type = "Proteomics",
              meta_file = './tests/datasets/CellMiner/sample_annotations.tsv',
              data_file = './tests/datasets/CellMiner/prot_data.csv',
              param_file = './R/params/params_gene_based_omics.R',
              id_col_meta = 'ID',
              type_column = 'Sample_type',
              group_column = 'Cancer type',
              batch_column = 'Batch',
              blank_pattern = "blank",
              qc_pattern = "quality",
              pool_pattern = "pool",
              excluded_samples = c('CNS:SF-539', 'PR:PC-3', 'PR:DU-145'),
              id_col_data = 'ID',
              blank_multiplier = 2,
              sample_threshold = 0.8,
              group_threshold = 0.8,
              excluded_features = NULL,
              imputation_method = "None",
              batch_effect_correction = "None",
              operation_order = c("Imputation", "Batch correction"),
              norm_col = "None")

trns_1 = example_omics(name = 'trns_1',
                       type = "Transcriptomics",
                       meta_file = './tests/datasets/CellMiner/sample_annotations.tsv',
                       data_file = './tests/datasets/CellMiner/trns_data.csv',
                       param_file = './R/params/params_gene_based_omics.R',
                       id_col_meta = 'ID',
                       type_column = 'Sample_type',
                       group_column = 'Cancer type',
                       batch_column = 'Batch',
                       blank_pattern = "blank",
                       qc_pattern = "quality",
                       pool_pattern = "pool",
                       excluded_samples = c('CNS:SF-539', 'PR:PC-3', 'PR:DU-145'),
                       id_col_data = 'ID',
                       blank_multiplier = 2,
                       sample_threshold = 0.8,
                       group_threshold = 0.8,
                       excluded_features = NULL,
                       imputation_method = "None",
                       batch_effect_correction = "None",
                       operation_order = c("Imputation", "Batch correction"),
                       norm_col = "None")

geno_1 = example_omics(name = 'geno_1',
                       type = "Genomics",
                       meta_file = './tests/datasets/CellMiner/sample_annotations.tsv',
                       data_file = './tests/datasets/CellMiner/geno_data.csv',
                       param_file = './R/params/params_gene_based_omics.R',
                       id_col_meta = 'ID',
                       type_column = 'Sample_type',
                       group_column = 'Cancer type',
                       batch_column = 'Batch',
                       blank_pattern = "blank",
                       qc_pattern = "quality",
                       pool_pattern = "pool",
                       excluded_samples = c('CNS:SF-539', 'PR:PC-3', 'PR:DU-145'),
                       id_col_data = 'ID',
                       blank_multiplier = 2,
                       sample_threshold = 0.8,
                       group_threshold = 0.8,
                       excluded_features = NULL,
                       imputation_method = "None",
                       batch_effect_correction = "None",
                       operation_order = c("Imputation", "Batch correction"),
                       norm_col = "None")


self = Mofa_class$new(
  name = "mofa_1"
)


self$tables$sample_metadata = NULL
self$tables$omics_tables = list()
self$tables$sample_tables = list()
self$tables$feature_tables = list()


# Add proteomics
self$add_data(name = prot_1$name,
              data_table = prot_1$tables$z_scored_data)
self$add_sample_meta(name = prot_1$name,
                     prot_1$tables$raw_meta)
self$add_feature_data(name = prot_1$name,
                      feature_data = prot_1$tables$feature_table)

# Add transcriptomics
self$add_data(name = trns_1$name,
              data_table = trns_1$tables$z_scored_data)
self$add_sample_meta(name = trns_1$name,
                     trns_1$tables$raw_meta)
self$add_feature_data(name = trns_1$name,
                      feature_data = trns_1$tables$feature_table)

# Add genomics
self$add_data(name = geno_1$name,
              data_table = geno_1$tables$z_scored_data)
self$add_sample_meta(name = geno_1$name,
                     geno_1$tables$raw_meta)
self$add_feature_data(name = geno_1$name,
                      feature_data = geno_1$tables$feature_table)

# Clean up tables
self$clean_datasets()

# Do MOFA
self$create_mofa_object()
self$prepare_mofa(scale_views = FALSE,
                  scale_groups = FALSE,
                  likelihoods = base::rep("gaussian", length(self$params$omics)),
                  num_factors = 10,
                  spikeslab_factors = FALSE,
                  spikeslab_weights = TRUE,
                  ard_factors = FALSE,
                  ard_weights = TRUE,
                  maxiter = 1000,
                  convergence_mode = "fast",
                  startELBO = 1,
                  freqELBO = 5,
                  stochastic = FALSE)
self$train_model(mofa_object = self$mofa_objects$pretrained,
               outfile = base::file.path("./models", timestamped_name("model.hdf5")),
               save_data = T)
self$add_metadata_to_mofa()

self$plot_explained_variance()
self$plots$explained_variance


model = self$mofa_objects$model
sample_metadata = self$tables$sample_metadata
factors = c(1:10)
scale = self$params$factor_plot$scale
groups = "Cancer type"
show_missing = self$params$factor_plot$show_missing
color_palette = "ggplot2"
marker_size = self$params$factor_plot$marker_size
opacity = self$params$factor_plot$opacity
add_violin = self$params$factor_plot$add_violin
show_legend = F
violin_alpha = self$params$factor_plot$violin_alpha
title_font_size = self$params$factor_plot$title_font_size
y_label_font_size = self$params$factor_plot$y_label_font_size
y_tick_font_size = self$params$factor_plot$y_tick_font_size
x_label_font_size = self$params$factor_plot$x_label_font_size
x_tick_font_size = self$params$factor_plot$x_tick_font_size
legend_font_size = self$params$factor_plot$legend_font_size
width = NULL
height = NULL

#------------------------------------------------------- SNF TEST CELLMINER ----

meta_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv'
param_file = './R/params/params_gene_based_omics.R'
id_col_meta = 'ID'
type_column = 'Sample_type'
group_column = 'Cancer type'
batch_column = 'Batch'
blank_pattern = "blank"
qc_pattern = "qc"
pool_pattern = "pool"
excluded_samples = NULL
id_col_data = 'ID'
blank_multiplier = 2
sample_threshold = 0.8
group_threshold = 0.8
excluded_features = NULL
imputation_method = "None"
batch_effect_correction = "None"
operation_order = c("Imputation", "Batch correction", "Filtering")
norm_col = "None"

prot_1 = example_omics(name = "prot_1",
                     type = "Proteomics",
                     meta_file = meta_file,
                     data_file = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/prot_data.csv",
                     param_file = param_file,
                     id_col_meta = id_col_meta,
                     type_column = type_column,
                     group_column = group_column,
                     batch_column = batch_column,
                     blank_pattern = blank_pattern,
                     qc_pattern = qc_pattern,
                     pool_pattern = pool_pattern,
                     excluded_samples = excluded_samples,
                     id_col_data = id_col_data,
                     blank_multiplier = blank_multiplier,
                     sample_threshold = sample_threshold,
                     group_threshold = group_threshold,
                     excluded_features = excluded_features,
                     imputation_method = imputation_method,
                     batch_effect_correction = batch_effect_correction,
                     operation_order = operation_order,
                     norm_col = norm_col)

trns_1 = example_omics(name = "trns_1",
                       type = "Transcriptomics",
                       meta_file = meta_file,
                       data_file = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data.csv",
                       param_file = param_file,
                       id_col_meta = id_col_meta,
                       type_column = type_column,
                       group_column = group_column,
                       batch_column = batch_column,
                       blank_pattern = blank_pattern,
                       qc_pattern = qc_pattern,
                       pool_pattern = pool_pattern,
                       excluded_samples = excluded_samples,
                       id_col_data = id_col_data,
                       blank_multiplier = blank_multiplier,
                       sample_threshold = sample_threshold,
                       group_threshold = group_threshold,
                       excluded_features = excluded_features,
                       imputation_method = imputation_method,
                       batch_effect_correction = batch_effect_correction,
                       operation_order = operation_order,
                       norm_col = norm_col)

geno_1 = example_omics(name = "geno_1",
                       type = "Genomics",
                       meta_file = meta_file,
                       data_file = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv",
                       param_file = param_file,
                       id_col_meta = id_col_meta,
                       type_column = type_column,
                       group_column = group_column,
                       batch_column = batch_column,
                       blank_pattern = blank_pattern,
                       qc_pattern = qc_pattern,
                       pool_pattern = pool_pattern,
                       excluded_samples = excluded_samples,
                       id_col_data = id_col_data,
                       blank_multiplier = blank_multiplier,
                       sample_threshold = sample_threshold,
                       group_threshold = group_threshold,
                       excluded_features = excluded_features,
                       imputation_method = imputation_method,
                       batch_effect_correction = batch_effect_correction,
                       operation_order = operation_order,
                       norm_col = norm_col)




self = Snf_class$new(
  name = "snf_1"
)

self$tables$metadata = NULL
self$tables$omics_tables = list()

self$add_meta(name = prot_1$name,
              prot_1$tables$raw_meta)
self$add_data(name = prot_1$name,
              data_table = prot_1$tables$z_scored_data)

self$add_meta(name = trns_1$name,
              trns_1$tables$raw_meta)
self$add_data(name = trns_1$name,
              data_table = trns_1$tables$z_scored_data)

self$add_meta(name = geno_1$name,
              geno_1$tables$raw_meta)
self$add_data(name = geno_1$name,
              data_table = geno_1$tables$z_scored_data)

print(names(self$tables$omics_tables))

self$clean_datasets()


self$plot_sample_clustering_1(data_table = "prot_1")
self$plots$sample_clustering_1


data_table = "prot_1"
meta_table = self$tables$sample_metadata
K_nearest_neighbors = self$params$sample_clustering_1$K_nearest_neighbors
sigma = self$params$sample_clustering_1$sigma
distance_method = self$params$sample_clustering_1$distance_method
K_clusters = self$params$sample_clustering_1$K_clusters
z_max = self$params$sample_clustering_1$z_max
z_min = self$params$sample_clustering_1$z_min
vertical_annotations = c("K clusters")
horizontal_annotations = self$params$sample_clustering_1$horizontal_annotations
color_palette = self$params$sample_clustering_1$color_palette
reverse_palette = self$params$sample_clustering_1$reverse_palette
title_font_size = self$params$sample_clustering_1$title_font_size
x_tick_font_size = self$params$sample_clustering_1$x_tick_font_size
y_tick_font_size = self$params$sample_clustering_1$y_tick_font_size
width = NULL
height = NULL





















