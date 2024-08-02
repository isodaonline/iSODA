base::source('./R/utils.R')
base::source('./R/class_omics_exp.R')
base::source('./R/class_mofa_exp.R')
base::source('./R/class_snf_exp.R')


name = 'geno_1'
type = "Genomics"
meta_file = 'C:/Users/dolivier/Desktop/local_work/230704_soda/240325_iSODA/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv'
data_file = 'C:/Users/dolivier/Desktop/local_work/230704_soda/240325_iSODA/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv'
param_file = './R/params/params_gene_based_omics.R'
id_col_meta = 'ID'
type_column = 'Sample_type'
group_column = 'Cancer type'
batch_column = 'Batch'
blank_pattern = "blank"
qc_pattern = "quality"
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


indexed_data = self$tables$indexed_data
indexed_meta = self$tables$indexed_meta
excluded_samples = self$indices$excluded_samples
excluded_features = self$indices$excluded_features
index_blanks = self$indices$index_blanks
index_qcs = self$indices$index_qcs
index_pools = self$indices$index_pools
batch_column = self$indices$batch_column
group_column = self$indices$group_column

self = example_omics(name = 'geno_1',
                     type = "Genomics",
                     meta_file = 'C:/Users/dolivier/Desktop/local_work/230704_soda/240325_iSODA/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv',
                     data_file = 'C:/Users/dolivier/Desktop/local_work/230704_soda/240325_iSODA/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv',
                     param_file = './R/params/params_gene_based_omics.R',
                     id_col_meta = 'ID',
                     type_column = 'Sample_type',
                     group_column = 'Cancer type',
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
                     operation_order = c("Imputation", "Batch correction", "Filtering"),
                     norm_col = "None")

#---------------------------------------------------------------- ALL TESTS ----
# CELLMINER DATA

meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_meta.tsv'
meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_non_epithelial.tsv'

self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_data.csv',
                          meta = meta,
                          group_column = 'Cancer type',
                          param_file = './R/params/params_gene_based_omics.R')

self = example_transcriptomics(name = 'trns_1',
                               data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/rna/rna_data.csv',
                               meta = meta,
                               group_column = 'Cancer type',
                               param_file = './R/params/params_gene_based_omics.R')

self = example_transcriptomics(name = 'geno_1',
                               data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv',
                               meta = meta,
                               group_column = 'Cancer type',
                               param_file = './R/params/params_gene_based_omics.R')

# CARLOS DATA
self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/240130_Carlos_data/2_Filtered_data/prot_data.csv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/240130_Carlos_data/2_Filtered_data/samp_annotations.csv',
                          param_file = './R/params/params_gene_based_omics.R',
                          group_column = 'APOE',
                          type_column = 'Final Status')


meta_table[c(1,10, 5, 16, 97, 100), 'sex'] = NA
rownames(meta_table)[is.na(meta_table$sex)]

## Tests
dataset = self$params$dendrogram$dataset
meta_table = self$tables$raw_meta
annotations = c("AAO", "sex")
distance_method = self$params$dendrogram$distance_method
p = self$params$dendrogram$p
clustering_method = self$params$dendrogram$clustering_method
k_clusters = self$params$dendrogram$k_clusters
color_palette = "Spectral"
show_dendrogram_dist = self$params$dendrogram$show_dendrogram_dist
rotate = self$params$dendrogram$rotate
width = NULL
height = NULL
self$plot_dendrogram(dataset = self$params$dendrogram$dataset,
                     meta_table = self$tables$raw_meta,
                     annotations = self$params$dendrogram$annotations,
                     distance_method = self$params$dendrogram$distance_method,
                     p = self$params$dendrogram$p,
                     clustering_method = self$params$dendrogram$clustering_method,
                     k_clusters = self$params$dendrogram$k_clusters,
                     color_palette = self$params$dendrogram$color_palette,
                     show_dendrogram_dist = self$params$dendrogram$show_dendrogram_dist,
                     rotate = self$params$dendrogram$rotate,
                     width = NULL,
                     height = NULL)


self$plot_heatmap()
self$plots$heatmap

dataset = self$params$heatmap$dataset
distance_method = self$params$heatmap$distance_method
clustering_method = self$params$heatmap$clustering_method
impute = self$params$heatmap$impute
center = T
meta_table = self$tables$raw_meta
meta_table_features = self$tables$feature_table
cluster_rows = self$params$heatmap$cluster_samples
cluster_cols = self$params$heatmap$cluster_features
row_annotations = self$params$heatmap$map_sample_data
col_annotations = self$params$heatmap$map_feature_data
map_feature_terms = self$params$heatmap$map_feature_terms
apply_da = self$params$heatmap$apply_da
group_column_da = self$params$heatmap$group_column_da
alpha_da = self$params$heatmap$alpha_da
seed_da = self$params$heatmap$seed_da
color_palette = self$params$heatmap$color_palette
reverse_palette = self$params$heatmap$reverse_palette
width = NULL
height = NULL

#-------------------------------------- COMBAT BATCH EFFECT CORRECTIO TESTS ----

base::source('./R/utils.R')
base::source('./R/class_omics_exp.R')
base::source('./R/class_mofa_exp.R')
base::source('./R/class_snf_exp.R')

#### Example lipidomics
name = 'lips_1'
id = NA
slot = NA
group_column = 'LTP_Family'
type_column = 'Sample_type'
batch_column = 'Batch'
blank_pattern = 'blank'
pool_pattern = 'pool'
qc_pattern = 'quality'
apply_imputation = F
impute_before = T
apply_filtering = T
data = './tests/datasets/LTP_KO/measurement_data.csv'
meta = './tests/datasets/LTP_KO/sample_annotations.csv'
param_file = './R/params/params_lipidomics.R'

lips_data = soda_read_table(data)
meta_data = soda_read_table(meta)

self = Omics_exp$new(name = name, type = "Lipidomics", id = id, slot = slot, preloaded = T, param_file = param_file)

self$tables$imp_meta = meta_data
self$tables$imp_data = lips_data

self$indices$id_col_meta = 'ID'
self$indices$id_col_data = 'ID'

self$indices$group_col = group_column
self$indices$type_col = type_column
self$indices$batch_col = batch_column
self$set_raw_meta()

# self$tables$raw_meta = self$tables$raw_meta[self$tables$raw_meta$Batch %in% c(1,2),]

type_vector = self$tables$imp_meta[, type_column]
blank_idx = grep(pattern = blank_pattern,
                 x = type_vector,
                 ignore.case = TRUE)
qc_idx = grep(pattern = qc_pattern,
              x = type_vector,
              ignore.case = TRUE)
pool_idx = grep(pattern = pool_pattern,
                x = type_vector,
                ignore.case = TRUE)

sample_idx = 1:nrow(self$tables$imp_meta)
sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

self$indices$idx_blanks = blank_idx
self$indices$idx_qcs = qc_idx
self$indices$idx_pools = pool_idx
self$indices$idx_samples = sample_idx

self$indices$rownames_blanks = self$tables$imp_meta[blank_idx, self$indices$id_col_meta]
self$indices$rownames_qcs = self$tables$imp_meta[qc_idx, self$indices$id_col_meta]
self$indices$rownames_pools = self$tables$imp_meta[pool_idx, self$indices$id_col_meta]
self$indices$rownames_samples = self$tables$imp_meta[sample_idx, self$indices$id_col_meta]

self$tables$raw_meta = self$tables$raw_meta[self$indices$rownames_samples,]

self$get_blank_table()
self$get_qc_table()
self$get_pool_table()

#### Set raw data
batch_effect_correction = "QC"
apply_imputation = apply_imputation
impute_before = impute_before
apply_filtering = apply_filtering
imputation_function = 'median'
blank_multiplier = 2
sample_threshold = 0.8
group_threshold = 0.8
norm_col = ''
imp_meta = self$tables$imp_meta
id_col_meta = self$indices$id_col_meta
idx_blanks = self$indices$idx_blanks
idx_qcs = self$indices$idx_qcs
idx_pools = self$indices$idx_pools
idx_samples = self$indices$idx_samples
batch_col = self$indices$batch_col



# Copy imported table
data_table = self$tables$imp_data

# Set ID column
rownames(data_table) = data_table[,self$indices$id_col_data]
data_table[,self$indices$id_col_data] = NULL
data_table = as.matrix(data_table)
#
# if (batch_effect_correction == "Pool") {
#   ctrl_idx = idx_pools
# } else if (batch_effect_correction == "QC") {
#   ctrl_idx = idx_qcs
# } else if (batch_effect_correction == "No controls") {
#   ctrl_idx = NULL
# } else {
#   base::stop('Batch effect correction: select either None, Pool, QC or No controls')
# }

# Set ID for imp meta
rownames(imp_meta) = imp_meta[,id_col_meta]
imp_meta[,id_col_meta] = NULL
data_table = data_table[rownames(imp_meta), ]
batches = imp_meta[, batch_col]


# if (length(ctrl_idx) > 0) {
#   imp_meta[, 'is_ctrl'] = F
#   imp_meta[ctrl_idx, 'is_ctrl'] = T
#   mod = stats::model.matrix(~is_ctrl, data=imp_meta)
# } else {
#   base::warning("Batch effect correction: no controls available, correcting without.")
#   mod = NULL
# }

# drop_cols = c()
# for (col in colnames(data_table)) {
#   ratio = (sum(is.na(data_table[,col])) / nrow(data_table))
#   if (ratio > 0.5) {
#     drop_cols = c(drop_cols, col)
#   }
# }
#
# if (length(ctrl_idx) > 0.5) {
#   for (col in colnames(data_table)) {
#     ratio = (sum(is.na(data_table[ctrl_idx,col])) / length(ctrl_idx))
#     if (ratio > 0.5) {
#       drop_cols = c(drop_cols, col)
#     }
#   }
# }



# if (length(drop_cols) > 0) {
#   data_table = data_table[,-(which(colnames(data_table) %in% drop_cols))]
#   base::warning(paste0('Batch effect correction: removed ', length(drop_cols), ' features with > 50% missing values.'))
# }


# # parametric adjustment
# data_table = sva::ComBat(dat=t(data_table),
#                          batch=batches,
#                          mod=mod,
#                          par.prior=TRUE,
#                          prior.plots=FALSE,
#                          mean.only = F,
#                          ref.batch = NULL)
#
# data_table = t(data_table)

dim(data_table)
dim(imp_meta)
data_table_=data_table[imp_meta$Sample_type=="Sample",]
imp_meta_=imp_meta[imp_meta$Sample_type=="Sample",]
batches_=imp_meta_$Batch
dim(data_table_)
dim(imp_meta_)

data_table2=data_table_[batches_%in%c(1,2),]
imp_meta2=imp_meta_[batches_%in%c(1,2),]
batches2=batches_[batches_%in%c(1,2)]
dim(data_table2)
length(batches2)

tmp=table(which(is.na(data_table2),arr.ind = T)[,2])

ind_to_remove=as.numeric(names(tmp)[which(tmp>(dim(data_table2)[1] * 0.5))])

data_table3=data_table2[,-ind_to_remove]

data_table4 = sva::ComBat(dat=t(data_table3),
                         batch=batches2,
                         mod=NULL,
                         par.prior=TRUE,
                         prior.plots=T,
                         mean.only = F,
                         ref.batch = NULL)

data_table4 = t(data_table4)


### Future development
fun1=function(x)
  x+x
fun2=function(x)
  x*x
fun3=function(x)
  x*x+x


funs=c(fun1,fun2,fun3)

ord=c(1,2,1)

res1=funs[[ord[3]]](1)
res2=funs[[ord[2]]](res1)
res3=funs[[ord[1]]](res2)
res3


#---------------------------------------------------- LIPIDOMICS BIROL TEST ----
name = 'lips_1'
id = NA
slot = NA
group_column = 'LTP_Family'
type_column = 'Sample_type'
batch_column = 'Batch'
blank_pattern = 'blank'
pool_pattern = 'pool'
qc_pattern = 'quality'
apply_imputation = F
impute_before = T
apply_filtering = T
data = './tests/datasets/LTP_KO/measurement_data.csv'
meta = './tests/datasets/LTP_KO/sample_annotations.csv'
param_file = './R/params/params_lipidomics.R'

self = example_lipidomics(name = 'lips_1',
                          id = NA,
                          slot = NA,
                          data = './tests/datasets/LTP_KO/measurement_data.csv',
                          meta = './tests/datasets/LTP_KO/sample_annotations.csv',
                          group_column = 'LTP_Family',
                          type_column = 'Sample_type',
                          param_file = './R/params/params_lipidomics.R')


data_table = self$params$pca$data_table
meta_table = self$tables$raw_meta
feature_table = self$tables$feature_table
sample_groups_col = self$params$pca$sample_groups_col
feature_groups_col = self$params$pca$feature_groups_col
impute_min = self$params$pca$impute_min
apply_da = self$params$pca$apply_da
sample_groups_da = self$params$pca$sample_groups_da
alpha_da = self$params$pca$alpha_da
seed_da = self$params$pca$seed_da
pca_method = self$params$pca$pca_method
nPcs = self$params$pca$nPcs
displayed_pc_1 = self$params$pca$displayed_pc_1
displayed_pc_2 = self$params$pca$displayed_pc_2
completeObs = self$params$pca$completeObs
displayed_plots = self$params$pca$displayed_plots
colors_palette = self$params$pca$colors_palette
marker_size = self$params$pca$marker_size
opacity = self$params$pca$opacity
title_font_size = self$params$pca$title_font_size
y_label_font_size = self$params$pca$y_label_font_size
y_tick_font_size = self$params$pca$y_tick_font_size
x_label_font_size = self$params$pca$x_label_font_size
x_tick_font_size = self$params$pca$x_tick_font_size
legend_font_size = self$params$pca$legend_font_size
return_data = TRUE
width = NULL
height = NULL

self$plot_feature_correlation(dataset = self$params$feature_correlation$dataset,
                              meta_table = self$tables$feature_table,
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
                              title_font_size = 15,
                              y_label_font_size = 15,
                              y_tick_font_size = 0,
                              x_label_font_size = 15,
                              x_tick_font_size = 0,
                              # legend_font_size = 15,
                              width = NULL,
                              height = NULL)



self$plots$feature_correlation

dataset = self$params$samples_correlation$dataset
meta_table = self$tables$raw_meta
correlation_method = self$params$samples_correlation$correlation_method
use = self$params$samples_correlation$use
distance_method = self$params$samples_correlation$distance_method
clustering_method = self$params$samples_correlation$clustering_method
k_clusters = self$params$samples_correlation$k_clusters
apply_clustering = self$params$samples_correlation$apply_clustering
center = self$params$samples_correlation$center
row_annotations = self$params$samples_correlation$row_annotations
col_annotations = self$params$samples_correlation$col_annotations
color_palette = self$params$samples_correlation$color_palette
reverse_palette = self$params$samples_correlation$reverse_palette
title_font_size = 15
y_label_font_size = 15
y_tick_font_size = 5
x_label_font_size = 15
x_tick_font_size = 5
# legend_font_size = 15
width = NULL
height = NULL

# Process fonts
xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
legend_show = base::ifelse(legend_font_size > 0, T, F)
x_axis_title = base::ifelse(x_label_font_size > 0, "Lipid class", "")
y_axis_title = base::ifelse(y_label_font_size > 0, "Concentration", "")
title = base::ifelse(title_font_size > 0, "Class distribution", "")



title_font_size = self$params$class_comparison$title_font_size,
y_label_font_size = self$params$class_comparison$y_label_font_size,
y_tick_font_size = self$params$class_comparison$y_tick_font_size,
x_label_font_size = self$params$class_comparison$x_label_font_size,
x_tick_font_size = self$params$class_comparison$x_tick_font_size,
legend_font_size = self$params$class_comparison$legend_font_size,

self$params$class_comparison$title_font_size = title_font_size
self$params$class_comparison$y_label_font_size = y_label_font_size
self$params$class_comparison$y_tick_font_size = y_tick_font_size
self$params$class_comparison$x_label_font_size = x_label_font_size
self$params$class_comparison$x_tick_font_size = x_tick_font_size
self$params$class_comparison$legend_font_size = legend_font_size



xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
legend_show = base::ifelse(legend_font_size > 0, T, F)
# x_axis_title = base::ifelse(x_label_font_size > 0, "Lipid class", "")
y_axis_title = base::ifelse(y_label_font_size > 0, "Concentration", "")
title = base::ifelse(title_font_size > 0, "Class distribution", "")



title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size,


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
  inputId = ns("class_comparison_x_label_font_size"),
  label = 'x-axis label font size',
  value = r6$params$class_comparison$x_label_font_size,
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




input$class_comparison_title_font_size,
input$class_comparison_y_label_font_size,
input$class_comparison_y_tick_font_size,
input$class_comparison_x_label_font_size,
input$class_comparison_x_tick_font_size,
input$class_comparison_legend_font_size,


title_font_size = input$class_comparison_title_font_size,
y_label_font_size = input$class_comparison_y_label_font_size,
y_tick_font_size = input$class_comparison_y_tick_font_size,
x_label_font_size = input$class_comparison_x_label_font_size,
x_tick_font_size = input$class_comparison_x_tick_font_size,
legend_font_size = input$class_comparison_legend_font_size,





#--------------------------------------------------------- LIPIDOMICS TESTS ----

self = example_lipidomics(name = 'lips_1',
                          id = NA,
                          slot = NA,
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/lipidomics.csv',
                          meta = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/lipidomics_metadata.csv",
                          group_column = 'Group_type',
                          type_column = 'Sample_type',
                          param_file = './R/params/params_lipidomics.R')

self$get_double_bonds_table()
self$plot_double_bonds_plot()

data_table = self$tables$double_bonds_table
adjustment = self$params$double_bonds_comparison$adjustment
group_1 = self$params$double_bonds_comparison$group_1
group_2 = self$params$double_bonds_comparison$group_2
carbon_selection = self$params$double_bonds_plot$selected_carbon_chain
unsat_selection = self$params$double_bonds_plot$selected_unsat
lipid_class = self$params$double_bonds_plot$selected_lipid_class
min_fc = self$params$double_bonds_plot$min_fc
max_pval = self$params$double_bonds_plot$max_pval
color_palette = self$params$double_bonds_plot$color_palette
reverse_palette = self$params$double_bonds_plot$reverse_palette
width = NULL
height = NULL

self$plot_double_bonds_plot(data_table = self$tables$double_bonds_table,
                            adjustment = self$params$double_bonds_comparison$adjustment,
                            group_1 = self$params$double_bonds_comparison$group_1,
                            group_2 = self$params$double_bonds_comparison$group_2,
                            carbon_selection = self$params$double_bonds_plot$selected_carbon_chain,
                            unsat_selection = self$params$double_bonds_plot$selected_unsat,
                            lipid_class = "TG",
                            fc_limits = self$params$double_bonds_plot$fc_limits,
                            pval_limits = self$params$double_bonds_plot$pval_limits,
                            width = NULL,
                            height = NULL)
self$plots$double_bond_plot


self$get_volcano_table(data_table = self$tables$total_norm_data,
                       adjustment_method = 'BH')
self$plot_volcano(feature_metadata = 'Lipid class',
                  displayed_plot = 'all')
self$plots$volcano_plot

data_table = self$tables$volcano_table
adjustment = self$params$volcano_plot$adjustment
group_1 = self$params$volcano_plot$group_1
group_2 = self$params$volcano_plot$group_2
feature_metadata = 'Lipid class'
keep_significant = self$params$volcano_plot$keep_significant
displayed_plot = 'all'
p_val_threshold = self$params$volcano_plot$p_val_threshold
fc_threshold = self$params$volcano_plot$fc_threshold
marker_size = self$params$volcano_plot$marker_size
opacity = self$params$volcano_plot$opacity
color_palette = self$params$volcano_plot$color_palette
width = NULL
height = NULL


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

#------------------------------------------------- FILTERING CELLMINER TEST ----

trns_data = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data.csv')
rownames(trns_data) = trns_data$ID
trns_heatmap = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/155946_heatmap_table_trns.csv', first_column_as_index = T)
trns_data = trns_data[rownames(trns_heatmap), c('ID', colnames(trns_heatmap))]


geno_data = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv')
rownames(geno_data) = geno_data$ID
geno_heatmap = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/160047_heatmap_table_geno.csv', first_column_as_index = T)
geno_data = geno_data[rownames(geno_heatmap), c('ID', colnames(geno_heatmap))]

sample_data = soda_read_table('/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv')
rownames(sample_data) = sample_data$ID
sample_data = sample_data[rownames(geno_heatmap),]

write.table(x = trns_data,
          file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data_filtered.csv',
          row.names = F,
          sep = '\t')
write.table(x = geno_data,
          file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data_filtered.csv',
          row.names = F,
          sep = '\t')
write.table(x = sample_data,
          file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.csv',
          row.names = F,
          sep = '\t')


#------------------------------------------------------- MOFA TEST RIK DATA ----

lips_1 = example_lipidomics(name = 'lips_1',
                            id = NA,
                            slot = NA,
                            data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/lipidomics.csv',
                            meta = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/lipidomics_metadata.csv",
                            group_column = 'Group_type',
                            param_file = './R/params/params_lipidomics.R')


prot_1 = example_proteomics(name = 'prot_1',
                            id = NA,
                            slot = NA,
                            data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/proteomics_2.tsv',
                            meta = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/metadata.csv",
                            group_column = 'Group_type',
                            param_file = './R/params/params_gene_based_omics.R')

trns_1 = example_transcriptomics(name = 'trns_1',
                                 id = NA,
                                 slot = NA,
                                 data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/transcriptomics_2.tsv',
                                 meta = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/metadata.csv",
                                 group_column = 'Group_type',
                                 param_file = './R/params/params_gene_based_omics.R')

self = Mofa_class$new(
  name = "mofa_1"
)

self$tables$sample_metadata = NULL
self$tables$omics_tables = list()
self$tables$sample_tables = list()
self$tables$feature_tables = list()



# Add lipidomics
self$add_data(name = lips_1$name,
              data_table = lips_1$tables$z_scored_data)
self$add_sample_meta(name = lips_1$name,
                     lips_1$tables$raw_meta)
self$add_feature_data(name = lips_1$name,
                      feature_data = lips_1$tables$feature_table)

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


# Clean up tables
self$clean_datasets()

# Do MOFA
self$create_mofa_object()
self$prepare_mofa(scale_views = FALSE,
                  scale_groups = FALSE,
                  likelihoods = base::rep("gaussian", length(self$params$omics)),
                  num_factors = 5,
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

meta_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_non_epithelial.tsv'
# meta_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv'

prot_1 = example_proteomics(name = 'prot_1',
                            id = NA,
                            slot = NA,
                            data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/prot_data.csv',
                            meta = meta_file,
                            group_column = 'Cancer type',
                            param_file = './R/params/params_gene_based_omics.R')

trns_1 = example_transcriptomics(name = 'trns_1',
                                 id = NA,
                                 slot = NA,
                                 data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data.csv',
                                 meta = meta_file,
                                 group_column = 'Cancer type',
                                 param_file = './R/params/params_gene_based_omics.R')

geno_1 = example_transcriptomics(name = 'geno_1',
                                 id = NA,
                                 slot = NA,
                                 data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv',
                                 meta = meta_file,
                                 group_column = 'Cancer type',
                                 param_file = './R/params/params_gene_based_omics.R')

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


self$plot_sample_clustering_1(data_table = "prot_1",
                              title_font_size = 10,
                              x_tick_font_size = 10,
                              y_tick_font_size = 10)

self$tables$sample_annotation_tables$prot_1
self$tables$sample_metadata


data_table = "prot_1"
meta_table = self$tables$sample_metadata
K_nearest_neighbors = self$params$sample_clustering_1$K_nearest_neighbors
sigma = self$params$sample_clustering_1$sigma
distance_method = self$params$sample_clustering_1$distance_method
K_clusters = self$params$sample_clustering_1$K_clusters
z_max = self$params$sample_clustering_1$z_max
z_min = self$params$sample_clustering_1$z_min
vertical_annotations = self$params$sample_clustering_1$vertical_annotations
horizontal_annotations = self$params$sample_clustering_1$horizontal_annotations
color_palette = self$params$sample_clustering_1$color_palette
reverse_palette = self$params$sample_clustering_1$reverse_palette
title_font_size = 10
x_tick_font_size = 10
y_tick_font_size = 10
width = NULL
height = NULL


#---------------------------------------------------- LIPIDOMICS TEST APO-E ----

self = example_lipidomics(name = 'lips_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/lipidomics.csv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/lipidomics_metadata.csv',
                          param_file = './R/params/params_lipidomics.R')


self$derive_data_tables()



self$plot_samples_correlation(color_palette = 'RdYlGn',
                              reverse_palette = T,
                              width = NULL,
                              height = NULL)
self$plots$samples_correlation


#------------------------------------------------ PROTEOMICS TEST CellMiner ----

self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_data.csv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_meta.tsv',
                          param_file = './R/params/params_gene_based_omics.R')

self$plot_samples_correlation(dataset = self$params$samples_correlation$dataset,
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
                              title_font_size = 15,
                              y_label_font_size = 15,
                              y_tick_font_size = 5,
                              x_label_font_size = 15,
                              x_tick_font_size = 5,
                              # legend_font_size = 15,
                              width = NULL,
                              height = NULL)

self$plots$samples_correlation


self$get_volcano_table()
self$plot_volcano(title_font_size = 0,
                  y_label_font_size = 0,
                  y_tick_font_size = 0,
                  x_label_font_size = 0,
                  x_tick_font_size = 0,
                  legend_font_size = 0)
self$plots$volcano_plot



title_font_size = 16
y_label_font_size = 20
y_tick_font_size = 15
x_label_font_size = 20
x_tick_font_size = 15
legend_font_size = 15





#------------------------------------------- TRANSCRIPTOMICS TEST CellMiner ----

# data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data.csv',
# meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv',

self = example_transcriptomics(name = 'trns_1',
                               data = 'C:/Users/dolivier/Desktop/local_work/230704_soda/230728_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/rna_data.csv',
                               meta = 'C:/Users/dolivier/Desktop/local_work/230704_soda/230728_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv',
                               group_column = 'Cancer type',
                               type_column = 'Sample_type',
                               param_file = './R/params/params_gene_based_omics.R')

# self$add_feature_table(name = 'feat_1',
#                        feature_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_feat_annotation_clean.tsv')
# self$derive_data_tables()


self$get_volcano_table(data_table = self$tables$total_norm_data,
                       group_1 = "Leukemia",
                       group_2 = "Melanoma",
                       adjustment_method = "BH")
self$plot_volcano(group_1 = "Leukemia",
                  group_2 = "Melanoma",
                  adjustment = "BH",
                  fc_threshold = 1.5)
self$plots$volcano_plot

self$push_volcano_to_meta()
unique(self$tables$feature_table$volcano_plot_expression)


selected_features = rownames(self$tables$volcano_plot)[self$tables$volcano_plot$expression %in% c("Underexpressed", "Overexpressed")]
selected_features = NULL


######### FEATURE SET ENRICHMENT ANALYSIS
self$get_ea_feature_table(data_table = 'Total normalized table',
                          group_col = "Cancer type",
                          group_1 = "Leukemia",
                          group_2 = "Melanoma",
                          fc_function = "mean",
                          statistical_test = "t-Test",
                          adjustment_method = "BH")

self$get_ea_object(custom_col = NULL,
                   selected_features = selected_features,
                   ont = "ALL",
                   minGSSize = 3,
                   maxGSSize = 800,
                   terms_p_value_cutoff = 0.05,
                   terms_pAdjustMethod = "BH",
                   verbose = TRUE,
                   OrgDb = "org.Hs.eg.db")

print(nrow(self$tables$ea_object))

######### OVER-REPRESENTATION ANALYSIS
self$get_ora_feature_table(data_table = 'Total normalized table',
                           group_col = "Cancer type",
                           group_1 = "Leukemia",
                           group_2 = "Melanoma",
                           fc_function = "mean",
                           statistical_test = "t-Test",
                           adjustment_method = "BH")
self$get_ora_object(custom_col = NULL,
                    selected_features = selected_features,
                    ont = "ALL",
                    minGSSize = 3,
                    maxGSSize = 800)


self$plot_ora_bar_plot(legend_font_size = 0)
self$plots$ora_bar_plot

label_font_size = self$params$ea_emap_plot$label_font_size,


self$plot_ea_ridge_plot()
self$plots$ea_ridge_plot


object = self$tables$ora_object
x = self$params$ora_bar_plot$x
color = self$params$ora_bar_plot$color
show_category = self$params$ora_bar_plot$show_category
displayed_label = self$params$ora_bar_plot$displayed_label
order_by = self$params$ora_bar_plot$order_by
order_decreasing = self$params$ora_bar_plot$order_decreasing
color_palette = self$params$ora_bar_plot$color_palette
reverse_palette = self$params$ora_bar_plot$reverse_palette
title_font_size = self$params$ora_bar_plot$title_font_size
yaxis_word_split = self$params$ora_bar_plot$yaxis_word_split
y_label_font_size = self$params$ora_bar_plot$y_label_font_size
y_tick_font_size = self$params$ora_bar_plot$y_tick_font_size
x_label_font_size = self$params$ora_bar_plot$x_label_font_size
x_tick_font_size = self$params$ora_bar_plot$x_tick_font_size
legend_font_size = self$params$ora_bar_plot$legend_font_size
width = NULL
height = NULL


title_font_size = 10
yaxis_word_split = 0
y_label_font_size = 10
y_tick_font_size = 10
x_label_font_size = 10
x_tick_font_size = 10
legend_font_size = 10

title_font_size = title_font_size,
yaxis_word_split = yaxis_word_split,
y_label_font_size = y_label_font_size,
y_tick_font_size = y_tick_font_size,
x_label_font_size = x_label_font_size,
x_tick_font_size = x_tick_font_size,
legend_font_size = legend_font_size,

# Process fonts
xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
legend_show = base::ifelse(legend_font_size > 0, T, F)
x_axis_title = base::ifelse(x_label_font_size > 0, "Lipid class", "")
y_axis_title = base::ifelse(y_label_font_size > 0, "Concentration", "")
title = base::ifelse(title_font_size > 0, "Class distribution", "")

colorbar = list(title = fill,
                tickformat = ".1e",
                tickfont = list(size = legend_font_size)),

title_font_size = self$params$ora_bar_plot$title_font_size,
yaxis_word_split = self$params$ora_bar_plot$yaxis_word_split,
y_label_font_size = self$params$ora_bar_plot$y_label_font_size,
y_tick_font_size = self$params$ora_bar_plot$y_tick_font_size,
x_label_font_size = self$params$ora_bar_plot$x_label_font_size,
x_tick_font_size = self$params$ora_bar_plot$x_tick_font_size,
legend_font_size = self$params$ora_bar_plot$legend_font_size,

self$params$ora_bar_plot$title_font_size = title_font_size
self$params$ora_bar_plot$yaxis_word_split = yaxis_word_split
self$params$ora_bar_plot$y_label_font_size = y_label_font_size
self$params$ora_bar_plot$y_tick_font_size = y_tick_font_size
self$params$ora_bar_plot$x_label_font_size = x_label_font_size
self$params$ora_bar_plot$x_tick_font_size = x_tick_font_size
self$params$ora_bar_plot$legend_font_size = legend_font_size



xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
legend_show = base::ifelse(legend_font_size > 0, T, F)
x_axis_title = base::ifelse(x_label_font_size > 0, "Lipid class", "")
y_axis_title = base::ifelse(y_label_font_size > 0, "Concentration", "")
title = base::ifelse(title_font_size > 0, "Class distribution", "")



title_font_size, yaxis_word_split, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size,


plot = plotly::layout(
  p = plot,
  title = list(text = title,
               font = list(size = title_font_size)),

  xaxis = list(
    title = list(
      text = x_axis_title,
      font = list(size = x_label_font_size)
    ),
    showticklabels = xtick_show,
    # showline = TRUE,
    # zeroline = FALSE,
    # mirror = TRUE,
    tickfont = list(size = x_tick_font_size)
  ),

  yaxis = list(
    title = list(
      text = y_axis_title,
      font = list(size = y_label_font_size)
    ),
    showticklabels = ytick_show,
    # showline = TRUE,
    # zeroline = FALSE,
    # mirror = TRUE,
    tickfont = list(size = y_tick_font_size)
  ),


  legend = list(
    font = list(
      size = legend_font_size
    )
  ),
  plot_bgcolor='rgba(0,0,0,0)',
  paper_bgcolor='rgba(0,0,0,0)'
)



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




input$ora_bar_plot_title_font_size,
input$ora_bar_plot_yaxis_word_split,
input$ora_bar_plot_y_label_font_size,
input$ora_bar_plot_y_tick_font_size,
input$ora_bar_plot_x_label_font_size,
input$ora_bar_plot_x_tick_font_size,
input$ora_bar_plot_legend_font_size,


title_font_size = input$ora_bar_plot_title_font_size,
yaxis_word_split = input$ora_bar_plot_yaxis_word_split,
y_label_font_size = input$ora_bar_plot_y_label_font_size,
y_tick_font_size = input$ora_bar_plot_y_tick_font_size,
x_label_font_size = input$ora_bar_plot_x_label_font_size,
x_tick_font_size = input$ora_bar_plot_x_tick_font_size,
legend_font_size = input$ora_bar_plot_legend_font_size,



#-------------------------------------------------- GENOMICS TEST CellMiner ----
self = example_transcriptomics(name = 'geno_1',
                               data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv',
                               meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv',
                               group_column = 'Cancer type',
                               param_file = './R/params/params_gene_based_omics.R')


self$add_feature_table(name = 'feat_1',
                       feature_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_feat_annotation_clean.tsv')



self$derive_data_tables()


self$get_volcano_table(data_table = self$tables$total_norm_data,
                       group_col = "Epithelial",
                       group_1 = "yes",
                       group_2 = "no",
                       adjustment_method = "BH")
self$plot_volcano(group_1 = "yes",
                  group_2 = "no",
                  adjustment = "BH",
                  fc_threshold = 1.5)
self$plots$volcano_plot

truffles = self$tables$volcano_plot
truffles = self$tables$volcano_plot

self$push_volcano_to_meta()

truffles = self$tables$feature_table


#---------------------------------------------------- PROTEOMICS TEST APO-E ----
self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_2.tsv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/metadata.csv',
                          param_file = './R/params/params_gene_based_omics.R')


self$add_feature_table(name = 'feat_1',
                       feature_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_feat_annotation_clean.tsv')



self$derive_data_tables()




#---------------------------------------------------- LIPIDOMICS RATNA TEST ----
self = example_lipidomics(name = 'lips_1',
                          id = NA,
                          slot = NA,
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/240605_Ratna/Lipidyzer_50_ANCHOR.csv',
                          meta = "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/240605_Ratna/Metadata_50_ANCHOR.csv",
                          group_column = 'GroupName',
                          type_column = 'SampleType',
                          qc_pattern = 'qc',
                          apply_imputation = F,
                          param_file = './R/params/params_lipidomics.R')

self$get_volcano_table(data_table = "Class normalized table",
                       sample_table = self$tables$raw_meta,
                       feature_table = self$tables$feature_table,
                       group_col = "Criterium2",
                       group_1 = 0,
                       group_2 = 1,
                       fc_function = self$params$volcano_plot_comparison$fc_function,
                       statistical_test = self$params$volcano_plot_comparison$statistical_test,
                       adjustment_method = self$params$volcano_plot_comparison$adjustment_method)

self$plot_volcano(data_table = self$tables$volcano_table,
                  adjustment = self$params$volcano_plot_comparison$adjustment,
                  group_1 = 0,
                  group_2 = 1,
                  feature_metadata = self$params$volcano_plot$feature_metadata,
                  keep_significant = self$params$volcano_plot$keep_significant,
                  displayed_plot = self$params$volcano_plot$displayed_plot,
                  p_val_threshold = self$params$volcano_plot$p_val_threshold,
                  fc_threshold = self$params$volcano_plot$fc_threshold,
                  marker_size = self$params$volcano_plot$marker_size,
                  opacity = self$params$volcano_plot$opacity,
                  color_palette = self$params$volcano_plot$color_palette,
                  reverse_palette = self$params$volcano_plot$reverse_palette,
                  width = NULL,
                  height = NULL)
self$plots$volcano_plot

data_table = "Class normalized table"
sample_table = self$tables$raw_meta
feature_table = self$tables$feature_table
group_col = "Criterium2"
group_1 = 0
group_2 = 1
fc_function = self$params$volcano_plot_comparison$fc_function
statistical_test = self$params$volcano_plot_comparison$statistical_test
adjustment_method = self$params$volcano_plot_comparison$adjustment_method






#------------------------------------------------------ NEW STRUCTURE TESTS ----

# Cellminer proteomics
name = 'prot_1'
type = 'Proteomics'
param_file = './R/params/params_gene_based_omics.R'
meta_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv'
id_col_meta = 'ID'
type_column = "Sample_type"
group_column = "Group_type"
batch_column = "Batch"
blank_pattern = "blank"
qc_pattern = "quality"
pool_pattern = "pool"
excluded_samples = NULL
id_col_data = 'ID'
blank_multiplier = 2
sample_threshold = 0.8
group_threshold = 0.8
excluded_features = NULL
imputation_method = "None"
batch_effect_correction = "None"

# Rik data lipidomics
name = 'lips_1'
type = 'Lipidomics'
param_file = './R/params/params_lipidomics.R'
meta_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/lipidomics_metadata.csv'
data_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_2/lipidomics.csv'
id_col_meta = 'ID'
type_column = "Sample_type"
group_column = "Group_type"
batch_column = "Batch"
blank_pattern = "blank"
qc_pattern = "quality"
pool_pattern = "pool"
excluded_samples = NULL
id_col_data = 'ID'
blank_multiplier = 2
sample_threshold = 0.8
group_threshold = 0.8
excluded_features = NULL
imputation_method = "None"
batch_effect_correction = "QC"

# Birol data lipidomics
name = 'lips_1'
type = 'Lipidomics'
param_file = './R/params/params_lipidomics.R'
meta_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/240419_Birol_data/iso_data/sample_annotations.csv'
data_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/240419_Birol_data/iso_data/measurement_data.csv'
id_col_meta = 'ID'
type_column = "Sample_type"
group_column = "LTP_Family"
batch_column = "Batch"
blank_pattern = "blank"
qc_pattern = "quality"
pool_pattern = "pool"
excluded_samples = NULL
id_col_data = 'ID'
blank_multiplier = 2
sample_threshold = 0.8
group_threshold = 0.8
excluded_features = NULL
imputation_method = "None"
batch_effect_correction = "QC"

# Birol data lipidomics (Windows)
name = 'lips_1'
type = 'Lipidomics'
param_file = './R/params/params_lipidomics.R'
meta_file = './tests/datasets/LTP_KO/sample_annotations.csv'
data_file = './tests/datasets/LTP_KO/measurement_data.csv'
id_col_meta = 'ID'
type_column = "Sample_type"
group_column = "LTP_Family"
batch_column = "Batch"
blank_pattern = "blank"
qc_pattern = "quality"
pool_pattern = "pool"
excluded_samples = NULL
id_col_data = 'ID'
blank_multiplier = 2
sample_threshold = 0.8
group_threshold = 0.8
excluded_features = NULL
imputation_method = "None"
batch_effect_correction = "QC"

####---- Initialize
self = Omics_exp$new(name = name,
                     type = type,
                     id = NULL,
                     slot = NULL,
                     preloaded = F,
                     param_file = param_file)


####---- Meta import
self$import_meta(meta_file)
self$set_indexed_meta(id_col = id_col_meta)

self$set_type_column(type_column = type_column)
self$set_group_column(group_column = group_column)
self$set_batch_column(batch_column = batch_column)

self$set_blank_indices(blank_pattern = blank_pattern)
self$set_qc_indices(qc_pattern = qc_pattern)
self$set_pool_indices(pool_pattern = pool_pattern)
self$set_sample_indices()



self$exclude_samples(manual_selection = excluded_samples,
                     select_blanks = T,
                     select_qcs = T,
                     select_pools = T,
                     exclude = T)

if (!is.null(excluded_samples)) {
  print(paste0('Excluded samples: ', self$indices$excluded_samples))
}

# self$reset_sample_exclusion()

self$set_raw_meta()

####---- Data import
self$import_data(data_file)
self$set_indexed_data(id_col = id_col_data)
self$feature_manual_exclusion(selection = excluded_features,
                              drop = T)
self$set_raw_data(operation_order = c("Imputation", "Batch correction", "Filtering"),
                  blank_multiplier = 2, # 2
                  sample_threshold = 0.8, # 0.8
                  group_threshold = 0.8, # 0.8
                  imputation_method = "median", # 'minimum' 'mean' 'median' 'max'
                  batch_effect_correction = "No controls", # None No controls Pool QC
                  norm_col = "None")

truffles = self$tables$raw_data


indexed_data = self$tables$indexed_data
indexed_meta = self$tables$indexed_meta
excluded_samples = self$indices$excluded_samples
excluded_features = self$indices$excluded_features
index_blanks = self$indices$index_blanks
index_qcs = self$indices$index_qcs
index_pools = self$indices$index_pools
batch_column = self$indices$batch_column
group_column = self$indices$group_column
operation_order = c("Imputation", "Batch correction", "Filtering")
blank_multiplier = 2 # 2
sample_threshold = 0.8 # 0.8
group_threshold = 0.8 # 0.8
imputation_method = "median" # 'minimum' 'mean' 'median' 'max'
batch_effect_correction = "No controls" # None No controls Pool QC
norm_col = "None"



### Future development
fun1=function(x)
  x+x
fun2=function(x)
  x*x
fun3=function(x)
  x*x+x


funs=c(fun1,fun2,fun3)

ord=c(1,2,1)

res1=funs[[ord[3]]](1)
res2=funs[[ord[2]]](res1)
res3=funs[[ord[1]]](res2)
res3






















#------------------------------------------------------------ Cite packages ----


used_packages = c("shiny",
                  "shinyjs",
                  "bs4Dash",
                  "shinyWidgets",
                  "shinybrowser",
                  "shinymanager",
                  "spsComps",
                  "waiter",
                  "ggplot2",
                  "gridExtra",
                  "plotly",
                  "visNetwork",
                  "heatmaply",
                  "ggpubr",
                  "ggupset",
                  "networkD3",
                  "igraph",
                  "ellipse",
                  "stringr",
                  "stringi",
                  "markdown",
                  "DT",
                  "readxl",
                  "Matrix",
                  "grDevices",
                  "RColorBrewer",
                  "viridisLite",
                  "stats",
                  "glmnet",
                  "pcaMethods",
                  "scales",
                  "org.Hs.eg.db",
                  "ggtree",
                  "clusterProfiler",
                  "enrichplot",
                  "ggridges",
                  "MOFA2",
                  "basilisk",
                  "SNFtool",
                  "sva",
                  "reshape2",
                  "dplyr",
                  "xfun",
                  "ggdendro",
                  "R6")


package_citations = ""
for (lib in used_packages) {
  package_citations = paste0(package_citations, ',\n', paste0(utils::toBibtex(utils::citation(lib)), collapse = "\n"))
}

writeLines(package_citations, "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/isoda_packages.bib")

install.packages(c("shiny",
"shinyjs",
"bs4Dash",
"shinyWidgets",
"shinybrowser",
"shinymanager",
"spsComps",
"waiter",
"ggplot2",
"gridExtra",
"plotly",
"visNetwork",
"heatmaply",
"ggpubr",
"ggupset",
"networkD3",
"igraph",
"ellipse",
"stringr",
"stringi",
"markdown",
"DT",
"readxl",
"Matrix",
"grDevices",
"RColorBrewer",
"viridisLite",
"stats",
"glmnet",
"scales",
"ggtree",
"ggridges",
"SNFtool",
"reshape2",
"dplyr",
"xfun",
"R6",
"ggdendro",
"BiocManager"))


BiocManager::install(c("pcaMethods",
                       "org.Hs.eg.db",
                       "clusterProfiler",
                       "enrichplot",
                       "MOFA2",
                       "basilisk",
                       "sva"))


