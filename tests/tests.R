base::source('./iSODA/R/utils.R')
base::source('./iSODA/R/class_omics_exp.R')
base::source('./iSODA/R/class_mofa_exp.R')
base::source('./iSODA/R/class_snf_exp.R')

#--------------------------------------------- DEBUG TRANSCRIPTOMICS 241010 ----

if (F) {
  name = 'lips_1'
  type = "Lipidomics"
  meta_file = './test_data/230828_multiomics_1/t_lipidomics_metadata.csv'
  data_file = './test_data/230828_multiomics_1/t_lipidomics.tsv'
  feat_file = NULL
  meta_file_format = "Long"
  data_file_format = "Long"
  feat_file_format = "Wide"
  param_file = './R/params/params_lipidomics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
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
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "median"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # LIPS
if (F) {
  name = 'lips_1'
  type = "Lipidomics"
  meta_file = './test_data/240605_Ratna/Metadata_50_ANCHOR.csv'
  data_file = './test_data/240605_Ratna/Lipidyzer_50_ANCHOR.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_lipidomics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
  type_column = 'SampleType'
  # group_column = 'NAS_groups'
  group_column = 'GroupName'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "qc"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = T
  drop_qcs = T
  drop_pools = T
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # LIPS
if (F) {
  name = 'lips_1'
  type = "Lipidomics"
  meta_file = './test_data/240419_Birol_data/iso_data/sample_annotations.csv'
  data_file = './test_data/240419_Birol_data/iso_data/measurement_data.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_lipidomics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
  type_column = 'Sample_type'
  group_column = 'LTP_Family'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "quality"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = T
  drop_qcs = T
  drop_pools = T
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # LIPS
if (F) {
  name = 'prot_1'
  type = "Proteomics"
  meta_file = './test_data/230828_multiomics_1/metadata.csv'
  data_file = './test_data/230828_multiomics_1/proteomics_2.tsv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
  type_column = 'Sample_type'
  group_column = 'Group_type'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "quality"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = F
  drop_qcs = F
  drop_pools = F
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # PROT
if (T) {
  name = 'prot_1'
  type = "Proteomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/prot_data.csv'
  feat_file = './test_data/231023_feature_tables/proteomics_feat_annotation_clean.tsv'
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'From'
  param_file = './R/params/params_gene_based_omics.R'
  type_column = 'Sample_type'
  group_column = 'Cancer type'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "quality"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = F
  drop_qcs = F
  drop_pools = F
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # PROT
if (F) {
  name = 'trns_1'
  type = "Transcriptomics"
  meta_file = './test_data/241010_Yassene/trns_meta.csv'
  data_file = './test_data/241010_Yassene/trns_data.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
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
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # TRNS
if (F) {
  name = 'trns_1'
  type = "Transcriptomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/t_sample_annotations.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/t_rna_data.tsv'
  feat_file = NULL
  meta_file_format = "Long"
  data_file_format = "Long"
  feat_file_format = "Wide"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
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
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # TRNS
if (F) {
  name = 'trns_1'
  type = "Transcriptomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/rna_data.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
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
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # TRNS
if (F) {
  name = 'geno_1'
  type = "Genomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/dna_data.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
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
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
} # GENO

self = initialize_omics(
  verbose = verbose,
  name = name,
  type = type,
  meta_file = meta_file,
  data_file = data_file,
  feat_file = feat_file,
  meta_file_format = meta_file_format,
  data_file_format = data_file_format,
  feat_file_format = feat_file_format,
  param_file = param_file,
  id_col_meta = id_col_meta,
  id_col_data = id_col_data,
  id_col_feat = id_col_feat,
  
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
  blank_multiplier = blank_multiplier,
  sample_threshold = sample_threshold,
  group_threshold = group_threshold,
  excluded_features = excluded_features,
  imputation_method = imputation_method,
  batch_effect_correction = batch_effect_correction,
  operation_order = operation_order,
  norm_col = norm_col
)

raw_feat = self$tables$raw_feat
colnames(raw_feat)
column_name = "Gene Ontology (GO)"
top_annotations = 50
self$add_sparse_feat(column_name = column_name)

names(self$tables$sparse_feat[[column_name]])
self$tables$sparse_feat[[column_name]]$sparse_matrix
terms_list = self$tables$sparse_feat[[column_name]]$terms_list
terms_table = self$tables$sparse_feat[[column_name]]$terms_table

#----------------------------------------------------- Tests by iSODA files ----

self = base::readRDS('./test_data/250108_Carla_data/20250108-104431_lips_1.isoda')

self$plot_fa_analysis()

data_table = self$params$fa_analysis_plot$data_table
feature_table = self$tables$raw_feat
sample_meta = self$tables$raw_meta
group_col = self$params$fa_analysis_plot$group_col
selected_view = self$params$fa_analysis_plot$selected_view
selected_lipidclass = "CE"
selected_fa = self$params$fa_analysis_plot$selected_fa
fa_norm = self$params$fa_analysis_plot$fa_norm
color_palette = self$params$fa_analysis_plot$color_palette
title_font_size = self$params$fa_analysis_plot$title_font_size
y_label_font_size = self$params$fa_analysis_plot$y_label_font_size
y_tick_font_size = self$params$fa_analysis_plot$y_tick_font_size
x_label_font_size = self$params$fa_analysis_plot$x_label_font_size
x_tick_font_size = self$params$fa_analysis_plot$x_tick_font_size
legend_font_size = self$params$fa_analysis_plot$legend_font_size
width = NULL
height = NULL


#------------------------------------------------------ MOFA TEST CELLMINER ----
if (T) {
  name = 'prot_1'
  type = "Proteomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/sample_annotations_filtered.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/prot_data.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'From'
  param_file = './R/params/params_gene_based_omics.R'
  type_column = 'Sample_type'
  group_column = 'Cancer type'
  batch_column = 'Batch'
  blank_pattern = "blank"
  qc_pattern = "quality"
  pool_pattern = "pool"
  excluded_samples = NULL
  drop_blanks = F
  drop_qcs = F
  drop_pools = F
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
  
  prot_1 = initialize_omics(
    verbose = verbose,
    name = name,
    type = type,
    meta_file = meta_file,
    data_file = data_file,
    feat_file = feat_file,
    meta_file_format = meta_file_format,
    data_file_format = data_file_format,
    feat_file_format = feat_file_format,
    param_file = param_file,
    id_col_meta = id_col_meta,
    id_col_data = id_col_data,
    id_col_feat = id_col_feat,
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
    blank_multiplier = blank_multiplier,
    sample_threshold = sample_threshold,
    group_threshold = group_threshold,
    excluded_features = excluded_features,
    imputation_method = imputation_method,
    batch_effect_correction = batch_effect_correction,
    operation_order = operation_order,
    norm_col = norm_col
  )
} # PROT
if (T) {
  name = 'trns_1'
  type = "Transcriptomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/rna_data.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
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
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
  
  trns_1 = initialize_omics(
    verbose = verbose,
    name = name,
    type = type,
    meta_file = meta_file,
    data_file = data_file,
    feat_file = feat_file,
    meta_file_format = meta_file_format,
    data_file_format = data_file_format,
    feat_file_format = feat_file_format,
    param_file = param_file,
    id_col_meta = id_col_meta,
    id_col_data = id_col_data,
    id_col_feat = id_col_feat,
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
    blank_multiplier = blank_multiplier,
    sample_threshold = sample_threshold,
    group_threshold = group_threshold,
    excluded_features = excluded_features,
    imputation_method = imputation_method,
    batch_effect_correction = batch_effect_correction,
    operation_order = operation_order,
    norm_col = norm_col
  )
} # TRNS
if (T) {
  name = 'geno_1'
  type = "Genomics"
  meta_file = './test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv'
  data_file = './test_data/230927_Cellminer_data/cellminer_data/dna_data.csv'
  feat_file = NULL
  meta_file_format = "Wide"
  data_file_format = "Wide"
  feat_file_format = "Long"
  param_file = './R/params/params_gene_based_omics.R'
  id_col_meta = 'ID'
  id_col_data = 'ID'
  id_col_feat = 'ID'
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
  blank_multiplier = 2
  sample_threshold = 0.8
  group_threshold = 0.8
  excluded_features = NULL
  imputation_method = "None"
  batch_effect_correction = "None"
  operation_order = c("Imputation", "Batch correction", "Filtering")
  norm_col = "None"
  verbose = T
  
  geno_1 = initialize_omics(
    verbose = verbose,
    name = name,
    type = type,
    meta_file = meta_file,
    data_file = data_file,
    feat_file = feat_file,
    meta_file_format = meta_file_format,
    data_file_format = data_file_format,
    feat_file_format = feat_file_format,
    param_file = param_file,
    id_col_meta = id_col_meta,
    id_col_data = id_col_data,
    id_col_feat = id_col_feat,
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
    blank_multiplier = blank_multiplier,
    sample_threshold = sample_threshold,
    group_threshold = group_threshold,
    excluded_features = excluded_features,
    imputation_method = imputation_method,
    batch_effect_correction = batch_effect_correction,
    operation_order = operation_order,
    norm_col = norm_col
  )
} # GENO

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
                      feature_data = prot_1$tables$raw_feat)

# Add transcriptomics
self$add_data(name = trns_1$name,
              data_table = trns_1$tables$z_scored_data)
self$add_sample_meta(name = trns_1$name,
                     trns_1$tables$raw_meta)
self$add_feature_data(name = trns_1$name,
                      feature_data = trns_1$tables$raw_feat)

# Add genomics
self$add_data(name = geno_1$name,
              data_table = geno_1$tables$z_scored_data)
self$add_sample_meta(name = geno_1$name,
                     geno_1$tables$raw_meta)
self$add_feature_data(name = geno_1$name,
                      feature_data = geno_1$tables$raw_feat)

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

base::saveRDS(self, "/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/230927_Cellminer_data/cellminer_data/MOFA_RDS.isoda")

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

prot_1 = initialize_omics(name = "prot_1",
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

trns_1 = initialize_omics(name = "trns_1",
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

geno_1 = initialize_omics(name = "geno_1",
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





















