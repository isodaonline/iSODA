tooltip_data = list(
  data_upload = list( # Sublists based on R modules / namespaces
    # Metadata
    file_meta = "Browse for the sample annotations file (recommended: tsv, csv, csv2 or excel first sheet)",
    meta_file_format = "File format: Wide = samples as rows, descriptors as columns, Long: descriptors as rows, samples as columns. Default is Wide",
    select_meta_table = "Selected table to display",
    download_metatable = "Download the displayed table",
    select_id_meta = "Select ID column for samples (IDs must be unique)",
    select_group_col = "Main sample groups to compare (at least two groups and two samples per group)",
    select_type_col = "Column containing text pattern that allow to match blank, QC and pool samples",
    select_batch_col = "Batch column, if no batch column is found, samples are assumed from the same batch. Blanks, QCs and pools must also have a batch value for filtering",
    blank_pattern = "Substring to be searched in the type column to identify blanks",
    qc_pattern = "Substring to be searched in the type column to identify QCs",
    pool_pattern = "Substring to be searched in the type column to identify Pools",
    non_samples_selection = "Sample filtering - select non-samples (all three selected by default)",
    selection_manual = "Manual selection of samples",
    exclusion_meta_col = "Sample annotation column from which to select samples",
    exclusion_meta_val = "Select samples based on a specific value within the selected column",
    exclusion_meta_row = "Samples selected",
    selection_drop = "Sample filtering - drop selection",
    selection_keep = "Sample filtering - keep selection",
    clear_filters = "Sample filtering - clear selection",
    reset_meta = "Sample filtering - reset sample table",
    
    # Data
    file_data = "Browse for the measurements file (only numeric, recommended: tsv, csv, csv2 or excel first sheet)",
    data_file_format = "File format: Wide = samples as rows, features as columns, Long: features as rows, samples as columns. Default is Wide",
    select_data_table = "Selected table to display",
    download_datatable = "Download the displayed table",
    select_id_data = "Select ID column for samples (IDs must be unique)",
    select_feature_type = "Select the feature ID type (relevant for proteomics, transcriptomics and genomics data",
    operation_order = "Order in which Imputation, Batch correction and Filtering should be run. Removing = not running",
    batch_effect_correction = "Performs batch effect correction using ComBat, either without reference (No control), with reference samples (Pool or QC), or skipped (None).",
    na_imputation = "Replaces missing feature values by either their respective minimum, mean, median or maximum value. None = no imputation",
    blank_multiplier = "Tags samples with feature signals not significantly above blanks: sample signal > (mean(blank signal) * blank multiplier)",
    sample_threshold = "Sets the minimum percentage of samples tagged above blank signals for a feature to be kept, e.g. for 0.8: for feature A to be kept, 80% of samples must have signals tagged above blanks for that feature to be kept",
    group_threshold = "Alternative to the Sample threshold: operates on sample groups instead of all samples to also salvage features which might be produced only in one specific group",
    normalise_to_col = "Normalizes signals by dividing them by a column value in the sample annotations table (numeric)",
    feature_col_selection = "Choose a column from the feature table (if provided or if lipidomics) to start selecting features based on metadata",
    class_selection = "Select features based on a specific value within the selected column",
    manual_selection = "Manual selection of features",
    drop_cols = "Feature filtering - drop selection",
    keep_cols = "Feature filtering - keep selection",
    clear_data_filters = "Feature filtering - clear selection",
    reset_data_table = "Feature filtering - reset sample table",
    
    # Features
    feat_add = "Browse for feature metadata files (recommended: tsv, csv, csv2 or excel first sheet)",
    feat_name_add = "Name to store the table (several tables can be uploaded. Defaults to feat_$n",
    feat_file_format = "File format: Wide = descriptors as rows, features as columns, Long: features as rows, descriptors as columns. Default is Long",
    feat_table_select = "Selected table to display",
    download_feature_table = "Download the displayed table",
    feat_name_del = "Select the feature table to remove",
    feat_del = "Remove selected feature table"

    
  )
)