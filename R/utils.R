# Utility functions
#--------------------------------------------------------- Switch functions ----

plotbox_switch = function(type) {
  switch (EXPR = type,
          "a" = plotly::renderPlotly,
          "b" = networkD3::renderSimpleNetwork,
          "c" = visNetwork::renderVisNetwork,
          "d" = shiny::renderPlot
  )
}

gene_ontology_switch = function(selection) {
  switch (EXPR = selection,
    'Gene ontology (ALL)' = 'ALL',
    'Gene ontology (BP)'= 'BP',
    'Gene ontology (MF)' = 'MF',
    'Gene ontology (CC)' = 'CC'
  )
}

experiment_switch = function(selection) {
  switch(EXPR = selection,
         'Lipidomics' = 'lips',
         'Metabolomics' = 'meta',
         'Proteomics' = 'prot',
         'Transcriptomics' = 'trns',
         'Genomics' = 'geno'
  )
}

adjustment_switch = function(selection){
  switch(EXPR = selection,
         "None" = "minus_log10_p_value",
         "Benjamini-Hochberg" = "minus_log10_p_value_bh_adj"
  )
}

adjustment_title_switch = function(selection) {
  switch(EXPR = selection,
         "minus_log10_p_value" = "-Log10(p-value)",
         "minus_log10_p_value_bh_adj" = "-Log10(BH(p-value))"
  )
}

feature_table_cols_switch = function(col) {
  switch(EXPR = col,
         'Lipid class' = 'lipid_class',
         'Double bonds (chain 1)' = 'unsat_1',
         'Carbon count (chain 1)' = 'carbons_1',
         'Double bonds (chain 2)' = 'unsat_2',
         'Carbon count (chain 2)' = 'carbons_2',
         'Double bonds (sum)' = 'unsat_sum',
         'Carbon count (sum)' = 'carbons_sum'
  )
}

r6_switch = function(exp_type, name, id, slot){
  switch(EXPR = exp_type,
         "Lipidomics" = Omics_exp$new(name = name, type = "Lipidomics",id = id, slot = slot, param_file = './R/params/params_lipidomics.R'),
         "Metabolomics" = Omics_exp$new(name = name, type = "Metabolomics",id = id, slot = slot, param_file = './R/params/params_metabolomics.R'),
         "Proteomics" = Omics_exp$new(name = name, type = "Proteomics",id = id, slot = slot, param_file = './R/params/params_gene_based_omics.R'),
         "Transcriptomics" = Omics_exp$new(name = name, type = "Transcriptomics",id = id, slot = slot, param_file = './R/params/params_gene_based_omics.R'),
         # "Transcriptomics" = base::readRDS("/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/iSODA_online_project/test_data/cellminer_iSODArds"),
         "Genomics" = Omics_exp$new(name = name, type = "Genomics",id = id, slot = slot, param_file = './R/params/params_gene_based_omics.R')

  )
}

table_switch = function(table_name, r6) {
  switch(EXPR = table_name,
         'Imported metadata table' = r6$tables$imp_meta,
         'Indexed metadata table' = r6$tables$indexed_meta,
         'Raw metadata table' = r6$tables$raw_meta,
         'Imported data table' = r6$tables$imp_data,
         'Indexed data table' = r6$tables$indexed_data,
         'Raw data table' = r6$tables$raw_data,
         'Imported feature table' = r6$tables$imp_feat,
         'Indexed feature table' = r6$tables$indexed_feat,
         'Raw feature table' = r6$tables$raw_feat,
         'Blank table' = r6$tables$blank_table,
         'Class normalized table' = r6$tables$class_norm_data,
         'Total normalized table' = r6$tables$total_norm_data,
         'Z-scored table' = r6$tables$z_scored_data,
         'Z-scored class normalized table' = r6$tables$z_scored_class_norm_data,
         'Z-scored total normalized table' = r6$tables$z_scored_total_norm_data,
         'Class table' = r6$tables$class_table,
         'Class table z-scored' = r6$tables$class_table_z_scored,
         'Class table total normalized' = r6$tables$class_table_total_norm,
         'Class table z-scored total normalized' = r6$tables$class_table_z_scored_total_norm,
         'Species summary table' = r6$tables$summary_species_table,
         'Class summary table' = r6$tables$summary_class_table,
         'GSEA prot list' = r6$tables$prot_list
         )
}

table_name_switch = function(table_name) {
  switch(EXPR = table_name,
         'Imported metadata table' = 'imp_meta',
         'Raw metadata table' = 'raw_meta',
         'Imported data table' = 'imp_data',
         'Raw data table' = 'raw_data',
         'Imported feature table' = 'imp_feature_table',
         'Feature table' = 'feature_table',
         'Blank table' = 'blank_table',
         'Class normalized table' = 'class_norm_data',
         'Total normalized table' = 'total_norm_data',
         'Z-scored table' = 'z_scored_data',
         'Z-scored class normalized table' = 'z_scored_class_norm_data',
         'Z-scored total normalized table' = 'z_scored_total_norm_data',
         'Class table' = 'class_table',
         'Class table z-scored' = 'class_table_z_scored',
         'Class table total normalized' = 'class_table_total_norm',
         'Class table z-scored total normalized' = 'class_table_z_scored_total_norm',
         'Species summary table' = 'summary_species_table',
         'Class summary table' = 'summary_class_table',
         'GSEA prot list' = 'prot_list'
  )
}

method_switch = function(method) {
  switch(EXPR = method,
         'minimum' = base::min,
         'mean' = base::mean,
         'median' = stats::median,
         'maximum' = base::max
  )
}

#--------------------------------------------------------------- Plot lists ----
lipidomics_plot_list = function() {
  plot_list = c("Dendrogram" = "select_dendrogram",
                "Class distribution" = "select_class_distribution",
                "Class comparison" = "select_class_comparison",
                "Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "Samples correlation" = "select_samples_correlation",
                "Feature correlation" = "select_feature_correlation",
                "PCA" = "select_pca",
                "Fatty acid analysis" = "select_fatty_acid_analysis",
                "Fatty acid composition" = "select_fatty_acid_composition",
                "Double bonds plot" = "select_double_bonds_plot"
  )
  return(plot_list)
}

generic_plot_list = function() {
  plot_list = c("Dendrogram" = "select_dendrogram",
                "Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "Samples correlation" = "select_samples_correlation",
                "Feature correlation" = "select_feature_correlation",
                "PCA" = "select_pca"
  )
  return(plot_list)
}

proteomics_plot_list = function() { # DEPRECATED
  plot_list = c("Dendrogram" = "select_dendrogram",
                "Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "Samples correlation" = "select_samples_correlation",
                "Feature correlation" = "select_feature_correlation",
                "PCA" = "select_pca"
  )
  return(plot_list)
}

gsea_plot_list = function() {
  plot_list = c("Dot plot" = "select_dot_plot",
                "Ridge plot" = "select_ridge_plot",
                "CNET plot" = "select_cnet_plot",
                "eMap plot" = "select_emap_plot"
  )
  return(plot_list)
}

or_plot_list = function() {
  plot_list = c("Dot plot" = "select_dot_plot",
                "Bar plot" = "select_bar_plot",
                "CNET plot" = "select_cnet_plot",
                "eMap plot" = "select_emap_plot"
  )
  return(plot_list)
}

get_mofa_plot_list = function() {
  plot_list = c("Explained variance" = "select_explained_variance",
                "Factor plot" = "select_factor_plot",
                "Combined factors plot" = "select_combined_factors_plot",
                "Feature weights" = "select_feature_weights",
                "Feature top weights" = "select_feature_top_weights",
                "MOFA Heatmap" = "select_mofa_heatmap",
                "Scatterplot" = "select_scatter_plot"
  )
  return(plot_list)
}

get_snf_plot_list = function() {
  plot_list = c("Sample clustering 1 " = "select_sample_clustering_1",
                "Sample clustering 2 " = "select_sample_clustering_2",
                'Similarity network 1' = 'select_similarity_network_1',
                'Similarity network 2' = 'select_similarity_network_2',
                "Fusion heatmap" = "select_fusion_heatmap",
                'Similarity network fusion' = 'select_similarity_network_fusion'
  )
  return(plot_list)
}


#---------------------------------------------------------- Purge functions ----
purge_module_inputs = function(id, input_object) {
  base::invisible(
    lapply(grep(id, names(input_object), value = TRUE), function(i) {
      .subset2(input_object, "impl")$.values$remove(i)
    })
  )
}

#---------------------------------------------------------- Table utilities ----

get_indexed_table = function(id_col,
                             input_table) {
  # Check if input_table exists
  if (is.null(input_table)) {
    base::stop('Import data table before setting an ID column')
  }
  
  # Check if id_col exists
  if (is.null(id_col)) {
    base::stop('ID column missing')
  }

  # If id_col is numeric
  if (is.numeric(id_col) && id_col == as.integer(id_col)) {
    id_col = colnames(input_table)[id_col]
  }
  
  # Check if id_col is a valid column name
  if (!id_col %in% colnames(input_table)) {
    stop(paste("Requested ID column '", id_col, "' not found in the table"))
  }

  # Check if missing values
  if (base::any(base::is.na((input_table[[id_col]])))) {
    stop(paste("Requested ID column '", id_col, "' contains missing values"))
  }
  
  # Check if id_col has unique values
  if (base::anyDuplicated(input_table[[id_col]]) > 0) {
    stop(paste("Requested ID column '", id_col, "' contains duplicate values"))
  }

  # # Check if id_col has mixed types
  # if (length(coercible_positions(input_table[[id_col]])) != nrow(input_table)) {
  #   stop(paste("Requested ID column '", id_col, "' mixed data types, must be either fully numeric or fully string"))
  # }

  # Check if id_col is coercible to numeric
  if (is_coercible_to_numeric(input_table[[id_col]])) {

    if (!is_integer_char_vector(input_table[[id_col]])) {
      base::stop(paste0('Values in requested ID column <', id_col, '> contains floats: only integers allowed'))
    }
    
    ids = as.numeric(input_table[[id_col]])
    input_table[[id_col]] = as.character(round(ids))
    

    # warning(paste("Requested ID column '", id_col, "' is numeric, coercing to strings"))
    # 
    # ids = as.numeric(input_table[[id_col]])
    # max_digits = nchar(as.character(max(ids)))
    # input_table[[id_col]] = sprintf(paste0("S%0", max_digits, "d"), ids)

  }

  # Set id_col as the row names of the table
  rownames(input_table) = input_table[[id_col]]

  # Remove the original id_col from the table
  input_table[[id_col]] = NULL

  return(input_table)
}

measurement_filtering = function(raw_data,
                                    blank_table,
                                    indexed_meta,
                                    batch_column,
                                    group_column,
                                    blank_multiplier,
                                    sample_threshold,
                                    group_threshold) {

  # Add blank rows to indexed meta, only if they are not already there
  blank_table_idx = base::setdiff(rownames(blank_table), rownames(raw_data))
  if (length(blank_table_idx) > 0) {
    indexed_meta = indexed_meta[c(rownames(raw_data),
                                  rownames(blank_table)),]
  }


  # Blank filtering
  excluded_features = c()
  all_batches = sort(unique(indexed_meta[, batch_column]))
  for (b in all_batches) {
    batch_idx = rownames(indexed_meta)[indexed_meta[, batch_column] == b]
    batch_blanks = base::intersect(batch_idx, rownames(blank_table))
    batch_samples = base::intersect(batch_idx, rownames(raw_data))

    excluded_features = c(excluded_features,
                          blank_filter(data_table = raw_data[batch_samples,],
                                       blank_table = blank_table[batch_blanks,],
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold))
  }

  # Group filtering
  if (!is.null(excluded_features)) {
    salvaged_features = c()
    for (g in unique(indexed_meta[rownames(raw_data),group_column])) {
      if ((g == "") | is.na(g)) {
        next
      }
      group_idx = rownames(indexed_meta)[indexed_meta[, group_column] == g]
      above_threshold = rep(0, length(excluded_features))
      names(above_threshold) = excluded_features
      for (b in unique(indexed_meta[group_idx, batch_column])) {

        batch_idx = rownames(indexed_meta)[indexed_meta[, batch_column] == b]
        batch_blanks = base::intersect(batch_idx, rownames(blank_table))
        batch_samples = base::intersect(batch_idx, group_idx)

        # get batch blank means
        blank_means = get_col_means(data_table = blank_table[batch_blanks,])
        threshold = blank_multiplier * blank_means

        # Find features / columns below threshold
        for (col in excluded_features) {
          above_threshold[col] = above_threshold[col] + sum(raw_data[batch_samples,col] > threshold[col], na.rm = T)
        }
      }
      above_threshold = above_threshold / length(group_idx) >= group_threshold
      salvaged_features = c(salvaged_features, names(above_threshold)[above_threshold])
    }
    excluded_features = sort(unique(setdiff(excluded_features, salvaged_features)))
    if (length(excluded_features) > 1) {
      raw_data = raw_data[,-which(colnames(raw_data) %in% excluded_features)]
    }
  }
  return(raw_data)
}

batch_effect_correction_combat = function(raw_data,
                                          batch_effect_correction,
                                          batch_column,
                                          indexed_meta,
                                          blank_table,
                                          qc_table,
                                          pool_table) {

  if (batch_effect_correction == "Pool") {
    ctrl_table = pool_table
    ctrl_index = rownames(ctrl_table)
  } else if (batch_effect_correction == "QC") {
    ctrl_table = qc_table
    ctrl_index = rownames(ctrl_table)
  } else if (batch_effect_correction == "No controls") {
    ctrl_index = NULL
  } else {
    base::stop('Batch effect correction: select either None, Pool, QC or No controls')
  }

  if (!is.null(ctrl_index)) {

    ctrl_table = ctrl_table[, colSums(is.na(ctrl_table)) == 0]

    remaining_features = base::intersect(colnames(raw_data), colnames(ctrl_table))
    if (ncol(raw_data) != length(remaining_features)) {
      base::warning(paste0("Batch effect correction: dropped ", ncol(raw_data) - length(remaining_features), " features not found in control."))
    }

    fused_data = base::rbind(raw_data[,remaining_features],
                             blank_table[,remaining_features],
                             ctrl_table[,remaining_features])
    fused_meta = indexed_meta[rownames(fused_data),]

  } else {
    remaining_features = base::intersect(colnames(raw_data), colnames(blank_table))
    fused_data = base::rbind(raw_data[,remaining_features],
                             blank_table[,remaining_features])
    fused_meta = indexed_meta[rownames(fused_data),]

  }

  # Set ID for imp meta
  batches = fused_meta[,batch_column]

  if (length(ctrl_index) > 0) {
    fused_meta[, 'is_ctrl'] = F
    fused_meta[ctrl_index, 'is_ctrl'] = T
    mod = stats::model.matrix(~is_ctrl, data=fused_meta)
  } else {
    mod = NULL
  }

  # parametric adjustment
  base::tryCatch({
    fused_data = sva::ComBat(dat=t(fused_data),
                             batch=batches,
                             mod=mod,
                             par.prior=TRUE,
                             prior.plots=FALSE,
                             mean.only = F,
                             ref.batch = NULL)
  },
  error = function(e) {
    base::stop('Too many missing values, consider imputation.')
  })


  fused_data = t(fused_data)

  raw_data = fused_data[rownames(raw_data),]
  blank_table = fused_data[rownames(blank_table),]

  return(list(
    raw_data = raw_data,
    blank_table = blank_table
  ))

}


#--------------------------------------------------------- Import functions ----

find_delim = function(path) {
  probe = paste(readLines(con = path, n = 10), collapse = "")
  sep = c("\t" = lengths(regmatches(probe, gregexpr("\t", probe))),
          "," = lengths(regmatches(probe, gregexpr(",", probe))),
          ";" = lengths(regmatches(probe, gregexpr(";", probe))))
  return(names(which.max(sep)))
}

soda_read_table = function(file_path, sep = NA, first_column_as_index = FALSE, transpose = F) {

  # Set sep
  if (is.na(sep)) {
    if (stringr::str_sub(file_path, -4, -1) == ".tsv") {
      sep = '\t'
    } else if (stringr::str_sub(file_path, -5, -1) != ".xlsx"){
      sep = find_delim(path = file_path)
    }
  }

  
  # Read table (if transpose, header is temporarily ignored)
  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table = readxl::read_xlsx(
        path = file_path,
        col_names = base::ifelse(transpose, F, T))
  } else {
    data_table = data.table::fread(
      file_path,
      header = base::ifelse(transpose, F, T),
      sep = sep)

  }
  
  # Transpose and set header on the final result
  if (transpose) {
    data_table = t(data_table)
    rownames(data_table) = NULL
    colnames(data_table) = data_table[1,]
    data_table = data_table[-1,]
  } 
  
  # # Convert to dataframe for consistency
  data_table = as.data.frame(data_table, check.names = F)

  # Set index if relevant and remove duplicated rows
  if (first_column_as_index) {
    duplicates = base::duplicated(data_table[,1])
    if (sum(duplicates) > 0) {
      warning(paste0('Removed ', sum(duplicates), ' duplicated rows'))
      data_table = data_table[!duplicates,]
    }
    rownames(data_table) = data_table[,1]
    data_table[,1] = NULL
  }
  
  # Remove duplicated columns
  duplicated_columns = which(base::duplicated(colnames(data_table)))
  if (length(duplicated_columns) > 0) {
    data_table = data_table[,-duplicated_columns]
    warning(paste0('Removed ', length(duplicated_columns), ' duplicated columns'))
  }
    
  return(data_table)
}

augment_feature_table = function(feature_table, external_table_name, external_feature_table) {
  feature_table$merge_on = rownames(feature_table)
  external_feature_table$merge_on = rownames(external_feature_table)

  feature_table = base::merge(feature_table, external_feature_table, by = 'merge_on', all.x = TRUE, suffixes = c('', paste0('_', external_table_name)))

  rownames(feature_table) = feature_table$merge_on
  feature_table$merge_on = NULL
  return(feature_table)
}

annotate_go = function(feature_names,
                       keyType,
                       ont,
                       pvalueCutoff) {
  # Checks
  if (!(ont %in% c('ALL', 'BP', 'MF', 'CC'))) {
    print('ont should be one of [ALL, BP, MF, CC]')
    return()
  }

  # Get GO annotations
  go_enrich_data = clusterProfiler::enrichGO(gene = feature_names,
                                             OrgDb = 'org.Hs.eg.db',
                                             keyType = keyType,
                                             ont = ont,
                                             pvalueCutoff = pvalueCutoff)

  # Extract the GO table & filter
  go_table = go_enrich_data@result
  go_table = go_table[go_table$p.adjust < pvalueCutoff,]

  if (nrow(go_table) == 0) {
    return(NULL)
  }

  if (ont != 'ALL') {
    go_table$ONTOLOGY = ont
  }

  # Split geneIDs by '/'
  feature_id = strsplit(go_table$geneID, "/")

  # Repeat GO terms based on the number of feature_id it corresponds to
  goIDs_rep = rep(go_table$ID, sapply(feature_id, length))

  # Convert feature_id list to dataframe
  feature_table = data.frame(feature_id = unlist(feature_id), ID = goIDs_rep, stringsAsFactors = FALSE)

  # Group by gene and concatenate GO terms
  feature_table = aggregate(ID ~ feature_id, data = feature_table, FUN = function(x) paste(unique(x), collapse = "|"))
  rownames(feature_table) = feature_table$feature_id
  feature_table$feature_id = NULL

  # Add features not associates with go terms
  missing_features = feature_names[!(feature_names %in% rownames(feature_table))]
  missing_features = data.frame(feature_id = missing_features,
                                ID = NA)
  rownames(missing_features) = missing_features$feature_id
  missing_features$feature_id = NULL
  feature_table = rbind(feature_table, missing_features)


  return(list(
    go_table = go_table[,c('Description', 'ONTOLOGY')],
    feature_table = feature_table
  ))

}


#-------------------------------------------------------- General utilities ----

is_none = function(value) {
  if (is.null(value)) {
    return(T)
  } else if (is.na(value)) {
    return(T)
  } else if (value == "") {
    return(T)
  } else {
    return(F)
  }
}

axis_word_split = function(ticks, n) {
  if (n == 0) {return(ticks)}
  out_ticks = c()
  for (tick in ticks) {
    out_ticks = c(out_ticks, replace_nth_space(tick, n))
  }
  return(out_ticks)
}

replace_nth_space = function(string, n) {
  # Split the string by spaces
  parts = unlist(strsplit(string, " "))

  # Replace every n-th element with the element and a newline character
  parts = sapply(seq_along(parts), function(i) {
    if (i %% n == 0) {
      return(paste0(parts[i], "\n"))
    } else {
      return(parts[i])
    }
  })

  # Collapse the parts back into a single string
  parts = paste(parts, collapse = " ")
  parts = base::trimws(base::gsub('\n ', '\n', parts))
  return(parts)
}


format_values = function(values, digits = 2) {
  # Check if the values is an integer by comparing it to a rounded version of itself
  if (all(values == round(values))) {
    if (stats::median(abs(values), na.rm = T) < 10000) {
      # Value is an integer and within a "readable" range
      return(as.character(values))
    } else {
      # Value is an integer but large, format using scientific notation
      return(base::format(values, scientific = TRUE, digits = digits))
    }
  } else {
    # The values is not an integer
    if(stats::median(abs(values), na.rm = T) < 0.02 || stats::median(abs(values), na.rm = T) > 10000) {
      # Value is very small or very large, use scientific notation
      return(base::format(values, scientific = TRUE, digits = digits))
    } else {
      # Value is within a reasonable range, round to a fixed number of decimal places
      return(base::format(round(values, digits), nsmall = 2))
    }
  }
}

hex_to_rgba = function(hex, alpha = 1) {
  # Ensure hex is a character and remove possible '#' symbol
  hex = base::gsub("^#", "", as.character(hex))

  # Convert hex to RGB values
  rgb = t(sapply(seq(1, nchar(hex), by=2), function(i) {
    as.integer(paste0("0x", substr(hex, i, i+1)))
  }))

  # Combine RGB values with alpha to create RGBA string
  rgba = paste0("rgba(", rgb[1], ", ", rgb[2], ", ", rgb[3], ", ", alpha, ")")

  return(rgba)
}

generate_id = function(n = 10) {
  chars = c(base::letters, base::LETTERS, 0:9)
  random_chars = base::sample(chars, n, replace = TRUE)
  out_id = paste(random_chars, collapse = "")
  return(out_id)
}

numeric_check = function(value, default = NULL) {
  if (!is.null(value)) {
    if (is_coercible_to_numeric(value)) {
      value = as.numeric(value)
    } else {
      warning(paste0("Non-numeric value supplied (", value, "), defaulting to  ", default))
      value = default
    }
  } else {
    warning(paste0("NULL supplied as value, defaulting to ", default))
    value = default
  }
  return(value)
}

rowsort_df = function(rows, dataframe) {
  original_col_names = colnames(dataframe)

  # If ordered vector
  if (is_coercible_to_numeric(rows)){
    row_names = rownames(dataframe)[rows]
  } else {
    row_names = rows
  }

  dataframe = base::as.data.frame(dataframe[rows,], row.names = row_names)
  colnames(dataframe) = original_col_names
  return(dataframe)
}

normalize_continuous = function(x, min, max, new_min, new_max) {
  ((x - min) / (max - min)) * (new_max - new_min) + new_min
}

adapt_color_palette = function(color_palette, values, named_vector = F, continuous = F) {

  # Extract hexadecimal colors from palette names
  color_palette = get_colors(color_count = colors_switch(color_palette), color_palette = color_palette)
  # color_palette = RColorBrewer::brewer.pal(colors_switch(color_palette), color_palette)

  # Scale colors if continuous, otherwise map 1:1
  if (continuous) {
    color_palette = colorRampPalette(color_palette)(100)
    normalized_values = normalize_continuous(x = values,
                                             min = min(values),
                                             max = max(values),
                                             new_min = 1,
                                             new_max = 100)
    normalized_values = round(normalized_values)
    color_palette = color_palette[normalized_values]
  } else {
    color_palette = colorRampPalette(color_palette)(length(values))
  }

  # Add names
  if (named_vector) {
    color_palette = stats::setNames(color_palette, values)
  }
  return(color_palette)
}

is_coercible_to_numeric = function(vector) {
  numeric_values = suppressWarnings(as.numeric(vector))
  if (any(is.na(numeric_values))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

coercible_positions = function(vector) {
  # Try to convert the vector to numeric
  numeric_values = suppressWarnings(as.numeric(vector))

  # Get the positions of values that are coercible to numeric
  coercible_positions = which(!is.na(numeric_values))

  return(coercible_positions)
}

is_integer_char_vector = function(vector) {
  numeric_vec = as.numeric(vector)
  !any(is.na(numeric_vec)) && all(floor(numeric_vec) == numeric_vec)
}

unique_na_rm = function(vector) {
  vector = vector[!is.na(vector)]
  vector = unique(vector)
  return(vector)
}

drop_rows = function(data_table, rows) {
  return(data_table[!(row.names(data_table) %in% rows),])
}

keep_rows = function(data_table, rows) {
  return(data_table[(row.names(data_table) %in% rows),])
}

drop_cols = function(data_table, cols) {
  return(data_table[,!(colnames(data_table) %in% cols), drop = F])
}

remove_empty_cols = function(table) {
  # filter out columns which are only NA
  del_cols = c()
  row_count = base::nrow(table)
  for (col in colnames(table)) {
    if (sum(is.na(table[,col])) == row_count){
      del_cols = c(del_cols, col)
    }
  }
  if (!is.null(del_cols)) {
    table = table[,-which(colnames(table) %in% del_cols)]
  }
  return(table)
}

is_num_coercible = function(x) {
  all(grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$', x, perl = TRUE))
}

#----------------------------------------------------- Print time functions ----

get_time = function() {
  return(format(Sys.time(), "%H:%M:%S"))
}

get_day_time = function() {
  return(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

get_time_code = function() {
  return(format(Sys.time(), "%H%M%S"))
}

get_day_time_code = function() {
  return(format(Sys.time(), "%Y%m%d-%H%M%S"))
}


timestamped_name = function(file_name) {
  return(paste0(get_day_time_code(), '_', file_name))
}

timestamped_module_name = function(r6, file_name) {
  return(paste0(get_day_time_code(), '_', r6$name, '_', file_name))
}

print_t = function(in_print) {
  spsComps::shinyCatch({message(in_print)}, prefix = '',
                       position = "bottom-left",
                       blocking_level = "none")
  write_to_log(paste0(get_time(), " - ",  in_print))
}

print_tm = function(m, in_print) {
  module_msg = paste0(m, " - ", in_print)
  write_to_log(paste0(get_time(), " - ",  module_msg))
  spsComps::shinyCatch({message(module_msg)}, prefix = '',
                       position = "bottom-left",
                       blocking_level = "none")
}

print_tme = function(m, e) {
  module_msg = paste0(m, " - ", e)
  write_to_log(paste0(get_time(), " - ERROR:",  module_msg))
  spsComps::shinyCatch({stop(module_msg)}, prefix = '',
                       position = "bottom-left",
                       blocking_level = "error")
}

print_tmw = function(m, w) {
  module_msg = paste0(m, " - ", w)
  write_to_log(paste0(get_time(), " - WARNING:",  module_msg))
  spsComps::shinyCatch({warning(module_msg)}, prefix = '',
                       position = "bottom-left",
                       blocking_level = "none")

}


write_to_log = function(msg) {
  if (!is.null(log_file)){
    if (!file.exists(log_file)) {
      base::writeLines(text = "START", log_file)
    }
    base::cat(base::trimws(msg), "\n", file=log_file, append=TRUE)
  }
}

#---------------------------------------------------------------- Plotboxes ----
# Plotly plotbox
get_plotly_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      ),
      tags$head(tags$style(HTML(paste0(".", paste0(id, "-boxtype"), " {display: none;}")))),
      tags$div(
        shiny::textInput(ns(paste0(id, "_boxtype")), label = NULL, value = "a"),
        class = paste0(id, "-boxtype")
      ),
    ),
    shiny::uiOutput(
      outputId = ns(paste0(id, "_error"))
    ),
    plotly::plotlyOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}


# NetworkD3 plotbox
get_networkd3_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      ),
      tags$head(tags$style(HTML(paste0(".", paste0(id, "-boxtype"), " {display: none;}")))),
      tags$div(
        shiny::textInput(ns(paste0(id, "_boxtype")), label = NULL, value = "b"),
        class = paste0(id, "-boxtype")
      ),
    ),
    shiny::uiOutput(
      outputId = ns(paste0(id, "_error"))
    ),
    networkD3::simpleNetworkOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}


# Visnet plotbox (for networks)
get_visnet_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      ),
      tags$head(tags$style(HTML(paste0(".", paste0(id, "-boxtype"), " {display: none;}")))),
      tags$div(
        shiny::textInput(ns(paste0(id, "_boxtype")), label = NULL, value = "c"),
        class = paste0(id, "-boxtype")
      ),
    ),
    shiny::uiOutput(
      outputId = ns(paste0(id, "_error"))
    ),
    visNetwork::visNetworkOutput(outputId = ns(paste0(id, "_plot")),
                                 width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                 height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

# Regular plotbox (static plots)
get_plot_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      ),
      tags$head(tags$style(HTML(paste0(".", paste0(id, "-boxtype"), " {display: none;}")))),
      tags$div(
        shiny::textInput(ns(paste0(id, "_boxtype")), label = NULL, value = "d"),
        class = paste0(id, "-boxtype")
      ),
    ),
    shiny::uiOutput(
      outputId = ns(paste0(id, "_error"))
    ),
    shiny::plotOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

#-------------------------------------------------------- Plotbox utilities ----

try_method = function(r6, method_name, ...) {
  base::withCallingHandlers({
    do.call(r6[[method_name]], list(...))
  },warning = function(w){
    print_tmw(r6$name, paste0(" Warning: " , w))
  },error=function(e){
    print_tme(r6$name, paste0(" Error:" , e))
  })
}

try_plot = function(prefix, r6, dimensions_obj, gen_function, spawn_function, img_format, toggle_function = NULL, input, output, session) {
  ns = session$ns
  error_output = paste0(gsub(" ", "_", tolower(prefix)), '_error')
  plot_output = paste0(gsub(" ", "_", tolower(prefix)), '_plot')
  plotbox_type = input[[paste0(gsub(" ", "_", tolower(prefix)), '_boxtype')]]
  waiter::waiter_show(
    id = ns(paste0(gsub(" ", "_", tolower(prefix)), '_plot')),
    html = spin_3k(),
    color = NULL
  )
  base::withCallingHandlers({
    gen_function(r6, dimensions_obj, input)
    output[[error_output]] = shiny::renderUI({NULL})
    spawn_function(r6 = r6, format = img_format, output = output)
    if (!is.null(toggle_function)) {
      r6[[toggle_function]]()
    }
    print_tm(r6$name, paste0(prefix, " Completed"))
    waiter::waiter_hide(
      id = ns(paste0(gsub(" ", "_", tolower(prefix)), '_plot'))
    )
  },warning = function(w){
    print_tmw(r6$name, paste0(prefix, " Warning: " , w))
    waiter::waiter_hide(
      id = ns(paste0(gsub(" ", "_", tolower(prefix)), '_plot'))
    )
  },error=function(e){
    msg = shiny::HTML(paste0('<h1 style="color: red;"><strong>Error</strong></h1><br><span style="color: red;"><strong>Error while producing the plot: </strong>', e, '</span>'))
    render_function = plotbox_switch(plotbox_type)
    output[[error_output]] = shiny::renderUI({msg})
    output[[plot_output]] = render_function({NULL})
    print_tme(r6$name, paste0(prefix, " Error:" , e))
    waiter::waiter_hide(
      id = ns(paste0(gsub(" ", "_", tolower(prefix)), '_plot'))
    )
  })

}


#----------------------------------------------------- Lipidomics functions ----

get_fa_tails = function(feature_table) {
  # get unique FA's, ignore PA
  feature_table = feature_table[feature_table[['Lipid class']] != "PA", ]
  fa_tails = c(
    paste0(feature_table[['Carbon count (chain 1)']][feature_table[['Lipid class']] != "TG"], ":", feature_table[['Double bonds (chain 1)']][feature_table[['Lipid class']] != "TG"]),
    paste0(feature_table[['Carbon count (chain 2)']], ":", feature_table[['Double bonds (chain 2)']])
  )

  fa_tails = unique(fa_tails)
  fa_tails = sort(fa_tails[fa_tails != "0:0"])

  return(fa_tails)
}

get_group_median_table = function(data_table,
                                  meta_table,
                                  group_col) {
  # Get the group information from the metadata
  groups = meta_table[, group_col]
  unique_groups = unique(groups)
  
  # Initialize an output matrix
  out_table = matrix(NA, nrow = length(unique_groups), ncol = ncol(data_table))
  colnames(out_table) = colnames(data_table)
  rownames(out_table) = unique_groups
  
  # Precompute logical indices for each group
  group_indices = lapply(unique_groups, function(g) groups == g)

  for (i in seq_along(unique_groups)) {
    group_idx = group_indices[[i]]
    group_table = data_table[group_idx, , drop = FALSE]
    
    # Use colMedians from the matrixStats package (optimized for large matrices)
    if (nrow(group_table) == 1) {
      group_medians = group_table
    } else {
      group_medians = matrixStats::colMedians(group_table, na.rm = TRUE)
    }
    
    # Replace 0 with NA
    group_medians[group_medians == 0] = NA
    
    # Assign the result to the appropriate row in the output matrix
    out_table[i, ] = group_medians
  }
  return(out_table)
}

get_lipid_class_table = function(table){

  # Get unique lipid classes
  classes = get_lipid_classes(feature_list = colnames(table), uniques = TRUE)

  # Get a column vector to find easily which columns belong to each lipid group
  col_vector = get_lipid_classes(feature_list = colnames(table), uniques = FALSE)
  
  is_tg_lipidyzer <- grepl(pattern = "^TG [0-9]{1,2}:[0-9]{1,2}-FA[0-9]{1,2}:[0-9]{1}$",
                           x = colnames(table))
  
  if(sum(is_tg_lipidyzer) > 0) {
    table[, col_vector == "TG"] = table[, col_vector == 'TG'] / 3
  }
  
  # table[, col_vector == "TG"] = table[, col_vector == 'TG'] / 3

  # Fill the table
  out_table = sapply(X = classes,
                     FUN = function(x) {
                       col_list = which(col_vector == x)
                       if (length(col_list) > 1) {
                         rowSums(table[,col_list], na.rm = T)
                       } else {
                         table[,col_list]
                       }
                     }
  )

  return(out_table)
}

normalise_lipid_class = function(lips_table) {
  # Get classes and unique classes for the lipid features
  classes = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = FALSE)
  classes_unique = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = TRUE)

  # For each unique lipid class...
  for (lip_class in classes_unique){

    # Get columns from that class...
    cols = which(classes == lip_class)
    if (length(cols) > 1) {
      class_row_sums = rowSums(lips_table[, cols], na.rm = T)
    } else {
      class_row_sums = lips_table[, cols]
      class_row_sums[is.na(class_row_sums)] = 0
    }
    class_row_sums[class_row_sums == 0] = 1
    lips_table[, cols] = lips_table[, cols] / class_row_sums
  }
  return(lips_table)
}

z_score_normalisation = function(data_table) {
  
  col_means = colMeans(data_table, na.rm = TRUE)
  col_sds = apply(data_table, 2, sd, na.rm = TRUE)
  normalized_data = base::sweep(data_table, 2, col_means, "-")
  normalized_data = base::sweep(normalized_data, 2, col_sds, "/")
  
  return(normalized_data)
}

impute_na = function(data_table, method) {
  imputation_func = method_switch(method)
  for (feature in colnames(data_table)){
    if (length(stats::na.omit(data_table[, feature])) > 0) {
      imputed_value = imputation_func(data_table[, feature], na.rm = T)
    } else {
      imputed_value = 0
    }
    data_table[is.na(data_table[, feature]), feature] = imputed_value
  }
  return(data_table)
}


get_lipid_classes = function(feature_list, uniques = TRUE){



  classes = sapply(feature_list, function(x)
    strsplit(x = x,
             split = " ",
             fixed = TRUE)[[1]][1])
  classes = as.vector(classes)
  if (uniques) {
    return(unique(classes))}
  else{
    return(classes)
  }
}

get_feature_metadata <- function(feature_table) {
  is_tg_lipidyzer <- grepl(pattern = "^TG [0-9]{1,2}:[0-9]{1,2}-FA[0-9]{1,2}:[0-9]{1}$",
                           x = rownames(feature_table))
  
  if(sum(is_tg_lipidyzer) > 0) {
    results <- get_feature_metadata.lipidyzer(feature_table = feature_table)
  } else {
    # results <- get_feature_metadata.general(feature_table)
  }
  
  return(results)
}

get_feature_metadata.lipidyzer = function(feature_table) {
  
  feature_table[, 'Lipid class'] = get_lipid_classes(feature_list = rownames(feature_table),
                                                     uniques = FALSE)
  # Collect carbon and unsaturation counts
  new_feature_table = list()
  
  for (c in unique(feature_table[, 'Lipid class'])) {
    idx = rownames(feature_table)[feature_table[, 'Lipid class'] == c]
    
    if (c == "TG") {
      # For triglycerides
      truffles = stringr::str_split(string = idx, pattern = " |:|-FA")
      names(truffles) = idx
      new_feature_table = c(new_feature_table, truffles)
      
    } else if (sum(stringr::str_detect(string = idx, pattern = "/|_")) > 0) {
      # For species with asyl groups ("/" or "_")
      truffles = stringr::str_split(string = idx, pattern = " |:|_|/")
      names(truffles) = idx
      new_feature_table = c(new_feature_table, truffles)
      
    } else {
      # For the rest
      truffles = paste0(idx, ':0:0')
      truffles = stringr::str_split(string = truffles, pattern = " |:")
      names(truffles) = idx
      new_feature_table = c(new_feature_table, truffles)
      
    }
  }
  
  new_feature_table = as.data.frame(t(data.frame(new_feature_table, check.names = F)), check.names = F)
  new_feature_table[,2] = gsub("[^0-9]", "", new_feature_table[,2])
  for (col in colnames(new_feature_table)[2:ncol(new_feature_table)]) {
    new_feature_table[,col] = as.numeric(new_feature_table[,col])
  }
  new_feature_table[,6] = new_feature_table[,2] + new_feature_table[,4]
  new_feature_table[,7] = new_feature_table[,3] + new_feature_table[,5]
  
  idx_tgs = which(new_feature_table[,1] == "TG")
  if (length(idx_tgs) > 0) {
    new_feature_table[idx_tgs,6] = new_feature_table[idx_tgs,2]
    new_feature_table[idx_tgs,7] = new_feature_table[idx_tgs,3]
  }
  
  colnames(new_feature_table) = c(
    'Lipid class',
    'Carbon count (chain 1)',
    'Double bonds (chain 1)',
    'Carbon count (chain 2)',
    'Double bonds (chain 2)',
    'Carbon count (sum)',
    'Double bonds (sum)'
  )
  
  new_feature_table = new_feature_table[rownames(feature_table),]
  
  return(new_feature_table)
}

get_col_means = function(data_table) {
  means = colMeans(data_table, na.rm = TRUE)
  means[is.na(means)] = 0
  return(means)
}

blank_filter = function(data_table, blank_table, blank_multiplier, sample_threshold, saved_cols = FALSE) {
  # Find features / columns below threshold
  blank_means = get_col_means(data_table = blank_table)
  del_cols = c()
  total_samples = nrow(data_table)
  for (col in colnames(data_table)){
    threshold = blank_multiplier * blank_means[col]
    above_threshold = sum(data_table[, col] > threshold, na.rm = T)
    if ((above_threshold/total_samples) < sample_threshold) {
      del_cols = c(del_cols, col)
    }
  }
  if (saved_cols) {
    del_cols = setdiff(colnames(data_table), del_cols)
  }
  return(del_cols)
}

# Implementation of blank filtering methods with r6 object
lips_get_del_cols = function(data_table,
                             blank_table,
                             imp_meta,
                             raw_meta,
                             idx_blanks,
                             idx_samples,
                             id_col_meta,
                             group_col,
                             batch_col,
                             blank_multiplier,
                             sample_threshold,
                             group_threshold) {

  # Blank filtering
  del_cols = c()
  all_batches = unique(imp_meta[, batch_col])
  for (b in all_batches) {
    batch_idx = which(imp_meta[, batch_col] == b)
    batch_blanks = base::intersect(batch_idx, idx_blanks)
    batch_samples = base::intersect(batch_idx, idx_samples)

    # Get rownames
    batch_blanks = imp_meta[batch_blanks, id_col_meta]
    batch_samples = imp_meta[batch_samples, id_col_meta]
    batch_samples = base::intersect(rownames(data_table), batch_samples)

    del_cols = c(del_cols, blank_filter(data_table = data_table[batch_samples,],
                                        blank_table = blank_table[as.character(batch_blanks),],
                                        blank_multiplier = blank_multiplier,
                                        sample_threshold = sample_threshold))
  }

  del_cols = unique(del_cols)
  del_cols = sort(del_cols)

  if (is.null(del_cols)) {
    return(del_cols)
  }

  # Group filtering
  saved_cols = c()
  for (g in unique(raw_meta[,group_col])) {
    group_idx = which(imp_meta[, group_col] == g)
    above_threshold = rep(0, length(del_cols))
    names(above_threshold) = del_cols
    for (b in unique(imp_meta[group_idx, batch_col])) {

      batch_idx = which(imp_meta[, batch_col] == b)
      batch_blanks = base::intersect(batch_idx, idx_blanks)
      batch_samples = base::intersect(batch_idx, group_idx)

      # Get rownames
      batch_blanks = imp_meta[batch_blanks, id_col_meta]
      batch_samples = imp_meta[batch_samples, id_col_meta]
      batch_samples = base::intersect(rownames(data_table), batch_samples)

      # get batch blank means
      blank_means = get_col_means(data_table = blank_table[as.character(batch_blanks),])
      threshold = blank_multiplier * blank_means

      # Find features / columns below threshold
      for (col in del_cols) {
        above_threshold[col] = above_threshold[col] + sum(data_table[batch_samples,col] >= threshold[col], na.rm = T)
      }
    }
    above_threshold = above_threshold / length(group_idx) >= group_threshold
    saved_cols = c(saved_cols, names(above_threshold)[above_threshold])
  }

  saved_cols = unique(saved_cols)
  saved_cols = sort(saved_cols)

  del_cols = setdiff(del_cols, saved_cols)

  return(del_cols)
}

#------------------------------------------------------- Plotting functions ----

plot_feature_annotation_distribution = function(indexed_feat = self$tables$indexed_feat,
                                                input_table = self$tables$raw_feat,
                                                column) {
  
  if (!(column %in% colnames(indexed_feat))) {
    return(create_blank_plot(label = "Annotation distribution (select below)"))
  }
  
  if (length(unique(table(indexed_feat[,column]))) > 50) {
    return(create_blank_plot(label = "Annotation distribution (select below)"))
  }
  
  full_data = table(indexed_feat[,column])
  filtered_data = table(factor(input_table[, column], levels = names(full_data)))
  filtered_data =  full_data - filtered_data
  
  plot_data = data.frame(list(
    remaining = as.vector(unname(full_data - filtered_data)),
    filtered = as.vector(unname(filtered_data)),
    name = factor(paste(names(filtered_data), " "), levels = paste(names(filtered_data), " "))
  ),
  check.names = F)
  
  fig = plotly::plot_ly(
    x = plot_data$remaining,
    y = plot_data$name,
    type = 'bar',
    orientation = "h",
    marker = list(color = 'deepskyblue'),
    name = 'Remaining')
  fig = plotly::add_trace(
    p = fig,
    x = plot_data$filtered,
    marker = list(color = 'lightblue'),
    name = 'Filtered')
  fig = plotly::layout(
    p = fig,
    barmode = 'stack',
    title = paste0(column, " distribution"),
    dragmode = FALSE,
    legend = list(
      orientation = 'h',
      x = 0.5, 
      y = -0.05, 
      xanchor = 'center',
      yanchor = 'top' 
    ))
  fig = plotly::config(
    p = fig,
    displayModeBar = FALSE,
    scrollZoom = FALSE
  )
  return(fig)
}

plot_bar_missingness = function(input_table,
                                type) {
  if (!(type %in% c('Samples', 'Features'))){
    base::stop('type should be either Samples or Features')
  } else if (type == "Samples") {
    idx = 1
    autorange = "reversed"
    table_names = rownames(input_table)
  } else {
    idx = 2
    autorange = NULL
    table_names = colnames(input_table)
  }
  
  missing_data = base::which(is.na(input_table), arr.ind = T)
  if (length(missing_data) == 0){
    return(create_blank_plot(label = paste0(type, ": no missing values")))
  }
  missing_data = sort(table(missing_data[,idx]), decreasing = T)
  names(missing_data) = table_names[as.integer(names(missing_data))]
  missing_data = missing_data[1:min(10, length(missing_data))]
  missing_data = sort(missing_data, decreasing = F)
  data_names = factor(names(missing_data), levels = names(missing_data))
  
  
  fig = plotly::plot_ly(
    x = missing_data,
    y = data_names,
    type = "bar",
    marker = list(color = 'deepskyblue'),
    orientation = "h"
  )
  fig = plotly::layout(
    p = fig,
    title = paste0(type, " missingness (top 10)"),
    xaxis = list(autorange = autorange),
    dragmode = FALSE
  )
  fig = plotly::config(
    p = fig,
    displayModeBar = FALSE
  )
  return(fig)
}

plot_sample_types_per_batch = function(
    imp_meta,
    type_column,
    batch_column = NULL,
    blank_pattern,
    qc_pattern,
    pool_pattern) {
  if (is.null(batch_column)) {
    batch_column = 'tmp_batch'
    imp_meta[,batch_column] = 1
  }
  batches = unique(imp_meta[,batch_column])
  batch_blanks = c()
  batch_qcs = c()
  batch_pools = c()
  batch_mixed = c()
  batch_samples = c()
  
  for (batch in batches) {
    batch_idx = rownames(imp_meta)[imp_meta[,batch_column] == batch]
    
    index_blanks = rownames(imp_meta)[grep(
      pattern = blank_pattern,
      x = imp_meta[batch_idx, type_column],
      ignore.case = TRUE)]
    
    index_qcs = rownames(imp_meta)[grep(
      pattern = qc_pattern,
      x = imp_meta[batch_idx, type_column],
      ignore.case = TRUE)]
    
    index_pools = rownames(imp_meta)[grep(
      pattern = pool_pattern,
      x = imp_meta[batch_idx, type_column],
      ignore.case = TRUE)]
    
    mixed_samples = unique(c(intersect(index_blanks, index_qcs), intersect(index_qcs, index_pools), intersect(index_blanks, index_pools)))
    
    index_samples = base::setdiff(rownames(imp_meta), c(index_blanks, index_qcs, index_pools))
    
    batch_blanks = c(batch_blanks, length(index_blanks))
    batch_qcs = c(batch_qcs, length(index_qcs))
    batch_pools = c(batch_pools, length(index_pools))
    batch_mixed = c(batch_mixed, length(mixed_samples))
    batch_samples = c(batch_samples, length(index_samples))
    
  }
  
  data = base::data.frame(
    "Batch" = as.character(batches),
    "Samples" = batch_samples,
    "Blanks" = batch_blanks,
    "QCs" = batch_qcs,
    "Pools" = batch_pools,
    "Mixed" = batch_mixed)
  
  fig = plotly::plot_ly(x = data$Batch, y = data$Samples, type = 'bar', name = 'Samples')
  for (col in colnames(data)[3:length(data)]) {
    fig = plotly::add_trace(
      p = fig,
      y = data[[col]],
      type = 'bar',
      name = col)
  }
  fig = plotly::layout(
    p = fig,
    yaxis = list(title = 'Count'),
    barmode = 'stack')
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
  
  return(fig)
  
}

plot_sample_groups_per_batch = function(
    imp_meta,
    batch_column = NULL,
    group_column) {
  
  if (is.null(batch_column)) {
    batch_column = 'tmp_batch'
    imp_meta[,batch_column] = 1
  }
  
  imp_meta[,group_column] = factor(imp_meta[,group_column], levels = unique(imp_meta[,group_column]))
  
  batches = unique(imp_meta[,batch_column])
  data = list(
    "Batch" = batches
  )
  
  for (batch in batches) {
    batch_idx = rownames(imp_meta)[imp_meta[,batch_column] == batch]
    freq = base::table(imp_meta[batch_idx, group_column])
    for (grp in names(freq)) {
      data[[grp]] = c(data[[grp]], as.numeric(freq[grp]))
    }
  }
  
  data = data.frame(data, check.names = F)
  
  fig = plotly::plot_ly(x = data$Batch)
  for (col in levels(imp_meta[,group_column])) {
    fig = plotly::add_trace(
      p = fig,
      y = data[[col]],
      type = 'bar',
      name = col)
  }
  fig = plotly::layout(
    p = fig,
    yaxis = list(title = 'Count'),
    barmode = 'stack')
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
  
  return(fig)
  
}

fa_comp_hm_calc = function(data_table = NULL,
                           feature_table = NULL,
                           composition = NULL,
                           group_col = NULL,
                           selected_group = NULL,
                           sample_meta = NULL,
                           selected_lipidclass = NULL) {

  res = switch(
    composition,
    "fa_tail" = fa_comp_hm_calc.fa(data_table = data_table,
                                   feature_table = feature_table,
                                   group_col = group_col,
                                   selected_group = selected_group,
                                   sample_meta = sample_meta,
                                   selected_lipidclass = selected_lipidclass),
    "total_lipid" = fa_comp_hm_calc.total(data_table = data_table,
                                          feature_table = feature_table,
                                          group_col = group_col,
                                          selected_group = selected_group,
                                          sample_meta = sample_meta,
                                          selected_lipidclass = selected_lipidclass)
  )

  return(res)
}

fa_comp_hm_calc.fa = function(data_table = NULL,
                              feature_table = NULL,
                              composition = NULL,
                              group_col = NULL,
                              selected_group = NULL,
                              sample_meta = NULL,
                              selected_lipidclass = NULL) {
  ## samples
  idx_samples = rownames(sample_meta)[sample_meta[, group_col] == selected_group]
  hm_data = data_table[idx_samples, , drop = FALSE]

  ## features
  feature_table$lipid = rownames(feature_table)
  if(selected_lipidclass == "All") {
    # leave PA's out
    selected_features = feature_table[feature_table[["Lipid class"]] != "PA", ]
  } else {
    selected_features = feature_table[feature_table[["Lipid class"]] == selected_lipidclass, ]
  }
  # get the unique chain lengths and unsaturation
  # special lipid classes
  tail1_only = c("CE", "FA", "LPC", "LPE")
  tail2_only = c("Cer", "HexCER", "LacCER", "SM", "TG")

  if(selected_lipidclass == "All") {
    uniq_carbon = c(min(c(selected_features[["Carbon count (chain 2)"]][selected_features[["Lipid class"]] %in% tail2_only],
                          selected_features[["Carbon count (chain 1)"]][!(selected_features[["Lipid class"]] %in% tail2_only)],
                          selected_features[["Carbon count (chain 2)"]][!(selected_features[["Lipid class"]] %in% c(tail1_only, tail2_only))])),
                    max(c(selected_features[["Carbon count (chain 2)"]][selected_features[["Lipid class"]] %in% tail2_only],
                          selected_features[["Carbon count (chain 1)"]][!(selected_features[["Lipid class"]] %in% tail2_only)],
                          selected_features[["Carbon count (chain 2)"]][!(selected_features[["Lipid class"]] %in% c(tail1_only, tail2_only))])))
    uniq_unsat = c(min(c(selected_features[["Double bonds (chain 2)"]][selected_features[["Lipid class"]] %in% tail2_only],
                         selected_features[["Double bonds (chain 1)"]][!(selected_features[["Lipid class"]] %in% tail2_only)],
                         selected_features[["Double bonds (chain 2)"]][!(selected_features[["Lipid class"]] %in% c(tail1_only, tail2_only))])),
                   max(c(selected_features[["Double bonds (chain 2)"]][selected_features[["Lipid class"]] %in% tail2_only],
                         selected_features[["Double bonds (chain 1)"]][!(selected_features[["Lipid class"]] %in% tail2_only)],
                         selected_features[["Double bonds (chain 2)"]][!(selected_features[["Lipid class"]] %in% c(tail1_only, tail2_only))])))
  } else {
    if(selected_lipidclass %in% tail2_only) {
      uniq_carbon = c(min(selected_features[["Carbon count (chain 2)"]]),
                      max(selected_features[["Carbon count (chain 2)"]]))
      uniq_unsat = c(min(selected_features[["Double bonds (chain 2)"]]),
                     max(selected_features[["Double bonds (chain 2)"]]))
    } else {
      uniq_carbon = c(min(c(selected_features[["Carbon count (chain 1)"]], selected_features[["Carbon count (chain 2)"]][selected_features[["Carbon count (chain 2)"]] != 0])),
                      max(c(selected_features[["Carbon count (chain 1)"]], selected_features[["Carbon count (chain 2)"]])))
      uniq_unsat = c(min(c(selected_features[["Double bonds (chain 1)"]], selected_features[["Double bonds (chain 2)"]])),
                     max(c(selected_features[["Double bonds (chain 1)"]], selected_features[["Double bonds (chain 2)"]])))
    }
  }

  ## calculations
  # initialize result matrix
  res = matrix(ncol = length(uniq_carbon[1]:uniq_carbon[2]),
               nrow = length(uniq_unsat[1]:uniq_unsat[2]))
  colnames(res) = uniq_carbon[1]:uniq_carbon[2]
  rownames(res) = uniq_unsat[1]:uniq_unsat[2]

  for(a in rownames(res)) { # unsaturation
    for(b in colnames(res)) { # carbons
      idx_lipids = selected_features$lipid[(selected_features[["Carbon count (chain 1)"]] == b &
                                              selected_features[["Double bonds (chain 1)"]] == a) |
                                             (selected_features[["Carbon count (chain 2)"]] == b &
                                                selected_features[["Double bonds (chain 2)"]] == a)]

      idx_lipids_double = selected_features$lipid[(selected_features[["Carbon count (chain 1)"]] == b &
                                                     selected_features[["Double bonds (chain 1)"]] == a) &
                                                    (selected_features[["Carbon count (chain 2)"]] == b &
                                                       selected_features[["Double bonds (chain 2)"]] == a)]
      if(length(idx_lipids) > 0) {
        res[a, b] = sum(hm_data[, idx_lipids], na.rm = TRUE)
      } else {
        res[a, b] = 0
      }

      # compensate for if a specific tails appears twice in a lipid, sum again
      if(length(idx_lipids_double) > 0) {
        res[a, b] = sum(res[a, b], hm_data[, idx_lipids_double], na.rm = TRUE)
      }
    }
  }

  # calculate the proportion
  res = res / sum(res)

  return(res)
}

fa_comp_hm_calc.total = function(data_table = NULL,
                                 feature_table = NULL,
                                 group_col = NULL,
                                 selected_group = NULL,
                                 sample_meta = NULL,
                                 selected_lipidclass = NULL) {
  ## samples
  idx_samples = rownames(sample_meta)[sample_meta[, group_col] == selected_group]
  hm_data = data_table[idx_samples, , drop = FALSE]

  ## features
  feature_table$lipid = rownames(feature_table)
  selected_features = feature_table[feature_table[["Lipid class"]] == selected_lipidclass, ]

  # get the unique chain lengths and unsaturation
  uniq_carbon = c(min(selected_features[["Carbon count (sum)"]]), max(selected_features[["Carbon count (sum)"]]))
  uniq_unsat = c(min(selected_features[["Double bonds (sum)"]]), max(selected_features[["Double bonds (sum)"]]))

  ## calculations
  # initialize result matrix
  res = matrix(ncol = length(uniq_carbon[1]:uniq_carbon[2]),
               nrow = length(uniq_unsat[1]:uniq_unsat[2]))
  colnames(res) = uniq_carbon[1]:uniq_carbon[2]
  rownames(res) = uniq_unsat[1]:uniq_unsat[2]
  for(a in rownames(res)) { # unsaturation
    for(b in colnames(res)) { # carbons
      idx_lipids = selected_features$lipid[selected_features[["Carbon count (sum)"]] == b &
                                             selected_features[["Double bonds (sum)"]] == a]
      if(length(idx_lipids) > 0) {
        res[a, b] = sum(hm_data[, idx_lipids], na.rm = TRUE)
      } else {
        res[a, b] = 0
      }
    }
  }

  # calculate the proportion
  res = res / sum(res)

  return(res)
}


fa_comp_heatmap = function(data = NULL,
                           hline = NULL,
                           vline = NULL,
                           composition = NULL,
                           color_limits = NULL,
                           color_palette = NULL,
                           reverse_palette = F,
                           y_pos_right = FALSE,
                           x_label_font_size = NULL,
                           x_tick_font_size = NULL,
                           x_tick_show = T,
                           y_label_font_size = NULL,
                           y_tick_font_size = NULL,
                           y_tick_show = T,
                           legend_label_font_size = NULL,
                           showlegend = FALSE) {
  # prepare data
  data_df = as.data.frame(data)
  data_df$row = rownames(data)

  data_df = data_df |>
    tidyr::pivot_longer(cols = -row,
                        names_to = "col",
                        values_to = "value")
  data_df$row = as.numeric(data_df$row)
  data_df$col = as.numeric(data_df$col)

  colors = get_color_palette(
    groups = 1:10,
    color_palette = color_palette,
    reverse_color_palette = reverse_palette,
    force_scale = F,
    force_list = F
  )

  colors = unname(colors)

  # make heatmap
  fig = plotly::plot_ly(data = data_df,
                        x = ~col,
                        y = ~row,
                        z = ~value,
                        type = "heatmap",
                        colors = colors,
                        hovertemplate = paste(
                          "Total carbons: %{x:d}<br>",
                          "Total double bond: %{y:d}<br>",
                          "Proportion: %{z:.3f}",
                          "<extra></extra>"
                        )) |>
    plotly::colorbar(limits = color_limits,
                     title = "Proportion",
                     tickfont = list(size = legend_label_font_size)) |>

    plotly::style(xgap = 3,
                  ygap = 3)

  if(!showlegend) {
    fig = fig |>
      plotly::hide_colorbar()
  }
  fig = fig |>
    # vertical line
    plotly::add_segments(
      x = vline,
      xend = vline,
      y = min(data_df$row) - 0.5,
      yend = max(data_df$row) + 0.5,
      inherit = FALSE,
      line = list(color = "black",
                  width = 2,
                  dash = "dot"),
      showlegend = FALSE
    ) |>
    # horizotal line
    plotly::add_segments(
      x = min(data_df$col) - 0.5,
      xend = max(data_df$col) + 0.5,
      y = hline,
      yend = hline,
      inherit = FALSE,
      line = list(color = "black",
                  width = 2,
                  dash = "dot"),
      showlegend = FALSE
    ) |>

    plotly::layout(
      font = list(
        size = 9
      ),
      xaxis = list(
        tick0 = 1,
        dtick = 1,
        zeroline = FALSE,
        showgrid = FALSE,
        fixedrange = TRUE,
        ticklen = 3,
        title = list(
          text = ifelse(composition == "fa_tail",
                        "Number of carbon atoms",
                        "Number of total carbon atoms"),
          standoff = 5,
          font = list(
            size = x_label_font_size
          )
        ),
        showticklabels = x_tick_show,
        tickfont = list(size = x_tick_font_size)
      )
    ) |>
    plotly::add_annotations(
      x = c(max(data_df$col), vline),
      y = c(hline, max(data_df$row)),
      text = c(sprintf("Avg. %0.1f", hline), sprintf("Avg. %0.1f", vline)),
      font = list(size = 10),
      xref = "x",
      yref = "y",
      showarrow = FALSE,
      xanchor = c("right", "left"),
      yanchor = c("bottom", "middle")
    )

  if(y_pos_right) {
    fig = fig |>
      plotly::layout(
        font = list(
          size = 9
        ),
        yaxis = list(
          tick0 = 1,
          dtick = 1,
          zeroline = FALSE,
          showgrid = FALSE,
          range = c(max(data_df$row) + 0.5, min(data_df$row) - 0.5),
          side = "right",
          fixedrange = TRUE,
          ticklen = 3,
          title = list(
            text = ifelse(composition == "fa_tail",
                          "Number of double bonds",
                          "Number of total double bonds"),
            standoff = 3,
            font = list(
              size = y_label_font_size
            )
          ),
          showticklabels = y_tick_show,
          tickfont = list(size = y_tick_font_size)
        )
      )
  } else {
    fig = fig |>
      plotly::layout(
        font = list(
          size = 9
        ),
        yaxis = list(
          tick0 = 1,
          dtick = 1,
          zeroline = FALSE,
          showgrid = FALSE,
          range = c(max(data_df$row) + 0.5, min(data_df$row) - 0.5),
          fixedrange = TRUE,
          ticklen = 3,
          title = list(
            text = ifelse(composition == "fa_tail",
                          "Number of double bonds",
                          "Number of total double bonds"),
            standoff = 3,
            font = list(
              size = y_label_font_size
            )
          ),
          showticklabels = y_tick_show,
          tickfont = list(size = y_tick_font_size)
        )
      )
  }

  return(fig)
}


format_label = function(
    label,
    font_size
  ) {
  if (is_none(label)) {
    label = NULL
    font_size = NULL
  } else if (is_none(font_size)){
    font_size = NULL
  } else if (font_size == 0) {
    label = NULL
    font_size = NULL
  }

  return(list(
    label = label,
    font_size = font_size
  ))
}

get_plot_font_data = function(
    title,
    title_font_size,
    x_label,
    x_label_font_size,
    x_tick_font_size,
    y_label,
    y_label_font_size,
    y_tick_font_size,
    legend_label,
    legend_font_size
  ) {

  title_data = format_label(label = title,
                            font_size = title_font_size)

  x_label_data = format_label(label = x_label,
                              font_size = x_label_font_size)

  y_label_data = format_label(label = y_label,
                              font_size = y_label_font_size)

  legend_label_data = format_label(label = legend_label,
                                   font_size = legend_font_size)

  if (is_none(x_tick_font_size)) {
    x_tick_font_size = NULL
    x_tick_show = T
  } else if (x_tick_font_size == 0) {
    x_tick_show = F
  } else {
    x_tick_show = T
  }

  if (is_none(y_tick_font_size)) {
    y_tick_font_size = NULL
    y_tick_show = T
  } else if (y_tick_font_size == 0) {
    y_tick_show = F
  } else {
    y_tick_show = T
  }



  if (!is.null(legend_label_data$font_size)) {
    if (legend_label_data$font_size == 0) {
      legend_show = F
    } else {
      legend_show = T
    }
  } else if (!is.null(legend_label_data$label)) {
    legend_show = T
  } else {
    legend_show = F
  }

  return(list(
    title = title_data$label,
    title_font_size = title_data$font_size,
    x_label = x_label_data$label,
    x_label_font_size = x_label_data$font_size,
    x_tick_font_size = x_tick_font_size,
    x_tick_show = x_tick_show,
    y_label = y_label_data$label,
    y_label_font_size = y_label_data$font_size,
    y_tick_font_size = y_tick_font_size,
    y_tick_show = y_tick_show,
    legend_label = legend_label_data$label,
    legend_font_size = legend_label_data$font_size,
    legend_show = legend_show
  ))
}





calculate_subplot_grid_dimensions = function(total_plots) {
  nearest_square_root = base::ceiling(sqrt(total_plots))
  rows = nearest_square_root
  cols = ceiling(total_plots / nearest_square_root)
  return(list(rows = rows, cols = cols))
}

create_blank_plot = function(label = NULL, width = NULL, height = NULL) {
  blank_plot = plotly::plot_ly(type = 'scatter', mode = 'markers', width = width, height = height)
  blank_plot = plotly::layout(blank_plot,
                              title = label,
                              xaxis = list(zeroline = F,
                                           showticklabels = F,
                                           showgrid = F),
                              yaxis = list(zeroline = F,
                                           showticklabels = F,
                                           showgrid = F))
  return(blank_plot)
}

pca_plot_scores = function(x, y, meta_table, group_col, width, height, colour_list){
  groups = unique(meta_table[,group_col])
  fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
  i = 1
  for (grp in groups) {
    idx = rownames(meta_table)[which(meta_table[,group_col] == grp)]
    fig = fig %>% add_trace(x = x[idx], y = y[idx],
                            name = grp, color = colour_list[i],
                            type  = "scatter", mode = "markers",
                            text = idx,
                            hoverinfo = "text",
                            legendgroup=grp)
    i = i + 1
  }
  fig = fig %>% layout(shapes = list(hline(0),
                                     vline(0),
                                     circle(x, y)),
                       legend = list(title=list(text = paste0('<b>', group_col, ': </b>'))))
  return(fig)
}

pca_plot_loadings = function(x, y, feature_list, width, height, colour_list){
  fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
  fig = fig %>% add_trace(x = x, y = y,
                          type = "scatter", mode = "text", text = feature_list,
                          textposition = 'middle right', showlegend = F)

  shape_list = list(
    hline(0),
    vline(0)
  )


  for (i in 1:length(feature_list)) {
    feature = feature_list[i]
    new_line = list(
      type = "line",
      line = list(color = "pink"),
      xref = "x",
      yref = "y",
      x0 = 0,
      y0 = 0,
      x1 = x[i],
      y1 = y[i]
    )
    shape_list[[length(shape_list) + 1]] = new_line
  }

  fig = fig %>% layout(shapes = shape_list)

  return(fig)
}

hline = function(y = 0, color = "black", dash = NULL) {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash=dash)
  )
}

vline <- function(x = 0, color = "black", dash = NULL) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash=dash)
  )
}

#' @title Calculate Hoteling T2
#'
#' @description Calculate Hoteling T2 for the scores plot
#'
#' @param x numeric vector with x values
#' @param y numeric vector with y values
#' @param alpha numeric(1), confidence interval
#' @param len numeric(1), number of points to create the ellipse
#'
#' @return A list is returned to be used in a plotly graph.
#'
#' @details This is a helper function which is used to create a confidence (Hotelling T2) interval for a
#'     PCA score plot.
#'
#' @importFrom stats var qf
#'
#' @noRd
#'
#' @author Damien Olivier
circle = function(x, y, alpha = 0.95, len = 200){
  N = length(x)
  mypi = seq(0, 2 * pi, length = len)
  r1 = sqrt(stats::var(x) * stats::qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  r2 = sqrt(stats::var(y) * stats::qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  list(
    type = "circle",
    xref = "x",
    x0= -r1,
    x1 = r1,
    yref = "y",
    y0 = -r2,
    y1 = r2,
    line = "black",
    opacity = 0.2
  )
}

lipidomics_summary_plot = function(r6, data_table) {
  groups = get_lipid_classes(colnames(r6$tables$imp_data)[2:length(colnames(r6$tables$imp_data))], uniques = T)

  plot_table = data.frame(table(base::factor((get_lipid_classes(colnames(data_table), uniques = F)), levels = groups)))
  names(plot_table) = c("class", "raw")
  plot_table$imported = table(base::factor((get_lipid_classes(colnames(r6$tables$imp_data)[2:length(colnames(r6$tables$imp_data))], uniques = F)), levels = groups))
  plot_table$removed = plot_table$imported - plot_table$raw

  absolute_counts = as.data.frame(base::matrix(nrow = 2*nrow(plot_table)))
  absolute_counts$classes = c(plot_table$class, plot_table$class)
  absolute_counts$values = c(plot_table$removed, plot_table$raw)
  absolute_counts$status = c(rep('kept', nrow(plot_table)), rep('removed', nrow(plot_table)))
  absolute_counts$V1 = NULL

  relative_counts = absolute_counts
  relative_counts$values = round(100*(relative_counts$values/c(plot_table$imported, plot_table$imported)))

  plot_1 = ggplot(absolute_counts ,
                  aes(
                    fill=status ,
                    y=values ,
                    x=classes ,
                    label = values )) +
    ggtitle("Absolute compound count")+
    geom_bar(position="stack",
             stat="identity",
             show.legend = FALSE) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent')
    ) +
    scale_fill_manual(values = c("#D2E8F5", "#007bff"))+
    coord_flip() +
    scale_y_reverse(limits = c(max(plot_table$imported), 0))


  plot_2 = ggplot(relative_counts,
                  aes(
                    fill=status ,
                    y=values ,
                    x=classes ,
                    label = values )) +
    ggtitle("Relative compound count (%)")+
    geom_bar(position="stack",
             stat="identity",
             show.legend = FALSE) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_text(hjust = 0.4),
      axis.ticks.y=element_blank(),
      axis.ticks.x=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent')
    ) +
    scale_fill_manual(values = c("#D2E8F5", "#007bff"))+
    coord_flip()

  return(gridExtra::grid.arrange(plot_1, plot_2, ncol=2))
}



#--------------------------------------------------------------- Statistics ----

get_pca_data = function(data_table){

  pca_data = pcaMethods::pca(object = data_table,
                             nPcs = 2,
                             scale = "none",
                             cv = "q2",
                             completeObs = T)

  return(pca_data)
}

apply_discriminant_analysis = function(data_table, group_list, nlambda = 100, alpha = 0.8, seed = 1) {

  ncol_1 = ncol(data_table)
  data_table = data_table[,!is.na(colSums(data_table))]
  ncol_2 = ncol(data_table)
  delta_1 = ncol_1 - ncol_2
  delta_1_percent = round(100*(delta_1 / ncol_1),1)

  if (delta_1_percent > 0){
    if(delta_1_percent < 50) {
      base::warning(paste0("Discriminant analysis : dropped ", delta_1, " (", delta_1_percent, "%)", " features containing NA values."))
    } else {
      base::stop(paste0("Discriminant analysis: ", delta_1_percent, "% of features contained NA values and were removed. Consider imputation."))
    }
  }

  count = table(group_list)
  if (any(count < 3)) {
    dead_groups = names(count)[count < 3]
    base::warning(paste0(length(dead_groups), " groups with fewer than 3 samples, dropped from analysis (", paste0(dead_groups, collapse = ', '), ")."))
    data_table = data_table[!(group_list %in% dead_groups),]
    group_list = group_list[!(group_list %in% dead_groups)]
  }

  if (length(unique(group_list)) > 2) {
    family = "multinomial"
  } else {
    family = "binomial"
  }

  coef = NULL
  attempt_count = 1
  while(is.null(coef)) {
    base::warning(paste0("Discriminant analysis: attempt ", attempt_count))
    if (attempt_count == 5) {
      base::stop("Discriminant analysis failed after 5 attempts")
      }
    attempt_count = attempt_count + 1
    base::tryCatch(
      {
        base::set.seed(seed)
        coef = glmnet::cv.glmnet(data_table,
                                 group_list,
                                 nlambda = nlambda,
                                 alpha = alpha,
                                 type.multinomial = "grouped",
                                 family = family)

      },error=function(e){
      },finally={}
    )
  }

  coef = stats::coef(coef, s = "lambda.min")
  if (family == 'multinomial'){
    keep_cols = as.matrix(coef[[1]])
  }else {
    keep_cols = as.matrix(coef)
  }

  keep_cols = rownames(keep_cols)[which(keep_cols != 0)]
  if (length(keep_cols) == 1) {
    stop("Discriminant analysis: no discriminant features could be found.")
  }
  keep_cols = keep_cols[2:length(keep_cols)]

  delta_2 = ncol(data_table) - length(keep_cols)
  delta_2_percent = round(100*(delta_2 / ncol(data_table)),1)

  base::warning(paste0(length(keep_cols), " features selected, dropped ", delta_2, " (", delta_2_percent, "%) with no signal variation."))

  data_table = data_table[,keep_cols]
  return(data_table)
}


get_fold_changes = function(data_table, idx_group_1, idx_group_2, used_function, impute_inf = F) {

  if (used_function == "median") {
    av_function = function(x) {return(stats::median(x, na.rm = T))}
  } else {
    av_function = function(x) {return(base::mean(x, na.rm = T))}
  }


  fold_changes = apply(data_table, 2, function(column) {
    mean_group1 = av_function(column[idx_group_1])
    mean_group2 = av_function(column[idx_group_2])

    # Impute NA means with 0
    if (is.na(mean_group1)) mean_group1 = 0
    if (is.na(mean_group2)) mean_group2 = 0

    fold_change = mean_group2 / mean_group1

    return(fold_change)
  })

  if (impute_inf) {
    # Impute infinite (x/0)
    if (length(which(fold_changes == Inf)) > 0) {
      fold_changes[which(fold_changes == Inf)] = max(fold_changes[which(fold_changes != Inf)]) * 1.01
    }

    # Impute zeros (0/x)
    if (length(which(fold_changes == 0)) > 0) {
      fold_changes[which(fold_changes == 0)] = min(fold_changes[which(fold_changes > 0)]) * 0.99
    }
  }

  # Impute NaNs (0/0)
  if (length(which(is.nan(fold_changes)) > 0)) {
    fold_changes[which(is.nan(fold_changes))] = 1
  }

  return(fold_changes)

}

get_p_val = function(data_table, idx_group_1, idx_group_2, used_function, impute_na = F) {

  if (used_function == "Wilcoxon") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T), na.rm = T)&all(y==mean(y, na.rm = T), na.rm = T)) {
        return(1)
      } else{
        return(stats::wilcox.test(x, y)$p.value)
      }
    }
  } else if (used_function == "t-Test") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T), na.rm = T)&all(y==mean(y, na.rm = T), na.rm = T)) {
        return(1)
      } else{
        return(stats::t.test(x, y)$p.value)
      }
    }

  }




  # pval_list = c()
  # for (column in colnames(data_table)) {
  #   group1 = data_table[idx_group_1, column]
  #   group2 = data_table[idx_group_2, column]
  #
  #   # Check if there are enough non-NA values to conduct a t-test
  #   if (sum(!is.na(group1)) < 2 || sum(!is.na(group2)) < 2) {
  #     pval_list = c(pval_list, NA)
  #   } else {
  #     test_result = test_function(group1, group2)  # Assuming equal variance
  #     pval_list = c(pval_list, test_result)
  #   }
  # }
  #


  p_values = apply(data_table, 2, function(column) {
    group1 = column[idx_group_1]
    group2 = column[idx_group_2]

    # Check if there are enough non-NA values to conduct a t-test
    if (sum(!is.na(group1)) < 2 || sum(!is.na(group2)) < 2) {
      return(NA)  # Return NA if not enough data
    }

    test_result = test_function(group1, group2)  # Assuming equal variance
    return(test_result)
  })


  if ((length(which(is.na(p_values))) > 0) & impute_na){
    p_values[which(is.na(p_values))] = min(p_values, na.rm = T) * 0.99
  }


  return(p_values)
}


get_comparison_table = function(data_table,
                                sample_table,
                                feature_table,
                                group_col,
                                group_1,
                                group_2,
                                fc_function = "mean",
                                statistical_test = "t-Test",
                                adjustment_method = "BH") {
  # Get rownames to be kept
  rownames_group_1 = rownames(sample_table)[sample_table[, group_col] == group_1]
  rownames_group_2 = rownames(sample_table)[sample_table[, group_col] == group_2]
  all_rownames = sort(unique(c(rownames_group_1, rownames_group_2)))

  # Filter data to keep only the two groups
  data_table = data_table[all_rownames,]

  # Get the indices for each group
  idx_group_1 = which(rownames(data_table) %in% rownames_group_1)
  idx_group_2 = which(rownames(data_table) %in% rownames_group_2)


  # Remove empty columns
  data_table = remove_empty_cols(table = data_table)

  comparison_table = data.frame(row.names = colnames(data_table))

  # Collect fold changes
  comparison_table[, 'Fold change'] = get_fold_changes(data_table = data_table,
                                                       idx_group_1 = idx_group_1,
                                                       idx_group_2 = idx_group_2,
                                                       used_function = fc_function)

  # Get log2 fold change
  comparison_table[,'Log2(fold change)'] = log2(comparison_table[, 'Fold change'])

  # Collect p-values
  comparison_table[, 'p-value'] = get_p_val(data_table = data_table,
                                            idx_group_1 = idx_group_1,
                                            idx_group_2 = idx_group_2,
                                            used_function = statistical_test)

  # Get log10 pval
  comparison_table[,'-Log10(p-value)'] = -log10(comparison_table[, 'p-value'])

  # Add adjusted p-value
  comparison_table[, 'p-adjusted'] = stats::p.adjust(comparison_table[, 'p-value'], method = adjustment_method)
  comparison_table[, '-Log10(p-adjusted)'] = -log10(comparison_table[, 'p-adjusted'])

  # Add feature table cols
  if (ncol(feature_table) > 0) {
    comparison_table[,colnames(feature_table)] = feature_table[rownames(comparison_table),]
  }

  return(comparison_table)
}



faster_cor=function(data_table, method="pearson") {
  cor1=NULL
  if(method=="pearson")
    cor1=Rfast::cora(data_table,large = T)
  if(method=="spearman")
    cor1=Rfast::cora(apply(data_table,MARGIN = 2,function(x) base::rank(x)),large = T)
  cor1
}

#--------------------------------------------------------- Example datasets ----
initialize_omics = function(name,
                            type,
                            version = NA,
                            meta_file,
                            data_file,
                            feat_file = NULL,
                            meta_file_format = "Wide",
                            data_file_format = "Wide",
                            feat_file_format = "Long",
                            id_col_meta = NULL,
                            id_col_data = NULL,
                            id_col_feat = NULL,
                            param_file = NULL,
                            type_column = NULL,
                            group_column = NULL,
                            batch_column = NULL,
                            blank_pattern = "blank",
                            qc_pattern = "quality",
                            pool_pattern = "pool",
                            excluded_samples = NULL,
                            drop_blanks = F,
                            drop_qcs = F,
                            drop_pools = F,
                            blank_multiplier = 2,
                            sample_threshold = 0.8,
                            group_threshold = 0.8,
                            excluded_features = NULL,
                            imputation_method = "None",
                            batch_effect_correction = "None",
                            operation_order = c("Imputation", "Batch correction", "Filtering"),
                            norm_col = "None",
                            verbose = F) {

  ####---- Initialize ----
  self = Omics_exp$new(name = name,
                       type = type,
                       id = NULL,
                       slot = NULL,
                       version = version,
                       param_file = param_file)
  if (verbose) {print("Initialized omics")}

  ####---- Imported tables ----
  self$import_meta(path = meta_file, input_format = meta_file_format)
  self$import_data(path = data_file, input_format = data_file_format)
  self$import_feat(path = feat_file, input_format = feat_file_format)
  if (verbose) {print("Imported tables")}
  
  ####---- Indexed tables ----
  if (!is.null(id_col_meta)) {self$set_indexed_meta(id_col = id_col_meta)}
  if (!is.null(id_col_data)) {self$set_indexed_data(id_col = id_col_data)}
  self$set_indexed_feat(id_col = id_col_feat)
  if (base::all(c(!is.null(id_col_meta), !is.null(id_col_data)))) {
    self$check_indexed_table_consistency()
  }
  if (verbose) {print("Indexed tables")}
  
  ####---- Raw tables ----
  if (base::all(
    !is.null(self$tables$indexed_meta),
    !is.null(self$tables$indexed_data),
    !is.null(self$tables$indexed_feat)
  )) {
    
    # Set sample column types
    if (base::all(c(
      !is.null(type_column),
      !is.null(group_column),
      !is.null(batch_column)))) {
      self$set_type_column(type_column = type_column)
      self$set_group_column(group_column = group_column)
      self$set_batch_column(batch_column = batch_column)
      
      self$set_blank_indices(blank_pattern = blank_pattern)
      self$set_qc_indices(qc_pattern = qc_pattern)
      self$set_pool_indices(pool_pattern = pool_pattern)
      self$set_sample_indices()
    }
    
    # Sample exclusion
    self$exclude_samples(
      selection = excluded_samples,
      drop = T
    )
    
    # Non-sample exclusion
    self$non_sample_exclusion(
      select_blanks = drop_blanks,
      select_qcs = drop_qcs,
      select_pools = drop_pools,
      exclude = TRUE)
    
    # Feature exclusion
    self$exclude_features(
      selection = excluded_features,
      drop = T
    )
    
    # Apply measurement filtering
    if (base::all(c(
      !is.null(type_column),
      !is.null(group_column),
      !is.null(blank_multiplier),
      !is.null(sample_threshold),
      !is.null(group_threshold)))) {
      
      self$param_measurement_filter(operation_order = operation_order,
                                    batch_effect_correction = batch_effect_correction,
                                    imputation_method = imputation_method,
                                    blank_multiplier = blank_multiplier,
                                    sample_threshold = sample_threshold,
                                    group_threshold = group_threshold,
                                    norm_col = norm_col)
    }
    
    # Set all raw tables
    self$set_raw_meta()
    self$set_raw_data()
    self$set_raw_feat()
    if (verbose) {print("Set raw tables")}
    
    # Derive data tables
    if (!is.null(group_column)) {
      self$derive_data_tables()
      if (verbose) {print("Derived data tables")}
    }

  }
  ####---- Return ----
  return(self)
}
  
    


#---------------------------------------------- Enrichment & GSEA utilities ----
get_ea_object = function(ea_feature_table,
                         terms_table = NULL,
                         selected_features = NULL,
                         feature_table,
                         keyType = "SYMBOL",
                         ont,
                         minGSSize,
                         maxGSSize,
                         p_value_cutoff,
                         verbose,
                         OrgDb,
                         pAdjustMethod,
                         seed = 1) {

  base::set.seed(seed)

  # Get the feature list
  feature_list = ea_feature_table$`Log2(fold change)`
  names(feature_list) = rownames(ea_feature_table)

  # NA omit and sort
  feature_list = na.omit(feature_list)
  feature_list = sort(feature_list, decreasing = TRUE)

  # Filter by selected features if relevant
  if (!is.null(selected_features)) {
    feature_list = feature_list[which(names(feature_list) %in% selected_features)]
    feature_table = feature_table[selected_features, ]
  }

  if (!is.null(terms_table)) {
    ea_object = custom_gsea(geneList = feature_list,
                            minGSSize = minGSSize,
                            maxGSSize = maxGSSize,
                            pvalueCutoff = p_value_cutoff,
                            verbose = verbose,
                            pAdjustMethod = pAdjustMethod,
                            term2gene = terms_table)
  } else {
    ea_object = clusterProfiler::gseGO(geneList=feature_list,
                                       ont = ont,
                                       keyType = keyType,
                                       minGSSize = minGSSize,
                                       maxGSSize = maxGSSize,
                                       eps = 0,
                                       pvalueCutoff = p_value_cutoff,
                                       verbose = verbose,
                                       OrgDb = OrgDb,
                                       pAdjustMethod = pAdjustMethod)
  }
  return(ea_object)
}


get_ora_object = function(ora_feature_table = self$tables$ora_feature_table,
                          terms_table = NULL,
                          selected_features = NULL,
                          feature_table = self$tables$feature_table,
                          pval_cutoff_features = self$params$overrepresentation$pval_cutoff_features,
                          padjust_features = self$params$overrepresentation$padjust_features,
                          pval_cutoff = self$params$overrepresentation$pval_cutoff,
                          pAdjustMethod = self$params$overrepresentation$pAdjustMethod,
                          fc_threshold = self$params$overrepresentation$fc_threshold,
                          keyType = self$indices$feature_id_type,
                          ont = self$params$overrepresentation$ont,
                          qval_cutoff = self$params$overrepresentation$qval_cutoff,
                          minGSSize = self$params$overrepresentation$minGSSize,
                          maxGSSize  = self$params$overrepresentation$maxGSSize,
                          seed = 1) {

  base::set.seed(seed)

  # Sort by log2(fc)
  ora_feature_table = ora_feature_table[order(-ora_feature_table$`Log2(fold change)`),]

  # Get universe (all features)
  universe = ora_feature_table$`Log2(fold change)`
  names(universe) = rownames(ora_feature_table)
  universe = na.omit(universe)
  universe = names(universe)

  # ora_feature_table = ora_feature_table[ora_feature_table$selected, ]

  if (!is.null(selected_features)) {
    ora_feature_table = ora_feature_table[which(rownames(ora_feature_table) %in% selected_features), ]
  } else {
    ora_feature_table = ora_feature_table[abs(ora_feature_table$`Log2(fold change)`) >= log2(fc_threshold),]
    if (padjust_features == "none") {
      ora_feature_table = ora_feature_table[ora_feature_table$`p-value` <= pval_cutoff_features,]
    } else {
      ora_feature_table = ora_feature_table[ora_feature_table$`p-adjusted` <= pval_cutoff_features,]
    }
  }

  if (nrow(ora_feature_table) == 0) {
    stop("Could not select any features under selected parameters.")
  } else {
    feature_list = rownames(ora_feature_table)
  }


  if (!is.null(terms_table)) {
    ora_object = custom_ora(geneList = feature_list,
                            pvalueCutoff = pval_cutoff,
                            pAdjustMethod = pAdjustMethod,
                            qvalueCutoff = qval_cutoff,
                            minGSSize = minGSSize,
                            maxGSSize = maxGSSize,
                            term2gene = terms_table)
  } else {
    ora_object = clusterProfiler::enrichGO(gene = feature_list,
                                           universe = universe,
                                           OrgDb = 'org.Hs.eg.db',
                                           keyType = keyType,
                                           readable = F,
                                           ont = ont,
                                           pvalueCutoff = pval_cutoff,
                                           pAdjustMethod = pAdjustMethod,
                                           qvalueCutoff = qval_cutoff,
                                           minGSSize = minGSSize,
                                           maxGSSize  = maxGSSize)
  }

  return(ora_object)
}




get_sparse_matrix = function(column_values, column_terms, terms_list) {
  # Initialize a list to store the one-hot encoded vectors
  one_hot_list = vector("list", length(column_values))

  # Loop through each gene and create a one-hot encoded vector
  for (i in seq_along(names(column_values))) {
    # Create a boolean vector for the presence of each GO term
    one_hot_vector = terms_list %in% column_terms[[i]]
    # Add the vector to the list
    one_hot_list[[i]] = one_hot_vector
  }

  # Combine the one-hot encoded vectors into a matrix
  sparse_matrix = do.call(rbind, one_hot_list)

  # Convert the matrix to a sparse matrix
  sparse_matrix = Matrix::Matrix(sparse_matrix, sparse = TRUE)

  # Add row and column names to the sparse matrix
  rownames(sparse_matrix) = names(column_values)
  colnames(sparse_matrix) = terms_list
  return(sparse_matrix)
}

match_go_terms = function(terms_list, sparse_table) {
  if (length(terms_list) > 1) {
    matches = rowSums(sparse_table[,terms_list])
  } else {
    matches = sparse_table[,terms_list]
  }
  return(matches)
}

get_terms_table = function(column_values, sep = "|") {
  terms_table=sapply(as.character(column_values), FUN = function(x) base::strsplit(x, split = sep, fixed = TRUE)[[1]])
  names(terms_table)=names(column_values)
  terms_table = utils::stack(terms_table)
  return(terms_table)
}

custom_ora = function(geneList, pvalueCutoff = 0.05, pAdjustMethod = "BH", qvalueCutoff = 0.2, minGSSize = 10, maxGSSize = 500, term2gene) {
  enricher_result = clusterProfiler::enricher(gene = geneList,
                                              pvalueCutoff = pvalueCutoff,
                                              pAdjustMethod = pAdjustMethod,
                                              qvalueCutoff = qvalueCutoff,
                                              minGSSize = minGSSize,
                                              maxGSSize  = maxGSSize,
                                              TERM2GENE=term2gene)
  return(enricher_result)
}
custom_gsea = function(geneList, minGSSize = 10, maxGSSize = 500, pvalueCutoff = 0.05, verbose = TRUE, pAdjustMethod = "BH", term2gene) {
  
  if (any(is.infinite(geneList))) {
    base::warning(paste0("Removing ", sum(is.infinite(geneList)), " features with infinite fold changes"))
    geneList = geneList[!is.infinite(geneList)]
  }
  
  gsea_result = clusterProfiler::GSEA(geneList = geneList,
                                      minGSSize = minGSSize,
                                      maxGSSize = maxGSSize,
                                      pvalueCutoff = pvalueCutoff,
                                      verbose = verbose,
                                      pAdjustMethod = pAdjustMethod,
                                      eps = 0,
                                      TERM2GENE=term2gene)
  return(gsea_result)
}

get_cp_results = function(object, showCategory) {
  showCategory = min(nrow(object@result), showCategory)
  df = object@result[1:showCategory,]
  if (!is.null(df$GeneRatio)) {
    df$GeneRatio = sapply(strsplit(as.character(df$GeneRatio), "/"), function(x) as.numeric(x[1]) / as.numeric(x[2]))
  }
  if (!is.null(df$BgRatio)) {
    df$BgRatio = sapply(strsplit(as.character(df$BgRatio), "/"), function(x) as.numeric(x[1]) / as.numeric(x[2]))
  }
  return(df)
}

#-------------------------------------------------------------- FA Dot plot ----
plot_fa_dot_plot = function(object,
                            x = "GeneRatio",
                            y = 'ID',
                            color = "p.adjust",
                            show_categories = 10,
                            size = "Count",
                            order_by = "GeneRatio",
                            reverse_order = F,
                            mode = "Both",
                            marker_opacity = 0.5,
                            color_palette = "Spectral",
                            reverse_palette = FALSE,
                            show_legend = T,
                            legend_size = 14,
                            size_ref = 2,
                            yaxis_word_split = 2,
                            title_size = 25,
                            xlabel_size = 15,
                            xtick_size = 15,
                            ytick_size = 15,
                            width = NULL,
                            height = NULL){

  xtick_show = base::ifelse(xtick_size > 0, T, F)
  ytick_show = base::ifelse(ytick_size > 0, T, F)


  # Checks
  if (!is.numeric(show_categories)) {
    base::stop("Invalid number of categories to display")
  }

  if (!(color %in% c("pvalue", "p.adjust"))) {
    base::stop("Invalid color, choose one of pvalue or p.adjust")
  }

  if (!(mode %in% c("Both", "Activated", "Suppressed"))) {
    base::stop("Invalid mode, choose one of Both, Activated or Suppressed")
  }

  data_table = as.data.frame(object) # Already sorted by p.value

  if (show_categories > nrow(data_table)) {
    show_categories = nrow(data_table)
    base::warning(paste0("Too many categories selected, setting to max: ", show_categories))
  }

  if (class(object)[1] == "enrichResult") {
    data_table = data_table[1:show_categories,]
    feature_counts = NULL
    set_size = NULL
    for (i in stringr::str_split(data_table$GeneRatio, "/")){
      feature_counts = c(feature_counts, as.numeric(i[1]))
      set_size = c(set_size, as.numeric(i[2]))
    }
    data_table$GeneCount = feature_counts
    data_table$setSize = set_size
    data_table$GeneRatio = data_table$GeneCount / data_table$setSize
    data_table$Sign = NA
    mode = "None"

  } else {
    top_activated = rownames(data_table)[data_table$enrichmentScore > 0][1:show_categories]
    top_suppressed = rownames(data_table)[data_table$enrichmentScore < 0][1:show_categories]
    if (mode == "Both") {
      data_table = data_table[c(top_activated, top_suppressed),]
    } else if (mode == "Activated") {
      data_table = data_table[top_activated,]
    } else if (mode == "Suppressed") {
      data_table = data_table[top_suppressed,]
    }
    # Add GeneCount and GeneRatio
    data_table$GeneCount = stringr::str_count(data_table[,"core_enrichment"], '/') + 1
    data_table$GeneRatio = data_table$GeneCount / data_table$setSize
    data_table$Sign = "Suppressed"
    data_table$Sign[data_table$enrichmentScore > 0] = "Activated"
  }

  # Order the table
  if (reverse_order) {
    data_table = data_table[base::rev(base::order(data_table[,order_by])), ]
  } else {
    data_table = data_table[base::order(data_table[,order_by]), ]
  }

  # Add hover
  data_table$hover = paste0(
    data_table[,"Description"],
    "\nGeneRatio: ",
    round(data_table[,"GeneRatio"],2),
    "\nSet size: ",
    data_table[,"setSize"],
    "\nCount: ",
    data_table[,"GeneCount"],
    "\np-value: ",
    base::format(data_table[,"pvalue"], scientific = TRUE, digits = 3),
    "\np-adjust: ",
    base::format(data_table[,"p.adjust"], scientific = TRUE, digits = 3),
    "\nSign: ",
    data_table[,"Sign"]
  )

  # Format y axis ticks & word split
  yaxis_ticks = axis_word_split(ticks = data_table[,y], n = yaxis_word_split)
  data_table[,y] = base::factor(yaxis_ticks, levels = yaxis_ticks)

  # Get the color palette values
  color_scale = get_color_palette(groups = data_table[,color],
                                  color_palette = color_palette,
                                  reverse_color_palette = reverse_palette,
                                  force_scale = T)



  if (mode == "Both") {
    activated_data = data_table[data_table$Sign == "Activated", ]
    suppressed_data = data_table[data_table$Sign == "Suppressed", ]

    plot_activated = plotly::plot_ly(width = width, height = height)
    plot_activated = plotly::add_trace(
      p = plot_activated,
      x = activated_data[,x],
      y = activated_data[,y],
      type = "scatter",
      mode = "markers",
      marker = list(color = activated_data[,color],
                    colorscale = color_scale,
                    colorbar = list(title = color,
                                    tickformat = ".1e",
                                    tickfont = list(size = legend_size)),
                    showscale = F,
                    sizemode ='diameter',
                    opacity = marker_opacity,
                    sizeref=size_ref,
                    size = activated_data[,size],
                    line = list(width = 0),
                    cmax = max(data_table[, color]),
                    cmin = min(data_table[, color])
      ),
      showlegend = F,
      text = activated_data$hover,
      hoverinfo = "text"
    )

    plot_activated = plotly::layout(
      p = plot_activated,
      plot_bgcolor='rgba(0,0,0,0)',
      paper_bgcolor='rgba(0,0,0,0)',
      xaxis = list(
        title = list(
          text = x,
          font = list(size = xlabel_size)
        ),
        showticklabels = xtick_show,
        showline = TRUE,
        zeroline = FALSE,
        mirror = F,
        tickfont = list(size = xtick_size)
      ),
      yaxis = list(
        domain = c(0, 0.95),
        showticklabels = ytick_show,
        showline = TRUE,
        zeroline = FALSE,
        mirror = TRUE,
        tickfont = list(size = ytick_size)
      ),
      annotations = list(
        list(
          text = base::ifelse(title_size == 0, "", "Activated"),
          x = 0.5,
          y = 0.98,
          showarrow = F,
          xref = "paper",
          yref = "paper",
          font = list(
            family = "Arial, sans-serif",
            size = title_size,
            color = "black"
          ),
          bgcolor = "transparent",
          bordercolor = "transparent"
        )
      )
    )


    plot_suppressed = plotly::plot_ly(width = width, height = height)
    plot_suppressed = plotly::add_trace(
      p = plot_suppressed,
      x = suppressed_data[,x],
      y = suppressed_data[,y],
      type = "scatter",
      mode = "markers",
      marker = list(color = suppressed_data[,color],
                    colorscale = color_scale,
                    colorbar = list(title = color,
                                    tickformat = ".1e",
                                    tickfont = list(size = legend_size)),
                    showscale = T,
                    sizemode ='diameter',
                    opacity = marker_opacity,
                    sizeref=size_ref,
                    size = suppressed_data[,size],
                    line = list(width = 0),
                    cmax = max(data_table[, color]),
                    cmin = min(data_table[, color])
      ),
      showlegend = F,
      text = suppressed_data$hover,
      hoverinfo = "text"
    )

    plot_suppressed = plotly::layout(
      p = plot_suppressed,
      plot_bgcolor='rgba(0,0,0,0)',
      paper_bgcolor='rgba(0,0,0,0)',
      xaxis = list(
        title = list(
          text = x,
          font = list(size = xlabel_size)
        ),
        showticklabels = xtick_show,
        showline = TRUE,
        zeroline = FALSE,
        mirror = F,
        tickfont = list(size = xtick_size)
      ),
      yaxis = list(
        domain = c(0, 0.95),
        showticklabels = ytick_show,
        showline = TRUE,
        zeroline = FALSE,
        mirror = TRUE,
        tickfont = list(size = ytick_size)
      ),
      annotations = list(
        list(
          text = base::ifelse(title_size == 0, "", "Suppressed"),
          x = 0.5,
          y = 0.98,
          showarrow = F,
          xref = "paper",
          yref = "paper",
          font = list(
            family = "Arial, sans-serif",
            size = title_size,
            color = "black"
          ),
          bgcolor = "transparent",
          bordercolor = "transparent"
        )
      )
    )

    plot = plotly::subplot(
      list(
        activated = plot_activated,
        suppressed = plot_suppressed),
      nrows = 2,
      shareX = T,
      shareY = F
    )


  } else {
    if (mode == "None") {mode = ""}
    plot = plotly::plot_ly(width = width, height = height)
    plot = plotly::add_trace(
      p = plot,
      x = data_table[,x],
      y = data_table[,y],
      type = "scatter",
      mode = "markers",
      marker = list(color = data_table[,color],
                    colorscale = color_scale,
                    colorbar = list(title = color,
                                    tickformat = ".1e",
                                    tickfont = list(size = legend_size)),
                    showscale = show_legend,
                    sizemode ='diameter',
                    opacity = marker_opacity,
                    sizeref=size_ref,
                    size = data_table[,size],
                    line = list(width = 0),
                    cmax = max(data_table[, color]),
                    cmin = min(data_table[, color])
      ),
      text = data_table$hover,
      hoverinfo = "text"
    )

    plot = plotly::layout(
      p = plot,
      title = list(text = base::ifelse(title_size == 0, "", mode),
                   font = list(size = title_size)),

      xaxis = list(
        title = list(
          text = base::ifelse(xlabel_size == 0, "", x),
          font = list(size = xlabel_size)
        ),
        showticklabels = xtick_show,
        showline = TRUE,
        zeroline = FALSE,
        mirror = TRUE,
        tickfont = list(size = xtick_size)
      ),

      yaxis = list(
        showticklabels = ytick_show,
        showline = TRUE,
        zeroline = FALSE,
        mirror = TRUE,
        tickfont = list(size = ytick_size)
      ),
      plot_bgcolor='rgba(0,0,0,0)',
      paper_bgcolor='rgba(0,0,0,0)'
    )
  }

  return(list(plot = plot,
              table = data_table))

}





#------------------------------------------------------------ FA Ridge plot ----
plot_fa_ridge_plot = function(object,
                              show_category = 50,
                              fill="p.adjust",
                              core_enrichment = TRUE,
                              color_palette = "Spectral",
                              reverse_palette = FALSE,
                              displayed_label = "None",
                              orderBy = "NES",
                              decreasing = FALSE,
                              title_font_size = 10,
                              yaxis_word_split = 0,
                              y_label_font_size = 10,
                              y_tick_font_size = 10,
                              x_label_font_size = 10,
                              x_tick_font_size = 10,
                              legend_font_size = 10,
                              width = NULL,
                              height = NULL) {

  # Process fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  legend_show = base::ifelse(legend_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, "Log2(fold change)", "")
  y_axis_title = base::ifelse(y_label_font_size > 0, "Feature sets", "")
  title = base::ifelse(title_font_size > 0, "Ridge plot", "")

  # Get data & check
  data_table = as.data.frame(object)

  # Checks
  if (show_category > nrow(data_table)) {
    show_category = nrow(data_table)
    base::warning(paste0("Too many categories selected, setting to max: ", show_category))
  }
  data_table = data_table[1:show_category, ]

  # Select the desired features
  non_core_enriched = NULL
  for (gs in rownames(data_table)) {
    non_core_enriched = c(non_core_enriched, paste0(object@geneSets[gs][[1]], collapse = "/"))
  }
  data_table$non_core_enriched = non_core_enriched
  data_table$core_enrichment_count = stringr::str_count(data_table$core_enrichment, '/') + 1
  data_table$non_core_enriched_count = stringr::str_count(data_table$non_core_enriched, '/') + 1

  if (core_enrichment) {
    queried_data = "core_enrichment"
  } else {
    queried_data = "non_core_enriched"
  }


  # Get feature values
  gs2val = list()
  for (gs in rownames(data_table)) {
    feature_list = stringr::str_split(data_table[gs, queried_data], "/(?=[a-zA-Z])")[[1]]
    out_list = NULL
    for (feature in feature_list) {
      val = object@geneList[feature]
      if (!is.na(val)) {
        out_list = c(out_list, val)
      }
    }
    gs2val[[gs]] = out_list
  }
  data_table$gs2val = gs2val
  max_x = max(unlist(gs2val))

  # Order the data
  data_table = data_table[base::order(data_table[,orderBy], decreasing = decreasing),]

  # Calculate bandwidth
  min_value = min(unname(unlist(gs2val)), na.rm = T)
  max_value = max(unname(unlist(gs2val)), na.rm = T)
  dropped_gs = rownames(data_table)[data_table[paste0(queried_data, '_count')] < 1]
  if (length(dropped_gs) > 0) {
    base::warning(paste0("Dropping feature sets with only a single feature: ", paste0(dropped_gs, collapse = ", ")))
    data_table[data_table[paste0(queried_data, '_count')] > 1, ]
  }
  bandwidth = base::mean(base::vapply(data_table$gs2val, stats::bw.nrd0, numeric(1)))
  total_seq = seq(floor(min_value),ceiling(max_value), by=bandwidth)

  # Get the color palette values
  color_count = colors_switch(color_palette)
  color_list = get_colors(color_count = color_count, color_palette = color_palette)
  if (reverse_palette) {
    color_list = base::rev(color_list)
  }
  unique_values = sort(unique(data_table[,fill]))
  color_list = grDevices::colorRampPalette(color_list)(length(unique_values))
  names(color_list) = unique_values

  # Get the color scale
  color_scale = get_color_palette(groups = data_table[,fill],
                                  color_palette = color_palette,
                                  reverse_color_palette = reverse_palette,
                                  force_scale = T)

  # Produce the plot
  plot = plotly::plot_ly(width = width, height = height)
  incr = 0
  if (displayed_label %in% c('Description', 'ID')) {
    # Format y axis ticks & word split
    y_labels = axis_word_split(ticks = data_table[,displayed_label], n = yaxis_word_split)
    y_labels = base::factor(y_labels, levels = y_labels)
    tick_vals = 0:(nrow(data_table)-1)
  } else {
    y_labels = list()
    tick_vals = NULL
  }
  for (gs in rownames(data_table)) {
    fill_value = data_table[gs, fill]
    fill_col = color_list[as.character(fill_value)]
    fill_value = base::format(fill_value, scientific = TRUE, digits = 3)

    tmp_table = as.data.frame(table(cut(unname(data_table[gs, "gs2val"][[1]]), breaks=total_seq)))
    tmp_table$Var1 = strsplit(gsub("\\(|]", "", levels(tmp_table$Var1)), ',')
    x_values = c()
    for (l in tmp_table$Var1) {
      x_values = c(x_values, mean(as.numeric(stringr::str_split(l, ",")[[1]])))
    }
    tmp_table$Var1 = x_values

    tmp_table$hover = paste0(gs,
                             ":\nCount: ",
                             tmp_table$Freq,
                             "\n", fill, ": ",
                             fill_value,
                             "\nLog2(fold change): ",
                             round(tmp_table$Var1,2))

    tmp_table$Freq = tmp_table$Freq / max(tmp_table$Freq) + incr

    plot = plotly::add_trace(plot,
                             line = list(
                               color = "#FFFFFF",
                               width = 0.1
                             ),
                             mode = "lines",
                             type = "scatter",
                             x = c(0, max_x),
                             y = c(incr-0.01, incr-0.01),
                             legendgroup=0,
                             showlegend = F)

    incr = incr + 1

    plot = plotly::add_trace(plot,
                             fill = "tonexty",
                             line = list(color = "#000000",
                                         width = 0.5,
                                         shape = "spline",
                                         smoothing = 1.3),
                             mode = "lines",
                             type = "scatter",
                             x=tmp_table$Var1,
                             y=tmp_table$Freq,
                             name = gs,
                             fillcolor = fill_col,
                             text = tmp_table$hover,
                             hoverinfo = "text",
                             legendgroup=0,
                             showlegend = F)

  }

  # Trace for color scale only
  plot = add_trace(plot,
                   x = unique_values,
                   y = unique_values,
                   type = "scatter",
                   mode = "markers",
                   marker = list(
                     color = unique_values,
                     colorscale = color_scale,
                     colorbar = list(title = fill,
                                     tickformat = ".1e",
                                     tickfont = list(size = legend_font_size)),
                     size = 1,
                     opacity = 0.1),
                   legendgroup=0,
                   showlegend = F
  )

  plot = plotly::layout(
    p = plot ,
    title = title,
    showlegend = legend_show,
    yaxis = list(
      title = list(
        text = y_axis_title,
        font = list(size = y_label_font_size)
      ),
      showticklabels = ytick_show,
      tickfont = list(size = y_tick_font_size),
      type = "linear",
      range = c(0, nrow(data_table)),
      ticklen = 4,
      showgrid = TRUE,
      showline = FALSE,
      ticktext = y_labels,
      tickvals = tick_vals,
      zeroline = FALSE,
      gridcolor = "rgb(255,255,255)",
      gridwidth = 1
    ),

    xaxis = list(
      title = list(
        text = x_axis_title,
        font = list(size = x_label_font_size)
      ),
      showticklabels = xtick_show,
      tickfont = list(size = x_tick_font_size)
    ),
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )

  return(list(plot = plot,
              table = data_table))
}

#-------------------------------------------------------------- FA Bar plot ----
plot_fa_bar_plot = function(object,
                            x = "GeneRatio",
                            color = "pvalue",
                            show_category = 40,
                            displayed_label = "None",
                            order_by = "pvalue",
                            order_decreasing = F,
                            color_palette = 'RdYlBu',
                            reverse_palette = F,
                            title_font_size = 10,
                            yaxis_word_split = 0,
                            y_label_font_size = 10,
                            y_tick_font_size = 10,
                            x_label_font_size = 10,
                            x_tick_font_size = 10,
                            legend_font_size = 10,
                            width = NULL,
                            height = NULL) {

  # Fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  legend_show = base::ifelse(legend_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, x, "")
  y_axis_title = base::ifelse(y_label_font_size > 0, "Feature sets", "")
  title = base::ifelse(title_font_size > 0, "Bar plot", "")

  # Extract data
  data_table = as.data.frame(object)
  if (show_category > nrow(data_table)) {
    show_category = nrow(data_table)
    base::warning(paste0("Too many categories selected, setting to max: ", show_category))
  }
  data_table = data_table[1:show_category,]
  data_table$GeneRatio = unname(sapply(data_table$GeneRatio, function(x) eval(parse(text=x))))
  colnames(data_table)[which(colnames(data_table) == 'Count')] = "GeneCount"

  # Order data
  data_table = data_table[base::order(data_table[,order_by], decreasing = order_decreasing), ]

  # Displayed labels
  if (displayed_label != "None") {

    yaxis_ticks = axis_word_split(ticks = data_table[, displayed_label], n = yaxis_word_split)
    data_table[,displayed_label] = base::factor(yaxis_ticks, levels = yaxis_ticks)
    ticks = levels(data_table[, displayed_label])
  } else {
    data_table[, "None"] = 1:nrow(data_table)
    ticks = list()
  }

  # Get the color scale
  color_scale = get_color_palette(groups = data_table[,color],
                                  color_palette = color_palette,
                                  reverse_color_palette = reverse_palette,
                                  force_scale = T)

  plot = plotly::plot_ly(width = width, height = height)

  plot = plotly::add_trace(
    p = plot,
    x = data_table[,x],
    y = data_table[, displayed_label],
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = data_table[,color],
      colorscale = color_scale,
      colorbar = list(title = color,
                      tickformat = ".1e",
                      tickfont = list(size = legend_font_size)),
      showscale = legend_show
    )
  )

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
      showline = TRUE,
      tickfont = list(size = x_tick_font_size)
    ),

    yaxis = list(
      title = list(
        text = y_axis_title,
        font = list(size = y_label_font_size)
      ),
      showticklabels = ytick_show,
      showline = TRUE,
      tickfont = list(size = y_tick_font_size),
      tickvals = ticks,
      range = c(0, nrow(data_table))
    ),
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )

  return(list(
    plot = plot,
    table = data_table
  ))

}

#------------------------------------------------------------- FA CNET plot ----
plot_fa_cnet_plot = function(x,
                             prot_list,
                             show_category = 10,
                             displayed_labels = 'ID',
                             set_node_annotations = "p.adjust",
                             feature_node_annotations = "log2_fold_change",
                             set_node_color_palette = "Purples",
                             reverse_set_palette = F,
                             feature_node_color_palette = "RdYlBu",
                             reverse_feature_palette = F,
                             label_font_size = 40,
                             static_network = F,
                             solver = "barnesHut",
                             gravitationalConstant = -10000,
                             nodeDistance = 500,
                             centralGravity = 0.3,
                             springLength = 800,
                             springConstant = 0.01,
                             width = NULL,
                             height = NULL) {

  # Checks
  if (!(solver %in% c("barnesHut", "repulsion"))) {
    base::stop("solver must be one of [barnesHut, repulsion")
  }

  data_table = as.data.frame(x)

  if (show_category > nrow(data_table)){
    base::warning(paste0("Too many categories selected, reducing to max: ", nrow(data_table)))
    show_category = nrow(data_table)
  }

  if (class(x)[1] == "gseaResult") {
    feature_col = 'core_enrichment'
  } else if (class(x)[1] == "enrichResult") {
    feature_col = 'geneID'
    data_table$GeneRatio = unname(sapply(data_table$GeneRatio, function(x) eval(parse(text=x))))
    colnames(data_table)[which(colnames(data_table) == 'Count')] = "GeneCount"
  }

  data_table = data_table[1:show_category,]

  if (displayed_labels == 'Description') {
    data_table$set_nodes = data_table$Description
  } else if (displayed_labels == 'ID') {
    data_table$set_nodes = data_table$ID
  } else if (displayed_labels == 'ID and Description') {
    data_table$set_nodes = paste0(data_table$ID, '\n', data_table$Description)
  } else {
    stop("displayed_labels must be in ['Description', 'ID', 'ID and Description']")
  }
  set_nodes = data_table$set_nodes
  feature_nodes = unique(stringr::str_split(paste0(data_table[,feature_col], collapse = '/'), '/(?=[a-zA-Z])')[[1]])

  all_nodes = c(set_nodes, feature_nodes)
  set_nodes_idx = 1:length(set_nodes)
  feature_nodes_idx = (length(set_nodes) + 1):(length(feature_nodes) + length(set_nodes))

  node_table = data.frame(matrix(nrow = length(all_nodes), ncol = 1))
  colnames(node_table) = c("id")
  node_table$id = all_nodes
  node_table$label = all_nodes
  node_table$shape = rep("dot", nrow(node_table))
  node_table$values = NA
  node_table$color = NA
  node_table$title = NA
  node_table$font.size = label_font_size

  # Add hovers
  hover_values = c('enrichmentScore', 'NES', 'pvalue', 'p.adjust', 'GeneRatio', 'GeneCount')
  hover_values = hover_values[hover_values %in% colnames(data_table)]


  if (set_node_annotations == 'None') {
    node_table[set_nodes_idx, "color"] = "#ff8000"
  } else {
    node_table[set_nodes_idx, "values"] = data_table[set_nodes_idx, set_node_annotations]
    set_node_colors = get_color_palette(groups = node_table[set_nodes_idx, "values"],
                                        color_palette = set_node_color_palette,
                                        reverse_color_palette = reverse_set_palette,
                                        force_scale = F,
                                        force_list = T)
    node_table[set_nodes_idx,"color"] = unname(set_node_colors[as.character(node_table[set_nodes_idx, "values"])])
    node_table[set_nodes_idx,"title"] = paste0(set_node_annotations, ': ', format_values(node_table[set_nodes_idx, "values"]))
  }

  if (feature_node_annotations == 'None') {
    node_table[feature_nodes_idx, "color"] = "#0000FF"
  } else {
    node_table[feature_nodes_idx, "values"] = prot_list[feature_nodes,feature_node_annotations]
    feature_node_colors = get_color_palette(groups = node_table[feature_nodes_idx, "values"],
                                            color_palette = feature_node_color_palette,
                                            reverse_color_palette = reverse_feature_palette,
                                            force_scale = F,
                                            force_list = T)
    node_table[feature_nodes_idx,"color"] = unname(feature_node_colors[as.character(node_table[feature_nodes_idx, "values"])])
    node_table[feature_nodes_idx,"title"] = paste0(feature_node_annotations, ': ', format_values(node_table[feature_nodes_idx, "values"]))
  }

  source_nodes = c()
  target_nodes = c()
  for (i in rownames(data_table)) {
    set_features = stringr::str_split(data_table[i, feature_col], '/(?=[a-zA-Z])')[[1]]
    target_nodes = c(target_nodes, set_features)
    source_nodes = c(source_nodes, rep(data_table[i, 'set_nodes'], length(set_features)))
  }

  edge_table = data.frame(matrix(nrow = length(target_nodes), ncol = 2))
  colnames(edge_table) = c("from", "to")
  edge_table$from = source_nodes
  edge_table$to = target_nodes
  edge_table$width = rep(1, nrow(edge_table))

  plot = visNetwork::visNetwork(node_table, edge_table)
  if (static_network) {
    g = igraph::graph_from_data_frame(edge_table, directed = TRUE)
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

  return(list(plot = plot,
              table = list(node_table = node_table,
                           edge_table = edge_table))
  )

}

#------------------------------------------------------------- FA EMAP plot ----
plot_fa_emap_plot = function(x,
                             show_category = 50,
                             color = "p.adjust",
                             size = "GeneCount",
                             mode = "Both", # "Both", "Activated", "Suppressed"
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
                             node_magnifier = 0.2,
                             label_font_size = 40,
                             static_network = F,
                             solver = "barnesHut",
                             gravitationalConstant = -8000,
                             nodeDistance = 200,
                             centralGravity = 0.2,
                             springLength = 500,
                             springConstant = 0.04,
                             width = NULL,
                             height = NULL
) {

  data_table = as.data.frame(x)

  if (mode == "Activated") {
    data_table = data_table[data_table$enrichmentScore > 0,]
  } else if (mode == "Suppressed") {
    data_table = data_table[data_table$enrichmentScore < 0,]
  }

  # check if show_category appropriate
  if (show_category > nrow(data_table)) {
    base::warning(paste0("Too many categories selected, reducing to max: ", nrow(data_table)))
    show_category = nrow(data_table)
  }
  data_table = data_table[1:show_category,]

  if (class(x)[1] == "gseaResult") {
    data_table$GeneCount = stringr::str_count(data_table[,"core_enrichment"], '/') + 1
    data_table$GeneRatio = data_table$GeneCount / data_table$setSize
  } else if (class(x)[1] == "enrichResult") {
    data_table$GeneRatio = unname(sapply(data_table$GeneRatio, function(x) eval(parse(text=x))))
    colnames(data_table)[which(colnames(data_table) == 'Count')] = "GeneCount"
  }

  # Set displayed label
  if (displayed_labels == 'Description') {
    data_table$label = data_table$Description
  } else if (displayed_labels == 'ID') {
    data_table$label = data_table$ID
  } else if (displayed_labels == 'ID and Description') {
    data_table$label = paste0(data_table$ID, '\n', data_table$Description)
  } else {
    stop("displayed_labels must be in ['Description', 'ID', 'ID and Description']")
  }

  # Get description to ID dict
  desc_id_dict = rownames(data_table)
  names(desc_id_dict) = data_table$Description

  # Calculate similarities
  x = enrichplot::pairwise_termsim(x = x,
                                   method = similarity_score,
                                   semData = NULL,
                                   showCategory = show_category*3)

  # Get edge table
  edge_table = x@termsim
  base::diag(edge_table) = 0
  edge_table = reshape2::melt(edge_table, na.rm = TRUE)
  colnames(edge_table) = c("from", "to", "score")
  edge_table = edge_table[edge_table$score >= score_threshold, ]
  if (nrow(edge_table) == 0) {
    base::stop(paste0("No edges found above current threshold (", score_threshold, ")"))
  }
  edge_table$from = unname(desc_id_dict[as.character(edge_table$from)])
  edge_table$to = unname(desc_id_dict[as.character(edge_table$to)])
  edge_table$title = paste0(similarity_score, ' score: ', format_values(edge_table$score))


  # Edge aesthetics
  if (edge_width == "Similarity score") {
    edge_table$width = edge_table$score
    edge_table$width = edge_table$width * edge_magnifier
  }
  if (edge_color == "Similarity score") {
    edge_colors = get_color_palette(groups = edge_table$score,
                                    color_palette = edge_color_palette,
                                    reverse_color_palette = reverse_edge_palette,
                                    force_scale = F,
                                    force_list = T)
    edge_table[,"color"] = unname(edge_colors[as.character(edge_table[, "score"])])
  } else {
    edge_table$color = "black"
  }

  # Filter out missing edges
  if (any(is.na(edge_table$from))) {
    edge_table = edge_table[!is.na(edge_table$from),]
  }
  if (any(is.na(edge_table$to))) {
    edge_table = edge_table[!is.na(edge_table$to),]
  }


  # Get node table
  node_table = data_table[,c('ID', 'label', size, color)]
  colnames(node_table) = c("id", "label", "size", "color_values")
  node_table$shape = "dot"
  node_table$title = paste0(size,
                            ' (size): ',
                            format_values(node_table$size),
                            '<p>',
                            color,
                            ' (color): ',
                            format_values(node_table$color_values)
                            )


  # Setting the color gradient
  node_colors = get_color_palette(groups = node_table[, "color_values"],
                                  color_palette = node_color_palette,
                                  reverse_color_palette = reverse_node_palette,
                                  force_scale = F,
                                  force_list = T)
  node_table[,"color"] = unname(node_colors[as.character(node_table[, "color_values"])])
  node_table$size = node_table$size * node_magnifier
  node_table$font.size = label_font_size


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

  return(list(plot = plot,
              table = list(
                node_table = node_table,
                edge_table = edge_table
              )))

}
#-------------------------------------------------------- PLOTLY DENDROGRAM ----
# Define function
plot_dendrogram = function(data_table, meta_table, annotations, distance_method = "euclidian", p = 2, clustering_method = "ward.D2", k_clusters = NULL, color_palette = 'Spectral', rotate = FALSE, x_tick_font_size, y_label_font_size, y_tick_font_size, width = NULL, height = NULL) {

  # Fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ylabel_show = base::ifelse(y_label_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)

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

  # Get clusters if specified
  if (!is.null(k_clusters)) {
    clusters = stats::cutree(tree = hc, k = k_clusters)
    meta_table[,'k_clusters'] = clusters
    annotations = c(annotations, 'k_clusters')
  }

  # Deal with colors
  if (length(annotations) > length(color_palette)) {
    all_colors = c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                   'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
                   'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd', 'BrBG', 'PiYG', 'PRGn',
                   'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral', 'Accent',
                   'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3',
                   'Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo',
                   'plotly_1', 'plotly_2')
    all_colors = all_colors[!all_colors %in% c(color_palette)]
    additional_palettes = length(annotations) - length(color_palette)
    color_palette = c(color_palette, all_colors[1:additional_palettes])
  }

  p = ggdendro::ggdendrogram(hc, rotate = rotate, size = 2)

  plotly_dendro = plotly::ggplotly(p)
  plotly_dendro = plotly::layout(p = plotly_dendro,
                                 yaxis = list(showticklabels = ytick_show,
                                              tickfont = list(size = y_tick_font_size)))



  # Reorder the rownames according to clustering
  original_rownames = original_rownames[hc$order]
  meta_table = as.data.frame(meta_table[hc$order, annotations])
  colnames(meta_table) = annotations


  # Dealing with colors
  heatmaps_list = list(
    dendro = plotly_dendro
  )
  for (i in 1:length(annotations)) {
    annotations_i = annotations[i]
    groups = sort(unique(meta_table[,annotations_i]))

    color_palette_i = get_color_palette(groups = meta_table[,annotations_i],
                                        color_palette = color_palette[i],
                                        reverse_color_palette = F)

    # Assign numeric values according to groups
    groups_numeric = 1:length(groups)
    names(groups_numeric) = as.character(groups)
    groups_numeric = unname(groups_numeric[as.character(meta_table[,annotations_i])])
    hover_text = paste(original_rownames, meta_table[,annotations_i], sep = '\n')


    # Produce the side color heatmap
    rownames(meta_table) = NULL
    x_values = factor(as.numeric(rownames(meta_table)), ordered = T, levels = unique(as.numeric(rownames(meta_table))))
    if (typeof(color_palette_i) == 'list') {
      plotly_heatmap = plotly::plot_ly(x = x_values,
                                       y = rep(annotations_i,nrow(meta_table)),
                                       z = groups_numeric,
                                       type = "heatmap",
                                       colorscale = color_palette_i,
                                       # colorbar = list(title = "Colorbar Title"),
                                       # colors = color_palette_i,
                                       xgap = 1,
                                       ygap = NULL,
                                       text = hover_text,
                                       hoverinfo = 'text',
                                       showscale = F)
    } else if (typeof(color_palette_i) == 'character') {
      plotly_heatmap = plotly::plot_ly(x = x_values,
                                       y = rep(annotations_i,nrow(meta_table)),
                                       z = groups_numeric,
                                       type = "heatmap",
                                       colors = color_palette_i,
                                       xgap = 1,
                                       ygap = NULL,
                                       text = hover_text,
                                       hoverinfo = 'text',
                                       showscale = F)
    } else {
      stop('Unexpected annotation type')
    }

    plotly_heatmap = plotly::layout(p = plotly_heatmap,
                                    yaxis = list(showticklabels = ylabel_show,
                                                 tickfont = list(size = y_label_font_size)),
                                    xaxis = list(tickvals = 1:length(x_values),
                                                 showticklabels = xtick_show,
                                                 tickfont = list(size = x_tick_font_size)))

    heatmaps_list[[annotations_i]] = plotly_heatmap

  }

  heights = rep(0.1, length(annotations))
  heights = c(1 - sum(heights), heights)

  out_plot = plotly::subplot(
    heatmaps_list,
    margin = 0,
    nrows = length(heatmaps_list),
    shareX = T,
    shareY = F,
    heights = heights
  )

  out_plot = plotly::layout(p = out_plot,
                            xaxis = list(ticktext = original_rownames))

  # Set sample names back to the meta_table
  rownames(meta_table) = original_rownames

  return(list(
    plot = out_plot,
    data_table = meta_table)
    )

}



# SNF

sample_clustering = function(data_table,
                             meta_table,
                             K_nearest_neighbors = 3,
                             sigma = 0.5,
                             distance_method = 'euclidean', # "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
                             K_clusters = 3,
                             z_max = NULL,
                             z_min = NULL,
                             vertical_annotations = "K clusters",
                             horizontal_annotations = NULL,
                             color_palette = "Viridis",
                             reverse_palette = F,
                             title_font_size = 0,
                             x_tick_font_size = 0,
                             y_tick_font_size = 0,
                             width = NULL,
                             height = NULL) {

  # Fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  title = base::ifelse(title_font_size > 0, paste0('<span style="font-size: ', title_font_size, 'px;">Sample clustering</span>'), "")

  # Get the affinity matrix
  data_table = base::as.matrix(stats::dist(x = data_table,
                                           method = distance_method))
  data_table = SNFtool::affinityMatrix(data_table, K = K_nearest_neighbors, sigma = sigma)

  # Change samplenames if coercible to numeric (plotly requirement)
  if (is_coercible_to_numeric(rownames(data_table))) {
    rownames(data_table) = paste0('X',rownames(data_table))
    colnames(data_table) = paste0('X',colnames(data_table))
  }

  clusters = SNFtool::spectralClustering(data_table, K = K_clusters)
  order_by_cluster = base::order(clusters)
  data_table = data_table[order_by_cluster,order_by_cluster]
  diag(data_table) = min(data_table, na.rm = T)



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

  plot = heatmaply::heatmaply(
    data_table,
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
    width = width,
    height = height
  )
  plot = plotly::layout(
    p = plot,
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
    )
  return(list(data_table = data_table,
              plot = plot))
}


plot_similarity_network = function(data_table,
                                   group_colors,
                                   node_color_palette,
                                   K_nearest_neighbors,
                                   K_clusters,
                                   sigma,
                                   distance_method,
                                   top_edges = 0.1,
                                   node_opacity = 1,
                                   label_font_size = 40,
                                   edge_magnifier = 1,
                                   legend,
                                   static_network = F,
                                   solver = "barnesHut",
                                   gravitationalConstant = -8000,
                                   nodeDistance = 200,
                                   centralGravity = 0.2,
                                   springLength = 500,
                                   springConstant = 0.04,
                                   width = NULL,
                                   height = NULL
                                   ) {
  # Get the affinity matrix
  data_table = base::as.matrix(stats::dist(x = data_table,
                                           method = distance_method))
  data_table = SNFtool::affinityMatrix(data_table, K = K_nearest_neighbors, sigma = sigma)

  if (length(group_colors) == 1){
    if (group_colors == 'K clusters') {
      group_colors = SNFtool::spectralClustering(data_table, K = K_clusters)
    }
  }

  edge_table = igraph::graph_from_adjacency_matrix(adjmatrix = data_table,
                                                   weighted= TRUE,
                                                   mode="undirected",
                                                   diag=F)

  weights = igraph::E(edge_table)$weight
  max_weights = max(weights)
  edge_table = base::as.data.frame(igraph::as_edgelist(edge_table))
  colnames(edge_table) = c("from" , "to")
  edge_table$value = weights
  edge_table = edge_table[order(-edge_table$value),]
  edge_table = edge_table[1:ceiling(nrow(edge_table) * top_edges),]

  # Create the node table with coloring
  node_table = data.frame(id = 1:ncol(data_table),
                          label = colnames(data_table))
  node_table$group = group_colors

  # Node colors
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
  edge_table$title = paste0("Weight: ", round(edge_table$value, 5))

  # Adapt edge width for visibility
  edge_table$value = edge_table$value * edge_magnifier
  node_table$opacity = node_opacity
  node_table$font.size = label_font_size
  node_table$borderWidth = 0

  # # Produce the plot
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
    plot = visNetwork::visLegend(graph = plot)
  }

  out = list(
    node_table = node_table,
    edge_table = edge_table,
    plot = plot
  )
}

#--------------------------------------------------------- Server utilities ----

plot_one_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ora_plotbox_switch_ui(selection_list = selection_list)

  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ora_plotbox_switch_server(selection_list = input$show_plots_or)

  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ora_plotbox_switch_ui(selection_list = selection_list)
  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ora_plotbox_switch_server(selection_list = input$show_plots_or)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ora_plotbox_switch_ui(selection_list = selection_list)
  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ora_plotbox_switch_server(selection_list = input$show_plots_or)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ora_plotbox_switch_ui(selection_list = selection_list)
  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ora_plotbox_switch_server(selection_list = input$show_plots_or)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_one_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ea_plotbox_switch_ui(selection_list = selection_list)

  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ea_plotbox_switch_server(selection_list = input$show_plots_gsea)

  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ea_plotbox_switch_ui(selection_list = selection_list)
  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ea_plotbox_switch_server(selection_list = input$show_plots_gsea)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ea_plotbox_switch_ui(selection_list = selection_list)
  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ea_plotbox_switch_server(selection_list = input$show_plots_gsea)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = ea_plotbox_switch_ui(selection_list = selection_list)
  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = ea_plotbox_switch_server(selection_list = input$show_plots_gsea)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}



plot_one_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}



# switches

prot_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_dendrogram" = dendrogram_ui,
                                          "select_pca" = pca_ui,
                                          "select_heatmap" = heatmap_ui,
                                          "select_samples_correlation" = samples_correlation_ui,
                                          "select_feature_correlation" = feature_correlation_ui,
                                          "select_volcano_plot" = volcano_plot_ui
    )
    )
  }
  return(ui_functions)
}

prot_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_dendrogram" = dendrogram_server,
                                                  "select_pca" = pca_server,
                                                  "select_heatmap" = heatmap_server,
                                                  "select_samples_correlation" = samples_correlation_server,
                                                  "select_feature_correlation" = feature_correlation_server,
                                                  "select_volcano_plot" = volcano_plot_server
    )
    )
  }
  return(server_functions)
}

ea_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_dot_plot" = ea_dot_plot_ui,
                                          "select_ridge_plot" = ea_ridge_plot_ui,
                                          "select_cnet_plot" = ea_cnet_plot_ui,
                                          "select_emap_plot" = ea_emap_plot_ui
    )
    )
  }
  return(ui_functions)
}

ea_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_dot_plot" = ea_dot_plot_server,
                                                  "select_ridge_plot" = ea_ridge_plot_server,
                                                  "select_cnet_plot" = ea_cnet_plot_server,
                                                  "select_emap_plot" = ea_emap_plot_server)
    )
  }
  return(server_functions)
}


ora_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_dot_plot" = ora_dot_plot_ui,
                                          "select_bar_plot" = ora_bar_plot_ui,
                                          "select_cnet_plot" = ora_cnet_plot_ui,
                                          "select_emap_plot" = ora_emap_plot_ui
    )
    )
  }
  return(ui_functions)
}

ora_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_dot_plot" = ora_dot_plot_server,
                                                  "select_bar_plot" = ora_bar_plot_server,
                                                  "select_cnet_plot" = ora_cnet_plot_server,
                                                  "select_emap_plot" = ora_emap_plot_server)
    )
  }
  return(server_functions)
}

plotbox_switch_ui_lips = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_dendrogram" = dendrogram_ui,
                                          "select_class_distribution" = class_distribution_ui,
                                          "select_class_comparison" = class_comparison_ui,
                                          "select_volcano_plot" = volcano_plot_ui,
                                          "select_heatmap" = heatmap_ui,
                                          "select_samples_correlation" = samples_correlation_ui,
                                          "select_feature_correlation" = feature_correlation_ui,
                                          "select_pca" = pca_ui,
                                          "select_fatty_acid_analysis" = fa_analysis_plot_ui,
                                          "select_fatty_acid_composition" = fa_comp_plot_ui,
                                          "select_double_bonds_plot" = double_bonds_plot_ui
    )
    )
  }
  return(ui_functions)
}

plotbox_switch_server_lips = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_dendrogram" = dendrogram_server,
                                                  "select_class_distribution" = class_distribution_server,
                                                  "select_class_comparison" = class_comparison_server,
                                                  "select_volcano_plot" = volcano_plot_server,
                                                  "select_heatmap" = heatmap_server,
                                                  "select_samples_correlation" = samples_correlation_server,
                                                  "select_feature_correlation" = feature_correlation_server,
                                                  "select_pca" = pca_server,
                                                  "select_fatty_acid_analysis" = fa_analysis_plot_server,
                                                  "select_fatty_acid_composition" = fa_comp_plot_server,
                                                  "select_double_bonds_plot" = double_bonds_plot_server
    )
    )
  }
  return(server_functions)
}

plot_one_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


reset_sample_filters = function(input, session, r6) {
  # Set all checkboxes to False
  shinyWidgets::updateCheckboxGroupButtons(
    session = session,
    inputId = "non_samples_selection",
    selected = character(0)
  )

  # Set manual row selection to None and update
  shiny::updateSelectizeInput(
    session = session,
    inputId = "selection_manual",
    choices = rownames(r6$tables$meta_filtered),
    selected = character(0)
  )


  # Set the metacolumn value to None and update
  shiny::updateSelectInput(
    session = session,
    inputId = "exclusion_meta_val",
    choices = unique(r6$tables$meta_filtered[,input$exclusion_meta_col]),
    selected = character(0)
  )

  # Set metadata row exclusion to None
  shiny::updateSelectizeInput(
    session = session,
    inputId = "exclusion_meta_row",
    selected = character(0)
  )
}

update_sample_filters = function(input, session, r6) {
  # Update input for the manual exclusion
  shiny::updateSelectizeInput(
    session = session,
    inputId = "selection_manual",
    choices = rownames(r6$tables$raw_meta)
  )

  # Update available groups to filter
  if (input$exclusion_meta_col != "") {
    shiny::updateSelectInput(
      session = session,
      inputId = "exclusion_meta_val",
      choices = unique(r6$tables$raw_meta[,input$exclusion_meta_col]),
      selected = character(0)
    )
  }
}

update_lipid_filters = function(input, session, r6, prog_bars = T) {
  # Update class selection
  shiny::updateSelectizeInput(
    session = session,
    inputId = "class_selection",
    choices = unique(r6$tables$feature_table$lipid_class),
    selected = character(0)
  )

  # Update manual selection
  shiny::updateSelectizeInput(
    session = session,
    inputId = "manual_selection",
    # choices = remaining_cols,
    choices = colnames(r6$tables$raw_data),
    selected = character(0)
  )

  if (prog_bars) {
    shinyWidgets::updateProgressBar(
      session = session,
      id = "col_count_bar",
      value = ncol(r6$tables$raw_data),
      total = ncol(r6$tables$imp_data)
    )

    shinyWidgets::updateProgressBar(
      session = session,
      id = "row_count_bar_data",
      value = nrow(r6$tables$raw_data),
      total = nrow(r6$tables$imp_data)
    )
  }
}


sample_row_selection = function(input, r6) {
  # Initialise selection
  selected_rows = c()

  # Get blank rows
  if ("Blanks" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_blanks)
  }

  # Get QC rows
  if ("QCs" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_qcs)
  }

  # Get Pool rows
  if ("Pools" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_pools)
  }

  # Add metadata and manual exclusions
  selected_rows = c(selected_rows,input$exclusion_meta_row,input$selection_manual)
  selected_rows = sort(unique(selected_rows))

  return(selected_rows)
}


#------------------------------------------------------------ SNF utilities ----

snf_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_sample_clustering_1" = sample_clustering_1_ui,
                                          "select_sample_clustering_2" = sample_clustering_2_ui,
                                          'select_similarity_network_1' = similarity_network_1_ui,
                                          'select_similarity_network_2' = similarity_network_2_ui,
                                          'select_fusion_heatmap' = fusion_heatmap_ui,
                                          'select_similarity_network_fusion' = similarity_network_fusion_ui)
    )
  }
  return(ui_functions)
}

snf_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_sample_clustering_1" = sample_clustering_1_server,
                                                  "select_sample_clustering_2" = sample_clustering_2_server,
                                                  'select_similarity_network_1' = similarity_network_1_server,
                                                  'select_similarity_network_2' = similarity_network_2_server,
                                                  'select_fusion_heatmap' = fusion_heatmap_server,
                                                  'select_similarity_network_fusion' = similarity_network_fusion_server)
    )
  }
  return(server_functions)
}

snf_plot_one = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


snf_plot_two = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

snf_plot_three = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

snf_plot_four = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

#----------------------------------------------------------- MOFA utilities ----

mofa_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_explained_variance" = explained_variance_ui,
                                          "select_factor_plot" = factor_plot_ui,
                                          "select_combined_factors_plot" = combined_factors_plot_ui,
                                          "select_feature_weights" = feature_weights_ui,
                                          "select_feature_top_weights" = feature_top_weights_ui,
                                          "select_mofa_heatmap" = mofa_heatmap_ui,
                                          "select_scatter_plot" = scatter_plot_ui)
    )
  }
  return(ui_functions)
}

mofa_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_explained_variance" = explained_variance_server,
                                                  "select_factor_plot" = factor_plot_server,
                                                  "select_combined_factors_plot" = combined_factors_plot_server,
                                                  "select_feature_weights" = feature_weights_server,
                                                  "select_feature_top_weights" = feature_top_weights_server,
                                                  "select_mofa_heatmap" = mofa_heatmap_server,
                                                  "select_scatter_plot" = scatter_plot_server)
    )
  }
  return(server_functions)
}

mofa_plot_one = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


mofa_plot_two = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

mofa_plot_three = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

mofa_plot_four = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

#--------------------------------------------- MOFA explained variance plot ----
plot_mofa_explained_variance = function(data_table, color_palette = 'Blues', reverse_color_palette = F, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, width = NULL, height = NULL) {

  # Fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  legend_show = base::ifelse(legend_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, "Factors", "")
  y_axis_title = base::ifelse(y_label_font_size > 0, "Omics", "")
  title = base::ifelse(title_font_size > 0, "Explained variance", "")
  secondary_title = "Var. Expl. (%)"

  factor_sums = base::rowSums(data_table)
  omics_sums = base::colSums(data_table)

  # Get color palette
  color_count = colors_switch(color_palette)
  colors = get_colors(color_count = color_count, color_palette = color_palette)
  if (reverse_color_palette) {
    colors = base::rev(colors)
  }

  heatmap = plotly::plot_ly(
    x = colnames(data_table),
    y = rownames(data_table),
    z = data_table,
    type = "heatmap",
    colors = colors,
    colorbar = list(title = "Variance",
                    tickfont = list(size = legend_font_size)),
    width = width,
    height = height
  )


  heatmap = plotly::layout(
    p = heatmap,
    xaxis = list(
      title = list(
        text = x_axis_title,
        font = list(size = x_label_font_size)
      ),
      showticklabels = xtick_show,
      tickfont = list(size = x_tick_font_size)
    ),

    yaxis = list(
      title = list(
        text = y_axis_title,
        font = list(size = y_label_font_size)
      ),
      showticklabels = ytick_show,
      tickfont = list(size = y_tick_font_size)
    ),
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )


  omics_bars = plotly::plot_ly(width = width, height = height)
  omics_bars = plotly::add_trace(
    p = omics_bars,
    x = factor(names(omics_sums), levels = names(omics_sums)),
    y = omics_sums,
    hoverinfo = 'text',
    hovertext = paste0(names(omics_sums), ': ', round(omics_sums,1)),
    type = "bar",
    marker = list(color = '#ff9b5f'),
    showlegend = F
  )

  omics_bars = plotly::layout(
    p = omics_bars,
    xaxis = list(
      tickvals = list(),
      showticklabels = FALSE,
      showline = TRUE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = list(
        text = secondary_title,
        font = list(size = y_tick_font_size)
      ),
      showticklabels = ytick_show,
      tickfont = list(size = 0.8*y_tick_font_size),
      showline = TRUE,
      zeroline = FALSE
    )
  )



  factors_bars = plotly::plot_ly(width = width, height = height)
  factors_bars = plotly::add_trace(
    p = factors_bars,
    x = factor_sums,
    y = factor(names(factor_sums), levels = names(factor_sums)),
    hoverinfo = 'text',
    hovertext = paste0(names(factor_sums), ': ', round(factor_sums,1)),
    type = "bar",
    marker = list(color = '#ff9b5f'),
    orientation = 'h',
    showlegend = F
  )

  factors_bars = plotly::layout(
    p = factors_bars,
    xaxis = list(
      title = list(
        text = secondary_title,
        font = list(size = x_tick_font_size)
      ),
      showticklabels = xtick_show,
      tickfont = list(size = 0.8*x_tick_font_size),
      showline = TRUE,
      zeroline = FALSE
    ),
    yaxis = list(
      tickvals = list(),
      showticklabels = FALSE,
      showline = FALSE,
      zeroline = FALSE
    )
  )



  blank_plot = create_blank_plot()

  plot = plotly::subplot(
    list(omics_bars, blank_plot,
         heatmap, factors_bars),
    nrows = 2,
    shareX = F,
    shareY = F,
    titleX = T,
    titleY = T,
    margin = 0.005,
    widths = c(0.8, 0.2),
    heights = c(0.2, 0.8)
  )

  plot = plotly::layout(
    p = plot,
    title = list(
      text = title,
      x = 0.5,
      xanchor = "center",
      yanchor = "top"
    )
  )
  return(plot)
}
#--------------------------------------------------------- MOFA factor plot ----

create_factor_plot = function(factor_table, factor_name, marker_size = 10, opacity = 1, add_violin = TRUE, violin_alpha = 0.5, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, legend_show = T, width = NULL, height = NULL) {

  # Fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, factor_name, "")
  y_axis_title = base::ifelse(y_label_font_size > 0, 'Factor weight', "")
  title = base::ifelse(title_font_size > 0, "Factor weights", "")

  # Filter table for the given factor
  tmp_table = factor_table[factor_table$factor == factor_name,]
  # Initialize an empty plot
  plot = plotly::plot_ly(width = width, height = height)
  # Loop through unique groups to add traces
  for (grp in unique(tmp_table[,"group"])) {
    grp_table = tmp_table[tmp_table[,"group"] == grp,]
    grp_numeric = grp_table$group_numeric[1]
    grp_color = grp_table$color[1]

    if (add_violin) {
      plot = plotly::add_trace(p = plot,
                               y = grp_table$value,
                               x = grp_numeric,
                               type = "violin",
                               box = list(visible = FALSE),
                               line = list(color = grp_color),
                               fillcolor = grp_color,
                               meanline = list(visible = F),
                               opacity = violin_alpha,
                               points = FALSE,
                               legendgroup = grp,
                               hoverinfo = 'none',
                               showlegend = F,
                               orientation = 'v')
    }


    plot = plotly::add_trace(p = plot,
                             type = "scatter",
                             mode = "markers",
                             y = grp_table$value,
                             x = grp_numeric,
                             marker = list(size = marker_size,
                                           color = grp_color,
                                           opacity = opacity,
                                           line = list(width = 0.5, color = 'white')),
                             name = grp,
                             legendgroup = grp,
                             text = grp_table$hover,
                             hoverinfo = 'text',
                             showlegend = legend_show)
  }
  # Set the layout for the plot
  plot = plotly::layout(p = plot,
                        title = list(text = title,
                                     font = list(size = title_font_size)),
                        yaxis = list(
                          title = list(
                            text = y_axis_title,
                            font = list(size = y_label_font_size)
                          ),
                          showticklabels = ytick_show,
                          tickfont = list(size = y_tick_font_size)
                        ),

                        xaxis = list(
                          title = list(
                            text = x_axis_title,
                            font = list(size = x_label_font_size)
                          ),
                          showticklabels = F,
                          tickfont = list(size = x_tick_font_size)
                        ),

                        legend = list(
                          font = list(
                            size = legend_font_size
                          )
                        ),
                        plot_bgcolor='rgba(0,0,0,0)',
                        paper_bgcolor='rgba(0,0,0,0)'
  )
  return(plot)
}
plot_factor_plot = function(model, sample_metadata, factors, scale = F, groups, show_missing = F, color_palette = 'Spectral', marker_size = 10, opacity = 1, add_violin = T, show_legend = T, violin_alpha = 0.5, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, width = NULL, height = NULL) {

  factors = MOFA2::factors_names(model)[factors]
  factor_table = MOFA2::get_factors(model, factors = factors, groups = "all",
                                    as.data.frame = TRUE)

  factor_table$group = sample_metadata[factor_table$sample, groups]

  # Check on the number of groups (must be < 10)
  if (length(base::unique(factor_table$group)) > 10) {
    base::warning('More than 10 groups supplied, displaying as a single group.')
    factor_table$group = 'Samples'
  }

  # If there are missing values
  factor_table$group[factor_table$group == ""] = NA
  if (show_missing) {
    factor_table$group[base::is.na(factor_table$group)] = 'NA'

  } else {
    factor_table = factor_table[!base::is.na(factor_table$group),]
  }


  factor_table$group_numeric = as.numeric(as.factor(factor_table$group))
  factor_table$hover = paste0(factor_table$sample,
                              '\n',
                              round(factor_table$value, 4))

  colors = get_color_palette(groups = factor_table$group,
                             color_palette = color_palette,
                             reverse_color_palette = F,
                             force_scale = F,
                             force_list = T)
  factor_table$color = unname(colors[factor_table$group])

  # Scale
  if (scale) {
    factor_table$value = factor_table$value/max(abs(factor_table$value))
  }

  # Plot
  legend_show = base::ifelse(legend_font_size > 0, T, F)

  if (length(factors) > 1) {
    plot_list = list()
    for (fact in factors) {
      plot_list[[fact]] = create_factor_plot(factor_table = factor_table,
                                             factor_name = fact,
                                             marker_size = marker_size,
                                             opacity = opacity,
                                             add_violin = add_violin,
                                             violin_alpha = violin_alpha,
                                             title_font_size = title_font_size,
                                             y_label_font_size = y_label_font_size,
                                             y_tick_font_size = y_tick_font_size,
                                             x_label_font_size = x_label_font_size,
                                             x_tick_font_size = x_tick_font_size,
                                             legend_font_size = legend_font_size,
                                             legend_show = legend_show,
                                             width = width,
                                             height = height)
      legend_show = F
    }
    plot = plotly::subplot(plot_list, nrows = 1, shareX = FALSE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
  } else {
    plot = create_factor_plot(factor_table = factor_table,
                              factor_name = factors,
                              marker_size = marker_size,
                              opacity = opacity,
                              add_violin = add_violin,
                              violin_alpha = violin_alpha,
                              title_font_size = title_font_size,
                              y_label_font_size = y_label_font_size,
                              y_tick_font_size = y_tick_font_size,
                              x_label_font_size = x_label_font_size,
                              x_tick_font_size = x_tick_font_size,
                              legend_font_size = legend_font_size,
                              legend_show = legend_show,
                              width = width,
                              height = height)
  }
  return(list(plot = plot,
              table = factor_table))
}





#----------------------------------------------- MOFA combined factors plot ----
create_factor_density_plot = function(factor_table, factor_name, color_palette, area_alpha = 0.5, show_legend = T, show_yaxis_label = T, y_label_font_size = 13, y_tick_font_size = 10, x_label_font_size = 13, x_tick_font_size = 10, legend_font_size = 10, width = NULL, height = NULL){

  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, factor_name, "")
  y_axis_title = base::ifelse(y_label_font_size > 0, factor_name, "")

  if (!show_yaxis_label) {
    y_axis_title = NULL
  }

  plot = plotly::plot_ly(width = width, height = height)
  for (grp in unique(factor_table$group)) {
    grp_idx = which(factor_table$group == grp)
    grp_color = color_palette[grp]
    density_data = stats::density(factor_table[grp_idx, factor_name])
    kept = seq(1, length(density_data$x), by = 10)
    plot = plotly::add_trace(
      p = plot,
      x = density_data$x[kept],
      y = density_data$y[kept],
      type = 'scatter',
      mode = 'lines',
      fill = 'tozeroy',
      name = grp,
      line = list(color = grp_color),
      fillcolor = hex_to_rgba(grp_color, alpha = area_alpha),
      legendgroup=grp,
      showlegend = show_legend
    )
  }

  plot = plotly::layout(
    p = plot,
    xaxis = list(
      title = list(
        text = x_axis_title,
        font = list(size = x_label_font_size)
      ),
      showticklabels = xtick_show,
      tickfont = list(size = x_tick_font_size)
    ),

    yaxis = list(
      title = list(
        text = y_axis_title,
        font = list(size = y_label_font_size)
      ),
      showticklabels = ytick_show,
      tickfont = list(size = y_tick_font_size)
    ),
    legend = list(
      font = list(
        size = legend_font_size
      )
    )
  )

  return(plot)
}

create_factor_comparison_plot = function(factor_table, factor_1, factor_2, color_palette, marker_size, marker_opacity, show_legend = T, show_yaxis_label = T, y_label_font_size = 13, y_tick_font_size = 10, x_label_font_size = 13, x_tick_font_size = 10, legend_font_size = 10, width = NULL, height = NULL) {

  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, factor_2, "")
  y_axis_title = base::ifelse(y_label_font_size > 0, factor_1, "")


  if (!show_yaxis_label) {
    y_axis_title = NULL
    # x_axis_title = NULL
  }


  factor_table$hover = paste0(rownames(factor_table), '\n', factor_2, ': ', round(factor_table[,factor_1], 4), '\n', factor_1, ': ', round(factor_table[,factor_2], 4))

  plot = plotly::plot_ly(width = width, height = height)
  for (grp in unique(factor_table$group)) {
    grp_idx = which(factor_table$group == grp)
    plot = plotly::add_trace(
      p = plot,
      x = factor_table[grp_idx, factor_1],
      y = factor_table[grp_idx, factor_2],
      type = 'scatter',
      mode = "markers",
      marker = list(size = marker_size,
                    color = color_palette[grp],
                    opacity = marker_opacity,
                    line = list(width = 0.5, color = 'white')),
      name = grp,
      text = factor_table[grp_idx, 'hover'],
      hoverinfo = 'text',
      legendgroup=grp,
      showlegend = show_legend
    )
  }

  plot = plotly::layout(
    p = plot,
    xaxis = list(
      title = list(
        text = x_axis_title,
        font = list(size = x_label_font_size)
      ),
      showticklabels = xtick_show,
      tickfont = list(size = x_tick_font_size)
    ),

    yaxis = list(
      title = list(
        text = y_axis_title,
        font = list(size = y_label_font_size)
      ),
      showticklabels = ytick_show,
      tickfont = list(size = y_tick_font_size)
    ),
    legend = list(
      font = list(
        size = legend_font_size
      )
    )
  )


  return(plot)
}

plot_combined_factors_plot = function(model, factors, sample_metadata, groups, scale = F, show_missing = F, color_palette = 'Spectral', marker_size = 10, marker_opacity = 1, area_alpha = 0.5, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, width = NULL, height = NULL) {

  legend_show = base::ifelse(legend_font_size > 0, T, F)
  title = base::ifelse(title_font_size > 0, "Combined factors", "")

  label_counters = 1
  for (i in 1:(length(factors)-1)) {
    label_counters = c(label_counters, label_counters[length(label_counters)] + length(factors))
  }

  factors = MOFA2:::.check_and_get_factors(model, factors)
  factor_table = MOFA2::get_factors(model, factors = factors, groups = "all",
                                    as.data.frame = F)
  factor_table = as.data.frame(factor_table$group1)
  factor_table$group = sample_metadata[rownames(factor_table), groups]

  # If there are missing values
  factor_table$group[factor_table$group == ""] = NA
  if (show_missing) {
    factor_table$group[base::is.na(factor_table$group)] = 'NA'

  } else {
    factor_table = factor_table[!base::is.na(factor_table$group),]
  }

  if (length(unique(factor_table$group)) > nrow(factor_table)/2) {
    warning('Less than two samples per group, removing groups')
    factor_table$group = 1
  }

  if (scale) {
    for (factor in factors) {
      factor_table[[factor]] = factor_table[[factor]]/max(abs(factor_table[[factor]]))
    }
  }

  # unique_groups = unique(factor_table$group)
  # color_count = colors_switch(color_palette)
  # color_palette = get_colors(color_count = color_count, color_palette = color_palette)
  # color_palette = grDevices::colorRampPalette(color_palette)(length(unique_groups))
  # color_palette = setNames(color_palette, unique_groups)
  # factor_table$color = unname(color_palette[factor_table$group])
  colors = get_color_palette(groups = factor_table$group,
                             color_palette = color_palette,
                             reverse_color_palette = F,
                             force_scale = F,
                             force_list = T)

  if (length(factors) > 1) {
    counter = 1
    plot_list = list()
    for (factor_1 in factors) {
      for (factor_2 in factors) {
        if (factor_1 == factor_2) {
          if (counter %in% label_counters) {
            show_yaxis_label = T
          } else {
            show_yaxis_label = F
          }
          if (counter > (length(factors) * length(factors) - length(factors))) {
            tmp_x_label_font_size = x_label_font_size
          } else {
            tmp_x_label_font_size = 0
          }
          counter = counter + 1
          plot_list[[length(plot_list) + 1]] = create_factor_density_plot(factor_table = factor_table,
                                                                          factor_name = factor_1,
                                                                          show_yaxis_label = show_yaxis_label,
                                                                          color_palette = colors,
                                                                          area_alpha = area_alpha,
                                                                          y_label_font_size = y_label_font_size,
                                                                          y_tick_font_size = y_tick_font_size,
                                                                          x_label_font_size = tmp_x_label_font_size,
                                                                          x_tick_font_size = x_tick_font_size,
                                                                          legend_font_size = legend_font_size,
                                                                          show_legend = legend_show)

          legend_show = F

        } else {

          if (counter %in% label_counters) {
            show_yaxis_label = T
          } else {
            show_yaxis_label = F
          }
          if (counter > (length(factors) * length(factors) - length(factors))) {
            tmp_x_label_font_size = x_label_font_size
          } else {
            tmp_x_label_font_size = 0
          }
          counter = counter + 1
          plot_list[[length(plot_list) + 1]] = create_factor_comparison_plot(factor_table = factor_table,
                                                                             factor_1 = factor_1,
                                                                             factor_2 = factor_2,
                                                                             show_yaxis_label = show_yaxis_label,
                                                                             color_palette = colors,
                                                                             marker_size = marker_size,
                                                                             marker_opacity = marker_opacity,
                                                                             y_label_font_size = y_label_font_size,
                                                                             y_tick_font_size = y_tick_font_size,
                                                                             x_label_font_size = tmp_x_label_font_size,
                                                                             x_tick_font_size = x_tick_font_size,
                                                                             legend_font_size = legend_font_size,
                                                                             show_legend = F)
        }
      }
    }
    plot = plotly::subplot(plot_list, nrows = length(factors), shareX = F, shareY = F, titleX = TRUE, titleY = TRUE)
  } else {
    plot = create_factor_density_plot(factor_table = factor_table,
                                      factor_name = factors,
                                      color_palette = colors,
                                      area_alpha = area_alpha,
                                      y_label_font_size = y_label_font_size,
                                      y_tick_font_size = y_tick_font_size,
                                      x_label_font_size = x_label_font_size,
                                      x_tick_font_size = x_tick_font_size,
                                      legend_font_size = legend_font_size,
                                      show_legend = legend_show)
  }


  plot = plotly::layout(
    p = plot,
    title = list(text = title,
                 font = list(size = title_font_size)),
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )

  return(list(plot = plot,
              table = factor_table))
}


#----------------------------------------------------- MOFA feature weights ----
plot_feature_weights = function(model, omics, feature_metadata, factors = 1, groups = NULL, scale = T, abs = F, color_palette = 'Spectral', reverse_color_palette = T, marker_size = 10, marker_opacity = 1, title_font_size = 10, y_label_font_size = 10, x_label_font_size = 10, y_tick_font_size = 10, x_tick_font_size = 10, legend_font_size = 10, width = NULL, height = NULL) {

  # Process fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  legend_show = base::ifelse(legend_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, "Weight", "")
  y_axis_title = base::ifelse(y_label_font_size > 0, "Rank", "")
  title = base::ifelse(title_font_size > 0, paste0('Feature weights - Omics: ', omics, ', Factor: ', factors), "")

  factors = MOFA2:::.check_and_get_factors(model, factors)
  factor_table = MOFA2::get_weights(model, views = omics, factors = factors,
                                    as.data.frame = F)
  factor_table = as.data.frame(factor_table[[1]])

  if (scale && sum(factor_table[,factors] > 0) > 0) {
    factor_table[,factors] = factor_table[,factors]/max(abs(factor_table[,factors]))
  }

  if (!is.null(groups)) {
    if (groups == "factor_weights") {
      factor_table$group = factor_table[,factors]
    } else if(groups %in% colnames(feature_metadata)) {
      factor_table$group = feature_metadata[rownames(factor_table), groups]
    } else {
      factor_table$group = "None"
    }
  }


  if (abs) {
    factor_table[,factors] = abs(factor_table[,factors])
    plot_range = c(-0.1,max(abs(factor_table[,factors])) + 0.1)
  } else {
    plot_range = c(-max(abs(factor_table[,factors])) -0.1 ,max(abs(factor_table[,factors])+0.1))
  }

  factor_table$rank = base::rank(factor_table[,factors])

  group_values = factor_table$group
  unique_groups = sort(unique(group_values))

  color_object = get_color_palette(groups = group_values,
                                   color_palette = color_palette,
                                   reverse_color_palette = reverse_color_palette)

  if (groups != 'factor_weights') {
    factor_table$hover = paste0(rownames(factor_table), '\nRank: ', factor_table$rank, '\nWeight: ', round(factor_table[[factors]], 4), '\nGroup: ', factor_table$group)
  } else {
    factor_table$hover = paste0(rownames(factor_table), '\nRank: ', factor_table$rank, '\nWeight: ', round(factor_table[[factors]], 4))
  }


  if (typeof(color_object) == 'list') {
    fig = plotly::plot_ly(x = factor_table[,factors],
                          y = factor_table$rank,
                          type  = "scatter",
                          mode = "markers",
                          marker = list(color = factor_table$group,
                                        colorscale = color_object,
                                        colorbar = list(title = groups,
                                                        tickfont = list(size = legend_font_size)),
                                        showscale = legend_show,
                                        size = marker_size,
                                        opacity = marker_opacity),
                          text = factor_table$hover,
                          hoverinfo = 'text',
                          width = width,
                          height = height)
  } else if (typeof(color_object) == 'character') {
    fig = plotly::plot_ly(width = width, height = height)
    for (group in unique_groups) {
      group_data = factor_table[factor_table$group == group, ]
      fig = plotly::add_trace(p = fig,
                              x = group_data[,factors],
                              y = group_data[,"rank"],
                              type = "scatter",
                              mode = "markers",
                              marker = list(size = marker_size,
                                            opacity = marker_opacity,
                                            color = color_object[as.character(group)]),
                              name = group,
                              text = group_data$hover,
                              hoverinfo = 'text',
                              showlegend = legend_show)
    }

  }



  fig = plotly::layout(
    p = fig,
    title = list(text = title,
                 font = list(size = title_font_size)),

    xaxis = list(
      title = list(
        text = x_axis_title,
        font = list(size = x_label_font_size)
      ),
      showticklabels = xtick_show,
      showline = TRUE,
      range = plot_range,
      tickfont = list(size = x_tick_font_size)
    ),
    yaxis = list(
      title = list(
        text = y_axis_title,
        font = list(size = y_label_font_size)
      ),
      showticklabels = ytick_show,
      showline = TRUE,
      tickfont = list(size = y_tick_font_size)
    ),
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )

  return(list(plot = fig,
              table = factor_table))
}


#------------------------------------------------- MOFA feature top weights ----
plot_feature_top_weights = function(model, omics, feature_table, factors = 1, nfeatures = 20, abs = TRUE, scale = TRUE, sign = "all", groups, color_palette = 'Spectral', reverse_color_palette = F, marker_size = 10, marker_opacity = 1, title_font_size, y_label_font_size, x_label_font_size, y_tick_font_size, x_tick_font_size, legend_font_size, width = NULL, height = NULL) {

  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  legend_show = base::ifelse(legend_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, "Factor weight", "")
  y_axis_title = base::ifelse(y_label_font_size > 0, "Features", "")
  title = base::ifelse(title_font_size > 0, paste0('Omics: ', omics, ', Factor: ', factors), "")


  # Checks
  if (!(sign %in% c('all', 'negative', 'positive'))){
    stop('sign should be one of all, negative or positive')
  }


  if (nfeatures <= 0) {
    stop("'nfeatures' has to be greater than 0")
  }
  if (sign == "all") {
    abs = TRUE
  }

  factors = MOFA2:::.check_and_get_factors(object = model, factors = factors)
  factor_table = MOFA2::get_weights(object = model, factors = factors, views = omics,
                                    as.data.frame = F)
  factor_table = as.data.frame(factor_table[[1]])

  if (scale) {
    factor_table[,factors] = factor_table[,factors]/max(abs(factor_table[,factors]))
  }

  factor_table = factor_table[factor_table[,factors] != 0, ,drop = FALSE]
  factor_table$sign = base::ifelse(factor_table[,factors] > 0, "positive", "negative")

  if (sign == "positive") {
    factor_table = factor_table[factor_table[,factors] > 0, ,drop = FALSE]
  } else if (sign == "negative") {
    factor_table = factor_table[factor_table[,factors] < 0, ,drop = FALSE]
  }

  if (abs) {
    factor_table$value = abs(factor_table[,factors])
  } else {
    factor_table$value = factor_table[,factors]
  }

  factor_table = factor_table[order(-factor_table$value), ]
  factor_table$rank = 1:nrow(factor_table)

  rownames(factor_table) = base::gsub(paste0('_', omics), '', rownames(factor_table))

  if (groups == 'sign') {
    factor_table$groups = factor_table$sign
    factor_table$hover = paste0(rownames(factor_table), '\nRank: ', factor_table$rank, '\nSign: ', factor_table$sign, '\nWeight: ',  round(factor_table[[factors]],4))
  } else {
    factor_table$groups = feature_table[rownames(factor_table), groups]
    factor_table$hover = paste0(rownames(factor_table), '\nRank: ', factor_table$rank, '\nSign: ', factor_table$sign, '\nWeight: ',  round(factor_table[[factors]],4), '\n', groups, ': ', factor_table$groups)
  }

  # Keep top features
  if (nfeatures > nrow(factor_table)) {
    nfeatures = nrow(factor_table)
    base::warning(paste0("Requested more features than available, displayed: ", nfeatures))
  } else {
    factor_table = factor_table[1:nfeatures,]
  }


  group_values = factor_table$groups
  unique_groups = sort(unique(group_values))
  if (groups == 'sign') {
    color_object = c("#FF0000", "#0000FF")
    color_object = setNames(color_object, c('positive', 'negative'))
  } else {
    color_object = get_color_palette(groups = group_values,
                                     color_palette = color_palette,
                                     reverse_color_palette = reverse_color_palette)
  }

  min_value = min(factor_table$value)

  if (typeof(color_object) == 'character') {
    plot = plotly::plot_ly(width = width, height = height)
    for (group in unique_groups) {
      group_data = factor_table[factor_table$groups == group, ]
      plot = plotly::add_segments(
        p = plot,
        x = rep(min_value, nrow(group_data)),
        y = rownames(group_data),
        xend = group_data$value,
        yend = rownames(group_data),
        line = list(color = color_object[as.character(group)]),
        legendgroup = group,
        name = group,
        showlegend = F
      )
      plot = plotly::add_trace(
        p = plot,
        type = 'scatter',
        x = group_data$value,
        y = rownames(group_data),
        mode = 'markers',
        marker = list(size = marker_size,
                      opacity = marker_opacity,
                      color = color_object[as.character(group)]),
        legendgroup = group,
        text = group_data$hover,
        hoverinfo = 'text',
        name = group
      )
    }
  } else if (typeof(color_object) == 'list') {
    plot = plotly::plot_ly(width = width, height = height)
    plot = plotly::add_segments(
      p = plot,
      x = rep(min_value, nrow(factor_table)),
      y = rownames(factor_table),
      xend = factor_table$value,
      yend = rownames(factor_table),
      line = list(color = "gray"),
      showlegend = F
    )
    plot = plotly::add_trace(
      p = plot,
      type = 'scatter',
      x = factor_table$value,
      y = rownames(factor_table),
      mode = 'markers',
      marker = list(color = factor_table$group,
                    colorscale = color_object,
                    colorbar = list(title = groups),
                    showscale = legend_show,
                    size = marker_size,
                    opacity = marker_opacity),
      text = factor_table$hover,
      hoverinfo = 'text'
    )
  }

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
      tickfont = list(size = x_tick_font_size)
    ),
    yaxis = list(
      title = list(
        text = y_axis_title,
        font = list(size = y_label_font_size)
      ),
      showticklabels = ytick_show,
      tickfont = list(size = y_tick_font_size),
      automargin = TRUE,
      categoryorder = 'array',
      categoryarray = rev(rownames(factor_table))
    ),
    showlegend = legend_show,
    legend = list(font = list(
      size = legend_font_size
    )
    ),
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )
  return(list(plot = plot,
              table = factor_table))
}


#------------------------------------------------------------- MOFA heatmap ----
plot_mofa_heatmap = function(model, omics, samples_annotation_table, features_annotation_table, factor = 1, features = 50, feature_annotations = NULL, sample_annotations = NULL, imputed = FALSE, denoise = FALSE, distance_method = 'euclidean', clustering_method = 'ward.D2', p_minkowski = 2, k_clusters_samples = 1, k_clusters_features = 1, center = F, apply_clustering = TRUE, color_palette = 'Spectral', reverse_color_palette = F, title_font_size = 0, y_label_font_size = 17, x_label_font_size = 17, y_tick_font_size = 0, x_tick_font_size = 0, width = NULL, height = NULL) {

  # Fonts
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  x_axis_title = base::ifelse(x_label_font_size > 0, "Samples", "")
  y_axis_title = base::ifelse(y_label_font_size > 0, "Features", "")
  title = base::ifelse(title_font_size > 0, "MOFA heatmap", "")

  if (features <= 1) {
    stop("Select more than one features")
  }
  if (features > nrow(features_annotation_table)) {
    features = nrow(features_annotation_table)
    warning(paste0("Maximum number of features exceeded, only using ", features))
  }

  factor = MOFA2:::.check_and_get_factors(model, factor)
  factor_table = MOFA2::get_weights(object = model,
                                    views = omics,
                                    factors = factor,
                                    as.data.frame = FALSE)
  factor_table = as.data.frame(factor_table[[1]])

  sample_values = MOFA2::get_factors(model)[[1]][,factor,drop = F]
  sample_values = sample_values[!is.na(sample_values), ,drop = F]



  if (denoise) {
    data = MOFA2::predict(model, views = omics, groups = 'all')[[omics]][[1]]
  } else if(imputed) {
    model = MOFA2::impute(object = model,
                          views = omics,
                          groups = 'all',
                          factors = 'all')
    data = MOFA2::get_imputed_data(object = model, view = omics, groups = 'all')[[omics]][[1]]
  } else {
    data = MOFA2::get_data(object = model, views = omics, groups = 'all')[[omics]][[1]]
  }


  factor_table$abs_value = abs(factor_table[,factor])
  factor_table = factor_table[order(-abs(factor_table$abs_value)), ]
  top_features = rownames(factor_table)[1:features]

  data = data[top_features, ]
  data = data[, rownames(sample_values)]
  data = data[, apply(data, 2, function(x) !all(is.na(x)))]

  order_samples = names(sort(sample_values[,factor], decreasing = TRUE))
  order_samples = order_samples[order_samples %in% colnames(data)]
  data = data[, order_samples]


  # Set zmax and zmin
  if (center) {
    if (min(data, na.rm = T) < 0) {
      zmax = min(c(max(data, na.rm = T), - min(data, na.rm = T)))
      zmin = -zmax
    } else {
      zmax = max(data, na.rm = T)
      zmin = min(data, na.rm = T)
    }
  } else {
    zmax = max(data, na.rm = T)
    zmin = min(data, na.rm = T)
  }


  # Annotations
  if (!is.null(sample_annotations)) {
    sample_annotations_data = samples_annotation_table[colnames(data), sample_annotations, drop = F]
  } else {
    sample_annotations_data = NULL
  }
  if (!is.null(feature_annotations)) {
    feature_annotations_data = features_annotation_table[rownames(data), feature_annotations, drop = F]
    rownames(feature_annotations_data) = base::gsub(paste0('_', omics), '', rownames(feature_annotations_data))
  } else {
    feature_annotations_data = NULL
  }
  rownames(data) = base::gsub(paste0('_', omics), '', rownames(data))


  # Set the clustering
  if (apply_clustering) {
    dendrogram_list = "both"
    Colv= stats::hclust(d = stats::dist(x = t(data),
                                        method = distance_method,
                                        p = p_minkowski),
                        method = clustering_method)
    Rowv = stats::hclust(d = stats::dist(x = data,
                                         method = distance_method,
                                         p = p_minkowski),
                         method = clustering_method)
  } else {
    dendrogram_list = "none"
    Colv = NULL
    Rowv = NULL
  }

  data_filtered = data
  data_filtered[data > zmax] = zmax
  data_filtered[data < zmin] = zmin

  color_count = colors_switch(color_palette)
  colors = get_colors(color_count = color_count, color_palette = color_palette)
  if (reverse_color_palette) {
    colors = base::rev(colors)
  }


  plot = heatmaply::heatmaply(x = data_filtered,
                              colors = colors,
                              limits = c(zmin, zmax),
                              Colv = Colv,
                              Rowv = Rowv,
                              k_col = k_clusters_samples,
                              k_row = k_clusters_features,
                              col_side_colors = sample_annotations_data,
                              row_side_colors = feature_annotations_data,
                              dendrogram = dendrogram_list,
                              xlab = x_axis_title,
                              ylab = y_axis_title,
                              main = title,
                              label_names = c("Feature", "Sample", "Value"),
                              fontsize_row = y_tick_font_size,
                              fontsize_col = x_tick_font_size,
                              showticklabels = c(xtick_show, ytick_show),
                              width = width,
                              height = height)

  plot = plotly::layout(
    p = plot,
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )

  return(list(plot = plot,
              table = data))
}
#-------------------------------------------------------- MOFA Scatter plot ----
plot_mofa_scatter_plot = function(model, factor, omics, features, sign = "all", sample_annotations, show_legend = TRUE, marker_size = 10, marker_opacity = 1, add_lm = TRUE, imputed = FALSE, fixed_axes = F, color_palette = 'Turbo', reverse_palette = F, width = NULL, height = NULL) {

  if (features > 25) {
    stop("Maximum displayed features = 25")
  }

  factor = MOFA2:::.check_and_get_factors(model, factor)

  feature_weights = as.data.frame(MOFA2::get_weights(model)[[omics]][, factor])
  colnames(feature_weights) = 'weights'

  if (imputed) {
    model = MOFA2::impute(object = model,
                          views = omics,
                          groups = 'all',
                          factors = 'all')
    sample_feature_weights = as.data.frame(model@imputed_data[[omics]][[1]])
  } else {
    sample_feature_weights = as.data.frame(model@data[[omics]][[1]])
  }

  sample_weights = MOFA2::get_factors(model, factors = factor, groups = 'all',
                                      as.data.frame = TRUE)
  sample_weights = sample_weights[, c("sample", "value")]
  colnames(sample_weights)  = c('sample', 'sample_weights')


  feature_weights$abs_weights = abs(feature_weights$weights)

  if (sign == "positive") {
    feature_weights = feature_weights[feature_weights$weights > 0,]
  } else if (sign == "negative") {
    feature_weights = feature_weights[feature_weights$weights < 0,]
  }

  feature_weights = feature_weights[order(-feature_weights$abs_weights),]

  if (features > nrow(feature_weights)) {
    warning("Requested more than the max features available, displaying all.")
    features = nrow(feature_weights)
  }

  feature_weights = feature_weights[1:features,]

  sample_weights = merge(sample_weights, MOFA2:::.set_colorby(model, sample_annotations), by = "sample")
  colnames(sample_weights) = c("sample", "sample_weights", "sample_annotations")

  if (imputed) {
    data_table = MOFA2::get_imputed_data(model, groups = 'all', views = omics, as.data.frame = TRUE)
  } else {
    data_table = MOFA2::get_data(model, groups = 'all' ,as.data.frame = TRUE)
  }
  data_table = data_table[data_table$feature %in% rownames(feature_weights),]

  data_table$sample = as.character(data_table$sample)
  data_table = dplyr::left_join(sample_weights, data_table, by = "sample")
  data_table = data_table[!is.na(data_table$value), ]

  # Get the color palette
  colors = get_color_palette(groups = data_table$sample_annotations,
                             color_palette = color_palette,
                             reverse_color_palette = reverse_palette,
                             force_scale = F,
                             force_list = F)

  data_table$hover = paste0(
    data_table$sample,
    "\n",
    "Sample factor weight: ",
    round(data_table$sample_weights,1),
    '\n',
    "Feature measurement value: ",
    round(data_table$value,1)
  )


  subplot_shape = calculate_subplot_grid_dimensions(total_plots = features)

  font_size_regression = stats::lm(y ~ x, data = data.frame(x = c(1,4),
                                                            y = c(16,9)))
  box_y0_regression = stats::lm(y ~ x, data = data.frame(x = c(1,4),
                                                         y = c(0.90,0.83)))

  font_size = unname(stats::predict(font_size_regression, data.frame(x = subplot_shape$rows)))
  box_y0 = unname(stats::predict(box_y0_regression, data.frame(x = subplot_shape$rows)))

  min_y = round(min(data_table$value, na.rm = T)) * 1.4
  max_y = round(max(data_table$value, na.rm = T)) * 1.4
  min_x = round(min(data_table$sample_weights, na.rm = T)) * 1.4
  max_x = round(max(data_table$sample_weights, na.rm = T)) * 1.4
  x_range = c(min_x, max_x)
  y_range = c(min_y, max_y)
  mid_val = (min_x + max_x) / 2

  plot_list = list()

  data_table$feature = gsub(paste0('_', omics), '', data_table$feature)

  for (feature in unique(data_table$feature)){
    feature_data = data_table[data_table$feature == feature, ]

    if (add_lm) {
      regression_model = stats::lm(feature_data$value ~ feature_data$sample_weights)
      confidence_interval = stats::predict(regression_model, interval = "confidence")
      correlation = stats::cor.test(feature_data$sample_weights, feature_data$value,
                                    method = "pearson")
      corr = base::round(correlation$estimate, 2)
      p_val = base::format(correlation$p.value, scientific = TRUE, digits = 3)
    }


    if (!fixed_axes) {
      x_range = NULL
      y_range = NULL
      mid_val = (min(feature_data$sample_weights, na.rm = T) + max(feature_data$sample_weights, na.rm = T)) / 2
    }

    feature_plot = plotly::plot_ly(width = width, height = height)

    if (add_lm) {
      feature_plot = plotly::add_trace(
        p = feature_plot,
        y = fitted(regression_model),
        x = feature_data$sample_weights,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'blue'),
        showlegend = F
      )

      feature_plot = plotly::add_ribbons(
        p = feature_plot,
        ymin = confidence_interval[, "lwr"],
        ymax = confidence_interval[, "upr"],
        x = feature_data$sample_weights,
        line = list(color = 'transparent'),
        fillcolor = 'rgba(173,216,230, 0.2)',
        showlegend = F
      )
    }


    for (group in unique(feature_data$sample_annotations)){
      group_data = feature_data[feature_data$sample_annotations == group,]
      feature_plot = plotly::add_trace(
        p = feature_plot,
        x = group_data$sample_weights,
        y = group_data$value,
        marker = list(size = marker_size,
                      color = colors[group],
                      opacity = marker_opacity,
                      line = list(width = 0.5, color = 'white')),
        name = group,
        type  = "scatter",
        mode = "markers",
        text = group_data$hover,
        hoverinfo = "text",
        legendgroup=group,
        showlegend = show_legend
      )
    }
    show_legend = F
    feature_plot = plotly::layout(
      p = feature_plot,
      xaxis = list(
        showline = TRUE,
        linecolor = 'black',
        linewidth = 2,
        mirror = T,
        range = x_range
      ),
      yaxis = list(
        domain = c(0, 0.95),
        showline = TRUE,
        linecolor = 'black',
        linewidth = 2,
        mirror = T,
        range = y_range
      ),
      shapes = list(
        list(
          type = "rect",
          xref = "paper",
          yref = "paper",
          x0 = 0,
          y0 = box_y0,
          x1 = 1,
          y1 = 0.95,
          fillcolor = "#ff7f0e",
          opacity = 1,
          line = list(
            width = 0
          )
        )
      ),
      annotations = list(
        list(
          text = feature,
          x = mid_val,
          y = 1.5 * max(feature_data$value),
          showarrow = F,
          xref = "x",
          yref = "y",
          font = list(
            family = "Arial, sans-serif",
            size = font_size,
            color = "black"
          ),
          bgcolor = "transparent",
          bordercolor = "transparent"
        )
      )
    )

    if (add_lm) {
      feature_plot = plotly::layout(
        p = feature_plot,
        annotations = list(
          list(
            text = paste0('R = ',
                          corr,
                          '\n',
                          'p-val < ',
                          p_val),
            x = min(feature_data$sample_weights) + abs(min(feature_data$sample_weights)*0.4),
            xref = 'x',
            y = max(feature_data$value),
            yref = 'y',
            showarrow = FALSE,
            font = list(size = font_size * 0.8)
          )
        )
      )
    }


    plot_list[[length(plot_list) + 1]] = feature_plot
  }

  plot = plotly::subplot(plot_list,
                         nrows = subplot_shape$rows,
                         shareX = F,
                         shareY = F,
                         titleX = TRUE,
                         titleY = TRUE)

  return(list(plot = plot,
              table = data_table))

}
#------------------------------------------------------------- Volcano plot ----
volcano_main = function(fc_vals = volcano_table$fold_change,
                        p_vals = volcano_table$q_val_bh,
                        names = rownames(volcano_table),
                        y_label = '-Log10(p-value)',
                        left_label = 'Left',
                        right_label = 'Right',
                        groups = NULL,
                        displayed_plot = 'main',
                        color_palette = 'Spectral',
                        reverse_palette = F,
                        p_val_threshold = 0.05,
                        fc_threshold = 2,
                        marker_size = 6,
                        title_font_size = 16,
                        y_label_font_size = 20,
                        y_tick_font_size = 15,
                        x_label_font_size = 20,
                        x_tick_font_size = 15,
                        legend_font_size = 15,
                        opacity = 1) {

  # Checks
  if (!(displayed_plot %in% c('main', 'all', 'left', 'right', 'top'))) {
    stop("displayed_plot should be one of ['main', 'all', 'left', 'right', 'top']")
  }

  data = data.frame(
    "fold_change" = fc_vals,
    "p_values" = p_vals,
    "names" = names
  )

  if(nchar(left_label) != nchar(right_label)) {
    if(nchar(left_label) > nchar(right_label)) {
      num = nchar(left_label) - nchar(right_label)
      right_label = paste0(right_label, "&nbsp;&nbsp;&nbsp;&#8658;", paste(rep("&nbsp;", num), collapse = ""))
      left_label = paste0("&#8656;&nbsp;&nbsp;&nbsp;", left_label)
    } else {
      num = nchar(right_label) - nchar(left_label)
      right_label = paste0(right_label, "&nbsp;&nbsp;&nbsp;&#8658;")
      left_label = paste0(paste(rep("&nbsp;", num), collapse = ""), "&#8656;&nbsp;&nbsp;&nbsp;", left_label)
    }
  } else {
    right_label = paste0(right_label, "&nbsp;&nbsp;&nbsp;&#8658;")
    left_label = paste0("&#8656;&nbsp;&nbsp;&nbsp;", left_label)
  }
  plot_label = paste0(left_label, ' - ', right_label)

  # Format data
  data$log2_fold_change = log2(data$fold_change)
  data$log10_p_values = -log10(data$p_values)



  if (is.null(groups)) {
    data$groups = 'Inconclusive'
    data$groups[(data$p_values > p_val_threshold) & (data$log2_fold_change < log2(fc_threshold)) & (data$log2_fold_change > -log2(fc_threshold))] = 'Not significant'
    data$groups[((data$p_values < p_val_threshold) | (is.na(data$p_values))) & (data$log2_fold_change > log2(fc_threshold))] = "Overexpressed"
    data$groups[((data$p_values < p_val_threshold) | (is.na(data$p_values))) & (data$log2_fold_change < -log2(fc_threshold))] = "Underexpressed"

    # Add count data
    replacement_vector = table(data$groups)
    original_names = names(replacement_vector)
    replacement_vector = paste0(names(replacement_vector), ' (', replacement_vector, ')')
    names(replacement_vector) = original_names
    data$groups = replacement_vector[as.character(data$groups)]


    color_object = setNames(c('#787878', '#bebebe', '#FF0000', '#0000FF'), sort(unique( data$groups)))
    data$color = unname(color_object[data$groups])

  } else {
    data$groups = groups

    # Add count data
    replacement_vector = table(data$groups)
    original_names = names(replacement_vector)
    replacement_vector = paste0(names(replacement_vector), ' (', replacement_vector, ')')
    names(replacement_vector) = original_names
    color_object = get_color_palette(groups = data$groups,
                                     color_palette = color_palette,
                                     reverse_color_palette = reverse_palette,
                                     force_list = T
                                     )
    data$color = color_object[as.character(data$groups)]
    data$groups = replacement_vector[as.character(data$groups)]
  }

  # Produce the data tables & plots
  if (length(which(is.na(data$log10_p_values))) > 0) { # Top violin
    top_data = data[which(is.na(data$log10_p_values)),]
    data = data[-which(is.na(data$log10_p_values)),]
    inf_idx = which(base::is.infinite(top_data$log2_fold_change))
    if (length(inf_idx) > 0) {
      top_data = top_data[-inf_idx, drop = F]
      warning(paste0('Dropped ', length(inf_idx), ' features with no p-values & infinite FC.'))
    }
    if (ncol(top_data) > 0) {
      top_violin = plot_volcano_violin(data = top_data,
                                       threshold = log2(fc_threshold),
                                       side = 'top',
                                       opacity = opacity,
                                       marker_size = marker_size,
                                       x_label_font_size = x_label_font_size,
                                       y_label_font_size = y_label_font_size,
                                       legend_font_size = 0)
    } else {
      top_data = NULL
    }


  } else {top_data = NULL}

  if (length(which(data$log2_fold_change == -Inf)) > 0) { # Left violin
    left_data = data[which(data$log2_fold_change == -Inf),]
    data = data[-which(data$log2_fold_change == -Inf),]

    left_violin = plot_volcano_violin(data = left_data,
                                      threshold = -log10(p_val_threshold),
                                      side = 'left',
                                      opacity = opacity,
                                      marker_size = marker_size,
                                      x_label_font_size = x_label_font_size,
                                      y_label_font_size = y_label_font_size,
                                      legend_font_size = 0)

  } else {left_data = NULL}

  if (length(which(data$log2_fold_change == Inf)) > 0) { # right violin
    right_data = data[which(data$log2_fold_change == Inf),]
    data = data[-which(data$log2_fold_change == Inf),]

    right_violin = plot_volcano_violin(data = right_data,
                                       threshold = -log10(p_val_threshold),
                                       side = 'right',
                                       opacity = opacity,
                                       marker_size = marker_size,
                                       x_label_font_size = x_label_font_size,
                                       y_label_font_size = y_label_font_size,
                                       legend_font_size = 0)

  } else {right_data = NULL}



  # Main plot y_label
  main_plot = plot_volcano(data = data,
                           label = plot_label,
                           marker_size = marker_size,
                           p_val_threshold = p_val_threshold,
                           fc_threshold = fc_threshold,
                           opacity = opacity,
                           y_axis_title = y_label,
                           title_font_size = title_font_size,
                           y_label_font_size = 0,
                           y_tick_font_size = y_tick_font_size,
                           x_label_font_size = x_label_font_size,
                           x_tick_font_size = x_tick_font_size,
                           legend_font_size = legend_font_size)

  # Blank plot
  blank_plot = create_blank_plot()




  if (is.null(left_data) & is.null(right_data) & is.null(top_data) | (displayed_plot == 'main')) { # Only main
    out_plot = main_plot
    out_plot = plotly::layout(out_plot,
                              yaxis = list(title = list(text = ifelse(y_label_font_size == 0, "", y_label),
                                                        font = list(size = y_label_font_size))))

  } else if (!is.null(top_data) & (displayed_plot == 'top')) { # Export top violin

    out_plot = top_violin

  } else if (!is.null(left_data) & (displayed_plot == 'left')) { # Export left violin

    out_plot = left_violin

  } else if (!is.null(right_data) & (displayed_plot == 'right')) { # Export right violin

    out_plot = right_violin

  } else if (is.null(left_data) & is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # Top only

    out_plot = plotly::subplot(
      top_violin,
      main_plot,
      nrows = 2,
      shareX = TRUE,
      heights = c(0.1, 0.9)
    )

  } else if (!is.null(left_data) & is.null(right_data) & is.null(top_data) & (displayed_plot == 'all')) { # Left only

    out_plot = plotly::subplot(
      list(left_violin, main_plot),
      shareX = TRUE,
      shareY = TRUE,
      titleX = T,
      titleY = F,
      widths = c(0.1, 0.9)
    )

  } else if (is.null(left_data) & !is.null(right_data) & is.null(top_data) & (displayed_plot == 'all')) { # Right only

    out_plot = plotly::subplot(
      list(main_plot, right_violin),
      shareX = TRUE,
      shareY = TRUE,
      titleX = T,
      titleY = F,
      widths = c(0.9, 0.1)
    )

  } else if (!is.null(left_data) & !is.null(right_data) & is.null(top_data) & (displayed_plot == 'all')) { # Left and Right

    out_plot = plotly::subplot(
      matrix(list(left_violin, main_plot, right_violin),
             ncol = 3, byrow = TRUE),
      shareX = TRUE,
      shareY = TRUE,
      titleX = T,
      titleY = F,
      widths = c(0.1, 0.8, 0.1)
    )

    out_plot = plotly::layout(out_plot,
                              yaxis = list(title = y_label),
                              yaxis = list(title = list(text = y_label,
                                                        font = list(size = y_label_font_size)),
                                           showticklabels = ifelse(y_tick_font_size == 0, F, T),
                                           tickfont = list(size = y_tick_font_size)))
  } else if (!is.null(left_data) & !is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # All violins

    out_plot = plotly::subplot(
      list(blank_plot, top_violin, blank_plot,
           left_violin, main_plot, right_violin),
      nrows = 2,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.02,
      widths = c(0.1, 0.8, 0.1),
      heights = c(0.1, 0.9)
    )
  } else if (!is.null(left_data) & is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # Top and left

    out_plot = plotly::subplot(
      list(blank_plot, top_violin,
           left_violin, main_plot),
      nrows = 2,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.02,
      widths = c(0.1, 0.9),
      heights = c(0.1, 0.9)
    )

  } else if (is.null(left_data) & !is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # Top and right

    out_plot = plotly::subplot(
      list(top_violin, blank_plot,
           main_plot, right_violin),
      nrows = 2,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.02,
      widths = c(0.9, 0.1),
      heights = c(0.1, 0.9)
    )
  } else {
    warning('Selected plot unavailable, returning blank.')
    out_plot = blank_plot
  }

  out_plot = plotly::layout(
    p = out_plot,
    plot_bgcolor='rgba(0,0,0,0)',
    paper_bgcolor='rgba(0,0,0,0)'
  )

  return(out_plot)

}

plot_volcano_violin = function(
    data,
    threshold,
    side,
    opacity = 1,
    marker_size = 6,
    x_label_font_size = 20,
    y_label_font_size = 20,
    legend_font_size = 0) {

  if (!(side %in% c('left', 'right', 'top'))) {
    stop('side must be in [left, right, top]')
  }

  show_legend = base::ifelse(legend_font_size == 0, F, T)
  label = paste0(side, ' only')
  label = base::ifelse(x_label_font_size == 0, "", label)

  if (side == 'left') {
    col_line = 'blue'
    col_fill = 'lightblue'
  } else if (side == 'right') {
    col_line = 'red'
    col_fill = 'pink'
  } else if (side == 'top') {
    return(plot_volcano_violin_top(data = data,
                                   threshold = threshold,
                                   opacity = opacity,
                                   marker_size = marker_size,
                                   x_label_font_size = x_label_font_size,
                                   y_label_font_size = y_label_font_size,
                                   legend_font_size = legend_font_size))
  }

  p = plotly::plot_ly()

  if (length(data$log10_p_values[which(data$log10_p_values >= threshold)]) > 1) {
    sub_data = data[which(data$log10_p_values >= threshold),]
    p = plotly::add_trace(p,
                          y = sub_data$log10_p_values,
                          x = label, type = "violin",
                          box = list(visible = FALSE),
                          line = list(color = col_line),
                          fillcolor = col_fill,
                          meanline = list(visible = F),
                          opacity = opacity,
                          points = FALSE,
                          name = sub_data$groups[1],
                          legendgroup = sub_data$groups[1],
                          hoverinfo = 'none',
                          showlegend = F)
  }

  if (length(data$log10_p_values[which(data$log10_p_values < threshold)]) > 1) {
    sub_data = data[which(data$log10_p_values < threshold),]
    p = plotly::add_trace(p,
                          y = sub_data$log10_p_values,
                          x = label, type = "violin",
                          box = list(visible = FALSE),
                          line = list(color = 'darkgray'),
                          fillcolor = 'lightgray',
                          meanline = list(visible = F),
                          opacity = opacity,
                          points = FALSE,
                          name = sub_data$groups[1],
                          legendgroup = sub_data$groups[1],
                          hoverinfo = 'none',
                          showlegend = F)
  }

  for (group in unique(data$groups)) {
    group_table = data[data$groups == group,]
    p = plotly::add_trace(p,
                          type = "scatter",
                          mode = "markers",
                          y = group_table$log10_p_values,
                          x = label,
                          marker = list(size = marker_size,
                                        color = group_table$color[1], #### HERE!!!
                                        opacity = opacity,
                                        line = list(width = 0.5, color = 'white')),
                          name = group,
                          legendgroup = group,
                          text = group_table$names,
                          hoverinfo = 'text',
                          showlegend = F) #
  }
  p = plotly::layout(p,

                     xaxis = list(title = list(font = list(size = x_label_font_size))
                                  ),
                     shapes = list(
                       list(
                         type = "line",
                         x0 = -0.3,
                         x1 = 0.3,
                         y0 = threshold,
                         y1 = threshold,
                         line = list(color = "black", width = 1, dash = "dot")
                       )
                     )
  )

  return(p)

}

plot_volcano_violin_top = function(data,
                                   threshold,
                                   opacity,
                                   marker_size,
                                   x_label_font_size,
                                   y_label_font_size,
                                   legend_font_size) {

  xlabel = base::ifelse(x_label_font_size == 0, '', 'Log2(Fold Change)')
  ylabel = base::ifelse(y_label_font_size == 0, '', 'No p-value')
  show_legend = base::ifelse(legend_font_size == 0, F, T)

  p = plotly::plot_ly()

  p = plotly::add_trace(p,
                        y = ylabel,
                        x = data$log2_fold_change,
                        type = "violin",
                        box = list(visible = FALSE),
                        line = list(color = 'darkgray'),
                        fillcolor = 'ghostwhite',
                        meanline = list(visible = F),
                        opacity = opacity,
                        points = FALSE,
                        name = 'Not significant',
                        legendgroup = 'Not significant',
                        hoverinfo = 'none',
                        showlegend = F, #
                        orientation = 'h')

  for (group in unique(data$groups)) {
    group_table = data[data$groups == group,]

    p = plotly::add_trace(p,
                          type = "scatter",
                          mode = "markers",
                          y = 'No p-value',
                          x = group_table$log2_fold_change,
                          marker = list(size = marker_size, color = group_table$color[1], opacity = opacity, line = list(width = 0.5, color = 'white')), # HERE!!
                          name = group,
                          legendgroup = group,
                          text = group_table$names,
                          hoverinfo = 'text',
                          showlegend = F) 

  }

  p = plotly::layout(p,
                     xaxis = list(title = list(text = xlabel,
                                               font = x_label_font_size)
                     ),
                     yaxis = list(title = list(text = ylabel,
                                               font = y_label_font_size)
                     ),
                     shapes = list(
                       # Vertical line at x = -1
                       list(
                         type = "line",
                         x0 = -threshold,
                         x1 = -threshold,
                         y0 = -0.3,
                         y1 = 0.3,
                         line = list(color = "black", width = 1, dash = "dot")
                       ),
                       # Vertical line at x = 1
                       list(
                         type = "line",
                         x0 = threshold,
                         x1 = threshold,
                         y0 = -0.3,
                         y1 = 0.3,
                         line = list(color = "black", width = 1, dash = "dot")
                       )
                     ))
  return(p)
}

plot_volcano = function(data, label = NULL, marker_size, p_val_threshold = 0.05, fc_threshold = 2, opacity = 1, y_axis_title = '-Log10(p-value)', title_font_size = 16, y_label_font_size = 20, y_tick_font_size = 15, x_label_font_size = 20, x_tick_font_size = 15, legend_font_size = 15) {

  label = base::ifelse(title_font_size == 0, "", label)
  y_axis_title = base::ifelse(y_label_font_size == 0, "", y_axis_title)
  x_axis_title = base::ifelse(x_label_font_size == 0, "", "Log2(Fold Change)")
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  legend_show = base::ifelse(legend_font_size > 0, T, F)

  main_plot = plotly::plot_ly()

  for (group in unique(data$groups)) {
    subset_data = data[data$groups == group, ]
    main_plot = plotly::add_trace(
      p = main_plot,
      x = subset_data$log2_fold_change,
      y = subset_data$log10_p_values,
      text = subset_data$names,
      type = "scatter",
      mode = "markers",
      marker = list(size = marker_size,
                    opacity = opacity,
                    color = subset_data$color[1],
                    line = list(width = 0.5, color = 'white')),
      legendgroup = group,
      name = group,
      showlegend = legend_show,
      hoverinfo = 'text'
    )
  }

  main_plot = plotly::layout(main_plot,
                             title = list(text = label,
                                          xref = "paper",
                                          font = list(size = title_font_size)),
                             xaxis = list(title = list(text = x_axis_title,
                                                       font = list(size = x_label_font_size)),
                                          showticklabels = xtick_show,
                                          tickfont = list(size = x_tick_font_size),
                                          zeroline = T,
                                          range = c(-ceiling(max(abs(data$log2_fold_change))),
                                                    ceiling(max(abs(data$log2_fold_change)))
                                          )
                             ),
                             yaxis = list(title = list(text = y_axis_title,
                                                       font = list(size = y_label_font_size)),
                                          showticklabels = ytick_show,
                                          tickfont = list(size = y_tick_font_size)
                             ),
                             hovermode = "closest",
                             shapes = list(
                               # Vertical line at x = -1
                               list(
                                 type = "line",
                                 x0 = -log2(fc_threshold),
                                 x1 = -log2(fc_threshold),
                                 y0 = 0,
                                 y1 = max(data$log10_p_values),
                                 line = list(color = "black", width = 1, dash = "dot")
                               ),
                               # Vertical line at x = 1
                               list(
                                 type = "line",
                                 x0 = log2(fc_threshold),
                                 x1 = log2(fc_threshold),
                                 y0 = 0,
                                 y1 = max(data$log10_p_values),
                                 line = list(color = "black", width = 1, dash = "dot")
                               ),
                               # Horizontal line at y = -log10(0.05)
                               list(
                                 type = "line",
                                 x0 = -round(max(abs(data$log2_fold_change))),
                                 x1 = round(max(abs(data$log2_fold_change))),
                                 y0 = -log10(p_val_threshold),
                                 y1 = -log10(p_val_threshold),
                                 line = list(color = "black", width = 1, dash = "dot")
                               )

                             ),
                             legend = list(font = list(size = legend_font_size))
  )
  return(main_plot)
}

#----------------------------------------------------------------- PCA plot ----
pca_main = function(data_table, sample_groups = NULL, feature_groups = NULL, nPcs = 2, displayed_pc_1 = 1, displayed_pc_2 = 2, pca_method = 'svd', completeObs = T, displayed_plots = "both", colors_palette = "Spectral", marker_size = 5, opacity = 1, title_font_size = 10, y_label_font_size = 5, y_tick_font_size = 2, x_label_font_size = 5, x_tick_font_size = 2, legend_font_size = 2, return_data = FALSE, width = NULL, height = NULL) {

  # Check if arguments are valid
  if (!(pca_method %in% pcaMethods::listPcaMethods())){
    print(paste0('Invalid pca_method: must be one of [', paste(pcaMethods::listPcaMethods(), collapse = ', '), '], defaulting to svd'))
    pca_method = 'svd'
  }

  if ((pca_method %in% c('robustPca', 'nlpca', 'llsImpute'))){
    print(paste0(pca_method, ' is not currently supported, defaulting to svd'))
    pca_method = 'svd'
  }

  if (!(displayed_plots %in% c('both', 'loadings', 'scores', 'variance'))){
    print('Error: displayed_plots must be in both, loadings or scores')
    return()
  }

  if (max(c(displayed_pc_1, displayed_pc_2)) > nPcs) {
    print('At least one displayed PC outside of nPcs range, adjusting nPcs')
    nPcs = max(c(displayed_pc_1, displayed_pc_2))
  }

  if (displayed_pc_1 == displayed_pc_2) {
    print('displayed PCs are the same, defaulting to PC1 and PC2')
    displayed_pc_1 = 1
    displayed_pc_2 = 2
  }

  # Apply PCA
  pca_data = pcaMethods::pca(object = data_table,
                             method = pca_method,
                             nPcs = nPcs,
                             scale = "none",
                             cv = "q2",
                             completeObs = completeObs)

  # Plot depending on the type requested
  if (displayed_plots == 'both') {
    fig = c()
    fig[[1]] = plot_pca(x = pca_data@scores[,paste0('PC', displayed_pc_1)],
                        y = pca_data@scores[,paste0('PC', displayed_pc_2)],
                        label_1 = paste0('PC', displayed_pc_1),
                        label_2 = paste0('PC', displayed_pc_2),
                        weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                        weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                        names = rownames(data_table),
                        type = 'scores',
                        groups = sample_groups,
                        colors = colors_palette,
                        marker_size = marker_size,
                        opacity = opacity,
                        title_font_size = title_font_size,
                        y_label_font_size = y_label_font_size,
                        y_tick_font_size = y_tick_font_size,
                        x_label_font_size = x_label_font_size,
                        x_tick_font_size = x_tick_font_size,
                        legend_font_size = legend_font_size,
                        width = width,
                        height = height)


    fig[[2]] = plot_pca(x = pca_data@loadings[, paste0('PC', displayed_pc_1)],
                        y = pca_data@loadings[, paste0('PC', displayed_pc_2)],
                        label_1 = paste0('PC', displayed_pc_1),
                        label_2 = paste0('PC', displayed_pc_2),
                        weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                        weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                        names = colnames(data_table),
                        type = 'loadings',
                        groups = feature_groups,
                        colors = colors_palette,
                        marker_size = marker_size,
                        opacity = opacity,
                        title_font_size = title_font_size,
                        y_label_font_size = y_label_font_size,
                        y_tick_font_size = y_tick_font_size,
                        x_label_font_size = x_label_font_size,
                        x_tick_font_size = x_tick_font_size,
                        legend_font_size = legend_font_size,
                        width = width,
                        height = height)

    fig = plotly::subplot(fig, nrows = 1, margin = 0.035, titleX = T, titleY = T)

    fig = plotly::layout(fig,
                         title = list(text = "PCA scores and loadings",
                                      font = list(size = title_font_size)))


  } else if (displayed_plots == 'loadings'){

    fig = plot_pca(x = pca_data@loadings[, paste0('PC', displayed_pc_1)],
                   y = pca_data@loadings[, paste0('PC', displayed_pc_2)],
                   label_1 = paste0('PC', displayed_pc_1),
                   label_2 = paste0('PC', displayed_pc_2),
                   weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                   weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                   names = colnames(data_table),
                   type = 'loadings',
                   groups = feature_groups,
                   colors = colors_palette,
                   marker_size = marker_size,
                   opacity = opacity,
                   title_font_size = title_font_size,
                   y_label_font_size = y_label_font_size,
                   y_tick_font_size = y_tick_font_size,
                   x_label_font_size = x_label_font_size,
                   x_tick_font_size = x_tick_font_size,
                   legend_font_size = legend_font_size,
                   width = width,
                   height = height)

  } else if (displayed_plots == 'scores') {

    fig = plot_pca(x = pca_data@scores[,paste0('PC', displayed_pc_1)],
                   y = pca_data@scores[,paste0('PC', displayed_pc_2)],
                   label_1 = paste0('PC', displayed_pc_1),
                   label_2 = paste0('PC', displayed_pc_2),
                   weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                   weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                   names = rownames(data_table),
                   type = 'scores',
                   groups = sample_groups,
                   colors = colors_palette,
                   marker_size = marker_size,
                   opacity = opacity,
                   title_font_size = title_font_size,
                   y_label_font_size = y_label_font_size,
                   y_tick_font_size = y_tick_font_size,
                   x_label_font_size = x_label_font_size,
                   x_tick_font_size = x_tick_font_size,
                   legend_font_size = legend_font_size,
                   width = width,
                   height = height)
  } else if (displayed_plots == 'variance') {

    fig = plot_explained_variance(variance_explained = pca_data@R2,
                                  title_font_size = title_font_size,
                                  y_label_font_size = y_label_font_size,
                                  y_tick_font_size = y_tick_font_size,
                                  x_label_font_size = x_label_font_size,
                                  x_tick_font_size = x_tick_font_size,
                                  legend_font_size = legend_font_size,
                                  width = width,
                                  height = height)
  }

  if (return_data) {
    return(list(
      pca_data = pca_data,
      fig = fig))
  } else {
    return(fig)
  }


}


plot_pca = function(x, y, label_1, label_2, weight_1, weight_2, names, type, groups = NULL, colors = "Spectral", marker_size = 5, opacity = 1, title_font_size = 10, y_label_font_size = 5, y_tick_font_size = 2, x_label_font_size = 5, x_tick_font_size = 2, legend_font_size = 2, width = NULL, height = NULL) {

  if (!(type %in% c("scores", "loadings"))) {
    print('Error: type must either be scores or loadings.')
    return()
  }

  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)
  legend_show = base::ifelse(legend_font_size > 0, T, F)

  if (is.null(groups) & (type == "scores")) { # Score plot without groups (should not exist)
    data_table = data.frame(
      x = x,
      y = y,
      names = names
    )

    conf_ellipse = ellipse::ellipse(x = stats::cov(cbind(data_table$x, data_table$y)),
                                    centre = c(mean(data_table$x), mean(data_table$y)),
                                    level = 0.95)

    plot = plotly::plot_ly(data = data_table, width = width, height = height)

    plot = plotly::add_markers(plot,
                               x = ~x,
                               y = ~y,
                               text = ~names,
                               mode = "markers+text",
                               marker = list(size = marker_size,
                                             opacity = opacity,
                                             line = list(width = 0.5, color = 'white')))


    plot = plotly::add_trace(plot,
                             data = as.data.frame(conf_ellipse),
                             x = ~x,
                             y = ~y,
                             mode = "lines",
                             line = list(color = 'gray',
                                         width = 1),
                             inherit = FALSE,
                             name = 'Hotelling',
                             showlegend = TRUE,
                             type = "scatter")

    plot = plotly::layout(plot,
                          title = list(text = base::ifelse(title_font_size == 0, "", "PCA Scores Plot"),
                                       font = list(size = title_font_size)),
                          xaxis = list(
                            title = list(
                              text = base::ifelse(x_label_font_size == 0, "", paste0(label_1, ' (', weight_1 * 100, '%)')),
                              font = list(size = x_label_font_size)
                            ),
                            showticklabels = xtick_show,
                            zeroline = TRUE,
                            tickfont = list(size = x_tick_font_size)
                          ),
                          yaxis = list(
                            title = list(
                              text = base::ifelse(y_label_font_size == 0, "", paste0(label_2, ' (', weight_2 * 100, '%)')),
                              font = list(size = y_label_font_size)
                            ),
                            showticklabels = ytick_show,
                            zeroline = TRUE,
                            tickfont = list(size = y_tick_font_size)
                          ),
                          legend = list(font = list(size = legend_font_size)),
                          plot_bgcolor='rgba(0,0,0,0)',
                          paper_bgcolor='rgba(0,0,0,0)'
    )



    return(plot)

  } else if (is.null(groups) & (type == "loadings")) { # Loadings plot without groups

    data_table = data.frame(
      x = x,
      y = y,
      names = names
    )

    plot = plotly::plot_ly(data = data_table, width = width, height = height)

    plot = plotly::add_segments(plot,
                                x = 0,
                                y = 0,
                                xend = ~x,
                                yend = ~y,
                                line = list(dash = "solid"),
                                showlegend = FALSE)

    plot = plotly::add_markers(plot,
                               x = ~x,
                               y = ~y,
                               text = ~names,
                               mode = "markers+text",
                               marker = list(size = marker_size,
                                             opacity = opacity,
                                             line = list(width = 0.5, color = 'white')),
                               showlegend = FALSE)

    plot = plotly::layout(plot,
                          title = list(text = base::ifelse(title_font_size == 0, "", "PCA Loadings Plot"),
                                       font = list(size = title_font_size)),
                          xaxis = list(
                            title = list(
                              text = base::ifelse(x_label_font_size == 0, "", paste0(label_1, ' (', weight_1 * 100, '%)')),
                              font = list(size = x_label_font_size)
                            ),
                            showticklabels = xtick_show,
                            zeroline = TRUE,
                            tickfont = list(size = x_tick_font_size)
                          ),
                          yaxis = list(
                            title = list(
                              text = base::ifelse(y_label_font_size == 0, "", paste0(label_2, ' (', weight_2 * 100, '%)')),
                              font = list(size = y_label_font_size)
                            ),
                            showticklabels = ytick_show,
                            zeroline = TRUE,
                            tickfont = list(size = y_tick_font_size)
                          ),
                          legend = list(font = list(size = legend_font_size)),
                          plot_bgcolor='rgba(0,0,0,0)',
                          paper_bgcolor='rgba(0,0,0,0)'
    )

    return(plot)

  } else if (!is.null(groups) & (type == "scores")) { # Score plot with groups

    data_table = data.frame(
      x = x,
      y = y,
      names = names,
      groups = as.factor(groups)
    )

    conf_ellipse = ellipse::ellipse(x = stats::cov(cbind(data_table$x, data_table$y)),
                                    centre = c(mean(data_table$x), mean(data_table$y)),
                                    level = 0.95)

    plot = plotly::plot_ly(data = data_table, width = width, height = height)

    plot = plotly::add_markers(plot,
                               x = ~x,
                               y = ~y,
                               text = ~names,
                               mode = "markers+text",
                               marker = list(size = marker_size,
                                             opacity = opacity,
                                             line = list(width = 0.5, color = 'white')),
                               color = ~groups,
                               colors = colors,
                               legendgroup = ~groups,
                               showlegend = legend_show)

    plot = plotly::add_trace(plot,
                             data = as.data.frame(conf_ellipse),
                             x = ~x,
                             y = ~y,
                             mode = "lines",
                             line = list(color = 'gray',
                                         width = 1),
                             inherit = FALSE,
                             name = 'Hotelling',
                             showlegend = legend_show,
                             type = "scatter")


    plot = plotly::layout(plot,
                          title = list(text = base::ifelse(title_font_size == 0, "", "PCA Scores Plot"),
                                       font = list(size = title_font_size)),
                          xaxis = list(
                            title = list(
                              text = base::ifelse(x_label_font_size == 0, "", paste0(label_1, ' (', weight_1 * 100, '%)')),
                              font = list(size = x_label_font_size)
                            ),
                            showticklabels = xtick_show,
                            zeroline = TRUE,
                            tickfont = list(size = x_tick_font_size)
                          ),
                          yaxis = list(
                            title = list(
                              text = base::ifelse(y_label_font_size == 0, "", paste0(label_2, ' (', weight_2 * 100, '%)')),
                              font = list(size = y_label_font_size)
                            ),
                            showticklabels = ytick_show,
                            zeroline = TRUE,
                            tickfont = list(size = y_tick_font_size)
                          ),
                          legend = list(font = list(size = legend_font_size)),
                          plot_bgcolor='rgba(0,0,0,0)',
                          paper_bgcolor='rgba(0,0,0,0)'
    )




    return(plot)

  } else if (!is.null(groups) & (type == "loadings")) {

    data_table = data.frame(
      x = x,
      y = y,
      names = names,
      groups = as.factor(groups)
    )

    plot = plotly::plot_ly(data = data_table, width = width, height = height)

    plot = plotly::add_segments(plot,
                                x = 0,
                                y = 0,
                                xend = ~x,
                                yend = ~y,
                                line = list(dash = "solid"),
                                color = ~groups,
                                colors = colors,
                                legendgroup = ~groups,
                                showlegend = FALSE)

    plot = plotly::add_markers(plot,
                               x = ~x,
                               y = ~y,
                               text = ~names,
                               mode = "markers+text",
                               marker = list(size = marker_size,
                                             opacity = opacity,
                                             line = list(width = 0.5, color = 'white')),
                               color = ~groups,
                               colors = colors,
                               legendgroup = ~groups,
                               showlegend = legend_show)

    plot = plotly::layout(plot,
                          title = list(text = base::ifelse(title_font_size == 0, "", "PCA Loadings Plot"),
                                       font = list(size = title_font_size)),
                          xaxis = list(
                            title = list(
                              text = base::ifelse(x_label_font_size == 0, "", paste0(label_1, ' (', weight_1 * 100, '%)')),
                              font = list(size = x_label_font_size)
                            ),
                            showticklabels = xtick_show,
                            zeroline = TRUE,
                            tickfont = list(size = x_tick_font_size)
                          ),
                          yaxis = list(
                            title = list(
                              text = base::ifelse(y_label_font_size == 0, "", paste0(label_2, ' (', weight_2 * 100, '%)')),
                              font = list(size = y_label_font_size)
                            ),
                            showticklabels = ytick_show,
                            zeroline = TRUE,
                            tickfont = list(size = y_tick_font_size)
                          ),
                          legend = list(font = list(size = legend_font_size)),
                          plot_bgcolor='rgba(0,0,0,0)',
                          paper_bgcolor='rgba(0,0,0,0)'
    )

    return(plot)

  }
}

plot_explained_variance = function(variance_explained, title_font_size = 10, y_label_font_size = 5, y_tick_font_size = 2, x_label_font_size = 5, x_tick_font_size = 2, legend_font_size = 2, width, height) {

  xtick_show = base::ifelse(x_tick_font_size > 0, T, F)
  ytick_show = base::ifelse(y_tick_font_size > 0, T, F)

  if (variance_explained[1] < 1) {
    variance_explained = variance_explained * 100
  }

  cumulative_variance = base::cumsum(variance_explained)

  plot = plotly::plot_ly(x = 1:length(variance_explained),
                         y = variance_explained,
                         type = 'bar',
                         name = 'Variance Explained',
                         marker = list(color = 'lightblue'),
                         width = width,
                         height = height)

  # Add line for cumulative variance
  plot = plotly::add_trace(plot,
                           x = 1:length(variance_explained),
                           y = cumulative_variance,
                           type = 'scatter',
                           mode = 'lines+markers',
                           name = 'Cumulative Variance',
                           line = list(color = 'red'),
                           marker = list(color = 'red'))

  # Customize the layout
  plot = plotly::layout(plot,
                        title = list(text = base::ifelse(title_font_size == 0, "", "Variance Explained by Each PC"),
                                     font = list(size = title_font_size)),
                        xaxis = list(
                          title = list(
                            text = base::ifelse(x_label_font_size == 0, "", "Principal Component"),
                            font = list(size = x_label_font_size)
                          ),
                          showticklabels = xtick_show,
                          tickfont = list(size = x_tick_font_size)
                        ),
                        yaxis = list(
                          title = list(
                            text = base::ifelse(y_label_font_size == 0, "", "Variance Explained (%)"),
                            font = list(size = y_label_font_size)
                          ),
                          showticklabels = ytick_show,
                          rangemode = "tozero",
                          tickfont = list(size = y_tick_font_size)
                        ),
                        barmode = 'overlay'
  )

  return(plot)
}

#------------------------------------------------------ Fatty acid analysis ----
fa_analysis_calc = function(data_table = NULL,
                            feature_table = NULL,
                            sample_meta = NULL,
                            selected_lipidclass = NULL,
                            fa_norm = FALSE) {
  ## Features
  feature_table = feature_table[colnames(data_table),]
  feature_table$lipid = rownames(feature_table)

  # fix TG's
  idx_tg = feature_table$lipid[feature_table[["Lipid class"]] == "TG"]
  idx_tg = base::intersect(idx_tg, colnames(data_table))
  
  if (length(idx_tg) == 0) {
    base::stop("No TGs found in data")
  }
  
  data_table[, idx_tg] = data_table[, idx_tg] / 3

  # get the species from the selected lipid classes
  if(selected_lipidclass == "All") {
    # all lipids, but remove PA
    sel_feat_idx = feature_table$lipid[!(feature_table[["Lipid class"]] %in% c("PA"))]
  } else if(selected_lipidclass == "All_noTG") {
    # all lipids, but remove PA
    sel_feat_idx = feature_table$lipid[!(feature_table[["Lipid class"]] %in% c("PA", "TG"))]
  } else {
    sel_feat_idx = feature_table$lipid[feature_table[["Lipid class"]] %in% selected_lipidclass]
  }
  sel_feature_table = feature_table[feature_table$lipid %in% sel_feat_idx, ]

  ## Data
  # select the correct data
  sel_data_table = data_table[, sel_feat_idx, drop = F]

  # get the unique chain lengths and unsaturation
  uniq_carbon = sort(union(unique(sel_feature_table[["Carbon count (chain 1)"]][sel_feature_table[["Lipid class"]] != "TG"]),
                           unique(sel_feature_table[["Carbon count (chain 2)"]])))
  uniq_carbon = uniq_carbon[uniq_carbon != 0]
  uniq_unsat = sort(union(unique(sel_feature_table[["Double bonds (chain 1)"]][sel_feature_table[["Lipid class"]] != "TG"]),
                          unique(sel_feature_table[["Double bonds (chain 2)"]])))

  # Initialize results data.frame
  fa_chains = expand.grid(uniq_unsat, uniq_carbon)
  fa_chains = paste(fa_chains[, 2], fa_chains[, 1], sep = ":")
  res = as.data.frame(matrix(ncol = length(fa_chains),
                             nrow = nrow(sel_data_table)))
  colnames(res) = fa_chains
  rownames(res) = rownames(sel_data_table)

  # do the calculations
  for(a in uniq_carbon) {
    for(b in uniq_unsat) {
      sel_fa_chain = paste(a, b, sep = ":")
      sel_lipids = sel_feature_table$lipid[(sel_feature_table[["Carbon count (chain 1)"]] == a &
                                              sel_feature_table[["Double bonds (chain 1)"]] == b) |
                                             (sel_feature_table[["Carbon count (chain 2)"]] == a &
                                                sel_feature_table[["Double bonds (chain 2)"]] == b)]
      sel_lipids_double = sel_feature_table$lipid[(sel_feature_table[["Carbon count (chain 1)"]] == a &
                                                     sel_feature_table[["Double bonds (chain 1)"]] == b) &
                                                    (sel_feature_table[["Carbon count (chain 2)"]] == a &
                                                       sel_feature_table[["Double bonds (chain 2)"]] == b)]

      res[, sel_fa_chain] = `+`(
        rowSums(sel_data_table[, sel_lipids, drop = FALSE], na.rm = TRUE),
        rowSums(sel_data_table[, sel_lipids_double, drop = FALSE], na.rm = TRUE)
      )
    }
  }

  # remove empty columns
  empty_idx = apply(res, 2, function(x) {
    all(x == 0)
  })
  res = res[, !empty_idx]

  # normalise by total FA's
  if(fa_norm) {
    res = res / rowSums(res, na.rm = TRUE)
  }

  return(res)
}


fa_analysis_rev_calc = function(data_table = NULL,
                                feature_table = NULL,
                                sample_meta = NULL,
                                selected_fa = NULL,
                                fa_norm = FALSE) {
  uniq_lipid_classes = unique(feature_table[["Lipid class"]][!(feature_table[["Lipid class"]] %in% c("PA"))])

  ## Features
  feature_table$lipid = rownames(feature_table)

  sel_feat_idx = feature_table$lipid[!(feature_table[["Lipid class"]] %in% c("PA"))]
  sel_feature_table = feature_table[feature_table$lipid %in% sel_feat_idx, ]

  ## Data
  # select the correct data
  sel_data_table = data_table[, sel_feat_idx]

  # Initialize results data.frame
  res = as.data.frame(matrix(ncol = length(uniq_lipid_classes),
                             nrow = nrow(sel_data_table)))
  colnames(res) = uniq_lipid_classes
  rownames(res) = rownames(sel_data_table)

  # do the calculations
  fa_norm_tot = 0
  for(lipid_class in uniq_lipid_classes) {
    for(fa_tail in selected_fa) {
      split_fa = as.numeric(unlist(strsplit(fa_tail,
                                            split = ":",
                                            fixed = TRUE)))
      sel_lipids = sel_feature_table$lipid[sel_feature_table[["Lipid class"]] == lipid_class &
                                             ((sel_feature_table[["Carbon count (chain 1)"]] == split_fa[1] &
                                                 sel_feature_table[["Double bonds (chain 1)"]] == split_fa[2]) |
                                                (sel_feature_table[["Carbon count (chain 2)"]] == split_fa[1] &
                                                   sel_feature_table[["Double bonds (chain 2)"]] == split_fa[2]))]
      sel_lipids_double = sel_feature_table$lipid[sel_feature_table$lipid == lipid_class &
                                                    (sel_feature_table[["Carbon count (chain 1)"]] == split_fa[1] &
                                                       sel_feature_table[["Double bonds (chain 1)"]] == split_fa[2]) &
                                                    (sel_feature_table[["Carbon count (chain 2)"]] == split_fa[1] &
                                                       sel_feature_table[["Double bonds (chain 2)"]] == split_fa[2])]

      res[, lipid_class] = rowSums(sel_data_table[, c(sel_lipids, sel_lipids_double), drop = FALSE], na.rm = TRUE)
    } # end selected_fa
  } # end lipid_class

  # fix the TG's
  res[, "TG"] = res[, "TG"] / 3

  # remove empty columns
  empty_idx = apply(res, 2, function(x) {
    all(x == 0)
  })
  res = res[, !empty_idx]

  # get rid of the zero's
  res[res == 0] = NA

  # normalise by total FA's
  if(fa_norm) {
    res = res / rowSums(res, na.rm = TRUE)
  }

  return(res)
}


#---------------------------------------------------------- COLOR FUNCTIONS ----


colors_switch = function(selection) {
  switch(EXPR = selection,
         'Blues' = 9,
         'BuGn' = 9,
         'BuPu' = 9,
         'GnBu' = 9,
         'Greens' = 9,
         'Greys' = 9,
         'Oranges' = 9,
         'OrRd' = 9,
         'PuBu' = 9,
         'PuBuGn' = 9,
         'PuRd' = 9,
         'Purples' = 9,
         'RdPu' = 9,
         'Reds' = 9,
         'YlGn' = 9,
         'YlGnBu' = 9,
         'YlOrBr' = 9,
         'YlOrRd' = 9,
         'BrBG' = 11,
         'PiYG' = 11,
         'PRGn' = 11,
         'PuOr' = 11,
         'RdBu' = 11,
         'RdGy' = 11,
         'RdYlBu' = 11,
         'RdYlGn' = 11,
         'Spectral' = 11,
         'Accent' = 8,
         'Dark2' = 8,
         'Paired' = 12,
         'Pastel1' = 9,
         'Pastel2' = 8,
         'Set1' = 9,
         'Set2' = 8,
         'Set3' = 12,
         'Magma' = 256,
         'Inferno' = 256,
         'Plasma' = 256,
         'Viridis' = 256,
         'Cividis' = 256,
         'Rocket' = 256,
         'Mako' = 256,
         'Turbo' = 256,
         'plotly_1' = 10,
         'plotly_2' = 10,
         'ggplot2' = 10
  )
}

viridis_switch = function(color_palette) {
  switch(EXPR = color_palette,
         "Magma" = "A",
         "Inferno" = "B",
         "Plasma" = "C",
         "Viridis" = "D",
         "Cividis" = "E",
         "Rocket" = "F",
         "Mako" = "G",
         "Turbo" = "H"
  )
}

custom_colors_switch = function(color_palette) {
  switch(EXPR = color_palette,
         'plotly_1' = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", '#8c564b', "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
         'plotly_2' = c("#636efa", "#ef553b", "#00cc96", "#d62728", "#9467bd", '#8c564b', "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
         'ggplot2' = c("#F8766D", "#D89000", "#A3A500", "#39B600", "#00BF7D", "#00BFC4", "#00B0F6", "#9590FF", "#E76BF3", "#FF62BC")
  )
}

get_colors = function(color_count, color_palette) {
  if (color_palette %in% c('Viridis', 'Magma', 'Inferno', 'Plasma', 'Cividis', 'Rocket', 'Mako', 'Turbo')) {
    short_name = viridis_switch(color_palette)
    color_palette =  viridisLite::viridis(n = color_count, alpha = 1, begin = 0, end = 1, direction = -1, option = short_name)
  } else if (color_palette %in% c('plotly_1', 'plotly_2', 'ggplot2')) {
    color_palette = custom_colors_switch(color_palette)
  } else {
    color_palette = RColorBrewer::brewer.pal(color_count, color_palette)
  }
  return(color_palette)
}

create_color_scale = function(color_palette) {
  # Calculate evenly spaced positions for each color
  positions = base::seq(0, 1, length.out = length(color_palette))

  # Combine the positions and colors into the correct format for Plotly
  custom_colorscale = stats::setNames(as.list(color_palette), as.character(positions))

  # Convert the list to the format Plotly expects (list of lists with position and color)
  plotly_colorscale = lapply(names(custom_colorscale), function(x) c(as.numeric(x), custom_colorscale[[x]]))
  return(plotly_colorscale)
}

get_color_palette = function(groups, color_palette, reverse_color_palette = F, force_scale = F, force_list = F) {

  # Checks
  if (force_scale & force_list) {
    stop("force_scale and force_list cannot both be TRUE")
  }

  # Get unique groups
  unique_groups = sort(unique(groups))

  # Get the color palette values
  color_count = colors_switch(color_palette)
  color_palette = get_colors(color_count = color_count, color_palette = color_palette)
  if (reverse_color_palette) {
    color_palette = base::rev(color_palette)
  }

  # Is data numeric or string
  if (is_coercible_to_numeric(groups)) {
    groups = base::as.numeric(groups)

    # Is data continuous or discrete
    if (((length(unique_groups) > 25) | force_scale) & !force_list) {
      # If continuous, export a color scale (for plotly)
      out_colors = create_color_scale(color_palette)
    } else {
      # If low number of groups, export simple dict
      out_colors = grDevices::colorRampPalette(color_palette)(length(unique_groups))
      if (length(unique_groups) > 1) {
        out_colors = ggplot2::scale_color_gradientn(colours = color_palette, limits = range(unique_groups))
        out_colors = out_colors$rescale(as.numeric(unique_groups))
        out_colors = color_palette[findInterval(out_colors, seq(min(out_colors), max(out_colors), length.out = length(color_palette)))]
      }
      out_colors = setNames(out_colors, as.character(unique_groups))
    }
  } else {
    # For string categorical data, export a simple dict
    out_colors = grDevices::colorRampPalette(color_palette)(length(unique_groups))
    out_colors = setNames(out_colors, as.character(unique_groups))
  }
  return(out_colors)
}

