## Input formats

Supported formats are coma-separated files (CSV, CSV2), tab-separated files (TSV, TXT) and Excel files (XLSX).  

### Sample annotations:  

Contains all metadata pertaining to each sample, values can be of any type. There is no mandatory naming for the columns, but there should be minimum three of them containing:
- **Sample IDs**: unique identifiers (same in all the tables).  
- **Sample type**: specifies which rows are blanks, QC, pools and actual samples.  
- **Group type**: specifies which groups each sample belongs to.   
- **Batch** (optional): batch number for each row (cannot contain NAs). If not supplied, all samples will be assumed to be from the same batch.  

This is the default order in which columns are selected in iSODA and they can be arranged thus in the file to have them automatically selected.

### Measurement data:  

Contains samples as rows and features as columns. The first column should be the sample IDs (same as in the sample annotations table), the rest are the feature names, values are only numerical or missing. Currently supported feature names are the following:
- Shorthand lipid IDs (Lipidomics)
- Symbol (Proteomics, Transcriptomics, Genomics)
- Entrez ID (Proteomics, Transcriptomics, Genomics)
- Uniprot (Proteomics, Transcriptomics, Genomics)  

### Feature annotations (optional):  

This table is formatted with features as rows and annotations as columns. Feature annotations can be simple – one value per cell – or complex: multiple values per cell, separated by a pipe (“|”) character. Complex annotations typically include ontologies, like classifications, pathways or gene ontologies and can be used in certain plots like the volcano plot, or for functional analysis.  

