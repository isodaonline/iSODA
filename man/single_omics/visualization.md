Interpreting results from experiments relies on processing the measured data in a specific way, depending on what needs to be achieved, and using sample and feature annotations to further enhance interpretability. The output then needs to be presented graphically in a meaningful manner for the user to assess the results and explore their data. The visualization tab is designed to automate all these steps in a few clicks: the user can select up to four plots to be displayed simultaneously and they will immediately appear on screen using the most relevant input parameters. In addition to the selection, zooming and hovering capabilities provided by plotly, iSODA’s plots can be interacted with using the four sets of parameters accessed via the sidebars:  

* Input settings let the user select which transformed version of the measured data to use (raw, total normalized, z-scored) as well as other relevant input to be mapped on the plot, like sample or feature annotations.  

* Data settings are used to access the parameters used in statistics and other background processes. 

* Aesthetics settings control the colors, marker size, opacity and other display options used to enhance the readability of the data. 

* Output settings allow a selection of the output formats for images (JPG, PNG, WEBP and SVG) as well as export options within or outside the app. Some of the plot results can be exported to the annotation tables to be used elsewhere on the app, like for instance sample or feature clusters. Most plots also have associated tables which can be downloaded from here, to be used outside the app.  

Networks also have their own settings to adjust the physics parameters and display clear and uncluttered networks.  

Current plot selection includes a dendrogram, volcano plot, sample correlation, feature correlation, heatmap and PCA. Moreover, the lipidomics module has three specific plots: class comparison, class distribution and double bonds plots.  

**Dendrogram**: Provides a rapid assessment of the similarity between samples and sample groups. The samples are clustered using hierarchical clustering and displayed in the form of a dendrogram, which can be mapped with sample annotations to find out if the unsupervised groups can be explained by known factors. The number of desired clusters can be set and saved on the sample annotations table to be used elsewhere on the app. For example, in Figure 4 one can see how this functionality is used to highlight that cancer cell lines are primarily clustered according to whether the cells are epithelial or not.  

**Volcano Plot**: A standard way of identifying features that distinguish two sample groups. The features are displayed in a scatter plot, the y-axis representing the -Log10(p-value) and the x-axis the Log2(fold change). The p-values are calculated using either a t-Test or a Wilcoxon test and can be adjusted with multiple methods. The fold changes are calculated using either the mean or the median values. In some cases, p-values or fold changes cannot be calculated because of one group having too few or no values for a feature. These features are reported in violin plots surrounding the main scatter plot. The left-to-right spread of the features reflects their differential production or expression between the two sample groups (higher absolute fold change), and the more a feature is located on the top of the plot, the more significant it is (lower p-value). The adjacent violin plots display the features that can be regarded as some of the most important, since they are absent in one of both groups. To highlight relevant features, the user can set p-value and fold change thresholds, manifesting in dashed lines across the volcano plot. Features above these thresholds can be exported to the feature annotations table and used, for instance, in functional analysis. As an alternative to viewing the differential expression, the user can also map feature annotations on the volcano plot to assess if a known group of features is being over- or underexpressed.  

**Sample Correlation Plot**: An alternative representation to the dendrogram visualization, augmented with a heatmap to display and better understand sample clusters. The heatmap displays the correlation coefficient (Pearson or Spearman) between each sample pair, and the sample order is arranged using hierarchical clustering, the results of which are also displayed with the dendrograms on the sides of the heatmap. As with the dendrogram, sample annotations can be mapped on the leaves of the dendrograms. While the dendrogram only displayed the clusters, the sample correlation heatmap also shows how close (or different) samples are based on the correlation coefficients. The sample clusters are also emphasized with the heatmap colors. The calculated clusters can be exported to the sample annotations table.   

**Feature Correlation Plot**: Shows which features are correlated and highlights feature groups thanks to the hierarchical clustering and a heatmap. It is the same representation as the sample correlation but applied to features. Due to the limited rendering capabilities on browsers, the plotted data can be reduced using a maximum feature count filter and a minimum correlation coefficient threshold, thus displaying only the best scoring correlations. Like the sample correlation, the correlation coefficients are shown on the heatmap, and the feature ordering is based on hierarchical clustering, helping highlight groups of features and their correlations, notably with the color coding. Obtaining feature annotations is not as straightforward as gathering sample information: creating groups of features based on this feature correlation is a good starting point when nothing else is available. The generated feature clusters can then be exported to the feature annotations table.  

**Heatmap**: A visual representation of the measured data combining sample and feature clustering, allowing to find out if some groups of samples are associated with groups of features. This is achieved by displaying the z-score scaled data as a heatmap with samples as columns and features as rows. Based on the data, hierarchical clustering is applied to samples and features, highlighting areas in the dataset where groups of samples and feature expression coincide. Since this plot combines sample and features, their respective annotations can be mapped on the heatmap. Moreover, Lasso and Elastic-Net Regularized Generalized Linear Models can be applied on the features to keep only those best segregating the sample groups. Using the representation, the user can directly assess if the sample group mapping can be associated with known feature groups, driving the observed clustering.  

**Principal Component Analysis**: Representation of the samples and the features in a reduced dimensional space that retains as much variance as possible from the dataset, highlighting potential sample groups and the features driving the groupings. In iSODA, PCA is delineated in three plots: explained variance, scores plot and loadings plot, with the option to display the scores and loadings plots together. The user can choose the number of computed principal components and display the associated explained variance. Two PCs can then be chosen for the 2D scores and loadings plots. The scores plot displays the coordinates of the original samples in this new 2D space. Likewise, the loadings plot represents each feature’s contribution to the two selected PCs. Sample and feature annotations can be mapped on the markers. The scores plot can be used to identify trends and sample groupings, while the loadings plot can identify the features contributing to these trends. Like with the heatmap, Lasso and Elastic-Net Regularized Generalized Linear Models can be used to specifically select the most segregating features.  

**Class Distribution**: A lipidomics specific visualization that provides a summary of the mean lipid class concentrations for all sample groups. The lipid concentration is displayed on the y-axis and the lipid classes on the x-axis. The class-wise group concentrations will be represented as colored bar plots. This plot allows the user to directly spot concentration differences between sample groups and assess the relative lipid class concentrations with the shared y-axis.  

**Class Comparison**: Grid version of the class distribution, allowing a better assessment of the minute group concentration variations. Sample group concentrations are represented by bar plots for each lipid class. Plots are arranged in a grid, each one with its own y-axis. Like for the class distribution, the bars represent the mean concentrations. On top of the bars, box plots represent the median concentrations and the quartiles, along with the individual sample concentrations as markers. With the separate axes, group concentrations can be better examined. The additional box plots and markers help identify the sample distribution for each group and potential outliers.  

**Double Bonds Plot**: Another lipidomics specific plot that works in conjunction with the volcano plot, allowing a more structure specific examination of the lipid class differences between two sample groups. Two sample groups and a lipid class are selected, and like with the volcano plot, p-values and fold changes are calculated. Each individual lipid species is displayed on a bubble plot, the y-axis representing the double bond count and the x-axis the carbon count. The carbon and double bond counts can be specified to the side-chain level or the total values. Each bubble – or lipid species – is displayed with a size relative to the p-value and colored according to fold change. The bubble size emphasizes the most relevant features while the coloring highlights the direction of the expression. Combined with the structural information provided by the double bonds and carbon counts, this plot can reveal structural trends that might be biologically relevant to differentiate the two conditions.  

