list(params = list(
    heatmap = list(
        lock_da = T)),
    hardcoded_settings = list(
      volcano_plot = list(
        datasets = list(
          "Raw data table",
          "Total normalized table"
        )
      ),
      heatmap = list(
        datasets = list(
          'Z-scored table',
          'Z-scored total normalized table'
        )
      ),
        enrichment_analysis = list(
            terms = c("Gene ontology (ALL)",
                      "Gene ontology (BP)",
                      "Gene ontology (MF)",
                      "Gene ontology (CC)")),
        over_representation_analysis = list(
            terms = c("Gene ontology (ALL)",
                      "Gene ontology (BP)",
                      "Gene ontology (MF)",
                      "Gene ontology (CC)")
        )
    )
)
