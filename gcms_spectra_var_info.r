

gcms_spectra_var_info <- list(
    "seqNum" = list(units = NULL, descr = "Unique sequence number (ID)"),
    "acquisitionNum" = list(units = NULL, descr = "Unique sequence number (ID)"),
    "msLevel" = list(units = NULL, descr = "The level of mass spectrometry data: level 1 meaning only one stage of mass spectrometry was performed (no MS/MS fragmentation)"),
    "polarity" = list(units = NULL, descr = "The value -1 indicates the scans were recorded in negative ion mode"),
    "peaksCount" = list(units = NULL, descr = "How many peaks (detected ions) were recorded in each scan"),
    "totIonCurrent" = list(units = NULL, descr = "The total ion current, representing the overall signal strength"),
    "retentionTime" = list(units = "s", descr = "The time taken for a solute to pass through a chromatography column"),
    "basePeakMZ" = list(units = NULL, descr = "The mass-to-charge (m/z) ratio of the most intense peak"),
    "basePeakIntensity" = list(units = NULL, descr = "The intensity of the most intense peak"),
    "centroided" = list(units = NULL, descr = "TRUE indicates that the data points have already been processed to represent peak centers"),

)
