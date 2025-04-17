## ---- settings ----
# file_names & paths
    info_and_carb_data_file_name <- "Data.xlsx" # the name of your excel file with info data
    petro_data_file_name <- "petrography.xlsx" # the name for petrgography data file
    gcms_data_file_name <- "Preliminary analysis Shang and Zhou.xlsx" # the name for gcms data
    inpath <- file.path(wd, "input")
    outpath <- file.path(wd, "output")
    path <- "p:/2025-vessels/spectra/mz"

# plot settings
    parts <- c("lip", "neck", "shoulder", "body", "foot", "crotch") # sequence of vessel parts
    gap <- 250 # gap used for plotting
    cex <- 10 # font size setting
    
    drywetColors <- c("WET" = "#1f77b4", "DRY" = "#ff7f0e") # colors for dry and wet in pie charts
    na_col <- "#b8241a" # color for a missing vessel part
     # list of modes for carb_plotter function
    file_format <- "pdf" # preferred file format for plots

# procesing setting
    print_res <- TRUE
    excluded_vars <- c("c.conf", "g.conf")

# dictionaries
    # The rule set for dry/wet assignment
        dry_wet_conditions <- data.frame(
            query_string = c(
                "col_in_neck == 'carb' & col_in_shoulder == 'carb' & col_in_body == 'carb'", 
                "col_in_neck == 'clear' & col_in_shoulder == 'carb' & col_in_body == 'carb'", 
                "col_in_neck != 'water' & !is.na(col_in_neck) & col_in_shoulder != 'water' & !is.na(col_in_shoulder) & col_in_body == 'carb'", 
                "col_in_neck == 'water' | col_in_shoulder == 'water' | col_in_body == 'water'",
                "col_in_neck == 'carb' & !is.na(col_in_shoulder) & col_in_body == 'clear'",
                "col_in_neck == 'clear' & col_in_shoulder == 'carb' & col_in_body == 'clear'",
                "col_in_neck == 'clear' & col_in_shoulder == 'clear' & col_in_body == 'clear'",
                "col_in_foot == 'water'",
                "TRUE"
            ),
            value = c(-3, -2, -1, 3, 2, 2, 1, 1, 0),
            header = c(
                "DRY, level 3", "DRY, level 2", "DRY, level 1", "WET, level 3", "WET, level 2", "WET, level 2", "WET, level 1", "WET, level 1", "na"
            ),
            help = c(
                "from neck to body: CARB CARB CARB", "from neck to body: CLEAR CARB CARB", "from neck to body: CLEAR CLEAR CARB", "from neck to body: ANY is WATER", "from neck to body: CARB ANY CLEAR", "from neck to body: CLEAR CARB CLEAR", "from neck to body: CLEAR CLEAR CLEAR", "foot is water and body is absent", "the rest combinations"
            )
        )

# Variable descriptions
    var_info <- list(
        "id"                      = list(type = "text id", units = NULL, options = NULL, descr = "Unique sample id"),
        "i.site"                  = list(type = "txt fct", units = NULL, options = c("Gaoqingcaopo", "Kanjiazhai", "Yangxinliwu"), descr = "The name of the site"),
        "i.period"                = list(type = "txt ord", units = NULL, options = c("Shang", "Zhou"), descr = "The dynastie"),
        "i.lip"                   = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "TRUE if lip is present in a sample"),
        "i.neck"                  = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "TRUE if neck is present in a sample"),
        "i.shoulder"              = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "TRUE if shoulder is present in a sample"),
        "i.body"                  = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "TRUE if body is present in a sample"),
        "i.foot"                  = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "TRUE if foot is present in a sample"),
        "i.crotch"                = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "TRUE if crotch is present in a sample"),
        "i.completeness"          = list(type = "num dbl", units = "%%", options = NULL, descr = "100 means all parts are present in a sample, less means a fragment is missing some parts"),
        "i.collection"            = list(type = "txt fct", units = NULL, options = NULL, descr = "A combination of period and a first letter of the site"),

        "m.body_width"            = list(type = "num dbl", units = "mm", options = NULL, descr = "Width of the body"),
        "m.height"                = list(type = "num dbl", units = "mm", options = NULL, descr = "Height of the vessel fragment"),
        "m.rim"                   = list(type = "num dbl", units = "mm", options = NULL, descr = "The size of the rim"),                  
        "m.orifice"               = list(type = "num dbl", units = "mm", options = NULL, descr = "The size of the orifice"),
        "m.cord_mark_width"       = list(type = "num dbl", units = "mm", options = NULL, descr = "The width of a cordmark"),      
        "m.cord_mark_type"        = list(type = "txt fct", units = "mm", options = NULL, descr = "Cordmark type"),

        "c.col_in_lip"            = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the lip part from inside"),           
        "c.col_in_neck"           = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the neck part from inside"),
        "c.col_in_shoulder"       = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the shoulder part from inside"),      
        "c.col_in_body"           = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the body part from inside"),
        "c.col_out_lip"           = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the lip part from outside"),          
        "c.col_out_neck"          = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the neck part from outside"),
        "c.col_out_shoulder"      = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the shoulder part from outside"),     
        "c.col_out_body"          = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the body part from outside"),
        "c.col_in_foot"           = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the foot part from inside"),          
        "c.col_out_foot"          = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the foot part from outside"),
        "c.col_out_crotch"        = list(type = "txt fct", units = NULL, options = c("carb", "clear", "water"), descr = "A coloration of the crotch part from outside"),       
        "c.group"                 = list(type = "txt ord", units = NULL, options = c("dry", "wet"), descr = "A classification according to carbonisation analysis"),
        "c.conf"                  = list(type = "num dbl", units = "%", options = c(0.3, 0.5, 1), descr = "A confidence level for 'Dry or Wet' classification, 1 means dead certainty"),

        "g.lipids_conc"           = list(type = "num dbl", units = "%", options = NULL, descr = "A concentration of lipids in a sample, promille?"),
        "g.scfa"                  = list(type = "num dbl", units = "%", options = NULL, descr = "Relative abundance of short-chain fatty acids (C2:C6)"),
        "g.mcfa"                  = list(type = "num dbl", units = "%", options = NULL, descr = "Relative abundance of medium-chain fatty acids (C6:C12)"),
        "g.lcfa"                  = list(type = "num dbl", units = "%", options = NULL, descr = "Relative abundance of long-chain fatty acids (>C12)"),
        "g.uns_fa"                = list(type = "num dbl", units = "%", options = NULL, descr = "Relative abundance of unsaturated fatty acids"),
        "g.diacids"               = list(type = "num dbl", units = "%", options = NULL, descr = "Relative abundance of dicarboxylic acids: oxidized fatty acids with two carboxyl groups"),
        "g.alkanes"               = list(type = "num dbl", units = "%", options = NULL, descr = "Relative abundance of saturated hydrocarbons"),
        "g.p_s"                   = list(type = "num dbl", units = NULL, options = NULL, descr = "Pristane/Phytane ratio ??"),
        "g.o_s"                   = list(type = "num dbl", units = NULL, options = NULL, descr = "Odd-over-even carbon number predominance ??"),
        "g.aapac18e_h"            = list(type = "num dbl", units = NULL, options = NULL, descr = "An index related to C18 alkylphenanthrene homologs (E/H variants) ??"),
        "g.srr"                   = list(type = "num dbl", units = NULL, options = NULL, descr = "A sterane ratio expressed as a percentage ??"),
        "g.group"                 = list(type = "txt ord", units = NULL, options = c("plant", "animal", "mixture"), descr = "A classification for organics source based on expert analysis"),
        "g.conf"                  = list(type = "num dbl", units = NULL, options = NULL, descr = "A confidence level for organic source classification (g.group), 1 means dead certainty"),
        "g.plant_count"           = list(type = "num int", units = NULL, options = NULL, descr = "A rowwise count for 'plant'"),
        "g.tree_count"            = list(type = "num int", units = NULL, options = NULL, descr = "A rowwise count for 'tree'"),
        "g.cereal"                = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'cereal' was mention in the comments"),               
        "g.fruit"                 = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'fruit' was mention in the comments"),
        "g.vegetable"             = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'vegetable' was mention in the comments"),            
        "g.resin"                 = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'resin' was mention in the comments"),
        "g.fish"                  = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'fish' was mention in the comments"),                 
        "g.complex_mxtr"          = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'complex mixture' was mention in the comments"),

        "p.optical_activity"      = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if optical activity is 'Active'"),     
        "p.mica"                  = list(type = "txt ord", units = NULL, options = NULL, descr = "The relative abundance of mica minerals"),
        "p.group"                 = list(type = "txt fct", units = NULL, options = NULL, descr = "A classification for petrography based on expert analysis"),                
        "p.lower_fr_bound"        = list(type = "num dbl", units = "mm", options = NULL, descr = "A lower bound for rock fragments approximate size range"),
        "p.upper_fr_bound"        = list(type = "num dbl", units = NULL, options = NULL, descr = "An upper bound for rock fragments approximate size range"),       
        "p.lower_roundness_bound" = list(type = "txt ord", units = NULL, options = c("angular", "subangular", "subround", "round"), descr = "A lower bound for rock roundndess"),
        "p.upper_roundness_bound" = list(type = "txt ord", units = NULL, options = c("angular", "subangular", "subround", "round"), descr = "An upper bound for rock roundndess"),
        "p.granite"               = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'granite' was mentioned in the comments"),
        "p.granodiorite"          = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'granodiorite' was mentioned in the comments"),         
        "p.diorite"               = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'diorite' was mentioned in the comments"),
        "p.sandstone"             = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'sandstone' was mentioned in the comments"),            
        "p.mudstone"              = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'mudstone' was mentioned in the comments"),
        "p.limestone"             = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'limestone' was mentioned in the comments"),            
        "p.gritstone"             = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'gritstone' was mentioned in the comments"),
        "p.volcanic"              = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'volcanic' was mentioned in the comments"),             
        "p.andesite"              = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'andesite' was mentioned in the comments"),
        "p.microgranite"          = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'microgranite' was mentioned in the comments"),
        "p.ksp"                   = list(type = "lgl", units = NULL, options = c("TRUE", "FALSE"), descr = "True if 'ksp' was mentioned in the comments")
    )

# GCMS var description
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
    "centroided" = list(units = NULL, descr = "TRUE indicates that the data points have already been processed to represent peak centers")
)



# plot titles #! plot settings
    plot_titles <- list(
        "carb_pattern" = list(
            order = 1,
            title = "Mean Carbonization Pattern in-and-out",
            details = "Darker color means larger percentage of carbonized sherds"
            ),
        "dry_wet" = list(
            order = 2,
            title = "Dry/Wet diagnostics summary results",
            details = "Top table is a list of conditions defining the categorization, button table and plots show the results"
            ),
        "group_plots" = list(
            order = 3,
            title = "Combined Classification Results of Dry/Wet, Residue, and Petrography Analyses",
            details = NULL
            ),
        "i_plots" = list(
            order = 4,
            title = "Plots of Fragment Presence in the Samples",
            details = NULL
            ),
        "c_plots" = list(
            order = 5,
            title = "Plots of Coloration Patterns of Pottery Sherds",
            details = NULL
            ),
        "m_plots" = list(
            order = 6,
            title = "Plots of Measurements Taken on Pottery Sherds",
            details = NULL
            ),
        "p_plots" = list(
            order = 7,
            title = "Plots of Petrography Analysis Variables",
            detail = NULL
            ),
        "r_plots" = list(
            order = 8,
            title = "Plots of Residue Analysis Variables",
            details = NULL
            ),
        "spectrum_overview" = list(
            order = 9,
            title = "Heatmap representation of GCMS data of a single sample",
            details = "Plots left (Mass Spectrum) and below (TIC) are the summations along the relevant axes"
            ),
        "tics_all" = list(
            order = 10,
            title = "Principal Component Analysis of Total Ion Chromatograms",
            details = "Data for PCA was Box-Cox transformed and scaled"
            ),
        "tics_sel" = list(
            order = 11,
            title = "Principal Component Analysis of Only Intense Ion Signal Chromatograms",
            details = "Data for PCA was Box-Cox transformed and scaled"
            )
    )
