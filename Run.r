###   SETTINGS AND LOADINGS   ###
    # Path settings
        wd <- getwd()
        inpath <- file.path(wd, "input")
        outpath <- file.path(wd, "output")

        if (!dir.exists(outpath)) {
            dir.create(outpath, recursive = TRUE)
        }

    
    # Load dependencies
        source("load_pckgs.r")
        source("settings.r")
        source("carbPlotter.r")
        source("dryOrWet.r")
        source("overviewer.r")
        source("pca_analysis.r")
        source("lda_analysis.r")
        source("ran_for_analysis.r")
        source("cluster_analysis.r")
        source("means_clust_analysis.r")

        load_pkg("readxl")
        load_pkg("dplyr")

    # Load data
    raw_df <- read_excel(file.path(inpath, info_and_carb_data_file_name), na = "")

###   ANALYSIS   ###
    # Run carbonization pattern plotter
        carb_plotter(raw_df)

    # Run dry/wet analysis
        dryOrWet(raw_df, "by_site_and_period")

    # Overview plotter through all the data
        # prepare the data

        # Run overviwer
        source("overviewer.r") #! remove
        r <- overviewer(inpath, outpath, talky =F)

        summary(r)
        print(r$numeric_plots$m.rim)
        lapply(r$numeric_plots, print)
        invisible(lapply(list.files(outpath, pattern = "\\.tiff$", full.names = TRUE), browseURL))


        source("pca_analysis.r")
        pca_analysis(r$data, "i.collection")
        pca_analysis(r$data, "i.period")
        pca_analysis(r$data, "i.site")

        source("lda_analysis.r")
        lda_analysis(r$data, "i.collection")
        