lda_analysis <- function(df, var) {
  # dependencies
  source("load_pckgs.r")
  dependencies <- c("MASS", "dplyr", "tidyr", "fastDummies", "ggplot2", "rlang", "caret")
  invisible(lapply(dependencies, load_pkg))
  
  # 1) Data transformation
  data <- df %>%
    dplyr::select(
      -dplyr::all_of(names(.)[startsWith(names(.), "i.") & names(.) != var]),
      -dplyr::starts_with("c.col"),
      -p.optical_activity,
      -m.body_width, -m.height,
      -g.conf, -g.srr, -g.aapac18e_h,
      -id
    ) %>%
    tidyr::drop_na() %>%
    dplyr::mutate_if(is.logical, as.factor)
  
  # Drop unused levels from the grouping variable
  data[[var]] <- droplevels(data[[var]])
  
  # 2) Dummy encoding
  dummy_cols <- setdiff(names(data)[sapply(data, is.factor)], var)
  data <- fastDummies::dummy_cols(
    data,
    select_columns = dummy_cols,
    remove_first_dummy = TRUE
  )
  
  # 3) Keep numeric columns + grouping
  keep_cols <- names(data)[!sapply(data, is.factor) | names(data) == var]
  data <- data %>% dplyr::select(all_of(keep_cols))
  
  # 4) Remove columns with too many missing values
  missing_threshold <- 0.5
  data <- data[, sapply(data, function(x) mean(is.na(x))) <= missing_threshold]
  
  # 5) Remove zero-variance columns manually
  non_group_cols <- setdiff(names(data), var)
  data <- data[, c(
    var,
    non_group_cols[sapply(data[, non_group_cols, drop = FALSE],
                          function(x) sd(x, na.rm = TRUE) > 0)]
  )]
  
  message("📊 The data for LDA (before caret collinearity checks):")
  dplyr::glimpse(data)
  
  # -----------------------------
  # Remove collinearity using caret
  # -----------------------------
  # Convert to matrix (excluding the grouping variable)
  X <- data[, setdiff(names(data), var), drop = FALSE]
  
  # (a) Remove near-zero-variance predictors
  nzv <- caret::nearZeroVar(X)
  if (length(nzv) > 0) {
    X <- X[, -nzv, drop = FALSE]
  }
  
  # (b) Remove linear combinations
  comboInfo <- caret::findLinearCombos(X)
  if (!is.null(comboInfo$remove)) {
    X <- X[, -comboInfo$remove, drop = FALSE]
  }
  
 
# Remove collinear predictors using cor()
predictor_names <- setdiff(names(data), var)
predictors <- data[, predictor_names, drop = FALSE]

# Compute correlation matrix on predictors only
cor_matrix <- cor(predictors, use = "pairwise.complete.obs")

# Use caret's function to identify highly correlated columns
high_corr <- caret::findCorrelation(cor_matrix, cutoff = 0.99)
if(length(high_corr) > 0) {
  predictors_clean <- predictors[, -high_corr, drop = FALSE]
} else {
  predictors_clean <- predictors
}

# Rebuild the data with the cleaned predictors and the grouping variable
data <- data.frame(predictors_clean, data[[var]])
names(data)[ncol(data)] <- var


  message("📊 The data for LDA (after removing collinearity):")
  dplyr::glimpse(data)
  
  # 6) Scale numeric predictors
  numeric_data <- data[, setdiff(names(data), var), drop = FALSE]
  scaled_data <- scale(numeric_data)
  
  # Combine scaled predictors with grouping
  data_std <- data.frame(scaled_data, data[[var]])
  names(data_std)[ncol(data_std)] <- var
  
  # 7) Fit LDA
  lda_fit <- MASS::lda(as.formula(paste(var, "~ .")), data = data_std)
  
  # 8) Extract LDA scores
  lda_values <- as.data.frame(predict(lda_fit)$x)
  
  # Add grouping back to scores
  lda_values[[var]] <- data_std[[var]]
  
  # 9) Plot
  p <- ggplot(lda_values, aes(x = LD1, y = LD2, color = !!sym(var))) +
    geom_point(size = 3) +
    stat_ellipse(level = 0.95, linetype = "dashed") +
    labs(
      title = "LDA: LD1 vs LD2",
      x = "Linear Discriminant 1",
      y = "Linear Discriminant 2"
    ) +
    theme_minimal()
  
  print(p)
  
  return(lda_fit)
}
