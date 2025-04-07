get_anova_for_var <- function(df, var) {

  cleaned_df <- df[!is.na(df$var), ]
  result <- aov(var ~ site_name * period, data = cleaned_df)
  print(result)

}
