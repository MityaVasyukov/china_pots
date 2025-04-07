

        lda_analysis(r$data)
        ran_for_analysis(r$data)
        cluster_analysis(r$data)
        k-means_clust_analysis(r$data)


data <- r$data %>%
      dplyr::select(i.collection, dplyr::where(is.numeric)) %>%
      dplyr::select(-m.body_width, -m.height, -g.conf, -g.srr, -g.aapac18e_h)



data <- na.omit(data)

numeric_data <- data[, -1]



# random forest
# Load necessary libraries
library(randomForest)
library(dplyr)

# Prepare the data for random forest:
# Exclude the id variable since it's not a predictor.
data_rf <- data_class %>% dplyr::select(-id)

# Run a random forest model with i.collection as the response.
set.seed(123)  # for reproducibility
rf_fit <- randomForest(i.collection ~ ., data = data_rf, importance = TRUE)

# Print the random forest model summary.
print(rf_fit)

# Plot variable importance.
varImpPlot(rf_fit)



# --- Cluster Analysis ---
# Compute a distance matrix on the scaled numeric data
dist_matrix <- dist(scale(numeric_data))

# Hierarchical clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc,
     labels =  data$i.collection,
     main = "Hierarchical Clustering Dendrogram")

# Alternatively, perform K-means clustering (e.g., with 3 clusters, adjust centers as needed)
set.seed(123)  # For reproducibility
km <- kmeans(scale(numeric_data), centers = 3, nstart = 25)

# Visualize K-means clustering
fviz_cluster(km, data = numeric_data, geom = "point") +
  ggtitle("K-means Clustering")

# Compare cluster assignments to your combined group (or site, period)
table(km$cluster, data$i.collection)








# Load necessary libraries
library(FactoMineR)
library(factoextra)

# If you have logical variables, consider converting them to factors
data_mixed <-  r$data %>%
    dplyr::select(
        -p.optical_activity,
        -m.body_width,
        -m.height, -g.conf, -g.srr,
        -g.aapac18e_h, -i.cord_mark_type,
        -starts_with("c.col"),
        -i.site, -i.period, - i.lip, -i.neck, -i.shoulder, -i.body, - i.foot, -i.crotch
     )

data_mixed <- na.omit(data_mixed)

glimpse(data_mixed)


library(tibble)  # for column_to_rownames
library(FactoMineR)
library(factoextra)
library(dplyr)

# Convert 'id' to row names (ensure 'id' has unique values)
data_mixed2 <- data_mixed %>%
  column_to_rownames(var = "id")

# Now run FAMD on the new data frame (which no longer has the 'id' column)
famd_result <- FAMD(data_mixed2, graph = FALSE)

# Visualize individuals, colored by i.collection
fviz_famd_ind(famd_result,
              habillage = data_mixed2$i.collection,
              addEllipses = TRUE,
              ellipse.level = 0.95) +
  ggtitle("FAMD: Mixed Data Analysis")




















    # Correlation
        data_cor <- data %>% select(-site, -period)
        cor_res <- rcorr(as.matrix(data_cor), type = "spearman")
        M <- cor_res$r     # Spearman correlation coefficients
        p_mat <- cor_res$P # the corresponding p-values
        corplot <- ggcorrplot(
            M,
            type = "upper",
            hc.order = FALSE,
            lab = TRUE,
            p.mat = p_mat,
            sig.level = 0.05,
            ggtheme = ggplot2::theme_minimal(),
                legend.title = "Spearman rho",
                colors = c("blue", "white", "blue"),
            )
        print(corplot)

    # Violin plots for all vars
        data_long <- data %>%
            pivot_longer(
                cols = -c(site, period, DRY, group, petro),
                names_to = "variable",
                values_to = "value"
            )


library(ggplot2)
library(dplyr)
library(rlang)

grid_plotter <- function(data, grvar) {
  # Compute sample sizes (n) and maximum value (ymax) for each variable and grouping variable combination
  counts <- data %>%
    group_by(variable, .data[[grvar]]) %>%
    summarise(
      n = sum(!is.na(value)),
      ymax = max(value, na.rm = TRUE),
      .groups = "drop"  # drop grouping after summarise
    )
  
  # Create the violin plot faceted by variable
  ggplot(data, aes(x = .data[[grvar]], y = value)) +
    geom_violin(trim = FALSE, na.rm = TRUE) +
    geom_text(
      data = counts,
      aes(x = .data[[grvar]], y = ymax, label = paste0("n=", n)),
      vjust = -0.5,
      na.rm = TRUE
    ) +
    facet_wrap(~ variable, scales = "free_y") +
    theme_bw() +
    labs(
      title = paste("Violin Plots Grouped by", grvar),
      x = grvar,
      y = "Value"
    ) +
    theme(
       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
}

# Example call:
grid_plotter(data_long, "petro")
















# CARB PATTERN

carb_plotter(raw_df, )
carb_plotter(pots, "by_site")
carb_plotter(pots, "by_period")

# DRY or WET
plot_data <- dryOrWet(raw_df, "by_site_and_period")
# OR
# dryOrWet(raw_df, )
# dryOrWet(raw_df, "by_site")
# dryOrWet(raw_df, "by_period")

save_plot(dryOrWet, raw_df, ) 


# ANOVA


cleaned_df <- raw_df %>% 
  filter(!is.na(rim))  %>%
  select(site_name, period, rim) %>%
  mutate(
    period = factor(period),
    site_name = factor(site_name)
  )



ggboxplot(cleaned_df, x = "site_name", y = "rim", color = "period",
          palette = c("#00AFBB", "#E7B800"))


nrow(cleaned_df)
anova_result <- aov(orifice ~ site_name * period, data = cleaned_df)
summary(anova_result)
TukeyHSD(anova_result)
result_orifice <- aov(orifice ~ site_name * period, data = cleaned_df)
summary(result_orifice)
print(tukey_orifice)

#  wilcoxon test better !!!!!!!!!!!!!




###########################################################################################################

petro <- read_excel("petrography.xlsx", na = "")

petro <- petro %>%
    rename( aux_id = "Sample #",
    petro = "Group Simplified") %>%
        select(aux_id, petro)


data <- raw_df %>%
    left_join(petro, by = join_by(aux_id))

drywet <- as.data.frame(dryOrWet(raw_df, "by_site_and_period")$table) %>%
    select(record_id, state)

data <- data %>%
    left_join(drywet, by = join_by(record_id))








datac <- data %>%
    rename(site = site_name) %>%
      # filter(orifice < 25 | is.na(orifice)) %>%
            mutate(
                decoration = ifelse(is.na(decoration), FALSE, TRUE),
                site = factor(site, levels = c("Yangxinliwu", "Gaoqingcaopo", "Kanjiazhai"))) %>%
                    #select(record_id, site, period, decoration, petro, rim, orifice, ) %>%
                    select(record_id, site, period, orifice, petro, state) %>%
                        mutate(petro = factor(petro)) %>%
                            mutate(
                                state = ifelse(state == 0, NA, state)
                            ) %>%
                                mutate(
                                    state = factor(ifelse(state < 0, "dry", "wet"))
                                 )
                         #   filter(state != 0) %>%
                        drop_na()

# write.table(datac, "vessels-2024.csv")


wilcox.test(table(datac$state, datac$orifice))

"Fisher's Exact Test for Count Data
data:  table(datac$petro, datac$period)
p-value = 3.745e-08
alternative hypothesis: two.sided
"

fisher.test(table(datac$petro, datac$site)) #NA

fisher.test(table(datac$state, datac$site))


wilcox.test(datac$petro, datac$) # p-value = 3.745e-08
wilcox.test(datac$state, datac$period) # p-value = 0.004147
wilcox.test(datac$state, datac$site) # p-value = 0.0001488


ggplot(datac, aes(x = petro, fill = period)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Petro Types by Period",
       x = "Petro Type", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 scale_fill_viridis_d(option = "C") 


ggplot(datac, aes(x = petro, fill = period)) +
  geom_bar() +
  labs(title = "Stacked Count of Petro Types by Site",
       x = "Petro Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
   scale_fill_viridis_d(option = "C") 

ggplot(datac, aes(x = petro)) +
  geom_bar(aes(fill = petro)) +
  facet_wrap(~ site) +
  labs(title = "Petro Types by Site",
       x = "Petro Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



table_data <- as.data.frame(table(datac$petro, datac$site))
colnames(table_data) <- c("Petro", "Site", "Count")

ggplot(table_data, aes(x = Site, y = Petro, fill = Count)) +
  geom_tile() +
  labs(title = "Heatmap of Petro Types by Site",
       x = "Site", y = "Petro Type") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal()








>

        Fisher's Exact Test for Count Data

data:  table(datac$petro, datac$period)
p-value = 3.745e-08
alternative hypothesis: two.sided

> aov_state_site <- aov(state ~ site, data = datac)
> summary(aov_state_site)
             Df Sum Sq Mean Sq F value Pr(>F)
site          2   13.5   6.765   1.548  0.218
Residuals   100  437.0   4.370               
> 
> # Kruskal-Wallis test
> kruskal.test(state ~ period, data = datac)

        Kruskal-Wallis rank sum test

data:  state by period
Kruskal-Wallis chi-squared = 0.14595, df = 1, p-value = 0.7024

> aov_state_petro <- aov(state ~ petro, data = datac)
> summary(aov_state_petro)
            Df Sum Sq Mean Sq F value Pr(>F)
petro       10   27.7   2.772   0.603  0.807
Residuals   92  422.8   4.596               
> glm_model <- glm(state ~ site + period + decoration + petro, data = datac, family = gaussian())
> summary(glm_model)

Call:
glm(formula = state ~ site + period + decoration + petro, family = gaussian(), 
    data = datac)

Coefficients:
                          Estimate Std. Error t value Pr(>|t|)
(Intercept)               -0.90370    0.87375  -1.034    0.304
siteGaoqingcaopo          -1.04300    0.77334  -1.349    0.181
siteKanjiazhai             0.08318    1.25798   0.066    0.947
periodZhou                 0.06441    0.80032   0.080    0.936
decorationTRUE            -0.57778    0.65132  -0.887    0.377
petroFeldspar Sand         0.89811    1.36291   0.659    0.512
petroFeldspar Sand + Lime  1.23739    1.27219   0.973    0.333
petroFine Silt             3.88228    2.40481   1.614    0.110
petroGranitic + Sed        2.34670    1.50941   1.555    0.124
petroGranitic Sand         0.94670    1.58234   0.598    0.551
petroGranodiorite          1.56226    0.97447   1.603    0.112
petroMetamorphic           2.30376    1.57360   1.464    0.147
petroSandstone             1.23559    1.90319   0.649    0.518
petroTonalite              2.31920    1.85652   1.249    0.215
petroVolcanic              2.91449    1.84922   1.576    0.119

(Dispersion parameter for gaussian family taken to be 4.509909)

    Null deviance: 450.52  on 102  degrees of freedom
Residual deviance: 396.87  on  88  degrees of freedom
AIC: 463.24

Number of Fisher Scoring iterations: 2
