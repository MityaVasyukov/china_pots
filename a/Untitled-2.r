
# Calculate the IQR for the 'cord_mark_width' variable
Q1 <- quantile(datac$cord_mark_width, 0.25, na.rm = TRUE)
Q3 <- quantile(datac$cord_mark_width, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define the outlier threshold (1.5 * IQR above Q3 or below Q1)
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filter the data to exclude outliers for the points
datac_filtered <- datac[datac$cord_mark_width >= lower_bound & datac$cord_mark_width <= upper_bound, ] %>%
    drop_na(cord_mark_width)

# Calculate the IQR and filter out outliers for each site and period
datac_filtered <- datac %>%
  group_by(site, period) %>%
  mutate(
    Q1 = quantile(cord_mark_width, 0.25, na.rm = TRUE),
    Q3 = quantile(cord_mark_width, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  ) %>%
  filter(cord_mark_width >= lower_bound & cord_mark_width <= upper_bound) %>%
  ungroup()




# Summarize the data to get counts by site and period
count_data <- datac %>%
  group_by(site, period) %>%
  drop_na(cord_mark_width) %>%
  summarize(count = n(), .groups = 'drop')

# Now plot with the filtered data
ggplot(datac, aes(x = site, y = state, fill = petro)) +
  geom_violin(trim = FALSE, alpha = 0.2, position = position_dodge(width = 0.8)) +
  geom_boxplot(width = 0.2, outlier.shape = 8, outlier.size = 5, position = position_dodge(width = 0.8)) +
  stat_boxplot(
    aes(x = site, y = cord_mark_width, fill = period),
    position = position_dodge(width = 0.8),
    geom = 'errorbar',
    width = 0.2,
    color = "black",
    alpha = 0.7
  ) +
  geom_point(
    data = datac_filtered,  # Use the filtered data without outliers
    aes(x = site, y = cord_mark_width),
    color = "black", 
    position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), 
    alpha = 0.2,
    size = 1.5
  ) +
  # Add count text for each group
  geom_text(
    data = count_data,
    aes(x = site, y = max(datac$cord_mark_width, na.rm = TRUE) * 1.05, label = count, group = period),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    color = "black",
    size = 4
  ) +
  scale_y_continuous(
    limits = c(min(datac$cord_mark_width, na.rm = TRUE), max(datac$cord_mark_width, na.rm = TRUE) * 1.1),
    n.breaks = 10
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Site and Period",
    y = "Size [mm]",
    title = "cord_mark_width by Site and Period"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_text(size = 20, vjust = -2.5),  # Shift x-axis label down
    axis.title.y = element_text(size = 20, margin = margin(r = 20)),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    strip.text.x = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = c(0.97, 0.97),
    legend.justification = c(1, 1),
    plot.margin = margin(10, 10, 20, 10)
  )



