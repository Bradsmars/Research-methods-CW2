#Loading the necessary libraries to view my visuals.
library(ggplot2)
library(scales)
library(dplyr)
library(showtext)
library(plotly)
library(readr)
library(tibble)
library(tidyr)
library(sysfonts)
library(showtext)
library(tidyverse)
library(heatmaply)
library(viridis)
library(RColorBrewer)

#loading fonts
font_add("Lobster", "C:/Users/Bradley/Documents/research_methods/LobsterTwo-Bold.ttf")

font_add_google("Lobster Two", "lobstertwo")
font_add_google("Roboto", "roboto")

showtext_auto()


#Loading dataset into notebook
df <- read_csv("Results_21MAR2022.csv")



# Renamed "meat" to "meat 50-99" in the diet_group column based on research paper
df <- df %>%
  mutate(diet_group = ifelse(diet_group == "meat", "meat 50-99", diet_group))




# Created an aggregated data by sex and diet_group with normalized values
diet_gender_summary <- df %>%
  group_by(sex, diet_group) %>%
  summarise(
    GHG_Emissions = mean(mean_ghgs),
    Land_Use = mean(mean_land),
    Water_Scarcity = mean(mean_watscar),
    Eutrophication = mean(mean_eut),
    Acidification = mean(mean_acid),
    Biodiversity = mean(mean_bio),
    CH4_Emissions = mean(mean_ghgs_ch4),
    N2O_Emissions = mean(mean_ghgs_n2o),
    Water_Use = mean(mean_watuse),
  ) %>%
  ungroup()

# Created a matrix for heatmap with row names as gender_diet combinations
heatmap_data <- diet_gender_summary %>%
  mutate(group = paste(sex, diet_group, sep = " - ")) %>%
  select(-sex, -diet_group) %>%
  column_to_rownames("group")

# Scaled the data for better comparison
heatmap_data_scaled <- as.data.frame(scale(heatmap_data))

# Calculated statistics for annotation
group_stats <- diet_gender_summary %>%
  mutate(group = paste(sex, diet_group, sep = " - "))

# Extracted the unique diet groups for proper coloring.
diet_groups <- unique(diet_gender_summary$diet_group)
n_diets <- length(diet_groups)

# Produced a colour palette for diet groups
diet_colors <- colorRampPalette(brewer.pal(8, "Set2"))(n_diets)
names(diet_colors) <- diet_groups

diet_palette <- diet_colors

# Created row annotations for gender and diet
gender_diet_info <- data.frame(
  group = rownames(heatmap_data_scaled),
  stringsAsFactors = FALSE
) %>%
  separate(group, into = c("Gender", "Diet"), sep = " - ")

# This step involved Creating custom side colors for row annotations
row_side_colors <- data.frame(
  Gender = gender_diet_info$Gender,
  Diet = gender_diet_info$Diet,
  stringsAsFactors = TRUE
)

# This step involved creating a more informative heatmap title that I use my custom fonts with

title <- "Environmental Impact Profiles by Gender and Diet Groups"
subtitle <- "Hierarchical Clustering Analysis of Nutritional Environmental Footprints"

# Got unique gender values for proper coloring
gender_values <- unique(row_side_colors$Gender)

# This step I experimented with different color palettes for gender and diet.
# Implementing gender colours.
gender_palette <- c("#FF9AA2", "#A0D2EB")  
names(gender_palette) <- gender_values

# This step involved me Creating the diet palette
names(diet_palette) <- levels(factor(row_side_colors$Diet))

# Instead of using custom_hovertext function, I created a hovertext matrix directly
hovertext_matrix <- matrix("", nrow = nrow(heatmap_data_scaled), ncol = ncol(heatmap_data_scaled))
for (i in 1:nrow(heatmap_data_scaled)) {
  for (j in 1:ncol(heatmap_data_scaled)) {
    # Determining level based on Z-score
    z_value <- heatmap_data_scaled[i, j]
    
    # Assigning impact level based on Z-score
    if (z_value >= 1.5) {
      level <- "High"
    } else if (z_value >= 0.5) {
      level <- "Medium-high"
    } else if (z_value >= -0.5) {
      level <- "Medium"
    } else if (z_value >= -1.5) {
      level <- "Medium-low"
    } else {
      level <- "Low"
    }
    # Created a detailed tooltips that appear when users hover over each heatmap cell
    hovertext_matrix[i, j] <- paste0(
      "<b>Group:</b> ", rownames(heatmap_data_scaled)[i], "<br>",
      "<b>Impact:</b> ", colnames(heatmap_data_scaled)[j], "<br>",
      "<b>Z-Score:</b> ", round(heatmap_data_scaled[i, j], 2), "<br>",
      "<b>Impact Level:</b> ", level, "<br>",
      "<b>Raw Value:</b> ", round(heatmap_data[i, j], 2), "<br>",
      "<b>Age Range:</b> 20-79"
    )
  }
}

# Creating the interactive heatmap with corrected color palettes
heatmaply(
  heatmap_data_scaled,
  # Viridis color palette for main heatmap
  colors = viridis(256, option = "D"), 
  
 
  # Size of the indicator text
  fontsize_cell = 10, 
  
  
  # Dendrogram customisation
  k_row = 4,  # Suggested number of row clusters
  k_col = 3,  # Suggested number of column clusters
  
  # Row and column settings
  fontsize_row = 10,
  fontsize_col = 10,
  
  # Appearance customisations
  showticklabels = c(TRUE, TRUE),
  margin = c(80, 120, 80, 70),
  
  # Label formatting
  labRow = rownames(heatmap_data_scaled),
  labCol = colnames(heatmap_data_scaled),
  
  # Annotations and side colors
  row_side_colors = row_side_colors,
  
  # Use the pre-computed hovertext matrix instead of a function
  custom_hovertext = hovertext_matrix,
  
  # Dendrogram options
  dendrogram = "both",
  Rowv = TRUE,
  Colv = TRUE,
  
  # Main plot title and subtitle
  main = title,
  subplot_margin = 0,
  
  # Key display options
  key.title = "Z-Score",
  
  # Layout adjustments
  plot_method = "plotly",
  
  # Adding branches to dendrogram
  branches_lwd = 0.5,
  
  # interactive features
  width = 900,
  height = 700
) %>%
  # Adding source annotation
  plotly::layout(
    title = list(
      text = title,
      font = list(family = "Lobster Two", size = 22)
    ),
    annotations = list(
      list(
        x = 0.5,
        y = 1.06, 
        text = subtitle,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 12)
      ),
      list(
        x = 1,
        y = -0.2,
        text = "",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(family = "Lobster Two", size = 18)
      )
    )
  )





# Attempted to add the Lobster font into visual but ran into errors
font_add_google("Lobster", "lobster")

# Created a dataset focused specifically on age-related environmental effects
# Group the data by age, sex, and diet type, then calculate the mean of all metrics
age_effects <- df %>%
  group_by(age_group, sex, diet_group) %>%
  # Calculated the mean for all columns starting with "mean_"
  summarize(across(starts_with("mean_"), mean)) %>%
  #Removed grouping to avoid unexpected behavior
  ungroup()


# Created a heatmap visualisation focused on greenhouse gas emissions across age groups
ggplot(age_effects, aes(x = age_group, y = diet_group, fill = mean_ghgs)) +
  # Produced tiles with white borders for better visual separation between cells
  geom_tile(color = "white", size = 0.5) +  
  # This step involved splitting the visualisation by gender
  facet_wrap(~sex, labeller = labeller(sex = c("female" = "Female", "male" = "Male"))) +
  # Used the plasma color palette from viridis for better perception of values
  scale_fill_viridis_c(option = "plasma", name = "GHG Emissions") + 
  # Added a descriptive text elements to the visualisation
  labs(
    title = "How Greenhouse Gas Emissions Change with Age",
    subtitle = "Comparison across different diet types and gender groups",
    x = "Age Group",
    y = "Diet Type",
    caption = "Based on environmental impact dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5),
    plot.subtitle = element_text(family = "Helvetica", size = 12, hjust = 0.5),
    strip.text = element_text(family = "Helvetica", face = "bold", size = 12),
    # Light background for facet headers
    strip.background = element_rect(fill = "#f8f8f8", colour = "grey80"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Remove major grid lines for cleaner look
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Very light gray panel background
    panel.background = element_rect(fill = "#fbfbfb"),
    # Subtle cream background for entire plot
    plot.background = element_rect(fill = "#fbf9f4", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # This added text labels showing the actual GHG values for the min and max values in each gender group
  # This highlighted the extreme values to draw attention to the range
  geom_text(
    data = age_effects %>% 
      group_by(sex) %>% 
      # Select only min and max values
      filter(mean_ghgs == max(mean_ghgs) | mean_ghgs == min(mean_ghgs)),
    # Show the GHG value rounded to 1 decimal place
    aes(label = round(mean_ghgs, 1)), 
    color = "white", 
    fontface = "bold",
    size = 3
  )



