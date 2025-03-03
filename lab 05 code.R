########################################################################
#        Lab 05 Code file
########################################################################
########################################################################


########################################################################
#step 1: Write a function that uses tidyverse to determine whether 
# allentown is out of range, unusual, or within the range for each band
# start w? overall_loudness before trying to apply to whole data set
########################################################################

install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(xtable)

allentown.data <- read_csv("data/essentia.data.allentown.csv")
essentia.data <- read_csv("data/essentia.data.csv")
  

#function that takes the current feature, and generates a comparison result for allentown vs. the three artists, and returns the comparisions
compare <- function(feature, allen.feature){
  essentia.data |>
    group_by(artist) %>%
    summarize(
      min = min(get(feature), na.rm=T),
      q1 = quantile(get(feature), .25, na.rm=T),
      q3 = quantile(get(feature), .75, na.rm=T),
      qr = q3-q1,
      LF = q1 - (1.51*qr),
      UF = q3 + (1.51*qr),
      max = max(get(feature), na.rm=T)
    ) %>%
    mutate(out.of.range = (min > allen.feature)|(max< allen.feature),
           unusual = (LF>allen.feature)|(max<allen.feature),
    ) %>%
    mutate(
      description = case_when(out.of.range==TRUE~ "Out of Range",
                              unusual == TRUE~ "Outlying",
                              TRUE ~ "Within Range")
    )%>%
    select(artist, description)
}

# goes through all different features for both the essentia data and the allentown line of data
# and calls func() for each feature to determine if allentown is in/out/close to the range for the current feature
num.of.cols <- ncol(essentia.data)

#make data frame to store "description" into every time, this is what rest of the lab is for
comparison.output.df <- tibble(feature =character(), `The Front Bottoms` = character(), `All Get Out`= character(), `Manchester Orchestra`=character())

for (i in 4:num.of.cols){
  feature = colnames(essentia.data)[i] 
  if (!is.numeric(essentia.data[[feature]])){
    next
  }
  allen.feature = allentown.data[[feature]]
  description = compare(feature, allen.feature) |> #outputs a table, col is either artist/description, row is for each artist and description
    pivot_wider(id_cols=everything(),
                names_from = artist,
                values_from = description)  |>
    mutate(feature = feature) |>
    select(feature, everything())
  
  
  comparison.output.df <-  comparison.output.df|>
    bind_rows(description)
} 

view(comparison.output.df)

features.to.keep <- c()

for(i in 1:nrow(comparison.output.df)){
  if (comparison.output.df[[2]][i] == "Outlying" || comparison.output.df[[3]][i] == "Outlying" || comparison.output.df[[4]][i] == "Outlying"){
    features.to.keep <- c(features.to.keep, comparison.output.df[[1]][i])
  }
}

(features.to.keep)

comparison.output.df <-comparison.output.df|>
  filter(comparison.output.df$feature == "overall_loudness" | comparison.output.df$feature== "spectral_entropy" | comparison.output.df$feature=="dissonance" | comparison.output.df$feature== "average_loudness" | comparison.output.df$feature == "Authentic")
(comparison.output.df)



# Reshape data into long format for plotting
all_data_long <- essentia.data %>%
  select(artist, all_of(features.to.keep)) %>%
  pivot_longer(cols = -artist, names_to = "feature", values_to = "value") %>%
  mutate(is_allentown = ifelse(artist == "Allentown", TRUE, FALSE))

# Add a column for the Allentown song feature value to use as reference
allen_values <- allentown.data %>%
  select(all_of(features.to.keep)) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "allen_value")

all_data_long <- all_data_long %>%
  left_join(allen_values, by = "feature")

# Create the ggplot to visualize the data
ggplot(all_data_long, aes(x = artist, y = value, color = artist)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6) + 
  geom_point(aes(color = artist), size = 2, alpha = 0.3) + 
  geom_hline(aes(yintercept = allen_value, color = "Allentown"), linetype = "dashed", size = 1.0) + 
  facet_wrap(~feature, scales = "free_y") + 
  scale_color_manual(values = c("The Front Bottoms" = "#1f77b4", 
                                "All Get Out" = "#ff7f0e", 
                                "Manchester Orchestra" = "#2ca02c",
                                "Allentown" = "#d62728")) + 
  theme_minimal() +
  labs(
    title = "Comparison of Features: Allentown vs Artists",
    y = "Feature Value",
    x = "Artist",
    color = "Artist"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8), 
    legend.position = "top", 
    legend.title = element_blank(),
    legend.text = element_text(size = 8), 
  ) +
  guides(color = guide_legend(override.aes = list(size = 5)))