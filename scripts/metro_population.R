## Load packages and data ----------------------------------

# Packages
library(dplyr)
library(forcats)
library(glue)
library(ggplot2)
library(ggtext)
library(showtext)

# Data
pop_data <- read.csv("data/pop_data.csv")


## Cleaning ------------------------------------------------
pop_data$year_2023 <- as.numeric(pop_data$year_2023)

pop_data <- pop_data %>% 
  mutate(category = ifelse(percent_diff >= 0, "positive", "negative"))

pop_data_2mil <- pop_data %>% 
  filter(year_2020 >= 2000000)


## Plotting ------------------------------------------------

# Add fonts
font_add_google("Roboto")
showtext_auto()

# Plot
plot_population <- pop_data_2mil %>% 
  ggplot(mapping = aes(x = percent_diff, y = fct_reorder(msa_short, percent_diff), color = category, label = glue("{msa_short}  {percent_diff}%"))) +
  theme_classic() +
  theme(
    text = element_text(family = "Roboto", face = "bold"),
    panel.background = element_rect(fill = "#FEFAEA"),
    plot.background = element_rect(fill = "#FEFAEA"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), 
    axis.text = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_markdown(size = 18, color = "gray50", hjust = 0, margin = margin(t = 5, b = 15)),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1, margin = margin(t = -20)),
    legend.position = "none"
  ) +
  labs(
    title = "Since 2020, many major U.S. cities have seen population <span style = 'color:#041E42;'>**growth**</span>, but some<br> have experienced <span style = 'color:#8C1515;'>**decline**</span>.", 
    caption = c("Data: Wikipedia ('20 to '23, 2m+ pop)\nViz: Tim Fulton, PhD")
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 1,
    color = "gray80"
  ) +
  geom_segment(
    mapping = aes(xend = 0, yend = fct_reorder(msa_short, percent_diff)),
    linewidth = 1.5
  ) +
  geom_label(
    fill = ifelse(pop_data_2mil$category == "positive", "#041E42", "#8C1515"),
    color = "#FEFAEA",
    label.padding = unit(0.35, "lines"),
    hjust = ifelse(pop_data_2mil$category == "positive", 0, 1)
  ) +
  scale_x_continuous(limits = c(-7, 10)) +
  scale_color_manual(values = c("#8C1515", "#041E42"))


# Save plot
ggsave("plots/population.png", 
       plot = plot_population, 
       width = 9.5, 
       height = 10.5, 
       dpi = 600,
       units = "in")
