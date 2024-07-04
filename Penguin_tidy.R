# https://towardsdatascience.com/ten-up-to-date-ways-to-do-common-data-tasks-in-r-4f15e56c92d
# https://gist.github.com/keithmcnulty
library(tidyverse)
library(palmerpenguins)
library(tidylog)

penguins

#1. Selecting columns in data
penguins %>% 
  dplyr::select(!contains("_"), starts_with("bill"))

#2. Reordering columns in data
penguins <- penguins %>% 
  dplyr::relocate(contains("_"), .after = year)

penguins

#3. Controlling mutated column location
penguins_id <- penguins %>% 
  dplyr::group_by(species, island, sex, year) %>% 
  dplyr::mutate(penguinid = row_number(), .before = contains("_"))

penguins_id

#4. Transforming from wide to long
penguins_long <- penguins_id %>% 
  tidyr::pivot_longer(contains("_"), # break out the measurement cols
                      names_to = c("part", "measure", "unit"), # break them into these three columns
                      names_sep = "_") #  use the underscore to separate

penguins_long

#5. Transforming from long to wide
penguins_wide <- penguins_long %>% 
  tidyr::pivot_wider(names_from = c("part", "measure", "unit"), # pivot these columns
                     values_from = "value", # take the values from here
                     names_sep = "_") # combine col names using an underscore

penguins_wide

#6. Running group statistics across multiple columns
penguin_stats <- penguins %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(across(ends_with("mm"), # do this for any column ending in mm
                          list(~mean(.x, na.rm = TRUE), ~sd(.x, na.rm = TRUE)))) # calculate a mean and sd

penguin_stats

#7. Control how output columns are named when summarising across multiple columns
penguin_stats <- penguins %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(across(ends_with("mm"), 
                          list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), # name summary functions
                          .names = "{gsub('_|_mm', '', col)}_{fn}")) # structure for summarised column names

penguin_stats

#8. Running models across subsets of data
penguin_models <- penguins %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(model = list(lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm)))  # store models in a list column

penguin_models


library(broom)
penguin_models <- penguins %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(broom::glance(lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm))) # summarise model stats

penguin_models

#9. Nesting data
penguins %>% 
  dplyr::group_by(species) %>% 
  tidyr::nest() %>% 
  dplyr::rowwise()

# Alternate
penguins %>% 
  nest_by(species)

#10. Graphing across subsets
# generic function for generating a simple scatter plot in ggplot2
scatter_fn <- function(df, col1, col2, title) {
  df %>% 
    ggplot2::ggplot(aes(x = {{col1}}, y = {{col2}})) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::labs(title = title)
}

# run function across species and store plots in a list column
penguin_scatters <- penguins %>% 
  dplyr::nest_by(species) %>% 
  dplyr::mutate(plot = list(scatter_fn(data, bill_length_mm, bill_depth_mm, species))) 

penguin_scatters

library(patchwork)
# generate scatter for entire dataset
p_all <- scatter_fn(penguins, bill_length_mm, bill_depth_mm, "All Species") 

# get species scatters from penguin_scatters dataframe
for (i in 1:3) {
  assign(paste("p", i, sep = "_"),
         penguin_scatters$plot[i][[1]]) 
}

# display nicely using patchwork in R Markdown
p_all /
  (p_1 | p_2 | p_3)
