library(dplyr)
library(ggplot2)

ess <- readRDS("data/2022-11-24_ess.rds")

data_plot_income <- ess |> 
  filter(!is.na(income) & !is.na(pp_index)) |> 
  group_by(income) |> 
  summarize(pp_index_mean = mean(pp_index)) |> 
  mutate(pp_index_mean_lbl = sprintf(pp_index_mean, fmt = "%.1f"),
         pp_index_mean_lbl = gsub(pattern = "\\.", 
                                  replacement = ",", 
                                  x = pp_index_mean_lbl)) # Fix decimal comma
data_plot_edu <- ess |> 
  filter(!is.na(edu) & !is.na(pp_index)) |> 
  group_by(edu) |> 
  summarize(pp_index_mean = mean(pp_index)) |> 
  mutate(pp_index_mean_lbl = sprintf(pp_index_mean, fmt = "%.1f"),
         pp_index_mean_lbl = gsub(pattern = "\\.", 
                                  replacement = ",", 
                                  x = pp_index_mean_lbl)) # Fix decimal comma

data_plot_income |> 
  ggplot(aes(x = income, y = pp_index_mean, label = pp_index_mean_lbl)) +
  geom_col() + 
  geom_text(nudge_y = 0.05) + 
  labs(x = "Haushaltseinkommen",
       y = "Anzahl politischer Aktivitäten") +
  theme_bw()

ggsave("figures/2022-11-24_income.png")

data_plot_edu |> 
  ggplot(aes(x = edu, y = pp_index_mean, label = pp_index_mean_lbl)) +
  geom_col() +
  geom_text(nudge_y = 0.05) +
  labs(x = "Höchster Bildungsgrad",
       y = "Anzahl politischer Aktivitäten") +
  theme_bw()

ggsave("figures/2022-11-24_edu.png")
