library(dplyr)
library(ggplot2)

ess <- readRDS("data/2022-11-24_ess.rds")

data_plot_income <- ess |> 
  filter(!is.na(income) & !is.na(pp_index)) |> 
  group_by(income) |> 
  summarize(pp_index = mean(pp_index))

data_plot_edu <- ess |> 
  filter(!is.na(edu) & !is.na(pp_index)) |> 
  group_by(edu) |> 
  summarize(pp_index = mean(pp_index))

data_plot_income |> 
  ggplot(aes(x = income, y = pp_index)) +
  geom_col()

data_plot_edu |> 
  ggplot(aes(x = edu, y = pp_index)) +
  geom_col()
