library(dplyr)
library(ggplot2)
library(haven)

# Load functions ---------------------------------------------------------------

recode_polpart <- function(var){
  newvar <- as.numeric(NA)
  newvar[var == 1] <- 1
  newvar[var == 2] <- 0
  newvar
}

# Load data --------------------------------------------------------------------

ess_raw <- read_dta("data_raw/ESS9DE.dta", 
                    encoding = "latin1")

# Political participation battery in the ESS -----------------------------------
# vote     Voted last national election
# contplt  Contacted politician or government official last 12 months
# wrkprty  Worked in political party or action group last 12 months
# wrkorg   Worked in another organisation or association last 12 months
# badge    Worn or displayed campaign badge/sticker last 12 months
# sgnptit  Signed petition last 12 months
# pbldmn   Taken part in lawful public demonstration last 12 months
# bctprd   Boycotted certain products last 12 months
# pstplonl Posted or shared anything about politics online last 12 months

# Other variables --------------------------------------------------------------
# gndr     Gender
# agea     Age of respondent, calculated
# netustm  Internet use, how much time on typical day, in minutes
# hinctnta Household's total net income, all sources

# Coding of 'region' -----------------------------------------------------------
# DE1	Baden-Württemberg
# DE2	Bayern
# DE3	Berlin
# DE4	Brandenburg
# DE5	Bremen
# DE6	Hamburg
# DE7	Hessen
# DE8	Mecklenburg-Vorpommern
# DE9	Niedersachsen
# DEA	Nordrhein-Westfalen
# DEB	Rheinland-Pfalz
# DEC	Saarland
# DED	Sachsen
# DEE	Sachsen-Anhalt
# DEF	Schleswig-Holstein
# DEG	Thüringen

ess <- ess_raw %>%
  mutate(
    across(.cols = c(contplt,
                     wrkprty,
                     wrkorg,
                     badge,
                     sgnptit,
                     pbldmn,
                     bctprd,
                     pstplonl),
           .fns = recode_polpart)
  ) %>%
  mutate(vote = case_when(vote == 1 ~ 1,
                          vote == 2 ~ 0),
         pp_index = contplt +
           wrkprty +
           wrkorg +
           badge +
           sgnptit + 
           pbldmn +
           bctprd +
           pstplonl +
           vote, 
         gndr = case_when(gndr == 1 ~ "Männlich",
                          gndr == 2 ~ "Weiblich"),
         gndr = factor(gndr, levels = c("Männlich", "Weiblich")),
         agea = as.numeric(agea),
         region_de = if_else(
           region %in% c("DE3", "DE4", "DE8", "DED", "DEE", "DEG"), 
           "Ost", 
           "West"),
         region_de = factor(region_de, levels = c("West", "Ost")),
         hinctnta = case_when(hinctnta %in% 1:2 ~ "0 bis 1.560",
                              hinctnta %in% 3:4 ~ "1.561 bis 2.330",
                              hinctnta %in% 5:6 ~ "2.331 bis 3.200",
                              hinctnta %in% 7:8 ~ "3.201 bis 4.470",
                              hinctnta %in% 9:10 ~ "4.470 oder mehr"),
         hinctnta = as.factor(hinctnta),
         edu = case_when(eisced %in% c(1:2) ~ "Niedrig", 
                         eisced %in% c(3:4) ~ "Mittel",
                         eisced %in% c(5:7) ~ "Hoch"),
         edu = factor(edu, levels = c("Niedrig", "Mittel", "Hoch"))
  ) %>%
  rename(gender = gndr, 
         age = agea,
         income = hinctnta) %>%
  select(
    pp_index,
    gender,
    age,
    income,
    edu,
    region_de
  )


# Add labels --------------------------------------------------------------

attr(ess$pp_index, "label") <- "Partizipationsindex"
attr(ess$gender, "label") <- "Gender"
attr(ess$age, "label") <- "Alter"
attr(ess$income, "label") <- "Haushaltseinkommen"
attr(ess$edu, "label") <- "Höchster Bildungsgrad"
attr(ess$region_de, "label") <- "West-/Ostdeutschland"


# Grafiken  ---------------------------------------------------------------

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
  geom_text(nudge_y = 0.1) + 
  labs(x = "Haushaltseinkommen",
       y = "Anzahl politischer Aktivitäten") +
  theme_bw() +
  theme(text = element_text(size = 15))

ggsave("figures/2022-11-24_income.png")

data_plot_edu |> 
  ggplot(aes(x = edu, y = pp_index_mean, label = pp_index_mean_lbl)) +
  geom_col() +
  geom_text(nudge_y = 0.05) +
  labs(x = "Höchster Bildungsgrad",
       y = "Anzahl politischer Aktivitäten") +
  theme_bw()

ggsave("figures/2022-11-24_edu.png")
