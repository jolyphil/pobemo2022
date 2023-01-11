library(dplyr)
library(haven)

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
# eisced   Education
# hinctnta Household's total net income, all sources
# region   Region

ess <- ess_raw %>%
  mutate(vote = case_when(vote == 1 ~ "Ja",
                          vote == 2 ~ "Nein",
                          vote == 3 ~ "Nicht wahlberechtigt"),
         vote = factor(vote),
         boycott = case_when(bctprd == 1 ~ "Ja",
                             bctprd == 2 ~ "Nein"),
         boycott = factor(boycott), 
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
    vote,
    boycott,
    gender,
    age,
    income,
    edu,
    region_de
  )


# Add labels --------------------------------------------------------------

attr(ess$vote, "label") <- "Wahlbeteiligung"
attr(ess$boycott, "label") <- "Boykott"
attr(ess$gender, "label") <- "Gender"
attr(ess$age, "label") <- "Alter"
attr(ess$income, "label") <- "Haushaltseinkommen"
attr(ess$edu, "label") <- "Höchster Bildungsgrad"
attr(ess$region_de, "label") <- "West-/Ostdeutschland"

# Save data --------------------------------------------------------------------

saveRDS(ess, file = "data/ess_2023-01-12.rds")
