library(dplyr)
library(haven)

# Load data ---------------------------------------------------------------

ess_raw <- read_dta("data_raw/ESS9DE.dta", encoding = "latin1")


# Recode and select variables ---------------------------------------------

ess <- ess_raw %>%
  mutate(
    
    # Wahlbeteiligung
    vote = case_when(vote == 1 ~ "Ja",
                     vote == 2 ~ "Nein",
                     vote == 3 ~ "Nicht wahlberechtigt"),
    vote = factor(vote),
    
    # Boykott
    boycott = case_when(bctprd == 1 ~ "Ja",
                        bctprd == 2 ~ "Nein"),
    boycott = factor(boycott),
    
    # Bildung 
    edu = case_when(eisced %in% c(1:2) ~ "Niedrig", 
                    eisced %in% c(3:4) ~ "Mittel",
                    eisced %in% c(5:7) ~ "Hoch"),
    edu = factor(edu, levels = c("Niedrig", "Mittel", "Hoch")),
    
    # Ost-/West-Deutschland
    region = if_else(
      region %in% c("DE3", "DE4", "DE8", "DED", "DEE", "DEG"), 
      "Ost", 
      "West"),
    region = factor(region, levels = c("West", "Ost"))
    
  )  %>%
  select(
    vote,
    boycott,
    edu,
    region
  )


# Add labels --------------------------------------------------------------

attr(ess$vote, "label") <- "Wahlbeteiligung"
attr(ess$boycott, "label") <- "Boykott"
attr(ess$edu, "label") <- "Höchster Bildungsgrad"
attr(ess$region, "label") <- "West-/Ostdeutschland"

# Save data --------------------------------------------------------------------

saveRDS(ess, file = "data/ess_2023-01-12.rds")
