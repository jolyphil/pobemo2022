library(countrycode) # Harmonize country codes
library(dplyr)       # Data transformation
library(readr)       # Import parse functions to transform population into numeric
library(readxl)      # Import Excel files
library(rvest)       # Import HTML tables
library(wbstats)     # Import World Bank data

# Load functions ----------------------------------------------------------

# Create function to systematically rename variables in the dataset
fix_varnames <- function(df){
  colnames(df) <- colnames(df) %>% 
    tolower() %>% 
    gsub(pattern = "\\s", replacement = "_")
  df
}

# Create function to recode percent as numeric
percent_as_numeric <- function(var) {
  var %>% 
    gsub(pattern = "\\s%", replacement = "") %>% 
    as.numeric()
}


# Import IDEA raw data ----------------------------------------------------

idea_raw <- read_xls("data_raw/idea_export_40_63da3e808bc7e.xls")


# Clean IDEA data ---------------------------------------------------------

idea <- idea_raw %>% 
  fix_varnames() %>% 
  filter(election_type == "Parliamentary") %>% 
  select(country, year, voter_turnout, compulsory_voting, population) %>% 
  group_by(country) %>% 
  filter(year >= 2015 & year <= 2021) %>% 
  slice_max(year) %>% # Keep last election between 2015 and 2021
  ungroup() %>% 
  mutate(voter_turnout = percent_as_numeric(voter_turnout),
         population = parse_number(population) / 1000000, # Population in millions
         # Generate country codes for merging with VDem and World Bank
         ccode = countrycode(country,
                             origin = "country.name",
                             destination = "iso3c"), 
         compulsory_voting = factor(compulsory_voting)
  ) %>% 
  rename(turnout = voter_turnout) %>% 
  filter(!is.na(turnout)) %>% 
  relocate(ccode, .after = country)

# Import World Bank Data --------------------------------------------------

# Variables:
# NY.GDP.PCAP.PP.KD: GDP per capita, PPP (constant 2017 international $) 
# NY.GDP.MKTP.KD.ZG: GDP growth (annual %)

wb_raw <- wb_data(indicator = c("NY.GDP.PCAP.PP.KD", 
                                "NY.GDP.MKTP.KD.ZG",
                                "SL.UEM.TOTL.ZS"), 
                  start_date = 2015,
                  end_date = 2021)

wb <- wb_raw %>%
  select(iso2c, 
         date, 
         NY.GDP.PCAP.PP.KD,
         NY.GDP.MKTP.KD.ZG,
         SL.UEM.TOTL.ZS) %>% 
  rename(ccode = iso2c, 
         year = date,
         gdp_cap = NY.GDP.PCAP.PP.KD, 
         gdp_growth = NY.GDP.MKTP.KD.ZG,
         unemp = SL.UEM.TOTL.ZS) %>% 
  mutate(ccode = countrycode(ccode,
                             origin = "iso2c",
                             destination = "iso3c"), 
         gdp_cap = gdp_cap / 1000)


# Import V-Dem raw data ---------------------------------------------------

vdem_raw <- readRDS("data_raw/V-Dem-CY-Full+Others-v12.rds")
  
vdem <- vdem_raw %>% 
  select(country_text_id, 
         year, 
         v2x_polyarchy, 
         v2x_corr, 
         v2x_regime,
         v2elparlel)  %>% 
  rename(ccode = country_text_id,
         dem_index = v2x_polyarchy,
         corruption = v2x_corr,
         regime = v2x_regime,
         elect_system = v2elparlel) %>% 
  mutate(regime = case_when(regime == 0 ~ "Closed autocracy",
                            regime == 1 ~ "Electoral autocracy",
                            regime == 2 ~ "Electoral democracy",
                            regime == 3 ~ "Liberal democracy"),
         regime = factor(regime, levels = c("Closed autocracy",
                                            "Electoral autocracy",
                                            "Electoral democracy",
                                            "Liberal democracy")),
         elect_system = case_when(elect_system == 0 ~ "Majoritarian",
                                  elect_system == 1 ~ "Proportional",
                                  elect_system == 2 ~ "Mixed"),
         elect_system = if_else(ccode == "NLD" & year == 2021, # Fix missing value
                                "Proportional",
                                elect_system),
         elect_system = factor(elect_system, levels = c("Majoritarian",
                                                        "Proportional",
                                                        "Mixed")))


# Extract number of parties from Wikipedia --------------------------------

html <- read_html("https://en.wikipedia.org/wiki/Effective_number_of_parties")

wiki_tables <- html %>% 
  html_elements(".wikitable") %>% 
  html_table()

parties <- wiki_tables[[2]] %>%
  rename(country = Country,
         year = Year,
         num_parties = `Effective number of parties`) %>%
mutate(ccode = countrycode(country,
                           origin = "country.name",
                           destination = "iso3c")) %>%
  select(ccode, 
         year, 
         num_parties)

# Merge datasets ----------------------------------------------------------

main <- idea %>% 
  left_join(vdem, by = c("ccode", "year")) %>% 
  # Keep only observations for electoral and liberal democracies
  filter(regime == "Electoral democracy" | regime == "Liberal democracy") %>% 
  select(-c(regime)) %>% 
  left_join(parties, by = c("ccode", "year")) %>%
  left_join(wb, by = c("ccode", "year")) 
  


# Add variable labels -----------------------------------------------------

attr(main$country, "label") <- "Country"
attr(main$ccode, "label") <- "Country code, ISO-3C"
attr(main$year, "label") <- "Year"
attr(main$turnout, "label") <- "Voter turnout based on registered voters"
attr(main$compulsory_voting, "label") <- "Compulsory voting"
attr(main$population, "label") <- "Population, in millions"
attr(main$dem_index, "label") <- "Electoral democracy index"
attr(main$corruption, "label") <- "Corruption index"
attr(main$elect_system, "label") <- "Electoral system"
attr(main$num_parties, "label") <- "Effective number of"
attr(main$gdp_cap, "label") <- "GDP per capita, PPP (thousands of dollars)"
attr(main$gdp_growth, "label") <- "GDP growth (%)"
attr(main$unemp, "label") <- "Unemployment, total (%)"

# Save dataset ------------------------------------------------------------

saveRDS(main, "data/idea_2023-02-02.rds")
