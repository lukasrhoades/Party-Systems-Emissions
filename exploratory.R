library(readxl)
library(tidyverse)
library(countries)
library(labelled)

# Initial -----------------------------------------------------------------

# Read in consumption emissions and melt data into tidy format
consumption_emissions <- read_excel(
  "National_Fossil_Carbon_Emissions_2024v1.0-1.xlsx",
  sheet = "Consumption Emissions",
  skip = 8
  ) |> 
  rename(year = `...1`) |> 
  pivot_longer(
    cols = !year,
    names_to = "country",
    values_to = "emissions"
  )

# Intial plot to see changes over time by country
consumption_emissions |> 
  ggplot(aes(x = year, y = emissions, group = country)) +
  geom_line()
# Some countries have actually reduced!

# Calculate emissions reductions
initial <- consumption_emissions |> 
  group_by(country) |> 
  filter(!is.na(emissions)) |> 
  rename(
    initial_emissions = emissions,
    initial_year = year
  ) |> 
  slice_head()

present <- consumption_emissions |> 
  group_by(country) |> 
  filter(!is.na(emissions)) |> 
  rename(
    present_emissions = emissions,
    present_year = year
  ) |> 
  slice_tail()

reductions <- full_join(initial, present, by = join_by(country)) |> 
  mutate(
    reduction = present_emissions - initial_emissions
  ) |> 
  relocate(initial_year, .before = initial_emissions)

# Which countries/regions have reduced most emissions?
reductions |> 
  arrange(reduction)
# Russia, Germany, Ukraine

# Which countries have increased their emissions the most?
reductions |> 
  arrange(desc(reduction))
# China, India, US

# Let's consider population now
populations <- read_csv("population.csv", skip = 4) |> 
  janitor::clean_names() |> 
  select(!c(country_code, indicator_name, indicator_code)) |> 
  rename(country = country_name) |> 
  pivot_longer(
    cols = !country,
    names_to = "year",
    values_to = "population"
  ) |> 
  mutate(
    year = parse_number(year)
  )

# Standardize country names
reductions <- reductions |> 
  filter(country != "Statistical Difference")
reductions$country <- country_name(
  reductions$country,
  to = "simple",
  na_fill = TRUE,
  verbose = TRUE
)
populations <- populations |> 
  filter(country != "Africa Eastern and Southern")
populations$country <- country_name(
  populations$country,
  to = "simple",
  na_fill = TRUE,
  verbose = TRUE
)

# Merge
reductions_per_capita <- left_join(
  reductions,
  populations,
  by = join_by(country == country, initial_year == year)
) |> 
  rename(initial_pop = population) |> 
  relocate(initial_pop, .before = initial_emissions) |> 
  left_join(
    populations,
    by = join_by(country == country, present_year == year)
  ) |> 
  rename(present_pop = population) |> 
  relocate(present_pop, .before = present_emissions) |> 
  mutate(
    initial_epc = initial_emissions / initial_pop,
    present_epc = present_emissions / present_pop,
    reduction_epc = present_epc - initial_epc
  ) |> 
  relocate(contains("epc"), .after = country) |> 
  filter(!is.na(initial_epc))

# Which countries have reduced emissions per capita the most?
reductions_per_capita |> 
  arrange(reduction_epc)
# Luxembourg, Bahrain, Kazakhstan

# Which countries have increased emissions per capita the most?
reductions_per_capita |> 
  arrange(desc(reduction_epc))
# Malta, Oman, Mongolia

# Consumption Emissions vs Effective # of Parties -------------------------

# Read in consumption emissions and melt data into tidy format
consumption_emissions <- read_excel(
  "National_Fossil_Carbon_Emissions_2024v1.0-1.xlsx",
  sheet = "Consumption Emissions",
  skip = 8
) |> 
  rename(year = `...1`) |> 
  pivot_longer(
    cols = !year,
    names_to = "country",
    values_to = "emissions"
  ) |> 
  # Filter out non-countries and countries with no data
  filter(!country %in% c("KP Annex B", "Non KP Annex B", "OECD", "Non-OECD",
                         "EU27", "Africa", "Asia", "Central America",
                         "Europe", "Middle East", "North America", "Oceania",
                         "South America", "Bunkers", "Statistical Difference",
                         "World")) |> 
  filter(!is.na(emissions))

# Standardize country names
consumption_emissions$country <- country_name(
  consumption_emissions$country,
  to = "simple"
)

# Read in other data
qog_data <- read_csv2("qogdata_12_05_2025.csv") |> 
  rename(
    country = cname
  ) |> 
  select(country, year, br_pres:wdi_ane)

qog_data$country <- country_name(
  qog_data$country,
  to = "simple"
)

# Merge data
merged_data <- left_join(
  consumption_emissions,
  qog_data,
  by = join_by(country == country, year == year)
) |> 
  relocate(country, 1) |> 
  arrange(country)

# Plot GDP per capita to emissions per capita
merged_data |> 
  ggplot(aes(x = wdi_gdpcapcur, y = emissions / wdi_pop)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Plot emissions per capita to % renewable energy
merged_data |> 
  ggplot(aes(x = wdi_ane, y = emissions / wdi_pop)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Plot emissions per capita over time
merged_data |> 
  ggplot(aes(x = year, y = emissions / wdi_pop, group = country)) +
  geom_line(aes(color = country)) +
  guides(color = "none")

# Lets check for democracies
merged_data |> 
  filter(bmr_dem == 1) |> 
  ggplot(aes(x = year, y = emissions / wdi_pop, group = country)) +
  geom_line(aes(color = country)) +
  guides(color = "none")

# And non-democracies
merged_data |> 
  filter(bmr_dem == 0) |> 
  ggplot(aes(x = year, y = emissions / wdi_pop, group = country)) +
  geom_line(aes(color = country)) +
  guides(color = "none")

# Survey data
survey_data <- read_rds("WVS_Time_Series_1981-2022_rds_v5_0.rds") |> 
  select(COUNTRY_ALPHA, S020, B001:B004,B008) |> 
  distinct(COUNTRY_ALPHA, S020, .keep_all = TRUE) |> 
  # Convert from haven_labelled to doubles
  mutate(
    S020 = unclass(S020),
    B001 = unclass(B001),
    B002 = unclass(B002),
    B003 = unclass(B003),
    B004 = unclass(B004),
    B008 = unclass(B008),
  ) |> 
  # Get rid of non-response
  mutate(
    B001 = case_when(
      B001 < 0 ~ NA,
      .default = B001
    ),
    B002 = case_when(
      B002 < 0 ~ NA,
      .default = B002
    ),
    B003 = case_when(
      B003 < 0 ~ NA,
      .default = B003
    ),
    B004 = case_when(
      B004 < 0 ~ NA,
      .default = B004
    ),
    B008 = case_when(
      B008 < 0 ~ NA,
      .default = B008
    )
  ) |> 
  # Filter out rows with no data
  filter(!if_all(B001:B008, is.na)) |> 
  # Combine survey response to get a score
  rowwise() |> 
  mutate(
    enviro_conscious = mean(c(B001, B002, B003, B004), na.rm = TRUE)
  ) |> 
  # Clean up column names and select key columns
  rename(
    country = COUNTRY_ALPHA,
    year = S020,
    env_or_growth = B008
  ) |> 
  select(country, year, env_or_growth, enviro_conscious)

survey_data$country = country_name(
  survey_data$country,
  to = "simple",
  fuzzy_match = FALSE,
  na_fill = TRUE
)

# Join to emissions/QOG merged data
merged_data <- left_join(
  merged_data,
  survey_data,
  by = join_by(country == country, year == year)
)

# Plot relationship between consciousness and emissions per capita
merged_data |> 
  ggplot(aes(x = enviro_conscious, y = emissions / wdi_pop)) +
  scale_x_reverse() +
  geom_point(aes(color = as.factor(env_or_growth))) +
  scale_color_manual(values = c("chartreuse3", "coral1", "deepskyblue1")) +
  geom_smooth(se = FALSE)
# Countries with highest enviro consciousness (1) prefer environment to growth
# Relationship is weak but more enviro conscious, fewer consumption emissions

# Plot relationship between consciousness and gdp per capita
merged_data |> 
  ggplot(aes(x = enviro_conscious, y = wdi_gdpcapcur)) +
  scale_x_reverse() +
  geom_point(aes(color = as.factor(env_or_growth))) +
  scale_color_manual(values = c("chartreuse3", "coral1", "deepskyblue1")) +
  geom_smooth(se = FALSE)
# Countries with higher gdp per capita tend to be more likely to prefer environment to economic growth

# Plot relationship between consciousness and renewable energy
merged_data |> 
  ggplot(aes(x = enviro_conscious, y = wdi_ane)) +
  scale_x_reverse() +
  geom_point(aes(color = as.factor(env_or_growth))) +
  scale_color_manual(values = c("chartreuse3", "coral1", "deepskyblue1")) +
  geom_smooth(se = FALSE)
# All countries above 40% renewable energy prefer environment to growth

# Plot enviro consciousness in each country over time
merged_data |> 
  filter(!is.na(enviro_conscious)) |> 
  ggplot(aes(x = year, y = enviro_conscious, group = country)) +
  geom_line(aes(color = country)) +
  guides(color = "none")

# Plot effective # of parties vs emissions per capita
merged_data |> 
  filter(bmr_dem == 1) |> 
  ggplot(aes(x = gol_enpp1, y = emissions / wdi_pop)) +
  geom_point(aes(color = as.factor(iaep_es))) +
  geom_smooth(method = "lm", se = FALSE)

# Next step: use wave and then either average other statistics over that period or clone the enviro consciousness to multiple rows
survey_data <- read_rds("WVS_Time_Series_1981-2022_rds_v5_0.rds") |> 
  select(COUNTRY_ALPHA, S002VS, B001:B004,B008) |> 
  distinct(COUNTRY_ALPHA, S002VS, .keep_all = TRUE) |> 
  # Convert from haven_labelled to doubles
  mutate(
    S002VS = unlabelled(S002VS),
    B001 = unclass(B001),
    B002 = unclass(B002),
    B003 = unclass(B003),
    B004 = unclass(B004),
    B008 = unclass(B008),
  ) |> 
  # Get rid of non-response
  mutate(
    B001 = case_when(
      B001 < 0 ~ NA,
      .default = B001
    ),
    B002 = case_when(
      B002 < 0 ~ NA,
      .default = B002
    ),
    B003 = case_when(
      B003 < 0 ~ NA,
      .default = B003
    ),
    B004 = case_when(
      B004 < 0 ~ NA,
      .default = B004
    ),
    B008 = case_when(
      B008 < 0 ~ NA,
      .default = B008
    )
  ) |> 
  # Filter out rows with no data
  filter(!if_all(B001:B008, is.na)) |> 
  # Combine survey response to get a score
  rowwise() |> 
  mutate(
    enviro_conscious = mean(c(B001, B002, B003, B004), na.rm = TRUE),
    S002VS = as.character(S002VS) 
  ) |> 
  # Clean up column names and select key columns
  rename(
    country = COUNTRY_ALPHA,
    wave = S002VS,
    env_or_growth = B008
  ) |> 
  select(country, wave, env_or_growth, enviro_conscious) |> 
  # Clone the rows
  rowwise() |> 
  mutate(
    wave = map(
      as.integer(strsplit(wave, "-")[[1]][1]),
      seq,
      to = as.integer(strsplit(wave, "-")[[1]][2])
    )
  ) |> 
  unnest(cols = c(wave)) |> 
  rename(year = wave)

survey_data$country = country_name(
  survey_data$country,
  to = "simple",
  fuzzy_match = FALSE,
  na_fill = TRUE
)

# Now merge with master dataset
master_data <- left_join(
  merged_data,
  survey_data,
  by = join_by(country == country, year == year)
)

# Try modeling
summary(
  lm(
    scale(emissions / wdi_pop) ~ scale(gol_enpp1) + scale(enviro_conscious) + scale(wdi_gdpcapcur),
    master_data |> filter(bmr_dem == 1)
  )
)

# Next: turn effective number of parties into binary, or use electoral systems
# Also control for other things 

# Consumption Emissions vs Share of Green ---------------------------------

qog_data <- read_csv2("qogdata_12_05_2025-2.csv") |> 
  rename(
    country = cname
  ) |> 
  select(country, year, br_pres:undp_hdi)

qog_data$country <- country_name(
  qog_data$country,
  to = "simple"
)

# Merge with emissions data
master_data <- left_join(
  consumption_emissions,
  qog_data,
  by = join_by(country == country, year == year)
)

# Plot relationship between green party share of parliament & emissions
master_data |> 
  ggplot(aes(x = cpds_lg, y = emissions / wdi_pop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 

# Plot relationship between green party share & renewable energy mix
master_data |> 
  ggplot(aes(x = cpds_lg, y = wdi_ane)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 

# Model emissions per capita and share of green
summary(
  lm(
    scale(emissions / wdi_pop) ~ scale(cpds_lg) + scale(wdi_gdpcapcur) + scale(undp_hdi),
    master_data
  )
)
# Controlling for development reduces the positive influence of share of green parties 

# Let's reintroduce enviro consciousness
master_data <- left_join(
  master_data,
  survey_data,
  by = join_by(country == country, year == year)
)

# Another model
summary(
  lm(
    scale(emissions / wdi_pop) ~ scale(cpds_lg) + scale(wdi_gdpcapcur) + scale(undp_hdi) + scale(enviro_conscious),
    master_data
  )
)
# Higher share of green now accounts for fewer emissions per capita
