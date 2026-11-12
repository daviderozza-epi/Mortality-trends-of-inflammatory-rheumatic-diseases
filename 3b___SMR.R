# Crude ----
## globale ----
wt <- whoimp %>% 
  #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population

# comprimo l'età
wtage <- wt %>% group_by(.i, year, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  smr = mortality / population * 1e6
) %>% ungroup()

g3 <- ggplot(wtage %>% filter(!cause %in% c("CVD", "Neoplasms", "All causes") &
                              year >= REPR_YEARS[1] & year <= 2021
                                ), aes(x = year, y = smr, group = cause, color = cause, fill = cause)) +
  geom_smooth(size = 1, alpha = 0.3, se = T) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  scale_fill_manual(values = wes_palette("IsleofDogs1")) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
  scale_y_continuous(labels = scales::comma)+
  labs(
    x = "",
    y = "Crude mortality rate per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )
g3
jpeg(filename = "G:/Il mio Drive/UNIVPM/WHO/figure/global-crude-smr.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g3); dev.off()

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    smr = sum(smr, na.rm = T)
  ) %>% ungroup() %>%
  group_by(year, cause) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    smr = mortality / population * 1e6
  ) %>% ungroup()

setDT(WTS)[, crude := {
  rates <- smr
  if (rates > 0) {
    lower <- qchisq(0.025, 2 * rates) / 2
    upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  } else {
    lower <- 0
    upper <- -log(0.05)  # For rate = 0, use one-sided upper limit
  }
  
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

# average 2000-2019
t1_global <- WTS %>% group_by(cause) %>% summarise(
  smr = weighted.mean(smr, w = mortality),
  total_weight = sum(mortality),
  weighted_smr_sum = sum(smr * mortality)
) %>% 
  mutate(
    lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
    upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
    formatted = ifelse(smr > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted) %>%
  pivot_wider(names_from = cause, values_from = c(formatted))

t1_global <- data.frame("Stratification" = "Global", t1_global)

## per continente ----
wt <- whoimp %>%
  group_by(.i, year, cause, age, continent) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()

wt$smr <- wt$mortality / wt$population
WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2])  %>% 
  group_by(.i, year, cause, continent) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    smr = sum(smr, na.rm = T), 
  ) %>% ungroup() %>%
  group_by(year, cause, continent) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    smr = mortality / population * 1e6
  ) %>% ungroup()

setDT(WTS)[, crude := {
  rates <- smr
  if (rates > 0) {
    lower <- qchisq(0.025, 2 * rates) / 2
    upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  } else {
    lower <- 0
    upper <- -log(0.05)  # For rate = 0, use one-sided upper limit
  }
  
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]
# average 2000-2019
t1_continent <- WTS %>% group_by(cause, continent) %>% summarise(smr = weighted.mean(smr, w = mortality),
    total_weight = sum(mortality),
    weighted_smr_sum = sum(smr * mortality)
  ) %>% 
    mutate(
      lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
      upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
      formatted = ifelse(smr > 10,
                         sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                         sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
    ) %>% ungroup() %>% dplyr::select(cause, continent, formatted) %>%
    pivot_wider(names_from = cause, values_from = c(formatted))


## per zona ricchezza ----
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()

wt$smr <- wt$mortality / wt$population

wtrich <- wt %>% mutate(everything = 1) %>% group_by(.i, year, cause, world_bank) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  smr = mortality / population * 1e6)


# in che luogo?
# quando?
# per quali cause di morte?
g3 <- ggplot(wtrich %>% filter(cause != "All causes" & world_bank != "LI"), aes(x = year, y = smr, color = world_bank, fill = world_bank, group = world_bank)) +
  geom_smooth(size = 1, alpha = 0.3, se = F) +
  scale_color_manual(values = wes_palette("Moonrise3")) + 
  scale_fill_manual(values = wes_palette("Moonrise3")) + 
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Crude Mortality Rates",
    x = "",
    y = "smr per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + facet_wrap(~ cause, scales = "free_y")
jpeg(filename = "figure/crude-smr-wealth.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g3); dev.off()
g3

# tabelle
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), 
  ) %>% ungroup() %>%
  group_by(year, cause, world_bank) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population), smr = mortality / population * 1e6,
  ) %>% ungroup()

# format
setDT(WTS)[, crude := {
  rates <- smr
  lower <- qchisq(0.025, 2 * rates) / 2
  upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

# average 2000-2019
t1_wealth <- WTS %>% group_by(cause, world_bank) %>% summarise(smr = weighted.mean(smr, w = mortality),
    total_weight = sum(mortality),
    weighted_smr_sum = sum(smr * mortality)
  ) %>% 
    mutate(
      lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
      upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
      formatted = ifelse(smr > 10,
                         sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                         sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
    ) %>% ungroup() %>% dplyr::select(cause, world_bank, formatted) %>%
    pivot_wider(names_from = cause, values_from = c(formatted))



## per sesso ----
wt <- whoimp %>%
  group_by(.i, year, cause, sex) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()


wtsex <- wt %>% group_by(.i, year, sex, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), smr = mortality / population * 1e6
) %>% ungroup()


# in che luogo?
# quando?
# per quali cause di morte?
g3 <- ggplot(wtsex %>% filter(cause != "All causes"), aes(x = year, y = smr, color = sex, fill = sex, group = sex)) +
  geom_smooth(size = 1, alpha = 0.3, se = F) +
  scale_color_manual(values = wes_palette("AsteroidCity1")) + 
  scale_fill_manual(values = wes_palette("AsteroidCity1")) + 
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Crude Mortality Rates",
    x = "",
    y = "smr per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + facet_wrap(~ cause, scales = "free_y")
jpeg(filename = "figure/crude-smr-sex.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g3); dev.off()
g3


# tabelle

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, sex) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population) 
  ) %>% ungroup() %>%
  group_by(year, cause, sex) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    smr = mortality / population * 1e6
  ) %>% ungroup()

# format
setDT(WTS)[, crude := {
  rates <- smr
  lower <- qchisq(0.025, 2 * rates) / 2
  upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

# average 2000-2019
t1_sex <- WTS %>% group_by(cause, sex) %>% summarise(smr = weighted.mean(smr, w = mortality),
    total_weight = sum(mortality),
    weighted_smr_sum = sum(smr * mortality)
  ) %>% 
    mutate(
      lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
      upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
      formatted = ifelse(smr > 10,
                         sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                         sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
    ) %>% ungroup() %>% dplyr::select(cause, sex, formatted) %>%
    pivot_wider(names_from = cause, values_from = c(formatted))

## per fasce di età ----

wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  mutate(age_cut = ifelse(age %in% c("0-4", "5-9", "10-14", "15-19"), "0-19", 
                   ifelse(age %in% c("20-24", "25-29", "30-34", "35-39"), "20-39", 
                   ifelse(age %in% c("40-44", "45-49", "50-54", "55-59"), "40-59", 
                   ifelse(age %in% c("60-64", "65-69", "70-74"), "60-74",
                   ifelse(age %in% c("75-79", "80-84", "85-89", "90-94", "95-99"), "75+", NA)))))) %>%
  group_by(.i, year, cause, age_cut) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()

wtage <- wt %>% group_by(.i, year, age_cut, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  smr = mortality / population * 1e6
) %>% ungroup()

# in che luogo?
# quando?
# per quali cause di morte?
g3 <- ggplot(wtage %>% filter(cause != "All causes"), aes(x = year, y = smr, color = age_cut, fill = age_cut, group = age_cut)) +
  geom_smooth(size = 1, alpha = 0.3, se = F) +
  scale_color_manual(values = wes_palette("Rushmore1")) + 
  scale_fill_manual(values = wes_palette("Rushmore1")) + 
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Crude Mortality Rates",
    x = "",
    y = "smr per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + facet_wrap(~ cause, scales = "free_y")
jpeg(filename = "figure/crude-smr-age.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g3); dev.off()
g3

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, age_cut) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>%
  group_by(year, cause, age_cut) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population), smr = mortality / population * 1e6
  ) %>% ungroup()

setDT(WTS)[, crude := {
  rates <- smr
  lower <- qchisq(0.025, 2 * rates) / 2
  upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

# average 2000-2019
t1_age <- WTS %>% group_by(cause, age_cut) %>% summarise(smr = weighted.mean(smr, w = mortality),
    total_weight = sum(mortality),
    weighted_smr_sum = sum(smr * mortality)
  ) %>% 
    mutate(
      lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
      upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
      formatted = ifelse(smr > 10,
                         sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                         sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
    ) %>% ungroup() %>% dplyr::select(cause, age_cut, formatted) %>%
    pivot_wider(names_from = cause, values_from = c(formatted))
t1_age
## per nazione ----
wt <- whoimp %>% 
  group_by(.i, year, cause, age, country_name, continent, usability, usable_broad, usable_strict, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, country_name) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population) 
  ) %>% ungroup() %>%
  group_by(year, cause, country_name) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population), smr = mortality / population * 1e6
  ) %>% ungroup()

# 
setDT(WTS)[, crude := {
  rates <- smr
  lower <- qchisq(0.025, 2 * rates) / 2
  upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

t1_country <- WTS %>% group_by(cause, country_name) %>% summarise(smr = weighted.mean(smr, w = mortality),
    total_weight = sum(mortality),
    weighted_smr_sum = sum(smr * mortality)
  ) %>% 
    mutate(
      lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
      upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
      formatted = ifelse(smr > 10,
                         sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                         sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
    ) %>% ungroup() %>% dplyr::select(cause, country_name, formatted) %>%
    pivot_wider(names_from = cause, values_from = c(formatted)) %>%
  mutate(country_name = as.character(country_name)) %>% arrange(country_name) %>% rename(Stratification = country_name)
t1_country
## per global south e north ----
EU <- c("Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
        "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
global.south <- c("Argentina", "Australia", "Brazil", "Mexico", "South Africa", "Chile", "Venezuela", "Nicaragua", "Guatemala",
                  "Ecuador", "Colombia", "Cuba", "Guyana", "Jamaica", "Kyrgyzstan", "Panama", "Paraguay", "Philippines", "Suriname", "Thailand",
                  "Belize", "Uzbekistan", "Uruguay", "Georgia", "New Zealand", "Republic of Moldova", "Armenia", "Costa Rica")
global.north <- c("Canada", "Japan", EU, "Iceland", "Norway", "United Kingdom", "United States of America", "Turkey", "Bahamas", "Israel", "Switzerland", "Serbia")

wt <- whoimp %>% mutate(
  north_south = ifelse(country_name %in% global.north, "North", "South")
) %>%
  group_by(.i, year, cause, north_south) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()
wtage <- wt %>% group_by(.i, year, north_south, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), smr = mortality / population * 1e6
) %>% ungroup()

# in che luogo?
# quando?
# per quali cause di morte?
g3 <- ggplot(wtage %>% filter(cause != "All causes"), aes(x = year, y = smr, color = north_south, fill = north_south, group = north_south)) +
  geom_smooth(size = 1, alpha = 0.3, se = F) +
  scale_color_manual(values = c("navyblue", "orangered")) + 
  scale_fill_manual(values = c("navyblue", "orangered")) + 
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Crude Mortality Rates",
    x = "",
    y = "smr per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + facet_wrap(~ cause, scales = "free_y")
jpeg(filename = "figure/crude-smr-age.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g3); dev.off()
g3

WTS <- wt %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, north_south) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>%
  group_by(year, cause, north_south) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population), smr = mortality / population * 1e6,
  ) %>% ungroup()

setDT(WTS)[, crude := {
  rates <- smr
  lower <- qchisq(0.025, 2 * rates) / 2
  upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

# average 2000-2019
t1_northsouth <- WTS %>% group_by(cause, north_south) %>% summarise(smr = weighted.mean(smr, w = mortality),
    total_weight = sum(mortality),
    weighted_smr_sum = sum(smr * mortality)
  ) %>% 
    mutate(
      lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
      upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
      formatted = ifelse(smr > 10,
                         sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                         sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
    ) %>% ungroup() %>% dplyr::select(cause, north_south, formatted) %>%
    pivot_wider(names_from = cause, values_from = c(formatted))
t1_northsouth$Stra <- c("   Global North", "   Global South")
## per western eastern europe ----
west.EU <- c("Belgium", "Denmark", "France", "Germany", #"Greece", 
             "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "United Kingdom",
             "Austria", "Sweden", "Malta", "Finland", "Iceland", "Norway", "Switzerland")
east.EU <- c(setdiff(EU, west.EU), "Serbia", "Republic of Moldova")
wt <- whoimp %>% filter(country_name %in% c(east.EU, west.EU)) %>% mutate(
  east_west = ifelse(country_name %in% east.EU, "Eastern Europe", "Western Europe")
) %>%
  group_by(.i, year, cause, age, east_west) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()
wtage <- wt %>% group_by(.i, year, east_west, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), smr = mortality / population * 1e6
) %>% ungroup()

# in che luogo?
# quando?
# per quali cause di morte?
g3 <- ggplot(wtage %>% filter(cause != "All causes"), aes(x = year, y = smr, color = east_west, fill = east_west, group = east_west)) +
  geom_smooth(size = 1, alpha = 0.3, se = F) +
  scale_color_manual(values = c("green4", "orange")) + 
  scale_fill_manual(values = c("green4", "orange")) + 
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Crude Mortality Rates",
    x = "",
    y = "smr per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + facet_wrap(~ cause, scales = "free_y")
jpeg(filename = "figure/crude-smr-westeastEU.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g3); dev.off()
g3

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, east_west) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>%
  group_by(year, cause, east_west) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population), smr = mortality / population * 1e6
  ) %>% ungroup()

setDT(WTS)[, crude := {
  rates <- smr
  lower <- qchisq(0.025, 2 * rates) / 2
  upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

# average 2000-2019 ----
t1_east_west <- WTS %>% group_by(cause, east_west) %>% summarise(smr = weighted.mean(smr, w = mortality),
    total_weight = sum(mortality),
    weighted_smr_sum = sum(smr * mortality)
  ) %>% 
    mutate(
      lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
      upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
      formatted = ifelse(smr > 10,
                         sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                         sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
    ) %>% ungroup() %>% dplyr::select(cause, east_west, formatted) %>%
    pivot_wider(names_from = cause, values_from = c(formatted))
t1_east_west

t1_east_west$east_west <- paste0("   ", t1_east_west$east_west)
## STRATIFICATA PER ANNO ----
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>%
  group_by(year, cause) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population), smr = mortality / population * 1e6
  ) %>% ungroup()

# format
setDT(WTS)[, crude := {
  rates <- smr
  lower <- qchisq(0.025, 2 * rates) / 2
  upper <- qchisq(0.975, 2 * (rates + 1)) / 2
  if (rates > 10){
    sprintf("%.0f [%.0f-%.0f]", 
            round(rates, 0), 
            round(lower, 0), 
            round(upper, 0))
  } else {
    sprintf("%.2f [%.2f-%.2f]", 
            round(rates, 2), 
            round(lower, 2), 
            round(upper, 2))
  }
}, by = .I]

t1_years <- WTS %>% group_by(cause, year) %>% summarise(smr = weighted.mean(smr, w = mortality),
                                                        total_weight = sum(mortality),
                                                        weighted_smr_sum = sum(smr * mortality)
) %>% 
  mutate(
    lower = qchisq(0.025, 2 * weighted_smr_sum) / (2 * total_weight),
    upper = qchisq(0.975, 2 * (weighted_smr_sum + 1)) / (2 * total_weight),
    formatted = ifelse(smr > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(smr, 0), round(lower, 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(smr, 2), round(lower, 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, year, formatted) %>%
  pivot_wider(names_from = cause, values_from = c(formatted))

t1_years

# FINALE ----
t1_northsouth <- t1_northsouth[, 1:9]
t1_northsouth <- t1_northsouth[, 1:9]
t1_sex$sex <- ifelse(t1_sex$sex == "M", "Male", "Female")
t1_wealth$world_bank <- ifelse(t1_wealth$world_bank == "HI", "High income", ifelse(t1_wealth$world_bank == "UMI",
                                  "Upper-middle income", "Lower-middle income"))
h <- ncol(t1_age)-1
names(t1_global) <- names(t1_sex) <- names(t1_age) <- names(t1_continent) <- names(t1_wealth) <- names(t1_northsouth) <- names(t1_east_west) <- names(t1_years) <- names(t1_country)
t1 <- rbind(t1_global, t1_northsouth[, 1:8], c("Sex", rep(NA, h)), t1_sex, 
            c("Age class", rep(NA, h)), t1_age, c("Continent", rep(NA, h)), t1_continent[1:which(t1_continent$Stratification == "Europe"), ], 
            t1_east_west, t1_continent[(which(t1_continent$Stratification == "Europe")+1):nrow(t1_continent), ], 
            c("Wealth category", rep(NA, h)), t1_wealth,
            c("Calendar year", rep(NA, h)), t1_years, c("Country", rep(NA, h)), t1_country)
t1 <- as.data.frame(lapply(t1, function(col) {
  col[col %in% c("Inf [-Inf, Inf]", "Inf [Inf, Inf]")] <- "NA"
  col
}))
t1

