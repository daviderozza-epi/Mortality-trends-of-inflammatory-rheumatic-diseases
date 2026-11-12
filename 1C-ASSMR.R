# SMR ----
EU <- c("Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
        "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
global.south <- c("Argentina", "Australia", "Brazil", "Mexico", "South Africa", "Chile", "Venezuela", "Nicaragua", "Guatemala",
                  "Ecuador", "Colombia", "Cuba", "Guyana", "Jamaica", "Kyrgyzstan", "Panama", "Paraguay", "Philippines", "Suriname", "Thailand",
                  "Belize", "Uzbekistan", "Uruguay", "Georgia", "New Zealand", "Republic of Moldova", "Armenia", "Costa Rica")
global.north <- c("Canada", "Japan", EU, "Iceland", "Norway", "United Kingdom", "United States of America", "Turkey", "Bahamas", "Israel", "Switzerland", "Serbia")
west.EU <- c("Belgium", "Denmark", "France", "Germany", #"Greece", 
             "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "United Kingdom",
             "Austria", "Sweden", "Malta", "Finland", "Iceland", "Norway", "Switzerland")
east.EU <- c(setdiff(EU, west.EU), "Serbia", "Republic of Moldova")
# popolazioni standard ...----
STD_POPbyage <- pop_adj %>% dplyr::select(`Region, subregion, country or area *`, Year:`100+`) %>% 
  rename(year = Year, country_name = `Region, subregion, country or area *`) %>%
  filter(country_name %in% "World" & year == 2020) %>%
  pivot_longer(
    cols = matches("\\d+-\\d+|100\\+"),
    names_to = "age",
    values_to = "population"
  ) %>% mutate( population = as.numeric(population) * 1000, # originally in thousands
                age = case_when(
                  age == "100+" ~ "95-99",
                  TRUE ~ age
                )) %>%
  group_by(across(-population), age) %>%
  summarise(population = sum(population), .groups = "drop")
TOT <- STD_POPbyage$population %>% sum()
STD_POPbyage$population <- round(STD_POPbyage$population / TOT * 1e6, 0)
STD_POPbyage <- STD_POPbyage  %>% dplyr::select(-country_name, -year) %>% rename(world2020 = population) 

STD_POPbysex <- pop_adj %>% dplyr::select(sex, `Region, subregion, country or area *`, Year:`100+`) %>% 
  rename(year = Year, country_name = `Region, subregion, country or area *`) %>%
  filter(country_name %in% "World" & year == 2020) %>%
  pivot_longer(
    cols = matches("\\d+-\\d+|100\\+"),
    names_to = "age",
    values_to = "population"
  ) %>% mutate( population = as.numeric(population) * 1000, # originally in thousands
                age = case_when(
                  age == "100+" ~ "95-99",
                  TRUE ~ age
                )) %>%
  group_by(across(-population), age) %>%
  summarise(population = sum(population), .groups = "drop")
TOT <- STD_POPbysex$population %>% sum()
STD_POPbysex$population <- round(STD_POPbysex$population / TOT * 1e6, 0)
STD_POPbysex <- STD_POPbysex %>% dplyr::select(-country_name, -year) %>% rename(world2020 = population) 

pop_scaled <- STD_POPbysex %>% ungroup() %>%
  mutate(age_cut = ifelse(age %in% c("0-4", "5-9", "10-14", "15-19"), "0-19", 
                   ifelse(age %in% c("20-24", "25-29", "30-34", "35-39"), "20-39", 
                   ifelse(age %in% c("40-44", "45-49", "50-54", "55-59"), "40-59", 
                   ifelse(age %in% c("60-64", "65-69", "70-74"), "60-74",
                   ifelse(age %in% c("75-79", "80-84", "85-89", "90-94", "95-99"), "75+", NA)))))) %>%
  group_by(sex, age_cut) %>%
  mutate(
    total_pop = sum(world2020),
    w_i = world2020 / total_pop * 1e6
  ) %>% ungroup()
Page <- pop_scaled %>% dplyr::select(sex, age, age_cut, w_i) %>% rename(world2020 = w_i)

## globale ----
wt <- whoimp %>% 
  #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, sex, age) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
# comprimo l'età
wtage <- wt %>% group_by(.i, year, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()
wtjpo <- wtage
g3 <- ggplot(wtage %>% filter(!cause %in% c("CVD", "Neoplasms", "All causes") &
                              year >= REPR_YEARS[1] & year <= REPR_YEARS[2]
                                ), aes(x = year, y = asmr, group = cause, color = cause, fill = cause)) +
  geom_smooth(size = 1, alpha = 0.3, se = T) +
  scale_color_manual(values = wes_palette("IsleofDogs1")) + 
  scale_fill_manual(values = wes_palette("IsleofDogs1")) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
  scale_y_continuous(labels = scales::comma)+
  labs(
    x = "",
    y = "ASMR per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top"
  )
g3
jpeg(filename = "G:/Il mio Drive/UNIVPM/WHO/figure/global-cause-ASMR.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g3); dev.off()

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T)
  ) %>% ungroup() %>%
  group_by(year, cause) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()

# format
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

# average 2000-2023
t1_global <- WTS %>% group_by(cause) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(lower, 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(lower, 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted) %>%
  pivot_wider(names_from = cause, values_from = c(formatted))

t1_global <- data.frame("Stratification" = "Global", t1_global)



## per continente ----
rm(wt); gc()
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex, continent) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2])  %>% 
  group_by(.i, year, cause, continent) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup() %>%
  group_by(year, cause, continent) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()

# unisco le due per il grafico
names(WTS)
WTS <- bind_rows(WTS) %>% filter(!cause %in% c("All causes") & !continent %in% "Africa")
pal <- colorRampPalette(c("#4A6FA5", "#6D435A", "#FF6978"))(length(levels(as.factor(WTS$continent))))
cause_list <- unique(WTS %>% pull(cause))

plots <- list()
WTS$continent <- factor(WTS$continent, levels = c("Asia", "Central-South America", "Europe", "North America", "Oceania"))
for(i in seq_along(cause_list)) {
  plots[[i]] <- ggplot(WTS %>% filter(cause == cause_list[i]), 
                       aes(x = year, y = asmr, color = continent, fill = continent, group = continent)) +
    geom_smooth(size = 1, alpha = 0.3, se = F) +
    scale_color_manual(values = pal, name = "") + 
    scale_fill_manual(values = pal, name = "") + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
    labs(
      title = cause_list[i],
      x = "",
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = if(i == 1) "top" else "none",  # Show legend only on first plot
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
require(patchwork)
gigio <- (plots[[4]] + plots[[1]] + plots[[2]]) / (plots[[3]] + labs(y = "asSMR per 1,000,000") +plots[[5]]) / (plots[[6]]+plots[[7]]) + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center"
  )
gigio
if (QUALITY == "usa60"){
  jpeg(filename = paste0("figure/GEO-TRENDS-", QUALITY, ".jpg"), units = "px", width = 3000, height = 1800, res = 200, quality = 100)
  print(gigio); dev.off()
}
# format
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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
# average 2000-2023
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex, continent) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2])  %>% 
  group_by(.i, year, cause, continent) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup() %>%
  group_by(year, cause, continent) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()
t1_continent <- WTS %>% group_by(cause, continent) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, continent) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = continent)

t1_continent
## per zona ricchezza ----
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

wtrich <- wt %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2])  %>% mutate(everything = 1) %>% group_by(.i, year, cause, world_bank) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6) %>% group_by(year, world_bank, cause) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), asmr = mean(asmr, na.rm = T), stderr_asmr = mean(stderr_asmr, na.rm = T), smr = mortality / population * 1e6
  ) %>% ungroup()
WTS <- wtrich %>% filter(!cause %in% c("All causes") & !world_bank %in% "LI")
pal <- c("#80B192", "#6A8D92", "#4B4E6D")
cause_list <- unique(WTS %>% pull(cause))

plots <- list()

for(i in seq_along(cause_list)) {
  plots[[i]] <- ggplot(WTS %>% filter(cause == cause_list[i]), 
                       aes(x = year, y = asmr, color = world_bank, fill = world_bank, group = world_bank)) +
    geom_smooth(size = 1, alpha = 0.3, se = T) +
    scale_color_manual(values = pal, name = "") + 
    scale_fill_manual(values = pal, name = "") + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
    labs(
      color = "", fill = "",
      title = cause_list[i],
      x = "",
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = if(i == 1) "top" else "none",  # Show legend only on first plot
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
require(patchwork)
gigio2 <- (plots[[4]] + plots[[1]] + plots[[2]]) / (plots[[3]]+labs(y = "asSMR per 1,000,000") + plots[[5]]) / (plots[[6]]+plots[[7]]) + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center"
  )
gigio2
if (QUALITY == "usa60"){
  jpeg(filename = paste0("figure/WEALTH-TRENDS-", QUALITY, ".jpg"), units = "px", width = 3000, height = 1800, res = 200, quality = 100)
  print(gigio2); dev.off()
}

# SALVO PER JOINPOINT PROGRAM ----
wtjp <- wtrich %>% dplyr::select(year, cause, world_bank, asmr, stderr_asmr)
#wtjp <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
#  group_by(.i, year, cause, age, sex, world_bank) %>% summarise(
#    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
#  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
#wtjp$w_i <- wtjp$world2020 # peso relativo della fascia di popolazione
#wtjp <- wtjp %>%
#  group_by(.i, year, cause, world_bank) %>% summarise(
#    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), w_i = sum(w_i, na.rm = T)
#  ) %>% ungroup()
#
#wtjp <- wtjp %>%
#  group_by(year, cause, world_bank) %>% summarise(
#    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population), w_i = mean(w_i, na.rm = T)
#  ) %>% ungroup()
#wtjp$asmr <- wtjp$mortality * wtjp$w_i / wtjp$population
#wtjp$v_asmr <- sqrt( wtjp$mortality * (wtjp$w_i / wtjp$population)^2 )
#ggplot(wtjp %>% filter(cause == "CVD"), aes(year, asmr, colour = world_bank))+geom_smooth()

wtjpo <- wtjpo %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2] & cause != "All causes") %>% 
  group_by(year, cause) %>% summarise(
    asmr = mean(asmr), stderr_asmr = mean(stderr_asmr)
  ) %>% ungroup() %>%
  dplyr::select(any_of(names(wtjp)))
wtjpo$world_bank <- "Total"
# chjeck
ggplot(wtjp %>% filter(cause == "RA"), aes(year, asmr, colour = world_bank))+geom_smooth()
wtjp <- wtjp %>% bind_rows(wtjpo) %>% filter(cause != "All causes")
wtjp <-  wtjp %>% arrange(cause, world_bank, year)
xlsx::write.xlsx(wtjp, file = "data/WTJP.xlsx")

# tabelle
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup() %>%
  group_by(year, cause, world_bank) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()

# format
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

# average 2000-2023
t1_wealth <- WTS %>% group_by(cause, world_bank) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, world_bank) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = world_bank)
t1_wealth

## per sesso ----

wt <- whoimp %>%
  #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, sex, age) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbyage, by = c("age"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

wtsex <- wt %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% group_by(.i, year, sex, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup() %>% group_by(year, sex, cause) %>% summarise(
  mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
  w_i = mean(w_i, na.rm = T), asmr = mean(asmr, na.rm = T), stderr_asmr = mean(stderr_asmr, na.rm = T), smr = mortality / population * 1e6
) %>% ungroup()

WTS <- wtsex %>% filter(!cause %in% c("All causes"))
if (!all(WTS$w_i %in% 1000001)) {
  stop("No")
}
pal <- c("#564D65", "#DACC3E")
cause_list <- unique(WTS %>% pull(cause))

plots <- list()

for(i in seq_along(cause_list)) {
  plots[[i]] <- ggplot(WTS %>% filter(cause == cause_list[i]) %>% mutate(sex = recode(sex, "M" = "Male", "F" = "Female")), 
                       aes(x = year, y = asmr, color = sex, fill = sex, group = sex)) +
    geom_smooth(size = 1, alpha = 0.3, se = T) +
    scale_color_manual(values = pal, name = "") + 
    scale_fill_manual(values = pal, name = "") + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
    labs(
      color = "", fill = "",
      title = cause_list[i],
      x = "",
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = if(i == 1) "top" else "none",  # Show legend only on first plot
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
gigio4 <- (plots[[4]] + plots[[1]] + plots[[2]]) / (plots[[3]]+labs(y = "asSMR per 1,000,000") + plots[[5]]) / (plots[[6]]+plots[[7]]) + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center"
  )
gigio4
if (QUALITY == "usa60"){
  jpeg(filename = paste0("figure/SEX-TRENDS-", QUALITY, ".jpg"), units = "px", width = 3000, height = 1800, res = 200, quality = 100)
  print(gigio4); dev.off()
}

# tabelle
wtsex <- wt %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% group_by(.i, year, sex, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup() %>% group_by(year, sex, cause) %>% summarise(
  mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
  w_i = mean(w_i, na.rm = T), asmr = mean(asmr, na.rm = T), stderr_asmr = mean(stderr_asmr, na.rm = T), smr = mortality / population * 1e6
) %>% ungroup()
WTS <- wtsex
if (!all(WTS$w_i %in% 1000001)) {
  stop("No")
}
# format
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

# average 2000-2023
t1_sex <- WTS %>% group_by(cause, sex) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, sex) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = sex)
t1_sex

## per fasce di età ----

wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  mutate(age_cut = ifelse(age %in% c("0-4", "5-9", "10-14", "15-19"), "0-19", 
                   ifelse(age %in% c("20-24", "25-29", "30-34", "35-39"), "20-39", 
                   ifelse(age %in% c("40-44", "45-49", "50-54", "55-59"), "40-59", 
                   ifelse(age %in% c("60-64", "65-69", "70-74"), "60-74",
                   ifelse(age %in% c("75-79", "80-84", "85-89", "90-94", "95-99"), "75+", NA)))))) %>%
  group_by(.i, year, cause, sex, age, age_cut) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(Page %>% mutate(world2020 = world2020/2), by = c("sex", "age", "age_cut"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

wtage <- wt %>% group_by(.i, year, age_cut, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup() %>% filter(!age_cut %in% "0-19")

pal <- c("#9ECAE1", "#41B6C4", "#253494", "#081D58")
cause_list <- unique(WTS %>% pull(cause))

plots <- list()

for(i in seq_along(cause_list)) {
  plots[[i]] <- ggplot(wtage %>% filter(cause == cause_list[i]), 
                       aes(x = year, y = asmr, color = age_cut, fill = age_cut, group = age_cut)) +
    geom_smooth(size = 1, alpha = 0.3, se = T) +
    scale_color_manual(values = pal) + 
    scale_fill_manual(values = pal) + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
    labs(
      color = "", fill = "",
      title = cause_list[i],
      x = "",
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = if(i == 1) "top" else "none",  # Show legend only on first plot
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

gigio5 <- (plots[[4]] + plots[[1]] + plots[[2]]) / (plots[[3]]+ labs(y = "asSMR per 1,000,000") + plots[[5]]) / (plots[[6]]+plots[[7]]) + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center"
  )
gigio5
if (QUALITY == "usa60"){
  jpeg(filename = paste0("figure/AGE-TRENDS-", QUALITY, ".jpg"), units = "px", width = 3000, height = 1800, res = 200, quality = 100)
  print(gigio5); dev.off()
}


WTS <- wtage %>%
  group_by(year, cause, age_cut) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(stderr_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()
if (!all(WTS$w_i %in% c(1000001, 1e6))) {
  stop("No")
}
setDT(WTS)[, crude := {
  rates <- mortality / population * 1e6
  lower <- qgamma(0.025, mortality) / population * 1e6
  upper <- qgamma(0.975, mortality + 1) / population * 1e6
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

# 
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

# average 2000-2023
t1_age <- WTS %>% group_by(cause, age_cut) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, age_cut) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = age_cut)
t1_age
## per nazione ----
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, age, sex, country_name) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
wt <- wt %>%
  group_by(.i, year, cause, country_name) %>% summarise(
    world2020 = sum(world2020, na.rm = T), mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup()

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>%
  group_by(year, cause, country_name) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()
# tasssi grezzi #
library(data.table)
if (!all(WTS$w_i %in% c(1e6, 1000001))) {
  stop("No")
}
setDT(WTS)[, crude := {
  rates <- mortality / population * 1e6
  lower <- qgamma(0.025, mortality) / population * 1e6
  upper <- qgamma(0.975, mortality + 1) / population * 1e6
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

# 
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

t1_country <- WTS %>% group_by(cause, country_name) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, country_name) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = country_name) %>%
  mutate(Stratification = as.character(Stratification)) %>% arrange(Stratification)

## per global south e north ----

wt <- whoimp %>% mutate(
  north_south = ifelse(country_name %in% global.north, "North", "South")
) %>%
  group_by(.i, year, cause, age, sex, north_south) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>% group_by(.i, year, north_south, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()


pal <- c("navy", "red4")
cause_list <- unique(WTS %>% pull(cause))

plots <- list()

for(i in seq_along(cause_list)) {
  plots[[i]] <- ggplot(WTS %>% filter(cause == cause_list[i]), 
                       aes(x = year, y = asmr, color = north_south, fill = north_south, group = north_south)) +
    geom_smooth(size = 1, alpha = 0.3, se = T) +
    scale_color_manual(values = pal, name = "") + 
    scale_fill_manual(values = pal, name = "") + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
    labs(
      color = "", fill = "",
      title = cause_list[i],
      x = "",
      y = ""
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = if(i == 1) "top" else "none",  # Show legend only on first plot
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
require(patchwork)
gigio2 <- (plots[[4]] + plots[[1]] + plots[[2]]) / (plots[[3]]+labs(y = "asSMR per 1,000,000") + plots[[5]]) / (plots[[6]]+plots[[7]]) + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center"
  )
gigio2
if (QUALITY == "usa60"){
  jpeg(filename = paste0("figure/NS-TRENDS-", QUALITY, ".jpg"), units = "px", width = 3000, height = 1800, res = 200, quality = 100)
  print(gigio2); dev.off()
}


#
WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, north_south) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup() %>%
  group_by(year, cause, north_south) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()
if (!all(WTS$w_i %in% c(1000001, 1e6))) {
  stop("No")
}
setDT(WTS)[, crude := {
  rates <- mortality / population * 1e6
  lower <- qgamma(0.025, mortality) / population * 1e6
  upper <- qgamma(0.975, mortality + 1) / population * 1e6
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

# 
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

# average 2000-2023
t1_northsouth <- WTS %>% group_by(cause, north_south) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, north_south) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = north_south)
t1_northsouth$Stratification <- c("   Global North", "   Global South")
t1_northsouth
## per western eastern europe ----
west.EU <- c("Belgium", "Denmark", "France", "Germany", #"Greece", 
             "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "United Kingdom",
             "Austria", "Sweden", "Malta", "Finland", "Iceland", "Norway", "Switzerland")
east.EU <- c(setdiff(EU, west.EU), "Serbia", "Republic of Moldova")
wt <- whoimp %>% filter(country_name %in% c(east.EU, west.EU)) %>% mutate(
  east_west = ifelse(country_name %in% east.EU, "Eastern Europe", "Western Europe")
) %>%
  group_by(.i, year, cause, age, sex, east_west) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

wtage <- wt %>% group_by(.i, year, east_west, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup() %>% group_by(year, east_west, cause) %>% summarise(
  mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
  w_i = mean(w_i, na.rm = T), asmr = mean(asmr, na.rm = T), stderr_asmr = mean(stderr_asmr, na.rm = T), smr = mortality / population * 1e6
) %>% ungroup()

# in che luogo?
# quando?
# per quali cause di morte?
g3 <- ggplot(wtage %>% filter(!cause %in% c("All causes")), aes(x = year, y = asmr, color = east_west, fill = east_west, group = east_west)) +
  geom_smooth(size = 1, alpha = 0.3, se = F) +
  scale_color_manual(values = c("green4", "orange")) + 
  scale_fill_manual(values = c("green4", "orange")) + 
  scale_y_continuous(labels = scales::comma)+
  labs(
    title = "Age-Standardised Mortality Rates",
    x = "",
    y = "ASMR per 1,000,000",
    color = "", fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) + facet_wrap(~ cause, scales = "free_y")
#jpeg(filename = "figure/westeastEU.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
#print(g3); dev.off()
g3

WTS <- wt %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, east_west) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup() %>%
  group_by(year, cause, east_west) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()
if (!all(WTS$w_i %in% c(1e6, 1000001))) {
  stop("No")
}
setDT(WTS)[, crude := {
  rates <- mortality / population * 1e6
  lower <- qgamma(0.025, mortality) / population * 1e6
  upper <- qgamma(0.975, mortality + 1) / population * 1e6
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

# 
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

# average 2000-2023 ----
t1_east_west <- WTS %>% group_by(cause, east_west) %>% summarise(
  ASMR = weighted.mean(asmr, w = population),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, east_west) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = east_west)
t1_east_west$Stratification <- paste0("   ", t1_east_west$Stratification)
t1_east_west
## STRATIFICATA PER ANNO ----
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

wty <- wt %>% mutate(everything = 1) %>% group_by(.i, year, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6)

# tabelle
wt <- whoimp %>% #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup() %>%
  group_by(year, cause) %>% summarise(
    mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population),
    w_i = mean(w_i, na.rm = T), stderr_asmr = sqrt(mean(v_asmr, na.rm = T)), smr = mortality / population * 1e6,
    asmr = mean(asmr, na.rm = T)
  ) %>% ungroup()

# format
setDT(WTS)[, std := {
  rates <- asmr
  lower <- (asmr - min(asmr, 1.96*stderr_asmr))
  upper <- (asmr + 1.96*stderr_asmr)
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

t1_years <- WTS %>% group_by(cause, year) %>% summarise(
  ASMR = weighted.mean(asmr, w = mortality),
  stderr_ASMR = sqrt(weighted.mean(stderr_asmr^2, w = mortality))  # Approximate pooled standard error
) %>% 
  mutate(
    lower = ASMR - 1.96 * stderr_ASMR,
    upper = ASMR + 1.96 * stderr_ASMR,
    formatted = ifelse(ASMR > 10,
                       sprintf("%.0f [%.0f-%.0f]", round(ASMR, 0), round(ifelse(lower<0, 0, lower), 0), round(upper, 0)),
                       sprintf("%.2f [%.2f-%.2f]", round(ASMR, 2), round(ifelse(lower<0, 0, lower), 2), round(upper, 2)))
  ) %>% ungroup() %>% dplyr::select(cause, formatted, year) %>%
  pivot_wider(names_from = cause, values_from = formatted) %>% rename(Stratification = year)



# FINALE ----
gc()
h <- ncol(t1_age)-1
names(t1_global) <- names(t1_sex)

t1_sex$Stratification <- ifelse(t1_sex$Stratification == "M", "Male", "Female")
t1_wealth$Stratification <- ifelse(t1_wealth$Stratification == "HI", "High income", ifelse(t1_wealth$Stratification == "UMI",
                                                                                       "Upper-middle income", "Lower-middle income"))
t1 <- rbind(t1_global, t1_northsouth, c("Sex", rep(NA, h)), t1_sex, 
            c("Age class", rep(NA, h)), t1_age, c("Continent", rep(NA, h)), t1_continent[1:which(t1_continent$Stratification == "Europe"), ], 
            t1_east_west, t1_continent[(which(t1_continent$Stratification == "Europe")+1):nrow(t1_continent), ], 
            c("Wealth category", rep(NA, h)), t1_wealth,
            c("Calendar year", rep(NA, h)), t1_years, c("Country", rep(NA, h)), t1_country)
t1 <- as.data.frame(lapply(t1, function(col) {
  col[col %in% c("Inf [-Inf, Inf]", "Inf [Inf, Inf]")] <- "NA"
  col
}))

