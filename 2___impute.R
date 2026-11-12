require(forecast)
a <- whoimp %>% filter(!is.na(population) & !is.na(continent) & !is.na(cause) & !is.na(country_name) & !is.na(world_bank)) %>% 
  filter(get(QUALITY) == 1 & !country_name %in% BANNED_COUNTRIES) %>%
  group_by(year, country_name, usa60, usa80) %>% slice(1) %>% ungroup() %>%
  group_by(country_name) %>% mutate(n = n()) %>% ungroup() %>%
  filter(n >= 10)
atleast10 <- unique(a$country_name) %>% sort()
mp <- a %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% summarise(
  N = n()
) 
missing_percentage = 1 - as.numeric(mp)/ ((REPR_YEARS[2] - REPR_YEARS[1]) * length(sc))

# usability parameter geo distribution
coverage <- ggplot(a %>% arrange(country_name, year), 
                   aes(x = year, 
                       y = factor(country_name, levels = rev(sort(unique(country_name)))), 
                       color = year,  
                       group = country_name)) +
  geom_line(color = "gray70") +  
  geom_point() +
  scale_color_gradientn(colors = colorRampPalette(c("#E5625E", "#66C7F4"))(length(unique(a$year))),
                        limits = c(1995, REPR_YEARS[2])) +
  scale_x_continuous(limits = c(1995, REPR_YEARS[2]), breaks = seq(1995, REPR_YEARS[2], by = 2)) +
  labs(title = "",
       x = "Year",
       y = "",
       color = "Year") +
  theme_minimal() + 
  theme(legend.position = "none", 
        axis.text.y = element_text(size = 14))
coverage
jpeg(filename = "figure/coverage.jpg", units = "px", width = 2000, height = 1600, res = 120, quality = 100)
print(coverage); dev.off()


griglia <- expand_grid(country_name = unique(a$country_name), year = REPR_YEARS[1]:REPR_YEARS[2])
griglia <- griglia %>% left_join(a %>% mutate(complete = 1)) %>% arrange(country_name, year) %>% dplyr::select(country_name, year, complete) %>% distinct()
griglia$complete[is.na(griglia$complete)] <- 0
63*24
missing.by.years <- griglia %>% group_by(year) %>% summarise(complete_prop = round((100*sum(complete)/length(unique(griglia$country_name))), 1))
missing.by.country <- griglia %>% group_by(country_name) %>% summarise(complete_prop = round(100*sum(complete)/(1 + REPR_YEARS[2]- REPR_YEARS[1]), 1))

w <- who %>% filter(!is.na(population) & !is.na(continent) & !is.na(cause) & !is.na(country_name) & !is.na(world_bank)) %>% 
  filter(!country_name %in% BANNED_COUNTRIES) %>% mutate(quality = ifelse(usa80 == 1, "≥80%", ifelse(usa60 == 1, "≥60%", "<60%"))) %>% 
  arrange(country_name, desc(usa80), desc(usa60)) %>% group_by(country_name) %>% slice(1) 

library(sf); library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf") %>% mutate(country_name = sovereignt)
world_data <- merge(world, 
                    w %>% dplyr::select(country_name, quality), 
                    by.x = "country_name", 
                    by.y = "country_name", 
                    all.x = TRUE)
w$quality
g2 <- ggplot(world_data) +
  geom_sf(aes(fill = factor(quality)), color = "black", size = 0.1) +
  scale_fill_manual(
    values = c(
      "<60%" = "#E5625E",
      "≥60%" = "lightblue",
      "≥80%" = "#66C7F4"
    ),
    name = "Usability (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
g2
jpeg(filename = "figure/world-coverage.jpg", units = "px", width = 2400, height = 1800, res = 300, quality = 100)
print(g2); dev.off()

#
pop_adj <- readxl::read_excel("pop-un-est.xlsx")
STD_POP <- pop_adj %>% select(sex, `Region, subregion, country or area *`, Year:`100+`) %>% 
  rename(year = Year, country_name = `Region, subregion, country or area *`) %>%
  filter(country_name %in% "World" & year == 2020) %>%
  pivot_longer(
    cols = matches("\\d+-\\d+|100\\+"),
    names_to = "age",
    values_to = "population"
  ) %>% mutate( population = as.numeric(population) * 1000, 
                age = case_when(
                  age == "100+" ~ "95-99",
                  TRUE ~ age
                )) %>%
  group_by(across(-population), age) %>%
  summarise(population = sum(population), .groups = "drop")
TOT <- STD_POP$population %>% sum()
STD_POP$population <- round(STD_POP$population / TOT * 1e6, 0)
STD_POP <- STD_POP  %>% select(-country_name, -year) %>% rename(world2020 = population) 

who2 <- who %>% filter(country_name %in% unique(atleast10))

u6 <- who %>% 
  filter( !country_name %in% BANNED_COUNTRIES & !is.na(continent) & !is.na(cause) & usa60 == 1) %>%
  group_by(year, country_name, cause, age) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POP, by = c("age"))
u6 <- sort(unique(u6$country_name))
63*2*length(CAUSE)*length(age.class)*29
grill <- expand.grid(country_name = setdiff(u6, BANNED_COUNTRIES), year = (REPR_YEARS[1] -5):REPR_YEARS[2], 
                     age = age_labels, sex = c("M", "F"), cause = CAUSE) %>%
  left_join(pop_est, by = c("country_name", "year", "age", "sex")) %>% 
  filter(population > 0 & !is.na(population))
country_data <- grill %>% left_join(who2 %>% dplyr::select(-population, -continent), by = c("country_name", "cause", "year", "age", "sex")) 
country_data <- country_data %>% left_join(STD_POP, by = c("age", "sex"))
country_data <- country_data %>% arrange(country_name, cause, age, sex) %>%
  tidyr::fill(world_bank, continent, country_name, usability, sym(QUALITY), .direction = "down") %>%
  filter(!is.na(population) & !is.na(continent) & !is.na(cause) & !is.na(country_name) & !is.na(world_bank))
country_data$smr <- country_data$mortality / country_data$population
country_data$w_i <- country_data$world2020 
country_data$asmr <- country_data$mortality * country_data$w_i / country_data$population
country_data$v_asmr <- country_data$mortality * (country_data$w_i / country_data$population)^2

## prepare data for imputation and boot process ----
dd <- country_data %>% subset(population > 0 & !is.na(population) & world_bank != "LI" & !country_name %in% BANNED_COUNTRIES) %>%
  mutate(cause = factor(cause, levels = CAUSE)) %>%
    mutate(age_cut = ifelse(age %in% c("0-4", "5-9", "10-14", "15-19"), "0-19", 
                   ifelse(age %in% c("20-24", "25-29", "30-34", "35-39"), "20-39", 
                   ifelse(age %in% c("40-44", "45-49", "50-54", "55-59"), "40-59", 
                   ifelse(age %in% c("60-64", "65-69", "70-74"), "60-74",
                   ifelse(age %in% c("75-79", "80-84", "85-89", "90-94", "95-99"), "75+", NA)))))) %>% 
  arrange(year, cause, country_name, age, sex) %>% mutate(age = factor(age, levels = age_labels), sex = factor(sex, levels = c("M", "F")), 
                                          continent = factor(continent, levels = c("Europe", "North America", "Africa", "Oceania", "Central-South America", "Asia")),
                                          world_bank = factor(world_bank, levels = c("HI", "UMI", "LMI")),
                                          cause = factor(cause, levels = CAUSE))
dd <- dd %>%
  arrange(country_name, year, age, sex, cause) %>%
  group_by(country_name, age, sex, cause) %>%
  mutate(lag_death = lag(log(1 + mortality), 1, default = NA)) %>%
  mutate(lag_death = ifelse(is.na(lag_death), mean(log(1 + mortality), na.rm = TRUE), lag_death)) %>% ungroup()

  hist(dd$mortality, 1000)

dd_complete <- dd %>% 
    mutate(id = paste0(country_name, age, sex, cause))
fc <- unique(dd_complete$country_name)

ae <- dd_complete %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>%
  group_by(country_name, cause) %>% summarise(morti = sum(na.rm = T, mortality)) %>%
  pivot_wider(names_from = cause, values_from = morti) %>%
  mutate(ARDs = RA + SLE + Gout + SSc, 
         deaths = RA + SpA + SLE + Gout + SSc + CVD + Neoplasms + T2D) %>%
  dplyr::select(-SpA)
nrow(ae)
overall <- ae %>% filter(ARDs > 10)
nrow(overall)

# actual modelling loop call 
source("script/2c-model.R")

strater <- "Europe"
imputazione1 <- AJ 
save(imputazione1, file = "data/WHO-imputed-2023.Rdata")
rm(AJ); rm(ct)
gc()
plot_d <- imputazione1 %>%
  filter(cause != "All causes") %>%
  group_by(year, cause, world_bank) %>%
  summarise(.groups = "drop",
    mortality = mean(mortality, na.rm = TRUE),
    population = mean(population, na.rm = TRUE),
    Predicted = mean(Predicted, na.rm = TRUE)
  )
plot_d$population
setDT(plot_d)
plot_d[, rate := Predicted / population * 10^6]
ggplot(plot_d, aes(x = year)) +
  geom_line(aes(y = rate, color = world_bank), size = .4, linetype = "dashed") +
  labs(x = "Year", y = "Mortality Rate",
       color = "Series") +
  theme_minimal()+facet_wrap(~ cause, scales = "free_y")
ggplot(plot_d, aes(x = year)) +
  geom_line(aes(y = mortality, color = world_bank), size = .4, linetype = "dashed") +
  labs(x = "Year", y = "Mortality Rate",
       color = "Series") +
  theme_minimal()+facet_wrap(~ cause, scales = "free_y")

require(purrr)
imputazione1 <- imputazione1 %>%
        mutate(mortality = ifelse(is.na(mortality), Predicted, mortality)
  )

rm(AJ)
gc()
imputazione1 <- imputazione1 %>% filter(country_name %in% u6)
u <- sort(table(imputazione1$.i, imputazione1$country_name) %>% apply(2, sum)/8000, decreasing = T)
ggplot(data.frame(nu = names(u), u))+
  geom_bar(aes(x = nu, y = u), stat = "identity")+coord_flip()

plot_d <- setDT(imputazione1)

plot_d <- plot_d[
  , .(mortality = median(mortality, na.rm = TRUE),
      population = median(population, na.rm = TRUE)),
  by = .(.i, year, cause, country_name, sex, age, continent, world_bank)
]
gc()

STD_POP_dt <- as.data.table(STD_POP)
plot_d <- merge(plot_d, STD_POP_dt, by = c("age", "sex"), all.x = TRUE)

gc()
plot_d[, smr := mortality / population]
plot_d[, w_i := world2020] 
plot_d[, asmr := mortality * w_i / population]
plot_d[, v_asmr := mortality * (w_i / population)^2]

plot_d <- plot_d[
  , .(mortality = sum(mortality, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      asmr = sum(asmr, na.rm = TRUE)),
  by = .(.i, year, cause, world_bank)
]
plot_d <- plot_d %>% as.data.frame()

ggplot() +
  geom_point(data = plot_d, aes(x = year, y = asmr, colour = world_bank), size = .4, colour = "navy") +
  labs(title = ,
       x = "Year", y = "N. cause specific deaths",
       color = "Series") +
  theme_minimal()+facet_wrap(~ cause, scales = "free_y")

rm(plot_d)
gc()

library(tidyverse)
names(imputazione1)
plot_data <- imputazione1 %>% filter(cause == "CVD" ) %>%
  group_by(year, age, sex, country_name, world_bank, continent) %>%
  summarise(
    population = mean(population),
    mortality = mean(mortality, na.rm = TRUE),
    Predicted = mean(Predicted, na.rm = TRUE),
    .groups = "drop"
  ) %>% mutate(mortality = ifelse(is.nan(mortality), NA, mortality)) %>%
  group_by(year, country_name, world_bank, continent) %>%
  summarise(
    population = sum(population),
    mortality = sum(mortality, na.rm = TRUE),
    Predicted = sum(Predicted, na.rm = TRUE),
    .groups = "drop"
  ) %>% mutate(mortality = ifelse(is.nan(mortality), NA, mortality))

table(imputazione1$world_bank[imputazione1$country_name %in% "Japan"])
