library(haven); library(dplyr); library(stringr); library(tidyr); library(data.table); require(ggplot2); require(parallel); require(wesanderson)
setwd("your-dir")

# some macros
TIMESPAN <- 1980:2023 # initial EDA for WHO MDB
CAUSE <- c("RA", "SpA", "SLE", "Gout", "SSc", "CVD", "Neoplasms", "All causes")
age_breaks <- seq(0, 100, by = 5); age_labels <- paste0(age_breaks[-length(age_breaks)], "-", age_breaks[-1] - 1)
REPR_YEARS <- c(2000, 2023) # actual analysis time frame

QUALITY <- "usa60"
source("script/1___preprocess.R")
gc()
# after pre-processing the data and link UN prospects population size denominators, just load the who.Rdata
load("data/who.Rdata")

# imputation step ----
BOOT <- 100 # number of bootstrap replications (becomes heavy computing > 100...)
source("script/2___impute.R") 
# after saving IMPUTED data, just load WHO-imputed-2023.Rdata to speed up reproduction
#load("data/WHO-imputed-2023.Rdata")
# then use whoimp, the imputed, bootstrapped datasets for everything else
whoimp <- whoimp %>% filter(cause != "All causes" & year <= REPR_YEARS[2] & year >= REPR_YEARS[1])
rm(imputazione1); gc()

## standard population ----
whoimp <- whoimp %>% mutate(mortality = ifelse(is.na(mortality) | mortality %in% 0, round(Predicted/2, 1), mortality))
whoimp$mortality[whoimp$year > 2019] <- whoimp$Predicted[whoimp$year > 2019]
pop_adj <- readxl::read_excel("pop-un-est.xlsx")
STD_POP <- pop_adj %>% dplyr::select(sex, `Region, subregion, country or area *`, Year:`100+`) %>% 
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
TOT <- STD_POP$population %>% sum()
STD_POP$population <- round(STD_POP$population / TOT * 1e6, 0)
# quick pyramid of world pop 2020
a <- STD_POP %>% mutate(age = factor(age, levels = age_labels), percent = population / sum(population) * 100) %>% ungroup()
pyramid <- ggplot(a, aes(
  x = ifelse(sex == "M", -percent, percent),  # Males on left
  y = age,
  fill = sex
)) +
  geom_col(width = 0.7) +
  scale_x_continuous(
    labels = function(x) paste0(abs(x), "%"),  # Show absolute values with %
    name = "Percentage of Group"
  ) +
  scale_y_discrete(name = "Age Group") +
  scale_fill_manual(values = c("M" = "steelblue", "F" = "salmon")) +
  labs(fill = "Sex")+
  theme_minimal()
pyramid

# 
STD_POP <- STD_POP %>% dplyr::select(-country_name, -year) %>% rename(world2020 = population) %>%
  group_by(age) %>% summarise(world2020 = sum(world2020))

# usability filtering checks and data quality overall measures ----
u6 <- whoimp %>% 
  filter( !country_name %in% BANNED_COUNTRIES & !is.na(continent) & !is.na(cause) & usa60 == 1) %>%
  group_by(year, country_name, cause, age) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POP, by = c("age"))
u8 <- whoimp %>% 
  filter( !country_name %in% BANNED_COUNTRIES & !is.na(continent) & !is.na(cause) & usa80 == 1) %>%
  group_by(year, country_name, cause, age) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POP, by = c("age"))

u6 <- sort(unique(u6$country_name)) # 63
u8 <- setdiff(u6, sort(unique(u8$country_name))) # 48

complete_cases <- who %>% 
  filter(country_name %in% u6 & !is.na(mortality)) %>% 
  group_by(year, country_name, usa60, usa80) %>% 
  slice(1) %>% 
  ungroup() %>%
  group_by(country_name) %>% 
  filter(all(2005:2019 %in% year)) %>%
  ungroup() %>% pull(country_name) %>% unique()
# keep countries based on the macro filter set at the beginning of this script
sc <- if (QUALITY == "usa60") {
  u6
} else if (QUALITY == "usa80") {
  setdiff(u6, u8)
} else if (QUALITY == "g20"){
  G20
} else{
  complete_cases
}

whoimp <- whoimp %>% filter(country_name %in% sc)
sc
unique(whoimp$country_name[whoimp$world_bank == "LMI"])
unique(whoimp$country_name[whoimp$continent == "Europe"])
GUIDELINES <- data.frame(RA = c(1987, 2010), SLE = c(1997, 2012), SSc = c(1980, 2013), Gout = c(1977, 2015), SpA = c(2005, 2010))[2, ]

# age and sex standardised mortality rates (asSMR) ----
source("script/1C-ASSMR.R")
t1_orig <- t1
# crude SMR ----
source("script/1-crude-mr.R")
t1_crude <- t1

# average percent change (APC) to evaluate trends ----
source("script/3B-ASAPC.R")

fn <- paste0("res/", paste0(REPR_YEARS, collapse = "-"), "-EPI-WHO-", QUALITY, "-", Sys.Date(), ".xlsx")
xlsx::write.xlsx(OVERALL %>% as.data.frame(), file = fn, sheetName = "Deaths", showNA = F, append = F, row.names = F)
xlsx::write.xlsx(missing.by.years %>% as.data.frame(), file = fn, sheetName = "Missing year", showNA = F, append = T, row.names = F)
xlsx::write.xlsx(missing.by.country %>% as.data.frame(), file = fn, sheetName = "Missing countries", showNA = F, append = T, row.names = F)
xlsx::write.xlsx(t1_crude, file = fn, sheetName = "SMR x 1 MLN", showNA = F, append = T, row.names = F)
xlsx::write.xlsx(t1_orig, file = fn, sheetName = "ASMR x 1 MLN", showNA = F, append = T, row.names = F)
xlsx::write.xlsx(results, file = fn, sheetName = "Average AAPC", showNA = F, append = T, row.names = F)

# epidemiology analysis on GBD data ----
source("script/3-epidemiology.R")
xlsx::write.xlsx(incidence, file = fn, sheetName = "incidence-APC", showNA = F, append = T, row.names = F)
xlsx::write.xlsx(prevalence, file = fn, sheetName = "prevalence-APC", showNA = F, append = T, row.names = F)

# ecologic relationship between prevalence, incidence and mortality ----
source("script/5-ecol.R")
xlsx::write.xlsx(INC.table, file = fn, sheetName = "incidence-asmr-model", showNA = F, append = T, row.names = F)
xlsx::write.xlsx(PREV.table, file = fn, sheetName = "prevalence-asmr-model", showNA = F, append = T, row.names = F)