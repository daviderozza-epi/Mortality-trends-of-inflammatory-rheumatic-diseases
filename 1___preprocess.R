# WHO - Mortalità per RHeuma

# import data ----
dta_files <- list.files(
  path = getwd(),    # replace with your folder path
  pattern = "\\.dta$",             # regex: ends with .dta
  full.names = TRUE                # get full file paths
)
setDTthreads(percent = 70)

excluded <- c(paste0("database", 1:6), "database_appended", "ra", "sle", "gout") # ridond
library(parallel)
num_cores <- detectCores() - 1  # leave one core free
dta_pattern <- "\\.dta$"
space_pattern <- "\\s+"
cl <- makeCluster(num_cores)  # Initialize cluster
clusterExport(cl, c("dta_pattern", "space_pattern", "excluded"))  # Export variables
dta_list <- parLapply(cl, dta_files, function(f) {
  library(haven); library(dplyr); library(stringr); library(tidyr); library(data.table); require(ggplot2); require(parallel)
  name <- str_replace_all(str_replace(basename(f), dta_pattern, ""), space_pattern, "_")
  if (!name %in% excluded) {
    message("Reading: ", name)
    as.data.table(read_dta(f))
  } else {
    NULL
  }
})
stopCluster(cl)
gc()
names(dta_list) <- str_replace_all(str_replace(basename(dta_files), "\\.dta", ""), "\\s+", "_")
names(dta_list) <- sapply(dta_files, function(f) {
  str_replace_all(str_replace(basename(f), dta_pattern, ""), space_pattern, "_")
})
dta_list <- dta_list[!names(dta_list) %in% excluded]
names(dta_list)
countries <- readr::read_csv("country_codes.csv") %>% rename(country_name = name, country_code = country)
db <- dta_list[["database_new"]] %>% select(-country_name) %>% left_join(countries, by = "country_code")
length(unique(db$country_name))
length(unique(db$country_name[db$year == 2020]))
length(unique(db$country_name[db$year == 2021]))
length(unique(db$country_name[db$year == 2022]))
length(unique(db$country_name[db$year == 2023]))
db <- db %>% filter(year <= REPR_YEARS[2] & cause != "AAA")
pop <- dta_list[["population_new"]]
pop_adj <- readxl::read_excel("pop-un-est.xlsx") # World Population Prospects database - 2024
sle <- dta_list[["sle_with_pop"]]
dq <- dta_list[["data_quality"]]
gc()

# load vectors containing country features, functions, and other data processing features
source("script/plugin-countries.R")
dq <- dq %>% left_join(ghe, by = "country_name") %>% mutate(usability = 0.01*completeness * (100 - percentageofilldefinedornonspeci)) %>% rename(ill_defined = percentageofilldefinedornonspeci)

table(a <- unique(countries$country_name))
table(b <- unique(pop_adj$`Region, subregion, country or area *`))
setdiff(b, a)
pop_est <- pop_adj %>% select(sex, `Region, subregion, country or area *`, Year:`100+`) %>% rename(year = Year, country_name = `Region, subregion, country or area *`) %>%
  filter(country_name %in% intersect(b, a)) %>%
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

# population data pre-processing ----
melt_cols <- paste0("pop", 2:26)
age.class <- c()
pop_long <- melt(
  pop,
  measure.vars = melt_cols,
  variable.name = "age",
  value.name = "value"
) %>% mutate(age = as.character(age)) %>% select(-lb, -epi, -epi2, -admin1, -subdiv)
table(pop_long$age, exclude = F)

for (i in melt_cols){
  j <- as.numeric(str_replace(i, "pop", ""))
  if (j <= 6) {
    pop_long$age[pop_long$age %in% i] <- "0-4"
  } else {
    age.class[j] <- paste0((j-6)*5, "-", (j-5)*5 - 1)
    pop_long$age[pop_long$age %in% i] <- age.class[j]
  }
}
age.class <- factor(c("0-4", age.class[!is.na(age.class)])[1:20], levels = c("0-4", age.class[!is.na(age.class)])[1:20])
table(pop_long$age, exclude = F)

pop_long$age <- factor(pop_long$age, levels = age.class)

pop_long <- pop_long %>% filter(!is.na(age)) %>% group_by(country_code, sex, year, age) %>% summarise(population = sum(value, na.rm = T)) %>% ungroup()
pop_long <- pop_long %>% left_join(countries, by = "country_code") %>% filter(year %in% TIMESPAN)
table(pop_est$country_name[str_detect(pop_est$country_name, "Korea|Grenadines")])
pop_est <- pop_est %>% mutate(continent = case_when(
  country_name %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                 "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros",
                 "Congo", "Democratic Republic of the Congo", "Djibouti", "Egypt",
                 "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon",
                 "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya",
                 "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali",
                 "Mauritania", "Mauritius", "Mayotte", "Morocco", "Mozambique",
                 "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe",
                 "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa",
                 "South Sudan", "Sudan", "United Republic of Tanzania", "Togo", "Tunisia", "Uganda",
                 "Zambia", "Zimbabwe") ~ "Africa",
  
  country_name %in% c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh",
                 "Bhutan", "Brunei Darussalam", "Cambodia", "China", "Cyprus",
                 "Georgia", "Hong Kong SAR", "India", "Indonesia", "Iran", "Iraq",
                 "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan",
                 "Laos", "Lebanon", "Macau", "Malaysia", "Maldives", "Mongolia",
                 "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine",
                 "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea",
                 "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand",
                 "Timor-Leste", "Turkey", "Turkmenistan", "United Arab Emirates",
                 "Uzbekistan", "Viet Nam", "Yemen") ~ "Asia",
  
  country_name %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
                 "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Finland",
                 "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy",
                 "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Monaco",
                 "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland",
                 "Portugal", "Republic of Moldova", "Romania", "Russia",
                 "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden",
                 "Switzerland", "Ukraine", "United Kingdom", "Vatican City") ~ "Europe",
  
  country_name %in% c("Antigua and Barbuda", "Aruba", "Argentina", "Bahamas", "Barbados", "Belize",
                 "Bolivia", "Brazil", "Cayman Islands", "Chile", "Colombia", "Costa Rica",
                 "Cuba", "Dominica", "Dominican Republic", "Ecuador", "El Salvador",
                 "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Jamaica",
                 "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico",
                 "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and Grenadines",
                 "Suriname", "Trinidad and Tobago", 
                 "Uruguay", "Venezuela") ~ "Central-South America",
  country_name %in% c("Greenland", "Canada", "United States of America", "Mexico") ~ "North America",
  country_name %in% c("Australia", "Fiji", "New Zealand", "Papua New Guinea", "Samoa",
                 "Solomon Islands", "Vanuatu", "Kiribati", "Marshall Islands",
                 "Micronesia", "Nauru", "Palau", "Tonga", "Tuvalu") ~ "Oceania",
  
  country_name %in% c("Anguilla", "French Guiana", "Guadeloupe", "Martinique", "Falkland Islands (Malvinas)", "Montserrat",
                 "Saint Pierre and Miquelon", "Bermuda", "Turks and Caicos Islands") ~ "Central-South America",
  
  country_name %in% c("American Samoa", "British Virgin Islands", "Cook Islands", "French Polynesia", "New Caledonia",
                 "Niue", "Tokelau", "Wallis and Futuna", "Virgin Islands (USA)") ~ "Oceania",
  TRUE ~ NA
))
a <- pop_est %>% group_by(continent, year) %>% summarise(population = sum(population, na.rm = T))
table(a$continent, exclude = F)

ggplot(a, aes(x = year, y = population, colour = continent)) +
  geom_line() +
  scale_y_continuous(
    labels = function(x) {
      paste0(sprintf(x/1e9, fmt = "%.2f"), "B")
    }
  ) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Population Distribution",
       x = "Year",
       y = "Population") +
  theme_minimal()
# by age groups
a <- pop_est %>% group_by(age, year) %>% summarise(population = sum(population, na.rm = T))
table(a$age)
ggplot(a, aes(x = year, y = population, colour = age)) +
  geom_line() +
  scale_y_continuous(
    labels = function(x) {
      paste0(sprintf(x/1e9, fmt = "%.2f"), "B")
    }
  ) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Population Distribution",
       x = "Year",
       y = "Population") +
  theme_minimal()

# WHO MDB pre-processing ----
mort <- db %>% filter(sex != 9) %>% select(-usability, -completeness, -percentageofilldefinedornonspeci) %>%
  left_join(dq %>% select(country_name, quality, ill_defined, usability, completeness), by="country_name") %>%
  mutate(
    # revisione Lancet --> RA: M05
    ra_M05 = if_else(str_starts(cause, "M05"), 1, 0),
    ra_M06 = if_else(str_starts(cause, "M06"), 1, 0),
    t2d = ifelse(str_starts(cause, "E11|E13"), 1, 0),
    cvd = if_else(str_starts(cause, "I"), 1, 0),
         SpA = if_else(str_starts(cause, "M07|M46.0|M46.1|M46.8|M46.9|M45"), 1, 0),
         PsA = if_else(str_starts(cause, "L405|M070|M071|M072|M073"), 1, 0),
         neoplasm = if_else(str_starts(cause, "C|D0|D1|D2|D3|D4"), 1, 0),
         sex = factor(if_else(sex == "1", "M", "F", missing = NA)))

melt_cols <- paste0("deaths", 2:26) #remove all ages deaths
mort_long <- melt(
  mort,
  measure.vars = melt_cols,
  variable.name = "age",
  value.name = "value"
) %>% mutate(age = as.character(age))

setDT(mort_long)
gc()
age_mapping <- c(rep("0-4", 5), as.character(age.class))
mort_long[, age := {
  num <- as.numeric(gsub("deaths", "", age))
  age_mapping[num]
}]
gc()

rm(mort); rm(db); gc()
mort_long <- mort_long %>% select(-admin1, -subdiv, -list, -frmat, -im_frmat, -im_deaths1:-im_deaths4) %>% filter(!is.na(age)); gc()
mort_long$age <- factor(mort_long$age, levels = age.class)
table(mort_long$age, exclude = F)
gc()
mort_long <- setDT(mort_long)[, .(value = sum(value)), by = setdiff(names(mort_long), "value")]
#head(mort_long)
gc()
mort_long <- mort_long[, cause := fcase(
  cvd == 1, "CVD",
  neoplasm == 1, "Neoplasms",
  t2d == 1, "T2D",
  gout == 1, "Gout",
  ra == 1, "RA",
  SpA == 1, "SpA",
  sle == 1, "SLE",
  ssc == 1, "SSc",
  default = "All causes"  # if none of the above
)]

mort_long$cause <- factor(mort_long$cause, levels = c(CAUSE, "All causes"))
table(mort_long$cause)
mort_long$other_cause <- factor(
  ifelse(mort_long$ra_M05 == 1, "ra_M05", ifelse( 
    mort_long$ra_M06 ==1, "ra_M06", ifelse(
    mort_long$PsA == 1, "PsA", NA))), levels = c("ra_M05", "ra_M06", "PsA"))
a <- mort_long %>% group_by(year, cause) %>% summarise(mortality = sum(value, na.rm = T)) %>% ungroup()
a[is.na(a)] <- 0
gc()

library(wesanderson); library(ggplot2)
custom_palette <- colorRampPalette(colors = c("#475B63", "#DB7F8E"))(length(CAUSE)+1)

# mortality
g1 <- ggplot(a, aes(x = year, y = mortality, colour = cause)) +
  geom_line(linewidth = 1, alpha = 0.8) +  
  scale_color_manual(values = custom_palette) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::label_comma()  
  ) +
  labs(
    title = "WHO Mortality Trends",
    x = "Year",
    y = "Mortality (log)",
    colour = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),  # Cleaner grid
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  guides(colour = guide_legend(nrow = 2)) 
g1


# link pop and mortality ----
k <- mort_long %>% group_by(country_name, year, sex, age, usable_broad, usable_strict, usability, other_cause) %>% 
  summarise(mortality = sum(value, na.rm = T)) %>% ungroup()
table(k$other_cause)
a <- mort_long %>% group_by(country_name, year, sex, age, usable_broad, usable_strict, usability, cause) %>% 
  summarise(mortality = sum(value, na.rm = T)) %>% ungroup()
gc()
table(a$cause)
rm(mort_long); gc()
# check duplicates
pp <- a %>% group_by(across(c("country_name", "sex", "age", "year", "cause"))) %>% tally() %>% filter(n > 1)  

PAESI <- unique(a$country_name[!is.na(a$country_name)]) #paesi con una qualche mortalità mai mandata in cause a WHO

GRIGLIA <- expand.grid(year = TIMESPAN, 
                       sex = c("F", "M"), 
                       age = age_labels, 
                       country_name = PAESI, 
                       cause = CAUSE)
aa <- GRIGLIA %>% left_join(a, by = c("country_name", "sex", "age", "year", "cause")) %>% arrange(country_name, year, age, sex, cause) %>% 
  left_join(pop_est %>% filter(year %in% TIMESPAN), by = c("country_name", "sex", "age", "year"))

GRIGLIA2 <- expand.grid(year = TIMESPAN, 
                       sex = c("F", "M"), 
                       age = age_labels, 
                       country_name = PAESI, 
                       cause = c("ra_M05", "ra_M06", "PsA"))
kk <- GRIGLIA2 %>% left_join(k %>% rename(cause = other_cause), by = c("country_name", "sex", "age", "year", "cause")) %>% arrange(country_name, year, age, sex, cause) %>% 
  left_join(pop_est %>% filter(year %in% TIMESPAN), by = c("country_name", "sex", "age", "year"))
# imputo a 0 le cause non rilevate per blocchi, se c'è qualche dato di mortalità
who_broad <- aa %>% 
  group_by(year, age, sex, country_name) %>% 
  mutate(
    usable_broad = if (all(is.na(usable_broad))) NA_real_ else first(na.omit(usable_broad)),
    usable_strict = if (all(is.na(usable_strict))) NA_real_ else first(na.omit(usable_strict)),
    usability = if (all(is.na(usability))) NA_real_ else first(na.omit(usability)),
    mortality = if (all(is.na(mortality))) {
      NA_real_
    } else {
      replace(mortality, is.na(mortality), 0)
    }
  ) %>% 
  ungroup()

who_broad$country_name[who_broad$country_name %in% "Republic of Korea"] <- "South Korea"
who_broad$world_bank <- NA
who_broad$world_bank[who_broad$country_name %in% low_income] <- "LI"
who_broad$world_bank[who_broad$country_name %in% lower_middle_income] <- "LMI"
who_broad$world_bank[who_broad$country_name %in% upper_middle_income] <- "UMI"
who_broad$world_bank[who_broad$country_name %in% high_income] <- "HI"
table(who_broad$country_name[is.na(who_broad$world_bank)])

setDT(who_broad)
wt <- who_broad %>% 
  group_by(year, country_name, cause) %>% summarise(mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)) %>% 
  ungroup() %>% filter(cause == "All causes") %>% arrange(country_name, desc(year)) %>% group_by(country_name) %>% slice(1) %>% as.data.table()
wt[, incidence_rate := (mortality / population) * 1e5]
library(sf); library(rnaturalearth)

world <- ne_countries(scale = "medium", returnclass = "sf") %>% mutate(country_name = sovereignt)

world$sovereignt[world$sovereignt %in% "Federated States of Micronesia"] <- "Micronesia"
world$sovereignt[world$sovereignt %in% "eSwatini"] <- "Eswatini"
world_data <- merge(world, 
                    wt %>% select(country_name, incidence_rate) %>% filter(!is.na(country_name) & !incidence_rate == Inf), 
                    by.x = "country_name", 
                    by.y = "country_name", 
                    all.x = TRUE)


# save data ----
who <- who_broad %>% filter(!is.na(population) & !is.na(continent) & !is.na(cause) & !is.na(country_name) & !is.na(world_bank)) %>% 
  mutate(usa60 = if_else(usability >= 60, 1, 0, missing = 0), usa80 = if_else(usability >= 80, 1, 0, missing = 0)) %>% as.data.frame()
save(who, file = "data/who.Rdata")

# checks
ae <- who %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>%
  group_by(country_name, cause) %>% summarise(morti = sum(na.rm = T, mortality)) %>%
  pivot_wider(names_from = cause, values_from = morti) %>%
  mutate(IMDs = RA + SLE + Gout + SSc + SpA, 
         deaths = RA + SpA + SLE + Gout + SSc + CVD + Neoplasms + `All causes`) 
ae$country_name[ae$IMDs == 0]
nrow(ae)
ae$country_name[ae$IMDs < 10]
overall <- ae %>% filter(IMDs >= 10)
nrow(overall)

Pae <- who %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>%
  group_by(country_name, age, sex, year) %>% slice(1) %>% group_by(country_name, year) %>% 
  summarise(population = sum(na.rm = T, population)) %>%
  group_by(country_name) %>% summarise(population = round(sum(na.rm = T, population)))
overall <- overall %>% left_join(Pae)
Q <- ifelse(QUALITY %in% "usa60", 60, 80)
overall <- overall %>% left_join(dq, by = "country_name") 
overall$country_name[is.na(overall$usability)]
overall <- overall %>% filter(usability >= Q)
# OVERALL deaths summary
OVERALL <- overall %>% dplyr::select(country_name, population, deaths, IMDs, RA, Gout, SpA, SLE, SSc, CVD, Neoplasms, T2D) %>% 
  mutate(cruderate = round(as.numeric(deaths) / as.numeric(population) * 1000, 2))
OVERALL <- OVERALL %>% filter(!country_name %in% BANNED_COUNTRIES)

OVERALL1 <- OVERALL %>% ungroup() %>% summarise(across(population:T2D, ~ sum(.x))) %>% 
  mutate(country_name = "Overall")
OVERALL <- OVERALL1 %>% bind_rows(OVERALL) %>% mutate(cruderate = round(as.numeric(deaths) / as.numeric(population) * 1000, 2))
OVERALL$country_name
