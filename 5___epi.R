require(readr); library(mgcv); require(MASS); require(splines); library(RColorBrewer); require(ggplot2); library(gratia)
PALETTA <- c(1, "#401F3E", "#759AAB")
z2 <- read_csv("G:/Il mio Drive/UNIVPM/WHO/data/epidems/old2.csv")
due <- read_csv("G:/Il mio Drive/UNIVPM/WHO/data/epidems/princ.csv")
tre <- read_csv("G:/Il mio Drive/UNIVPM/WHO/data/epidems/integra.csv")
quattro <- read_csv("G:/Il mio Drive/UNIVPM/WHO/data/epidems/integra2.csv")
cinque <- read_csv("G:/Il mio Drive/UNIVPM/WHO/data/epidems/CVD-integration.csv")
sei <- read_csv("G:/Il mio Drive/UNIVPM/WHO/data/epidems/neoplasms-integration.csv")
sette <- read_csv("G:/Il mio Drive/UNIVPM/WHO/data/epidems/IHME-GBD_2023_DATA-a01f5990-1.csv")[, -1]

quattro <- quattro %>% dplyr::select(any_of(names(tre)))
due2 <- bind_rows(z2, due, tre, quattro, cinque, sei, sette) %>% distinct()

setdiff(sc, unique(due$location))
due <- due2 %>% arrange(measure, cause, location, year, sex, age) %>% filter( cause %in% c("RA", "CVD", "Neoplasms", "Gout") & metric == "Rate" & year >= REPR_YEARS[1] & year <= REPR_YEARS[2] &
  location %in% sc
) %>% mutate(age_cut = ifelse(age %in% c("0-4", "5-9", "10-14", "15-19"), "0-19", 
                   ifelse(age %in% c("20-24", "25-29", "30-34", "35-39"), "20-39", 
                   ifelse(age %in% c("40-44", "45-49", "50-54", "55-59"), "40-59", 
                   ifelse(age %in% c("60-64", "65-69", "70-74"), "60-74",
                   ifelse(age %in% c("75-79", "80-84", "85-89", "90-94", "95-99"), "75+", NA))))))
due <- due %>% arrange(measure, cause, location, year, sex, age, desc(val)) %>% 
  group_by(measure, cause, location, year, sex, age) %>% slice(1) %>% ungroup()
table(due$location)
# inserisco continent e world_bank
load("data/WHO-imputed-2023.Rdata")

app0 <- whoimp %>% dplyr::select(country_name, continent, world_bank, population, year, age, sex) %>% distinct()
app0$sex <- factor(ifelse(app0$sex == "F", "Female", "Male"), levels = c("Female", "Male"))
due <- due %>% left_join(app0, by = join_by(location == country_name, year == year, age == age, sex == sex))
due$age_cut <- factor(due$age_cut)
table(due$location[due$continent == "Europe" & due$cause == "CVD"])
prev <- due %>% filter(measure == "Prevalence")
inc <- due %>% filter(measure == "Incidence")
63*4*20*24*2*2 # per country
table(due$continent, due$world_bank, exclude = F)
dis <- "CVD"
table(inc$location[inc$cause==dis])
table(prev$location[prev$cause==dis])
setdiff(sc, unique(inc$location[inc$cause=="RA"]))
setdiff(sc, unique(inc$location[inc$cause=="Gout"]))
setdiff(sc, unique(inc$location[inc$cause=="CVD"]))
setdiff(sc, unique(inc$location[inc$cause=="Neoplasms"]))
setdiff(sc, unique(prev$location[prev$cause=="RA"]))
setdiff(sc, unique(prev$location[prev$cause=="Gout"]))
setdiff(sc, unique(prev$location[prev$cause=="CVD"]))
setdiff(sc, unique(prev$location[prev$cause=="Neoplasms"]))

# visual GOTTA ----
load("figure/PREDS-Gout.Rdata")
# from x 1,000,000 to x 100,000
pred_data <- pred_data %>% subset(year >= 2000)
pred_data$fit <- pred_data$fit/10
pred_data$lower <- pred_data$lower/10
pred_data$upper <- pred_data$upper/10
gout_inc <- bind_rows(
  inc %>% filter(cause %in% "Gout") %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)

gout_prev <- bind_rows(
  prev %>% filter(cause %in% c("Gout")) %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)
g.gout <- ggplot() +
  geom_ribbon(data = gout_prev, aes(x = year, ymin = lower, ymax = upper, fill = "Prevalence"), alpha = 0.3) +
  geom_line(data = gout_prev, aes(x = year, y = val, color = "Prevalence"), linewidth = 1) +
  geom_ribbon(data = gout_inc, aes(x = year, ymin = lower, ymax = upper, fill = "Incidence"), alpha = 0.3) +
  geom_line(data = gout_inc, aes(x = year, y = val, color = "Incidence"), linewidth = 1) +
  coord_cartesian(xlim = c(2000, 2023)) +
  scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
  labs(x = "Year", y = "Rate (x100,000)", color = "", fill = "", title = "Gout") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
g.mort <- ggplot() + 
  geom_line(data = pred_data, aes(x = year, y = fit, color = "Mortality"), linewidth = 1) +
  geom_ribbon(data = pred_data, aes(x = year, ymin = lower, ymax = upper, fill = "Mortality"), alpha = 0.6) +
  coord_cartesian(xlim = c(2000, 2023)) +
  scale_y_continuous(limits = c(.01, .5))+
  scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
  labs(x = "Year", y = "ASMR (x100,000)", color = "", fill = "") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
fg.gout <- g.gout + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  g.mort+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(
      #plot.caption = element_text(size = 14, hjust = 0.5)
    )
  ) &
  theme(
    plot.caption.position = "panel",
    legend.position = "bottom",                    # Move legend to bottom
    legend.direction = "horizontal",               # Horizontal arrangement
    legend.text = element_text(size = 12),         # Legend text size
    legend.title = element_text(size = 14),        # Legend title size
    legend.key.size = unit(1.5, "lines"),          # Legend key (color box) size
    legend.spacing = unit(0.5, "cm"),              # Spacing between legend items
    legend.box = "horizontal",                     # Horizontal legend box
    legend.justification = "center"                # Center justify the legend
  )
fg.gout
# visual RA ----
load("figure/PREDS-RA.Rdata")
# from x 1,000,000 to x 100,000
pred_data <- pred_data %>% subset(year >= 2000)
pred_data$fit <- pred_data$fit/10
pred_data$lower <- pred_data$lower/10
pred_data$upper <- pred_data$upper/10
ra_inc <- bind_rows(
  inc %>% filter(cause %in% "RA") %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)

ra_prev <- bind_rows(
  prev %>% filter(cause %in% c("RA")) %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)

PALETTA[2:3] <- c("purple4", "aquamarine4")
g.ra <- ggplot() +
  geom_ribbon(data = ra_prev, aes(x = year, ymin = lower, ymax = upper, fill = "Prevalence"), alpha = 0.3) +
  geom_line(data = ra_prev, aes(x = year, y = val, color = "Prevalence"), linewidth = 1) +
  geom_ribbon(data = ra_inc, aes(x = year, ymin = lower, ymax = upper, fill = "Incidence"), alpha = 0.3) +
  geom_line(data = ra_inc, aes(x = year, y = val, color = "Incidence"), linewidth = 1) +
  coord_cartesian(xlim = c(2000, 2023)) +
  scale_x_continuous(breaks = 2000:2023)+
  labs(x = "Year", y = "Rate (x100,000)", color = "", fill = "", title = "Rheumatoid arthritis") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
g.ra.mort <- ggplot() + 
  geom_line(data = pred_data, aes(x = year, y = fit, color = "Mortality"), linewidth = 1) +
  geom_ribbon(data = pred_data, aes(x = year, ymin = lower, ymax = upper, fill = "Mortality"), alpha = 0.3) +
  coord_cartesian(xlim = c(2000, 2023)) +
  scale_y_continuous(limits = c(.01, .2))+
  scale_x_continuous(breaks = 2000:2023)+
  labs(x = "Year", y = "ASMR (x100,000)", color = "", fill = "") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
fg.ra <- g.ra + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  g.ra.mort+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(
      #plot.caption = element_text(size = 14, hjust = 0.5)
    )
  ) &
  theme(
    plot.caption.position = "panel",
    legend.position = "bottom",                    # Move legend to bottom
    legend.direction = "horizontal",               # Horizontal arrangement
    legend.text = element_text(size = 12),         # Legend text size
    legend.title = element_text(size = 14),        # Legend title size
    legend.key.size = unit(1.5, "lines"),          # Legend key (color box) size
    legend.spacing = unit(0.5, "cm"),              # Spacing between legend items
    legend.box = "horizontal",                     # Horizontal legend box
    legend.justification = "center"                # Center justify the legend
  )
fg.ra
# GRAFICO PER ARTICOLO ----
require(patchwork)
epi <- (fg.ra / fg.gout) + plot_layout(guides = "collect")
epi
jpeg(filename = paste0("figure/EPI-RA-Gout-", QUALITY, ".jpg"), units = "px", width = 2500, height = 1800, res = 200, quality = 100)
print(epi); dev.off()

# visual CVD ----
load("figure/PREDS-CVD.Rdata")
# from x 1,000,000 to x 100,000
PALETTA[2:3] <- c("#8FB8DE", "#6A605C")
pred_data <- pred_data %>% subset(year >= 2000)
pred_data$fit <- pred_data$fit/10
pred_data$lower <- pred_data$lower/10
pred_data$upper <- pred_data$upper/10
cvd_inc <- bind_rows(
  inc %>% filter(cause %in% "CVD") %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)

cvd_prev <- bind_rows(
  prev %>% filter(cause %in% c("CVD")) %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)
g.cvd <- ggplot() +
  geom_ribbon(data = cvd_prev, aes(x = year, ymin = lower, ymax = upper, fill = "Prevalence"), alpha = 0.3) +
  geom_line(data = cvd_prev, aes(x = year, y = val, color = "Prevalence"), linewidth = 1) +
  geom_ribbon(data = cvd_inc, aes(x = year, ymin = lower, ymax = upper, fill = "Incidence"), alpha = 0.3) +
  geom_line(data = cvd_inc, aes(x = year, y = val, color = "Incidence"), linewidth = 1) +
  coord_cartesian(xlim = c(2000, 2023)) +
  scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
  labs(x = "Year", y = "Rate (x100,000)", color = "", fill = "", title = "CVD") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
g.mort <- ggplot() + 
  geom_line(data = pred_data, aes(x = year, y = fit, color = "Mortality"), linewidth = 1) +
  geom_ribbon(data = pred_data, aes(x = year, ymin = lower, ymax = upper, fill = "Mortality"), alpha = 0.6) +
  coord_cartesian(xlim = c(2000, 2023)) +
  #scale_y_continuous(limits = c(.01, .20))+
  scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1)) +
  labs(x = "Year", y = "ASMR (x100,000)", color = "", fill = "") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
fg.cvd <- g.cvd + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  g.mort+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plot_layout(guides = "collect") &
  theme(
    plot.caption.position = "panel",
    legend.position = "bottom",                    # Move legend to bottom
    legend.direction = "horizontal",               # Horizontal arrangement
    legend.text = element_text(size = 12),         # Legend text size
    legend.title = element_text(size = 14),        # Legend title size
    legend.key.size = unit(1.5, "lines"),          # Legend key (color box) size
    legend.spacing = unit(0.5, "cm"),              # Spacing between legend items
    legend.box = "horizontal",                     # Horizontal legend box
    legend.justification = "center"                # Center justify the legend
  )
fg.cvd
# visual Neoplasms ----
load("figure/PREDS-Neoplasms.Rdata")
# from x 1,000,000 to x 100,000
pred_data <- pred_data %>% subset(year >= 2000)
pred_data$fit <- pred_data$fit/10
pred_data$lower <- pred_data$lower/10
pred_data$upper <- pred_data$upper/10
neoplasms_inc <- bind_rows(
  inc %>% filter(cause %in% "Neoplasms") %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)

neoplasms_prev <- bind_rows(
  prev %>% filter(cause %in% c("Neoplasms")) %>% group_by(year, cause) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population))
)

PALETTA[2:3] <- c("#429EA6", "#CC998D")
g.neoplasms <- ggplot() +
  geom_ribbon(data = neoplasms_prev, aes(x = year, ymin = lower, ymax = upper, fill = "Prevalence"), alpha = 0.3) +
  geom_line(data = neoplasms_prev, aes(x = year, y = val, color = "Prevalence"), linewidth = 1) +
  geom_ribbon(data = neoplasms_inc, aes(x = year, ymin = lower, ymax = upper, fill = "Incidence"), alpha = 0.3) +
  geom_line(data = neoplasms_inc, aes(x = year, y = val, color = "Incidence"), linewidth = 1) +
  coord_cartesian(xlim = c(2000, 2023)) +
  scale_x_continuous(breaks = 2000:2023)+
  labs(x = "Year", y = "Rate (x100,000)", color = "", fill = "", title = "Neoplasms") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
g.neoplasms.mort <- ggplot() + 
  geom_line(data = pred_data, aes(x = year, y = fit, color = "Mortality"), linewidth = 1) +
  geom_ribbon(data = pred_data, aes(x = year, ymin = lower, ymax = upper, fill = "Mortality"), alpha = 0.3) +
  coord_cartesian(xlim = c(2000, 2023)) +
  #scale_y_continuous(limits = c(.01, .2))+
  scale_x_continuous(breaks = 2000:2023)+
  labs(x = "Year", y = "ASMR (x100,000)", color = "", fill = "") +
  scale_color_manual(values = c("Mortality" = PALETTA[1],
                                "Prevalence" = PALETTA[2], 
                                "Incidence" = PALETTA[3])) +
  scale_fill_manual(values = c("Mortality" = PALETTA[1],
                               "Prevalence" = PALETTA[2], 
                               "Incidence" = PALETTA[3])) +
  theme_minimal()
fg.neoplasms <- g.neoplasms + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  g.neoplasms.mort+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(
      #plot.caption = element_text(size = 14, hjust = 0.5)
    )
  ) &
  theme(
    plot.caption.position = "panel",
    legend.position = "bottom",                    # Move legend to bottom
    legend.direction = "horizontal",               # Horizontal arneoplasmsngement
    legend.text = element_text(size = 12),         # Legend text size
    legend.title = element_text(size = 14),        # Legend title size
    legend.key.size = unit(1.5, "lines"),          # Legend key (color box) size
    legend.spacing = unit(0.5, "cm"),              # Spacing between legend items
    legend.box = "horizontal",                     # Horizontal legend box
    legend.justification = "center"                # Center justify the legend
  )
fg.neoplasms
# GRAFICO PER ARTICOLO ----
require(patchwork)
epi <- (fg.cvd / fg.neoplasms) + plot_layout(guides = "collect")
epi
jpeg(filename = paste0("figure/EPI-Neo-CVD-", QUALITY, ".jpg"), units = "px", width = 2500, height = 1800, res = 200, quality = 100)
print(epi); dev.off()


# test
require(patchwork)
# visual by sex ----
dis <- "CVD"
dis <- "RA"
dis <- "Neoplasms"
dis <- "Gout"
prev$continent[prev$location == "New Zealand" & prev$cause == "CVD"]
# incidence
gout_inc <- bind_rows(
  inc %>% filter(cause %in% dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>% mutate(sex = "Total"),
  inc %>% filter(cause %in% dis) %>% group_by(year, sex) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup()
)
ggplot(gout_inc)+
  geom_line(aes(year, val, colour = sex))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)
# prevalence
gout_prev <- bind_rows(
  prev %>% filter(cause == dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% mutate(sex = "Total"),
  prev %>% filter(cause == dis) %>% group_by(year, sex) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)))
ggplot(gout_prev)+
  geom_line(aes(year, val, colour = sex))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)

# visual by age ----
require(ggplot2)
dis <- "CVD"
# incidence
gout_inc <- bind_rows(
  inc %>% filter(cause == dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% mutate(age_cut = "Total"),
  inc %>% filter(cause == dis) %>% group_by(year, age_cut) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)))
ggplot(gout_inc)+
  geom_line(aes(year, val, colour = age_cut))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)
# prevalence
gout_prev <- bind_rows(
  prev %>% filter(cause == dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% mutate(age_cut = "Total"),
  prev %>% filter(cause == dis) %>% group_by(year, age_cut) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)))
ggplot(gout_prev)+
  geom_line(aes(year, val, colour = age_cut))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)

# visual by continent ----
require(ggplot2)
# incidence
gout_inc <- bind_rows(
  inc %>% filter(cause == dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% mutate(continent = "Total"),
  inc %>% filter(cause == dis) %>% group_by(year, continent) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)))
ggplot(gout_inc)+
  geom_line(aes(year, val, colour = continent))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)
# prevalence
gout_prev <- bind_rows(
  prev %>% filter(cause == dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% mutate(continent = "Total"),
  prev %>% filter(cause == dis) %>% group_by(year, continent) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)))
ggplot(gout_prev)+
  geom_line(aes(year, val, colour = continent))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)
# visual by wealth ----
require(ggplot2)
# incidence
gout_inc <- bind_rows(
  inc %>% filter(cause == dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% mutate(world_bank = "Total"),
  inc %>% filter(cause == dis) %>% group_by(year, world_bank) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)))
ggplot(gout_inc)+
  geom_line(aes(year, val, colour = world_bank))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)
# prevalence
gout_prev <- bind_rows(
  prev %>% filter(cause == dis) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% mutate(world_bank = "Total"),
  prev %>% filter(cause == dis) %>% group_by(year, world_bank) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)))
ggplot(gout_prev)+
  geom_line(aes(year, val, colour = world_bank))+
  #geom_errorbar(aes(x = year, ymin = lower, ymax = upper))+
  xlim(2000, 2023)
# model APC ----
## incidence ----
### global ----
pg <- AAPCi <- list()
for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- inc %>% filter(cause == i) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
           ) 
  cat(i); AAPC <- d2 <- list()
  
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc, method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc, method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc, method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
  AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
  lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
  uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
  d2 <- derchange(d2, slope_threshold = 1)
  d2_c <- d2 %>% filter(meaningful_change == T) %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
    pull(year) %>% unique()
  pred_data <- data.frame(year = REPR_YEARS[1], REPR_YEARS[2], length = 100)
  pred_data$fit <- predict(g, newdata = pred_data)
  pg[[i]] <- ggplot() +
    geom_point(data = dinc, 
               aes(x = year, y = log_val), alpha = 0.3) +
    geom_line(data = pred_data, aes(x = year, y = fit), color = "blue", linewidth = 1) +
    geom_vline(xintercept = d2_c, color = "red", linetype = "dashed") +
    labs(title = i, x = "Year", y = "Incidence rate (x 100,000)") +
    theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1))
  print(pg[[i]])
  jpeg(filename = paste0("figure/inc-jp-", QUALITY, "-", i, ".jpg"), units = "px", width = 2400, height = 1800, res = 300, quality = 100)
  print(pg[[i]]); dev.off()
  # 
  AAPC$AAPC_lo <- lAAPC$AAPC
  AAPC$AAPC_hi <- uAAPC$AAPC
  if (lAAPC$AAPC > uAAPC$AAPC){
    AAPC$AAPC_lo <- uAAPC$AAPC
    AAPC$AAPC_hi <- lAAPC$AAPC
  }
  AAPCi[[i]] <- apply(AAPC, 2, mean)
  
}
global.AAPCic <- data.frame(AAPCi %>% bind_rows(), outcome = names(AAPCi))

### by sex ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- inc %>% filter(cause == i) %>% group_by(year, sex) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in c("Male", "Female")){
  g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(sex == j), method = "REML", select = TRUE)
  gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(sex == j), method = "REML", select = TRUE)
  gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(sex == j), method = "REML", select = TRUE)
  #gam.check(g)
  d2 <- derivatives(g, term = "s(year)", order = 2)
  
  AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
  lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
  uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
  
  d2 <- derchange(d2, slope_threshold = 1)
  d2_c <- d2 %>% filter(meaningful_change == T) %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
    pull(year) %>% unique()
  # 
  AAPC$AAPC_lo <- lAAPC$AAPC
  AAPC$AAPC_hi <- uAAPC$AAPC
  if (lAAPC$AAPC > uAAPC$AAPC){
    AAPC$AAPC_lo <- uAAPC$AAPC
    AAPC$AAPC_hi <- lAAPC$AAPC
  }
  AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(sex = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
bysex.AAPCic <- AAPCi %>% bind_rows()

### by age class ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- inc %>% filter(cause == i) %>% group_by(year, age_cut) %>% 
    #summarise(
    #  pop = sum(population, na.rm = T), 
    #  val2 = weighted.mean(na.rm = T, val, population)
    #) %>%
    summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(inc$age_cut))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(age_cut == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(age_cut == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(age_cut == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(age_cut = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
byage.AAPCic <- AAPCi %>% bind_rows()

### by continent ----

AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- inc %>% filter(cause == i) %>% group_by(year, continent) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(inc$continent))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(continent == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(continent == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(continent == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(continent = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
bycontinent.AAPCic <- AAPCi %>% bind_rows()

### by world_bank ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- inc %>% filter(cause == i) %>% group_by(year, world_bank) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(inc$world_bank))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(world_bank == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(world_bank == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(world_bank == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(world_bank = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- byworld_bank.AAPCic <- AAPCi %>% bind_rows()
cond <- byworld_bank.AAPCic$AAPC < byworld_bank.AAPCic$AAPC_lo

byworld_bank.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
byworld_bank.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]
### by country ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- inc %>% filter(cause == i) %>% group_by(year, location) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(inc$location))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(location == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(location == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(location == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(location = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- bylocation.AAPCic <- AAPCi %>% bind_rows()
cond <- bylocation.AAPCic$AAPC < bylocation.AAPCic$AAPC_lo

bylocation.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
bylocation.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]

### otheer regions ----
EU <- c("Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
        "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
west.EU <- c("Belgium", "Denmark", "France", "Germany", #"Greece", 
             "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "United Kingdom",
             "Austria", "Sweden", "Malta", "Finland", "Iceland", "Norway", "Switzerland")
east.EU <- c(setdiff(EU, west.EU), "Serbia", "Republic of Moldova")
global.south <- c("Argentina", "Australia", "Brazil", "Mexico", "South Africa", "Chile", "Venezuela", "Nicaragua", "Guatemala",
                  "Ecuador", "Colombia", "Cuba", "Guyana", "Jamaica", "Kyrgyzstan", "Panama", "Paraguay", "Philippines", "Suriname", "Thailand",
                  "Belize", "Uzbekistan", "Uruguay", "Georgia", "New Zealand", "Republic of Moldova", "Armenia", "Costa Rica")
global.north <- c("Canada", "Japan", EU, "Iceland", "Norway", "United Kingdom", "United States of America", "Turkey", "Bahamas", "Israel", "Switzerland", "Serbia")

AAPCj <- AAPCi <- list()

inc <- inc %>% mutate(northsouth = ifelse(location %in% global.north, "Global North", "Global South"))
for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- inc %>% filter(cause == i) %>% group_by(year, northsouth) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(inc$northsouth))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(northsouth == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(northsouth == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(northsouth == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(northsouth = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- bynorthsouth.AAPCic <- AAPCi %>% bind_rows()
cond <- bynorthsouth.AAPCic$AAPC < bynorthsouth.AAPCic$AAPC_lo

bynorthsouth.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
bynorthsouth.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]

# by eu
AAPCj <- AAPCi <- list()

ince <- inc %>% filter(location %in% c(east.EU, west.EU)) %>% mutate(eastwest = ifelse(location %in% east.EU, "Eastern Europe", "Western Europe"))
for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dinc <- ince %>% filter(cause == i) %>% group_by(year, eastwest) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(ince$eastwest))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(eastwest == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(eastwest == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dinc %>% filter(eastwest == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(eastwest = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- byeastwest.AAPCic <- AAPCi %>% bind_rows()
cond <- byeastwest.AAPCic$AAPC < byeastwest.AAPCic$AAPC_lo

byeastwest.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
byeastwest.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]

## metti in formato ----
fxic <- function(e, lo, up){
  formatted <- sprintf("%+.2f%% [%+.2f%%, %+.2f%%]", 
                       e, lo, up)
  return(formatted)
}

resu <- bind_rows(global.AAPCic %>% mutate( strat= "Global"), 
                  bysex.AAPCic %>% rename(strat = sex), byage.AAPCic %>% rename(strat = age_cut), 
                  bycontinent.AAPCic %>% rename(strat = continent), byworld_bank.AAPCic %>% rename(strat = world_bank),
                  byeastwest.AAPCic %>% rename(strat = eastwest), bynorthsouth.AAPCic %>% rename(strat =northsouth),
                  bylocation.AAPCic %>% rename(strat = location)
)
incidence <- resu %>% mutate(APC = fxic(AAPC, AAPC_lo, AAPC_hi)) %>% dplyr::select(outcome, strat, APC) %>%
  pivot_wider(
    names_from = outcome,
    values_from = APC
  ) %>% as.data.frame()


## prevalence ----
### global ----
pg <- AAPCi <- list()
for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- prev %>% filter(cause == i) %>% group_by(year) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev, method = "REML", select = TRUE)
  gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev, method = "REML", select = TRUE)
  gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev, method = "REML", select = TRUE)
  #gam.check(g)
  d2 <- derivatives(g, term = "s(year)", order = 2)
  
  AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
  lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
  uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
  d2 <- derchange(d2, slope_threshold = 1)
  d2_c <- d2 %>% filter(meaningful_change == T) %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
    pull(year) %>% unique()
  pred_data <- data.frame(year = REPR_YEARS[1], REPR_YEARS[2], length = 100)
  pred_data$fit <- predict(g, newdata = pred_data)
  pg[[i]] <- ggplot() +
    geom_point(data = dprev, 
               aes(x = year, y = log_val), alpha = 0.3) +
    geom_line(data = pred_data, aes(x = year, y = fit), color = "blue", linewidth = 1) +
    geom_vline(xintercept = d2_c, color = "red", linetype = "dashed") +
    labs(title = i, x = "Year", y = "previdence rate (x 100,000)") +
    theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(REPR_YEARS[1], REPR_YEARS[2], by = 1))
  print(pg[[i]])
  jpeg(filename = paste0("figure/prev-jp-", QUALITY, "-", i, ".jpg"), units = "px", width = 2400, height = 1800, res = 300, quality = 100)
  print(pg[[i]]); dev.off()
  # 
  AAPC$AAPC_lo <- lAAPC$AAPC
  AAPC$AAPC_hi <- uAAPC$AAPC
  if (lAAPC$AAPC > uAAPC$AAPC){
    AAPC$AAPC_lo <- uAAPC$AAPC
    AAPC$AAPC_hi <- lAAPC$AAPC
  }
  AAPCi[[i]] <- apply(AAPC, 2, mean)
  
}
global.AAPCic <- data.frame(AAPCi %>% bind_rows(), outcome = names(AAPCi))

### by sex ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- prev %>% filter(cause == i) %>% group_by(year, sex) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in c("Male", "Female")){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(sex == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(sex == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(sex == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(sex = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
bysex.AAPCic <- AAPCi %>% bind_rows()

### by age class ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- prev %>% filter(cause == i) %>% group_by(year, age_cut) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(prev$age_cut))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(age_cut == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(age_cut == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(age_cut == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(age_cut = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
byage.AAPCic <- AAPCi %>% bind_rows()

### by continent ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- prev %>% filter(cause == i) %>% group_by(year, continent) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(prev$continent))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(continent == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(continent == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(continent == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(continent = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
bycontinent.AAPCic <- AAPCi %>% bind_rows()

### by world_bank ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- prev %>% filter(cause == i) %>% group_by(year, world_bank) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(prev$world_bank))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(world_bank == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(world_bank == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(world_bank == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(world_bank = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- byworld_bank.AAPCic <- AAPCi %>% bind_rows()
cond <- byworld_bank.AAPCic$AAPC < byworld_bank.AAPCic$AAPC_lo

byworld_bank.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
byworld_bank.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]

### by country ----
AAPCj <- AAPCi <- list()

for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- prev %>% filter(cause == i) %>% group_by(year, location) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(prev$location))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(location == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(location == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(location == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(location = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- bylocation.AAPCic <- AAPCi %>% bind_rows()
cond <- bylocation.AAPCic$AAPC < bylocation.AAPCic$AAPC_lo

bylocation.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
bylocation.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]


### otheer regions ----
EU <- c("Austria", "Belgium", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
        "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
west.EU <- c("Belgium", "Denmark", "France", "Germany", #"Greece", 
             "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "United Kingdom",
             "Austria", "Sweden", "Malta", "Finland", "Iceland", "Norway", "Switzerland")
east.EU <- c(setdiff(EU, west.EU), "Serbia", "Republic of Moldova")
global.south <- c("Argentina", "Australia", "Brazil", "Mexico", "South Africa", "Chile", "Venezuela", "Nicaragua", "Guatemala",
                  "Ecuador", "Colombia", "Cuba", "Guyana", "Jamaica", "Kyrgyzstan", "Panama", "Paraguay", "Philippines", "Suriname", "Thailand",
                  "Belize", "Uzbekistan", "Uruguay", "Georgia", "New Zealand", "Republic of Moldova", "Armenia", "Costa Rica")
global.north <- c("Canada", "Japan", EU, "Iceland", "Norway", "United Kingdom", "United States of America", "Turkey", "Bahamas", "Israel", "Switzerland", "Serbia")

AAPCj <- AAPCi <- list()

prev <- prev %>% mutate(northsouth = ifelse(location %in% global.north, "Global North", "Global South"))
for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- prev %>% filter(cause == i) %>% group_by(year, northsouth) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(prev$northsouth))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(northsouth == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(northsouth == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(northsouth == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(northsouth = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- bynorthsouth.AAPCic <- AAPCi %>% bind_rows()
cond <- bynorthsouth.AAPCic$AAPC < bynorthsouth.AAPCic$AAPC_lo

bynorthsouth.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
bynorthsouth.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]

# by eu
AAPCj <- AAPCi <- list()

preve <- prev %>% filter(location %in% c(east.EU, west.EU)) %>% mutate(eastwest = ifelse(location %in% east.EU, "Eastern Europe", "Western Europe"))
table(preve$cause[preve$eastwest == "Eastern Europe"])
for (i in c("RA", "CVD", "Neoplasms", "Gout")){
  dprev <- preve %>% filter(cause == i) %>% group_by(year, eastwest) %>% summarise(
    val = weighted.mean(na.rm = T, val, population),
    lower = weighted.mean(na.rm = T, lower, population),
    upper = weighted.mean(na.rm = T, upper, population)) %>% ungroup() %>%
    mutate(log_val = log(val),
           log_low = log(lower), log_upp = log(upper),
           invars = 1 / ((log(upper) - log(lower)) / (2 * qnorm(1 - (.05)/2)))^2
    ) 
  cat(i); AAPC <- d2 <- list()
  
  for ( j in levels(as.factor(preve$eastwest))){
    g <- gam(log_val ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(eastwest == j), method = "REML", select = TRUE)
    gl <- gam(log_low ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(eastwest == j), method = "REML", select = TRUE)
    gu <- gam(log_upp ~ s(year, bs = "tp", k = 20), data = dprev %>% filter(eastwest == j), method = "REML", select = TRUE)
    #gam.check(g)
    d2 <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    lAAPC = aapc_gam(gl, REPR_YEARS[1], REPR_YEARS[2])
    uAAPC = aapc_gam(gu, REPR_YEARS[1], REPR_YEARS[2])
    
    d2 <- derchange(d2, slope_threshold = 1)
    d2_c <- d2 %>% filter(meaningful_change == T) %>%
      mutate(sign = sign(.derivative), year = round(year)) %>%
      filter(year > REPR_YEARS[1] & year < REPR_YEARS[2]) %>%
      pull(year) %>% unique()
    # 
    AAPC$AAPC_lo <- lAAPC$AAPC
    AAPC$AAPC_hi <- uAAPC$AAPC
    if (lAAPC$AAPC > uAAPC$AAPC){
      AAPC$AAPC_lo <- uAAPC$AAPC
      AAPC$AAPC_hi <- lAAPC$AAPC
    }
    AAPCj[[j]] <- AAPC %>% as.data.frame() %>% mutate(eastwest = j)
  }
  AAPCi[[i]] <- AAPCj %>% bind_rows() %>% mutate(outcome = i)
}
ops <- byeastwest.AAPCic <- AAPCi %>% bind_rows()
cond <- byeastwest.AAPCic$AAPC < byeastwest.AAPCic$AAPC_lo

byeastwest.AAPCic$AAPC_lo[cond] <- ops$AAPC[cond]
byeastwest.AAPCic$AAPC[cond] <- ops$AAPC_lo[cond]
## metti in formato ----

resu <- bind_rows(global.AAPCic %>% mutate( strat= "Global"), 
                  bysex.AAPCic %>% rename(strat = sex), byage.AAPCic %>% rename(strat = age_cut), 
                  bycontinent.AAPCic %>% rename(strat = continent), byworld_bank.AAPCic %>% rename(strat = world_bank),
                  byeastwest.AAPCic %>% rename(strat = eastwest), bynorthsouth.AAPCic %>% rename(strat =northsouth),
                  bylocation.AAPCic %>% rename(strat = location)
)
prevalence <- resu %>% mutate(APC = fxic(AAPC, AAPC_lo, AAPC_hi)) %>% dplyr::select(outcome, strat, APC) %>%
  pivot_wider(
    names_from = outcome,
    values_from = APC
  ) %>% as.data.frame()



# analisi formale dell'associazione tra APC di inc prev e mortalità ----
country_names <- sort((sc))

# Create 3-letter abbreviations
# Using standard ISO 3166-1 alpha-3 codes where appropriate
country_codes <- c(
  "ARG", "AUS", "AUT", "BHS", "BEL",
  "CAN", "CHL", "COL", "CUB", "CZE",
  "DNK", "ECU", "EST", "FIN", "DEU",
  "GTM", "GUY", "HUN", "JAM", "JPN",
  "KGZ", "LVA", "LTU", "MLT", "MEX",
  "NLD", "NOR", "PAN", "PRY", "PHL",
  "POL", "PRT", "ROU", "SVK", "SVN",
  "ZAF", "ESP", "SUR", "SWE", "THA",
  "TUR", "USA", "URY", "UZB", "BLZ",
  "HRV", "FRA", "GEO", "ISL", "IRL",
  "ITA", "LUX", "NZL", "NIC", "MDA",
  "CHE", "GBR", "VEN", "ARM", "BRA",
  "CRI", "SRB", "ISR"
)

# Create a data frame
countries_df <- data.frame(
  country_name = country_names,
  country_code = country_codes
)

APC <- readxl::read_excel("res/EPI-WHO-usa60-2026-04-21.xlsx", sheet = "Average AAPC")

parse_ci <- function(ci_strings) {
  # Split all strings at once
  parts_matrix <- str_split_fixed(ci_strings, " ", 3)
  
  # Extract numbers from all parts simultaneously
  estimates <- as.numeric(str_extract(parts_matrix[,1], "[-+]?[0-9]*\\.?[0-9]+"))
  lower <- as.numeric(str_extract(parts_matrix[,2], "[-+]?[0-9]*\\.?[0-9]+"))
  upper <- as.numeric(str_extract(parts_matrix[,3], "[-+]?[0-9]*\\.?[0-9]+"))
  
  # Return as data frame
  data.frame(
    estimate = estimates,
    lower = lower,
    upper = upper
  )
}
apc <- preva <- inci <- ex <- list()
i <- "Gout"
high_income <- unique(app0$country_name[app0$world_bank %in% "HI"])
um_income <- unique(app0$country_name[app0$world_bank %in% "UMI"])
lm_income <- unique(app0$country_name[app0$world_bank %in% "LMI"])

for (i in c("Gout", "RA", "CVD", "Neoplasms")){
  apc <- data.frame("strat" = APC$strat, "y" = i, parse_ci(APC[[i]]))
  inci <- data.frame("strat" = incidence$strat, "y" = i, parse_ci(incidence[[i]]))
  names(inci) <- c("strat", "y", "inc.est", "inc.low", "inc.upp")
  preva <- data.frame("strat" = prevalence$strat, "y" = i, parse_ci(prevalence[[i]]))
  names(preva) <- c("strat", "y", "pre.est", "pre.low", "pre.upp")
  
  ex <- apc %>% left_join(inci, by = c("y", "strat")) %>% left_join(preva, by = c("y", "strat")) %>%
    left_join(OVERALL %>% dplyr::select(country_name, IMDs), by = join_by(strat == country_name)) %>% 
    left_join(countries_df, by = join_by(strat == country_name)) %>% 
    mutate(wealth = ifelse(strat %in% high_income, "HI", ifelse(strat %in% lm_income, "LMI", ifelse(strat %in% um_income, "UMI", NA))))

  table(ex$wealth)
  ex <- ex %>% mutate(
    sign_concordance = case_when(
      is.na(inc.est) | is.na(estimate) ~ NA_real_,
      sign(inc.est) == sign(estimate) ~ 1,
      TRUE ~ 0
    ),
    logd = (estimate-pre.est))  %>%
    filter( estimate > -20 & estimate < 20 & #outlier!
            !is.na(estimate) & !is.na(wealth)
           #& estimate > 0
           #& !str_detect(strat, "United States") # outlier!
           )
  table(ex$sign_concordance)
  ggplot(ex, aes(x = estimate, y = inc.est, colour = wealth))+
    geom_point()
  #lowess_fit <- lowess(ex$estimate, ex$inc.est)
  #lines(lowess_fit)
  W <- "LMI"
  cor(ex$estimate[ex$wealth == W], 
      ex$inc.est[ex$wealth == W], method = "pearson")
  ex$wealth <- factor(ex$wealth, levels = c("HI", "UMI", "LMI"))
  table(ex$sign_concordance)
  require(ggrepel)
  pal <- c("#80B192", "#6A8D92", "#4B4E6D")
  PRE.MORT <- ggplot(ex, aes(x = pre.est, y = estimate, colour = wealth))+
    geom_point()+geom_text(aes(label = country_code), check_overlap = T, hjust = -.1, vjust = 0.5, size = 3) +
    geom_smooth(method = "lm", se = F)+
    theme_minimal()+xlab("Prevalence APC(%)")+ylab("Mortality APC(%)")+
    scale_color_manual(values = pal, name = "")+#xlim(-1, 4)+
    guides(colour = guide_legend(
      nrow = 1,                    # Single row for bottom legend
      keywidth = unit(1.5, "cm"),  # Wider color boxes
      keyheight = unit(0.5, "cm"), # Taller color boxes
      override.aes = list(size = 3) # Increase point size in legend
    )) + labs(tag = paste0(which(c("Gout", "RA", "CVD", "Neoplasms") %in% i), "b)")) +
    theme(panel.grid = element_line(color = "gray90", size = 0.3))
  PRE.MORT
  INC.MORT <- ggplot(ex, aes(x = inc.est, y = estimate, colour = wealth))+
    geom_point()+geom_text(aes(label = country_code), check_overlap = T, hjust = -.1, vjust = 0.5, size = 3) +
    geom_smooth(method = "lm", se = F)+
    theme_minimal()+xlab("Incidence APC(%)")+ylab("Mortality APC(%)")+
    scale_color_manual(values = pal, name = "")+#xlim(-1, 4)+
    guides(colour = guide_legend(
      nrow = 1,                    # Single row for bottom legend
      keywidth = unit(1.5, "cm"),  # Wider color boxes
      keyheight = unit(0.5, "cm"), # Taller color boxes
      override.aes = list(size = 3) # Increase point size in legend
    )) +labs(tag = paste0(which(c("Gout", "RA", "CVD", "Neoplasms") %in% i), "a)")) +
    theme(panel.grid = element_line(color = "gray90", size = 0.3))
  INC.MORT
  require(patchwork)
  epifinal <- INC.MORT + PRE.MORT +
    plot_layout(guides = 'collect') &  # Collect all legends
    theme(
      legend.position = 'bottom',
      legend.text = element_text(size = 14),       # Larger legend text
      legend.title = element_text(size = 16, face = "bold"), # Larger title
      legend.key.size = unit(2, "lines"),         # Larger legend keys
      legend.spacing.x = unit(0.5, "cm")          # Space between legend items
    )
  
  epifinal
  assign(paste0("EPI-", i), epifinal)
  png(filename = paste0("figure/Epidemiology-", i, ".jpg"), units = "px", width = 4000, height = 2000, res = 220)
  print(epifinal); dev.off()
  
  #cor(ex$estimate[ex$wealth == W], 
  #    ex$pre.est[ex$wealth == W], method = "pearson")
  #
  #mm <- lm(data = ex, estimate ~ inc.est*pre.est + wealth + inc.est:wealth + pre.est:wealth)
  #performance::check_model(mm)
  #summary(mm)
}

require(patchwork)
gg <- (`EPI-Gout` / `EPI-RA`) + plot_layout(guides = "collect")
gg
png(filename = paste0("figure/Epidemiology-goutRA.jpg"), units = "px", width = 3000, height = 2500, res = 220)
print(gg); dev.off()

gg <- (`EPI-CVD` / `EPI-Neoplasms`) + plot_layout(guides = "collect")
gg
png(filename = paste0("figure/Epidemiology-controls.jpg"), units = "px", width = 3000, height = 2500, res = 220)
print(gg); dev.off()
