require(readr); library(mgcv); require(MASS); require(splines); library(RColorBrewer); require(ggplot2); library(gratia)

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

# prevalence incidence and mortality ----
inc2 <- inc %>% dplyr::select(location, year, age, age_cut, sex, cause, val, continent, world_bank, northsouth) %>% rename(incidence = val, country_name = location)
prev2 <- prev %>% dplyr::select(location, year, age, age_cut, sex, cause, val) %>% rename(prevalence = val, country_name = location)
mort <- whoimp %>% group_by(year, cause, sex, age, age_cut, country_name) %>% summarise(
  mortality = mean(na.rm = T, mortality), population = mean(na.rm = T, population)
) %>% ungroup() # medie bootstrap 
mort <- mort %>% left_join(STD_POPbysex, by = c("age", "sex"))
mort$smr <- mort$mortality / mort$population
mort$w_i <- mort$world2020 # peso relativo della fascia di popolazione
mort$asmr <- mort$mortality * mort$w_i / mort$population * 10 # perché è standardizzato su 1 mln di abitanti
mort$v_asmr <- mort$mortality * (mort$w_i / mort$population)^2
mort$sex <- ifelse(mort$sex == "M", "Male", "Female")
gc()

IPM <- mort %>% subset(year > 1999 & cause %in% c("RA", "Gout", "CVD", "Neoplasms")) %>% left_join(inc2, by = c("year", "cause", "sex", "age", "age_cut", "country_name")) %>% 
  left_join(prev2, by = c("year", "cause", "sex", "age", "age_cut", "country_name")) %>% 
  # mean per le età quinquennali
  group_by(year, cause, sex, age_cut, country_name, continent, world_bank, northsouth) %>%
  summarise(incidence = mean(incidence), prevalence = mean(prevalence), asmr = mean(asmr), v_asmr = mean(v_asmr), 
            mortality = sum(mortality), population = sum(population)) %>% ungroup() %>%
  group_by(cause, sex, age_cut, country_name) %>%
  mutate(delta_asmr = (exp(log(1 + asmr) - lag(log(1 + asmr), default = NA)) - 1)*100) %>% filter(!is.na(delta_asmr))

# se vogliamo fare APC = incidence / prevalence + adjustment model

63*2*5*24*4 # stati * sessi * eta * anni * cause
save(IPM, file = "data/IPM.Rdata")
## meta regression? ----
library(tidyverse); library(splines); library(lme4); library(nlme); library(splines2) ; library(metafor)
OUTL <- 5; 
# Assuming your data frame structure:
# country_name | year | cause | sex | age_cut | asmr | incidence | prevalence
model_a <- model_b <- inc_df <- prev_df <- list()
for (i in c("RA", "Gout", "CVD", "Neoplasms")){
  df <- IPM %>%
    mutate(
      stratum_id = interaction(year, sex, age_cut, drop = TRUE),
      # Ensure factors are properly coded
      cause = factor(cause),
      sex = factor(sex),
      age_cut = factor(age_cut),
      country_name = factor(country_name)
    ) %>% filter(cause == i)# %>% mutate(logasmr = log(1 + asmr))
  # tolgo gli outlier!
  outliers_idx <- which(abs(scale(df$asmr)) > OUTL)
  outliers_data <- df[outliers_idx, c("asmr", "incidence")]
  # Scatter plot of outliers vs incidence
  plot(df$incidence, df$asmr, 
       col = ifelse(abs(scale(df$asmr)) > OUTL, "red", "gray60"),
       pch = 19, cex = 0.7,
       xlab = "Incidence", ylab = "Δ ASMR",
       main = "Bivariate Outliers (Δ ASMR > 10SD)")
  if (length(outliers_idx) > 0) df <- df[-outliers_idx, ]
  
  form1 <- delta_asmr ~ world_bank:ns(incidence, df = 1) + year#:ns(incidence, df = 2)
  form2 <- delta_asmr ~ world_bank:ns(prevalence, df = 1) + year#:ns(prevalence, df = 2)
  VARIANZA.p <- varPower(form = ~ year + prevalence)
  VARIANZA.i <- varPower(form = ~ year + incidence)
  
  if (i %in% c("RA")){
    form1 <- delta_asmr ~ world_bank:ns(I(log(1 + incidence)), df = 3) + year#:ns(incidence, df = 1)
    form2 <- delta_asmr ~ world_bank:ns(I(log(1 + prevalence)), df = 3) + year#:ns(prevalence, df = 1)
    VARIANZA.p <- varFixed(~ 1/population)
    VARIANZA.i <- varFixed(~ 1/population)
  }
  if (i %in% c("Neoplasms")){
    form1 <- delta_asmr ~ world_bank + ns(I(log(1 + incidence)), df = 3) + year#:ns(incidence, df = 1)
    form2 <- delta_asmr ~ world_bank + ns(I(log(1 + prevalence)), df = 3) + year#:ns(prevalence, df = 1)
    VARIANZA.p <- varFixed(~ 1/population)
    VARIANZA.i <- varFixed(~ 1/population)
  }
  if (i %in% c("CVD")){
    form1 <- delta_asmr ~ world_bank + I(log(1 + incidence))#:ns(incidence, df = 1)
    form2 <- delta_asmr ~ world_bank + I(log(1 + prevalence))#:ns(prevalence, df = 1)
    VARIANZA.p <- varPower(form = ~ year + prevalence)
    VARIANZA.i <- varPower(form = ~ year + incidence)
  }
  
  model_a[[i]] <- gls(#family = Gamma(link = "log"), offset = rep(-c, n),
    form2,
    #asmr ~ prevalence + world_bank:prevalence + age_cut + sex + world_bank*year,
    data = df,
    correlation = corAR1(form = ~ year | country_name/stratum_id),  # AR1 autocorrelation
    weights = VARIANZA.p,
    method = "REML"
  )
  model_b[[i]] <- gls(
    form1,
    #asmr ~ incidence + world_bank:incidence + age_cut + sex + world_bank*year,
    data = df,
    correlation = corAR1(form = ~ year | country_name/stratum_id),  # AR1 autocorrelation
    weights = VARIANZA.i,
    method = "REML"
  )
  AIC(model_a[[i]]); AIC(model_b[[i]])
  summary(model_a[[i]])
  summary(model_b[[i]])
  # Residual diagnostics
  library(performance)
  check_model(model_a[[i]])
  check_model(model_b[[i]])
  
  
  library(tidyverse); library(splines); library(emmeans); library(ggeffects); library(viridis); library(emmeans)
  
  # Method A: Using emmeans (marginal predictions)
  emm_model <- emmeans(model_b[[i]], 
                       specs = ~ incidence + world_bank,
                       at = list(
                         incidence = seq(min(df$incidence), 
                                         max(df$incidence), 
                                         length.out = 50),
                         world_bank = levels(df$world_bank),
                         sex = "Female",  # Reference level
                         year = median(df$year),
                         population = median(df$population)
                       ),
                       type = "response")
  
  # Convert to data frame for plotting
  inc_df[[i]] <- as.data.frame(emm_model) %>% rename(asmr = emmean) %>% mutate(cause = i)#, lower.CL = ifelse(lower.CL <0, 0, lower.CL), upper.CL = ifelse(upper.CL <0, 0, upper.CL))

  emm_model <- emmeans(model_a[[i]], 
                       specs = ~ prevalence + world_bank,
                       at = list(
                         prevalence = seq(min(df$prevalence), 
                                         max(df$prevalence), 
                                         length.out = 50),
                         world_bank = levels(df$world_bank),
                         sex = "Female",  # Reference level
                         year = median(df$year),
                         population = median(df$population)
                       ),
                       type = "response")
  
  # Convert to data frame for plotting
  prev_df[[i]] <- as.data.frame(emm_model) %>% rename(asmr = emmean) %>% mutate(cause = i)#, lower.CL = ifelse(lower.CL <0, 0, lower.CL), upper.CL = ifelse(upper.CL <0, 0, upper.CL))
  
}

library(ggplot2)
library(scales)
INC_DF <- bind_rows(inc_df); PREV_DF <- bind_rows(prev_df)

palette <- c("#677DB7", "#322A26", "#4C1036")

i <- "Gout"
for (i in c("RA", "Gout", "CVD", "Neoplasms")){
  
  p <- ggplot(INC_DF %>% filter(cause %in% i), aes(x = incidence, y = asmr, color = world_bank, fill = world_bank)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = world_bank, color = world_bank), alpha = 0.5) +
    geom_line(size = 1.3)+
    geom_hline(size = .8, yintercept = 0, col = "red4", linetype = "dashed")+
    scale_color_manual(values = get("palette"), name = "Wealth category") +
    scale_fill_manual(values = get("palette"), name = "Wealth category") +
    labs(
      tag = ifelse(i %in% c("Gout", "CVD"), "a) ", "c) "), title = i,
      x = "Standardised incidence rate (per 100,000)",
      y = "APC of asSMR (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      axis.title = element_text(face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray80"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  p
  #
  
  q <- ggplot(PREV_DF %>% filter(cause %in% i), aes(x = prevalence, y = asmr, color = world_bank, fill = world_bank)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.5) +
    geom_line(size = 1.3)+
    geom_hline(size = .8, yintercept = 0, col = "red4", linetype = "dashed")+
    scale_color_manual(values = get("palette"), name = "Wealth category") +
    scale_fill_manual(values = get("palette"), name = "Wealth category") +
    labs(tag = ifelse(i %in% c("Gout", "CVD"), "b) ", "d) "), title = i,
      x = "Standardised prevalence rate (per 100,000)",
      y = "APC of asSMR (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      axis.title = element_text(face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray80"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  require(patchwork)
  g <- p + #scale_x_continuous(limits = xiLIM) + 
    #scale_y_continuous(limits = yiLIM)+
    q + #scale_x_continuous(limits = xpLIM) + 
    #scale_y_continuous(limits = ypLIM) + 
    plot_layout(guides = "collect")
  assign(paste0("gepi.", i), g)
}
main <- gepi.Gout / gepi.RA
main
suppl <- gepi.CVD / gepi.Neoplasms
suppl
library(broom)
prev.table <- inc.table <- list()
for (i in c("RA", "Gout", "CVD", "Neoplasms")){
  # prevalence
  coef_summary <- summary(model_a[[i]])$tTable
  conf_ints <- confint(model_a[[i]], level = 0.95)
  tidy_table <- as.data.frame(coef_summary) %>%
    rename(
      Estimate = Value,
      `Std.Error` = Std.Error,
      `t.value` = `t-value`,
      `p.value` = `p-value`
    ) %>%
    rownames_to_column("Term") %>% mutate(
      Lower = conf_ints[, 1],
      Upper = conf_ints[, 2]
    ) %>%
    # Round values
    mutate(
      across(c(Std.Error, t.value), ~ sprintf("%.2f", .)),
      # Format p-values
      `p.value` = case_when(
        `p.value` < 0.001 ~ "<0.001",
        `p.value` < 0.01 ~ sprintf("%.3f", `p.value`),
        TRUE ~ sprintf("%.3f", `p.value`)
      ),
      # Create CI string
      `95% CI` = sprintf("%.2f [%.2f, %.2f]", Estimate, Lower, Upper)
    ) %>% mutate(espo = "Prevalence", cause = i)
  prev.table[[i]] <- tidy_table
  # incidence
  coef_summary <- summary(model_b[[i]])$tTable
  conf_ints <- confint(model_b[[i]], level = 0.95)
  tidy_table <- as.data.frame(coef_summary) %>%
    rename(
      Estimate = Value,
      `Std.Error` = Std.Error,
      `t.value` = `t-value`,
      `p.value` = `p-value`
    ) %>%
    rownames_to_column("Term") %>% mutate(
      Lower = conf_ints[, 1],
      Upper = conf_ints[, 2]
    ) %>%
    # Round values
    mutate(
      across(c(Std.Error, t.value), ~ sprintf("%.2f", .)),
      # Format p-values
      `p.value` = case_when(
        `p.value` < 0.001 ~ "<0.001",
        `p.value` < 0.01 ~ sprintf("%.3f", `p.value`),
        TRUE ~ sprintf("%.3f", `p.value`)
      ),
      # Create CI string
      `95% CI` = sprintf("%.2f [%.2f, %.2f]", Estimate, Lower, Upper)
    ) %>% mutate(espo = "Incidence", cause = i)
  inc.table[[i]] <- tidy_table
}

PREV.table <- bind_rows(prev.table)
INC.table <- bind_rows(inc.table)

jpeg(filename = paste0("figure/EPI-GoutRA-", QUALITY, ".jpg"), units = "px", width = 2500, height = 1800, res = 200, quality = 100)
print(main); dev.off()
jpeg(filename = paste0("figure/EPI-CVDNeo-", QUALITY, ".jpg"), units = "px", width = 2500, height = 1800, res = 200, quality = 100)
print(suppl); dev.off()

