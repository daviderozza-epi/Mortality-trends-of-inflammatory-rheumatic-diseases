library(mgcv); require(MASS); require(splines); library(RColorBrewer);
# Define a fancy palette (can customize)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
conts <- c("Europe", "Asia", "North America", "Central-South America", "Oceania", "Africa")
continent_palette <- brewer.pal(length(conts), "Set2")
world_bank_palette <- brewer.pal(3, "Set3")
sex_palette <- c("#fd1", "#98a")
age_palette <- brewer.pal(5, "Set3")
names(continent_palette) <- conts
names(world_bank_palette) <- c("HI", "UMI", "LMI")
names(sex_palette) <- c("F", "M")
names(age_palette) <- levels(as.factor(whoimp$age_cut))
JOINTS <- 1
JP.SENS <- .1
# globale ----
wt <- whoimp
ARDs <- c("RA", "SpA", "SLE", "Gout", "SSc")
CAUSE <- c(ARDs, "CVD", "Neoplasms")
# quanti morti totali in tempo-persona
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  # SENSITIVITY ANALYSIS: remove Asian countries
  filter(continent != "Asia") %>%
  group_by(.i, year, cause, age, sex) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

wt <- wt %>% group_by(.i, year, cause) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()

wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)
if (!all(wt$w_i %in% 1000001)) {
  stop("No")
}
library(gratia)  # derivatives() convenience
peak_years <- AAPCi <- pg <- d2 <- g <- AAPC <- list()
PALETTA <- colorRampPalette(c("#845A6D", "#3E1929", "#6E75A8", "#8D91C7", "#B0DAF1"))(8)
for (i in CAUSE){
  cat(i); AAPC <- d2 <- infl <- list()
  
  pred_data <- data.frame(year = seq(min(wt$year), max(wt$year), length = 100))
  boot_preds <- matrix(NA, nrow = nrow(pred_data), ncol = BOOT)
  
  for (b in 1:BOOT){
    g <- gam(log_asr ~ s(year, bs = "tp", k = 20), data = wt[wt$.i == b & wt$cause == i, ], weights = w_log, method = "REML", select = TRUE)
    d2[[b]] <- derivatives(g, term = "s(year)", order = 2)
    
    AAPC[[b]] = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    # Store bootstrap predictions
    boot_preds[, b] <- predict(g, newdata = pred_data)
  }
  
  d2_c <- d2 %>% bind_rows()
  d2_c <- derchange(d2_c, slope_threshold = JP.SENS)
  d2_c <- d2_c %>% filter(meaningful_change == T) %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(year > REPR_YEARS[1]) %>%
    pull(year)# %>% unique()
  if (length(d2_c) > 0){
    dens <- density(d2_c)
    peak_years[[i]] <- dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
    
    #jpeg(filename = paste0("figure/breaks-", i, ".jpg"), units = "px", width = 2400, height = 1800, res = 300, quality = 100)
    print({
      plot(dens, main = paste0("Breakpoints distribution - ", i), xlab = "Year")
      abline(v = peak_years[[i]], col = "red", lty = 2)
    }
    )#; dev.off()
    
  } else {peak_years[[i]] <- NULL }
  
  trim_quantile <- function(x) {
    q01 <- quantile(x, 0.01, na.rm = TRUE)
    q99 <- quantile(x, 0.99, na.rm = TRUE)
    med <- median(x, na.rm = TRUE)
    x[x < q01] <- med
    x[x > q99] <- med
    return(x)
  }
  
  pred_data$fit <- exp(apply(boot_preds, 1, mean))  # or use median: apply(boot_preds, 1, median)
  pred_data$se.fit <- apply(boot_preds, 1, sd)
  pred_data$lower <- apply(boot_preds, 1, quantile, probs = 0.025) %>% exp()
  pred_data$upper <- apply(boot_preds, 1, quantile, probs = 0.975) %>% exp()
  
  save(pred_data, file = paste0("figure/PREDS-", i, ".Rdata"))
  # Create plot with bootstrap confidence bands
  LIM <- c(2, 6); if (i %in% c("Neoplasms", "CVD")) LIM <- c(10, 13)
  #wt[wt$cause == i, ] <- 
  prova <-  wt[wt$cause == i, ] %>% filter(asmr >= quantile(asmr, 0.01, na.rm = TRUE),
                                            asmr <= quantile(asmr, 0.99, na.rm = TRUE))
  pg[[i]] <- ggplot() +
    geom_point(data =  wt[wt$cause == i, ], 
               aes(x = year, y = asmr), alpha = 0.1) +
    geom_ribbon(data = pred_data, 
                aes(x = year, ymin = lower, ymax = upper),
                fill = PALETTA[which(CAUSE %in% i)], alpha = 0.5) +
    geom_line(data = pred_data, aes(x = year, y = fit), color = PALETTA[which(CAUSE %in% i)], linewidth = 1) +
    #geom_vline(xintercept = peak_years[[i]], color = "red", linetype = "dashed") +
    labs(title = i, x = "", y = "asSMR (x 1,000,000)") +
    theme_minimal() + 
    #ylim(LIM)+
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increased axis text
      axis.text.y = element_text(size = 12),                         # Increased axis text
      axis.title = element_text(size = 14),                          # Increased axis title
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),  # Increased title
      legend.text = element_text(size = 12),                         # Increased legend text
      legend.title = element_text(size = 14)                         # Increased legend title
    ) +
    scale_x_continuous(limits = c(REPR_YEARS[1], REPR_YEARS[2]), 
                       breaks = seq(floor(min(wt$year)), ceiling(max(wt$year)), by = 1))
  print(pg[[i]])
  # risultati finali 
  AAPC <- AAPC %>% bind_rows()
  AAPCi[[i]] <- apply(AAPC, 2, mean)
  
}
global.peaks <- peak_years
global.AAPCic <- data.frame(AAPCi %>% bind_rows(), x = names(AAPCi))

require(patchwork)
aug <- (pg[[4]] + 
          pg[[1]] + theme(axis.title.y = element_blank()) + 
          pg[[2]] + theme(axis.title.y = element_blank())) / (
            pg[[3]] + 
              pg[[5]] + theme(axis.title.y = element_blank())) / (
                pg[[6]] +  pg[[7]] + theme(axis.title.y = element_blank()))
aug <- aug + plot_layout() +
  plot_annotation(
    theme = theme(
      plot.caption = element_text(size = 14, hjust = 0.5)
    ),
    caption = "Year"
  ) &
  theme(plot.caption.position = "panel")
aug
if (QUALITY == "usa60"){
  jpeg(filename = paste0("figure/Gout-TRENDS-", QUALITY, ".jpg"), units = "px", width = 3000, height = 1800, res = 200, quality = 100)
  print(aug); dev.off()
}

## per G-CAN ----
aug <- (pg[[4]] + scale_y_continuous(breaks = c(0.5, 1:4)) ) / (
  pg[[1]] + 
    pg[[2]] + theme(axis.title.y = element_blank())) / (
      pg[[3]] + pg[[5]] + theme(axis.title.y = element_blank())) / (
  pg[[6]] +  pg[[7]] + theme(axis.title.y = element_blank()))
aug <- aug + plot_layout() +
  plot_annotation(
    theme = theme(
      plot.caption = element_text(size = 14, hjust = 0.5)
    ),
    caption = "Year"
  ) &
  theme(plot.caption.position = "panel")
aug
if (QUALITY == "usa60"){
  jpeg(filename = paste0("figure/TRENDS-", QUALITY, ".jpg"), units = "px", width = 3000, height = 3000, res = 200, quality = 100)
  print(aug); dev.off()
}

# per sesso ----
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, age, sex) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbyage, by = c("age"))
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
# comprimo l'età
wt <- wt %>% group_by(.i, year, cause, sex) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()

wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)
library(gratia)  # derivatives() convenience
peak_years <- AAPCi <- pg <- d2 <- g <- AAPC <- list()
if (!all(wt$w_i %in% c(1e6, 1000001))) {
  stop("No")
}
for (i in CAUSE){
  cat(i); AAPC <- d2 <- list()
  for (b in 1:BOOT){
    bysex.appc <- d2sex <- list()
    for ( j in c("M", "F")){
      g <- gam(log_asr ~ s(year, bs = "tp", k = 20), data = wt[wt$.i == b & wt$cause == i & wt$sex == j, ], weights = w_log, method = "REML", select = TRUE)
      summary(g)
      #gam.check(g)
      d2sex[[j]] <- derivatives(g, term = "s(year)", order = 2)
      
      # Store results
      bysex.appc[[j]] = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    }
    d2[[b]] <- cbind(sex = names(d2sex), d2sex %>% bind_rows())
    AAPC[[b]] <- cbind(sex = names(bysex.appc), bysex.appc %>% bind_rows())
  }
  # annual % change > 10% significativo per scegliere un joinpoint
  d2_c <- d2 %>% bind_rows() %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(sign != dplyr::lag(sign) & year > REPR_YEARS[1]) %>% dplyr::select(sex, year)
  peak_years[[i]] <- d2_c %>% group_by(sex) %>%
    group_map(~{
      years <- .x$year
      if(length(years) > 1) {
        dens <- density(years)
        dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
      } else {
        numeric(0)
      }
    }) %>% setNames(unique(d2_c$sex))
  # 
  AAPC <- AAPC %>% bind_rows()
  AAPCi[[i]] <- AAPC %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) %>% 
    group_by(sex) %>% summarise(
    AAPC = mean(AAPC, na.rm = TRUE),
    AAPC_lower = mean(AAPC_lo, na.rm = TRUE),
    AAPC_upper = mean(AAPC_hi, na.rm = TRUE),
    p_value = Mode(p_value)
  ) %>% mutate(x = i)
}
bysex.peaks <- peak_years
bysex.AAPCic <- AAPCi %>% bind_rows()
# per classi di età ----
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, age, sex, age_cut) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(Page %>% mutate(world2020 = world2020/2), by = c("age", "age_cut", "sex"))
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
# comprimo l'età
wt <- wt %>% group_by(.i, year, cause, age_cut) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()

wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)
library(gratia)  # derivatives() convenience
peak_years <- AAPCi <- pg <- d2 <- g <- AAPC <- list()
if (!all(wt$w_i %in% c(1e6, 1000001))) {
  stop("No")
}
for (i in CAUSE){
  cat(i); AAPC <- d2 <- list()
  for (b in 1:BOOT){
    byage_cut.appc <- d2age_cut <- list()
    for ( j in setdiff(levels(as.factor(wt$age_cut)), "0-19")){
      g <- tryCatch({
        gam(log_asr ~ s(year, bs = "tp", k = 20), data = wt[wt$.i == b & wt$cause == i & wt$age_cut == j, ], weights = w_log, method = "REML", select = TRUE)
      }, 
      error = function(e){
        g <- gam(log_asr ~ s(year, bs = "tp", k = 10), data = wt[wt$.i == b & wt$cause == i & wt$age_cut == j, ], weights = w_log, method = "REML", select = TRUE)
      })
      summary(g)
      #gam.check(g)
      d2age_cut[[j]] <- derivatives(g, term = "s(year)", order = 2)
      
      # Store results
      byage_cut.appc[[j]] = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    }
    d2[[b]] <- cbind(age_cut = names(d2age_cut), d2age_cut %>% bind_rows())
    AAPC[[b]] <- cbind(age_cut = names(byage_cut.appc), byage_cut.appc %>% bind_rows())
  }
  # annual % change > 10% significativo per scegliere un joinpoint
  d2_c <- d2 %>% bind_rows() %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(sign != dplyr::lag(sign) & year > REPR_YEARS[1]) %>% dplyr::select(age_cut, year)
  peak_years[[i]] <- d2_c %>% group_by(age_cut) %>%
    group_map(~{
      years <- .x$year
      if(length(years) > 1) {
        dens <- density(years)
        dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
      } else {
        numeric(0)
      }
    }) %>% setNames(unique(d2_c$age_cut))
  # 
  AAPC <- AAPC %>% bind_rows()
  AAPCi[[i]] <- AAPC %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) %>% 
    group_by(age_cut) %>% summarise(
    AAPC = mean(AAPC, na.rm = TRUE),
    AAPC_lower = mean(AAPC_lo, na.rm = TRUE),
    AAPC_upper = mean(AAPC_hi, na.rm = TRUE),
    p_value = Mode(p_value)
  ) %>% mutate(x = i)
}
byage_cut.peaks <- peak_years
byage_cut.AAPCic <- AAPCi %>% bind_rows()

# per continente ----

whoimp <- whoimp %>% mutate(continent = case_when(
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
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex, continent) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
# comprimo l'età
wt <- wt %>% group_by(.i, year, cause, continent) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()

wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)
wt$w_log <- ifelse(is.nan(wt$w_log), 0, wt$w_log)
if (!all(wt$w_i %in% c(1e6, 1000001))) {
  stop("No")
}

library(gratia)  # derivatives() convenience
peak_years <- AAPCi <- pg <- d2 <- g <- AAPC <- list()
us <- setdiff(levels(as.factor(wt$continent)), "Africa")

for (i in CAUSE){
  cat(i); AAPC <- d2 <- list()
  for (b in 9:BOOT){
    bycontinent.appc <- d2continent <- list()
    for ( j in us){
      #if (all(is.na(wt[wt$.i == b & wt$cause == i & wt$continent == j, ]))) next
      if (nrow(wt[wt$.i == b & wt$cause == i & wt$continent == j, ]) == 0 | j == "Asia" & i %in% c("SpA", "RA") & QUALITY %in% c("complete_cases", "g20")) {
        d2continent[[j]] <- bycontinent.appc[[j]] <- NULL
      } else {
      g <- tryCatch({
        gam(log_asr ~ s(year, bs = "tp", k = 20), data = wt[wt$.i == b & wt$cause == i & wt$continent == j, ], weights = w_log, method = "REML", select = TRUE)
      }, 
      error = function(e){
        gam(log_asr ~ s(year, bs = "tp", k = 10), data = wt[wt$.i == b & wt$cause == i & wt$continent == j, ], weights = w_log, method = "REML", select = TRUE)
      })
      summary(g)
      #gam.check(g)
      d2continent[[j]] <- derivatives(g, term = "s(year)", order = 2)
      
      # Store results
      bycontinent.appc[[j]] = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
      }
    }
    d2[[b]] <- cbind(continent = names(d2continent), d2continent %>% bind_rows())
    AAPC[[b]] <- cbind(continent = names(bycontinent.appc), bycontinent.appc %>% bind_rows())
  }
  # annual % change > 10% significativo per scegliere un joinpoint
  d2_c <- d2 %>% bind_rows() %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(sign != dplyr::lag(sign) & year > REPR_YEARS[1]) %>% dplyr::select(continent, year)
  peak_years[[i]] <- d2_c %>% group_by(continent) %>%
    group_map(~{
      years <- .x$year
      if(length(years) > 1) {
        dens <- density(years)
        dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
      } else {
        numeric(0)
      }
    }) %>% setNames(unique(d2_c$continent))
  # 
  AAPC <- AAPC %>% bind_rows()
  AAPCi[[i]] <- AAPC %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) %>% 
    group_by(continent) %>% summarise(
    AAPC = mean(AAPC, na.rm = TRUE),
    AAPC_lower = mean(AAPC_lo, na.rm = TRUE),
    AAPC_upper = mean(AAPC_hi, na.rm = TRUE),
    p_value = Mode(p_value)
  ) %>% mutate(x = i)
}
bycontinent.peaks <- peak_years
bycontinent.AAPCic <- AAPCi %>% bind_rows()

# per ricchezza ----
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  #filter(get(QUALITY) == 1) %>%
  group_by(.i, year, cause, age, sex, world_bank) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
# comprimo l'età
wt <- wt %>% group_by(.i, year, cause, world_bank) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()

wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)

library(gratia)  # derivatives() convenience
peak_years <- AAPCi <- pg <- d2 <- g <- AAPC <- list()
if (!all(wt$w_i %in% c(1e6, 1000001))) {
  stop("No")
}
for (i in CAUSE){
  cat(i); AAPC <- d2 <- list()
  for (b in 1:BOOT){
    byworld_bank.appc <- d2world_bank <- list()
    for ( j in levels(as.factor(wt$world_bank))){
      wtb <- wt[wt$.i == b & wt$cause == i & wt$world_bank == j, ] %>% mutate(w_log = ifelse(is.nan(w_log), 1, w_log))
      if (nrow(wtb) == 0 | j == "LMI" & i %in% c("RA", "SpA", "Gout") & QUALITY %in% c("cc", "g20" )) {
        d2world_bank[[j]] <- byworld_bank.appc[[j]] <- NULL
      } else {
        g <- gam(log_asr ~ s(year, bs = "tp", k = 20), data = wtb, weights = w_log, method = "REML", select = TRUE)
      }
        summary(g)
      #gam.check(g)
      d2world_bank[[j]] <- derivatives(g, term = "s(year)", order = 2)
      
      # Store results
      byworld_bank.appc[[j]] = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
    }
    d2[[b]] <- cbind(world_bank = names(d2world_bank), d2world_bank %>% bind_rows())
    AAPC[[b]] <- cbind(world_bank = names(byworld_bank.appc), byworld_bank.appc %>% bind_rows())
  }
  # annual % change > 10% significativo per scegliere un joinpoint
  d2_c <- d2 %>% bind_rows() %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(sign != dplyr::lag(sign) & year > REPR_YEARS[1]) %>% dplyr::select(world_bank, year)
  peak_years[[i]] <- d2_c %>% group_by(world_bank) %>%
    group_map(~{
      years <- .x$year
      if(length(years) > 1) {
        dens <- density(years)
        dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
      } else {
        numeric(0)
      }
    }) %>% setNames(unique(d2_c$world_bank))
  # 
  AAPC <- AAPC %>% bind_rows()
  AAPCi[[i]] <- AAPC %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) %>% 
    group_by(world_bank) %>% summarise(
    AAPC = mean(AAPC, na.rm = TRUE),
    AAPC_lower = mean(AAPC_lo, na.rm = TRUE),
    AAPC_upper = mean(AAPC_hi, na.rm = TRUE),
    p_value = Mode(p_value)
  ) %>% mutate(x = i)
}
byworld_bank.peaks <- peak_years
byworld_bank.AAPCic <- AAPCi %>% bind_rows()

# per nazione ----
if (QUALITY %in% "usa60"){
  wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
    #filter(get(QUALITY) == 1) %>%
    group_by(.i, year, cause, age, sex, country_name) %>% summarise(
      mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
    ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
  #unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
  wt$smr <- wt$mortality / wt$population
  wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
  wt$asmr <- wt$mortality * wt$w_i / wt$population
  wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
  # comprimo l'età
  wt <- wt %>% group_by(.i, year, cause, country_name) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
    w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
  ) %>% ungroup()
  wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)
  if (!all(wt$w_i %in% c(1e6, 1000001))) {
    stop("No")
  }
  library(gratia)  # derivatives() convenience
  peak_years <- AAPCi <- pg <- d2 <- d2_c <- g <- AAPC <- AAPC_c <- list()
  us <- levels(as.factor(wt$country_name))
  
  for (i in CAUSE){
    cat(i); AAPC <- d2 <- d2_c <- AAPC_c <- list()
    for ( j in levels(as.factor(as.character(wt$country_name)))){
      bycountry_name.appc <- d2country_name <- list()
      for (b in 1:BOOT){
        wtb <- wt[wt$.i == b & wt$cause == i & wt$country_name == j, ] %>% mutate(w_log = ifelse(is.nan(w_log), 1, w_log))
        
        if (all(wtb$mortality == 0) | nrow(wtb) == 0 | i %in% c("RA", "SpA") & j %in% c("Malta", "Paraguay", "Iceland", "Romania") | i == "Gout" & j %in% c("Czechia", "Malta")) {
          d2country_name[[j]] <- bycountry_name.appc[[j]] <- NULL
        } else { 
          g <- tryCatch({
            gam(log_asr ~ s(year, bs = "tp", k = 20), data = wtb, weights = w_log, method = "REML", select = TRUE)
          }, 
          error = function(e){
            gam(log_asr ~ s(year, bs = "tp", k = 3), data = wtb, weights = w_log, method = "REML", select = TRUE)
          })
          #summary(g)
          #gam.check(g)
          d2country_name <- derivatives(g, term = "s(year)", order = 2)
          
          # Store results
          bycountry_name.appc = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
          
          d2[[b]] <- cbind(country_name = j, d2country_name)
          AAPC[[b]] <- cbind(country_name = j, bycountry_name.appc)
        }
      }
      
      # annual % change > 10% significativo per scegliere un joinpoint
      d2_c[[j]] <- d2 %>% bind_rows() %>%
        mutate(sign = sign(.derivative), year = round(year)) %>%
        filter(sign != dplyr::lag(sign) & year > REPR_YEARS[1]) %>% dplyr::select(country_name, year)
      
      AAPC_c[[j]] <- AAPC %>% bind_rows()
      
    }
    peak_years[[i]] <- d2_c %>% bind_rows() %>% group_by(country_name) %>%
      group_map(~{
        years <- .x$year
        if(length(years) > 1) {
          dens <- density(years)
          dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
        } else {
          numeric(0)
        }
      }) %>% setNames(unique(d2_c$country_name))
    #uy <- bind_rows(AAPC_c) %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) 
    AAPCi[[i]] <- bind_rows(AAPC_c) %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) %>% 
      group_by(country_name) %>% summarise(
        AAPC = mean(AAPC, na.rm = TRUE),
        AAPC_lower = mean(AAPC_lo, na.rm = TRUE),
        AAPC_upper = mean(AAPC_hi, na.rm = TRUE),
        p_value = Mode(p_value)
      ) %>% mutate(x = i)
  }
  bycountry_name.peaks <- peak_years
  bycountry_name.AAPCic <- AAPCi %>% bind_rows()
  
}
# other world diision ----
west.EU <- c("Belgium", "Denmark", "France", "Germany", #"Greece", 
             "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "United Kingdom",
             "Austria", "Sweden", "Malta", "Finland", "Iceland", "Norway", "Switzerland")
east.EU <- c(setdiff(EU, west.EU), "Serbia", "Republic of Moldova")
global.south <- c("Argentina", "Australia", "Brazil", "Mexico", "South Africa", "Chile", "Venezuela", "Nicaragua", "Guatemala",
                  "Ecuador", "Colombia", "Cuba", "Guyana", "Jamaica", "Kyrgyzstan", "Panama", "Paraguay", "Philippines", "Suriname", "Thailand",
                  "Belize", "Uzbekistan", "Uruguay", "Georgia", "New Zealand", "Republic of Moldova", "Armenia", "Costa Rica")
global.north <- c("Canada", "Japan", EU, "Iceland", "Norway", "United Kingdom", "United States of America", "Turkey", "Bahamas", "Israel", "Switzerland", "Serbia")

## east-west europe ----
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2] & country_name %in% c(east.EU, west.EU)) %>% 
  mutate(westeast = ifelse(country_name %in% east.EU, "Eastern Europe", "Western Europe")) %>%
  group_by(.i, year, cause, age, sex, westeast) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
# comprimo l'età
wt <- wt %>% group_by(.i, year, cause, westeast) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()

wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)
if (!all(wt$w_i %in% c(1e6, 1000001))) {
  stop("No")
}
library(gratia)  # derivatives() convenience
peak_years <- AAPCi <- pg <- d2 <- g <- AAPC <- list()
us <- c("Eastern Europe", "Western Europe")

for (i in CAUSE){
  cat(i); AAPC <- d2 <- list()
  for (b in 1:BOOT){
    bywesteast.appc <- d2westeast <- list()
    for ( j in us){
      if (nrow(wt[wt$.i == b & wt$cause == i & wt$westeast == j, ]) == 0 | j == "Asia" & i %in% c("SpA", "RA") & QUALITY %in% c("cc", "g20" )) {
        d2westeast[[j]] <- bywesteast.appc[[j]] <- NULL
      } else {
        g <- tryCatch({
          gam(log_asr ~ s(year, bs = "tp", k = 20), data = wt[wt$.i == b & wt$cause == i & wt$westeast == j, ], weights = w_log, method = "REML", select = TRUE)
        }, 
        error = function(e){
          gam(log_asr ~ s(year, bs = "tp", k = 10), data = wt[wt$.i == b & wt$cause == i & wt$westeast == j, ], weights = w_log, method = "REML", select = TRUE)
        })
        summary(g)
        #gam.check(g)
        d2westeast[[j]] <- derivatives(g, term = "s(year)", order = 2)
        
        # Store results
        bywesteast.appc[[j]] = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
      }
    }
    d2[[b]] <- cbind(westeast = names(d2westeast), d2westeast %>% bind_rows())
    AAPC[[b]] <- cbind(westeast = names(bywesteast.appc), bywesteast.appc %>% bind_rows())
  }
  # annual % change > 10% significativo per scegliere un joinpoint
  d2_c <- d2 %>% bind_rows() %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(sign != dplyr::lag(sign) & year > REPR_YEARS[1]) %>% dplyr::select(westeast, year)
  peak_years[[i]] <- d2_c %>% group_by(westeast) %>%
    group_map(~{
      years <- .x$year
      if(length(years) > 1) {
        dens <- density(years)
        dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
      } else {
        numeric(0)
      }
    }) %>% setNames(unique(d2_c$westeast))
  # 
  AAPC <- AAPC %>% bind_rows()
  AAPCi[[i]] <- AAPC %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) %>% 
    group_by(westeast) %>% summarise(
    AAPC = mean(AAPC, na.rm = TRUE),
    AAPC_lower = mean(AAPC_lo, na.rm = TRUE),
    AAPC_upper = mean(AAPC_hi, na.rm = TRUE),
    p_value = Mode(p_value)
  ) %>% mutate(x = i)
}
bywesteast.peaks <- peak_years
bywesteast.AAPCic <- AAPCi %>% bind_rows()

## north south ----
wt <- whoimp %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  mutate(northsouth = ifelse(country_name %in% global.north, "Global North", "Global South")) %>%
  group_by(.i, year, cause, age, sex, northsouth) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
#unique(whoimp %>% filter(get(QUALITY) == 1) %>% pull(country_name))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2
# comprimo l'età
wt <- wt %>% group_by(.i, year, cause, northsouth) %>% summarise(
  mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population), v_asmr = sum(v_asmr, na.rm = T),
  w_i = sum(w_i, na.rm = T), asmr = sum(asmr, na.rm = T), stderr_asmr = sqrt(sum(v_asmr, na.rm = T)), smr = mortality / population * 1e6
) %>% ungroup()
wt <- wt %>% mutate(log_asr = log(pmax(asmr, .Machine$double.eps)), w_log = (asmr^2) / v_asmr)
if (!all(wt$w_i %in% c(1e6, 1000001))) {
  stop("No")
}
library(gratia)  # derivatives() convenience
peak_years <- AAPCi <- pg <- d2 <- g <- AAPC <- list()
us <- c("Global North", "Global South")

for (i in CAUSE){
  cat(i); AAPC <- d2 <- list()
  for (b in 1:BOOT){
    bynorthsouth.appc <- d2northsouth <- list()
    for ( j in us){
      if (nrow(wt[wt$.i == b & wt$cause == i & wt$northsouth == j, ]) == 0 | j == "Asia" & i %in% c("SpA", "RA") & QUALITY %in% c("cc", "g20" )) {
        d2northsouth[[j]] <- bynorthsouth.appc[[j]] <- NULL
      } else {
        g <- tryCatch({
          gam(log_asr ~ s(year, bs = "tp", k = 20), data = wt[wt$.i == b & wt$cause == i & wt$northsouth == j, ], weights = w_log, method = "REML", select = TRUE)
        }, 
        error = function(e){
          gam(log_asr ~ s(year, bs = "tp", k = 10), data = wt[wt$.i == b & wt$cause == i & wt$northsouth == j, ], weights = w_log, method = "REML", select = TRUE)
        })
        summary(g)
        #gam.check(g)
        d2northsouth[[j]] <- derivatives(g, term = "s(year)", order = 2)
        
        # Store results
        bynorthsouth.appc[[j]] = aapc_gam(g, REPR_YEARS[1], REPR_YEARS[2])
      }
    }
    d2[[b]] <- cbind(northsouth = names(d2northsouth), d2northsouth %>% bind_rows())
    AAPC[[b]] <- cbind(northsouth = names(bynorthsouth.appc), bynorthsouth.appc %>% bind_rows())
  }
  # annual % change > 10% significativo per scegliere un joinpoint
  d2_c <- d2 %>% bind_rows() %>%
    mutate(sign = sign(.derivative), year = round(year)) %>%
    filter(sign != dplyr::lag(sign) & year > REPR_YEARS[1]) %>% dplyr::select(northsouth, year)
  peak_years[[i]] <- d2_c %>% group_by(northsouth) %>%
    group_map(~{
      years <- .x$year
      if(length(years) > 1) {
        dens <- density(years)
        dens$x[which(diff(sign(diff(dens$y))) == -2)] %>% round()
      } else {
        numeric(0)
      }
    }) %>% setNames(unique(d2_c$northsouth))
  # 
  AAPC <- AAPC %>% bind_rows()
  AAPCi[[i]] <- AAPC %>% mutate(p_value = as.numeric(as.character(cut(p_value, right = F, breaks = c(-1, 0.001, 0.01, 0.05, 1), labels = c(0.001, 0.01, 0.05, .5))))) %>% 
    group_by(northsouth) %>% summarise(
    AAPC = mean(AAPC, na.rm = TRUE),
    AAPC_lower = mean(AAPC_lo, na.rm = TRUE),
    AAPC_upper = mean(AAPC_hi, na.rm = TRUE),
    p_value = Mode(p_value)
  ) %>% mutate(x = i)
}
bynorthsouth.peaks <- peak_years
bynorthsouth.AAPCic <- AAPCi %>% bind_rows()
# mettiamoli in una forma migliore ----
fxic <- function(e, lo, up, p){
  formatted <- sprintf("%+.2f%% [%+.2f%%, %+.2f%%]", 
                       e, lo, up)
  
  asterisks <- ifelse(p < 0.001, "***",
                      ifelse(p < 0.01, "**",
                             ifelse(p < 0.05, "*", "")))
  same_sign <- (lo > 0 & up > 0) | (lo < 0 & up < 0)
  asterisks <- ifelse(!same_sign & asterisks != "", "", asterisks)
  asterisks <- ifelse(same_sign & asterisks == "", "*", asterisks)
  # Add asterisks to formatted strings
  formatted <- paste0(formatted, asterisks)
  return(formatted)
}

resu <- bind_rows(global.AAPCic %>% rename(AAPC_lower = AAPC_lo, AAPC_upper = AAPC_hi) %>% dplyr::select(-x1, -x2) %>% mutate( strat= "Global"), 
                  bysex.AAPCic %>% rename(strat = sex), byage_cut.AAPCic %>% rename(strat = age_cut), 
                  bycontinent.AAPCic %>% rename(strat = continent), bywesteast.AAPCic %>% rename(strat = westeast), bynorthsouth.AAPCic %>% rename(strat = northsouth), 
                  byworld_bank.AAPCic %>% rename(strat = world_bank)
)
if (QUALITY == "usa60") resu <- bind_rows(resu, bycountry_name.AAPCic %>% rename(strat = country_name))
results <- resu %>% mutate(APC = fxic(AAPC, AAPC_lower, AAPC_upper, p_value)) %>% dplyr::select(x, strat, APC) %>%
  pivot_wider(
  names_from = x,
  values_from = APC
) %>% as.data.frame()

