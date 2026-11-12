COL.PUNTI <- "#B0DAF1"
COL.LINEE <- "#6E75A8"
col.overall <- "#845A6D"
c("#845A6D", "#3E1929", "#6E75A8", "#8D91C7", "#B0DAF1")

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

process_data <- function(data, disease) {
  require(purrr)
  df <- data %>%
    dplyr::select(Stratification, !!disease) %>%
    mutate(
      results = map(get(disease), ~{
        vals <- parse_ci(.x)
        data.frame(
          estimate = vals["estimate"],
          lower = vals["lower"],
          upper = vals["upper"]
        )
      })
    ) %>%
    unnest(results)
  
  return(df)
}

# per ASMR ----
require(readxl)
file <- "res\\2000-2023-EPI-WHO-usa60-2026-06-03.xlsx"
ave <- readxl::read_excel(file, sheet = "figure-APC", trim_ws = F)
ave$Stratification[ave$Stratification %in% "Wealth category"] <- "Income category"
ave <- ave[!ave$Stratification %in% c("   Western Europe", "   Eastern Europe"), ]
ave2 <- ave[1:(which(ave$Stratification=="Country")-1), ]
ave <- ave[c(1:(which(ave$Stratification=="Age class")-1), which(ave$Stratification=="Income category"):(which(ave$Stratification=="Country")-1) ), ]
ave <- ave %>% mutate(Stratification = as.factor(Stratification)) 

create_full_forest_plot <- function(df, disease_name, GLOBAL) {
  df <- df %>% 
    mutate(significant = (lower > 0 & upper > 0) | (lower < 0 & upper < 0))
  df$face <- c("bold", "plain", "plain", "bold", "plain", "plain", "bold", "plain", "plain", "plain", "plain", "bold", rep("plain", 5), "bold", "plain", "plain", "plain")
  df <- df %>% 
    mutate(Stratification = factor(Stratification, levels = rev(unique(Stratification))),
           global = if_else(Stratification == "Global", 1, 0, missing = 0))
  
  ggplot(df, aes(x = estimate, y = Stratification)) +
    geom_point(aes(color = factor(global)), size = 3) +  # Color by global status
    geom_errorbarh(aes(xmin = lower, xmax = upper, color = factor(global)), 
                   height = 0.2, alpha = 0.7, size = 1.2) +  # Color by global status
    geom_vline(xintercept = GLOBAL, linetype = "dashed", color = col.overall, size = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", size = .7) +
    labs(
      title = disease_name,
      x = "APC (%)",
      y = ""
    ) +
    scale_color_manual(values = c("0" = COL.LINEE, "1" = col.overall), 
                       guide = "none") +  # Map colors and hide legend
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.y = element_text(size = 12, face = rev(df$face)),
      axis.text.x = element_text(size = 11),
      legend.position = "none"  # Remove legend since we're using guide = "none"
    )
}

create_forest_plot <- function(df, disease_name, GLOBAL) {
  df <- df %>% 
    mutate(significant = (lower > 0 & upper > 0) | (lower < 0 & upper < 0))
  df$face <- c("bold", "plain", "plain", "bold", "plain", "plain", "bold", "plain", "plain", "plain")
  df <- df %>% 
    mutate(Stratification = factor(Stratification, levels = rev(unique(Stratification))),
           global = if_else(Stratification == "Global", 1, 0, missing = 0))
  
  ggplot(df, aes(x = estimate, y = Stratification)) +
    geom_point(aes(color = factor(global)), size = 3) +  # Color by global status
    geom_errorbarh(aes(xmin = lower, xmax = upper, color = factor(global)), 
                   height = 0.2, alpha = 0.7, size = 1.2) +  # Color by global status
    geom_vline(xintercept = GLOBAL, linetype = "dashed", color = col.overall, size = 1.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", size = .7) +
    labs(
      title = disease_name,
      x = "APC (%)",
      y = ""
    ) +
    scale_color_manual(values = c("0" = COL.LINEE, "1" = col.overall), 
                       guide = "none") +  # Map colors and hide legend
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.y = element_text(size = 12, face = rev(df$face)),
      axis.text.x = element_text(size = 11),
      legend.position = "none"  # Remove legend since we're using guide = "none"
    )
}

# Create enhanced plots
diseases <- c("RA", "SpA", "SLE", "Gout", "SSc", "CVD", "Neoplasms")
for (disease in diseases) {
  df <- process_data(ave, disease)
  df2 <- process_data(ave2, disease)
  glob <- df$estimate[which(df$Stratification %in% "Global")]
  g <- create_forest_plot(df, disease, GLOBAL = glob)
  f <- create_full_forest_plot(df2, disease, GLOBAL = glob)
  png(filename = paste0("figure/FOREST-APC-", disease, ".jpg"), units = "px", width = 1200, height = 1500, res = 180)
  print(f); dev.off()
  print(g)
  assign(paste0("g.", disease), g)
  assign(paste0("f.", disease), f)
}
library(patchwork)

noy <- theme(axis.text.y = element_blank())

row1 <- g.Gout + (g.RA + noy) + (g.SpA + noy) +
  plot_layout(nrow = 1)
row2 <- g.SLE + (g.SSc + noy) + 
  plot_layout(nrow = 1)
row3 <- g.CVD + (g.Neoplasms + noy) + 
  plot_layout(nrow = 1)

fing <- row1 / row2 +#/ row3 +
  plot_annotation(
    theme = theme(plot.caption = element_text(size = 14, hjust = .5))
  ) &
  theme(plot.caption.position = "panel")
fing
png(filename = paste0("figure/FOREST-APC-", QUALITY, ".jpg"), units = "px", width = 3000, height = 2500, res = 250)
print(fing); dev.off()
  

## GEO GRAPHICAL DISTRIBUTION ----
ave <- readxl::read_excel(file, sheet = "figure-APC", trim_ws = F)
ave <- ave %>% mutate(Stratification = as.factor(Stratification)) 
library(sf); library(rnaturalearth)
ave <- ave %>% filter(
  Stratification %in% sc
) %>% mutate(Stratification = as.factor(Stratification))
MINAPC <- 5
# Create enhanced plots
diseases <- c("Gout", "RA", "SpA", "SLE", "SSc", "CVD", "Neoplasms")
mid <- colorRampPalette(c("navy", "red4"))(3)[2]          # "#450040"
mix <- function(col, w) rgb(t(col2rgb(col) * (1 - w) + 255 * w), maxColorValue = 255)

for (disease in diseases) {
  df <- process_data(ave, disease)
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    mutate(country_name = sovereignt)
  world$country_name[world$country_name %in% "The Bahamas"] <- "Bahamas"
  world$country_name[world$country_name %in% "Moldova"] <- "Republic of Moldova"
  world$country_name[world$country_name %in% "Republic of Serbia"] <- "Serbia"
  world_data <- world %>% left_join(df, by = join_by(country_name == Stratification))
  
  g2 <- ggplot(world_data) +
    geom_sf(aes(fill = pmin(pmax(estimate, -MINAPC), MINAPC)), color = "grey20", size = 0.1) +
    scale_fill_gradient2(
      low = "navy", high = "red4", mid = mix(mid, 0.7), midpoint = 0,
      name = "Average APC (%)", na.value = "grey90",
      limits = c(-MINAPC, MINAPC), breaks = c(-MINAPC, 0, MINAPC),
      labels = c(paste0("\u2212", MINAPC, "%"), "0%", paste0("+", MINAPC, "%"))
    ) +
    coord_sf(ylim = c(-57, 85), expand = FALSE) +      # drops Antarctica, no extra padding
    labs(title = disease) +
    theme_void() +                                      # no axes or grid; use theme_minimal() if you want them
    theme(
      legend.position   = "bottom",
      legend.key.width  = unit(1.8, "cm"),
      legend.key.height = unit(0.35, "cm"),
      legend.margin     = margin(0, 0, 0, 0),
      legend.box.margin = margin(-4, 0, 0, 0),
      plot.title        = element_text(face = "bold", hjust = 0.5),
      plot.margin       = margin(2, 2, 2, 2)
    )
  
  assign(paste0("map.", disease), g2)
  
  combo <- g2 + get(paste0("g.", disease)) + plot_layout(widths = c(1.7, 1))
  ggsave(paste0("figure/f-APC-", disease, ".png"), combo,
         width = 16, height = 5.5, dpi = 180)           # 2880 x 990 px
}

require(patchwork)
fing <- (map.Gout + map.RA + theme(axis.text.y = element_blank()) + map.SpA + theme(axis.text.y = element_blank())) / (
  map.SLE + map.SSc + theme(axis.text.y = element_blank()) ) +
  plot_layout(guides = 'collect') &  # Collect all legends
  theme(legend.position = 'bottom')
fing
png(filename = paste0("figure/MAP-APC-", QUALITY, ".jpg"), units = "px", width = 4000, height = 2000, res = 180)
print(fing); dev.off()

# Lancet revision ----
PALETTA <- colorRampPalette(c("#845A6D", "#3E1929", "#6E75A8", "#8D91C7", "#B0DAF1"))(8)
names(PALETTA) <- c(CAUSE[-8], "IRDs")

wt <- whoimp %>% mutate(wealth = ifelse(world_bank %in% "HI", "HI", "MI")) %>%
  group_by(.i, year, cause, age, sex, wealth) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
table(wt$cause)
wtird <- whoimp %>% mutate(wealth = ifelse(world_bank %in% "HI", "HI", "MI"), cause = ifelse(cause %in% c("RA", "SpA", "Gout", "SLE", "SSc"), "IRD", as.character(cause))) %>%
  group_by(.i, year, cause, age, sex, wealth) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
table(wtird$cause)
wt <- bind_rows(wt, wtird) %>% mutate(cause = factor(cause, levels = c("IRD", "Gout", "RA", "SpA", "SLE", "SSc", "CVD", "Neoplasms")))

wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>% mutate(everything = 1) %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2]) %>% 
  group_by(.i, year, cause, wealth) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup()

hi <- WTS %>% filter(wealth == "HI") %>% select(.i, year, cause, asmr) %>% rename(hi_asmr = asmr)
iWTS <- WTS %>% filter(wealth != "HI") %>% left_join(hi) %>% mutate(gap = asmr - hi_asmr)
table(iWTS$cause)
library(tidyverse)
gap_ci <- iWTS |> filter(cause != "IRD") |>
  filter(wealth %in% c("MI")) |>   # HI is the reference
  group_by(cause, year, wealth) |>
  summarise(
    est = median(gap),
    lo  = quantile(gap, 0.025),
    hi  = quantile(gap, 0.975),
    .groups = "drop"
  ) |>
  mutate(world_bank = factor(wealth, levels = c("UMI", "LMI")))

fig4new <- ggplot(iWTS, aes(year, gap, colour = cause, fill = cause)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 0.5, alpha = .2) +
  geom_smooth() +
  facet_wrap(~ cause, scales = "free_y", nrow = 2, axes = "all_x") +
  scale_colour_manual(values = PALETTA, name = "Wealth category") +
  scale_fill_manual(values = PALETTA, name = "Wealth category") +
  scale_x_continuous(breaks = seq(2000, 2023, 2)) +
  labs(x = "Year", y = "asSMR difference UMI/LMI vs HI (per 1,000,000)") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1, "lines")
  )

library(gtable); library(grid)

# 1) spazio per le frecce (non altera i dati/scale)
fig4new2 <- fig4new +
  theme(panel.spacing.x = unit(2.5, "lines"),
        plot.margin     = margin(5.5, 14, 5.5, 5.5, "mm"))

# 2) posizione dello 0 (in npc) per ogni panel, dai range y reali
b   <- ggplot_build(fig4new2)
lay <- b$layout$layout
lay <- lay[order(lay$ROW, lay$COL), ]
z0  <- sapply(lay$PANEL, function(i) {
  r <- b$layout$panel_params[[as.integer(as.character(i))]]$y.range
  (0 - r[1]) / diff(r)
})

# 3) aggiunta frecce nel gtable
gt <- ggplot_gtable(b)
pn <- gt$layout[grepl("^panel", gt$layout$name), ]
pn <- pn[order(pn$t, pn$l), ]

arrow_grob <- function(z, dir, col)
  segmentsGrob(
    x0 = unit(1, "npc") + unit(3, "mm"), x1 = unit(1, "npc") + unit(3, "mm"),
    y0 = unit(z, "npc"),                 y1 = unit(z, "npc") + unit(dir * 14, "mm"),
    arrow = arrow(length = unit(3.5, "mm"), angle = 25, type = "closed"),
    gp = gpar(col = col, fill = col, lwd = 3)
  )

for (k in seq_len(nrow(pn))) {
  for (a in list(list(1, "#D62728"), list(-1, "#1F77B4"))) {
    gt <- gtable_add_grob(gt, arrow_grob(z0[k], a[[1]], a[[2]]),
                          t = pn$t[k], l = pn$l[k], b = pn$b[k], r = pn$r[k],
                          z = Inf, clip = "off", name = paste0("arrow-", k, "-", a[[1]]))
  }
}

# 4) salvataggio (stessi parametri)
jpeg("figure/Fig4new.jpg", units = "px", width = 3000, height = 1800, res = 200, quality = 100)
grid.newpage(); grid.draw(gt); dev.off()

# Asia joinooint ----
wt <- whoimp %>% filter(continent %in% "Asia") %>% 
  group_by(.i, country_name, year, cause, age, sex) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population)
  ) %>% ungroup() %>% left_join(STD_POPbysex, by = c("age", "sex"))
wt$smr <- wt$mortality / wt$population
wt$w_i <- wt$world2020 # peso relativo della fascia di popolazione
wt$asmr <- wt$mortality * wt$w_i / wt$population
wt$v_asmr <- wt$mortality * (wt$w_i / wt$population)^2

WTS <- wt %>% filter(year >= REPR_YEARS[1] & year <= REPR_YEARS[2] & cause == "Gout") %>% 
  group_by(.i, year, cause, country_name) %>% summarise(
    mortality = sum(na.rm = T, mortality), population = sum(na.rm = T, population),
    w_i = sum(w_i, na.rm = T), v_asmr = sum(v_asmr, na.rm = T), asmr = sum(asmr, na.rm = T), 
  ) %>% ungroup()

library(segmented)

series <- WTS %>%
  arrange(.i, cause, country_name, year) %>%
  group_split(.i, cause, country_name)

res <- vector("list", length(series))

# 2. Loop over series
for (k in seq_along(series)) {
  d <- series[[k]]
  d$y  <- log(d$asmr)
  d$se <- sqrt(d$v_asmr) / d$asmr          # delta method: SE of log(ASMR)
  d$w  <- 1 / d$se^2
  
  fit0 <- lm(y ~ year, data = d, weights = w)
  dav  <- tryCatch(davies.test(fit0, ~year, k = 10), error = function(e) NULL)
  fit1 <- tryCatch(segmented(fit0, seg.Z = ~year, npsi = 1), error = function(e) NULL)
  
  jp <- jp_lo <- jp_hi <- apc1 <- apc2 <- NA_real_
  bic1 <- NA_real_
  
  if (!is.null(fit1) && !is.null(fit1$psi)) {
    bic1 <- BIC(fit1)
    jp   <- fit1$psi[1, "Est."]
    ci   <- tryCatch(confint(fit1), error = function(e) NULL)
    if (!is.null(ci)) { jp_lo <- ci[1, 2]; jp_hi <- ci[1, 3] }
    sl   <- tryCatch(slope(fit1, APC = TRUE)$year, error = function(e) NULL)
    if (!is.null(sl)) { apc1 <- sl[1, 1]; apc2 <- sl[2, 1] }
  }
  
  davies_p <- if (!is.null(dav)) dav$p.value else NA_real_
  bic0     <- BIC(fit0)
  
  res[[k]] <- tibble(
    .i = d$.i[1], cause = d$cause[1], country_name = d$country_name[1],
    n_years  = nrow(d),
    APC_0jp  = 100 * (exp(coef(fit0)[["year"]]) - 1),
    davies_p = davies_p,
    BIC_0jp  = bic0, BIC_1jp = bic1,
    joinpoint = jp, joinpoint_lo = jp_lo, joinpoint_hi = jp_hi,
    APC_1 = apc1, APC_2 = apc2,
    has_joinpoint = !is.na(davies_p) & davies_p < 0.05 & !is.na(bic1) & bic1 < bic0
  )
  print(k)
}

jp_res <- bind_rows(res)

# 3. Summarise across boot samples
jp_summary <- jp_res %>%
  group_by(cause, country_name) %>%
  summarise(
    n_boot        = n(),
    share_jp      = mean(has_joinpoint, na.rm = TRUE),   # share of samples with a joinpoint
    jp_median     = median(joinpoint[has_joinpoint], na.rm = TRUE),
    jp_q025       = quantile(joinpoint[has_joinpoint], 0.025, na.rm = TRUE),
    jp_q975       = quantile(joinpoint[has_joinpoint], 0.975, na.rm = TRUE),
    APC_0jp_med   = median(APC_0jp, na.rm = TRUE),
    APC_1_med     = median(APC_1[has_joinpoint], na.rm = TRUE),
    APC_2_med     = median(APC_2[has_joinpoint], na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(country_name, cause)
jp_summary
fmt_med_iqr <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("-")
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  sprintf(paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
          q[2], q[1], q[3])
}

jp_table <- jp_res %>%
  group_by(cause, country_name) %>%
  summarise(
    share_jp = sprintf("%.3f", mean(has_joinpoint, na.rm = TRUE)),
    jp_year  = fmt_med_iqr(joinpoint[has_joinpoint %in% TRUE]),
    APC_1    = fmt_med_iqr(APC_1[has_joinpoint %in% TRUE]),
    APC_2    = fmt_med_iqr(APC_2[has_joinpoint %in% TRUE]),
    .groups  = "drop"
  ) %>%
  arrange(cause, country_name) %>%
  rename(
    Cause = cause, Country = country_name,
    `Share with joinpoint` = share_jp,
    `Joinpoint year, median [IQR]` = jp_year,
    `APC 1 (%), median [IQR]` = APC_1,
    `APC 2 (%), median [IQR]` = APC_2
  )
