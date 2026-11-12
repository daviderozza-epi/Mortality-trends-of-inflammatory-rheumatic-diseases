RFs <- c("neuro_disorders", "mental", "T2D_CKD", "cvd", "resp_disorders", 
         "cancers", "digestive", "communicable", "inj")
G20 <- c("Argentina", "Australia", "Brazil", "Canada", "France", "Germany", "Italy", "Japan", "Mexico", "South Africa", "Turkey", "United Kingdom", "United States of America")
BANNED_COUNTRIES <- c("Aruba", "Cayman Islands", "Cabo Verde", "Bulgaria", "Virgin Islands", "Bolivia", "Botswana", "Bermuda", "Bahrain", "Belarus", "Russia", "San Marino", 
                      "Seychelles", "Turkmenistan", "Ukraine", "Greece", "Iran", "Bosnia and Herzegovina", "Jordan", "Mongolia", "Mauritius", "Tajikistan", "Singapore",
                      "Antigua and Barbuda", "Azerbaijan", "Belarus", "Brunei Darussalam", "Cyprus", "Dominica", "Grenada", "Kazakhstan", "Kuwait", "Saint Lucia", 
                      "Solomon Islands", "Sri Lanka", "Syria", "United Arab Emirates", "Virgin Islands (USA)", "Turks and Caicos Islands")


# data quality
dq$country_name[dq$country_name %in% "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
dq$country_name[dq$country_name %in% "Iran (Islamic Republic of)"] <- "Iran"
dq$country_name[dq$country_name %in% "Republic of Korea"] <- "South Korea"
dq$country_name[dq$country_name %in% "Russian Federation"] <- "Russia"
dq$country_name[dq$country_name %in% "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
dq$country_name[dq$country_name %in% "T?rkiye"] <- "Turkey"

# aggiungo giudizio complessivo GHE
# Create the data.frame
ghe <- data.frame(
  country_name = c("Albania", "Andorra", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
                   "Barbados", "Belarus", "Belgium", "Belize", "Bolivia", "Bosnia and Herzegovina", "Brazil", "Brunei Darussalam", "Bulgaria", "Cabo Verde", 
                   "Canada", "Chile", "Colombia", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", "Dominica", 
                   "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Estonia", "Fiji", "Finland", "France", "Georgia", 
                   "Germany", "Greece", "Grenada", "Guatemala", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "Iran", 
                   "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kiribati", "Kuwait", 
                   "Kyrgyzstan", "Latvia", "Lebanon", "Libya", "Lithuania", "Luxembourg", "Malaysia", "Maldives", "Malta", "Mauritius", 
                   "Mexico", "Mongolia", "Montenegro", "Morocco", "Netherlands", "New Zealand", "Nicaragua", "Norway", "Oman", "Panama", 
                   "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar", "South Korea", "Republic of Moldova", "Romania", 
                   "Russia", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "San Marino", "Saudi Arabia", "Serbia", "Seychelles", "Singapore", "Slovakia", 
                   "Slovenia", "Solomon Islands", "South Africa", "Spain", "Sri Lanka", "Suriname", "Sweden", "Switzerland", "Syria", "Tajikistan", 
                   "Thailand", "Republic of North Macedonia", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Ukraine", "United Arab Emirates", "United Kingdom", "United States of America", 
                   "Uruguay", "Uzbekistan", "Venezuela"),
  quality = as.character(c("low", "high", "high", "medium", "high", "high", "high", "very low", "medium", "low", 
                           "high", "high", "high", "high", "very low", "low", "high", "medium", "medium", "low", 
                           "high", "high", "high", "high", "high", "high", "medium", "high", "high", "medium", 
                           "low", "low", "low", "medium", "high", "low", "high", "high", "low", "high", 
                           "medium", "high", "medium", "medium", "very low", "very low", "high", "high", "medium", "low", 
                           "high", "high", "high", "high", "high", "medium", "high", "very low", "high", "high", 
                           "high", "very low", "low", "high", "high", "medium", "low", "high", "high", "high", 
                           "low", "low", "very low", "high", "high", "high", "high", "very low", "high", "medium", 
                           "low", "high", "medium", "high", "high", "very low", "high", "high", "high", "medium", 
                           "medium", "high", "high", "high", "very low", "high", "medium", "high", "high", "high", 
                           "very low", "medium", "high", "medium", "medium", "high", "high", "low", "low", "low", 
                           "medium", "high", "low", "medium", "medium", "medium", "very low", "high", "high", "medium", 
                           "medium", "high")
))

# costruire fasce ricchezza
countries$country_name[str_detect(countries$country_name, "Democratic People")] <- "North Korea" 
low_income <- c(
  "Afghanistan", "Benin", "Burkina Faso", "Burundi", "Central African Republic", 
  "Chad", "North Korea", "Democratic Republic of the Congo", 
  "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Haiti", "Liberia", 
  "Madagascar", "Malawi", "Mali", "Mozambique", "Nepal", "Niger", "Rwanda", 
  "Sierra Leone", "Somalia", "South Sudan", "Togo", "Uganda", "United Republic of Tanzania"
)
setdiff(low_income, countries$country_name)

countries$country_name[str_detect(countries$country_name, "Micronesia")] <- "Micronesia"
countries$country_name[str_detect(countries$country_name, "Lao Peopl")] <- "Laos"
countries$country_name[str_detect(countries$country_name, "Bolivia")] <- "Bolivia"
countries$country_name[str_detect(countries$country_name, "Ivoire")] <- "Ivory Coast"
countries$country_name[str_detect(countries$country_name, "Occupied Palestinian Territory")] <- "Palestine"

lower_middle_income <- c(
  "Angola", "Armenia", "Bangladesh", "Bhutan", "Bolivia", 
  "Cambodia", "Cameroon", "Comoros", "Congo", "Ivory Coast", 
  "Djibouti", "Egypt", "El Salvador", "Eswatini", "Ghana", "Guatemala", "Honduras", 
  "India", "Indonesia", "Jordan", "Kenya", "Kiribati", "Kyrgyzstan", 
  "Laos", "Lesotho", "Libya", "Mauritania", 
  "Micronesia", "Mongolia", "Morocco", "Myanmar", "Nicaragua", 
  "Nigeria", "Palestine", "Pakistan", "Papua New Guinea", "Philippines", "Republic of Moldova", 
  "Sao Tome and Principe", "Senegal", "Solomon Islands", "Sudan", 
  "Syria", "Tajikistan", "Tunisia", "Ukraine", 
  "Uzbekistan", "Vanuatu", "Viet Nam", "Yemen", 
  "Zambia", "Zimbabwe"
)
setdiff(lower_middle_income, countries$country_name)

countries$country_name[str_detect(countries$country_name, "Venezuela")] <- "Venezuela"
countries$country_name[str_detect(countries$country_name, "Iran")] <- "Iran"
upper_middle_income <- c(
  "Albania", "Algeria", "Azerbaijan", "Belarus", "Belize", 
  "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "China", "Colombia", 
  "Costa Rica", "Croatia", "Cuba", "Dominica", "Dominican Republic", "Ecuador", 
  "Equatorial Guinea", "Fiji", "Gabon", "Georgia", "Grenada", "Guyana", 
  "Iran", "Iraq", "Jamaica", "Kazakhstan", "Lebanon", 
  "Malaysia", "Maldives", "Mauritius", "Mexico", "Montenegro", "Namibia", "Panama", 
  "Paraguay", "Peru", "North Macedonia", "Romania", "Russia", 
  "Saint Lucia", "Saint Vincent and Grenadines", "Samoa", "Serbia", 
  "South Africa", "Sri Lanka", "Suriname", "Thailand", "Tonga", "Turkey", 
  "Turkmenistan", "Venezuela"
)
setdiff(upper_middle_income, countries$country_name)

countries$country_name[str_detect(countries$country_name, "Taiwan")] <- "Taiwan"
countries$country_name[countries$country_name %in% "Republic of Korea"] <- "South Korea"
high_income <- c(
  "Andorra", "Antigua and Barbuda", "Argentina", "Australia", "Austria", "Bahamas", 
  "Bahrain", "Barbados", "Belgium", "Brunei Darussalam", "Canada", "Chile", 
  "Taiwan", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", 
  "Hungary", "Germany", "Greece", "Hong Kong SAR", "Iceland", "Ireland", "Israel", "Italy", "Japan", 
  "Kuwait", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", 
  "New Zealand", "Norway", "Oman", "Poland", "Portugal", "Puerto Rico", "Qatar", 
  "San Marino", "Saudi Arabia", "Seychelles", "Singapore", "Slovakia", 
  "Slovenia", "South Korea", "Spain", "Sweden", "Switzerland", "Trinidad and Tobago", 
  "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay"
)
setdiff(high_income, countries$country_name)

.huber_w <- function(u, k = 1.5) {
  # u = studentized residual; k ≈ 1.345 (Huber), use 1.5 for a bit more robustness
  w <- ifelse(abs(u) <= k, 1, k / abs(u))
  w^2
}

auto_weights <- function(df, y = "asmr", x = "year",
                         huber_k = 1.5, leverage_strength = 0.3,
                         min_w = 1e-4) {
  fml <- reformulate(x, response = paste0("log(", y, ")"))
  lm0 <- lm(fml, data = df)
  
  # studentized residuals (scale-free “how outlier-y” in Y)
  u   <- rstudent(lm0)
  
  # leverage (how unusual in X); range not fixed but typically small
  h   <- hatvalues(lm0)
  
  # residual robustness
  w_r <- .huber_w(u, k = huber_k)
  
  # leverage penalty (soft): divide by (1 + α h)
  w   <- w_r / (1 + leverage_strength * h)
  
  # stabilize scale, avoid zeros
  w   <- pmax(w / mean(w, na.rm = TRUE), min_w)
  w
}

aapc_gam <- function(g, x1, x2) {
  stopifnot(is.numeric(x1), is.numeric(x2), x2 > x1)
  # linear predictor rows at endpoints
  Xlp1 <- predict(g, newdata = data.frame(year = REPR_YEARS[1]), type = "lpmatrix")
  Xlp2 <- predict(g, newdata = data.frame(year = REPR_YEARS[2]), type = "lpmatrix")
  
  # averaging contrast for the slope on the log scale
  cvec <- drop(Xlp2 - Xlp1) / (x2 - x1)
  
  # coefficients and vcov
  b <- coef(g)
  V <- vcov(g)
  
  # drop columns with NA coefs or NA in cvec (rank deficiency, etc.)
  ok <- !is.na(b) & !is.na(cvec)
  b  <- b[ok]
  cvec <- cvec[ok]
  V  <- V[ok, ok, drop = FALSE]
  
  beta_bar <- sum(cvec * b)                          # = drop(cvec %*% b)
  se_bar   <- sqrt(drop(t(cvec) %*% V %*% cvec))
  z_stat <- beta_bar / se_bar
  p_value <- 2 * pnorm(-abs(z_stat))
  AAPC    <- 100 * (exp(beta_bar) - 1)
  AAPC_lo <- 100 * (exp(beta_bar - 1.96 * se_bar) - 1)
  AAPC_hi <- 100 * (exp(beta_bar + 1.96 * se_bar) - 1)
  
  data.frame(x1 = x1, x2 = x2, AAPC, AAPC_lo, AAPC_hi,  p_value = p_value)
}
derchange <- function(deriv_df, slope_threshold = JP.SENS, significance_level = 0.1) {
  
  deriv_df %>%
    mutate(
      z_score = abs(.derivative / .se),
      p_value = 2 * pnorm(-z_score),
      
      is_significant = p_value < significance_level,
      exceeds_threshold = abs(.derivative) >= slope_threshold,
      
      meaningful_change = is_significant & exceeds_threshold,
      
      change_type = case_when(
        !meaningful_change ~ "No meaningful change",
        .derivative > slope_threshold ~ "Meaningful increase",
        .derivative < -slope_threshold ~ "Meaningful decrease",
        TRUE ~ "Stable"
      ),
      
      ci_width = .upper_ci - .lower_ci
    )
}
