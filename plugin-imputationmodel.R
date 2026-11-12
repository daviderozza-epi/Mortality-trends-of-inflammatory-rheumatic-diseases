require(glmmTMB)
res <- g <- m <- imputazione <- modelli <- list()

DF <- 3
set.seed(2209)
CAUSEr <- CAUSE
dd_complete <- dd_complete[!dd_complete$country_name %in% "South Africa", ]
dd_complete$continent <- factor(dd_complete$continent, levels = c("Asia", "Europe", "Central-South America", "North America", "Oceania"))
table(dd_complete$country_name[is.na(dd_complete$continent)])

for (b in 1:BOOT){
  gc()
    annib <- sample(fc, size = 2*length(fc), replace = T) # bootstrap on countries
    for (i in 1:length(CAUSEr)){
      ltd <- dd_complete %>% dplyr::filter(cause %in% CAUSEr[i] & country_name %in% c(annib)) %>%
        dplyr::mutate(
          continent = factor(continent),
          world_bank = factor(world_bank),
          age_cut = factor(age_cut),
          sex = factor(sex),
          country_name = factor(country_name)
        )
      
      table(ltd$continent, exclude = F)
      ltd <- ltd %>% dplyr::arrange(year, country_name) %>% dplyr::group_by(country_name) %>% dplyr::mutate(
        lag_mortality = scale(lag(mortality, 1, default = 0)), # scale to standardize to the specific cause
        lag_mortality = ifelse(is.na(lag_mortality) | is.nan(lag_mortality), 0, lag_mortality),
        
        lag_residual = 0  # initialize
      ) %>% dplyr::ungroup()
      ltd <- ltd %>% dplyr::mutate(
        obs_weight = ifelse(is.na(mortality) | mortality == 0, 0.1, 1),
        post_covid = ifelse(year >= 2020, 1, 0)
      )
      require(splines)
      spline_basis <- ns(as.numeric(ltd$year), df = DF, Boundary.knots = c(REPR_YEARS[1], REPR_YEARS[2]))
      ltd$year_spline1 <- spline_basis[, 1]
      ltd$year_spline2 <- spline_basis[, 2] 
      ltd$year_spline3 <- spline_basis[, 3]
      
      formu_arma <- mortality ~ age_cut + continent + world_bank + sex + post_covid +
        year_spline1:world_bank + year_spline2:world_bank + year_spline3:world_bank + 
        
        (1 | country_name) + 
        (0 + year_spline1 | country_name) +  
        (1 | age_cut) + 
        (1 | continent) +
        offset(log(population))
      tryCatch({
        cat("Bootstrap:", b, "Cause:", CAUSEr[i], "\n")
        cat("ltd dimensions:", dim(ltd), "\n")   
        if (!CAUSEr[i] %in% c("T2D", "CVD", "Neoplasms")){
          
          
          prior1 <- data.frame(
            prior = c("normal(0, 1)",
                      "gamma(2, 0.1)"),
            class = c("fixef", "ranef"),
            coef  = c("", "")
          )
          fit_arma <- try(
            glmmTMB::glmmTMB(
              formu_arma,
              data = ltd,
              family = nbinom2,
              weights = obs_weight, 
              priors = prior1
            ),
            silent = TRUE
          )
          summary(fit_arma)
          # se non è stimabile ne faccio uno semplice
          if (inherits(fit_arma, "try-error")) {
            cat("glmmTMB failed, falling back to glm.nb\n")
            fit_arma <- MASS::glm.nb(mortality ~ age_cut + continent + sex + 
                        year_spline1:world_bank + post_covid, 
                        weights = obs_weight, data = ltd)
          }
        } else {
          
          fit_arma <- try(
            glmmTMB(
              formu_arma,
              data = ltd,
              family = poisson,
              
              weights = obs_weight, #priors = prior1,              
              control = glmmTMBControl(
                optimizer = nlminb,
                optArgs = list(method = "BFGS", lower = -30, upper = 30), 
                profile = quote(length(parameters$beta) >= 5)
              )),
            silent = TRUE
          )
          if (inherits(fit_arma, "try-error")) {
            cat("glmmTMB failed, falling back to glm.nb\n")
            fit_arma <- MASS::glm.nb(mortality ~ age_cut + continent + world_bank + sex + post_covid +
                        year_spline1:world_bank , data = ltd)
          }
        }
        cat(CAUSEr[i]); cat(b)
        summary(fit_arma)
        # prediction step ---
        if (!inherits(fit_arma, "try-error")) {
        ltd$continent <- factor(ltd$continent, levels = levels(fit_arma$frame$continent))
        ltd$age_cut <- factor(ltd$age_cut, levels = levels(as.factor(fit_arma$frame$age_cut)))
        ltd$world_bank <- factor(ltd$world_bank, levels = levels(fit_arma$frame$world_bank))
        ltd$sex <- factor(ltd$sex, levels = levels(fit_arma$frame$sex))
        }
        ltd$Predicted <- predict(fit_arma, newdata = ltd, type = "response", allow.new.levels = TRUE)
        summary(ltd$Predicted)
        if (any(is.na(ltd$Predicted))) stop("Prediction failure")
        
        ur <- ltd %>% dplyr::group_by(year, world_bank) %>% dplyr::summarise(
          mortality = sum(mortality, na.rm=T), population = sum(population, na.rm = T),
          Predicted = sum(round(Predicted), na.rm = T)) %>% dplyr::ungroup()
        ur$actual <- ur$mortality/ur$population
        ur$pred <- ur$Predicted/ur$population
        y_min <- min(c(ur$actual, ur$pred), na.rm = TRUE)
        y_max <- max(c(ur$actual, ur$pred), na.rm = TRUE)
        
        # Add tolerance
        tolerance <- .2 
        y_min_adj <- y_min * (1 - tolerance)
        y_max_adj <- y_max * (1 + tolerance)
        g[[i]] <- ggplot(ur, aes(x = year))+
          geom_line(aes(y = actual, color = "Actual"), size = .4) +
          geom_line(aes(y = pred, color = "Predicted"), size = .4, linetype = "dashed") +
          labs(title = CAUSEr[i], x = "Year", y = "Mortality Rate", color = "") +
          theme_minimal() + facet_grid(world_bank ~ 1) +
          coord_cartesian(ylim = c(y_min_adj, y_max_adj))
        res[[CAUSEr[i]]] <- ltd
        m[[CAUSEr[i]]] <- fit_arma
        plot(g[[i]])
        gc()
        
        
      }, error = function(e) {
        cat("Error in bootstrap", b, "cause", CAUSEr[i], ":\n")
        cat("Error message:", e$message, "\n")
        cat("Traceback:\n")
        print(traceback())
      })
    }
    impout <- list(res, m, g)
    modelli[[b]] <- m
    res <- impout[[1]]
    dd_imp <- do.call(rbind, lapply(res, as.data.frame))
    # b-th dataset
    ur <- ltd %>% dplyr::group_by(year, age_cut) %>% dplyr::summarise(
      mortality = sum(mortality, na.rm=T), population = sum(population, na.rm = T),
      Predicted = sum(round(Predicted), na.rm = T), .groups = "drop")
    ur$actual <- ur$mortality/ur$population
    ur$pred <- ur$Predicted/ur$population
    y_min <- min(c(ur$actual, ur$pred), na.rm = TRUE)
    y_max <- max(c(ur$actual, ur$pred), na.rm = TRUE)
    ggplot(ur, aes(x = year))+
      geom_line(aes(y = actual, color = "Actual"), size = .4) +
      geom_line(aes(y = pred, color = "Predicted"), size = .4, linetype = "dashed") +
      labs(title = CAUSEr[i], x = "Year", y = "Mortality Rate", color = "") +
      theme_minimal() + facet_grid(age_cut ~ 1) %>% print()
    
    imputazione[[b]] <- dd_imp %>% dplyr::mutate(.i = b)
    if (b %in% seq(10, 100, by = 10)){
      cat("Siamo a ", b, " forza \n") # save chunks of data 
      saveRDS(imputazione, file = paste0("ctrloutcomes_", b, ".rds"))
      }  
    
    gc()
}

aj <- list()
j_values <- seq(10, 100, by = 10)
i=1
for (i in 1:10){
  gc()
  o <- readRDS(paste0("ctrloutcomes_", j_values[i], ".rds"))
  o <- tail(o, 10)
  aj[[i]] <- bind_rows(o) %>% as.data.table()
  cat(i)
}
ct <- rbindlist(aj) 
table(ct$world_bank, exclude = F)
AJ <- ct

rm(imputazione); rm(o); rm(aj); gc()

saveRDS(AJ %>% as.data.frame(), file = "data/imp-T2D.Rds")
