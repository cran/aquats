## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  message = FALSE,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(emmeans)
library(multcomp)
library(MASS)
library(aquats)

## ----generate-two-group-data--------------------------------------------------
set.seed(42)
river_survey <- data.frame(
  Station = factor(rep(c("Upstream", "Downstream"), each = 25)),
  DO_mgL  = c(rnorm(25, mean = 7.8, sd = 0.55), rnorm(25, mean = 6.2, sd = 1.15))
)

## ----style-a-box, fig.width=6.5, fig.height=4.8-------------------------------
res_box <- compare_two_groups(
  data         = river_survey,
  response_var = "DO_mgL",
  group_var    = "Station",
  var_equal    = "auto",
  plot_type    = "box",
  palette      = "Dark2",
  title        = "Style A: Two-Sample Boxplot with Significance Bracket"
)

print(res_box$Plot)

## ----style-b-violin, fig.width=6.5, fig.height=4.8----------------------------
res_violin <- compare_two_groups(
  data         = river_survey,
  response_var = "DO_mgL",
  group_var    = "Station",
  var_equal    = "auto",
  plot_type    = "violin",
  palette      = "Set2",
  title        = "Style B: Density Violin with Internal Quartiles"
)

print(res_violin$Plot)

## ----style-c-jitter, fig.width=6.5, fig.height=4.8----------------------------
res_jitter <- compare_two_groups(
  data         = river_survey,
  response_var = "DO_mgL",
  group_var    = "Station",
  var_equal    = "auto",
  plot_type    = "jitter",
  palette      = "Set1",
  title        = "Style C: Group Means and 95% CI Error Bars"
)

print(res_jitter$Plot)

## ----inspect-two-group-models-------------------------------------------------
# 1. Distribution Normality per group (Shapiro-Wilk)
knitr::kable(res_box$Normality_Test, caption = "Shapiro-Wilk Normality Evaluation")

# 2. Fisher's F-test for Variance Equality
knitr::kable(res_box$F_Test, caption = "Fisher's F-Test for Homogeneity of Variances")

# 3. Two-Sample t-Test Results
knitr::kable(res_box$T_Test, caption = "t-Test Statistical Summary")

# 4. Group Descriptives and Cohen's d
knitr::kable(res_box$Summary_Table, caption = "Group Statistics and Standardized Effect Sizes")

## ----generate-chisq-data------------------------------------------------------
set.seed(42)
fish_maturity <- data.frame(
  Reach = factor(rep(c("Upstream", "Midstream", "Estuary"), times = c(60, 70, 70))),
  Maturity = factor(c(
    sample(c("Immature", "Maturing", "Spent"), 60, replace = TRUE, prob = c(0.60, 0.30, 0.10)),
    sample(c("Immature", "Maturing", "Spent"), 70, replace = TRUE, prob = c(0.35, 0.45, 0.20)),
    sample(c("Immature", "Maturing", "Spent"), 70, replace = TRUE, prob = c(0.15, 0.35, 0.50))
  ))
)

## ----style-a-stacked, fig.width=6.8, fig.height=4.8---------------------------
res_stacked <- test_chisq(
  data      = fish_maturity,
  x         = "Reach",
  y         = "Maturity",
  plot_type = "stacked",
  palette   = "Set2",
  title     = "Style A: Maturity Proportions Across River Reaches"
)

print(res_stacked$Plot)

## ----style-b-dodged, fig.width=6.8, fig.height=4.8----------------------------
res_dodged <- test_chisq(
  data      = fish_maturity,
  x         = "Reach",
  y         = "Maturity",
  plot_type = "dodged",
  palette   = "Dark2",
  title     = "Style B: Absolute Frequency Comparison"
)

print(res_dodged$Plot)

## ----inspect-chisq-output-----------------------------------------------------
# 1. Statistical Test Summary and Effect Size (Cramér's V)
knitr::kable(res_stacked$Chi_Square_Test, caption = "Chi-Square Test Summary and Association Strength")

# 2. Observed Contingency Table
knitr::kable(res_stacked$Observed_Counts, caption = "Observed Cell Frequencies")

# 3. Expected Cell Frequencies
knitr::kable(res_stacked$Expected_Counts, caption = "Expected Frequencies Under Independence")

# 4. Standardized Pearson Residuals (|r| > 2 denotes significant deviation)
knitr::kable(res_stacked$Standardized_Residuals, caption = "Standardized Pearson Residuals")

## ----anova_data_prep----------------------------------------------------------
set.seed(101)
n_obs <- 30

do_survey <- data.frame(
  Zone = factor(
    rep(c("Upper Basin", "Mid Reach", "Lower Estuary"), each = n_obs),
    levels = c("Upper Basin", "Mid Reach", "Lower Estuary")
  ),
  DO_mgL = c(
    rnorm(n_obs, mean = 8.50, sd = 0.45),
    rnorm(n_obs, mean = 6.80, sd = 0.55),
    rnorm(n_obs, mean = 4.90, sd = 0.60)
  )
)

head(do_survey, 6)

## ----anova_boxplot, fig.width = 6.5, fig.height = 5.0, fig.align = "center"----
res_box <- one_way_anova(
  data           = do_survey,
  factor_var     = "Zone",
  numeric_var    = "DO_mgL",
  plot_type      = "boxplot",
  add_jitter     = TRUE,
  show_mean      = TRUE,
  mean_color     = "darkred",
  sig_display    = "letters",
  color_palette  = "Blues"
)

res_box$Plot

## ----anova_barplot, fig.width = 6.5, fig.height = 5.0, fig.align = "center"----
res_bar <- one_way_anova(
  data           = do_survey,
  factor_var     = "Zone",
  numeric_var    = "DO_mgL",
  plot_type      = "barplot",
  error_type     = "se",
  sig_display    = "letters",
  color_palette  = "Blues"
)

res_bar$Plot

## ----anova_pointrange, fig.width = 6.5, fig.height = 5.0, fig.align = "center"----
res_point <- one_way_anova(
  data           = do_survey,
  factor_var     = "Zone",
  numeric_var    = "DO_mgL",
  plot_type      = "pointrange",
  error_type     = "se",
  sig_display    = "letters",
  color_palette  = "Dark2"
)

res_point$Plot

## ----anova_inspection---------------------------------------------------------
# Global ANOVA Table (F-statistic and p-value)
res_box$ANOVA_Summary

# Pairwise Tukey HSD Differences
res_box$TukeyHSD

# Estimated Marginal Means (EMMs)
res_box$EMMs

# Computed Summary Statistics Table
res_box$Summary_Table

## ----twoway_data_prep---------------------------------------------------------
set.seed(42)
n_reps <- 15

basins <- c("Upper Reach", "Central Basin", "Lower Estuary")
seasons <- c("Wet Season", "Dry Season")

fish_condition <- data.frame(
  Basin = factor(rep(rep(basins, each = n_reps), times = 2), levels = basins),
  Season = factor(rep(seasons, each = n_reps * length(basins)), levels = seasons),
  Condition_Factor = c(
    # Wet Season: Upper, Central, Lower
    rnorm(n_reps, mean = 1.35, sd = 0.08),
    rnorm(n_reps, mean = 1.18, sd = 0.09),
    rnorm(n_reps, mean = 1.05, sd = 0.10),
    # Dry Season: Upper, Central, Lower
    rnorm(n_reps, mean = 1.20, sd = 0.07),
    rnorm(n_reps, mean = 0.98, sd = 0.08),
    rnorm(n_reps, mean = 0.82, sd = 0.09)
  )
)

head(fish_condition, 6)

## ----twoway_boxplot, fig.width = 7.0, fig.height = 5.2, fig.align = "center"----
res_two_box <- two_way_anova(
  data           = fish_condition,
  factor1_var    = "Basin",
  factor2_var    = "Season",
  numeric_var    = "Condition_Factor",
  plot_type      = "boxplot",
  add_jitter     = TRUE,
  show_mean      = TRUE,
  mean_color     = "darkred",
  color_palette  = "Set2"
)

res_two_box$Plot

## ----twoway_barplot_faceted, fig.width = 7.0, fig.height = 4.8, fig.align = "center"----
res_two_bar <- two_way_anova(
  data           = fish_condition,
  factor1_var    = "Basin",
  factor2_var    = "Season",
  numeric_var    = "Condition_Factor",
  facet_var      = "Season",
  plot_type      = "barplot",
  error_type     = "se",
  color_palette  = "Set2"
)

res_two_bar$Plot

## ----twoway_pointrange, fig.width = 7.0, fig.height = 5.0, fig.align = "center"----
res_two_point <- two_way_anova(
  data           = fish_condition,
  factor1_var    = "Basin",
  factor2_var    = "Season",
  numeric_var    = "Condition_Factor",
  plot_type      = "pointrange",
  error_type     = "se",
  color_palette  = "Dark2"
)

res_two_point$Plot

## ----twoway_inspection--------------------------------------------------------
# Factorial ANOVA Table (Main effects and Basin:Season interaction)
res_two_box$ANOVA_Summary

# Post-hoc pairwise differences (Tukey HSD)
res_two_box$TukeyHSD

# Estimated Marginal Means with compact letter displays
res_two_box$EMMs

# Summary statistics table
res_two_box$Summary_Table

## ----threeway_data_prep-------------------------------------------------------
set.seed(303)
n_reps <- 10

basins  <- c("North Basin", "South Basin")
seasons <- c("Pre-Monsoon", "Post-Monsoon")
depths  <- c("Epilimnion", "Hypolimnion")

design_grid <- expand.grid(
  Basin  = basins,
  Season = seasons,
  Depth  = depths
)

chla_data <- do.call(rbind, lapply(seq_len(nrow(design_grid)), function(idx) {
  row_info <- design_grid[idx, ]
  
  # Baseline Chlorophyll-a dynamic
  base_val <- 14.0
  if (row_info$Basin == "South Basin") base_val <- base_val + 4.5
  if (row_info$Season == "Post-Monsoon") base_val <- base_val + 6.0
  if (row_info$Depth == "Hypolimnion") base_val <- base_val - 5.5
  
  # Three-way synergistic interaction in productive shallow waters
  if (row_info$Basin == "South Basin" && row_info$Season == "Post-Monsoon" && row_info$Depth == "Epilimnion") {
    base_val <- base_val + 8.0
  }
  
  data.frame(
    Basin    = factor(rep(row_info$Basin, n_reps), levels = basins),
    Season   = factor(rep(row_info$Season, n_reps), levels = seasons),
    Depth    = factor(rep(row_info$Depth, n_reps), levels = depths),
    Chla_ugL = rnorm(n_reps, mean = base_val, sd = 1.2)
  )
}))

head(chla_data, 6)

## ----threeway_boxplot, fig.width = 7.5, fig.height = 5.2, fig.align = "center"----
res_three_box <- three_way_anova(
  data          = chla_data,
  factor1_var   = "Basin",
  factor2_var   = "Season",
  factor3_var   = "Depth",
  numeric_var   = "Chla_ugL",
  plot_type     = "boxplot",
  add_jitter    = TRUE,
  show_mean     = TRUE,
  mean_color    = "darkred",
  color_palette = "Set1"
)

res_three_box$Plot

## ----threeway_barplot, fig.width = 7.5, fig.height = 5.2, fig.align = "center"----
res_three_bar <- three_way_anova(
  data          = chla_data,
  factor1_var   = "Basin",
  factor2_var   = "Season",
  factor3_var   = "Depth",
  numeric_var   = "Chla_ugL",
  plot_type     = "barplot",
  error_type    = "se",
  color_palette = "Set1"
)

res_three_bar$Plot

## ----threeway_pointrange, fig.width = 7.5, fig.height = 5.2, fig.align = "center"----
res_three_point <- three_way_anova(
  data          = chla_data,
  factor1_var   = "Basin",
  factor2_var   = "Season",
  factor3_var   = "Depth",
  numeric_var   = "Chla_ugL",
  plot_type     = "pointrange",
  error_type    = "se",
  color_palette = "Dark2"
)

res_three_point$Plot

## ----threeway_inspection------------------------------------------------------
# Global Three-Way ANOVA Table (Main effects, 2-way, and 3-way interactions)
res_three_box$ANOVA_Summary

# Post-hoc pairwise differences (Tukey HSD)
res_three_box$TukeyHSD

# Estimated Marginal Means across the 3-way interaction grid
res_three_box$EMMs

# Aggregated summary statistics
res_three_box$Summary_Table

## ----ancova_data_prep---------------------------------------------------------
set.seed(2026)
n_per_habitat <- 35

habitats <- c("Estuary", "River", "Marine")

fish_growth <- do.call(rbind, lapply(habitats, function(hab) {
  # Generate continuous covariate: fish total length (cm)
  len <- runif(n_per_habitat, min = 12.0, max = 38.0)
  
  # Group elevation shifts (intercept differences)
  elevation_shift <- switch(hab,
    "Estuary" = -12.0,
    "River"   = -4.0,
    "Marine"  = 8.0
  )
  
  # Additive response: common slope (beta = 3.25) + habitat elevation + noise
  wt <- 3.25 * len + elevation_shift + rnorm(n_per_habitat, mean = 0, sd = 4.2)
  
  data.frame(
    Habitat = factor(rep(hab, n_per_habitat), levels = habitats),
    Length  = len,
    Weight  = wt
  )
}))

head(fish_growth, 6)

## ----ancova_plot_single, fig.width = 7.0, fig.height = 5.0, fig.align = "center"----
res_ancova <- ancova_analysis(
  data          = fish_growth,
  response_var  = "Weight",
  factor_var    = "Habitat",
  covariate_var = "Length",
  facet         = FALSE,
  color_palette = "Dark2"
)

# Render publication plot
res_ancova$Plot

## ----ancova_plot_faceted, fig.width = 7.5, fig.height = 4.2, fig.align = "center"----
res_ancova_facet <- ancova_analysis(
  data          = fish_growth,
  response_var  = "Weight",
  factor_var    = "Habitat",
  covariate_var = "Length",
  facet         = TRUE,
  color_palette = "Dark2"
)

res_ancova_facet$Plot

## ----ancova_inspection--------------------------------------------------------
# 1. ANCOVA Table (Testing covariate and factor significance)
res_ancova$ANCOVA_Table

# 2. Estimated Marginal Means (EMMs adjusted to mean Length)
res_ancova$Adjusted_Means

## ----manova_data_prep---------------------------------------------------------
set.seed(505)
n_fish <- 25

populations <- c("River_A", "River_B", "River_C")

# Simulate multivariate normal morphometric data
morpho_data <- do.call(rbind, lapply(populations, function(pop) {
  # Baseline centroid shifts per population
  shift <- switch(pop,
    "River_A" = c(TL = 24.0, HL = 5.2, BD = 6.5, CW = 2.1),
    "River_B" = c(TL = 28.5, HL = 6.4, BD = 7.8, CW = 2.6),
    "River_C" = c(TL = 21.0, HL = 4.8, BD = 5.9, CW = 1.9)
  )
  
  data.frame(
    Population = factor(rep(pop, n_fish), levels = populations),
    TL = rnorm(n_fish, mean = shift["TL"], sd = 1.5),
    HL = rnorm(n_fish, mean = shift["HL"], sd = 0.4),
    BD = rnorm(n_fish, mean = shift["BD"], sd = 0.5),
    CW = rnorm(n_fish, mean = shift["CW"], sd = 0.2)
  )
}))

head(morpho_data, 6)

## ----manova_boxplot, fig.width = 7.5, fig.height = 5.5, fig.align = "center"----
res_manova_box <- manova_analysis(
  data           = morpho_data,
  response_vars  = c("TL", "HL", "BD", "CW"),
  factor_var     = "Population",
  plot_type      = "boxplot",
  color_palette  = "Set2"
)

# Render faceted traits
res_manova_box$Plot

## ----manova_lda, fig.width = 7.0, fig.height = 5.0, fig.align = "center"------
res_manova_lda <- manova_analysis(
  data           = morpho_data,
  response_vars  = c("TL", "HL", "BD", "CW"),
  factor_var     = "Population",
  plot_type      = "lda",
  color_palette  = "Set2"
)

# Render canonical discriminant ordination
res_manova_lda$Plot

## ----manova_inspection--------------------------------------------------------
# 1. Omnibus MANOVA Summary (Pillai's Trace, F-approximation, p-value)
res_manova_box$MANOVA_Summary

# 2. Univariate ANOVA Breakdowns for Each Morphometric Trait
res_manova_box$Univariate_ANOVAs

## ----regression_data_prep-----------------------------------------------------
set.seed(606)
n_fish <- 25
rivers <- c("River A", "River B", "River C")

fish_allometry <- do.call(rbind, lapply(rivers, function(rv) {
  len <- runif(n_fish, min = 12, max = 38)
  # Allometric power parameters: W = a * L^b
  a_param <- switch(rv, "River A" = 0.012, "River B" = 0.009, "River C" = 0.015)
  b_param <- switch(rv, "River A" = 3.05,  "River B" = 3.18,  "River C" = 2.92)
  
  wt <- a_param * (len^b_param) * exp(rnorm(n_fish, mean = 0, sd = 0.12))
  data.frame(
    River  = rv,
    Length = len,
    Weight = wt,
    stringsAsFactors = FALSE
  )
}))

head(fish_allometry, 6)

## ----regression_power_pred, fig.width = 7.0, fig.height = 5.0, fig.align = "center"----
res_power_reg <- regression_analysis(
  data          = fish_allometry,
  x_var         = "Length",
  y_var         = "Weight",
  fit_type      = "power",
  interval      = "prediction",
  show_equation = TRUE,
  title         = "Allometric Fish Length-Weight Relationship (95% PI)"
)

# Render allometric power plot
res_power_reg$Plot

## ----regression_exponential, fig.width = 7.0, fig.height = 5.0, fig.align = "center"----
res_exp_reg <- regression_analysis(
  data          = fish_allometry,
  x_var         = "Length",
  y_var         = "Weight",
  fit_type      = "exponential",
  interval      = "confidence",
  show_equation = TRUE,
  title         = "Exponential Length-Weight Curve (95% CI)"
)

# Render exponential curve
res_exp_reg$Plot

## ----regression_poly_facet, fig.width = 7.5, fig.height = 4.5, fig.align = "center"----
res_poly_reg <- regression_analysis(
  data          = fish_allometry,
  x_var         = "Length",
  y_var         = "Weight",
  fit_type      = "polynomial",
  group_var     = "River",
  facet         = TRUE,
  interval      = "confidence",
  show_equation = TRUE,
  color_palette = "Dark2",
  title         = "Faceted Quadratic Growth Trajectories by Basin"
)

# Render faceted quadratic curves
res_poly_reg$Plot

## ----regression_cubic_facet, fig.width = 7.5, fig.height = 4.5, fig.align = "center"----
res_cubic_reg <- regression_analysis(
  data          = fish_allometry,
  x_var         = "Length",
  y_var         = "Weight",
  fit_type      = "cubic",
  group_var     = "River",
  facet         = TRUE,
  interval      = "confidence",
  show_equation = TRUE,
  color_palette = "Set1",
  title         = "Faceted Cubic Growth Polynomials Across Basins"
)

# Render faceted cubic curves
res_cubic_reg$Plot

## ----regression_inspection_updated--------------------------------------------
# 1. Tidy summary of model equations, R2 values, and p-values
res_poly_reg$Equation_Table

# 2. Inspect underlying linear model coefficients for River A
summary(res_poly_reg$Models[["River A"]])

# 3. Preview calculated prediction grid with 95% confidence intervals
head(res_poly_reg$Predictions, 6)

## ----glm_data_prep------------------------------------------------------------
set.seed(707)
n_trawls <- 120

temp_c   <- runif(n_trawls, min = 18, max = 30)
sal_psu  <- runif(n_trawls, min = 5, max = 32)
do_mgl   <- runif(n_trawls, min = 3.5, max = 9.0)
substr   <- sample(c("Mud", "Sand"), size = n_trawls, replace = TRUE)

# Multiplicative linear predictor
log_mu <- -1.8 + 
  0.08 * temp_c - 
  0.04 * sal_psu + 
  0.22 * do_mgl + 
  ifelse(substr == "Sand", 0.35, 0.0)

# Simulate Poisson counts
catch_counts <- rpois(n_trawls, lambda = exp(log_mu))

trawl_survey <- data.frame(
  Abundance   = catch_counts,
  Temperature = temp_c,
  Salinity    = sal_psu,
  DO          = do_mgl,
  Substrate   = factor(substr, levels = c("Mud", "Sand"))
)

head(trawl_survey, 6)

## ----glm_poisson_plot, fig.width = 7.0, fig.height = 4.8, fig.align = "center"----
res_glm_count <- glm_analysis(
  data           = trawl_survey,
  response_var   = "Abundance",
  predictor_vars = c("Temperature", "Salinity", "DO", "Substrate"),
  family_type    = "poisson",
  color_palette  = "Dark2"
)

# Render forest plot
res_glm_count$Plot

## ----glm_binomial_plot, fig.width = 7.0, fig.height = 4.8, fig.align = "center"----
# Create binary presence/absence column
trawl_survey$Presence <- ifelse(trawl_survey$Abundance > 0, 1, 0)

res_glm_binary <- glm_analysis(
  data           = trawl_survey,
  response_var   = "Presence",
  predictor_vars = c("Temperature", "Salinity", "DO", "Substrate"),
  family_type    = "binomial",
  color_palette  = "Set1"
)

res_glm_binary$Plot

## ----glm_inspection-----------------------------------------------------------
# 1. Global GLM Summary (Deviance, Dispersion, Estimates, z-values, p-values)
res_glm_count$Model_Summary

# 2. Tidy Coefficient Table with Exponentiated Effect Sizes (IRR) and 95% CIs
res_glm_count$Coefficient_Table

## ----nmds_data_prep-----------------------------------------------------------
set.seed(808)
n_sites <- 15
habitats <- c("Riffle", "Run", "Pool")

# Macroinvertebrate community matrix (7 diagnostic taxa)
comm_list <- lapply(habitats, function(hab) {
  # Baseline taxon abundance profiles
  means <- switch(hab,
    "Riffle" = c(Ephem = 28, Plecop = 22, Trichop = 18, Chirono = 4,  Oligoch = 1,  Odonat = 2,  Gastrop = 1),
    "Run"    = c(Ephem = 12, Plecop = 8,  Trichop = 14, Chirono = 16, Oligoch = 6,  Odonat = 5,  Gastrop = 3),
    "Pool"   = c(Ephem = 2,  Plecop = 0,  Trichop = 4,  Chirono = 32, Oligoch = 24, Odonat = 12, Gastrop = 18)
  )
  
  # Simulate Poisson counts
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_sites, lambda = m)))
  df <- as.data.frame(mat)
  df$Habitat <- hab
  df
})

macro_data <- do.call(rbind, comm_list)
# Place metadata as first column
macro_data <- macro_data[, c("Habitat", setdiff(names(macro_data), "Habitat"))]

# Corresponding environmental matrix
env_data <- data.frame(
  Velocity_ms = c(rnorm(n_sites, 0.85, 0.1), rnorm(n_sites, 0.45, 0.08), rnorm(n_sites, 0.12, 0.04)),
  DO_mgl      = c(rnorm(n_sites, 9.2, 0.4),  rnorm(n_sites, 7.8, 0.5),   rnorm(n_sites, 5.4, 0.6)),
  Temp_C      = c(rnorm(n_sites, 19.5, 0.8), rnorm(n_sites, 22.0, 0.7),  rnorm(n_sites, 25.8, 1.1))
)

head(macro_data[, 1:6], 6)

## ----nmds_plot_basic, fig.width = 7.0, fig.height = 5.2, fig.align = "center"----
res_nmds <- nmds_analysis(
  data        = macro_data,
  group_col   = "Habitat",
  transform   = "hellinger",
  dist_method = "bray",
  palette     = "Dark2"
)

# Render ordination plot
res_nmds$Plot

## ----nmds_plot_envfit, fig.width = 7.5, fig.height = 5.5, fig.align = "center"----
res_nmds_env <- nmds_analysis(
  data        = macro_data,
  group_col   = "Habitat",
  env_data    = env_data,
  transform   = "hellinger",
  dist_method = "bray",
  palette     = "Set2",
  title       = "Macroinvertebrate Community NMDS with Environmental Drivers"
)

res_nmds_env$Plot

## ----nmds_inspection----------------------------------------------------------
# 1. Final Ordination Stress
res_nmds$Stress

# 2. Tidy NMDS Axis Coordinates
head(res_nmds$Plot_Data, 6)

# 3. Environmental Vector Fits (r2, p-values)
res_nmds_env$Envfit_Results

## ----permanova_data_prep------------------------------------------------------
set.seed(909)
n_reps <- 12

substrates <- c("Coarse Gravel", "Fine Sand", "Silty Mud")

benthic_survey <- do.call(rbind, lapply(substrates, function(sub) {
  # Diagnostic species abundance baselines
  means <- switch(sub,
    "Coarse Gravel" = c(Polychaeta = 14, Bivalvia = 6,  Amphipoda = 28, Decapoda = 12, Echinodermata = 16),
    "Fine Sand"     = c(Polychaeta = 22, Bivalvia = 18, Amphipoda = 15, Decapoda = 8,  Echinodermata = 4),
    "Silty Mud"     = c(Polychaeta = 35, Bivalvia = 26, Amphipoda = 4,  Decapoda = 2,  Echinodermata = 1)
  )
  
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_reps, lambda = m)))
  df <- as.data.frame(mat)
  df$Substrate <- sub
  df
}))

# Organize Substrate as grouping metadata column
benthic_survey <- benthic_survey[, c("Substrate", setdiff(names(benthic_survey), "Substrate"))]

head(benthic_survey, 6)

## ----permanova_run------------------------------------------------------------
res_permanova <- permanova_analysis(
  data         = benthic_survey,
  group_col    = "Substrate",
  dist_method  = "bray",
  transform    = "hellinger",
  permutations = 999,
  seed         = 42
)

## ----permanova_inspection-----------------------------------------------------
# 1. PERMANOVA Table (adonis2 results)
res_permanova$PERMANOVA_Table

# 2. Distance Metric Applied
res_permanova$Distance_Method

# 3. Cleaned and Transformed Data Preview
head(res_permanova$Cleaned_Data, 4)

## ----simper_data_prep---------------------------------------------------------
set.seed(1010)
n_stations <- 12
zones <- c("Littoral", "Pelagic", "Profundal")

lake_survey <- do.call(rbind, lapply(zones, function(zn) {
  # Distinct abundance profiles across lake zones
  means <- switch(zn,
    "Littoral"  = c(Perch = 32, Pike = 8,  Roach = 45, Bream = 22, Bleak = 12, Zander = 2),
    "Pelagic"   = c(Perch = 10, Pike = 1,  Roach = 18, Bream = 6,  Bleak = 55, Zander = 14),
    "Profundal" = c(Perch = 2,  Pike = 0,  Roach = 4,  Bream = 1,  Bleak = 2,  Zander = 28)
  )
  
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_stations, lambda = m)))
  df <- as.data.frame(mat)
  df$Zone <- zn
  df
}))

# Organize Zone as grouping metadata column
lake_survey <- lake_survey[, c("Zone", setdiff(names(lake_survey), "Zone"))]

head(lake_survey, 6)

## ----simper_barplot, fig.width = 8.0, fig.height = 5.2, fig.align = "center"----
res_simper <- simper_analysis(
  data         = lake_survey,
  group_col    = "Zone",
  top_n        = 5,
  transform    = "hellinger",
  permutations = 999,
  seed         = 42
)

# Render faceted contribution plot
res_simper$Plot

## ----simper_inspection--------------------------------------------------------
# 1. Tidy summary table of top discriminating taxa
head(res_simper$Summary_Table, 10)

# 2. Inspect raw vegan SIMPER summary for a specific comparison
summary(res_simper$SIMPER_Object)$Littoral_Pelagic[1:5, ]

## ----pca_biplot_data_prep-----------------------------------------------------
set.seed(1111)
n_samples <- 14
salinity_regimes <- c("Oligohaline", "Mesohaline", "Polyhaline")

zooplankton_data <- do.call(rbind, lapply(salinity_regimes, function(reg) {
  # Diagnostic zooplankton abundance profiles across salinity regimes
  means <- switch(reg,
    "Oligohaline" = c(Rotifer_A = 48, Rotifer_B = 35, Cladocera_A = 26, Cladocera_B = 18, Cyclopoid = 12, Calanoid = 4,  Mysid = 1,  Cirripede = 0),
    "Mesohaline"  = c(Rotifer_A = 12, Rotifer_B = 8,  Cladocera_A = 14, Cladocera_B = 22, Cyclopoid = 32, Calanoid = 28, Mysid = 9,  Cirripede = 5),
    "Polyhaline"  = c(Rotifer_A = 2,  Rotifer_B = 0,  Cladocera_A = 3,  Cladocera_B = 4,  Cyclopoid = 15, Calanoid = 42, Mysid = 24, Cirripede = 38)
  )
  
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_samples, lambda = m)))
  df <- as.data.frame(mat)
  df$Salinity_Zone <- reg
  df
}))

# Place metadata as first column
zooplankton_data <- zooplankton_data[, c("Salinity_Zone", setdiff(names(zooplankton_data), "Salinity_Zone"))]

head(zooplankton_data[, 1:6], 6)

## ----pca_biplot_default, fig.width = 7.5, fig.height = 5.5, fig.align = "center"----
res_pca_biplot <- pca_community_biplot(
  data             = zooplankton_data,
  group_col        = "Salinity_Zone",
  transform        = "hellinger",
  scale            = FALSE,
  color_palette    = "Dark2",
  arrow_multiplier = 0.8
)

# Render community biplot
res_pca_biplot$Plot

## ----pca_biplot_top5, fig.width = 7.5, fig.height = 5.5, fig.align = "center"----
res_pca_top5 <- pca_community_biplot(
  data             = zooplankton_data,
  group_col        = "Salinity_Zone",
  transform        = "hellinger",
  top_n_species    = 5,
  color_palette    = "Set1",
  arrow_multiplier = 0.85,
  title            = "Zooplankton PCA Biplot (Top 5 Indicator Species)"
)

res_pca_top5$Plot

## ----pca_biplot_inspection----------------------------------------------------
# 1. Inspect species vector coordinates and loading lengths
head(res_pca_biplot$Species_Scores, 6)

# 2. Site ordination coordinates (PC1 and PC2 scores)
head(res_pca_biplot$Plot_Data, 6)

# 3. Eigenvalues and total variance summary
summary(res_pca_biplot$PCA_Object)$cont$importance[, 1:2]

## ----pca_circle_data_prep-----------------------------------------------------
set.seed(1212)
n_stations <- 40

# Simulate correlated water quality gradients
temperature <- rnorm(n_stations, mean = 24.5, sd = 3.0)
do_mgl      <- 14.5 - (0.35 * temperature) + rnorm(n_stations, 0, 0.5)
ph_val      <- 7.2 + (0.04 * temperature) + rnorm(n_stations, 0, 0.3)
turbidity   <- runif(n_stations, 5, 45)
secchi_cm   <- 120 - (2.1 * turbidity) + rnorm(n_stations, 0, 4)
bod_mgl     <- 1.2 + (0.08 * turbidity) + rnorm(n_stations, 0, 0.4)
nitrate_mgl <- rnorm(n_stations, mean = 2.4, sd = 0.6)
phosphate   <- 0.15 + (0.05 * nitrate_mgl) + rnorm(n_stations, 0, 0.03)

water_quality <- data.frame(
  StationID   = paste0("ST_", sprintf("%02d", 1:n_stations)),
  Temperature = temperature,
  DO          = do_mgl,
  pH          = ph_val,
  Turbidity   = turbidity,
  Secchi      = secchi_cm,
  BOD         = bod_mgl,
  Nitrate     = nitrate_mgl,
  Phosphate   = phosphate
)

head(water_quality, 6)

## ----pca_circle_plot, fig.width = 6.8, fig.height = 6.2, fig.align = "center"----
res_pca_circle <- pca_correlation_circle(
  data         = water_quality,
  exclude_cols = "StationID",
  dim1         = 1,
  dim2         = 2,
  palette      = c("#2A9D8F", "#E9C46A", "#F4A261", "#E76F51"),
  title        = "Water Quality Parameters Correlation Circle"
)

# Render correlation circle
res_pca_circle$Plot

## ----pca_circle_inspection----------------------------------------------------
# 1. Variable coordinates along PC1 and PC2 with cos2 representation quality
res_pca_circle$Variable_Stats

# 2. Percentage and cumulative variance explained per principal component
res_pca_circle$Variance_Summary

# 3. Individual station ordination scores (first 6 samples)
head(res_pca_circle$Sample_Scores[, 1:4], 6)

## ----rda_data_prep------------------------------------------------------------
set.seed(1313)
n_sites <- 12
zones <- c("Headwater", "Mid-River", "Lowland")

# Simulated environmental matrix
env_list <- lapply(zones, function(zn) {
  switch(zn,
    "Headwater" = data.frame(
      Temp_C = rnorm(n_sites, 14.5, 0.8),
      DO_mgL = rnorm(n_sites, 10.2, 0.4),
      Flow_ms = rnorm(n_sites, 0.95, 0.1),
      Cond_uS = rnorm(n_sites, 120, 15)
    ),
    "Mid-River" = data.frame(
      Temp_C = rnorm(n_sites, 19.0, 0.7),
      DO_mgL = rnorm(n_sites, 8.1, 0.5),
      Flow_ms = rnorm(n_sites, 0.55, 0.08),
      Cond_uS = rnorm(n_sites, 280, 25)
    ),
    "Lowland"   = data.frame(
      Temp_C = rnorm(n_sites, 24.5, 1.0),
      DO_mgL = rnorm(n_sites, 5.8, 0.6),
      Flow_ms = rnorm(n_sites, 0.18, 0.05),
      Cond_uS = rnorm(n_sites, 490, 35)
    )
  )
})
env_survey <- do.call(rbind, env_list)

# Matched community counts (7 fish species)
comm_list <- lapply(zones, function(zn) {
  means <- switch(zn,
    "Headwater" = c(Trout = 30, Sculpin = 22, Dace = 12, Minnow = 4,  Barb = 1,  Catfish = 0,  Carp = 0),
    "Mid-River" = c(Trout = 6,  Sculpin = 8,  Dace = 24, Minnow = 28, Barb = 14, Catfish = 4,  Carp = 2),
    "Lowland"   = c(Trout = 0,  Sculpin = 1,  Dace = 5,  Minnow = 12, Barb = 32, Catfish = 26, Carp = 38)
  )
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_sites, lambda = m)))
  df <- as.data.frame(mat)
  df$Zone <- zn
  df
})
comm_survey <- do.call(rbind, comm_list)

# Metadata as first column
comm_survey <- comm_survey[, c("Zone", setdiff(names(comm_survey), "Zone"))]

head(comm_survey[, 1:6], 4)
head(env_survey, 4)

## ----rda_plot_triplot, fig.width = 7.8, fig.height = 5.8, fig.align = "center"----
res_rda <- rda_analysis(
  comm_data          = comm_survey,
  env_data           = env_survey,
  group_col          = "Zone",
  transform          = "hellinger",
  color_palette      = "Dark2",
  species_arrow_mult = 1.0,
  env_arrow_mult     = 1.2
)

# Render publication triplot
res_rda$Plot

## ----rda_inspection-----------------------------------------------------------
# 1. Global RDA Model Permutation Test (adonis/anova)
res_rda$Model_Significance

# 2. Marginal Environmental Term Significance (unique partial effects)
res_rda$Variable_Significance

# 3. Constrained vs Unconstrained Inertia Breakdown
summary(res_rda$RDA_Object)$tot.chi
summary(res_rda$RDA_Object)$concont$importance[, 1:2]

## ----dbrda_data_prep----------------------------------------------------------
set.seed(1414)
n_stations <- 12
zones <- c("Upper Estuary", "Middle Basin", "Outer Marine")

# Simulated environmental predictors
env_list <- lapply(zones, function(zn) {
  switch(zn,
    "Upper Estuary" = data.frame(
      Salinity_PSU = rnorm(n_stations, 8.5, 1.2),
      TOC_percent  = rnorm(n_stations, 4.2, 0.5),
      Silt_percent = rnorm(n_stations, 68, 6),
      Depth_m      = rnorm(n_stations, 3.5, 0.4)
    ),
    "Middle Basin"  = data.frame(
      Salinity_PSU = rnorm(n_stations, 18.0, 1.5),
      TOC_percent  = rnorm(n_stations, 2.6, 0.4),
      Silt_percent = rnorm(n_stations, 42, 5),
      Depth_m      = rnorm(n_stations, 8.2, 0.8)
    ),
    "Outer Marine"  = data.frame(
      Salinity_PSU = rnorm(n_stations, 32.5, 0.9),
      TOC_percent  = rnorm(n_stations, 1.1, 0.2),
      Silt_percent = rnorm(n_stations, 15, 3),
      Depth_m      = rnorm(n_stations, 18.5, 1.5)
    )
  )
})
env_estuary <- do.call(rbind, env_list)

# Simulated benthic community matrix (6 diagnostic taxa)
comm_list <- lapply(zones, function(zn) {
  means <- switch(zn,
    "Upper Estuary" = c(Oligochaeta = 42, Chironomidae = 35, Polychaeta_A = 6,  Bivalvia_A = 2,  Amphipoda_A = 4,  Decapoda_A = 0),
    "Middle Basin"  = c(Oligochaeta = 14, Chironomidae = 18, Polychaeta_A = 28, Bivalvia_A = 22, Amphipoda_A = 16, Decapoda_A = 5),
    "Outer Marine"  = c(Oligochaeta = 1,  Chironomidae = 0,  Polychaeta_A = 48, Bivalvia_A = 34, Amphipoda_A = 30, Decapoda_A = 18)
  )
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_stations, lambda = m)))
  df <- as.data.frame(mat)
  df$Estuary_Zone <- zn
  df
})
comm_estuary <- do.call(rbind, comm_list)

# Set grouping metadata as first column
comm_estuary <- comm_estuary[, c("Estuary_Zone", setdiff(names(comm_estuary), "Estuary_Zone"))]

head(comm_estuary[, 1:6], 4)
head(env_estuary, 4)

## ----dbrda_plot_triplot, fig.width = 7.8, fig.height = 5.8, fig.align = "center"----
res_dbrda <- dbrda_analysis(
  comm_data          = comm_estuary,
  env_data           = env_estuary,
  group_col          = "Estuary_Zone",
  dist_method        = "bray",
  transform          = "none",
  color_palette      = "Dark2",
  species_arrow_mult = 1.0,
  env_arrow_mult     = 1.1
)

# Render publication triplot
res_dbrda$Plot

## ----dbrda_inspection---------------------------------------------------------
# 1. Global db-RDA Permutation Test (adonis/anova)
res_dbrda$Model_Significance

# 2. Marginal Environmental Predictor Significance (unique partial variance)
res_dbrda$Variable_Significance

# 3. Constrained vs Unconstrained Variance Breakdown
summary(res_dbrda$dbRDA_Object)$tot.chi
summary(res_dbrda$dbRDA_Object)$concont$importance[, 1:2]

## ----cluster_data_prep--------------------------------------------------------
set.seed(1515)
n_per_zone <- 8
lake_zones <- c("Littoral", "Pelagic", "Hypolimnetic")

phyto_list <- lapply(lake_zones, function(zn) {
  means <- switch(zn,
    "Littoral"     = c(Diatom_A = 35, Diatom_B = 24, Cyanob_A = 4,  Cyanob_B = 1,  Green_A = 18, Green_B = 12),
    "Pelagic"      = c(Diatom_A = 12, Diatom_B = 8,  Cyanob_A = 42, Cyanob_B = 30, Green_A = 25, Green_B = 18),
    "Hypolimnetic" = c(Diatom_A = 4,  Diatom_B = 2,  Cyanob_A = 2,  Cyanob_B = 0,  Green_A = 6,  Green_B = 4)
  )
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_per_zone, lambda = m)))
  df <- as.data.frame(mat)
  df$Zone <- zn
  df
})

phyto_survey <- do.call(rbind, phyto_list)
rownames(phyto_survey) <- paste0("ST_", sprintf("%02d", seq_len(nrow(phyto_survey))))

# Organize grouping metadata column
phyto_survey <- phyto_survey[, c("Zone", setdiff(names(phyto_survey), "Zone"))]

head(phyto_survey[, 1:5], 5)

## ----cluster_dendro_vertical, fig.width = 7.5, fig.height = 5.2, fig.align = "center"----
res_dendro <- cluster_dendrogram(
  data           = phyto_survey,
  group_col      = "Zone",
  dist_method    = "bray",
  transform      = "hellinger",
  cluster_method = "average",
  k              = 3,
  palette        = "Dark2",
  orientation    = "vertical"
)

# Render publication tree
res_dendro$Plot

## ----cluster_dendro_horizontal, fig.width = 7.5, fig.height = 6.0, fig.align = "center"----
res_dendro_horiz <- cluster_dendrogram(
  data           = phyto_survey,
  group_col      = "Zone",
  dist_method    = "bray",
  transform      = "hellinger",
  cluster_method = "ward.D2",
  k              = 3,
  palette        = "Dark2",
  orientation    = "horizontal",
  title          = "Phytoplankton Assemblage Clusters (Ward.D2 Linkage)"
)

res_dendro_horiz$Plot

## ----cluster_inspection-------------------------------------------------------
# 1. Sample Cluster Membership Assignments
head(res_dendro$Cluster_Assignments, 10)

# 2. Inspect Agglomerative Cluster Merges
res_dendro$HClust_Object$merge[1:6, ]

# 3. Distance Matrix Preview (First 4 Samples)
as.matrix(res_dendro$Distance_Matrix)[1:4, 1:4]

## ----cor_data_prep------------------------------------------------------------
set.seed(1616)
n_stations <- 50

temp_c   <- rnorm(n_stations, mean = 23.5, sd = 2.8)
do_mgl   <- 13.8 - (0.32 * temp_c) + rnorm(n_stations, 0, 0.4)
ph_val   <- 7.1 + (0.05 * temp_c) + rnorm(n_stations, 0, 0.25)
turb_ntu <- runif(n_stations, 4, 38)
secchi_m <- 2.8 - (0.055 * turb_ntu) + rnorm(n_stations, 0, 0.2)
tp_mgl   <- 0.04 + (0.006 * turb_ntu) + rnorm(n_stations, 0, 0.02)
tn_mgl   <- 0.85 + (12.5 * tp_mgl) + rnorm(n_stations, 0, 0.15)

limno_env <- data.frame(
  Temperature = temp_c,
  DO          = do_mgl,
  pH          = ph_val,
  Turbidity   = turb_ntu,
  Secchi      = secchi_m,
  Total_P     = tp_mgl,
  Total_N     = tn_mgl
)

head(limno_env, 6)

## ----cor_plot_square, fig.width = 6.8, fig.height = 6.0, fig.align = "center"----
res_cor_square <- correlation_heatmap(
  data          = limno_env,
  method        = "pearson",
  shape         = "square",
  view          = "lower",
  diag          = FALSE,
  label_type    = "both",
  color_palette = "RdBu"
)

# Render lower-triangle heatmap
res_cor_square$Plot

## ----cor_plot_circle, fig.width = 6.8, fig.height = 6.0, fig.align = "center"----
res_cor_circle <- correlation_heatmap(
  data          = limno_env,
  method        = "spearman",
  shape         = "circle",
  view          = "lower",
  diag          = FALSE,
  label_type    = "stars",
  color_palette = "RdYlBu",
  title         = "Environmental Matrix Spearman Correlations"
)

res_cor_circle$Plot

## ----cor_inspection-----------------------------------------------------------
# 1. Pearson Correlation Matrix Preview
round(res_cor_square$Correlation_Matrix[1:4, 1:4], 3)

# 2. Exact P-Value Matrix Preview
signif(res_cor_square$P_Value_Matrix[1:4, 1:4], 3)

# 3. Tidy Pairwise Summary Table
head(res_cor_square$Summary_Table, 8)

## ----mantel_data_prep---------------------------------------------------------
set.seed(1717)
n_stations <- 35

# 1. Environmental Gradients
temp_c   <- rnorm(n_stations, mean = 24.0, sd = 2.5)
do_mgl   <- 13.5 - (0.30 * temp_c) + rnorm(n_stations, 0, 0.4)
ph_val   <- 7.2 + (0.04 * temp_c) + rnorm(n_stations, 0, 0.2)
turb_ntu <- runif(n_stations, 5, 40)
tp_mgl   <- 0.03 + (0.007 * turb_ntu) + rnorm(n_stations, 0, 0.015)
tn_mgl   <- 0.70 + (14.0 * tp_mgl) + rnorm(n_stations, 0, 0.12)

env_matrix <- data.frame(
  Temp     = temp_c,
  DO       = do_mgl,
  pH       = ph_val,
  Turb     = turb_ntu,
  Total_P  = tp_mgl,
  Total_N  = tn_mgl
)

# 2. Phytoplankton Community Counts (sensitive to nutrients and turbidity)
phyto_mat <- data.frame(
  Cyanob_1  = rpois(n_stations, lambda = pmax(1, round(5 + 80 * tp_mgl))),
  Cyanob_2  = rpois(n_stations, lambda = pmax(1, round(3 + 60 * tp_mgl))),
  Diatom_1  = rpois(n_stations, lambda = pmax(1, round(28 - 0.4 * turb_ntu))),
  Diatom_2  = rpois(n_stations, lambda = pmax(1, round(20 - 0.3 * turb_ntu))),
  Chloroph_1 = rpois(n_stations, lambda = pmax(1, round(12 + 2.5 * ph_val))),
  Chloroph_2 = rpois(n_stations, lambda = pmax(1, round(10 + 1.8 * ph_val)))
)

# 3. Zooplankton Community Counts (sensitive to temperature and dissolved oxygen)
zoo_mat <- data.frame(
  Cladocera_1 = rpois(n_stations, lambda = pmax(1, round(2 + 2.2 * do_mgl))),
  Cladocera_2 = rpois(n_stations, lambda = pmax(1, round(4 + 1.8 * do_mgl))),
  Copepod_1   = rpois(n_stations, lambda = pmax(1, round(8 + 0.8 * temp_c))),
  Copepod_2   = rpois(n_stations, lambda = pmax(1, round(6 + 0.6 * temp_c))),
  Rotifer_1   = rpois(n_stations, lambda = pmax(1, round(15 + 0.3 * turb_ntu)))
)

# Bundle biological communities into a named list
aquatic_trophic_comms <- list(
  "Phytoplankton" = phyto_mat,
  "Zooplankton"   = zoo_mat
)

head(env_matrix, 4)

## ----mantel_plot_network, fig.width = 8.5, fig.height = 6.2, fig.align = "center"----
res_mantel_net <- mantel_heatmap_analysis(
  comm_data        = aquatic_trophic_comms,
  env_data         = env_matrix,
  plot_type        = "network",
  line_style       = "curve",
  method           = "pearson",
  spec_dist        = "bray",
  transform        = "hellinger",
  only_significant = FALSE,
  line_color_by    = "significance",
  permutations     = 999,
  seed             = 42,
  show_cor_text    = TRUE,
  color_palette    = "RdBu"
)

# Render publication network plot
res_mantel_net$Plot

## ----mantel_plot_bars, fig.width = 8.5, fig.height = 5.2, fig.align = "center"----
res_mantel_bars <- mantel_heatmap_analysis(
  comm_data     = aquatic_trophic_comms,
  env_data      = env_matrix,
  plot_type     = "bars",
  method        = "pearson",
  spec_dist     = "bray",
  transform     = "hellinger",
  permutations  = 999,
  seed          = 42,
  color_palette = "RdBu"
)

res_mantel_bars$Plot

## ----mantel_inspection--------------------------------------------------------
# 1. Environmental Correlation Matrix (First 4 Variables)
round(res_mantel_net$Correlation_Matrix[1:4, 1:4], 3)

# 2. Tidy Mantel Test Statistics and Permutation P-Values
head(res_mantel_net$Mantel_Results, 8)

## ----diversity_data_prep------------------------------------------------------
set.seed(1818)
n_reps <- 6

zones <- c("Headwater", "Mid-River", "Lowland")
seasons <- c("Dry Season", "Wet Season")

design <- expand.grid(Zone = zones, Season = seasons)

comm_runs <- lapply(seq_len(nrow(design)), function(idx) {
  zn <- as.character(design$Zone[idx])
  sn <- as.character(design$Season[idx])
  
  # Base abundance profiles
  means <- switch(zn,
    "Headwater" = c(Sp1 = 45, Sp2 = 38, Sp3 = 12, Sp4 = 2,  Sp5 = 0,  Sp6 = 0,  Sp7 = 0,  Sp8 = 0,  Sp9 = 0,  Sp10 = 0),
    "Mid-River" = c(Sp1 = 18, Sp2 = 22, Sp3 = 25, Sp4 = 16, Sp5 = 14, Sp6 = 8,  Sp7 = 4,  Sp8 = 1,  Sp9 = 0,  Sp10 = 0),
    "Lowland"   = c(Sp1 = 6,  Sp2 = 8,  Sp3 = 12, Sp4 = 24, Sp5 = 28, Sp6 = 32, Sp7 = 18, Sp8 = 15, Sp9 = 10, Sp10 = 6)
  )
  
  if (sn == "Wet Season") {
    means <- round(means * 1.35 + 2) # Seasonal flood pulse enhancement
  }
  
  mat <- do.call(cbind, lapply(means, function(m) rpois(n_reps, lambda = m)))
  df <- as.data.frame(mat)
  df$Zone <- zn
  df$Season <- sn
  df
})

fish_comm_data <- do.call(rbind, comm_runs)
# Organize grouping metadata columns at the front
fish_comm_data <- fish_comm_data[, c("Zone", "Season", paste0("Sp", 1:10))]

head(fish_comm_data[, 1:7], 6)

## ----diversity_calc-----------------------------------------------------------
alpha_table <- calc_diversity(
  data      = fish_comm_data,
  group_col = c("Zone", "Season")
)

# Display sample output with computed diversity indices
head(alpha_table, 6)

## ----diversity_plot_single, fig.width = 6.8, fig.height = 5.0, fig.align = "center"----
res_div_single <- plot_diversity(
  data          = fish_comm_data,
  group_col     = "Zone",
  index         = "shannon",
  test          = "kruskal",
  color_palette = "Dark2",
  permutations  = 1000
)

# Render single-factor diversity plot
res_div_single$Plot

## ----diversity_plot_twofactor, fig.width = 7.5, fig.height = 5.2, fig.align = "center"----
res_div_factorial <- plot_diversity(
  data          = fish_comm_data,
  group_col     = c("Zone", "Season"),
  index         = "pielou",
  test          = "anova",
  color_palette = "Set2",
  permutations  = 1000
)

# Render factorial diversity plot
res_div_factorial$Plot

## ----diversity_inspection-----------------------------------------------------
# 1. Omnibus Hypothesis Test Result (Kruskal-Wallis / ANOVA)
res_div_single$Test_Result

# 2. Bootstrap Summary Statistics (Means, 95% Lower and Upper Confidence Limits)
res_div_single$Group_Summary

# 3. Two-Way ANOVA Factorial Summary Table
res_div_factorial$Test_Result

## ----specaccum_data_prep------------------------------------------------------
set.seed(1919)
n_samples_per_hab <- 15
habitats <- c("Headwater", "Midland", "Lowland")

sim_list <- lapply(habitats, function(hab) {
  # Diagnostic baseline probabilities of encounter
  lambdas <- switch(hab,
    "Headwater" = c(Sp01 = 25, Sp02 = 18, Sp03 = 8,  Sp04 = 2,  Sp05 = 0,  Sp06 = 0,  Sp07 = 0,  Sp08 = 0,  Sp09 = 0,  Sp10 = 0,  Sp11 = 0,  Sp12 = 0,  Sp13 = 0,  Sp14 = 0),
    "Midland"   = c(Sp01 = 8,  Sp02 = 12, Sp03 = 20, Sp04 = 15, Sp05 = 10, Sp06 = 6,  Sp07 = 3,  Sp08 = 1,  Sp09 = 0,  Sp10 = 0,  Sp11 = 0,  Sp12 = 0,  Sp13 = 0,  Sp14 = 0),
    "Lowland"   = c(Sp01 = 1,  Sp02 = 2,  Sp03 = 6,  Sp04 = 14, Sp05 = 22, Sp06 = 28, Sp07 = 18, Sp08 = 12, Sp09 = 8,  Sp10 = 6,  Sp11 = 4,  Sp12 = 3,  Sp13 = 2,  Sp14 = 1)
  )
  
  mat <- do.call(cbind, lapply(lambdas, function(l) rpois(n_samples_per_hab, lambda = l)))
  df <- as.data.frame(mat)
  df$Habitat <- hab
  df
})

fish_inventory <- do.call(rbind, sim_list)
# Place metadata at the front
fish_inventory <- fish_inventory[, c("Habitat", setdiff(names(fish_inventory), "Habitat"))]

head(fish_inventory[, 1:7], 6)

## ----specaccum_pooled, fig.width = 6.8, fig.height = 5.0, fig.align = "center"----
res_acc_pooled <- specaccum_plot(
  data       = fish_inventory,
  group_col  = "Habitat",
  by_group   = FALSE,
  method     = "exact",
  line_color = "#2A9D8F",
  ci         = 1.96,
  show_ci    = TRUE,
  title      = "River Basin Fish Species Accumulation Curve"
)

# Render pooled curve
res_acc_pooled$Plot

## ----specaccum_grouped, fig.width = 7.2, fig.height = 5.2, fig.align = "center"----
res_acc_grouped <- specaccum_plot(
  data       = fish_inventory,
  group_col  = "Habitat",
  by_group   = TRUE,
  method     = "exact",
  palette    = "Dark2",
  show_ci    = TRUE,
  title      = "Comparative Species Accumulation Across River Zones"
)

res_acc_grouped$Plot

## ----specaccum_inspection-----------------------------------------------------
# 1. Inspect accumulation curve trajectory table (First 8 sample intervals)
head(res_acc_pooled$Curve_Data, 8)

# 2. Inspect grouped curve summary table preview
head(res_acc_grouped$Curve_Data, 6)

## ----wqi_data_prep------------------------------------------------------------
set.seed(2026)
n_reps <- 6

stations <- c("Station_1 (Upstream)", "Station_2 (Agricultural)", "Station_3 (Urban Discharge)", "Station_4 (Estuarine Mouth)")

wqi_survey <- do.call(rbind, lapply(stations, function(stn) {
  # Parameter baseline values reflecting an anthropogenic pollution gradient
  param_means <- switch(stn,
    "Station_1 (Upstream)"        = c(pH = 7.4, DO = 8.8, BOD = 1.8, TDS = 140, Turbidity = 2.5),
    "Station_2 (Agricultural)"    = c(pH = 7.8, DO = 6.8, BOD = 3.6, TDS = 320, Turbidity = 4.8),
    "Station_3 (Urban Discharge)" = c(pH = 8.3, DO = 4.1, BOD = 8.5, TDS = 680, Turbidity = 9.4),
    "Station_4 (Estuarine Mouth)" = c(pH = 7.9, DO = 5.4, BOD = 5.2, TDS = 510, Turbidity = 6.2)
  )
  
  data.frame(
    Station   = stn,
    pH        = rnorm(n_reps, mean = param_means["pH"], sd = 0.2),
    DO        = rnorm(n_reps, mean = param_means["DO"], sd = 0.35),
    BOD       = rnorm(n_reps, mean = param_means["BOD"], sd = 0.4),
    TDS       = rnorm(n_reps, mean = param_means["TDS"], sd = 25),
    Turbidity = rnorm(n_reps, mean = param_means["Turbidity"], sd = 0.5)
  )
}))

head(wqi_survey, 6)

## ----wqi_calc_run-------------------------------------------------------------
res_wqi <- calc_wqi(
  data      = wqi_survey,
  id_col    = "Station",
  standards = c(
    pH        = 8.5,
    DO        = 5.0,
    BOD       = 5.0,
    TDS       = 500,
    Turbidity = 5.0
  ),
  ci        = 0.95
)

# Station-level summary table with 95% Confidence Intervals
res_wqi$Summary_Data

## ----wqi_plot_bars, fig.width = 7.5, fig.height = 5.2, fig.align = "center"----
p_wqi_bar <- plot_wqi(
  data       = res_wqi,
  id_col     = "Station",
  type       = "bar",
  show_ci    = TRUE,
  show_bands = TRUE,
  title      = "River Basin Water Quality Index (WAWQI) with 95% CI"
)

# Render publication plot
p_wqi_bar

## ----wqi_plot_lollipop, fig.width = 7.5, fig.height = 5.0, fig.align = "center"----
p_wqi_lollipop <- plot_wqi(
  data       = res_wqi,
  id_col     = "Station",
  type       = "lollipop",
  show_ci    = TRUE,
  show_bands = FALSE,
  title      = "Station-Level Water Quality Ratings"
)

p_wqi_lollipop

## ----wqi_inspection-----------------------------------------------------------
# 1. Preview individual replicate evaluations (first 6 samples)
head(res_wqi$Replicate_Data, 6)

# 2. Inspect station-level summary statistics
res_wqi$Summary_Data[, c("Station", "N", "WQI_Mean", "WQI_SE", "Status")]

## ----metal_data_prep----------------------------------------------------------
set.seed(2121)
n_reps <- 5

sites <- c("Site_1 (Forest)", "Site_2 (Mining)", "Site_3 (Smelting)", "Site_4 (Urban)", "Site_5 (Estuary)")

metal_survey <- do.call(rbind, lapply(sites, function(stn) {
  # Baseline metal concentrations (ug/L)
  means <- switch(stn,
    "Site_1 (Forest)"   = c(As = 1.2,  Cd = 0.2, Cr = 4.5,  Cu = 45,   Pb = 1.1,  Zn = 120),
    "Site_2 (Mining)"   = c(As = 18.5, Cd = 5.8, Cr = 68.0, Cu = 1850, Pb = 24.5, Zn = 4200),
    "Site_3 (Smelting)" = c(As = 26.0, Cd = 8.2, Cr = 95.0, Cu = 2600, Pb = 38.0, Zn = 5800),
    "Site_4 (Urban)"    = c(As = 6.5,  Cd = 1.4, Cr = 28.0, Cu = 420,  Pb = 8.5,  Zn = 1450),
    "Site_5 (Estuary)"  = c(As = 4.2,  Cd = 0.9, Cr = 16.5, Cu = 210,  Pb = 4.2,  Zn = 850)
  )
  
  data.frame(
    Station = stn,
    As = pmax(0.1,  rnorm(n_reps, mean = means["As"], sd = means["As"] * 0.12)),
    Cd = pmax(0.05, rnorm(n_reps, mean = means["Cd"], sd = means["Cd"] * 0.15)),
    Cr = pmax(0.5,  rnorm(n_reps, mean = means["Cr"], sd = means["Cr"] * 0.10)),
    Cu = pmax(2.0,  rnorm(n_reps, mean = means["Cu"], sd = means["Cu"] * 0.12)),
    Pb = pmax(0.1,  rnorm(n_reps, mean = means["Pb"], sd = means["Pb"] * 0.14)),
    Zn = pmax(5.0,  rnorm(n_reps, mean = means["Zn"], sd = means["Zn"] * 0.10))
  )
}))

head(metal_survey, 6)

## ----metal_calc_run-----------------------------------------------------------
res_metals <- calc_metal_indices(
  data      = metal_survey,
  id_col    = "Station",
  standards = c(
    As = 10,
    Cd = 3,
    Cr = 50,
    Cu = 2000,
    Pb = 10,
    Zn = 3000
  ),
  ci        = 0.95
)

# Station summary statistics table (Means, 95% CIs, and assigned status)
head(res_metals$Summary_Data[, c("Station", "HPI_Mean", "HPI_Status", "PLI_Mean", "PLI_Status")], 5)

## ----metal_plot_hpi, fig.width = 7.5, fig.height = 5.2, fig.align = "center"----
p_hpi <- plot_metal_indices(
  data           = res_metals,
  metric         = "HPI",
  id_col         = "Station",
  type           = "bar",
  show_ci        = TRUE,
  show_threshold = TRUE,
  palette        = "auto",
  title          = "Heavy Metal Pollution Index (HPI) Across River Stations"
)

# Render HPI plot
p_hpi

## ----metal_plot_pli, fig.width = 7.5, fig.height = 5.0, fig.align = "center"----
p_pli <- plot_metal_indices(
  data           = res_metals,
  metric         = "PLI",
  id_col         = "Station",
  type           = "lollipop",
  show_ci        = TRUE,
  show_threshold = TRUE,
  palette        = "auto",
  title          = "Pollution Load Index (PLI) Across Sampling Reaches"
)

p_pli

## ----metal_plot_cd, fig.width = 7.5, fig.height = 5.0, fig.align = "center"----
p_cd <- plot_metal_indices(
  data           = res_metals,
  metric         = "Cd",
  id_col         = "Station",
  type           = "bar",
  show_ci        = TRUE,
  show_threshold = TRUE,
  palette        = "auto"
)

p_cd

## ----metal_inspection---------------------------------------------------------
# 1. Preview replicate contamination factors and computed indices (first 4 samples)
head(res_metals$Replicate_Data[, c("Station", "CF_As", "CF_Cd", "CF_Pb", "HPI", "PLI")], 4)

# 2. Comprehensive Station Summary
res_metals$Summary_Data[, c("Station", "HEI_Mean", "HEI_Status", "Nemerow_PN_Mean", "Nemerow_Status")]

## ----summarize_data_prep------------------------------------------------------
set.seed(2222)
n_per_combo <- 10

systems <- c("Karnafuli", "Halda", "Sangu")
seasons <- c("Dry", "Wet")

survey_grid <- expand.grid(River = systems, Season = seasons)

survey_rows <- lapply(seq_len(nrow(survey_grid)), function(i) {
  rv <- as.character(survey_grid$River[i])
  sn <- as.character(survey_grid$Season[i])
  
  mean_len <- switch(rv, "Karnafuli" = 28.5, "Halda" = 34.0, "Sangu" = 22.0)
  if (sn == "Wet") mean_len <- mean_len * 1.15
  
  len <- rnorm(n_per_combo, mean = mean_len, sd = 3.2)
  # Allometric length-weight relationship: W = 0.012 * L^3.05
  wt  <- 0.012 * (len^3.05) + rnorm(n_per_combo, mean = 0, sd = 15)
  k   <- (wt / (len^3)) * 100
  
  data.frame(
    River            = rv,
    Season           = sn,
    Total_Length_cm  = pmax(10, len),
    Body_Weight_g    = pmax(20, wt),
    Condition_Factor = k,
    stringsAsFactors = FALSE
  )
})

fish_morph_survey <- do.call(rbind, survey_rows)
head(fish_morph_survey, 6)

## ----summarize_single_factor--------------------------------------------------
res_summary_river <- summarize_obs(
  data   = fish_morph_survey,
  vars   = c("Total_Length_cm", "Body_Weight_g", "Condition_Factor"),
  group  = "River",
  digits = 2
)

# Display tabular summary
res_summary_river[, c("River", "Variable", "N", "Mean_SD", "SE", "Median", "Min", "Max")]

## ----summarize_two_factor-----------------------------------------------------
res_summary_factorial <- summarize_obs(
  data   = fish_morph_survey,
  vars   = c("Total_Length_cm", "Body_Weight_g"),
  group  = c("River", "Season"),
  digits = 2
)

head(res_summary_factorial[, c("River", "Season", "Variable", "Mean_SD", "SE", "Median")], 6)

## ----summarize_inspection-----------------------------------------------------
# 1. Structure of the exported summary table
str(res_summary_river)

# 2. Inspect Mean +/- SD column for direct manuscript inclusion
res_summary_river[, c("River", "Variable", "Mean_SD")]

## ----histogram_data_prep------------------------------------------------------
set.seed(2323)
n_fish <- 250

habitats <- sample(c("Mangrove Creek", "Mudflat", "Open Channel"), size = n_fish, replace = TRUE, prob = c(0.4, 0.35, 0.25))

# Simulate bimodal cohort growth
lengths <- vapply(habitats, function(hab) {
  cohort <- rbinom(1, size = 1, prob = 0.65) # Age-0 vs Age-1 cohort
  if (hab == "Mangrove Creek") {
    if (cohort == 1) rnorm(1, 14.5, 1.8) else rnorm(1, 22.0, 2.1)
  } else if (hab == "Mudflat") {
    if (cohort == 1) rnorm(1, 18.2, 2.0) else rnorm(1, 26.5, 2.4)
  } else {
    if (cohort == 1) rnorm(1, 22.8, 2.2) else rnorm(1, 31.0, 2.8)
  }
}, numeric(1))

estuary_fish <- data.frame(
  Habitat      = factor(habitats),
  Total_Length = pmax(8.0, lengths)
)

head(estuary_fish, 6)

## ----hist_plot_single, fig.width = 7.0, fig.height = 4.8, fig.align = "center"----
p_hist_single <- plot_histogram(
  data        = estuary_fish,
  x           = "Total_Length",
  bins        = 25,
  add_density = TRUE,
  add_normal  = TRUE,
  show_stats  = TRUE,
  xlab        = "Total Length (cm)",
  ylab        = "Density",
  title       = "Fish Length-Frequency Distribution Diagnostics"
)

# Render distribution plot
p_hist_single

## ----hist_plot_faceted, fig.width = 7.5, fig.height = 4.5, fig.align = "center"----
p_hist_faceted <- plot_histogram(
  data        = estuary_fish,
  x           = "Total_Length",
  group       = "Habitat",
  facet       = TRUE,
  bins        = 20,
  add_density = TRUE,
  palette     = "Dark2",
  xlab        = "Total Length (cm)",
  title       = "Comparative Length Distributions Across Habitats"
)

p_hist_faceted

## ----hist_inspection----------------------------------------------------------
# 1. Compare central tendency benchmarks
mean_len   <- mean(estuary_fish$Total_Length)
median_len <- median(estuary_fish$Total_Length)
sd_len     <- sd(estuary_fish$Total_Length)

data.frame(
  Metric    = c("Mean (cm)", "Median (cm)", "Std. Deviation (cm)", "Skewness Offset"),
  Value     = round(c(mean_len, median_len, sd_len, mean_len - median_len), 3)
)

## ----bar_data_prep------------------------------------------------------------
set.seed(2424)
n_trips <- 90

gears   <- sample(c("Gillnet", "Longline", "Trap"), size = n_trips, replace = TRUE, prob = c(0.45, 0.35, 0.20))
sectors <- sample(c("North Sector", "Central Sector", "South Sector"), size = n_trips, replace = TRUE)

landings_kg <- vapply(seq_len(n_trips), function(i) {
  g <- gears[i]
  base <- switch(g, "Gillnet" = 120, "Longline" = 210, "Trap" = 65)
  round(pmax(15, rnorm(1, mean = base, sd = base * 0.22)), 1)
}, numeric(1))

fisheries_catch_survey <- data.frame(
  Gear_Type   = factor(gears),
  Sector      = factor(sectors),
  Landings_kg = landings_kg
)

head(fisheries_catch_survey, 6)

## ----bar_plot_dodge, fig.width = 7.5, fig.height = 5.0, fig.align = "center"----
p_bar_dodge <- plot_bar(
  data         = fisheries_catch_survey,
  x            = "Gear_Type",
  y            = "Landings_kg",
  group        = "Sector",
  stat         = "mean",
  position     = "dodge",
  show_labels  = TRUE,
  palette      = "Dark2",
  title_prefix = "Small-Scale Fisheries"
)

p_bar_dodge

## ----bar_plot_fill, fig.width = 7.0, fig.height = 4.8, fig.align = "center"----
p_bar_fill <- plot_bar(
  data         = fisheries_catch_survey,
  x            = "Sector",
  group        = "Gear_Type",
  position     = "fill",
  palette      = "Set2",
  ylab         = "Proportion of Fishing Trips",
  title_prefix = "Gear Composition"
)

p_bar_fill

## ----bar_plot_horiz, fig.width = 7.0, fig.height = 4.5, fig.align = "center"----
p_bar_horiz <- plot_bar(
  data         = fisheries_catch_survey,
  x            = "Gear_Type",
  y            = "Landings_kg",
  stat         = "sum",
  horizontal   = TRUE,
  show_labels  = TRUE,
  palette      = "Blues",
  ylab         = "Total Catch Landings (kg)"
)

p_bar_horiz

## ----lollipop_single, fig.width = 7.0, fig.height = 4.5, fig.align = "center"----
p_lollipop <- plot_lollipop(
  data         = fisheries_catch_survey,
  x            = "Gear_Type",
  y            = "Landings_kg",
  stat         = "sum",
  point_size   = 4.0,
  line_size    = 1.0,
  show_labels  = TRUE,
  palette      = "Set1",
  ylab         = "Total Catch Landings (kg)",
  title_prefix = "Lollipop Summary"
)

p_lollipop

## ----lollipop_grouped, fig.width = 7.5, fig.height = 5.0, fig.align = "center"----
p_lollipop_grouped <- plot_bar(
  data         = fisheries_catch_survey,
  x            = "Gear_Type",
  y            = "Landings_kg",
  group        = "Sector",
  type         = "lollipop",
  stat         = "mean",
  point_size   = 3.8,
  line_size    = 0.9,
  palette      = "Dark2",
  title_prefix = "Dodged Lollipop Comparison"
)

p_lollipop_grouped

## ----bar_inspection-----------------------------------------------------------
# Summary table of Mean Landings per Gear and Sector
aggregate(Landings_kg ~ Gear_Type + Sector, data = fisheries_catch_survey, FUN = function(x) round(mean(x), 2))

## ----pie3d_demo, fig.width = 6.8, fig.height = 5.0, fig.align = "center"------
catch_composition <- data.frame(
  Species     = c("Hilsa Shad", "Rohu Carp", "Catla", "Prawn", "Catfish"),
  Landings_MT = c(420, 210, 150, 95, 45)
)

p_pie <- plot_pie3d(
  data         = catch_composition,
  x            = "Species",
  y            = "Landings_MT",
  stat         = "identity",
  tilt         = 0.52,
  depth        = 0.22,
  show_labels  = TRUE,
  min_percent  = 5,
  palette      = "Set2",
  title        = "Annual Landings Composition by Target Taxa (MT)"
)

# Render 3D pie chart
p_pie

## ----circular_bar_demo, fig.width = 6.5, fig.height = 6.5, fig.align = "center"----
gear_cpue <- data.frame(
  Gear = c("Current Net", "Set Bag Net", "Longline", "Cast Net", "Gillnet", 
           "Push Net", "Drag Net", "Stake Net", "Traps", "Hook & Line"),
  CPUE = c(42.5, 38.2, 29.0, 14.8, 24.5, 11.2, 18.6, 21.4, 8.5, 16.0)
)

p_circular <- plot_circular_bar(
  data         = gear_cpue,
  x            = "Gear",
  y            = "CPUE",
  stat         = "identity",
  inner_radius = 0.35,
  show_grid    = TRUE,
  palette      = "Spectral",
  title        = "Artisanal Gear Catch per Unit Effort (kg/haul)"
)

# Render circular bar plot
p_circular

## ----donut_demo, fig.width = 7.0, fig.height = 5.2, fig.align = "center"------
hierarchical_survey <- data.frame(
  Zone    = c("Inner", "Inner", "Inner", "Outer", "Outer", "Outer"),
  Guild   = c("Carnivore", "Herbivore", "Planktivore", "Carnivore", "Herbivore", "Planktivore"),
  Biomass = c(120, 85, 145, 210, 60, 95)
)

p_donut <- plot_donut(
  data        = hierarchical_survey,
  levels      = c("Zone", "Guild"),
  value       = "Biomass",
  show_labels = TRUE,
  min_percent = 5,
  palette     = "Dark2",
  title       = "Hierarchical Estuarine Biomass (Zone > Trophic Guild)"
)

# Render nested donut chart
p_donut

## ----likert_demo, fig.width = 8.5, fig.height = 4.8, fig.align = "center"-----
set.seed(2525)
n_fishers <- 100

likert_levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

survey_perceptions <- data.frame(
  Sanctuary_Support       = sample(likert_levels, n_fishers, replace = TRUE, prob = c(0.10, 0.15, 0.10, 0.35, 0.30)),
  Seasonal_Ban_Compliance = sample(likert_levels, n_fishers, replace = TRUE, prob = c(0.08, 0.12, 0.15, 0.40, 0.25)),
  Catch_Decline_Observed  = sample(likert_levels, n_fishers, replace = TRUE, prob = c(0.05, 0.08, 0.12, 0.35, 0.40)),
  Subsidy_Sufficiency     = sample(likert_levels, n_fishers, replace = TRUE, prob = c(0.35, 0.30, 0.15, 0.12, 0.08))
)

p_likert_diverging <- plot_likert(
  data         = survey_perceptions,
  items        = c("Sanctuary_Support", "Seasonal_Ban_Compliance", "Catch_Decline_Observed", "Subsidy_Sufficiency"),
  levels       = likert_levels,
  type         = "diverging",
  palette      = "RdYlBu",
  show_labels  = TRUE,
  clean_labels = TRUE,
  title        = "Fisher Community Conservation Perception Survey (Diverging %)"
)

# Render diverging Likert plot
p_likert_diverging

## ----generate-sankey-data-----------------------------------------------------
set.seed(42)
diet_survey <- data.frame(
  Life_Stage    = factor(
    sample(c("Juvenile", "Sub-Adult", "Adult"), 200, replace = TRUE, prob = c(0.40, 0.35, 0.25)),
    levels = c("Juvenile", "Sub-Adult", "Adult")
  ),
  Feeding_Guild = factor(
    sample(c("Planktivore", "Benthivore", "Piscivore"), 200, replace = TRUE, prob = c(0.45, 0.35, 0.20)),
    levels = c("Planktivore", "Benthivore", "Piscivore")
  ),
  Prey_Taxa     = factor(
    sample(c("Cladocera", "Chironomidae", "Small Teleost", "Detritus"), 200, replace = TRUE),
    levels = c("Cladocera", "Chironomidae", "Small Teleost", "Detritus")
  ),
  Biomass_g     = round(runif(200, min = 0.5, max = 18.5), 2)
)

head(diet_survey)

## ----style-a-weighted-sankey, fig.width=8.5, fig.height=5.2-------------------
p_weighted <- plot_sankey(
  data         = diet_survey,
  axes         = c("Life_Stage", "Feeding_Guild", "Prey_Taxa"),
  value        = "Biomass_g",
  fill_axis    = "Life_Stage",
  alpha        = 0.70,
  width        = 0.28,
  palette      = "Dark2",
  title        = "Style A: Biomass-Weighted Dietary Flux Across Life History Stages"
)

print(p_weighted)

## ----style-b-count-sankey, fig.width=8.5, fig.height=5.2----------------------
p_counts <- plot_sankey(
  data         = diet_survey,
  axes         = c("Life_Stage", "Feeding_Guild", "Prey_Taxa"),
  value        = NULL,             # Automatically computes record counts
  fill_axis    = "Feeding_Guild",  # Colors ribbons by guild category
  alpha        = 0.60,
  palette      = "Set2",
  title        = "Style B: Specimen Frequency Pathways Across Trophic Strata"
)

print(p_counts)

## ----style-c-two-tier-sankey, fig.width=7.5, fig.height=4.8-------------------
# Simulate upstream-to-downstream fish passage counts
set.seed(101)
passage_data <- data.frame(
  Release_Reach = sample(c("Headwater", "Mainstem Reservoir", "Tributary"), 150, replace = TRUE),
  Recapture_Zone = sample(c("Estuary Lower", "Brackish Bay", "Nearshore Marine"), 150, replace = TRUE)
)

p_passage <- plot_sankey(
  data          = passage_data,
  axes          = c("Release_Reach", "Recapture_Zone"),
  fill_axis     = 1,
  stratum_fill  = "grey96",
  stratum_color = "black",
  label_size    = 4.0,
  palette       = "Set1",
  title         = "Style C: Two-Tier Longitudinal Catchment Migration"
)

print(p_passage)

## ----customize-sankey, fig.width=8.5, fig.height=5.2--------------------------
# Add custom axis adjustments and styling layers
p_custom <- p_weighted +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title      = ggplot2::element_text(color = "navy", face = "bold", size = 14),
    axis.title.y    = ggplot2::element_text(size = 11, face = "bold")
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

print(p_custom)

## ----export-sankey, eval=FALSE------------------------------------------------
# ggplot2::ggsave(
#   filename = "trophic_sankey_diagram.png",
#   plot     = p_custom,
#   width    = 9,
#   height   = 5.5,
#   dpi      = 300
# )

## ----timeseries_data_prep-----------------------------------------------------
set.seed(2929)
months_seq <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
stations   <- c("Upper River", "Middle Basin", "Estuary Mouth")
depths     <- c("Surface", "Bottom")
n_reps     <- 4

ts_grid <- expand.grid(Month = months_seq, Station = stations, Depth = depths)

ts_rows <- lapply(seq_len(nrow(ts_grid)), function(i) {
  m  <- as.character(ts_grid$Month[i])
  st <- as.character(ts_grid$Station[i])
  dp <- as.character(ts_grid$Depth[i])
  
  # Base seasonal oxygen curve (higher in winter, lower during summer stratification)
  m_idx <- match(m, months_seq)
  base_do <- 7.5 + 2.2 * cos((m_idx - 1) * (2 * pi / 12))
  
  # Spatial and depth adjustments
  st_offset <- switch(st, "Upper River" = 0.8, "Middle Basin" = 0.0, "Estuary Mouth" = -0.6)
  dp_offset <- if (dp == "Bottom") -1.4 else 0.0
  
  sim_do <- pmax(2.0, rnorm(n_reps, mean = base_do + st_offset + dp_offset, sd = 0.35))
  
  data.frame(
    Month   = factor(m, levels = months_seq),
    Station = st,
    Depth   = dp,
    DO_mgL  = round(sim_do, 2),
    stringsAsFactors = FALSE
  )
})

aquatic_timeseries <- do.call(rbind, ts_rows)
head(aquatic_timeseries, 8)

## ----ts_plot_single, fig.width = 7.2, fig.height = 4.8, fig.align = "center"----
res_ts_single <- plot_timeseries(
  data       = aquatic_timeseries,
  time_var   = "Month",
  y_var      = "DO_mgL",
  ribbon     = "ci",
  point_size = 2.8,
  line_width = 1.1,
  ylab       = "Dissolved Oxygen (mg/L)",
  title      = "Seasonal Dissolved Oxygen Dynamics (Mean \u00b1 95% CI)"
)

# Render single-series plot
res_ts_single$Plot

## ----ts_plot_twofactor, fig.width = 7.5, fig.height = 5.0, fig.align = "center"----
res_ts_stations <- plot_timeseries(
  data          = aquatic_timeseries,
  time_var      = "Month",
  y_var         = "DO_mgL",
  color_var     = "Station",
  ribbon        = "se",
  color_palette = "Dark2",
  point_size    = 2.4,
  line_width    = 1.0,
  ylab          = "Dissolved Oxygen (mg/L)",
  title         = "Comparative Station Dissolved Oxygen Trends"
)

res_ts_stations$Plot

## ----ts_plot_threefactor, fig.width = 8.5, fig.height = 4.5, fig.align = "center"----
res_ts_factorial <- plot_timeseries(
  data          = aquatic_timeseries,
  time_var      = "Month",
  y_var         = "DO_mgL",
  color_var     = "Depth",
  linetype_var  = "Depth",
  facet_var     = "Station",
  ribbon        = "ci",
  color_palette = "Set1",
  point_size    = 2.2,
  ylab          = "Dissolved Oxygen (mg/L)",
  title         = "Spatial and Bathymetric Dissolved Oxygen Profiles Across Seasons"
)

res_ts_factorial$Plot

## ----ts_inspection------------------------------------------------------------
# 1. Preview aggregated summary statistics table (First 6 records)
head(res_ts_factorial$Summary_Data, 6)

## ----session_info, echo = FALSE-----------------------------------------------
sessionInfo()

