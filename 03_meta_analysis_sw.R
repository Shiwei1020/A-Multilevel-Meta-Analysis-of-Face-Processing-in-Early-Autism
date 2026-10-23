# ------ Loading Libraries ------
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(esc)
library(meta)
library(metafor)
library(purrr)
library(MAd)
library(clubSandwich)

# ------ Reading File ------
df_new <- read_csv(
  "C:/Users/ZXMLAB/Desktop/meta/data_analysis/processed_dataset.csv"
)


# --------------------------------
# ------ FUNCTIONS ------
# --------------------------------

# ------ Random-effects Model ------ (book p.291)
# Three-level random-effects meta-analysis:
# -- Level 1: V = Hedge_g_var (sampling variance)
# -- Level 2: paper_index (within-study variance)
# -- Level 3: Paper_ID (between-study variance)

fit_threelevel_model <- function(data) {
  
  model <- rma.mv(
    yi = Hedge_g_updated, # The name of the column in our dataset which contains the calculated effect sizes.
    V = Hedge_g_var_updated, # The name of the column in our dataset which contains the variance of the calculated effect sizes.
    
    random = ~ 1 | Paper_ID/paper_index, # Level 3 / level 2 (cluster / effects_within_cluster)
    data = data,
    test = "t",
    method = "REML" # Recommended; Restricted maximum-likelihood
  )
  return(model)
}

# ------ I^2 statistic (Higgins and Thompson, 2002）-------
# Reference: (Assinka and Wibbelink, 2016)

calc_i2_threelevel <- function(model,
                               data,
                               variance_col = "Hedge_g_var_updated") {
  # number of effect sizes
  n <- length(data[[variance_col]])
  
  # inverse variances
  list_inverse_variances <- 1 / data[[variance_col]]
  
  sum_inverse_variances <- sum(
    list_inverse_variances,
    na.rm = TRUE
  )
  
  squared_sum_inverse_variances <-
    sum_inverse_variances^2
  
  list_inverse_variances_square <-
    1 / (data[[variance_col]]^2)
  
  sum_inverse_variances_square <- sum(
    list_inverse_variances_square,
    na.rm = TRUE
  )
  
  # estimated sampling variance
  numerator <- (n - 1) * sum_inverse_variances
  
  denominator <-
    squared_sum_inverse_variances -
    sum_inverse_variances_square
  
  estimated_sampling_variance <-
    numerator / denominator
  
  # Level 1
  I2_1 <-
    estimated_sampling_variance /
    (
      model$sigma2[1] +
        model$sigma2[2] +
        estimated_sampling_variance
    )
  
  # Level 2
  I2_2 <-
    model$sigma2[2] /
    (
      model$sigma2[1] +
        model$sigma2[2] +
        estimated_sampling_variance
    )
  
  # Level 3
  I2_3 <-
    model$sigma2[1] /
    (
      model$sigma2[1] +
        model$sigma2[2] +
        estimated_sampling_variance
    )
  
  # convert to percentages
  amount_variance_level1 <<- I2_1 * 100
  amount_variance_level2 <<- I2_2 * 100
  amount_variance_level3 <<- I2_3 * 100
  
  total_I2 <- amount_variance_level2 + amount_variance_level3
  
  return(
    list(
      amount_sampling_error_level1 =
        amount_variance_level1,
      
      amount_variance_level2 =
        amount_variance_level2,
      
      amount_variance_level3 =
        amount_variance_level3,
      
      total_I2 = total_I2 
    )
  )
}


# --------------------------------
# ------ Main Meta-Analysis ------
# --------------------------------

# --- Three-Level Random Model Effect ---
m_overall <- fit_threelevel_model(df_new)
print("========= Main effect =========")
print(m_overall)
print("=============================")

# --- I^2 ---
print("========= I^2 =========")
print(calc_i2_threelevel(m_overall, df_new))
print("=============================")

# --------------------------------
# ------ Cohort Analysis ------
# --------------------------------
m_cohort <- rma.mv(
  yi = Hedge_g_updated,
  V = Hedge_g_var_updated,
  random = ~ 1 | cohort_id/paper_index,
  data = df_new,
  test = "t",
  method = "REML"
)

print(m_cohort)

# ------ CR2 Robustness Analysis ------
# Cohort-clustered CR2 robust inference.
# The original three-level model remains unchanged:
# Level 1: sampling variance
# Level 2: paper_index
# Level 3: cohort_id
#
# CR2 adjusts the coefficient inference (SE, CI, df, p-value)
# while clustering observations by cohort_id.

m_cohort_CR2 <- robust(
  m_cohort,
  cluster = df_new$cohort_id,
  clubSandwich = TRUE
)

print("========= Cohort-clustered CR2 =========")
print(m_cohort_CR2)
print("=========================================")

# ------ Four-Level Random-Effect Model ------
# Four-level random-effects meta-analysis:
# Level 1: sampling variance
# Level 2: paper_index (effect size)
# Level 3: wave_id
# Level 4: cohort_id

m_fourlevel_wave <- rma.mv(
  yi = Hedge_g_updated,
  V = Hedge_g_var_updated,
  random = ~ 1 | cohort_id/wave_id/paper_index,
  data = df_new,
  test = "t",
  method = "REML"
)

print("========= Four-Level Model with Wave =========")
print(m_fourlevel_wave)
print("==============================================")

# ------ Three-Level vs Four-Level Comparison ------
# Compare pooled effect estimates and inferential statistics
# to examine whether adding the wave level changes the results.

model_comparison <- data.frame(
  Model = c(
    "Three-level model",
    "Four-level model with wave"
  ),
  Estimate = c(
    as.numeric(coef(m_overall)[1]),
    as.numeric(coef(m_fourlevel_wave)[1])
  ),
  SE = c(
    m_overall$se[1],
    m_fourlevel_wave$se[1]
  ),
  CI_lower = c(
    m_overall$ci.lb[1],
    m_fourlevel_wave$ci.lb[1]
  ),
  CI_upper = c(
    m_overall$ci.ub[1],
    m_fourlevel_wave$ci.ub[1]
  ),
  p_value = c(
    m_overall$pval[1],
    m_fourlevel_wave$pval[1]
  )
)

print("========= Three-Level vs Four-Level =========")
print(model_comparison)
print("==============================================")

# Change in pooled Hedges' g
g_threelevel <- as.numeric(coef(m_overall)[1])
g_fourlevel <- as.numeric(coef(m_fourlevel_wave)[1])

absolute_change <- abs(g_fourlevel - g_threelevel)
percent_change <- abs((g_fourlevel - g_threelevel) / g_threelevel) * 100

cat(
  "Absolute change in pooled Hedges' g:",
  round(absolute_change, 4),
  "\n"
)

cat(
  "Percent change in pooled Hedges' g:",
  round(percent_change, 2),
  "%\n"
)

# Variance components of the four-level model
print("========= Four-Level Variance Components =========")
print(m_fourlevel_wave$sigma2)
print("===================================================")

# --- Forest Plot ---
# Reference: https://www.metafor-project.org/doku.php/plots:forest_plot
png(
  file = "\\Users\\ZXMLAB\\Desktop\\meta\\data_analysis\\forestplot.png",
  width = 2400,
  height = 28000,
  res = 300
)

forest(
  m_overall,
  slab = df_new$paper_index,# labels on the left
  
  xlim = c(-8, 6), # overall horizontal plotting region
  alim = c(-5, 5), # range of the effect size axis
  at = seq(-5, 5, 1), # tick mark locations on the effect size axis
  
  ylim = c(-8, m_overall$k + 4),  # leave space at bottom
  
  cex = 0.38, # text size
  psize = 1.5, # larger squares
  lwd = 2, # width of confidence interval lines
  efac = 0.15, # length of CI and caps
  
  header = "Paper ID", # headers
  xlab = "Hedges' g Scale", # x-axis title
  mlab = "", # suppress default model label
  shade = FALSE,
  refline = 0 #  draw vertical reference line at g = 0
)


# Draw vertical reference line at g = 0
text(
  x = -8,
  y = -4,
  pos = 4,
  cex = 0.75,
  bquote(
    paste(
      "Three-level RE Model (Q = ",
      .(round(m_overall$QE, 2)), # Cochran's Q statisticf
      ", df = ",
      .(m_overall$k - m_overall$p), # degrees of freedom
      ", p < .001; ",
      I^2, " total = ",
      .(round(amount_variance_level2 + amount_variance_level3, 1)),
      "%)"
    )
  )
)

# Add variance decomposition across the three levels
text(
  x = -8,
  y = -6,
  pos = 4,
  cex = 0.75,
  bquote(
    paste(
      I^2, " Level 1 = ",
      .(round(amount_variance_level1, 2)), "%; ",
      I^2, " Level 2 = ",
      .(round(amount_variance_level2, 2)), "%; ",
      I^2, " Level 3 = ",
      .(round(amount_variance_level3, 2)), "%"
    )
  )
)

dev.off()

# --- Aggregated ESes ---
forest_df <- MAd::agg(
  id = Paper_ID,
  es = Hedge_g_updated,
  var = Hedge_g_var_updated,
  cor = 0.5,
  data = df_new
)

forest_model <- rma(
  yi = es,
  vi = var,
  data = forest_df,
  method = "REML"
)

print(summary(forest_model))

# --- Forest Plot (Aggregated at Study Level) ---
# One effect size per Paper_ID

png(
  file = "\\Users\\ZXMLAB\\Desktop\\meta\\data_analysis\\forestplot_aggregated.png",
  width = 1800,
  height = 3000,
  res = 300
)


forest(
  forest_model,
  
  # labels on the left
  slab = forest_df$id,
  
  xlim = c(-8, 6),
  alim = c(-5, 5),
  at = seq(-5, 5, 1),
  
  ylim = c(-3, forest_model$k + 4),
  
  cex = 0.45,
  psize = 1.5,
  lwd = 1.5,
  efac = c(0.75, 2),
  
  header = "Paper ID",
  xlab = "Hedges' g Scale",
  
  mlab = "",
  shade = FALSE,
  refline = 0
  
)

dev.off()


# --------------------------------
# ------ Publication Bias ------
# --------------------------------

# ------ Funnel Plot Function -----
# Reference: https://wviechtb.github.io/metafor/reference/funnel.html
save_funnel_plot <- function(
    model,
    file_name,
    plot_title = NULL) 
{
  png(
    file_name,
    width = 2000,
    height = 2000,
    res = 300
  )
  
  funnel(
    model,
    xlab = "Hedges' g", # title of x-axis
    ylab = "Standard Error", # title of y-axis
    pch = 21, # plotting symbol to use for the observed effect sizes or outcomes
    cex = 1.2, # points size
    cex.axis = 1.5, # x/y axis tick labels
    cex.lab = 1.5,# x/y axis titles
    col = "grey15",
    bg = "white",
    back = "grey97",
    shade = c("grey75",  # 90%
              "grey50",  # 95%
              "grey30"),  # 99%
    refline = coef(model)[1], # overall pooled effect line
    level = c(90, 95, 99) # CI region
  )
  
  if (!is.null(plot_title)) {
    title(plot_title)
  }
  
  dev.off()
}

# ------ Step 1: 'small-study' effects ------
# Step 1.1: Funnel Plot - all effects:
save_funnel_plot(
  model = m_overall,
  file_name = "\\Users\\ZXMLAB\\Desktop\\meta\\data_analysis\\funnelplot1_AllEffects.png",
  plot_title = "All Effects"
)

# Step 1.2: Egger's Regression Test:
# Reference1: (Griffin et al., 2021 - sensitivity to small-study effects)
# Reference2: (Nakagwa & Santos, 2012)

m_bias <- rma.mv(
  yi = Hedge_g_updated,
  V = Hedge_g_var_updated,
  mods = ~ Hedge_g_SE_updated, # as a predictor
  random = ~ 1 | Paper_ID/paper_index,
  data = df_new,
  method = "REML",
  test = "t"
)

print ("========== Egger's Regression Test Result ==========")
print(summary(m_bias))
print("=============================")

# ------ Step 2: Sensitivity Analysis -------

# Step 2.1: Outliers were identified using standardized level-1 residuals; Effect sizes with |z| > 3 were considered outliers.
stu <- rstandard(m_overall)
outlier_idx <- which(abs(stu$z) > 3)
print(outlier_idx)

print(paste("The total number of outliers:", length(outlier_idx)))

# Step 2.2: Main Effects after the outliers removed
# Cleaned data after removed the outliers
df_cleaned <- df_new[-outlier_idx, ]

#Main effect after the outliers removed
m_overall_cleaned <- fit_threelevel_model(df_cleaned)
print ("========== Main Effect after the Outliers Removed Result ==========")
print(m_overall_cleaned)
print("=============================")

# Step 2.3: Cleaned modified Egger Test
m_bias_cleaned <- rma.mv(
  yi = Hedge_g_updated,
  V = Hedge_g_var_updated,
  mods = ~ Hedge_g_SE, # as a predictor
  random = ~ 1 | Paper_ID/paper_index,
  data = df_cleaned,
  method = "REML",
  test = "t"
)

print ("========== Cleaned the Outliers ==========")
print(summary(m_bias_cleaned))
print("=============================")

# Step 2.4: Cleaned funnel plot
save_funnel_plot(
  model = m_overall_cleaned,
  file_name = "\\Users\\ZXMLAB\\Desktop\\meta\\data_analysis\\funnelplot2_Cleaned_AllEffects.png",
  plot_title = 'Without "small-study" Effects'
)


# --- Step 3: Overall Sensitivity Test: Leave-One-Paper-Out ---

papers <- unique(df_new$Paper_ID)

loo_results <- lapply(
  papers,
  function(paper_removed){
    df_temp <- df_new %>%
      filter(Paper_ID != paper_removed)

    m_temp <- rma.mv(
      yi = Hedge_g_updated,
      V = Hedge_g_var_updated,
      random = ~1 | Paper_ID/paper_index,
      data = df_temp,
      method = "REML",
      test = "t"
    )
    
    data.frame(
      removed_paper = paper_removed,
      n_effects = nrow(df_temp),
      estimate = as.numeric(coef(m_temp)),
      ci_lb = m_temp$ci.lb,
      ci_ub = m_temp$ci.ub,
      tau_level3 = m_temp$sigma2[1],
      tau_level2 = m_temp$sigma2[2]
    )
  }
)

# LOPO results
loo_results <- bind_rows(loo_results)
print(loo_results)

max_estimate <- loo_results %>%
  slice_max(order_by = estimate, n = 1)

min_estimate <- loo_results %>%
  slice_min(order_by = estimate, n = 1)

print("The maximum estimate is:")
print(max_estimate)

print("The minimum estimate is:")
print(min_estimate)

