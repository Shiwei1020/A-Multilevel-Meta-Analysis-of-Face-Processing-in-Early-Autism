# ------ Loading Libraries ------
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(esc)
library(meta)
library(metafor)
library(purrr)
library(ggplot2)
library(cowplot)


# ------ Reading File ------
df_new <- read_csv(
  "C:/Users/ZXMLAB/Desktop/meta/data_analysis/processed_dataset.csv"
)

# --------------------------------
# ------ FUNCTIONS ------
# --------------------------------

# ------ Main Effect ------
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

# ------ Categorical ------
cat_mod <- function(data, moderator_var) {
  
  data[[moderator_var]] <- factor(data[[moderator_var]])

  # Effect sizes in each category
  effect_table <- data %>%
    dplyr::filter(
      !is.na(.data[[moderator_var]])
    ) %>%
    dplyr::group_by(
      .data[[moderator_var]]
    ) %>%
    dplyr::summarise(
      n_effect_sizes = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      effect_size_percent = round(
        n_effect_sizes /
          sum(n_effect_sizes) * 100,
        2
      )
    )
  
  cat(
    "\nNumber and percentage of effect sizes in each category:\n"
  )
  print(effect_table)
  
  # Papers in each category
  paper_table <- data %>%
    dplyr::filter(
      !is.na(.data[[moderator_var]])
    ) %>%
    dplyr::group_by(
      .data[[moderator_var]]
    ) %>%
    dplyr::summarise(
      n_papers = dplyr::n_distinct(Paper_ID),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      paper_percent = round(
        n_papers /
          sum(n_papers) * 100,
        2
      )
    )
  
  cat(
    "\nNumber and percentage of papers in each category:\n"
  )
  print(paper_table)
  
  # Actual age range
  age_table <- data %>%
    dplyr::filter(
      !is.na(.data[[moderator_var]]),
      !is.na(Mage_ASD)
    ) %>%
    dplyr::group_by(
      .data[[moderator_var]]
    ) %>%
    dplyr::summarise(
      min_age = round(
        min(Mage_ASD, na.rm = TRUE),
        2
      ),
      max_age = round(
        max(Mage_ASD, na.rm = TRUE),
        2
      ),
      age_range = paste0(
        round(
          min(Mage_ASD, na.rm = TRUE),
          2
        ),
        " - ",
        round(
          max(Mage_ASD, na.rm = TRUE),
          2
        )
      ),
      .groups = "drop"
    )
  
  cat(
    "\nActual age range in each category:\n"
  )
  print(age_table)
  
  
  # --- Moderator model ---
  mod_reg <- rma.mv(
    yi = Hedge_g_updated,
    V = Hedge_g_var_updated,
    mods = as.formula(
      paste("~", moderator_var)
    ),
    random = ~ 1 | Paper_ID/paper_index,
    data = data,
    method = "REML",
    test = "t"
  )
  
  return(mod_reg)
}

# ------ Numerical ------
num_mod <- function(data, moderator_var) {
  mod_reg <- rma.mv(
    yi = Hedge_g_updated,
    V = Hedge_g_var_updated,
    mods = as.formula(
      paste("~", moderator_var)
    ),
    random = ~ 1 | Paper_ID/paper_index,
    data = data,
    method = "REML",
    test = "t"
  )
  
  return(mod_reg)
}

# ------ Interaction Effect ------
int_mod <- function(data, var1, var2) {
  
  # Make categorical moderator a factor
  data[[var2]] <- factor(data[[var2]])
  
  # Effect sizes in each category
  effect_table <- data %>%
    dplyr::filter(
      !is.na(.data[[var1]]),
      !is.na(.data[[var2]]),
      !is.na(Hedge_g_updated),
      !is.na(Hedge_g_var_updated)
    ) %>%
    dplyr::group_by(
      .data[[var2]]
    ) %>%
    dplyr::summarise(
      n_effect_sizes = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      effect_size_percent = round(
        n_effect_sizes /
          sum(n_effect_sizes) * 100, 2
      )
    )
  
  cat(
    "\nNumber and percentage of effect sizes in each interaction category:\n"
  )
  print(effect_table)
  
  # Papers in each category
  paper_table <- data %>%
    dplyr::filter(
      !is.na(.data[[var1]]),
      !is.na(.data[[var2]]),
      !is.na(Hedge_g_updated),
      !is.na(Hedge_g_var_updated)
    ) %>%
    dplyr::group_by(
      .data[[var2]]
    ) %>%
    dplyr::summarise(
      n_papers = dplyr::n_distinct(Paper_ID),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      paper_percent = round(
        n_papers /
          sum(n_papers) * 100,
        2
      )
    )
  
  cat(
    "\nNumber and percentage of papers in each interaction category:\n"
  )
  print(paper_table)
  
  # Actual age range
  age_table <- data %>%
    dplyr::filter(
      !is.na(.data[[var1]]),
      !is.na(.data[[var2]]),
      !is.na(Mage_ASD),
      !is.na(Hedge_g_updated),
      !is.na(Hedge_g_var_updated)
    ) %>%
    dplyr::group_by(
      .data[[var2]]
    ) %>%
    dplyr::summarise(
      min_age = round(
        min(Mage_ASD, na.rm = TRUE),
        2
      ),
      max_age = round(
        max(Mage_ASD, na.rm = TRUE),
        2
      ),
      age_range = paste0(
        round(
          min(Mage_ASD, na.rm = TRUE),
          2
        ),
        " - ",
        round(
          max(Mage_ASD, na.rm = TRUE),
          2
        )
      ),
      .groups = "drop"
    )
  
  cat(
    "\nActual age range in each interaction category:\n"
  )
  print(age_table)
  
  # Interaction model
  formula_text <- paste0(
    "~ ",
    var1,
    " * ",
    var2
  )
  
  mod_reg <- rma.mv(
    yi = Hedge_g_updated,
    V = Hedge_g_var_updated,
    mods = as.formula(formula_text),
    random = ~ 1 | Paper_ID/paper_index,
    data = data,
    method = "REML",
    test = "t"
  )
  
  return(mod_reg)
}

# ------ ANOVA: Interaction Terms Only ------
# btt = ":" selects only coefficients containing ":" (i.e., interaction terms),
# excluding the intercept and both main effects.
interaction_anova <- function(model) {
  anova(model, btt = ":")
}

# ------ Extract subgroup-specific slopes from rma.mv interaction models ------
# Output: beta, SE, p, 95% CI

get_rma_slopes <- function(model, data, moderator, age_var = "Mage_ASD_c") {
  
  # Make sure moderator uses the same factor levels as in the model
  data[[moderator]] <- factor(data[[moderator]])
  levs <- levels(data[[moderator]])
  
  b <- coef(model)
  V <- vcov(model)
  
  # Degrees of freedom
  df_use <- min(model$ddf, na.rm = TRUE)
  
  results <- lapply(levs, function(g) {
    
    # Create two rows that differ by 1 unit of age
    d0 <- data.frame(Mage_ASD_c = 0)
    d1 <- data.frame(Mage_ASD_c = 1)
    
    names(d0)[1] <- age_var
    names(d1)[1] <- age_var
    
    d0[[moderator]] <- factor(g, levels = levs)
    d1[[moderator]] <- factor(g, levels = levs)
    
    # Same design matrix used by the interaction model
    form <- as.formula(
      paste0("~ ", age_var, " * ", moderator)
    )
    
    X0 <- model.matrix(form, data = d0)
    X1 <- model.matrix(form, data = d1)
    
    # Difference = slope for this factor level
    L <- as.numeric(X1 - X0)
    names(L) <- colnames(X1)
    
    # rma.mv excludes the intercept from newmods naming,
    # but coef(model) includes "intrcpt"
    L_full <- rep(0, length(b))
    names(L_full) <- names(b)
    
    # Match intercept name
    if ("(Intercept)" %in% names(L)) {
      L["(Intercept)"] <- 0
    }
    
    # Match coefficient names
    for (nm in names(b)) {
      if (nm == "intrcpt") next
      
      if (nm %in% names(L)) {
        L_full[nm] <- L[nm]
      }
    }
    
    # Estimate slope
    beta <- sum(L_full * b)
    
    # SE using full variance-covariance matrix
    SE <- sqrt(
      as.numeric(
        t(L_full) %*% V %*% L_full
      )
    )
    
    # t-test
    t_value <- beta / SE
    
    p_value <- 2 * pt(
      abs(t_value),
      df = df_use,
      lower.tail = FALSE
    )
    
    # 95% CI
    critical_t <- qt(
      0.975,
      df = df_use
    )
    
    CI_lower <- beta - critical_t * SE
    CI_upper <- beta + critical_t * SE
    
    data.frame(
      factor_level = g,
      beta = round(beta, 4),
      SE = round(SE, 4),
      p = round(p_value, 4),
      CI_lower = round(CI_lower, 4),
      CI_upper = round(CI_upper, 4)
    )
  })
  
  bind_rows(results)
}


# ------ Centralized MAge_ASD -------
df_new$Mage_ASD_c <- as.numeric(scale(df_new$Mage_ASD, center = TRUE, scale = FALSE))


# ------------------------------------------
# ------ Individual-level Moderators ------
# ------------------------------------------

# -----------------
# ------ Age ------
# -----------------
# a. Numerical:
m_num_age <- num_mod(df_new, "Mage_ASD")
print(summary(m_num_age))

# b. Categorical:
cat_age <- cat_mod(df_new, "cat_age")
print(summary(cat_age))

# Mean Hedges' g for each age category
cat_age_mean <- df_new %>%
  group_by(cat_age) %>%
  summarise(
    k = n(),
    mean_Hedges_g = mean(Hedge_g_updated, na.rm = TRUE),
    SD = sd(Hedge_g_updated, na.rm = TRUE),
    .groups = "drop"
  )

print(cat_age_mean)

# c. Exploratory Analysis:
df_cat24 <- subset(df_new, cat_age == 24)

res_age_cat24 <- num_mod(df_cat24, "Mage_ASD")
print(summary(res_age_cat24))

# -----------------
# ------ Sex ------
# -----------------
m_num_female <- num_mod(df_new, "female_per")

print(summary(m_num_female))

# -------------------------
# ------ Risk Status ------
# -------------------------

# check the effect sizes of each factors:
df_ASD_group <- cat_mod(df_new, "ASD_group")

# --- ASD subgroup ---
df_asd <- df_new %>%
  dplyr::filter(ASD_group %in% c("hr_ASD", "ASD"))

m_asd <- fit_threelevel_model(df_asd)

# --- HR subgroup ---
df_hr <- df_new %>%
  dplyr::filter(`HR_group` == "HR")

m_hr <- fit_threelevel_model(df_hr)

# --- HR-ASD subgroup ---
df_hr_asd <- df_hr %>%
  dplyr::filter(ASD_group == "hr_ASD")

main_hr_asd <- fit_threelevel_model(df_hr_asd)

# --- HR-noASD subgroup ---
df_hr_noasd <- df_hr %>%
  dplyr::filter(ASD_group == "hr_noASD")

main_hr_noasd <- fit_threelevel_model(df_hr_noasd)

# --- HR ASD-noASD Moderation Analysis ---
df_moderator <- df_new %>%
  dplyr::filter(ASD_group %in% c("hr_ASD", "hr_noASD")) %>%
  dplyr::mutate(
    ASD_group = stats::relevel(factor(ASD_group), ref = "hr_noASD")
  )

# Reference group = HR_noASD
m_cat_asd <- cat_mod(df_moderator, "ASD_group")

# --- Age x Risk Status ---
m_int_risk <- int_mod(
  data = df_moderator,
  var1 = "Mage_ASD_c",
  var2 = "ASD_group"
)

slope_risk <- get_rma_slopes(
  model = m_int_risk,
  data = df_moderator,
  moderator = "ASD_group"
)

# --- Risk Moderators: ASD; hr_ASD; hr_noASD; hr_combined ---

df_risk <- df_new %>%
  dplyr::mutate(
    ASD_group = stats::relevel(
      factor(ASD_group),
      ref = "hr_noASD"
    )
  )

# Check reference level
levels(df_risk$ASD_group)

# Moderation analysis
m_cat_risks <- cat_mod(
  df_risk,
  "ASD_group"
)

print(summary(m_cat_risks))

# Print the summaries
# ------------------------------
print(m_asd)
print(m_hr)

print(main_hr_asd)
print(main_hr_noasd)

print(summary(m_cat_asd))
print(summary(m_int_risk))
print(interaction_anova(m_int_risk))
print(slope_risk)
# ------------------------------

# --- Additional Analysis: Risks (full) x Age ---
m_int_full_risk <- int_mod(
  data = df_risk,
  var1 = "Mage_ASD_c",
  var2 = "ASD_group"
)

slope_full_risk <- get_rma_slopes(
  model = m_int_full_risk,
  data = df_risk,
  moderator = "ASD_group"
)

print(interaction_anova(m_int_full_risk))
print(slope_full_risk)
print(summary(m_int_full_risk))



# --------------------------------------
# ------ Stimuli Level Moderators ------
# --------------------------------------

# -------------------------
# ------ Familiarity ------
# -------------------------
df_fam <- subset(df_new, fam_group != "both" & !is.na(fam_group))
m_cat_fam <- cat_mod(df_fam, "fam_group")

# --- Age x Familiarity ---
m_int_fam <- int_mod(
  data = df_fam,
  var1 = "Mage_ASD_c",
  var2 = "fam_group")

print(summary(m_cat_fam))
print(summary(m_int_fam))
print(interaction_anova(m_int_fam))

# check the effect sizes of all familiar-related factors:
m_cat_fam_all <- cat_mod(df_new, "fam_group")


# ----------------------
# ------ Dynamics ------
# ----------------------
m_cat_dyna <- cat_mod(df_new, "dynamics")

# --- Age x Dynamics ---
m_int_dyna <- int_mod(
  data = df_new,
  var1 = "Mage_ASD_c",
  var2 = "dynamics")

slope_dynamics <- get_rma_slopes(
  model = m_int_dyna,
  data = df_new,
  moderator = "dynamics"
)

print(summary(m_cat_dyna))
print(summary(m_int_dyna))
print(interaction_anova(m_int_dyna))
print(slope_dynamics)

# --------------------------------
# ------ Scenery Complexity ------
# --------------------------------

# --- Moderation Analysis ---
n_cat_scene_scene <- cat_mod(df_new, "scene_comp")

# --- Age x Scenery Complexity Interaction Analysis ---
m_int_scene <- int_mod(
  data = df_new,
  var1 = "Mage_ASD_c",
  var2 = "scene_comp")

print(summary(n_cat_scene_scene))
print(summary(m_int_scene))
print(interaction_anova(m_int_scene))

# ----------------------
# ------ Emotions ------
# ----------------------

# --- Moderation Analysis ---
n_cat_emo <- cat_mod(df_new, "emo_condition")

# --- Age x Emotions Interaction Analysis ---
m_int_emo <- int_mod(
  data = df_new,
  var1 = "Mage_ASD_c",
  var2 = "emo_condition")

slope_emotion <- get_rma_slopes(
  model = m_int_emo,
  data = df_new,
  moderator = "emo_condition"
)

print(summary(n_cat_emo))
print(summary(m_int_emo))
print(interaction_anova(m_int_emo))
print(slope_emotion)


# ---------------------------------
# ------ Presentation Format ------
# ---------------------------------

# --- Moderation Analysis ---
n_cat_pre_format <- cat_mod(df_new, "pre_format")

# --- Age x Present Format Interaction Analysis ---
m_int_present <- int_mod(
  data = df_new,
  var1 = "Mage_ASD_c",
  var2 = "pre_format")

slope_preformat <- get_rma_slopes(
  model = m_int_present,
  data = df_new,
  moderator = "pre_format"
)

print(summary(n_cat_pre_format))
print(summary(m_int_present))
print(interaction_anova(m_int_present))
print(slope_preformat)

# ----------------------------------
# ------ Auditory Information ------
# ----------------------------------

# --- Moderation Analysis ---
n_cat_visualaudio <- cat_mod(df_new, "visualaudio")

# --- Age x Auditory Information Interaction Analysis ---
m_int_auditory <- int_mod(
  data = df_new,
  var1 = "Mage_ASD_c",
  var2 = "visualaudio")

slope_visualaudio <- get_rma_slopes(
  model = m_int_auditory,
  data = df_new,
  moderator = "visualaudio"
)

print(summary(n_cat_visualaudio))
print(summary(m_int_auditory))
print(interaction_anova(m_int_auditory))
print(slope_visualaudio)

# --- Additional: VisualAudio x Features ---
df_feature <- subset(df_new, paradigms_types == "feature learning task" &
                       !is.na(visualaudio)&
                       !is.na(paradigms_types))

m_int_visual_feature <- int_mod(
  data = df_feature,
  var1 = "visualaudio",
  var2 = "features")

print(summary(m_int_visual_feature))
print(interaction_anova(m_int_visual_feature))

# # ------ Auditory Subgroup Analysis -------
# # Remove missing auditory modality values
# df_auditory <- df_new %>%
#   dplyr::filter(
#     !is.na(visualaudio)
#   ) %>%
#   dplyr::mutate(
#     visualaudio = factor(visualaudio)
#   )
# 
# # Check actual levels
# levels(df_auditory$visualaudio)
# 
# # Store the two levels
# auditory_levels <- levels(
#   df_auditory$visualaudio
# )





# ------------------------------------------
# ------ Methodology Level Moderators ------
# ------------------------------------------

# --------------------
# ------ Memory ------
# --------------------

# --- Moderation Analysis ---

m_cat_memory <- cat_mod(df_new, "memory_yn")

# --- Age x Memory Interaction Analysis ---
m_int_memory <- int_mod(
  data = df_new,
  var1 = "Mage_ASD_c",
  var2 = "memory_yn")

print(summary(m_cat_memory))
print(summary(m_int_memory))
print(interaction_anova(m_int_memory))

# -----------------------
# ------ Paradigms ------
# -----------------------
# Set face preference task as the reference category for paradigms
df_para <- df_new %>%
  dplyr::filter(!is.na(paradigms_types)) %>%
  dplyr::mutate(
    paradigms_types = stats::relevel(
      factor(paradigms_types),
      ref = "face preference task"
    )
  )

# --- Moderation Analysis ---
m_cat_para <- cat_mod(df_para, "paradigms_types")

# --- Age x Paradigms Interaction Analysis: with all paradigms ---
m_int_para <- int_mod(
  data = df_para,
  var1 = "Mage_ASD_c",
  var2 = "paradigms_types")

slope_paradigm <- get_rma_slopes(
  model = m_int_para,
  data = df_para,
  moderator = "paradigms_types"
)

print(summary(m_cat_para))
print(summary(m_int_para))
print(interaction_anova(m_int_para))
print(slope_paradigm)

# --- Paradigms: Excluding 3 Sparse-Age Tasks -----

# Exclude:
# 1. emotion-related task
# 2. habituation task
# 3. lateralization task
#
# Reference category remains:
# face preference task

df_para_4 <- df_new %>%
  dplyr::filter(
    !is.na(paradigms_types),
    !paradigms_types %in% c(
      "emotion-related task",
      "habituation task",
      "lateralization task"
    )
  ) %>%
  dplyr::mutate(
    paradigms_types = droplevels(
      stats::relevel(
        factor(paradigms_types),
        ref = "face preference task"
      )
    )
  )

cat("\nRemaining paradigm categories:\n")
print(
  table(
    df_para_4$paradigms_types,
    useNA = "ifany"))

cat("\nReference category:\n")
print(levels(df_para_4$paradigms_types)[1])

# --- Moderation Analysis ---
m_cat_para_4 <- cat_mod(
  df_para_4,
  "paradigms_types"
)

print(summary(m_cat_para_4))

# --- Interaction Analysis ---
m_int_para_4 <- int_mod(
  data = df_para_4,
  var1 = "Mage_ASD_c",
  var2 = "paradigms_types")

slope_para_4 <- get_rma_slopes(
  model = m_int_para_4,
  data = df_para_4,
  moderator = "paradigms_types"
)

print(summary(m_int_para_4))
print(interaction_anova(m_int_para_4))
print(slope_para_4)


# --- Additional: Different Face Types in FP task ---
df_fp = subset(df_new, paradigms_types == "face preference task" &
                 !is.na(paradigms_types) &
                 !is.na(fp_compare) &
                 fp_compare != "mom vs. stranger" &
                 fp_compare != "face vs. monkey"
               )

m_cat_fp = cat_mod(df_fp, "fp_compare")
print(summary(m_cat_fp))

m_int_fp <- int_mod(
  data = df_fp,
  var1 = "Mage_ASD_c",
  var2 = "fp_compare")

slope_fp <- get_rma_slopes(
  model = m_int_fp,
  data = df_fp,
  moderator = "fp_compare"
)

print(summary(m_int_fp))
print(interaction_anova(m_int_fp))
print(slope_fp)

# check the effect sizes of all familiar-related factors:
df_fp_all = subset(df_new, paradigms_types == "face preference task" &
                 !is.na(paradigms_types) &
                 !is.na(fp_compare))

m_cat_fp_all <- cat_mod(df_fp_all, "fp_compare")


# --- Additional: Feature Learning Task ---
df_feature = subset(df_new, paradigms_types == "feature learning task" &
                 !is.na(paradigms_types) &
                 !is.na(features)
)

# main effect
m_cat_feature = cat_mod(df_feature, "features")
print(summary(m_cat_feature))

# interaction analysis
m_int_feature <- int_mod(
  data = df_feature,
  var1 = "Mage_ASD_c",
  var2 = "features")

slope_feature <- get_rma_slopes(
  model = m_int_feature,
  data = df_feature,
  moderator = "features"
)

print(summary(m_int_feature))
print(interaction_anova(m_int_feature))
print(slope_feature)


# --- Additional: Emotional Task ---

# main effect
df_emotional <- df_new %>%
  dplyr::filter(
    paradigms_types == "emotion-related task"&
      !is.na(paradigms_types)&
      !is.na(types_emo)
    
  ) %>%
  dplyr::mutate(
    types_emo = stats::relevel(
      factor(types_emo),
      ref = "neutral"
    )
  )

m_cat_emotions <- cat_mod(df_emotional, "types_emo")
print(summary(m_cat_emotions))

# interaction effect
df_emotional_interaction <- df_new %>%
  dplyr::filter(
    paradigms_types == "emotion-related task"&
      !is.na(paradigms_types)&
      !is.na(types_emo)&
      types_emo != "happy"
    
  ) %>%
  dplyr::mutate(
    types_emo = stats::relevel(
      factor(types_emo),
      ref = "neutral"
    )
  )

m_int_emotions <- int_mod(
  data = df_emotional_interaction,
  var1 = "Mage_ASD_c",
  var2 = "types_emo")

slope_emotions <- get_rma_slopes(
  model = m_int_emotions,
  data = df_emotional_interaction,
  moderator = "types_emo"
)

print(summary(m_int_emotions))
print(interaction_anova(m_int_emotions))
print(slope_emotions)

# # --- Valence ---
# df_valence <- subset(
#   df_new,
#   paradigms_types == "emotion-related task" &
#     !is.na(paradigms_types) &
#     !is.na(valence) &
#     valence != "neutral"
# )
# 
# # main effect
# m_cat_valence <- cat_mod(df_valence, "valence")
# print(summary(m_cat_valence))

# --- Additional: Gaze Related Task ---
df_gaze <- subset(df_new, paradigms_types == "gaze related task" &
                    !is.na(paradigms_types) &
                    !is.na(face_feature_types)
                  )

m_cat_gaze <- cat_mod(df_gaze, "face_feature_types")
print(summary(m_cat_gaze))

m_int_gaze <- int_mod(
  data = df_gaze,
  var1 = "Mage_ASD_c",
  var2 = "face_feature_types")

slope_gaze <- get_rma_slopes(
  model = m_int_gaze,
  data = df_gaze,
  moderator = "face_feature_types"
)

print(summary(m_int_gaze))
print(interaction_anova(m_int_gaze))
print(slope_gaze)

# -----------------------
# ------ Modality ------
# -----------------------

df_equip <- subset(df_new, equip_types != "fNIRS" & !is.na(equip_types))

# --- Moderation Analysis ---
m_cat_equip <- cat_mod(df_equip, "equip_types")

# --- Age x Modality Interaction Analysis ---
m_int_modality <- int_mod(
  data = df_equip,
  var1 = "Mage_ASD_c",
  var2 = "equip_types")

slope_modality <- get_rma_slopes(
  model = m_int_modality,
  data = df_equip,
  moderator = "equip_types"
)

print(summary(m_cat_equip))
print(summary(m_int_modality))
print(interaction_anova(m_int_modality))
print(slope_modality)

# check the effect sizes of all factors:
m_cat_equip_all <- cat_mod(df_new, "equip_types")

# --- Additional: EEG Task Analysis ---
df_eeg_amp = subset(df_new, DV_types == "ERP amplitude" &
                      !is.na(DV_types))

df_eeg_lat = subset(df_new, DV_types == "ERP latency" &
                      !is.na(DV_types))

m_amp <- fit_threelevel_model(df_eeg_amp)
m_lat <- fit_threelevel_model(df_eeg_lat)

print(m_amp)
print(m_lat)




