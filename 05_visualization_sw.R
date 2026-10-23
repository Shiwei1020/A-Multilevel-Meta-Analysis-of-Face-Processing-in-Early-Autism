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
# ------ Centralized MAge_ASD -------
df_new$Mage_ASD_c <- as.numeric(scale(df_new$Mage_ASD, center = TRUE, scale = FALSE))

# -------------------
# ------ Plots ------
# -------------------

# --------------------------------------------------------
# --- Age x 2-Group Interaction: Actual Age on X-axis ---
# --- CI + Predicted Lines only --------------------------
# --------------------------------------------------------

plot_age_interaction_actualx <- function(
    model,
    data,
    moderator_var,
    group_levels,
    age_var_model = "Mage_ASD_c",
    age_var_plot = "Mage_ASD",
    x_label = "Mean Age (months)",
    y_label = "Predicted Hedges' g",
    legend_label = moderator_var,
    legend_labels = NULL,
    x_limits = NULL,
    x_breaks = NULL,
    y_limits = NULL,
    y_breaks = NULL) {
  
  
  # -------------------------------
  # Check group levels
  # -------------------------------
  
  if (length(group_levels) != 2) {
    stop(
      "group_levels must contain exactly 2 groups."
    )
  }
  
  
  if (is.null(legend_labels)) {
    legend_labels <- group_levels
  }
  
  
  # -------------------------------
  # Prepare plotting data
  # -------------------------------
  
  plot_data <- data %>%
    dplyr::filter(
      !is.na(.data[[age_var_model]]),
      !is.na(.data[[age_var_plot]]),
      !is.na(.data[[moderator_var]]),
      !is.na(Hedge_g_updated),
      .data[[moderator_var]] %in% group_levels
    ) %>%
    dplyr::mutate(
      !!moderator_var := factor(
        .data[[moderator_var]],
        levels = group_levels
      )
    )
  
  
  # -------------------------------
  # Colors
  # -------------------------------
  
  group_colors <- c(
    "#1E76BC",
    "#D9A441"
  )
  
  names(group_colors) <- group_levels
  
  
  # -------------------------------
  # Centering value
  # -------------------------------
  
  center_value <- mean(
    plot_data[[age_var_plot]] -
      plot_data[[age_var_model]],
    na.rm = TRUE
  )
  
  
  # -------------------------------
  # Build prediction data
  # -------------------------------
  
  prediction_list <- lapply(
    group_levels,
    function(g) {
      
      group_data <- plot_data %>%
        dplyr::filter(
          .data[[moderator_var]] == g
        )
      
      age_plot_min <- min(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      age_plot_max <- max(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      new_group <- data.frame(
        age_plot_seq = seq(
          age_plot_min,
          age_plot_max,
          length.out = 100
        )
      )
      
      names(new_group)[1] <- age_var_plot
      
      # Convert actual age back to centered age
      new_group[[age_var_model]] <-
        new_group[[age_var_plot]] - center_value
      
      new_group[[moderator_var]] <- factor(
        g,
        levels = group_levels
      )
      
      new_group
    }
  )
  
  newdata <- dplyr::bind_rows(
    prediction_list
  )
  
  
  # -------------------------------
  # Model matrix uses centered age
  # -------------------------------
  
  interaction_formula <- as.formula(
    paste0(
      "~ ",
      age_var_model,
      " * ",
      moderator_var
    )
  )
  
  X <- model.matrix(
    interaction_formula,
    data = newdata
  )
  
  
  # -------------------------------
  # Predictions from ORIGINAL model
  # -------------------------------
  
  pred <- predict(
    model,
    newmods = X[, -1, drop = FALSE]
  )
  
  newdata$pred  <- pred$pred
  newdata$ci_lb <- pred$ci.lb
  newdata$ci_ub <- pred$ci.ub
  
  
  # -------------------------------
  # Plot
  # -------------------------------
  
  p <- ggplot() +
    
    # CI ribbons
    geom_ribbon(
      data = newdata,
      aes(
        x = .data[[age_var_plot]],
        ymin = ci_lb,
        ymax = ci_ub,
        fill = .data[[moderator_var]],
        group = .data[[moderator_var]]
      ),
      alpha = 0.08,
      colour = NA
    ) +
    
    # Predicted lines from centered-age interaction model
    geom_line(
      data = newdata,
      aes(
        x = .data[[age_var_plot]],
        y = pred,
        colour = .data[[moderator_var]],
        group = .data[[moderator_var]]
      ),
      linewidth = 1
    ) +
    
    scale_colour_manual(
      values = group_colors,
      breaks = group_levels,
      labels = legend_labels,
      name = legend_label,
      drop = FALSE
    ) +
    
    scale_fill_manual(
      values = group_colors,
      breaks = group_levels,
      labels = legend_labels,
      guide = "none",
      drop = FALSE
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_classic() +
    
    theme(
      legend.position = "right",
      
      legend.title = element_text(
        size = 2,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 2
      ),
      
      axis.title = element_text(
        size = 10
      ),
      
      axis.text = element_text(
        size = 10,
        colour = "black"
      ),
      
      axis.line = element_line(
        linewidth = 0.5
      ),
      
      axis.ticks = element_line(
        linewidth = 0.4
      )
    )
  
  
  # -------------------------------
  # Axis settings
  # -------------------------------
  
  if (!is.null(x_breaks)) {
    p <- p +
      scale_x_continuous(
        breaks = x_breaks
      )
  }
  
  if (!is.null(y_breaks)) {
    p <- p +
      scale_y_continuous(
        breaks = y_breaks
      )
  }
  
  p <- p +
    coord_cartesian(
      xlim = x_limits,
      ylim = y_limits
    )
  
  return(p)
}

# ------ Subgroup Bar Plot ------
plot_subgroup_bar <- function(models,
                              labels,
                              title,
                              x_label = "Pooled Hedges' g") {
  
  # Check input
  if (length(models) != length(labels)) {
    stop("The number of models must equal the number of labels.")
  }
  
  # Extract pooled effect size and 95% CI
  plot_data <- data.frame(
    Group = factor(
      labels,
      levels = rev(labels)
    ),
    
    Estimate = sapply(
      models,
      function(x) as.numeric(x$b[1])
    ),
    
    CI_lb = sapply(
      models,
      function(x) as.numeric(x$ci.lb[1])
    ),
    
    CI_ub = sapply(
      models,
      function(x) as.numeric(x$ci.ub[1])
    )
  )
  
  # Keep same colors as previous version
  bar_colors <- setNames(
    rep(
      c(
        "#1E76BC",  # blue
        "#D9A441"   # yellow
      ),
      length.out = length(labels)
    ),
    labels
  )
  
  ggplot(
    plot_data,
    aes(
      y = Group,
      x = Estimate,
      fill = Group
    )
  ) +
    
    geom_col(
      width = 0.45,
      colour = NA
    ) +
    
    geom_errorbar(
      aes(
        xmin = CI_lb,
        xmax = CI_ub
      ),
      orientation = "y",
      width = 0.12,
      linewidth = 0.7
    ) +
    
    geom_vline(
      xintercept = 0,
      colour = "grey40",
      linewidth = 0.6
    ) +
    
    scale_fill_manual(
      values = bar_colors
    ) +
    
    scale_x_continuous(
      breaks = seq(-1, 0.5, by = 0.5)
    ) +
    
    coord_cartesian(
      xlim = c(-1, 0.5)
    ) +
    
    labs(
      title = title,
      x = x_label,
      y = NULL
    ) +
    
    theme_classic() +
    
    theme(
      legend.position = "none",
      
      plot.title = element_text(
        size = 10,
        face = "bold",
        hjust = 0.5
      ),
      
      axis.text.y = element_text(
        size = 10,
        face = "bold",
        colour = "black"
      ),
      
      axis.text.x = element_text(
        size = 7,
        colour = "black"
      ),
      
      axis.title.x = element_text(
        size = 7,
        face = "bold"
      ),
      
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )
}


# ---------------------------------
# --- Age x Dynamics --------------
# ---------------------------------

dynamics_levels <- levels(
  factor(df_new$dynamics)
)

p_int1 <- plot_age_interaction_actualx(
  model = m_int_dyna,
  data = df_new,
  moderator_var = "dynamics",
  group_levels = dynamics_levels,
  
  legend_label = "Stimulus Dynamics",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)

ggsave(
  "age_dynamics_interaction_actual_age_CI.png",
  plot = p_int1,
  width = 5,
  height = 3.5,
  dpi = 300
)


# ---------------------------------
# --- Age x Emotions vs. Neutral --
# ---------------------------------

emo_levels <- levels(
  factor(df_new$emo_condition)
)

p_int2 <- plot_age_interaction_actualx(
  model = m_int_emo,
  data = df_new,
  moderator_var = "emo_condition",
  group_levels = emo_levels,
  
  legend_label = "Emotional Conditions",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)

ggsave(
  "age_emo_conditions_interaction_actual_age_CI.png",
  plot = p_int2,
  width = 5,
  height = 3.5,
  dpi = 300
)


# ---------------------------------
# --- Age x Presentation Format ---
# ---------------------------------

presentation_levels <- levels(
  factor(df_new$pre_format)
)

p_int3 <- plot_age_interaction_actualx(
  model = m_int_present,
  data = df_new,
  moderator_var = "pre_format",
  group_levels = presentation_levels,
  
  legend_label = "Presentation Format",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)

ggsave(
  "age_present_format_interaction_actual_age_CI.png",
  plot = p_int3,
  width = 5,
  height = 3.5,
  dpi = 300
)


# ---------------------------------
# --- Age x Auditory Information --
# ---------------------------------

auditory_levels <- c(
  "visual",
  "visualaudio"
)

p_int4 <- plot_age_interaction_actualx(
  model = m_int_auditory,
  data = df_new,
  moderator_var = "visualaudio",
  group_levels = auditory_levels,
  
  legend_label = "Presentation Modality",
  
  legend_labels = c(
    "Visual Only",
    "Visual with Audio"
  ),
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)

ggsave(
  "age_auditory_interaction_actual_age_CI.png",
  plot = p_int4,
  width = 5,
  height = 3.5,
  dpi = 300
)


# ---------------------------------
# --- Age x HR-ASD / HR-noASD ----
# ---------------------------------

risk_levels <- levels(
  factor(df_moderator$ASD_group)
)

p_int6 <- plot_age_interaction_actualx(
  model = m_int_risk,
  data = df_moderator,
  moderator_var = "ASD_group",
  group_levels = risk_levels,
  
  legend_label = "Risk Outcome",
  
  legend_labels = c(
    "HR_noASD",
    "HR_ASD"
  ),
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)

ggsave(
  "age_risk_outcomes_interaction_actual_age_CI.png",
  plot = p_int6,
  width = 5,
  height = 3.5,
  dpi = 300
)


# ---------------------------------
# ------ Subgroup Bar Plots -------
# ---------------------------------

# --- ASD vs HR ---
p_bar1 <- plot_subgroup_bar(
  
  models = list(
    m_asd,
    m_hr
  ),
  
  labels = c(
    "ASD",
    "HR"
  ),
  
  title = "ASD vs HR"
)

ggsave(
  "ASD_HR_subgroup_comparison.png",
  plot = p_bar1,
  width = 5,
  height = 3.5,
  dpi = 300
)


# --- HR-ASD vs HR-noASD ---
# ------ Risk Outcome Subgroups ---
p_bar_risk <- plot_subgroup_bar(
  
  models = list(
    main_hr_asd,
    main_hr_noasd
  ),
  
  labels = c(
    "HR_ASD",
    "HR_noASD"
  ),
  
  title = "Risk Outcome"
)

ggsave(
  "risk_outcome_subgroup_comparison.png",
  plot = p_bar_risk,
  width = 5,
  height = 3.5,
  dpi = 300
)

# ---------------------------------
# --- Auditory Info Subgroups -----
# ---------------------------------
# --- Visual Only subgroup ---
unique(df_new$visualaudio)

df_visual_only <- df_auditory %>%
  dplyr::filter(
    visualaudio == "visual"
  )

main_visual_only <- fit_threelevel_model(
  df_visual_only
)


# --- Visual with Audio subgroup ---
df_visual_audio <- df_auditory %>%
  dplyr::filter(
    visualaudio == "visualaudio"
  )

main_visual_audio <- fit_threelevel_model(
  df_visual_audio
)

p_bar_auditory <- plot_subgroup_bar(
  
  models = list(
    main_visual_only,
    main_visual_audio
  ),
  
  labels = c(
    "Visual Only",
    "Visual with Audio"
  ),
  
  title = "Auditory Information"
)

ggsave(
  "auditory_subgroup_comparison.png",
  plot = p_bar_auditory,
  width = 5,
  height = 3.5,
  dpi = 300
)


# ---------------------------------
# ------ Familiarity Subgroups ----
# ---------------------------------
df_familiar <- df_fam %>%
  dplyr::filter(
    fam_group == "familiar"
  )

df_unfamiliar <- df_fam %>%
  dplyr::filter(
    fam_group == "unfamiliar"
  )


main_familiar <- fit_threelevel_model(
  df_familiar
)

main_unfamiliar <- fit_threelevel_model(
  df_unfamiliar
)


print(main_familiar)
print(main_unfamiliar)

p_bar_familiarity <- plot_subgroup_bar(
  
  models = list(
    main_familiar,
    main_unfamiliar
  ),
  
  labels = c(
    "Familiar",
    "Unfamiliar"
  ),
  
  title = "Familiarity"
)

ggsave(
  "familiarity_subgroup_comparison.png",
  plot = p_bar_familiarity,
  width = 5,
  height = 3.5,
  dpi = 300
)

# ---------------------------------
# ------ Dynamics Subgroups -------
# ---------------------------------

df_dynamic <- df_new %>%
  dplyr::filter(
    dynamics == "dynamic"
  )

df_static <- df_new %>%
  dplyr::filter(
    dynamics == "static"
  )


main_dynamic <- fit_threelevel_model(
  df_dynamic
)

main_static <- fit_threelevel_model(
  df_static
)


print(main_dynamic)
print(main_static)

p_bar_dynamics <- plot_subgroup_bar(
  
  models = list(
    main_dynamic,
    main_static
  ),
  
  labels = c(
    "Dynamic",
    "Static"
  ),
  
  title = "Dynamics"
)

ggsave(
  "dynamics_subgroup_comparison.png",
  plot = p_bar_dynamics,
  width = 5,
  height = 3.5,
  dpi = 300
)

# ---------------------------------
# --- Scene Complexity Subgroups --
# ---------------------------------

df_scene_complex <- df_new %>%
  dplyr::filter(
    scene_comp == "complex"
  )

df_face_only <- df_new %>%
  dplyr::filter(
    scene_comp == "face only"
  )


main_scene_complex <- fit_threelevel_model(
  df_scene_complex
)

main_face_only <- fit_threelevel_model(
  df_face_only
)


print(main_scene_complex)
print(main_face_only)

p_bar_scene <- plot_subgroup_bar(
  
  models = list(
    main_scene_complex,
    main_face_only
  ),
  
  labels = c(
    "Complex",
    "Face Only"
  ),
  
  title = "Scene Complexity"
)

ggsave(
  "scene_complexity_subgroup_comparison.png",
  plot = p_bar_scene,
  width = 5,
  height = 3.5,
  dpi = 300
)

# ---------------------------------
# ------ Memory Subgroups ---------
# ---------------------------------

df_no_memory <- df_new %>%
  dplyr::filter(
    memory_yn == "no"
  )

df_with_memory <- df_new %>%
  dplyr::filter(
    memory_yn == "yes"
  )


main_no_memory <- fit_threelevel_model(
  df_no_memory
)

main_with_memory <- fit_threelevel_model(
  df_with_memory
)


print(main_no_memory)
print(main_with_memory)

p_bar_memory <- plot_subgroup_bar(
  
  models = list(
    main_no_memory,
    main_with_memory
  ),
  
  labels = c(
    "No Memory",
    "With Memory"
  ),
  
  title = "Memory"
)

ggsave(
  "memory_subgroup_comparison.png",
  plot = p_bar_memory,
  width = 5,
  height = 3.5,
  dpi = 300
)

# ---------------------------------
# ------ Modality Subgroups -------
# ---------------------------------

df_behavior <- df_equip %>%
  dplyr::filter(
    equip_types == "behavior"
  )

df_EEG <- df_equip %>%
  dplyr::filter(
    equip_types == "EEG"
  )

df_eye_tracking <- df_equip %>%
  dplyr::filter(
    equip_types == "eye-tracking"
  )


main_behavior <- fit_threelevel_model(
  df_behavior
)

main_EEG <- fit_threelevel_model(
  df_EEG
)

main_eye_tracking <- fit_threelevel_model(
  df_eye_tracking
)


print(main_behavior)
print(main_EEG)
print(main_eye_tracking)

p_bar_modality <- plot_subgroup_bar(
  
  models = list(
    main_behavior,
    main_EEG,
    main_eye_tracking
  ),
  
  labels = c(
    "Behavioral Recording",
    "EEG",
    "Eye-tracking"
  ),
  
  title = "Modality"
)

ggsave(
  "modality_subgroup_comparison.png",
  plot = p_bar_modality,
  width = 5,
  height = 4,
  dpi = 300
)


# ---------------------------------
# --- Additional Task: EEG Amplitude vs Latency ---
# ---------------------------------

# Keep EEG effect sizes only
df_eeg_dv <- df_new %>%
  dplyr::filter(
    equip_types == "EEG",
    DV_types %in% c(
      "ERP amplitude",
      "ERP latency"
    )
  )


# --- ERP Amplitude subgroup ---
df_eeg_amplitude <- df_eeg_dv %>%
  dplyr::filter(
    DV_types == "ERP amplitude"
  )

main_eeg_amplitude <- fit_threelevel_model(
  df_eeg_amplitude
)


# --- ERP Latency subgroup ---
df_eeg_latency <- df_eeg_dv %>%
  dplyr::filter(
    DV_types == "ERP latency"
  )

main_eeg_latency <- fit_threelevel_model(
  df_eeg_latency
)


# --- Check subgroup results ---
print(main_eeg_amplitude)
print(main_eeg_latency)


# --- Bar plot ---
p_bar_eeg_dv <- plot_subgroup_bar(
  
  models = list(
    main_eeg_amplitude,
    main_eeg_latency
  ),
  
  labels = c(
    "ERP Amplitude",
    "ERP Latency"
  ),
  
  title = "EEG Measures"
)


# --- Save plot ---
ggsave(
  "EEG_amplitude_latency_subgroup_comparison.png",
  plot = p_bar_eeg_dv,
  width = 5,
  height = 3.5,
  dpi = 300
)




# ---------------------------------
# ------ Paradigm Subgroups -------
# ---------------------------------

df_face_preference <- df_para %>%
  dplyr::filter(
    paradigms_types == "face preference task"
  )

df_emotion_related <- df_para %>%
  dplyr::filter(
    paradigms_types == "emotion-related task"
  )

df_feature_learning <- df_para %>%
  dplyr::filter(
    paradigms_types == "feature learning task"
  )

df_free_face_learning <- df_para %>%
  dplyr::filter(
    paradigms_types == "free face learning task"
  )

df_gaze_following <- df_para %>%
  dplyr::filter(
    paradigms_types == "gaze related task"
  )

df_habituation <- df_para %>%
  dplyr::filter(
    paradigms_types == "habituation task"
  )

df_lateralization <- df_para %>%
  dplyr::filter(
    paradigms_types == "lateralization task"
  )


# ------ Fit pooled subgroup models ------

main_face_preference <- fit_threelevel_model(
  df_face_preference
)

main_emotion_related <- fit_threelevel_model(
  df_emotion_related
)

main_feature_learning <- fit_threelevel_model(
  df_feature_learning
)

main_free_face_learning <- fit_threelevel_model(
  df_free_face_learning
)

main_gaze_following <- fit_threelevel_model(
  df_gaze_following
)

main_habituation <- fit_threelevel_model(
  df_habituation
)

main_lateralization <- fit_threelevel_model(
  df_lateralization
)


# ------ Check results ------

print(main_face_preference)
print(main_emotion_related)
print(main_feature_learning)
print(main_free_face_learning)
print(main_gaze_following)
print(main_habituation)
print(main_lateralization)

p_bar_paradigms <- plot_subgroup_bar(
  
  models = list(
    main_face_preference,
    main_emotion_related,
    main_feature_learning,
    main_free_face_learning,
    main_gaze_following,
    main_habituation,
    main_lateralization
  ),
  
  labels = c(
    "Face Preference",
    "Emotion-related",
    "Feature Learning",
    "Free Face Learning",
    "Gaze Following",
    "Habituation",
    "Lateralization"
  ),
  
  title = "Paradigms"
)

ggsave(
  "paradigms_subgroup_comparison.png",
  plot = p_bar_paradigms,
  width = 5.5,
  height = 5,
  dpi = 300
)


# ---------------------------------
# --- Bubble Plot: Main Effects ---
# ---------------------------------

plot_main_bubble <- function(model,
                             data,
                             var,
                             x_label = var,
                             y_label = "Hedges' g",
                             x_round = NULL,
                             y_round = 2,
                             x_limits = NULL,
                             x_breaks = NULL,
                             y_limits = NULL,
                             y_breaks = NULL) {
  
  # Keep complete cases
  plot_data <- data %>%
    dplyr::filter(
      !is.na(.data[[var]]),
      !is.na(Hedge_g_updated)
    ) %>%
    dplyr::mutate(
      x_raw = .data[[var]],
      y_raw = Hedge_g_updated
    )
  
  # Round values only for bubble aggregation
  if (is.null(x_round)) {
    plot_data <- plot_data %>%
      dplyr::mutate(
        x_plot = x_raw,
        y_plot = round(y_raw, y_round)
      )
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(
        x_plot = round(x_raw, x_round),
        y_plot = round(y_raw, y_round)
      )
  }
  
  # Count overlapping points
  bubble_data <- plot_data %>%
    dplyr::count(
      x_plot,
      y_plot,
      name = "count"
    )
  
  # Prediction data for regression line
  newdata <- data.frame(
    x = seq(
      min(plot_data$x_raw, na.rm = TRUE),
      max(plot_data$x_raw, na.rm = TRUE),
      length.out = 100
    )
  )
  
  names(newdata) <- var
  
  X <- model.matrix(
    as.formula(paste0("~ ", var)),
    data = newdata
  )
  
  pred <- predict(
    model,
    newmods = X[, -1, drop = FALSE]
  )
  
  newdata$pred  <- pred$pred
  newdata$ci_lb <- pred$ci.lb
  newdata$ci_ub <- pred$ci.ub
  
  
  p <- ggplot() +
    
    # Horizontal reference line at 0
    geom_hline(
      yintercept = 0,
      colour = "grey70",
      linewidth = 0.25
    ) +
    
    # Bubble points
    geom_point(
      data = bubble_data,
      aes(
        x = x_plot,
        y = y_plot,
        size = count
      ),
      shape = 21,
      fill = NA,
      colour = "#1E76BC",
      stroke = 0.6,
      alpha = 0.7
    ) +
    
    # Regression CI
    geom_ribbon(
      data = newdata,
      aes(
        x = .data[[var]],
        ymin = ci_lb,
        ymax = ci_ub
      ),
      inherit.aes = FALSE,
      fill = "#1E76BC",
      alpha = 0.12
    ) +
    
    # Regression line
    geom_line(
      data = newdata,
      aes(
        x = .data[[var]],
        y = pred
      ),
      inherit.aes = FALSE,
      colour = "#1E76BC",
      linewidth = 0.9
    ) +
    
    scale_size(
      range = c(1.5, 6),
      breaks = sort(unique(bubble_data$count)),
      name = "Count"
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_classic() +
    
    theme(
      legend.position = "right",
      
      legend.title = element_text(
        size = 10,
        face = "bold"
      ),
      
      legend.text = element_text(size = 9),
      
      axis.title = element_text(size = 10),
      
      axis.text = element_text(
        size = 9,
        colour = "black"
      )
    )
  
  
  # Add x-axis settings only if supplied
  if (!is.null(x_breaks)) {
    p <- p +
      scale_x_continuous(
        breaks = x_breaks
      )
  }
  
  # Add y-axis settings only if supplied
  if (!is.null(y_breaks)) {
    p <- p +
      scale_y_continuous(
        breaks = y_breaks
      )
  }
  
  # Add plotting limits
  p <- p +
    coord_cartesian(
      xlim = x_limits,
      ylim = y_limits
    )
  
  return(p)
}

# ---------------------------------
# ------ Create df_cleaned --------
# ---------------------------------

# Standardized level-1 residuals
m_overall <- fit_threelevel_model(df_new)
stu <- rstandard(m_overall)

# Identify outliers: |z| > 3
outlier_idx <- which(
  abs(stu$z) > 3
)

# Check number of outliers
print(outlier_idx)

cat(
  "The total number of outliers:",
  length(outlier_idx),
  "\n"
)

# Remove outliers
df_cleaned <- df_new[
  -outlier_idx,
]

# Check cleaned dataset
cat(
  "Original number of effect sizes:",
  nrow(df_new),
  "\n"
)

cat(
  "Cleaned number of effect sizes:",
  nrow(df_cleaned),
  "\n"
)

# ---------------------------------
# ------ Age Main Effect Plot -----
# ---------------------------------

p_age_bubble <- plot_main_bubble(
  model = m_num_age,
  data = df_cleaned,
  var = "Mage_ASD",
  x_label = "Mean Age",
  y_label = "Hedges' g",
  
  x_limits = c(0, 25),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_main_effect_bubble.png",
  plot = p_age_bubble,
  width = 5,
  height = 4,
  dpi = 300
)


# ---------------------------------
# -- Female Percentage Main Plot --
# ---------------------------------

p_female_bubble <- plot_main_bubble(
  model = m_num_female,
  data = df_new,
  var = "female_per",
  x_label = "Female Percentage",
  y_label = "Hedges' g",
  
  x_limits = c(0, 0.8),
  x_breaks = seq(0, 0.8, by = 0.2),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "female_percentage_main_effect_bubble.png",
  plot = p_female_bubble,
  width = 5,
  height = 4,
  dpi = 300
)


# --------------------------------------------------------
# --- Age x Paradigms: Plot actual age, model centered ---
# --------------------------------------------------------

plot_age_paradigms_actualx <- function(
    model,
    data,
    age_var_model = "Mage_ASD_c",
    age_var_plot = "Mage_ASD",
    paradigm_var = "paradigms_types",
    x_label = "Mean Age (months)",
    y_label = "Hedges' g",
    legend_label = "Paradigms",
    x_limits = NULL,
    x_breaks = NULL,
    y_limits = NULL,
    y_breaks = NULL) {
  
  
  # -------------------------------
  # Fixed paradigm order
  # -------------------------------
  
  paradigm_levels <- c(
    "face preference task",
    "feature learning task",
    "free face learning task",
    "gaze related task"
  )
  
  
  # -------------------------------
  # Prepare data
  # -------------------------------
  
  plot_data <- data %>%
    dplyr::filter(
      !is.na(.data[[age_var_model]]),
      !is.na(.data[[age_var_plot]]),
      !is.na(.data[[paradigm_var]]),
      !is.na(Hedge_g_updated),
      .data[[paradigm_var]] %in% paradigm_levels
    ) %>%
    dplyr::mutate(
      !!paradigm_var := factor(
        .data[[paradigm_var]],
        levels = paradigm_levels
      )
    )
  
  
  # -------------------------------
  # Fixed colors
  # -------------------------------
  
  paradigm_colors <- c(
    "face preference task" = "#C66DB7",
    "feature learning task" = "#F2C14E",
    "free face learning task" = "#53A6D3",
    "gaze related task" = "#57B77A"
  )
  
  # -------------------------------
  # Fixed labels
  # -------------------------------
  
  paradigm_labels <- c(
    "face preference task" = "Face Preference",
    "feature learning task" = "Feature Learning",
    "free face learning task" = "Free Face Learning",
    "gaze related task" = "Gaze Following"
  )
  
  
  # -------------------------------
  # Build prediction data
  # IMPORTANT:
  # x-axis shown as actual age (Mage_ASD),
  # but prediction uses Mage_ASD_c
  # -------------------------------
  
  prediction_list <- lapply(
    paradigm_levels,
    function(g) {
      
      group_data <- plot_data %>%
        dplyr::filter(
          .data[[paradigm_var]] == g
        )
      
      age_plot_min <- min(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      age_plot_max <- max(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      new_group <- data.frame(
        age_plot_seq = seq(
          age_plot_min,
          age_plot_max,
          length.out = 100
        )
      )
      
      names(new_group)[1] <- age_var_plot
      
      # convert actual age to centered age
      center_value <- mean(
        plot_data[[age_var_plot]] - plot_data[[age_var_model]],
        na.rm = TRUE
      )
      
      new_group[[age_var_model]] <- new_group[[age_var_plot]] - center_value
      
      new_group[[paradigm_var]] <- factor(
        g,
        levels = paradigm_levels
      )
      
      new_group
    }
  )
  
  newdata <- dplyr::bind_rows(
    prediction_list
  )
  
  
  # -------------------------------
  # Model matrix uses centered age
  # -------------------------------
  
  interaction_formula <- as.formula(
    paste0(
      "~ ",
      age_var_model,
      " * ",
      paradigm_var
    )
  )
  
  X <- model.matrix(
    interaction_formula,
    data = newdata
  )
  
  
  # -------------------------------
  # Predictions from model
  # -------------------------------
  
  pred <- predict(
    model,
    newmods = X[, -1, drop = FALSE]
  )
  
  newdata$pred <- pred$pred
  
  
  # -------------------------------
  # Plot
  # -------------------------------
  
  p <- ggplot() +
    
    geom_hline(
      yintercept = 0,
      colour = "grey70",
      linewidth = 0.3
    ) +
    
    # Raw observed effect sizes
    geom_point(
      data = plot_data,
      aes(
        x = .data[[age_var_plot]],
        y = Hedge_g_updated,
        colour = .data[[paradigm_var]]
      ),
      size = 0.3,
      alpha = 1
    ) +
    
    # Predicted lines from centered-age model
    geom_line(
      data = newdata,
      aes(
        x = .data[[age_var_plot]],
        y = pred,
        colour = .data[[paradigm_var]],
        group = .data[[paradigm_var]]
      ),
      linewidth = 1
    ) +
    
    scale_colour_manual(
      values = paradigm_colors,
      breaks = paradigm_levels,
      labels = paradigm_labels,
      name = legend_label,
      drop = FALSE
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_classic() +
    
    theme(
      legend.position = "right",
      legend.title = element_text(
        size = 9,
        face = "bold"
      ),
      legend.text = element_text(size = 8),
      axis.title = element_text(size = 10),
      axis.text = element_text(
        size = 9,
        colour = "black"
      ),
      axis.line = element_line(linewidth = 0.5),
      axis.ticks = element_line(linewidth = 0.4)
    )
  
  
  if (!is.null(x_breaks)) {
    p <- p +
      scale_x_continuous(
        breaks = x_breaks
      )
  }
  
  if (!is.null(y_breaks)) {
    p <- p +
      scale_y_continuous(
        breaks = y_breaks
      )
  }
  
  p <- p +
    coord_cartesian(
      xlim = x_limits,
      ylim = y_limits
    )
  
  return(p)
}

# ---------------------------------
# --- Age x 4 Paradigms -----------
# --- actual age on x-axis --------
# ---------------------------------

df_para_4 <- df_cleaned %>%
  dplyr::filter(
    !is.na(paradigms_types),
    paradigms_types %in% c(
      "face preference task",
      "feature learning task",
      "free face learning task",
      "gaze related task"
    )
  ) %>%
  dplyr::mutate(
    paradigms_types = factor(
      paradigms_types,
      levels = c(
        "face preference task",
        "feature learning task",
        "free face learning task",
        "gaze related task"
      )
    )
  )

p_age_paradigms_4 <- plot_age_paradigms_actualx(
  model = m_int_para_4,
  data = df_para_4,
  
  age_var_model = "Mage_ASD_c",
  age_var_plot = "Mage_ASD",
  paradigm_var = "paradigms_types",
  
  x_label = "Mean Age (months)",
  y_label = "Hedges' g",
  legend_label = "Paradigms",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_paradigms_4_actual_age_plot.png",
  plot = p_age_paradigms_4,
  width = 7,
  height = 5,
  dpi = 300
)

# --------------------------------------------------------
# --- Age x Paradigms: Actual age, model centered, CI only
# --------------------------------------------------------

plot_age_paradigms_actualx_ci <- function(
    model,
    data,
    age_var_model = "Mage_ASD_c",
    age_var_plot = "Mage_ASD",
    paradigm_var = "paradigms_types",
    x_label = "Mean Age (months)",
    y_label = "Predicted Hedges' g",
    legend_label = "Paradigms",
    x_limits = NULL,
    x_breaks = NULL,
    y_limits = NULL,
    y_breaks = NULL) {
  
  
  # -------------------------------
  # Fixed paradigm order
  # -------------------------------
  
  paradigm_levels <- c(
    "face preference task",
    "feature learning task",
    "free face learning task",
    "gaze related task"
  )
  
  
  # -------------------------------
  # Prepare data
  # -------------------------------
  
  plot_data <- data %>%
    dplyr::filter(
      !is.na(.data[[age_var_model]]),
      !is.na(.data[[age_var_plot]]),
      !is.na(.data[[paradigm_var]]),
      .data[[paradigm_var]] %in% paradigm_levels
    ) %>%
    dplyr::mutate(
      !!paradigm_var := factor(
        .data[[paradigm_var]],
        levels = paradigm_levels
      )
    )
  
  
  # -------------------------------
  # Colors
  # -------------------------------
  
  paradigm_colors <- c(
    "face preference task" = "#C66DB7",
    "feature learning task" = "#F2C14E",
    "free face learning task" = "#53A6D3",
    "gaze related task" = "#57B77A"
  )
  
  
  # -------------------------------
  # Legend labels
  # -------------------------------
  
  paradigm_labels <- c(
    "face preference task" = "Face Preference",
    "feature learning task" = "Feature Learning",
    "free face learning task" = "Free Face Learning",
    "gaze related task" = "Gaze Following"
  )
  
  
  # -------------------------------
  # Build prediction data
  # x-axis shown as actual age,
  # prediction uses centered age
  # -------------------------------
  
  prediction_list <- lapply(
    paradigm_levels,
    function(g) {
      
      group_data <- plot_data %>%
        dplyr::filter(
          .data[[paradigm_var]] == g
        )
      
      age_plot_min <- min(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      age_plot_max <- max(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      new_group <- data.frame(
        age_plot_seq = seq(
          age_plot_min,
          age_plot_max,
          length.out = 100
        )
      )
      
      names(new_group)[1] <- age_var_plot
      
      center_value <- mean(
        plot_data[[age_var_plot]] - plot_data[[age_var_model]],
        na.rm = TRUE
      )
      
      new_group[[age_var_model]] <-
        new_group[[age_var_plot]] - center_value
      
      new_group[[paradigm_var]] <- factor(
        g,
        levels = paradigm_levels
      )
      
      new_group
    }
  )
  
  newdata <- dplyr::bind_rows(
    prediction_list
  )
  
  
  # -------------------------------
  # Model matrix
  # -------------------------------
  
  interaction_formula <- as.formula(
    paste0(
      "~ ",
      age_var_model,
      " * ",
      paradigm_var
    )
  )
  
  X <- model.matrix(
    interaction_formula,
    data = newdata
  )
  
  
  # -------------------------------
  # Predictions from model
  # -------------------------------
  
  pred <- predict(
    model,
    newmods = X[, -1, drop = FALSE]
  )
  
  newdata$pred  <- pred$pred
  newdata$ci_lb <- pred$ci.lb
  newdata$ci_ub <- pred$ci.ub
  
  
  # -------------------------------
  # Plot
  # -------------------------------
  
  p <- ggplot() +
    
    # CI ribbons only
    geom_ribbon(
      data = newdata,
      aes(
        x = .data[[age_var_plot]],
        ymin = ci_lb,
        ymax = ci_ub,
        fill = .data[[paradigm_var]],
        group = .data[[paradigm_var]]
      ),
      alpha = 0.08,
      colour = NA
    ) +
    
    # Predicted lines
    geom_line(
      data = newdata,
      aes(
        x = .data[[age_var_plot]],
        y = pred,
        colour = .data[[paradigm_var]],
        group = .data[[paradigm_var]]
      ),
      linewidth = 1
    ) +
    
    scale_colour_manual(
      values = paradigm_colors,
      breaks = paradigm_levels,
      labels = paradigm_labels,
      name = legend_label,
      drop = FALSE
    ) +
    
    scale_fill_manual(
      values = paradigm_colors,
      breaks = paradigm_levels,
      labels = paradigm_labels,
      guide = "none",
      drop = FALSE
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_classic() +
    
    theme(
      legend.position = "right",
      legend.title = element_text(
        size = 2,
        face = "bold"
      ),
      legend.text = element_text(size = 2),
      axis.title = element_text(size = 10),
      axis.text = element_text(
        size = 10,
        colour = "black"
      ),
      axis.line = element_line(linewidth = 0.5),
      axis.ticks = element_line(linewidth = 0.4)
    )
  
  
  if (!is.null(x_breaks)) {
    p <- p +
      scale_x_continuous(
        breaks = x_breaks
      )
  }
  
  if (!is.null(y_breaks)) {
    p <- p +
      scale_y_continuous(
        breaks = y_breaks
      )
  }
  
  p <- p +
    coord_cartesian(
      xlim = x_limits,
      ylim = y_limits
    )
  
  return(p)
}

# ---------------------------------
# --- Age x 4 Paradigms: CI only --
# ---------------------------------

df_para_4 <- df_cleaned %>%
  dplyr::filter(
    !is.na(paradigms_types),
    paradigms_types %in% c(
      "face preference task",
      "feature learning task",
      "free face learning task",
      "gaze related task"
    )
  ) %>%
  dplyr::mutate(
    paradigms_types = factor(
      paradigms_types,
      levels = c(
        "face preference task",
        "feature learning task",
        "free face learning task",
        "gaze related task"
      )
    )
  )

p_age_paradigms_4_ci <- plot_age_paradigms_actualx_ci(
  model = m_int_para_4,
  data = df_para_4,
  
  age_var_model = "Mage_ASD_c",
  age_var_plot = "Mage_ASD",
  paradigm_var = "paradigms_types",
  
  x_label = "Mean Age (months)",
  y_label = "Predicted Hedges' g",
  legend_label = "Paradigms",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)

ggsave(
  "age_paradigms_4_actual_age_CI_only.png",
  plot = p_age_paradigms_4_ci,
  width = 5,
  height = 3.5,
  dpi = 300
)

# ========================================================
# === ALL 7 PARADIGMS ====================================
# ========================================================


# --------------------------------------------------------
# --- Age x 7 Paradigms: Plot actual age, model centered -
# --------------------------------------------------------

plot_age_paradigms_7_actualx <- function(
    model,
    data,
    age_var_model = "Mage_ASD_c",
    age_var_plot = "Mage_ASD",
    paradigm_var = "paradigms_types",
    x_label = "Mean Age (months)",
    y_label = "Hedges' g",
    legend_label = "Paradigms",
    x_limits = NULL,
    x_breaks = NULL,
    y_limits = NULL,
    y_breaks = NULL) {
  
  
  # -------------------------------
  # Fixed paradigm order: 7 tasks
  # -------------------------------
  
  paradigm_levels_7 <- c(
    "face preference task",
    "emotion-related task",
    "feature learning task",
    "free face learning task",
    "gaze related task",
    "habituation task",
    "lateralization task"
  )
  
  
  # -------------------------------
  # Prepare data
  # -------------------------------
  
  plot_data <- data %>%
    dplyr::filter(
      !is.na(.data[[age_var_model]]),
      !is.na(.data[[age_var_plot]]),
      !is.na(.data[[paradigm_var]]),
      !is.na(Hedge_g_updated),
      .data[[paradigm_var]] %in% paradigm_levels_7
    ) %>%
    dplyr::mutate(
      !!paradigm_var := factor(
        .data[[paradigm_var]],
        levels = paradigm_levels_7
      )
    )
  
  
  # -------------------------------
  # Fixed colors
  # -------------------------------
  
  paradigm_colors_7 <- c(
    "face preference task" = "#C66DB7",
    "emotion-related task" = "#E76F51",
    "feature learning task" = "#F2C14E",
    "free face learning task" = "#53A6D3",
    "gaze related task" = "#57B77A",
    "habituation task" = "#8E7DBE",
    "lateralization task" = "#8C6D5A"
  )
  
  
  # -------------------------------
  # Fixed labels
  # -------------------------------
  
  paradigm_labels_7 <- c(
    "face preference task" = "Face Preference",
    "emotion-related task" = "Emotion-related",
    "feature learning task" = "Feature Learning",
    "free face learning task" = "Free Face Learning",
    "gaze related task" = "Gaze Following",
    "habituation task" = "Habituation",
    "lateralization task" = "Lateralization"
  )
  
  
  # -------------------------------
  # Build prediction data
  # x-axis = actual age
  # prediction = centered age
  # -------------------------------
  
  prediction_list_7 <- lapply(
    paradigm_levels_7,
    function(g) {
      
      group_data <- plot_data %>%
        dplyr::filter(
          .data[[paradigm_var]] == g
        )
      
      age_plot_min <- min(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      age_plot_max <- max(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      new_group <- data.frame(
        age_plot_seq = seq(
          age_plot_min,
          age_plot_max,
          length.out = 100
        )
      )
      
      names(new_group)[1] <- age_var_plot
      
      
      # convert actual age to centered age
      center_value <- mean(
        plot_data[[age_var_plot]] -
          plot_data[[age_var_model]],
        na.rm = TRUE
      )
      
      new_group[[age_var_model]] <-
        new_group[[age_var_plot]] -
        center_value
      
      new_group[[paradigm_var]] <- factor(
        g,
        levels = paradigm_levels_7
      )
      
      new_group
    }
  )
  
  
  newdata_7 <- dplyr::bind_rows(
    prediction_list_7
  )
  
  
  # -------------------------------
  # Model matrix uses centered age
  # -------------------------------
  
  interaction_formula_7 <- as.formula(
    paste0(
      "~ ",
      age_var_model,
      " * ",
      paradigm_var
    )
  )
  
  
  X_7 <- model.matrix(
    interaction_formula_7,
    data = newdata_7
  )
  
  
  # -------------------------------
  # Predictions from model
  # -------------------------------
  
  pred_7 <- predict(
    model,
    newmods = X_7[, -1, drop = FALSE]
  )
  
  newdata_7$pred <- pred_7$pred
  
  
  # -------------------------------
  # Plot
  # -------------------------------
  
  p_7 <- ggplot() +
    
    geom_hline(
      yintercept = 0,
      colour = "grey70",
      linewidth = 0.3
    ) +
    
    # Raw observed effect sizes
    geom_point(
      data = plot_data,
      aes(
        x = .data[[age_var_plot]],
        y = Hedge_g_updated,
        colour = .data[[paradigm_var]]
      ),
      size = 0.3,
      alpha = 1
    ) +
    
    # Predicted lines
    geom_line(
      data = newdata_7,
      aes(
        x = .data[[age_var_plot]],
        y = pred,
        colour = .data[[paradigm_var]],
        group = .data[[paradigm_var]]
      ),
      linewidth = 1
    ) +
    
    scale_colour_manual(
      values = paradigm_colors_7,
      breaks = paradigm_levels_7,
      labels = paradigm_labels_7,
      name = legend_label,
      drop = FALSE
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_classic() +
    
    theme(
      legend.position = "right",
      legend.title = element_text(
        size = 9,
        face = "bold"
      ),
      legend.text = element_text(
        size = 8
      ),
      axis.title = element_text(
        size = 10
      ),
      axis.text = element_text(
        size = 9,
        colour = "black"
      ),
      axis.line = element_line(
        linewidth = 0.5
      ),
      axis.ticks = element_line(
        linewidth = 0.4
      )
    )
  
  
  if (!is.null(x_breaks)) {
    p_7 <- p_7 +
      scale_x_continuous(
        breaks = x_breaks
      )
  }
  
  
  if (!is.null(y_breaks)) {
    p_7 <- p_7 +
      scale_y_continuous(
        breaks = y_breaks
      )
  }
  
  
  p_7 <- p_7 +
    coord_cartesian(
      xlim = x_limits,
      ylim = y_limits
    )
  
  
  return(p_7)
}


# ---------------------------------
# --- Age x 7 Paradigms -----------
# --- Scatter + predicted lines ---
# ---------------------------------

df_para_7_plot <- df_cleaned %>%
  dplyr::filter(
    !is.na(paradigms_types),
    paradigms_types %in% c(
      "face preference task",
      "emotion-related task",
      "feature learning task",
      "free face learning task",
      "gaze related task",
      "habituation task",
      "lateralization task"
    )
  ) %>%
  dplyr::mutate(
    paradigms_types = factor(
      paradigms_types,
      levels = c(
        "face preference task",
        "emotion-related task",
        "feature learning task",
        "free face learning task",
        "gaze related task",
        "habituation task",
        "lateralization task"
      )
    )
  )


p_age_paradigms_7 <- plot_age_paradigms_7_actualx(
  model = m_int_para,
  data = df_para_7_plot,
  
  age_var_model = "Mage_ASD_c",
  age_var_plot = "Mage_ASD",
  paradigm_var = "paradigms_types",
  
  x_label = "Mean Age (months)",
  y_label = "Hedges' g",
  legend_label = "Paradigms",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)


ggsave(
  "age_paradigms_7_actual_age_plot.png",
  plot = p_age_paradigms_7,
  width = 7,
  height = 5,
  dpi = 300
)



# ========================================================
# === ALL 7 PARADIGMS: CI ONLY ===========================
# ========================================================


# --------------------------------------------------------
# --- Age x 7 Paradigms: Actual age, centered model, CI --
# --------------------------------------------------------

plot_age_paradigms_7_actualx_ci <- function(
    model,
    data,
    age_var_model = "Mage_ASD_c",
    age_var_plot = "Mage_ASD",
    paradigm_var = "paradigms_types",
    x_label = "Mean Age (months)",
    y_label = "Predicted Hedges' g",
    legend_label = "Paradigms",
    x_limits = NULL,
    x_breaks = NULL,
    y_limits = NULL,
    y_breaks = NULL) {
  
  
  # -------------------------------
  # Fixed paradigm order: 7 tasks
  # -------------------------------
  
  paradigm_levels_7 <- c(
    "face preference task",
    "emotion-related task",
    "feature learning task",
    "free face learning task",
    "gaze related task",
    "habituation task",
    "lateralization task"
  )
  
  
  # -------------------------------
  # Prepare data
  # -------------------------------
  
  plot_data <- data %>%
    dplyr::filter(
      !is.na(.data[[age_var_model]]),
      !is.na(.data[[age_var_plot]]),
      !is.na(.data[[paradigm_var]]),
      .data[[paradigm_var]] %in% paradigm_levels_7
    ) %>%
    dplyr::mutate(
      !!paradigm_var := factor(
        .data[[paradigm_var]],
        levels = paradigm_levels_7
      )
    )
  
  
  # -------------------------------
  # Colors
  # -------------------------------
  
  paradigm_colors_7 <- c(
    "face preference task" = "#C66DB7",
    "emotion-related task" = "#E76F51",
    "feature learning task" = "#F2C14E",
    "free face learning task" = "#53A6D3",
    "gaze related task" = "#57B77A",
    "habituation task" = "#8E7DBE",
    "lateralization task" = "#8C6D5A"
  )
  
  
  # -------------------------------
  # Legend labels
  # -------------------------------
  
  paradigm_labels_7 <- c(
    "face preference task" = "Face Preference",
    "emotion-related task" = "Emotion-related",
    "feature learning task" = "Feature Learning",
    "free face learning task" = "Free Face Learning",
    "gaze related task" = "Gaze Following",
    "habituation task" = "Habituation",
    "lateralization task" = "Lateralization"
  )
  
  
  # -------------------------------
  # Build prediction data
  # -------------------------------
  
  prediction_list_7_ci <- lapply(
    paradigm_levels_7,
    function(g) {
      
      group_data <- plot_data %>%
        dplyr::filter(
          .data[[paradigm_var]] == g
        )
      
      age_plot_min <- min(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      age_plot_max <- max(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      new_group <- data.frame(
        age_plot_seq = seq(
          age_plot_min,
          age_plot_max,
          length.out = 100
        )
      )
      
      names(new_group)[1] <- age_var_plot
      
      
      center_value <- mean(
        plot_data[[age_var_plot]] -
          plot_data[[age_var_model]],
        na.rm = TRUE
      )
      
      new_group[[age_var_model]] <-
        new_group[[age_var_plot]] -
        center_value
      
      new_group[[paradigm_var]] <- factor(
        g,
        levels = paradigm_levels_7
      )
      
      new_group
    }
  )
  
  
  newdata_7_ci <- dplyr::bind_rows(
    prediction_list_7_ci
  )
  
  
  # -------------------------------
  # Model matrix
  # -------------------------------
  
  interaction_formula_7_ci <- as.formula(
    paste0(
      "~ ",
      age_var_model,
      " * ",
      paradigm_var
    )
  )
  
  
  X_7_ci <- model.matrix(
    interaction_formula_7_ci,
    data = newdata_7_ci
  )
  
  
  # -------------------------------
  # Predictions
  # -------------------------------
  
  pred_7_ci <- predict(
    model,
    newmods = X_7_ci[, -1, drop = FALSE]
  )
  
  
  newdata_7_ci$pred <- pred_7_ci$pred
  newdata_7_ci$ci_lb <- pred_7_ci$ci.lb
  newdata_7_ci$ci_ub <- pred_7_ci$ci.ub
  
  
  # -------------------------------
  # Plot
  # -------------------------------
  
  p_7_ci <- ggplot() +
    
    # CI ribbons
    geom_ribbon(
      data = newdata_7_ci,
      aes(
        x = .data[[age_var_plot]],
        ymin = ci_lb,
        ymax = ci_ub,
        fill = .data[[paradigm_var]],
        group = .data[[paradigm_var]]
      ),
      alpha = 0.08,
      colour = NA
    ) +
    
    # Predicted lines
    geom_line(
      data = newdata_7_ci,
      aes(
        x = .data[[age_var_plot]],
        y = pred,
        colour = .data[[paradigm_var]],
        group = .data[[paradigm_var]]
      ),
      linewidth = 1
    ) +
    
    scale_colour_manual(
      values = paradigm_colors_7,
      breaks = paradigm_levels_7,
      labels = paradigm_labels_7,
      name = legend_label,
      drop = FALSE
    ) +
    
    scale_fill_manual(
      values = paradigm_colors_7,
      breaks = paradigm_levels_7,
      labels = paradigm_labels_7,
      guide = "none",
      drop = FALSE
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_classic() +
    
    theme(
      legend.position = "right",
      legend.title = element_text(
        size = 2,
        face = "bold"
      ),
      legend.text = element_text(
        size = 2
      ),
      axis.title = element_text(
        size = 10
      ),
      axis.text = element_text(
        size = 10,
        colour = "black"
      ),
      axis.line = element_line(
        linewidth = 0.5
      ),
      axis.ticks = element_line(
        linewidth = 0.4
      )
    )
  
  
  if (!is.null(x_breaks)) {
    p_7_ci <- p_7_ci +
      scale_x_continuous(
        breaks = x_breaks
      )
  }
  
  
  if (!is.null(y_breaks)) {
    p_7_ci <- p_7_ci +
      scale_y_continuous(
        breaks = y_breaks
      )
  }
  
  
  p_7_ci <- p_7_ci +
    coord_cartesian(
      xlim = x_limits,
      ylim = y_limits
    )
  
  
  return(p_7_ci)
}


# ---------------------------------
# --- Age x 7 Paradigms: CI only --
# ---------------------------------

df_para_7_ci <- df_cleaned %>%
  dplyr::filter(
    !is.na(paradigms_types),
    paradigms_types %in% c(
      "face preference task",
      "emotion-related task",
      "feature learning task",
      "free face learning task",
      "gaze related task",
      "habituation task",
      "lateralization task"
    )
  ) %>%
  dplyr::mutate(
    paradigms_types = factor(
      paradigms_types,
      levels = c(
        "face preference task",
        "emotion-related task",
        "feature learning task",
        "free face learning task",
        "gaze related task",
        "habituation task",
        "lateralization task"
      )
    )
  )


p_age_paradigms_7_ci <- plot_age_paradigms_7_actualx_ci(
  model = m_int_para,
  data = df_para_7_ci,
  
  age_var_model = "Mage_ASD_c",
  age_var_plot = "Mage_ASD",
  paradigm_var = "paradigms_types",
  
  x_label = "Mean Age (months)",
  y_label = "Predicted Hedges' g",
  legend_label = "Paradigms",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)


ggsave(
  "age_paradigms_7_actual_age_CI_only.png",
  plot = p_age_paradigms_7_ci,
  width = 5,
  height = 3.5,
  dpi = 300
)








# -------------------------------------------------------------------
# --------------------------------------------------------
# --- Age x 2-Group Interaction: Actual Age on X-axis ---
# --------------------------------------------------------

plot_age_interaction_actualx <- function(
    model,
    data,
    moderator_var,
    group_levels,
    age_var_model = "Mage_ASD_c",
    age_var_plot = "Mage_ASD",
    x_label = "Mean Age (months)",
    y_label = "Hedges' g",
    legend_label = moderator_var,
    legend_labels = NULL,
    x_limits = NULL,
    x_breaks = NULL,
    y_limits = NULL,
    y_breaks = NULL) {
  
  
  # -------------------------------
  # Check group levels
  # -------------------------------
  
  if (length(group_levels) != 2) {
    stop(
      "group_levels must contain exactly 2 groups."
    )
  }
  
  
  if (is.null(legend_labels)) {
    legend_labels <- group_levels
  }
  
  
  # -------------------------------
  # Prepare plotting data
  # -------------------------------
  
  plot_data <- data %>%
    dplyr::filter(
      !is.na(.data[[age_var_model]]),
      !is.na(.data[[age_var_plot]]),
      !is.na(.data[[moderator_var]]),
      !is.na(Hedge_g_updated),
      .data[[moderator_var]] %in% group_levels
    ) %>%
    dplyr::mutate(
      !!moderator_var := factor(
        .data[[moderator_var]],
        levels = group_levels
      )
    )
  
  
  # -------------------------------
  # Colors
  # Keep original blue / yellow
  # -------------------------------
  
  group_colors <- c(
    "#1E76BC",
    "#D9A441"
  )
  
  names(group_colors) <- group_levels
  
  
  # -------------------------------
  # Centering value
  #
  # Mage_ASD_c =
  # Mage_ASD - center_value
  #
  # Since df_cleaned was created by
  # deleting rows, the original
  # centered-age transformation remains.
  # -------------------------------
  
  center_value <- mean(
    plot_data[[age_var_plot]] -
      plot_data[[age_var_model]],
    na.rm = TRUE
  )
  
  
  # -------------------------------
  # Build prediction data
  #
  # Each regression line is restricted
  # to the actual observed age range
  # of that group.
  # -------------------------------
  
  prediction_list <- lapply(
    group_levels,
    function(g) {
      
      group_data <- plot_data %>%
        dplyr::filter(
          .data[[moderator_var]] == g
        )
      
      
      age_plot_min <- min(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      age_plot_max <- max(
        group_data[[age_var_plot]],
        na.rm = TRUE
      )
      
      
      new_group <- data.frame(
        age_plot_seq = seq(
          age_plot_min,
          age_plot_max,
          length.out = 100
        )
      )
      
      
      names(new_group)[1] <- age_var_plot
      
      
      # Convert actual age back to
      # centered age for model prediction
      new_group[[age_var_model]] <-
        new_group[[age_var_plot]] -
        center_value
      
      
      new_group[[moderator_var]] <- factor(
        g,
        levels = group_levels
      )
      
      
      new_group
    }
  )
  
  
  newdata <- dplyr::bind_rows(
    prediction_list
  )
  
  
  # -------------------------------
  # Model matrix uses centered age
  # -------------------------------
  
  interaction_formula <- as.formula(
    paste0(
      "~ ",
      age_var_model,
      " * ",
      moderator_var
    )
  )
  
  
  X <- model.matrix(
    interaction_formula,
    data = newdata
  )
  
  
  # -------------------------------
  # Predictions from ORIGINAL model
  # -------------------------------
  
  pred <- predict(
    model,
    newmods = X[, -1, drop = FALSE]
  )
  
  
  newdata$pred <- pred$pred
  
  
  # -------------------------------
  # Plot
  # -------------------------------
  
  p <- ggplot() +
    
    
    # Zero reference line
    geom_hline(
      yintercept = 0,
      colour = "grey70",
      linewidth = 0.3
    ) +
    
    
    # Raw observed effect sizes
    geom_point(
      data = plot_data,
      aes(
        x = .data[[age_var_plot]],
        y = Hedge_g_updated,
        colour = .data[[moderator_var]]
      ),
      size = 0.3,
      alpha = 1
    ) +
    
    
    # Predicted lines from centered-age
    # interaction meta-regression model
    geom_line(
      data = newdata,
      aes(
        x = .data[[age_var_plot]],
        y = pred,
        colour = .data[[moderator_var]],
        group = .data[[moderator_var]]
      ),
      linewidth = 1
    ) +
    
    
    scale_colour_manual(
      values = group_colors,
      breaks = group_levels,
      labels = legend_labels,
      name = legend_label,
      drop = FALSE
    ) +
    
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    
    theme_classic() +
    
    
    theme(
      legend.position = "right",
      
      legend.title = element_text(
        size = 9,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 8
      ),
      
      axis.title = element_text(
        size = 10
      ),
      
      axis.text = element_text(
        size = 9,
        colour = "black"
      ),
      
      axis.line = element_line(
        linewidth = 0.5
      ),
      
      axis.ticks = element_line(
        linewidth = 0.4
      )
    )
  
  
  # -------------------------------
  # Axis settings
  # -------------------------------
  
  if (!is.null(x_breaks)) {
    
    p <- p +
      scale_x_continuous(
        breaks = x_breaks
      )
  }
  
  
  if (!is.null(y_breaks)) {
    
    p <- p +
      scale_y_continuous(
        breaks = y_breaks
      )
  }
  
  
  p <- p +
    coord_cartesian(
      xlim = x_limits,
      ylim = y_limits
    )
  
  
  return(p)
}


# ---------------------------------
# --- Age x Dynamics --------------
# ---------------------------------

dynamics_levels <- levels(
  factor(df_new$dynamics)
)

p_int1_actual <- plot_age_interaction_actualx(
  model = m_int_dyna,
  data = df_cleaned,
  
  moderator_var = "dynamics",
  group_levels = dynamics_levels,
  
  legend_label = "Stimulus Dynamics",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_dynamics_actual_age_plot.png",
  plot = p_int1_actual,
  width = 6,
  height = 4.5,
  dpi = 300
)

# ---------------------------------
# --- Age x Emotional Conditions --
# ---------------------------------

emo_levels <- levels(
  factor(df_new$emo_condition)
)

p_int2_actual <- plot_age_interaction_actualx(
  model = m_int_emo,
  data = df_cleaned,
  
  moderator_var = "emo_condition",
  group_levels = emo_levels,
  
  legend_label = "Emotional Conditions",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_emo_conditions_actual_age_plot.png",
  plot = p_int2_actual,
  width = 6,
  height = 4.5,
  dpi = 300
)

# ---------------------------------
# --- Age x Auditory Information --
# ---------------------------------

auditory_levels <- levels(
  factor(df_new$visualaudio)
)

p_int4_actual <- plot_age_interaction_actualx(
  model = m_int_auditory,
  data = df_cleaned,
  
  moderator_var = "visualaudio",
  group_levels = auditory_levels,
  
  legend_label = "Presentation Modality",
  
  legend_labels = c(
    "Visual Only",
    "Visual with Audio"
  ),
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_auditory_actual_age_plot.png",
  plot = p_int4_actual,
  width = 6,
  height = 4.5,
  dpi = 300
)

# ---------------------------------
# --- Age x Risk Outcome ----------
# ---------------------------------

df_risk_plot <- df_cleaned %>%
  dplyr::filter(
    ASD_group %in% c(
      "hr_noASD",
      "hr_ASD"
    )
  ) %>%
  dplyr::mutate(
    ASD_group = factor(
      ASD_group,
      levels = c(
        "hr_noASD",
        "hr_ASD"
      )
    )
  )

p_int6_actual <- plot_age_interaction_actualx(
  model = m_int_risk,
  data = df_risk_plot,
  moderator_var = "ASD_group",
  group_levels = c(
    "hr_noASD",
    "hr_ASD"
  ),
  legend_label = "Risk Outcome",
  legend_labels = c(
    "HR_noASD",
    "HR_ASD"
  ),
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_risk_outcomes_actual_age_plot.png",
  plot = p_int6_actual,
  width = 6,
  height = 4.5,
  dpi = 300
)

# ---------------------------------
# --- Age x Presentation Format ---
# ---------------------------------

presentation_levels <- levels(
  factor(df_new$pre_format)
)

p_int3_actual <- plot_age_interaction_actualx(
  model = m_int_present,
  data = df_cleaned,
  moderator_var = "pre_format",
  group_levels = presentation_levels,
  legend_label = "Presentation Format",
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_present_format_actual_age_plot.png",
  plot = p_int3_actual,
  width = 6,
  height = 4.5,
  dpi = 300
)

# =================================
# --- Gaze Related Task -----------
# =================================

# Keep gaze-related task observations
# and explicitly define the two gaze categories
df_gaze <- df_new %>%
  dplyr::filter(
    paradigms_types == "gaze related task",
    !is.na(face_feature_types)
  ) %>%
  dplyr::mutate(
    face_feature_types = factor(
      face_feature_types,
      levels = c(
        "direct_averted",
        "gaze following"
      )
    )
  )


# Check subgroup sizes
df_gaze %>%
  dplyr::group_by(face_feature_types) %>%
  dplyr::summarise(
    n_effect_sizes = dplyr::n(),
    n_papers = dplyr::n_distinct(Paper_ID),
    min_age = min(Mage_ASD, na.rm = TRUE),
    max_age = max(Mage_ASD, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------
# --- Moderator Analysis ----------
# ---------------------------------

m_cat_gaze <- cat_mod(
  df_gaze,
  "face_feature_types"
)


# ---------------------------------
# --- Age x Gaze Type -------------
# ---------------------------------

m_int_gaze <- int_mod(
  data = df_gaze,
  var1 = "Mage_ASD_c",
  var2 = "face_feature_types"
)

slope_gaze <- get_rma_slopes(
  model = m_int_gaze,
  data = df_gaze,
  moderator = "face_feature_types"
)

print(summary(m_cat_gaze))
print(summary(m_int_gaze))
print(interaction_anova(m_int_gaze))
print(slope_gaze)


# =================================
# --- Gaze Type Subgroup Bar Plot -
# =================================

# --- Direct / Averted Gaze ---
df_direct_averted <- df_gaze %>%
  dplyr::filter(
    face_feature_types == "direct_averted"
  )

main_direct_averted <- fit_threelevel_model(
  df_direct_averted
)


# --- Gaze Following ---
df_gaze_following <- df_gaze %>%
  dplyr::filter(
    face_feature_types == "gaze following"
  )

main_gaze_following <- fit_threelevel_model(
  df_gaze_following
)


# Check pooled subgroup estimates
print(main_direct_averted)
print(main_gaze_following)


# --- Bar graph ---
p_bar_gaze <- plot_subgroup_bar(
  
  models = list(
    main_direct_averted,
    main_gaze_following
  ),
  
  labels = c(
    "Direct/Averted Gaze",
    "Gaze Following"
  ),
  
  title = "Gaze Related Task"
)

ggsave(
  "gaze_related_subgroup_comparison.png",
  plot = p_bar_gaze,
  width = 5,
  height = 3.5,
  dpi = 300
)


# =================================
# --- Age x Gaze: CI Plot ---------
# =================================

gaze_levels <- c(
  "direct_averted",
  "gaze following"
)

p_gaze_ci <- plot_age_interaction_actualx(
  model = m_int_gaze,
  data = df_gaze,
  
  moderator_var = "face_feature_types",
  group_levels = gaze_levels,
  
  legend_label = "Gaze Type",
  
  legend_labels = c(
    "Direct/Averted Gaze",
    "Gaze Following"
  ),
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-1, 1),
  y_breaks = seq(-1, 1, by = 0.5)
)

ggsave(
  "age_gaze_interaction_CI.png",
  plot = p_gaze_ci,
  width = 5,
  height = 3.5,
  dpi = 300
)


# =================================
# --- Age x Gaze: Scatter Plot ----
# =================================

# Use cleaned data only for displaying scatter points
df_gaze_cleaned <- df_cleaned %>%
  dplyr::filter(
    paradigms_types == "gaze related task",
    face_feature_types %in% c(
      "direct_averted",
      "gaze following"
    )
  ) %>%
  dplyr::mutate(
    face_feature_types = factor(
      face_feature_types,
      levels = c(
        "direct_averted",
        "gaze following"
      )
    )
  )


p_gaze_scatter <- plot_age_interaction_actualx(
  model = m_int_gaze,
  data = df_gaze_cleaned,
  
  moderator_var = "face_feature_types",
  group_levels = gaze_levels,
  
  legend_label = "Gaze Type",
  
  legend_labels = c(
    "Direct/Averted Gaze",
    "Gaze Following"
  ),
  
  x_limits = c(0, 24),
  x_breaks = seq(0, 24, by = 4),
  
  y_limits = c(-2, 2),
  y_breaks = seq(-2, 2, by = 0.5)
)

ggsave(
  "age_gaze_interaction_scatter.png",
  plot = p_gaze_scatter,
  width = 5,
  height = 3.5,
  dpi = 300
)