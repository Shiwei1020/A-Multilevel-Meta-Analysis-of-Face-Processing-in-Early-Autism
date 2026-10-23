# ------ Loading Libraries ------
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(esc)
library(meta)
library(metafor)
library(purrr)  

# ------ Reading File ------
df_new <- read_csv(
  "C:/Users/ZXMLAB/Desktop/meta/data_analysis/processed_dataset.csv"
)

# ------ Numbers of Papers ------
n_papers <- df_new %>%
  distinct(Paper_ID) %>%
  nrow()
cat("The total number of included papers is", n_papers, "\n")

# ------ Publication Year ------
max_yr <- max(df_new$pub_yr)
min_yr <- min(df_new$pub_yr)

cat("The earliest published year is", min_yr, "\n")
cat("The latest published year is", max_yr, "\n")

# ------ Numbers of Effect Sizes ------
n_effects <- nrow(df_new)
cat("The total number of effect sizes is", n_effects, "\n")

mean_effects = round(n_effects/n_papers, 2)
cat("The mean number of effect sizes is", mean_effects, "\n")

effect_counts <- table(df_new$Paper_ID)

max_effects_per_paper <- max(effect_counts)
min_effects_per_paper <- min(effect_counts)

cat("The maximun number of effect sizes is", max_effects_per_paper, "\n")
cat("The minimun number of effect sizes is", min_effects_per_paper, "\n")

# ------ Age Summary ------
ASD_age_summary <- df_new %>%
  summarise(
    mean_age_ASD = mean(Mage_ASD, na.rm = TRUE),
    sd_age_ASD = sd(Mage_ASD, na.rm = TRUE),
    min_age_ASD = min(Mage_ASD, na.rm = TRUE),
    max_age_ASD = max(Mage_ASD, na.rm = TRUE)
  )

print(ASD_age_summary)

TD_age_summary <- df_new %>%
  summarise(
    mean_age_TD = mean(Mage_TD, na.rm = TRUE),
    sd_age_TD = sd(Mage_TD, na.rm = TRUE),
    min_age_TD = min(Mage_TD, na.rm = TRUE),
    max_age_TD = max(Mage_TD, na.rm = TRUE)
  )

print(TD_age_summary)

df_asd <- df_new %>%
  dplyr::filter(ASD_group %in% c("hr_ASD", "ASD"))

asd_subgroup_age_summary <- df_asd %>%
  summarise(
    mean_age_ASD = mean(Mage_ASD, na.rm = TRUE),
    sd_age_ASD = sd(Mage_ASD, na.rm = TRUE),
    min_age_ASD = min(Mage_ASD, na.rm = TRUE),
    max_age_ASD = max(Mage_ASD, na.rm = TRUE)
  )

print(asd_subgroup_age_summary)

# ------ Group Distribution ------
print(table(df_new$ASD_group))

# ------ Moderators Counts ------
count_moderator <- function(data, moderator_var, moderator_label) {
  
  moderator_data <- data %>%
    filter(
      !is.na(Paper_ID),
      !is.na(.data[[moderator_var]])
    )
  
  total_effect_sizes <- nrow(moderator_data)
  total_papers <- n_distinct(moderator_data$Paper_ID)
  
  moderator_counts <- moderator_data %>%
    group_by(.data[[moderator_var]]) %>%
    summarise(
      n_effect_sizes = n(),
      n_papers = n_distinct(Paper_ID),
      .groups = "drop"
    ) %>%
    mutate(
      effect_size_percentage = round(
        n_effect_sizes / total_effect_sizes * 100,
        2
      ),
      paper_percentage = round(
        n_papers / total_papers * 100,
        2
      )
    )
  
  cat("------", moderator_label, "------")
  print(moderator_counts)
  
  cat(
    "Total number of effect sizes included:",
    total_effect_sizes,"
    "
    )
  cat(
    "Total number of unique papers included:",
    total_papers,"
    "
    )
  return(moderator_counts)
}

# Paradigms
paradigms_counts <- count_moderator(
  data = df_new,
  moderator_var = "paradigms_types",
  moderator_label = "Paradigms"
)

# The papers included two or more equipments
equipments_summary <- df_new %>%
  filter(
    !is.na(Paper_ID),
    !is.na(equip_types)
  ) %>%
  distinct(Paper_ID, equip_types) %>%
  group_by(Paper_ID) %>%
  summarise(
    n_equip_types = n(),
    equipment_used = paste(sort(equip_types), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_equip_types > 1) %>%
  arrange(desc(n_equip_types))

print(equipments_summary)

# Visual and Visual-audio
visualaudio_counts <- count_moderator(
  data = df_new,
  moderator_var = "visualaudio",
  moderator_label = "Visual and Visual-audio"
)

# Familiarity
df_fam = subset(df_new, fam_group != "both")
familiarity_counts <- count_moderator(
  data = df_fam,
  moderator_var = "fam_group",
  moderator_label = "Familiarity"
)

# Memory
memory_counts <- count_moderator(
  data = df_new,
  moderator_var = "memory_yn",
  moderator_label = "Memory"
)

# Dynamics
dynamics_counts <- count_moderator(
  data = df_new,
  moderator_var = "dynamics",
  moderator_label = "Dynamics"
)

# Scene Complexity
scene_complexity_counts <- count_moderator(
  data = df_new,
  moderator_var = "scene_comp",
  moderator_label = "Scenery Complexity"
)

# Presentation Format
presentation_format_counts <- count_moderator(
  data = df_new,
  moderator_var = "pre_format",
  moderator_label = "Presentation Format"
)




