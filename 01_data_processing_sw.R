# Shiwei Wang meta analysis

# -------------------------
# Data Processing and Preparation
# -------------------------

# ------ References ------

# 1.1 R codes for mata analysis:
# 1.1.1：
#     Harrer, M., Cuijpers, P., Furukawa, T. A., & Ebert, D. D. (2021).
#     Doing Meta-Analysis with R: A Hands-On Guide (1st ed.).
#     Chapman and Hall/CRC.
#     https://doi.org/10.1201/9781003107347
# 1.1.2：
#     Assinka and Wibbelink, 2016
# 1.1.3：
#     Nakagawa and Santos, 2012

# 1.2 Statistics Formular
# 1.2.1 SE -> SD
#     SD = SE * sqrt(n)

# 1.2.2 CI -> SD
#     ((CI_upper_TD - CI_lower_TD) / (2 * 1.96)) * sqrt(N_TD)

# 1.2.3 Cohen'd -> Hedge's g
#     Using esc package: g <- hedges_g(SMD, n)

# 1.2.4 t, p, df -> Hedge's g
#     Cohen's d = t * sqrt((N1+N2) / N1N2))
#     J = 1 - (3/(4df - 1))
#     Hedge's g = J x d

# 1.2.4 SMD, SE -> t -> Cohen's d -> g
#     Borenstein et., 2008 (Chapter 4)

# 1.2.5 Mean Diff, CI -> SE -> SD pooled -> d/g
#     Higgins & Green (2011) Cochrane Handbook for Systematic Reviews of Interventions Chapter 6, 7 & 10





# ------ Loading libraries ------
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(esc)
library(meta)
library(metafor)
library(purrr)  


# ------ Read dataset ------

df <- read_excel(
  "C:/Users/ZXMLAB/Desktop/meta/data_analysis/data.xlsx",
  sheet = 1
)

# ------ Create new dataset & recode the data ------

df_new <- df %>%
  # create running index within each Paper ID
  group_by(`Paper ID`) %>%
  mutate(
    paper_index = paste0( `Paper ID`, " [", row_number(), "]")
  ) %>%
  ungroup() %>%
  
  transmute(
    Paper_ID = `Paper ID`,
    paper_index,
    pub_yr = `Year`,
    N_ASD = `N ASD`,
    ASD_group = `ASD Group Specifics`,
    N_TD = `N Control(could be No ASD, or Low-risk group, any baseline group they used to make comparasion with the ASD group)`,
    TD_group = `Control Group Specifics`,
    DV_type = `DV type?`,
    Memory_involved = `Memory involved in the task?(s)`,
    
    Mean_ASD = `Mean ASD`,
    SD_ASD = `SD ASD`,
    SE_ASD = `SE ASD`,
    CI_upper_ASD = `CI_upper_ASD`,
    CI_lower_ASD= `CI_lower_ASD`,
    
    Mean_TD = `Mean Control`,
    SD_TD = `SD Control`,
    SE_TD = `SE Control`,
    CI_upper_TD = `CI_upper_TD`,
    CI_lower_TD= `CI_lower_TD`,
    
    t = `t`,
    df = `df`,
    p = `p`,
    Eff_size = `Eff Size Estimate (if available)`,
    Eff_size_type = `Effect Size Type`,
    
    Mean_Diff = `Mean Difference(Masd-Mc)`,
    Mean_Diff_SE = `mean difference SE`,
    
    direction = `direction`,
  
    
    # Demographic Info ---
    Mage_ASD  = `MAge_ASD`,
    Mage_TD = `MAge_Control(month)`,
    
    N_female = `# of Females in ASD group`,
    
    ASD_group = case_when(
      `ASD Group Specifics` %in% c(
        "high risk, sib of ASD_ASD",
        "high risk, sib or parent of ASD_ASD",
        "high risk_ASD"
      ) ~ "hr_ASD",
      
      `ASD Group Specifics` %in% c(
        "high risk, sib of ASD_no ASD",
        "high risk, sib of ASD_TYP",
        "high risk, sib or parent of ASD_no ASD",
        "high risk_no ASD"
      ) ~ "hr_noASD",
      
      `ASD Group Specifics` %in% c(
        "high risk, sib of ASD_combined",
        "high risk, sib of ASD" # no reports for whether the high risk infants had been diagnosized with ASD later
      ) ~ "hr_combined",
      
      TRUE ~ `ASD Group Specifics`
    ),
    
    TD_group = `control groups`,
    HR_group = `ASD groups`,
    
    # --- Face-related Methods ---
    
    # Familiarity
    fam_group = `stimuli _face familarity`,
    
    # Dynamic
    dynamics = `stimuli _dynamics`,
    
    # Face Types
    face_feature_types = `Face type for analysis`,
    features = `face types`,
    face_types = `human face or artifical (mechanine-created face)`,
    fp_compare = `compared objects`,
    
    # Emotions
    types_emo = `type of stimuli emotions`,
    valence = `valence`,
    emo_condition = `neutral or emotion`,
    
    # Sounds
    visualaudio = `visualaudio`,
    scene_comp = `scenary complexity`,
    pre_format = `Presentation Format`,
    
    # Others
    vis_angles = `Visual angles(w*h)`,
    vis_dis = `Visibility Distance (cm)`,
    
    # --- Designed-related Methods ---
    
    # Equipment Types
    equip_types = `Equipment used`,
    
    # Memory
    memory_yn = `identity memory task?`,
    
    # Paradigms
    paradigms_types = `Task Type`,
    lat_task_types = `lateralizaiton task_types`,
    
    # EEG
    DV_types = `DV type?`,
    erp = `ERP components`,
    
    # Study Design
    study_design = `1 = long 2 = cross`,
    cohort_id = `cohort_id`,
    wave_id = `wave_id`,
    
    updated_SD_ASD = NA_real_,
    updated_SD_TD = NA_real_,
    Hedge_g = NA_real_,
    Hedge_g_SE = NA_real_,
    Hedge_g_var = NA_real_,
    ES_source = NA_character_,
    
    cat_age = NA_real_,
    female_per = NA_real_,
  )

df_new <- df_new %>%
  mutate(
    
    # --- Identification Variables ---
    Paper_ID = as.character(Paper_ID),
    paper_index = as.character(paper_index),
    pub_yr = as.numeric(pub_yr),
    
    DV_type = as.character(DV_type),
    Memory_involved = as.character(Memory_involved),
    
    # --- Group Information ---
    ASD_group = as.character(ASD_group),
    TD_group = as.character(TD_group),
    HR_group = as.character(HR_group),
    N_ASD = as.numeric(N_ASD),
    N_TD = as.numeric(N_TD),
    
    # --- Effect Size Input ---
    Mean_ASD = as.numeric(Mean_ASD),
    SD_ASD = as.numeric(SD_ASD),
    SE_ASD = as.numeric(SE_ASD),
    
    Mean_TD = as.numeric(Mean_TD),
    SD_TD = as.numeric(SD_TD),
    SE_TD = as.numeric(SE_TD),
    
    CI_upper_ASD = as.numeric(CI_upper_ASD),
    CI_lower_ASD = as.numeric(CI_lower_ASD),
    
    CI_upper_TD = as.numeric(CI_upper_TD),
    CI_lower_TD = as.numeric(CI_lower_TD),
    
    t = as.numeric(t),
    df = as.numeric(df),
    p = as.numeric(p),
    
    Eff_size = as.numeric(Eff_size),
    Eff_size_type = as.character(Eff_size_type),
    
    Mean_Diff = as.numeric(Mean_Diff),
    Mean_Diff_SE = as.numeric(Mean_Diff_SE),
    
    direction = as.numeric(direction),

    # --- Demographic Variables ---
    Mage_ASD = as.numeric(Mage_ASD),
    Mage_TD = as.numeric(Mage_TD),
    
    N_female = as.numeric(N_female),
    
    # --- Moderators ---
    fam_group = as.character(fam_group),
    dynamics = as.character(dynamics),
    face_feature_types = as.character(face_feature_types),
    face_types = as.character(face_types),
    types_emo = as.character(types_emo),
    valence = as.character(valence),
    emo_codition = as.character(emo_condition),
    visualaudio = as.character(visualaudio),
    scene_comp = as.character(scene_comp),
    pre_format = as.character(pre_format),
    vis_angles = as.character(vis_angles),
    vis_dis = as.character(vis_dis),
    equip_types = as.character(equip_types),
    memory_yn = as.character(memory_yn),
    paradigms_types = as.character(paradigms_types),
    lat_task_types = as.character(lat_task_types),
    features = as.character(features),
    fp_compare = as.character(fp_compare),
    DV_types = as.character(DV_types),
    erp = as.character(erp),
    study_design = as.character(study_design),
    cohort_id = as.character(cohort_id),
    wave_id = as.character(wave_id),
    
    # --- Calculated Variables ---
    updated_SD_ASD = as.numeric(updated_SD_ASD),
    updated_SD_TD = as.numeric(updated_SD_TD),
    Hedge_g = as.numeric(Hedge_g),
    Hedge_g_SE = as.numeric(Hedge_g_SE),
    Hedge_g_var = as.numeric(Hedge_g_var),
    ES_source = as.character(ES_source),
    
    cat_age = as.numeric(cat_age),
    female_per = as.numeric(female_per)
  )



# ------ DEMO ------
df_new <- df_new %>%
  mutate(
    cat_age = case_when(
      !is.na(Mage_ASD) & Mage_ASD >= 0   & Mage_ASD <= 7.5  ~ 6,
      !is.na(Mage_ASD) & Mage_ASD > 7.5  & Mage_ASD <= 13.5 ~ 12,
      !is.na(Mage_ASD) & Mage_ASD > 13.5 & Mage_ASD <= 19.5 ~ 18,
      !is.na(Mage_ASD) & Mage_ASD > 19.5 & Mage_ASD <= 25  ~ 24,
      TRUE ~ NA_real_
    )
  )

df_new <- df_new %>%
  mutate(
    female_per =
      N_female / N_ASD
  )


# ------ Update SD using SD, SE, or CI ------
df_new <- df_new %>%
  mutate(
    # ASD Group
    updated_SD_ASD = case_when(
      
      # 5.1. use original SD
      !is.na(SD_ASD) ~ SD_ASD,
      
      # 5.2. calculate SD from SE
      is.na(SD_ASD) &
        !is.na(SE_ASD) &
        !is.na(N_ASD) ~
        SE_ASD * sqrt(N_ASD),
      
      # 5.3. calculate from CI
      is.na(SD_ASD) &
        is.na(SE_ASD) &
        !is.na(CI_lower_ASD) &
        !is.na(CI_upper_ASD) &
        !is.na(N_ASD) ~
        
        (
          abs(CI_upper_ASD - CI_lower_ASD) /
            (2 * qt(0.975, df = N_ASD - 1))
        ) * sqrt(N_ASD),
      
      # otherwise missing
      TRUE ~ NA_real_
    ),
    
    # TD group
    updated_SD_TD = case_when(
      
      !is.na(SD_TD) ~ SD_TD,
      
      is.na(SD_TD) &
        !is.na(SE_TD) &
        !is.na(N_TD) ~
        SE_TD * sqrt(N_TD),
      
      is.na(SD_TD) &
        is.na(SE_TD) &
        !is.na(CI_lower_TD) &
        !is.na(CI_upper_TD) &
        !is.na(N_TD) ~
        
        (
          abs(CI_upper_TD - CI_lower_TD) /
            (2 * qt(0.975, df = N_TD - 1))
        ) * sqrt(N_TD),
      
      TRUE ~ NA_real_
    )
  )


# ------ Update SD using SD, SE, or CI ------

df_new <- df_new %>%
  mutate(
    
    updated_SD_ASD = case_when(
      
      # use original SD
      !is.na(SD_ASD) ~ SD_ASD,
      
      # calculate SD from SE
      is.na(SD_ASD) &
        !is.na(SE_ASD) &
        !is.na(N_ASD) ~
        SE_ASD * sqrt(N_ASD),
      
      # calculate SD from CI
      is.na(SD_ASD) &
        is.na(SE_ASD) &
        !is.na(CI_lower_ASD) &
        !is.na(CI_upper_ASD) &
        !is.na(N_ASD) ~
        
        (
          abs(CI_upper_ASD - CI_lower_ASD) /
            (2 * qt(0.975, df = N_ASD - 1))
        ) * sqrt(N_ASD),
      
      TRUE ~ NA_real_
    ),
    
    
    updated_SD_TD = case_when(
      
      !is.na(SD_TD) ~ SD_TD,
      
      is.na(SD_TD) &
        !is.na(SE_TD) &
        !is.na(N_TD) ~
        SE_TD * sqrt(N_TD),
      
      is.na(SD_TD) &
        is.na(SE_TD) &
        !is.na(CI_lower_TD) &
        !is.na(CI_upper_TD) &
        !is.na(N_TD) ~
        
        (
          abs(CI_upper_TD - CI_lower_TD) /
            (2 * qt(0.975, df = N_TD - 1))
        ) * sqrt(N_TD),
      
      TRUE ~ NA_real_
    )
  )


# ------ Calculate Hedge's g using priority strategy ------

df_new <- df_new %>%
  mutate(
    
    # Priority 1: Mean + SD
    has_mean_sd =
      !is.na(Mean_ASD) &
      !is.na(Mean_TD) &
      !is.na(updated_SD_ASD) &
      !is.na(updated_SD_TD) &
      !is.na(N_ASD) &
      !is.na(N_TD),
    
    
    # Priority 2: reported Cohen's d
    has_cohen_d =
      !has_mean_sd &
      !is.na(Eff_size) &
      !is.na(Eff_size_type) &
      str_detect(
        str_to_lower(Eff_size_type),
        "cohen|d"
      ) &
      !is.na(N_ASD) &
      !is.na(N_TD),
    
    # Priority 3: Mean Difference + SE
    has_smd_se =
      !has_mean_sd &
      !has_cohen_d &
      !is.na(Mean_Diff) &
      !is.na(Mean_Diff_SE) &
      !is.na(N_ASD) &
      !is.na(N_TD),
    
    
    # Priority 4: t value
    has_t =
      !has_mean_sd &
      !has_cohen_d &
      !has_smd_se &
      !is.na(t) &
      !is.na(N_ASD) &
      !is.na(N_TD),
    
    
    # Calculate original Hedge's g
    Hedge_g = case_when(
      
      # Priority 1
      has_mean_sd ~ pmap_dbl(
        list(
          Mean_ASD,
          Mean_TD,
          updated_SD_ASD,
          updated_SD_TD,
          N_ASD,
          N_TD
        ),
        function(
    Mean_ASD,
    Mean_TD,
    updated_SD_ASD,
    updated_SD_TD,
    N_ASD,
    N_TD
        ){
          
          esc_mean_sd(
            grp1m = Mean_ASD,
            grp2m = Mean_TD,
            grp1sd = updated_SD_ASD,
            grp2sd = updated_SD_TD,
            grp1n = N_ASD,
            grp2n = N_TD,
            es.type = "g"
          )$es
        }
      ),
    
    # Priority 2
    has_cohen_d ~ hedges_g(
      d = Eff_size,
      totaln = N_ASD + N_TD
    ),
    
    # Priority 3
    has_smd_se ~ pmap_dbl(
      list(
        Mean_Diff,
        Mean_Diff_SE,
        N_ASD,
        N_TD
      ),
      function(
    Mean_Diff,
    Mean_Diff_SE,
    N_ASD,
    N_TD
      ){
        t_value <- Mean_Diff / Mean_Diff_SE
        df <- N_ASD + N_TD - 2
        d <- t_value * sqrt(
          (N_ASD + N_TD) /
            (N_ASD * N_TD)
        )
        J <- 1 - (3 / (4 * df - 1))
        J * d
      }
    ),
    
    # Priority 4
    has_t ~ pmap_dbl(
      list(t, N_ASD, N_TD),
      function(t, N_ASD, N_TD){
        esc_t(
          t = t,
          grp1n = N_ASD,
          grp2n = N_TD,
          es.type = "g"
        )$es
      }
    ),
    
    TRUE ~ NA_real_
    ),
    
    # Calculate SE of Hedge's g
    Hedge_g_SE = case_when(
      
      # Priority 1
      has_mean_sd ~ pmap_dbl(
        list(
          Mean_ASD,
          Mean_TD,
          updated_SD_ASD,
          updated_SD_TD,
          N_ASD,
          N_TD
        ),
        function(
    Mean_ASD,
    Mean_TD,
    updated_SD_ASD,
    updated_SD_TD,
    N_ASD,
    N_TD
        ){
          
          esc_mean_sd(
            grp1m = Mean_ASD,
            grp2m = Mean_TD,
            grp1sd = updated_SD_ASD,
            grp2sd = updated_SD_TD,
            grp1n = N_ASD,
            grp2n = N_TD,
            es.type = "g"
          )$se
        }
      ),
    
    # Priority 2
    has_cohen_d ~ sqrt(
      ((N_ASD + N_TD) /
         (N_ASD * N_TD)) +
        (Hedge_g^2 /
           (2 * (N_ASD + N_TD - 2)))
    ),
    
    # Priority 3
    has_smd_se ~ pmap_dbl(
      list(
        Mean_Diff,
        Mean_Diff_SE,
        N_ASD,
        N_TD
      ),
      function(
    Mean_Diff,
    Mean_Diff_SE,
    N_ASD,
    N_TD
      ){
        t_value <- Mean_Diff / Mean_Diff_SE
        df <- N_ASD + N_TD - 2
        d <- t_value * sqrt(
          (N_ASD + N_TD) /
            (N_ASD*N_TD))
        J <- 1 - (3/(4*df-1))
        g <- J*d
        sqrt(
          ((N_ASD+N_TD)/(N_ASD*N_TD)) +
            (g^2/(2*(N_ASD+N_TD-2)))
        )}),
    
    # Priority 4
    has_t ~ pmap_dbl(
      list(t, N_ASD, N_TD),
      function(t, N_ASD, N_TD){
        
        esc_t(
          t = t,
          grp1n = N_ASD,
          grp2n = N_TD,
          es.type = "g"
        )$se
      }
    ),
    
    TRUE ~ NA_real_
    ),
    
    
    ES_source = case_when(
      has_mean_sd ~ "Mean_SD",
      has_cohen_d ~ "Reported_Cohen_d",
      has_smd_se ~ "Mean_Difference_SE",
      has_t ~ "t_value",
      TRUE ~ NA_character_
    )
    
  ) %>%
  select(
    -has_mean_sd,
    -has_cohen_d,
    -has_smd_se,
    -has_t
  )

# ------ Calculate variance ------

df_new <- df_new %>%
  mutate(Hedge_g_var = Hedge_g_SE^2)

# ------ Direction Correction ------
# direction == 1 means:
# SE and variance DO NOT change

df_new <- df_new %>%
  mutate(
    Hedge_g_updated = case_when(
      direction == 1 ~ -1 * Hedge_g,
      TRUE ~ Hedge_g
    ),
    
    # keep unchanged
    Hedge_g_SE_updated = Hedge_g_SE,
    
    # variance unchanged
    Hedge_g_var_updated = Hedge_g_var
  )

write_csv(df_new, "\\Users\\ZXMLAB\\Desktop\\meta\\data_analysis\\processed_dataset.csv")
