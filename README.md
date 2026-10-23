
# A Multilevel Meta-Analysis of Face Processing in Early Autism

This repository contains R scripts for data processing, descriptive statistics, multilevel meta-analysis, moderator analyses, and visualization of results in a research project on face processing in early autism.

## Code Files

### 01 — Data Processing (`01_data_processing_sw.R`)
Imports extracted study data, standardizes variables and group labels, derives demographic variables, calculates Hedges' g from available summary statistics, records effect-size sources, and harmonizes effect-size directions. Exports `processed_dataset.csv`.

### 02 — Descriptive Statistics (`02_descriptive_statistics_sw.R`)
Summarizes study counts, publication years, effect-size counts, participant ages, group distributions, and selected moderators.

### 03 — Meta-Analysis (`03_meta_analysis_sw.R`)
Fits the primary three-level random-effects model, estimates variance components, runs cohort- and wave-structured sensitivity models and CR2 robust inference, generates forest and funnel plots, assesses small-study effects, identifies outliers, and conducts leave-one-paper-out analyses.

### 04 — Moderator Analyses (`04_moderators_analysis_sw.R`)
Conducts categorical and continuous meta-regressions and interaction analyses involving age, sex composition, ASD likelihood/diagnostic groups, stimulus characteristics, task paradigms, memory requirements, and recording modalities.

### 05 — Visualization (`05_visualization_sw.R`)
Creates visualizations of meta-regression and interaction results, including predicted age trends with confidence intervals, subgroup comparisons, and bubble plots.

## Author Contribution

**Shiwei Wang — First Author**

As the first author of this research project, I took the lead in developing and implementing the R analysis workflow, including data processing, statistical modeling, moderator analyses, and results visualization.
