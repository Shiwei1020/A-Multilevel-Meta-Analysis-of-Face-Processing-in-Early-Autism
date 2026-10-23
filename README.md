# A-Multilevel-Meta-Analysis-of-Face-Processing-in-Early-Autism
As the first author of this research project, I took the lead in developing and implementing the R code for the meta-analysis, including data processing, statistical modeling, and results visualization.

| File | Description |
| --- | --- |
| `01_data_processing_sw(4).R` | Imports the extracted study data, standardizes variables and group labels, derives demographic variables, calculates Hedges' *g* from available summary statistics, records effect-size sources, and harmonizes effect-size directions. Exports `processed_dataset.csv`. |
| `02_descriptive_statistics_sw(5).R` | Summarizes study counts, publication years, effect-size counts, participant ages, group distributions, and counts for selected moderators. |
| `03_meta_analysis_sw(3).R` | Fits the primary three-level random-effects model, estimates variance decomposition, conducts cohort- and wave-structured sensitivity models and CR2 robust inference, produces forest and funnel plots, examines small-study effects, identifies outliers, and performs leave-one-paper-out analyses. |
| `04_moderators_analysis_sw(20261023-032351).R` | Conducts categorical and continuous meta-regressions and interaction analyses for age, sex composition, ASD likelihood/diagnostic groups, stimulus characteristics, task paradigms, memory requirements, and recording modalities. |
| `05_visualization_sw(1).R` | Produces plots for meta-regression and interaction results, including predicted age trends with confidence intervals, subgroup comparisons, and bubble plots. |
