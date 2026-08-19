# Student Dropout Analysis Using R

## Project Overview
This project analyzes student dropout and academic success using R. The goal is to understand which academic, financial, demographic, and macroeconomic factors are most strongly associated with student dropout.

The project uses the Student Dropout and Academic Success dataset from the UCI Machine Learning Repository. The dataset includes 4,424 student records and 37 variables, with the target outcome classified as Dropout, Graduate, or Enrolled.

## Business Problem
Student dropout is a major challenge for universities because it affects student success, institutional completion rates, tuition revenue, and long-term career outcomes. This project explores how data analytics and predictive modeling can help identify dropout risk factors and support earlier intervention.

## Research Objectives
This project focuses on:

- Understanding the distribution of student outcomes
- Identifying academic, financial, and demographic variables associated with dropout
- Creating exploratory data visualizations using R
- Performing hypothesis testing
- Demonstrating the `corrplot` package for correlation and multicollinearity analysis
- Building and comparing logistic regression and random forest models
- Interpreting model performance and feature importance

## Dataset
- Source: UCI Machine Learning Repository
- Dataset: Student Dropout and Academic Success
- Records: 4,424
- Variables: 37
- Target variable: Dropout / Graduate / Enrolled

The project converts the original student outcome into a binary dropout target for modeling:

- Dropout
- Non-Dropout

## Tools and Packages Used
- R
- RStudio
- ggplot2
- dplyr
- corrplot
- randomForest
- caret
- scales
- pROC
- Base R statistics

## Analysis Workflow
The project follows this workflow:

1. Load and inspect the dataset
2. Clean and prepare variables
3. Create binary dropout target
4. Create readable factor labels
5. Perform exploratory data analysis
6. Generate eight visualizations using ggplot2
7. Conduct hypothesis testing
8. Use corrplot for multicollinearity screening
9. Prepare data for modeling
10. Build logistic regression model
11. Build random forest model
12. Compare models using ROC-AUC
13. Analyze feature importance
14. Summarize findings and recommendations

## Key Visualizations
The project includes visualizations such as:

- Student outcome distribution
- Dropout rate by scholarship status
- Dropout rate by gender
- Age at enrollment by outcome
- 2nd-semester approved units by outcome
- Admission grade distribution by outcome
- Dropout rate by age group
- Financial risk heatmap
- Correlation matrix using corrplot
- Random forest variable importance
- ROC curve comparison

## Key Findings
Some key findings from the analysis include:

- 2nd-semester academic performance is one of the strongest indicators of dropout risk
- Scholarship holders show lower dropout rates than non-scholarship holders
- Students with financial risk indicators, such as debtor status and tuition not being current, show higher dropout risk
- Older students have higher dropout risk, likely due to external responsibilities
- Random forest and logistic regression were compared to evaluate predictive performance

## Files in This Repository
- `student_dropout_analysis.R` — main R script
- `student_dropout_analysis.Rmd` — R Markdown project file
- `students_dropout_academic_success.csv` — dataset used for analysis
- `docs/Student_Dropout_R_Analysis_Report.pdf` — full project report

## Project Report
The full report is available here:

[Student Dropout R Analysis Report](docs/Student_Dropout_R_Analysis_Report.pdf)

## Skills Demonstrated
- R programming
- Exploratory data analysis
- Data visualization using ggplot2
- Data wrangling using dplyr
- Hypothesis testing
- Correlation analysis using corrplot
- Logistic regression
- Random forest modeling
- ROC-AUC model comparison
- Feature importance interpretation
- Analytical reporting
