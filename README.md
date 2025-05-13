# A Novel Approach to Non-Parametric Two-Way ANOVA

This project presents a novel bootstrap-permutation-based alternative to the traditional two-way ANOVA. It aims to provide a flexible and assumption-light method to test for main and interaction effects when data violate common assumptions such as normality and homoscedasticity.

## 🔍 Problem

Classical two-way ANOVA relies heavily on assumptions like normality, equal variances, and absence of outliers. This project introduces a robust non-parametric approach that handles:

- Heteroscedasticity
- Non-normal distributions
- Interaction effects
- Outliers

## ⚙️ Methodology

- **Bootstrap Resampling**: Used to estimate effect sizes under minimal assumptions.
- **Permutation Tests**: Applied to generate empirical p-values for each main and interaction effect.
- **Diagnostics**: Includes visual tools for checking normality, homoscedasticity, and assumption violations.

## 📂 Folder Guide

- `/codes/`: All R functions, test scripts, and diagnostics
- `/data/`: Real-world dataset used
- `/report/`: Final report and presentation slides
- `/vignette/`: A walk-through of how to use the method on example data

## 🧪 How to Use

1. Download Permova package (Permova_0.1.0.tar.gz folder ) 
2. Install 'Permova' library in R
3. Run the 'stratified_perm_test_sequential' function for your data
4. Explore the vignette in `/vignette/` for step-by-step instructions

## 📚 Tools Used

- R 
- Bootstrap & Permutation techniques
- Custom plotting functions

## 📜 License

This project is licensed under the MIT License. See the LICENSE file for more info.
