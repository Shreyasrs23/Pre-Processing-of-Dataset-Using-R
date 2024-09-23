# Data Preprocessing and Analysis

## Overview
This project focuses on data preprocessing, cleaning, transformation, and visualization steps performed on a dataset related to term deposit subscription. The workflow includes handling missing values, removing duplicates, treating outliers, noise removal, feature scaling, encoding categorical values, and feature selection for modeling. Furthermore, it utilizes techniques like Principal Component Analysis (PCA) for dimensionality reduction and explores various plots to gain insights into the data.

## Table of Contents
- [Installation](#installation)
- [Dataset](#dataset)
- [Data Preprocessing](#data-preprocessing)
  - [Handling Missing Values](#handling-missing-values)
  - [Removing Duplicates](#removing-duplicates)
  - [Outlier Treatment](#outlier-treatment)
  - [Noise Removal](#noise-removal)
  - [Data Transformation](#data-transformation)
  - [Encoding Categorical Values](#encoding-categorical-values)
  - [Feature Selection](#feature-selection)
  - [Dimensionality Reduction](#dimensionality-reduction)
- [Visualization](#visualization)
- [Results](#results)
- [How to Use](#how-to-use)
- [License](#license)

## Installation
1. Install the required libraries in R by running the following commands:
   ```r
   install.packages('ggplot2')
   install.packages('dplyr')
   install.packages('gplots')
   install.packages("tidyr")
   install.packages("caret")
   install.packages('data.table')
   install.packages('stringr')
   ```

2. Load the dataset into your R environment:
   ```r
   DATASET <- read.csv("path_to_your_dataset/dataset_DT.csv")
   ```

## Dataset
The dataset used in this project is related to a marketing campaign where customers are contacted to subscribe to a term deposit. The target variable `y` indicates whether the client has subscribed (`yes` or `no`).

Key features include:
- **age, job, marital status, education, default status**
- **balance, housing loan status, personal loan status, contact method**
- **number of contacts made, days since last contact, previous campaign outcome**

## Data Preprocessing
### Handling Missing Values
- Missing values represented by `"unknown"` or blank cells were replaced by `NA`.
- **Numerical columns**: Missing values were replaced using **median imputation**.
- **Categorical columns**: Missing values were replaced using **mode imputation**.

### Removing Duplicates
- Checked for duplicate rows. If found, they would be removed using the `distinct()` function.

### Outlier Treatment
- **Boxplots** were used to visualize outliers.
- Outliers were treated using the **IQR method**. Rather than removing them, outliers were capped by the lower and upper bounds (winsorization).

### Noise Removal
- Negative values in columns where they were logically incorrect (e.g., balance, days since last contact) were replaced with `0`.

### Data Transformation
- **Normalization and Scaling**: Numerical values were scaled using `caret` library's `preProcess()` function, transforming them into a range of `[0, 1]`.

### Encoding Categorical Values
- **Label Encoding** was used to convert categorical variables into numerical values. One-hot encoding was not used to keep the dimensionality simple.

### Feature Selection
- **Correlation matrix** was used to identify the six features most strongly correlated with the target variable (`y`).
- Top six features identified: `duration`, `housing`, `previous`, `education`, `loan`, `pdays`.

### Dimensionality Reduction
- **Principal Component Analysis (PCA)** was performed to reduce dimensionality while retaining 90% of the variance.
- PCA identified the most significant components for the model.

## Visualization
Several visualizations were created to explore the data:
1. **Box Plots**: Before and after outlier treatment and scaling.
2. **Scatter Plot**: Age vs. Balance, colored by the target variable `y`.
3. **Line Plot**: Job category vs. subscription status (`y`).
4. **Line Graphs**: Numerical features plotted against the target variable for comparison.

## Results
- The preprocessing steps effectively cleaned the dataset, removed noise, and dealt with outliers without losing valuable data.
- Dimensionality reduction through PCA allowed us to focus on the most significant features, improving model performance and reducing computational complexity.
- Visualizations provided valuable insights into relationships between key features and the target variable.

## How to Use
1. Clone or download this repository.
2. Open and run the R script in your R environment.
3. Replace the dataset path in the `read.csv()` function with the actual location of your dataset.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
