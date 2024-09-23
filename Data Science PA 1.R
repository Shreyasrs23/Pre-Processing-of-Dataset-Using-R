# installing all the required libraries

install.packages('ggplot2')
install.packages('dplyr')
install.packages('gplots')
install.packages("tidyr")
install.packages("caret")
install.packages('data.table')
install.packages('stringr')

#Loading the dataset

DATASET<-read.csv("C:\\Users\\Shreyas\\Downloads\\UTA\\DASC 5301 Data Science\\dataset_DT.csv")

head(DATASET)      #view first six rows of Dataset

tail(DATASET)      #view last six rows of Dataset

dim(DATASET)       #view the dimensions of the Dataset 

data<-DATASET  #making a copy of dataset

#DATA PRE-PROCESSING

# 1. Handling Missing Values

data[data=="unknown" | data==""]<- NA  #re-writing all "unknown" and blank values as NA

head(data)

missing_percentage <- colSums(is.na(data)) / nrow(data) * 100    #calculate the missing value percentage
print(missing_percentage)                   # Display the missing percentage for each column

#As poutcome has more than 70% of missing values, this column will not contribute anything to the model and hence can be dropped

data <- data %>%
  select(-poutcome)

dim(data) 

data <- data %>% 
  mutate_if(is.numeric, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))  #we use imputation technique that is Median replacement for all numerical values

Mode <- function(x) {                      #function to calculate mode for categorical values
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data <- data %>% 
  mutate_if(is.character, ~ ifelse(is.na(.), Mode(.), .))  #we use imputation technique that is Mode for replacement for all categorical values

missing_percentage <- colSums(is.na(data)) / nrow(data) * 100    #calculate the missing value percentage after replacement
print(missing_percentage)

head(data)

summary(data)         #to check that all NA values have been removed

#Now the data is accurate and complete

# 2. Removing Duplicate Values (Data Cleaning Process)

duplicates <- data %>% 
  filter(duplicated(.) | duplicated(., fromLast = TRUE))

print(duplicates)

#No Duplicate Rows present and hence we do not have to remove any duplicates.

#in case there were duplicate values the code to remove them is

data_cleaned <- data %>%
  distinct()

dim(data_cleaned)

#As we see that the number of rows did not change as there is no duplicate data

colnames(data_cleaned)

#And also from seeing the columns, we realize that there are no Primary key attributes where all values should be distinct and hence if the values are repeated within the column it will not be an issue for the training

#This is the end of Data Cleaning Stage

# 3. Removing Outliers from the data

#First Lets plot the box plot, to visualize the outliers, Q1, Q3 and IQR

data_long <- data_cleaned %>%
  select(where(is.numeric)) %>%  # Select only numeric columns
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value")                #converts the numerical data to long type so that its easy to plot

ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplot of Numeric Columns with Outliers",
       x = "Columns",
       y = "Values") +
  theme_minimal()               #plotting the Box plot for all the numerical value columns in a single Box plot with x axis as Column names and Y value as the actual Values


#Next lets calculate the number of outliers present, Here I have used the method of IQR to classify the outliers

remove_outliers <- function(x) {              #Function to calculate IQR , Q1, Q3 , Lower bound and Upper Bound
  if (is.numeric(x)) {  
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 2.0 * IQR_value
    upper_bound <- Q3 + 2.0 * IQR_value
    
    x <- ifelse(x < lower_bound, lower_bound, x)
    x <- ifelse(x > upper_bound, upper_bound, x)        #I have used the method of capping the data outliers, that is replace outlier below Q1 by Q1 and outlier value above Q3 as Q3.
  }
  return(x)
}


#There are many methods for Dealing with outliers such as Removal, Transformation of values, Winsorization, Imputation and regularization.
#Here I am using capping to avoid removal of the outliers and hence to stop losing many number of rows as data is valuable.

data_outliers_removed <- data_cleaned %>% mutate(across(where(is.numeric), remove_outliers)) #calling the function to detect all outliers

dim(data_cleaned)
dim(data_outliers_removed)

#Now the data is of same dimension, that is no rows have been lost and the outliers have been capped and updated.

# 4. Dealing with Noise in Data

negative_counts <- sapply(data_outliers_removed, function(x) {      #Function to identify all the noise(Negative data in columns)
  if (is.numeric(x)) {
    sum(x < 0, na.rm = TRUE)
  } else {
    NA
  }
})

negative_summary <- data.frame(Column = names(negative_counts), Negative_Count = negative_counts)

negative_summary <- negative_summary[negative_summary$Negative_Count > 0, ]   #calling function to store all negetive value count
print(negative_summary)

#We see that we have 660 negative values in column 'Balance' and 13254 negative values in column 'pdays'. Both these column cannot have negative values.

#There are many methods to deal with Negative values such as Impuation , replace with custom values, Log transformation,  Regularization.

#Here I have used the method of replacing with 0, as there columns lowest value is 0 and it does not make sense that they have negative value. And to assign a imputed value will also not make sense as it will provide wrong information

data_noise_and_outlier_removed <- data_outliers_removed %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))

negative_counts_after <- sapply(data_noise_and_outlier_removed, function(x) {      #Function to identify all the noise(Negative data in columns)
  if (is.numeric(x)) {
    sum(x < 0, na.rm = TRUE)
  } else {
    NA
  }
})

negative_summary_after <- data.frame(Column = names(negative_counts_after), Negative_Count = negative_counts_after)

# Filter to show only columns with negative values remaining
negative_summary_after <- negative_summary_after[negative_summary_after$Negative_Count > 0, ]

# Print the summary of negative counts after replacement
print(negative_summary_after)

# 4. DATA TRANSFORMATION

#First Lets Perform Normalization and scaling operation , Here I have used the caret library and preProcess() function.

preProc <- preProcess(data_noise_and_outlier_removed[, sapply(data_noise_and_outlier_removed, is.numeric)], 
                      method = c("range"))             #creating the filter to normalize/scale the data to values between 0 and 1.

data_scaled_and_normalized <- predict(preProc, data_noise_and_outlier_removed)  #applying the scaling operation

head(data_scaled_and_normalized)

# Now the data is scaled and normalized for all numerical values to values between 0 and 1.

#Now we can say that all numerical values contribute the same way to the model training.

#Next we need to deal with the categorical Values

# 5. Encoding Categorical Values

#Here we can use two main methods which are Label Encoding and One-Hot encoding. Label Encoding is usually used for ordinal data and can also work for nominal data.

#One-Hot encoding is for Data with nominal and adds many columns for each value and specfies 1 if true and 0 if false in those columns.

#Here I have chosen Label Encoding, as all my categorical columns are ordinal values and I do not want to add more columns using One-Hot encoding to keep my dimensionality simple.

data_label_encoded_after_scaling <- as.data.frame(lapply(data_scaled_and_normalized, function(x) {       #function to encode categorical values using Label Encoding
  if (is.character(x) || is.factor(x)) {
    as.numeric(factor(x))  
  } else {
    x  # Keep numeric columns unchanged
  }
}))

head(data_label_encoded_after_scaling)

# Now all the categorical values are Label Encoded and updated to numerical values.

#Now our dataset has only numerical values and all are scaled and normalized and hence the model can read and understand all the values easily.

summary(data_label_encoded_after_scaling)

# 6. Feature Selection

#Here we will check the best 6 features that has maximum correlation with the target variable. We will not delete the other features but we will analyze the most important features

correlation_matrix <- cor(data_label_encoded_after_scaling, use = "complete.obs")  #Here I am using the correlation matrix to plot all the columns against each other with thier correlation values

# Next step is to find the absolute correlations with the target variable

cor_target <- abs(correlation_matrix[,"y"])   #Here I find the absolute correlation with the target variable

print(correlation_matrix)
print(cor_target)

# Next we need to get the top six features

#Here I sort the values of correlation vs. target in descending order and pick the best 6 excluding the target variable

top_features <- sort(cor_target, decreasing = TRUE)[2:7]  # Exclude the target variable itself

print(top_features)

# Hence we find out that duration,housing,previous,education,loan,pdays are the 6 most influential features on the Target Variable.

#This is a very important statistic as we'll know which features are the main to train the model on and which feature is causing a lot of changes in the target variable.

# 7. Dimensionality Reduction

#We can also Perform Dimensionality Reduction to avoid overfitting, Improve model performance and reduce computational complexity.

#Here for dimensionality reduction we use Principal Component Analysis(PCA)

pca_result <- prcomp(data_label_encoded_after_scaling, center = TRUE, scale. = TRUE)

summary(pca_result)    #Summary of PCA analysis

variance <- summary(pca_result)$importance[2, ]  #calculate the variance
cumulative_variance <- cumsum(variance)           #calculate the cumulative variance

# Next We plot the cumulative variance just to visualize the number of components selected

ggplot(data.frame(PC = 1:length(cumulative_variance), CumulativeVariance = cumulative_variance), 
       aes(x = PC, y = CumulativeVariance)) +
  geom_line() +
  geom_point() +
  labs(title = "Cumulative Variance Explained by PCA", x = "Principal Components", y = "Cumulative Variance") +
  theme_minimal()

num_components <- which(cumulative_variance >= 0.90)[1]   #we select the number of components upto 90% of the data, that is cumulative frequency. PCA organizes the features such that the majority contributing factor is placed first.
cat("Number of components to retain for 90% variance:", num_components, "\n")

data_reduced <- pca_result$x[, 1:num_components]   #select the chosen number of components

dim(data_label_encoded_after_scaling)    #initial dimensions before PCA
dim(data_reduced)                        #dimensions after PCA

# We can clearly say that we have left out 3 columns as they do not come under 90% of the variance of correlated data with the target variable

# Now we Get the loadings for the selected principal components
loadings <- pca_result$rotation[, 1:num_components]
print(loadings)

# Next for each principal component, get the original features with the highest contribution

# Let's find the top 10 components contributing features per principal component 

top_contributors_per_component <- apply(loadings, 2, function(x) {
  original_columns <- rownames(loadings)
  # Sort the features by absolute contribution to each component
  top_contributors <- original_columns[order(abs(x), decreasing = TRUE)[1:10]]
  return(top_contributors)
})

# We will now get a unique list of all top-contributing original features across components
significant_columns <- unique(unlist(top_contributors_per_component))

# lets print the original column names that contributed to the selected principal components
print(significant_columns)

#This helps us understand the main features for each Principal component and this can be chosen for the training and will include high variance with less number of dimensions 

#And Hence we achieve Dimensionality Reduction

# 8. Data Aggregation

#In this step we check if we can merge any similar features to reduce the Dimensionality

#I observe that We can a single column called TotalContacts to the customer and merge feature campaign and previous. This gives us the total count of contact instances that has happened.

#Further we can even merge Duration and day to get an AvaergeDuration, a single column instead of 2 seperate columns and still have the same information in a understandable manner.

data_combined <- data_label_encoded_after_scaling %>%
  mutate(
    # Creating the new aggregated features based on relevant columns
    TotalContacts = campaign + previous,                                   # Total contacts made
    AverageDuration = rowMeans(select(., duration, day), na.rm = TRUE)   # Average of duration and day
  )

# Now we can check the updated dataframe
head(data_combined)

#Here 2 new aggregated features are created and this can be used for PCA or for direct training of the model and we can discard or ignore the other original features.

# 9. VISUALIZATION

#PLOT 1.

#First We plot a scatter plot of Age Vs. Balance with relation to Target Variable Y

data_label_encoded_after_scaling$y <- as.factor(data_label_encoded_after_scaling$y)

ggplot(data_label_encoded_after_scaling, aes(x = age, y = balance, color = y)) +
  geom_jitter(alpha = 0.6, width = 0.2) +  # Jitter to avoid overplotting
  labs(title = "Age vs. Balance Colored by Subscription Status",
       x = "Age",
       y = "Balance") +
  scale_color_manual(values = c("orange", "steelblue"), 
                     labels = c("No", "Yes"), 
                     name = "Subscribed") +
  theme_minimal()


#PLOT 2.

#Next Plot is Job category(categorical value) Vs the Target Variable and it is a Line plot for both the values of Target Variable

job_summary <- as.data.frame(table(data_label_encoded_after_scaling$job, data_label_encoded_after_scaling$y))
colnames(job_summary) <- c("Job", "Subscribed", "Count")

# To Ensure that the Subscribed column is a factor
job_summary$Subscribed <- as.factor(job_summary$Subscribed)

ggplot(job_summary, aes(x = Job, y = Count, group = Subscribed, color = Subscribed)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  labs(title = "Job Category vs. Subscription Status",
       x = "Job Category",
       y = "Count") +
  scale_color_manual(values = c("orange", "steelblue"), 
                     labels = c("No", "Yes"), 
                     name = "Subscribed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


#Plot 3

#This is a line plot for all the numerical columns which are scaled and is plotted against both the values of Target Variable and all sub-plots are put in a single plot.

data_with_target <- data_label_encoded_after_scaling %>%
  mutate(y = factor(y))  # To Ensure that 'y' is a factor for coloring

# We Reshape the data to long format for easier plotting
long_data <- data_with_target %>%
  pivot_longer(cols = -y, names_to = "Feature", values_to = "Value")

ggplot(long_data, aes(x = Value, color = y, group = y)) +
  geom_line(stat = "count", aes(y = after_stat(count)), linewidth = 1) +
  facet_wrap(~ Feature, scales = "free_x") +
  labs(title = "Line Graphs of Numerical Features by Term Deposit Subscription",
       x = "Value",
       y = "Count",
       color = "Subscribed (y)") +
  theme_minimal()



# 10. Box Plot Before and After Data Preparation

#Here we plot the box plot intially for data in which only null values were removed, and had a lot of noise, duplicates and outliers.

# 1. Box plots before data preparation
data_long_before <- data %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

ggplot(data_long_before, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of Numeric Columns Before Data Preparation",
       x = "Columns",
       y = "Values") +
  theme_minimal()


#Next we plot the boxplot for data after the null values, duplicates, noise and outliers have been removed.

# 2. Box plots after data preparation

data_long_after <- data_noise_and_outlier_removed %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

ggplot(data_long_after, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of Numeric Columns After Data Preparation without Scaling",
       x = "Columns",
       y = "Values") +
  theme_minimal()



data_long_after <- data_scaled_and_normalized %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

ggplot(data_long_after, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Boxplots of Numeric Columns After Data Preparation After Scaling",
       x = "Columns",
       y = "Values") +
  theme_minimal()

#We can Clearly see the differene between the box plots and that the Outliers have been smoothened out and is very less compared to intial plot.
#Infact, there no outliers present as we removed all of them. The reason it is still showing up is that, when we plot box plot it calculates the new ranges based on passed data and anything outside that range, it plots as outliers.

#Hence Now this is the complete DATA PRE PROCESSING of the Given Data Set.

#Now we have enough clarity on the data and it is now ready for model training.

summary(data_label_encoded_after_scaling)

#From this we can select the required number of features based on PCA and Feature aggregation and then give it to the Model for Training.

head(data_label_encoded_after_scaling)     #final look of data after pre-processing

#THANK YOUUUU
