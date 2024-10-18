###project for "Programming in R"###

###set working directory###
setwd("D:/Dokumente/Master Quantitative Data Science Methods/Programming in R")
getwd()

###import dataset###
#https://www.kaggle.com/datasets/prachi13/employeeattritionrate
data_job_satisfaction <- read.csv("D:/Dokumente/Master Quantitative Data Science Methods/Programming in R/project/Daten job Statisfaction.csv", sep=";")
View(data_job_satisfaction)

###load libraries###
library(lavaan)
library(car)
library(mosaic)
library(apaTables)
library(psych)
library(ggplot2)
library(rcompanion)
library(dplyr)
library(rlang)


### step 1: data preprocessing ###----------------------------------------------------------------------------------

# changing presentation of outputs
options(scipen = 999)

# exploring dimensions of data
dim(data_job_satisfaction) 

# displaying the column names  
variable_names <- names(data_job_satisfaction) 
print(variable_names) 

# checking the data types of variables(column) 
str(data_job_satisfaction)

# creating a new dataframe with encoded values
data_job_satisfaction_en <- data_job_satisfaction

# recoding categorical variables & substitute previous column with numeric values
unique(data_job_satisfaction$Attrition)
data_job_satisfaction_en$Attrition <- Recode(data_job_satisfaction$Attrition, "'No' = 0; 'Yes' = 1")
unique(data_job_satisfaction$Department)
data_job_satisfaction_en$Department <- Recode(data_job_satisfaction$Department, "'Sales' = 0; 'Research & Development' = 1; 'Human Resources' = 2")
unique(data_job_satisfaction$EducationField)
data_job_satisfaction_en$EducationField <- Recode(data_job_satisfaction$EducationField, "'Life Sciences' = 0; 'Medical' = 1; 'Marketing' = 2; 'Technical Degree'=3; 'Human Resources'=4; 'Other'=5")
unique(data_job_satisfaction$MaritalStatus)
data_job_satisfaction_en$MaritalStatus <- Recode(data_job_satisfaction$MaritalStatus, "'Single' = 0; 'Married' = 1; 'Divorced' = 2")


# checking data after recoding
str(data_job_satisfaction_en)

# changing Human Resources into HR for better readability while plotting
data_job_satisfaction$EducationField <- Recode(data_job_satisfaction$EducationField, "'Human Resources' = 'HR'")
head(data_job_satisfaction)

# recoding ordinal variables for plotting (for the labels of x-axis)
data_job_satisfaction$JobSatisfaction_r <- factor(data_job_satisfaction$JobSatisfaction, 
                                                  levels = c(1, 2, 3, 4), 
                                                  labels = c("Low", "Medium", "High", "Very high"))

data_job_satisfaction$Education_r <- factor(data_job_satisfaction$Education,
                                            levels = c(1, 2, 3, 4, 5),
                                            labels = c("Below College", "College", "Bachelor", "Master", "Doctor"))

data_job_satisfaction$EnvironmentSatisfaction_r <- factor(data_job_satisfaction$EnvironmentSatisfaction,
                                                          levels = c(1, 2, 3, 4), 
                                                          labels = c("Low", "Medium", "High", "Very high"))

data_job_satisfaction$WorkLifeBalance_r <- factor(data_job_satisfaction$WorkLifeBalance,
                                                  levels = c(1, 2, 3, 4),
                                                  labels = c("Bad","Good","Better","Best"))

# checking for missing values
#is.na(data_job_satisfaction)
# no NAs and no NANs in the dataset

### step 2: descriptive analysis ###-------------------------------------------------------------------------------
describe(data_job_satisfaction)
summary(data_job_satisfaction)

# searching path
search()
# creating a new environment for plotting functions
plots <- new.env()

### function to plot nominal variables ###
plots$plot_cat <- function(UV, column_name) {
  plot_env <- current_env()
  ggplot(data_job_satisfaction, aes(x = {{ UV }}, fill = Attrition)) +        
    geom_bar(position="dodge", stat="count") +
    labs(x = column_name, y = "Count", title = paste("Attrition by", column_name))
  }
plots$plot_cat(Department,"Department")
plots$plot_cat(EducationField, "EducationField")
plots$plot_cat(MaritalStatus, "MaritalStatus")

### function to plot ordinal variables ###
plots$plot_ord <- function(UV, column_name) {
  ggplot(data_job_satisfaction, aes(x = ({{ UV }}), fill = Attrition)) +
    geom_bar(position = "dodge", stat="count") +
    labs(x = column_name, y = "Count", title = paste("Attrition by", column_name)) +
    scale_fill_discrete(name = "Attrition")
}
plots$plot_ord(JobSatisfaction_r, "JobSatisfaction") 
plots$plot_ord(Education_r, "Education")
plots$plot_ord(EnvironmentSatisfaction_r, "EnvironmentSatisfaction")
plots$plot_ord(NumCompaniesWorked, "NumCompaniesWorked")
plots$plot_ord(WorkLifeBalance_r, "WorkLifeBalance")

### function to plot metric variables ### 
plots$plot_met <- function(UV, column_name) {
  ggplot(data_job_satisfaction, aes(x = Attrition, y = {{UV}})) +
    geom_boxplot() +
    labs(x = "Attrition", y = column_name) +
    theme_minimal()
}
plots$plot_met(MonthlyIncome, "MonthlyIncome")
plots$plot_met(Age, "Age")
plots$plot_met(DistanceFromHome, "DistanceFromHome")
plots$plot_met(YearsAtCompany, "YearsAtCompany")
plots$plot_met(NumCompaniesWorked, "NumCompaniesWorked")


### step 3: statistical analysis ###-------------------------------------------------------------------------------
### Analysis 1 ###-------------------------------------------------------------------------------------------------

# H1: employees that are single terminate their job more likely, than employees who are married or divorced
# IV: MaritalStatus (nominal), DV: Attrition (nominal)
# chi_square_test

# requirements------------------------------------------------------------------------------------------------------
# 1. variables are categorical
class(data_job_satisfaction$Attrition) #character
class(data_job_satisfaction$MaritalStatus) #character
# fulfilled

# 2. every row has 5 or more observations
if (nrow(data_job_satisfaction) > 5) {
  print("Dataset has more than 5 rows")
} else {
  print("Dataset has less than 5 rows")
}
# fulfilled

# 3. independence of data -> fulfilled

# descriptive analysis for variables of interest--------------------------------------------------------------------
tally(MaritalStatus ~ Attrition, data = data_job_satisfaction)

cross_table <- prop.table(table(data_job_satisfaction$MaritalStatus, data_job_satisfaction$Attrition))
print(cross_table)

# chi-square-test---------------------------------------------------------------------------------------------------
xchisq.test(MaritalStatus~Attrition, data = data_job_satisfaction)
# p-value < alpha 
# -> reject H0
# -> significant relationship between marital status and attrition

# effect size:  Cohen's Omega
cohenW(data_job_satisfaction$MaritalStatus, data_job_satisfaction$Attrition)
# cohens W = 0.1772 -> weak effect

# group-wise comparison----------------------------------------------------------------------------------------------
# group1 (single vs. divorced)
# creating a subset of the data
subset_group_1 <- subset(data_job_satisfaction, MaritalStatus %in% c("Single", "Divorced"))

# chi-square test for group1
xchisq.test(MaritalStatus~Attrition, data = subset_group_1)
# p-value < alpha
# -> reject H0
# -> significant difference in attrition between single employees and divorced employees

cohenW(subset_group_1$MaritalStatus, subset_group_1$Attrition)
# cohens W = 0.1928 -> weak effect

# group2 (single vs. married)
# creating a subset of data
subset_group_2 <- subset(data_job_satisfaction, MaritalStatus %in% c("Single", "Married"))

# chi-square-test for group 2
xchisq.test(MaritalStatus~Attrition, data = subset_group_2)
# p-value < alpha
# -> reject H0
# -> significant difference in attrition between single employees and married employees

cohenW(subset_group_2$MaritalStatus, subset_group_2$Attrition)
# cohen's W = 0.1677 -> weak effect

# group3 (divorced vs. married)
# creating a subset of the data
subset_group_3 <- subset(data_job_satisfaction, MaritalStatus %in% c("Married", "Divorced"))
# chi-square-tests for group 3
xchisq.test(MaritalStatus~Attrition, data = subset_group_3)
# p-value > alpha
# -> don't reject H0
# -> no significant difference in attrition between married employees and divorced employees

### Analysis 2 ###----------------------------------------------------------------------------------------------------

# H2: Environment satisfaction has a higher predictive power for attrition than work-life-balance
# logistic regression 

# null model
reg0 <- glm(Attrition ~ 1, data = data_job_satisfaction_en, family = binomial())
summary(reg0)

# creating a new environment
goodness_of_fit <- new.env()
### function for goodness of fit ###
goodness_of_fit$logisticR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}

# creating a new environment
odds_ratio <- new.env()
### function for OR ###
odds_ratio$OR <- function(LogModel){
  exp(cbind(OR = coef(LogModel), confint(LogModel)))
}

#looking up the global environment
ls()

##### 1. regression model ####-------------------------------------------------------------------------------------------
# IV: WorkLifeBalance (ordinal), DV: Attrition (binary)
reg1 <- glm(Attrition ~ WorkLifeBalance, data = data_job_satisfaction_en, family = 'binomial')
summary(reg1)
#p-value < alpha -> significant

goodness_of_fit$logisticR2s(reg1)
odds_ratio$OR(reg1)

#check requirements------------------------------------------------------------------------------------------------------
# 1. IV needs to be categorical, DV needs to be at least categorical
is.object(data_job_satisfaction$Attrition); typeof(data_job_satisfaction$Attrition); class(data_job_satisfaction$Attrition)
#character 
is.object(data_job_satisfaction$JobSatisfaction); typeof(data_job_satisfaction$JobSatisfaction); class(data_job_satisfaction$JobSatisfaction)
#integer

# 2. Independence of errors
durbinWatsonTest(reg1)
#D-W Statistic between 1 and 3 -> good
#ideally very close to 2 -> given
#p-value < alpha -> significant -> independence of errors not given based on significance level

 

##### 2. regression model ####--------------------------------------------------------------------------------------------
# IV:WorkLifeBalance (ordinal), EnvironmentSatisfaction (ordinal), DV: Attrition (binary)
reg2 <- glm(Attrition ~ EnvironmentSatisfaction + WorkLifeBalance, data = data_job_satisfaction_en, family = 'binomial')
summary(reg2)

goodness_of_fit$logisticR2s(reg2)
odds_ratio$OR(reg2)

# calculating p-value
modelchi <- reg0$deviance - reg1$deviance
cat("deviance", modelchi)  
chidf <- reg0$df.null - reg1$df.residual
cat("df", chidf )
chisqp <- 1-pchisq(modelchi, chidf)
cat("p-value ", chisqp)
# p-value < alpha -> reject H0

# check requirements------------------------------------------------------------------------------------------------------
# 1. DV needs to be binary, IV needs to be at least categorical
is.object(data_job_satisfaction$MonthlyIncome); typeof(data_job_satisfaction$MonthlyIncome); class(data_job_satisfaction$MonthlyIncome)
# integer

# 2. Independence of errors
durbinWatsonTest(reg2)
#D-W Statistic between 1 and 3 -> good
#ideally very close to 2 -> given
#p-value < alpha -> significant -> independence of errors not given based on significance level

# 3. multikollinearity (needs to be < 10)
vif(reg2)
1/vif(reg2)
# fulfilled 

### Model comparison (reg1, reg2) ###--------------------------------------------------------------------------------------
anova(reg1, reg2)

# calculating p-value 
modelchi <- reg1$deviance - reg2$deviance
cat("deviance", modelchi)  
chidf <- reg1$df.null - reg2$df.residual
cat("df", chidf )
chisqp <- 1-pchisq(modelchi, chidf)
cat("p-value ", chisqp)
# p-value < alpha -> reject H0


