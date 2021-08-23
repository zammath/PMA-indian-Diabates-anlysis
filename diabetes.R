#Load the dataset
dataset=read.csv("diabetes.csv")
print(getwd())

#first 10 rows
head(dataset,10)
#last 10 rows
tail(dataset,10)
#column names
names(dataset)
#datatypes
str(dataset)

#identify the missing values
sum(is.na(dataset))


#Summarising dataset
summary(dataset)
str(dataset)

#We use “sapply”" to check the number of missing values in each columns.
sapply(dataset, function(x) sum(is.na(x)))
#there is no null values in each column

# Age distribution
par(mfrow = c(2, 2))

# the $ notation can be used to subset the variable you're interested in.
hist(dataset$Pregnancies)
hist(dataset$Age)
hist(dataset$Glucose)
hist(dataset$BMI)
"""Age and number of times pregnant are not normal distributions as expected since the underlying population should not be 
normally distributed either. This 392 observations are just a sample of the original population. On the other hand, the glucose 
level and BMI seem to follow a normal distribution. When performing any analysis, it is always good to know what is the 
distribution of the data so all the assumptions for different tests or models can be met."""


# Age distribution
library(ggplot2)
ggplot(dataset,aes(x=Age))+geom_histogram(binwidth=10,col="blue",fill="green")+labs(title="Age column",x="Age","Count")

#Pregnancy distribution
str(dataset$Pregnancies)
table(dataset$Pregnancies)

library(ggthemes)
library(ggplot2)
library(psych)
library(dplyr)
library(caret)

dataset$Outcome <- as.factor(dataset$Outcome)
#All 8 independent variables are numeric. There are tow outcomes, this data is good for classifciation. 
#Lets change Outcome to categorical Variable


ggplot(dataset,aes(x = Pregnancies)) +
  geom_histogram(binwidth = 0.5,aes(fill = Outcome),position = "dodge") +
  ggtitle("Pregnancies Data Distribution") + ylab("OutCode Counts") +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))

#Pregnancies data is right skewed.

ggplot(data = dataset,aes(x = Outcome, y = Pregnancies)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(1,12,1),limits = c(0,12)) +
  ggtitle("Pregnancies boxplot") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() + 
  theme_update(plot.title = element_text(hjust = 0.5))
#Box plot shows, woman who had more pregnancies are more prone to diabetes.This may be important variable for model.

ggplot(data = dataset,aes(x = Outcome, y = Glucose)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(80,200,10),limits = c(80,200)) +
  ggtitle("Glucose") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() + 
  theme_update(plot.title = element_text(hjust = 0.5))
#Diabetics woman have high Plasma glucose concentration. 
#On average this value is 140 for diabetics woman while this is quite low for non-diabetics.

#Blood Pressure

table(dataset$BloodPressure)
#There are 35 peoples with 0 Blood pressure. It is not medically possiable. Let 0 blood pressure with the median value.
# Replace 0 blood pressure with median blood pressuure
dataset$BloodPressure <- ifelse(dataset$BloodPressure == 0, 
                                 median(dataset$BloodPressure,na.rm = TRUE),
                                 dataset$BloodPressure
)
ggplot(data = dataset,aes(x = Outcome, y = BloodPressure)) +
  geom_boxplot( aes(fill= Outcome)) +
  scale_y_continuous(breaks = seq(60,110,10),limits = c(60,110)) +
  ylab("Blood Pressure") +
  ggtitle("Blood Pressure Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
#Diastolic blood pressure for diabetic woman is higher compare to non-diabetics.

#Triceps skin fold thickness
#Triceps skin-fold thickness normal value for female 23
table(dataset$SkinThickness)
#There are 227 observation shows its value 0. Which is not medically true. Let’s replace this value with the median value.
dataset$SkinThickness <- ifelse(
  dataset$SkinThickness == 0 , 
  median(dataset$SkinThickness,na.rm = TRUE),
  dataset$SkinThickness)


ggplot(data = dataset,aes(x = Outcome, y = SkinThickness)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100)) +
  ylab("Triceps skin fold thickness") +
  ggtitle("Skin Thickness Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
#Boxplot shows that diabetics woman normally has high skin thickness. 
#Red big dots are outlier but ignoring this outlier to consider the extreme case.

#Body mass index 
table(dataset$BMI == 0)
#There are 11 observation where 0 BMI information provided. It is medically not possible.
#Let’s replace with healthy BMI at higher end to 30.


dataset$BMI <- ifelse(dataset$BMI == 0, 32, dataset$BMI)
ggplot(data = dataset,aes(x = Outcome, y = BMI)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(20,70,5),limits = c(20,70)) +
  ylab("BMI") +
  ggtitle("Body mass index Histogram") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
#BMI for diabetics’ woman is high compare to non-diabetics.
#There are few outlier, let not treat them to consider the extreme cases of BMI.

#Diabetes pedigree function
ggplot(data = dataset,aes(x = Outcome, y = DiabetesPedigreeFunction)) +
  geom_boxplot( aes(fill= Outcome),outlier.colour = "red", outlier.size = 5) +
  scale_y_continuous(breaks = seq(0,2,0.2),limits = c(0,2)) +
  ylab("Diabetes Pedigree Function") +
  ggtitle("Diabetes Pedigree Function") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show.legend = TRUE) +
  theme_gray() +
  theme_update(plot.title = element_text(hjust = 0.5))
#Check the balancing of data
table(dataset$Outcome)
prop.table(table(dataset$Outcome)) 
ggplot(dataset,aes(Outcome))+geom_bar(fill=c("red","green"))+geom_text(stat = "count",aes(label=stat(count),vjust=0.5))

#it seems to be unbalanced


# correlation matrix
library(reshape2)
cor_melt <- melt(cor(dataset[, 1:8]))
cor_melt <- cor_melt[which(cor_melt$value > 0.5 & cor_melt$value != 1), ]
cor_melt <- cor_melt[1:3, ]
cor_melt
#correlation values higher than 0.5.


#Let’s see the correlation between numerical variables. There are variables which are highly correlated.
#That’s the case of Age for example.



correlat <- cor(dataset[, setdiff(names(dataset), 'Outcome')])
corrplot(correlat)

#In this study, we used the diabetic patient health management follow-up data
#We have combined feature selection and imbalanced processing techniques.