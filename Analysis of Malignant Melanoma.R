#Summary Statistics in R

# set working directory
setwd("C:/Users/lucks/Downloads")

# read in the data
install.packages(readr)
library(readr)

Dataset <- read_csv("melanoma-2.csv")
summary(Dataset)

# nominal / categorical & ordinal measurement scale variables
install.packages("dplyr")

library(dplyr)
library(tidyverse)
library(ggpubr)
theme_set(theme_pubr())

Dataset %>%
  count(sex, status, ulcer)

library(dplyr)


Dataset$sex <- factor(Dataset$sex,
                      levels = c(1,0),
                      labels = c("Male", "Female"))
     
Dataset$ulcer <- factor(Dataset$ulcer,
                        levels = c(1,0),
                        labels = c("Present", "Absent"))

Dataset$status <- factor(Dataset$status,
                         levels = c(1,2,3),
                         labels = c("Died", "Alive", "Death from other causes"))
View(Dataset)
summary(Dataset)

sapply (Dataset[c (2,5,6,7)], mean)
sapply (Dataset[c (2,5,6,7)], median)


mode(Dataset$sex)
install.packages("psych")
library(psych)
describe(Dataset)

par(mfrow=c(1,2))


hist(Dataset$time,
     main="Survival period",
     xlab="Time",
     ylab="Frequency")
hist(Dataset$age,
     main="Age at the time of Operation",
     xlab="Age",
     ylab="Frequency")
hist(Dataset$year,
     main="Year of Operation",
     xlab="Year",
     ylab="Frequency")
hist(Dataset$thickness,
     main="Thickness of the Tumour",
     xlab="Size",
     ylab="Frequency")



par(mfrow=c(1,2))
boxplot(Dataset$age ~ Dataset$sex,
        xlab="Sex",
        ylab="Age")
boxplot(Dataset$time ~ Dataset$sex,
        xlab="Sex",
        ylab="Time")
boxplot(Dataset$thickness ~ Dataset$status,
        xlab="Status",
        ylab="Thickness")
boxplot(Dataset$age ~ Dataset$status,
        xlab="Status",
        ylab="Age")
boxplot(Dataset$time ~ Dataset$ulcer,
        xlab="Status",
        ylab="Time")
boxplot(Dataset$year ~ Dataset$ulcer,
        xlab="Status",
        ylab="Year")


#end
library(ggplot2)
View(Dataset)
par(mfrow=c(1,2))
Melanoma_lmmodel1 = lm(formula = Dataset$thickness ~ Dataset$time)
summary(Melanoma_lmmodel1)
ggplot(Dataset, aes(thickness, time)) +
  geom_point() +
  stat_smooth(method = lm)



View(Dataset)
Melanoma_lmModel2 = lm(formula = Dataset$age ~ Dataset$time)
summary(Melanoma_lmModel2)
ggplot(Dataset, aes(age, time)) +
  geom_point() +
  stat_smooth(method = lm)


View(Dataset)
Melanoma_lmModel3 = lm(formula = Dataset$age ~ Dataset$thickness)
summary(Melanoma_lmModel3)
ggplot(Dataset, aes(age, thickness)) +
  geom_point() +
  stat_smooth(method = lm)

par(mfrow=c(1,2))
attach(Dataset)
x<-thickness
y<-time
cor(thickness,time, method="pearson")
plot(thickness, time, xlab = "Thickness(mm)", ylab = "Survival Time", main = "Survival Time vs Thickness of the Tumour")


attach(Dataset)
x<-age
y<-time
cor(age, time, method="pearson")
plot(x, y, xlab = "Age", ylab = "Survival Time", main = "Age of the patient vs Thickness of the Tumour")


attach(Dataset)
x<-age
y<-thickness
cor(age, thickness, method="pearson")
plot(x, y, xlab = "Age", ylab = "Thickness(mm)", main = "Age of the patient vs Thickness of the Tumour")


#Task (v)

par(mfrow=c(1,2))
attach(Dataset)
library(ggplot2)

par(mfrow=c(1,2))
qplot(x = sex, y = time,
      geom = "boxplot", data = Dataset,
      xlab = "Sex",
      ylab = "Survival Time (Days)",
      fill = I("pink"))

Dataset %>%
  group_by(sex) %>%
  summarize(num.obs = n(),
            mean_time = round(mean(time), 0),
            sd_time = round(sd(time), 0),
            se_time = round(sd(time) / sqrt(num.obs), 0))

time_t_test <- t.test(time ~ sex, data = Dataset)
time_t_test

#end  

library(ggplot2)
qplot(x = sex, y = thickness,
      geom = "boxplot", data = Dataset,
      xlab = "Sex",
      ylab = "Thickness of the Tumour (mm)",
      fill = I("pink"))


Dataset %>%
  group_by(sex) %>%
  summarize(num.obs = n(),
            mean_time = round(mean(thickness), 0),
            sd_thickness = round(sd(thickness), 0),
            se_thickness = round(sd(thickness) / sqrt(num.obs), 0))

thickness_t_test <- t.test(thickness ~ sex, data = Dataset)
thickness_t_test

#end  

library(ggplot2)
qplot(x = sex, y = age,
      geom = "boxplot", data = Dataset,
      xlab = "Sex",
      ylab = "Age",
      fill = I("pink"))


Dataset %>%
  group_by(sex) %>%
  summarize(num.obs = n(),
            mean_time = round(mean(age), 0),
            sd_age = round(sd(age), 0),
            se_age = round(sd(age) / sqrt(num.obs), 0))

age_t_test <- t.test(age ~ sex, data = Dataset)
age_t_test


#end

p_time <- ggplot(data = Dataset, aes(sample = time))
p_time + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)


#end 
p_thickness <- ggplot(data = Dataset, aes(sample = thickness))
p_thickness + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)

#end
p_age <- ggplot(data = Dataset, aes(sample = age))
p_age + stat_qq() + stat_qq_line() + facet_grid(. ~ sex)

attach(Dataset)
x<-age
y<-ulcer
cor(age, ulcer, method="pearson")
plot(x, y, xlab = "Age", ylab = "Thickness(mm)", main = "Age of the patient vs Thickness of the Tumour")

attach(Dataset)
plot()
