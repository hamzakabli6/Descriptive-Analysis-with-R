
# loading the new questionare dataset in R 
df <- read.csv("D:/masters- digital innovation/business data analytics/final assignment (project)/Section 1 - data/Data1(1) (1).csv", header = T)
# fit the PCA model for the entire dataset
library(psych)
library(paran)
library(car)
library(reshape2)
library(ggpubr)
#omiting the missing data
df <- na.omit(df)

#removing the independent variable and keeping only likert scale items
df_lkrt <- df[6:33]
#doing KMO to check if data can support PCA
library(psych)
KMO(df_lkrt)
#looking agt the result of KMO,we can run PCA as there is enough redundancy

# fit the model and check the scree plot diagram to see how much variance is captured 
fit <- prcomp(df_lkrt)
screeplot(fit, type = "line")
# according to scree plot there are 02 components

#using factor/ parallel analysis to determine how many components 
library(paran)
paran(df_lkrt)
#according to parallel analysis 02 components are retained

#doing PCA now to see the statistic, applying the 2 components derived from parallel analysis in PCA
library(psych)
fit<- pca(df_lkrt, nfactors = 2)
fit
# Q1-12, loaded into RC2--RC2 = course satisfaction whereas, Q13-28 loaded in RC1-- inst satisfaction
#RC1 = instructor satisfaction
#RC2 = course satisfaction
#checking the component scores
RC1 <- fit$scores[,1]
RC2 <- fit$scores[,2]
#putting the components into the dataset
df <- read.csv("D:/masters- digital innovation/business data analytics/final assignment (project)/Section 1 - data/Data1(1) (1).csv", header = T)
df$RC1 <- RC1
df$RC2 <- RC2

#-------------------checking normality of data----------------------------------
# cannot perform Shapiro because the data is large
#check qualitatively the normality of data
qqnorm(df$RC1) # seems not fairly normal/ breaking parametric assumption 
qqnorm(df$RC2) # seems not fairly normal/ breaking parametric assumption
plot(density(RC1)) # seems not fairly normal/ breaking parametric assumption
plot(density(RC2)) # seems not fairly normal/ breaking parametric assumption
# converting IVs into factors
df$instr <- as.factor(df$instr)
df$class <- as.factor(df$class)
df$nb.repeat <- as.factor(df$nb.repeat)
df$difficulty <- as.factor(df$difficulty)
df$attendance <- as.factor(df$attendance)
str(df)

#see the effect/ significance at the group level
#doing levene to check the homogenity of data. if it is significant, then the parametric assumption will be violated, do kruskal, if not, then Annova
#if using annova, do tukey(post hoc test) to tell where the difference is
# if kruskal wallis, use dunn(post hoc)test

#levene test (RC1 ~instr)
leveneTest(df$RC1 ~ df$instr)
# here, the levene test is significant[F(2,5817) = 7.4841, p < 0.001] which means homogeneity of parametric assumption is violated 
#taking group mean to check the difference of mean later on
table(df$instr)
mean(df[df$instr == "1",]$RC1) #mean = -0.03
mean(df[df$instr == "2",]$RC1) #mean = 0.15
mean(df[df$instr == "3",]$RC1) #mean = -0.05
# doing the kruskal waliis test which is the omnibus test 
kw_df <- kruskal.test(df$RC1 ~ df$instr)
kw_df #the result of omnibus test are significant[chi-squared(2) = 44.44, p < 0.05, partial eta-squared = 0.0076]
44.44/5819
#there is a significant diff in the group mean
#dunn test
library(FSA)
dunnTest(df$RC1 ~ df$instr)
library(sjPlot)
fitLm <- lm(RC1 ~ instr, data = df)
plot_model(fitLm, type = "pred")

#levene test (RC1 ~ class)
leveneTest(df$RC1 ~ df$class)
# here, the levene test is significant[F(12,5807) = 4.1216, p < 0.001] which means homogeneity of parametric assumption is violated 
#taking group mean 
table(df$class)
mean(df[df$class == "1",]$RC1) #mean = 0.115
mean(df[df$class == "2",]$RC1) #mean = 0.134
mean(df[df$class == "3",]$RC1) #mean = -0.187
mean(df[df$class == "4",]$RC1) #mean = 0.071
mean(df[df$class == "5",]$RC1) #mean = -0.016
mean(df[df$class == "6",]$RC1) #mean = 0.119
mean(df[df$class == "7",]$RC1) #mean = -0.072
mean(df[df$class == "8",]$RC1) #mean = 0.369
mean(df[df$class == "9",]$RC1) #mean = -0.005
mean(df[df$class == "10",]$RC1) #mean = -0.069
mean(df[df$class == "11",]$RC1) #mean = 0.165
mean(df[df$class == "12",]$RC1) #mean = -0.132
mean(df[df$class == "13",]$RC1) #mean = -0.197
# doing the kruskal waliis test which is the omnibus test 
kw_df <- kruskal.test(df$RC1 ~ df$class)
kw_df #the result of omnibus test are significant [chi-squared = 180.24, p < 0.05, eta-squared = 0.031]
180.24/5819
dunnTest(df$RC1 ~ df$class)
#confidence interval plot
fitLm <- lm(RC1 ~ class, data = df)
plot_model(fitLm, type = "pred")
#levene test (RC1 ~nb.repeat)
leveneTest(df$RC1 ~ df$nb.repeat)
#levene test is not significant [F(2,5817) = 0.0914, p > 0.05]
#taking group mean
table(df$nb.repeat)
mean(df[df$nb.repeat == "1",]$RC1)
#mean = 0.021
mean(df[df$nb.repeat == "2",]$RC1)
#mean = -0.098
mean(df[df$nb.repeat == "3",]$RC1)
#mean = -0.141
#annova
aov_nb1 <- aov(RC1 ~ nb.repeat, data = df)
summary(aov_nb1)
summary.lm(aov_nb1)#ANOVA is significant (F[2,5817] = 7.24, p < 0.001, eta-squared = 0.002483)
#significant difference in group mean
TukeyHSD(aov_nb1)
#2-1 (Mdiff = -0.119, 95% CI [-0.223,-0.016], p < 0.05)
#3-1 (Mdiff = -0.162, 95% CI [-0.295,-0.030], p < 0.05)
#there is no significant difference between 3-2(Mdiff = -0.043, 95% CI [-0.204,0.118], p > 0.05)
#CI plot 
fitLm <- lm(RC1 ~ nb.repeat, data = df)
plot_model(fitLm, type = "pred")


#levene test(RC1~attendance)
leveneTest(df$RC1 ~ df$attendance)
#Levene's test is significant [F(4,5815) = 8.3093, p < 0.05],parametric assumption violated
#taking group mean 
table(df$attendance)
mean(df[df$attendance == "0",]$RC1)
#mean = -0.2607238
mean(df[df$attendance == "1",]$RC1)
#mean = -0.08861972
mean(df[df$attendance == "2",]$RC1)
#mean = 0.09380385
mean(df[df$attendance == "3",]$RC1)
#mean = 0.2464323
mean(df[df$attendance == "4",]$RC1)
#mean = 0.2413378
# doing the kruskal waliis test which is the omnibus test 
kw_df <- kruskal.test(df$RC1 ~ df$attendance)
kw_df #the result of omnibus test are significant[chi-squared = 279.83, p < 0.05, eta-squared = 0.048]
279.83/5819
#dunn test
library(FSA)
dunnTest(df$RC1 ~ df$attendance)
  #there is a significant difference between all except 3-4. attendance 3 (Mean = 0.246) and attendance 4 (Mean = 0.241) (Z = -0.38, p > 0.05) has no significant difference
library(sjPlot)
fitLm <- lm(RC1 ~ attendance, data = df)
plot_model(fitLm, type = "pred")
##levene test(RC1~difficulity)
leveneTest(df$RC1 ~ df$difficulty)
#Levene's test is significant [F(4,5815) = 13.915, p < 0.05],parametric assumption violated
#taking group mean
table(df$difficulty)
mean(df[df$difficulty == "1",]$RC1)
#-0.295
mean(df[df$difficulty == "2",]$RC1)
#0.100
mean(df[df$difficulty == "3",]$RC1)
#0.207
mean(df[df$difficulty == "4",]$RC1)
#0.147
mean(df[df$difficulty == "5",]$RC1)
#-0.190
kw_df <- kruskal.test(df$RC1 ~ df$difficulty)
kw_df #the result of omnibus test is significant[chi-squared = 277.09, p < 0.05, eta-squared = 0.048]
277.09/5819
#dunntest
dunnTest(df$RC1 ~ df$difficulty)
#there is a significant difference between all other except the following
#2-3 [2 (Mean = 0.1) and 3 (Mean = 0.207) (Z = -2.297, p > 0.05)]
#2 (Mean = 0.1) and 4 (Mean = 0.147) (Z = -0.864, p > 0.05),
#3 (Mean = 0.207) and 4 (Mean = 0.147) (Z = 1.825, p > 0.05),
#1 (Mean = -0.295) and 5 (Mean = -0.19) (Z = -1.804, p > 0.05)
fitLm <- lm(RC1 ~ difficulty, data = df)
plot_model(fitLm, type = "pred")

#--------taking RC2 now -----------
#levene test (RC2~instr)
leveneTest(df$RC2 ~ df$instr)
# here, the levene test is significant[F(2,5817) = 4.571, p < 0.05] which means homogeneity of parametric assumption is violated 
#taking group mean 
mean(df[df$instr == "1",]$RC2) #mean = 0.275
mean(df[df$instr == "2",]$RC2) #mean = 0.104
mean(df[df$instr == "3",]$RC2) #mean = -0.101
# doing the kruskal waliis test which is the omnibus test 
kw_df <- kruskal.test(df$RC2 ~ df$instr)
kw_df #the result of omnibus test are significant[chi-squared = 122.35, p < 0.05, eta-squared = 0.021]
122.35/5819
#dunn test
dunnTest(df$RC2 ~ df$instr)
#there is a significant difference among 3
#1 (Mean = 0.275) and 2 (Mean = 0.104) (Z = 3.82, p < 0.05),
#1 (Mean = 0.275) and 3 (Mean = -0.1) (Z = 9.87, p < 0.05),
#2 (Mean = 0.104) and 3 (Mean = -0.1) (Z = 7.09, p < 0.05).

fitLm <- lm(RC2 ~ instr, data = df)
plot_model(fitLm, type = "pred")
#levene test (RC2 ~ class)
leveneTest(df$RC2 ~ df$class)
# here, the levene test is significant[F(12,5807) = 4.3021, p < 0.05] which means homogeneity of parametric assumption is violated 
#taking group mean 
table(df$class)
mean(df[df$class == "1",]$RC2) #mean = 0.195
mean(df[df$class == "2",]$RC2) #mean = 0.370
mean(df[df$class == "3",]$RC2) #mean = -0.046
mean(df[df$class == "4",]$RC2) #mean = -0.384
mean(df[df$class == "5",]$RC2) #mean = 0.035
mean(df[df$class == "6",]$RC2) #mean = 0.142
mean(df[df$class == "7",]$RC2) #mean = -0.06
mean(df[df$class == "8",]$RC2) #mean = -0.27
mean(df[df$class == "9",]$RC2) #mean = -0.015
mean(df[df$class == "10",]$RC2) #mean = 0.386
mean(df[df$class == "11",]$RC2) #mean = 0.05
mean(df[df$class == "12",]$RC2) #mean = -0.167
mean(df[df$class == "13",]$RC2) #mean = -0.162
# doing the kruskal waliis test which is the omnibus test 
kw_df <- kruskal.test(df$RC2 ~ df$class)
kw_df #the result of omnibus test are significant [chi-squared = 206.87, p < 0.05, eta-squared = 0.036]
206.87/5819
dunnTest(df$RC2 ~ df$class)
#there ar many significant difference between groups
#confidence interval plot
fitLm <- lm(RC2 ~ class, data = df)
plot_model(fitLm, type = "pred")

#levene test (RC2 ~nb.repeat)
leveneTest(df$RC2 ~ df$nb.repeat)
#levene test is not significant [F(2,5817) = 0.3571 , p > 0.05, which means homogenity is not violated
#taking group mean
table(df$nb.repeat)
mean(df[df$nb.repeat == "1",]$RC2)#mean = 0.00495
mean(df[df$nb.repeat == "2",]$RC2)#mean = -0.044
mean(df[df$nb.repeat == "3",]$RC2)#mean = 0.003
#annova
aov_nb1 <- aov(RC2 ~ nb.repeat, data = df)
summary(aov_nb1)
summary.lm(aov_nb1)#ANOVA is not significant (F[2,5817] = 0.62, p > 0.05, eta -squared = 0.000213)
#no significant difference in group mean
#CI plot 
fitLm <- lm(RC2 ~ nb.repeat, data = df)
plot_model(fitLm, type = "pred")
#nb.repeat has no significant effect on course satisfaction

#levene test(RC2~attendance)
leveneTest(df$RC2 ~ df$attendance)
#Levene's test is significant [F(4,5815) = 8.1744, p < 0.05],parametric assumption violated
#taking group mean 
table(df$attendance)
mean(df[df$attendance == "0",]$RC2)
#mean = -0.047
mean(df[df$attendance == "1",]$RC2)
#mean = -0.050
mean(df[df$attendance == "2",]$RC2)
#mean = 0.00763
mean(df[df$attendance == "3",]$RC2)
#mean = 0.04224
mean(df[df$attendance == "4",]$RC2)
#mean = 0.0985
# doing the kruskal waliis test which is the omnibus test 
kw_df <- kruskal.test(df$RC2 ~ df$attendance)
kw_df #the result of omnibus test are significant[chi-squared = 24.268, p < 0.05, eta-squared = 0.004]

#dunn test
dunnTest(df$RC2 ~ df$attendance)
#there is significant difference between
# 0 (Mean = -0.0475) and 3 (Mean = 0.042) (Z = -3.15, p < 0.05)
# 0 (Mean = -0.0475) and 4 (Mean = 0.099) (Z = -4.19, p < 0.05)
# 1 (Mean = -0.051) and 4 (Mean = 0.099) (Z = -3.52, p < 0.05)
#;there are no significant differences between the other groups
fitLm <- lm(RC2 ~ attendance, data = df)
plot_model(fitLm, type = "pred")
##levene test(RC2~difficulity)
leveneTest(df$RC2 ~ df$difficulty)
#Levene's test is significant [F(4,5815) = 11.244, p < 0.05],parametric assumption violated
#taking group mean
table(df$difficulty)
mean(df[df$difficulty == "1",]$RC2)
#-0.069
mean(df[df$difficulty == "2",]$RC2)
#-0.02
mean(df[df$difficulty == "3",]$RC2)
#0.12
mean(df[df$difficulty == "4",]$RC2)
#0.03
mean(df[df$difficulty == "5",]$RC2)
#-0.196
kw_df <- kruskal.test(df$RC2 ~ df$difficulty)
kw_df #the result of omnibus test is significant[chi-squared = 64.579, p < 0.05, eta-squared = 0.011]

#dunntest
dunnTest(df$RC2 ~ df$difficulty)
#there is a significant difference between many groups
fitLm <- lm(RC2 ~ difficulty, data = df)
plot_model(fitLm, type = "pred") 

#------------MANOVA-------------------------------
#doing MANOVA to cross check the significant difference between variables,as it is also very robust 
fit <- manova(cbind(RC1, RC2) ~ instr + class + nb.repeat + difficulty + attendance, data = df)
#overall model
summary(fit)
#the overall MANOVA model showed a significant effect 
#instr [F(2,5796) = 40.908, p < 0.001, V =  0.03] 
#class [F(11,5796) = 11.973, p < 0.001, V =  0.04]
#nb.repeat [F(2,5796) = 4.070, p < 0.005, V =  0.003]
#difficulty [F(4,5796) = 37.239, p < 0.001, V =  0.05]
#attendance [F(4,5796) = 13.442, p < 0.001, V =  0.02] 
#show in detail, the DVs
summary.aov(fit) # this shows that all other variable have significant interaction except RC2 on nb.repeat- no significant interaction, ANNOVA applied above also showed the same so its verified

#---------------------two way annova----------------------------
#------------------------      --------------------   ------------------
#doing 2 way annova to see the interactions 
fit <- aov(RC1 ~ instr*class, data = df)
summary(fit)
ggline(df, x = "class", y = "RC1", color = "instr", add = c("mean_ci"))
fit <- aov(RC1 ~ instr*nb.repeat, data = df)
summary(fit) 
#RC1 ~ instr*nb.repeat- no significant interaction
fit <- aov(RC1 ~ instr*attendance, data = df)
summary(fit) 
#RC1 ~ instr*attendance- p> 0.05 no significant interaction
fit <- aov(RC1 ~ instr*difficulty, data = df)
summary(fit)
#RC1 ~ instr*difficulty- p>0.05 no significant interaction
fit <- aov(RC1 ~ class*nb.repeat, data = df)
summary(fit)
#RC1 ~ class*nb.repeat- p<0.05 significant interaction
# as the value come out significant so plotting the value 

ggline(df, x = "nb.repeat", y = "RC1", color = "class", add = c("mean_ci"))
fit <- aov(RC1 ~ class*attendance, data = df)
summary(fit) 
#class*attendance- p<0.05 significant interaction
#plotting the interaction
ggline(df, x = "attendance", y = "RC1", color = "class", add = c("mean_ci"))
fit <- aov(RC1 ~ class*difficulty, data = df)
summary(fit)
#RC1 ~ class*difficulty-p<0.05 significant interaction
ggline(df, x = "difficulty", y = "RC1", color = "class", add = c("mean_ci"))

fit <- aov(RC1 ~ nb.repeat*attendance, data = df)
summary(fit)
#RC1 ~ nb.repeat*attendance-p<0.05 significant interaction
#plotting the interaction
ggline(df, x = "attendance", y = "RC1", color = "nb.repeat", add = c("mean_ci"))
fit <- aov(RC1 ~ nb.repeat*difficulty, data = df)
summary(fit)
#RC1 ~ nb.repeat*difficulty- p<0.05 significant interaction
#plotting the interaction
ggline(df, x = "difficulty", y = "RC1", color = "nb.repeat", add = c("mean_ci"))

fit <- aov(RC1 ~ attendance*difficulty, data = df)
summary(fit)
# RC1 ~ attendance*difficulty- p<0.05 significant interaction
#plotting the interaction
ggline(df, x = "difficulty", y = "RC1", color = "attendance", add = c("mean_ci"))

fit <- aov(RC2 ~ instr*nb.repeat, data = df)
summary(fit) 
#RC2 ~ instr*nb.repeat- p> 0.05 no significant interaction
fit <- aov(RC2 ~ instr*class, data = df)
summary(fit)
ggline(df, x = "class", y = "RC2", color = "instr", add = c("mean_ci"))
fit <- aov(RC2 ~ instr*attendance, data = df)
summary(fit) 
#RC2 ~ instr*attendance - p<0.05 , significant interaction
#plotting the interaction
ggline(df, x = "attendance", y = "RC2", color = "instr", add = c("mean_ci"))

fit <- aov(RC2 ~ instr*difficulty, data = df)
summary(fit) 
#RC2 ~ instr*difficulty- p<0.05 , significant interaction
#plotting the interaction
ggline(df, x = "difficulty", y = "RC2", color = "instr", add = c("mean_ci"))

fit <- aov(RC2 ~ class*nb.repeat, data = df)
summary(fit)
#RC2 ~ class*nb.repeat - p>0.05, no significant interaction

fit <- aov(RC2 ~ class*attendance, data = df)
summary(fit) 
#RC2 ~ class*attendance - p<0.05 , significant interaction
ggline(df, x = "attendance", y = "RC2", color = "class", add = c("mean_ci"))

fit <- aov(RC2 ~ class*difficulty, data = df)
summary(fit)
#RC2 ~ class*difficulty - p<0.05 , significant interaction
#plotting the interaction
ggline(df, x = "difficulty", y = "RC2", color = "class", add = c("mean_ci"))

fit <- aov(RC2 ~ nb.repeat*difficulty, data = df)
summary(fit)
#RC2 ~ nb.repeat*difficulty - p<0.05 , significant interaction
ggline(df, x = "difficulty", y = "RC2", color = "nb.repeat", add = c("mean_ci"))

fit <- aov(RC2 ~ nb.repeat*attendance, data = df)
summary(fit)
#nb.repeat*attendance- p>0.05, no significant interaction
fit <- aov(RC2 ~ attendance*difficulty, data = df)
summary(fit) 
#RC2 ~ attendance*difficulty - p>0.05, no significant interaction


write.csv(df, file = "descriptive.csv")
descriptive_csv <- read.csv(file = "descriptive.csv")
View(descriptive_csv)

