#patikrinu ar duomenu rinkinyje nevyrauja viena is reiksmiu
table(diabetes2$Outcome)

# tikrinu ar nera praleistu reiksmiu
col1 <- mapply(anyNA, diabetes2)
print(col1)

#tikrinu ar nera isskirciu
boxplot(diabetes2)
outlier_values1 <- boxplot.stats(diabetes2$Pregnancies)$out
print(outlier_values1)
outlier_values2 <- boxplot.stats(diabetes2$Glucose)$out
print(outlier_values2)
outlier_values3 <- boxplot.stats(diabetes2$BloodPressure)$out
print(outlier_values3)
outlier_values4 <- boxplot.stats(diabetes2$SkinThickness)$out
print(outlier_values4)
outlier_values5 <- boxplot.stats(diabetes2$Insulin)$out
print(outlier_values5)
outlier_values6 <- boxplot.stats(diabetes2$BMI)$out
print(outlier_values6)
outlier_values7 <- boxplot.stats(diabetes2$DiabetesPedigreeFunction)$out
print(outlier_values7)
outlier_values8 <- boxplot.stats(diabetes2$Age)$out
print(outlier_values8)

#pasalinu išskirtis, kurios lygios 0
diabetes2 <- diabetes2[-which (diabetes2$Glucose == 0), ]
diabetes2 <- diabetes2[-which (diabetes2$BloodPressure == 0), ]
diabetes2 <- diabetes2[-which (diabetes2$BMI == 0), ]
boxplot(diabetes2)

summary(diabetes2)
sd(diabetes2$Pregnancies)
sd(diabetes2$Glucose)
sd(diabetes2$BloodPressure)
sd(diabetes2$SkinThickness)
sd(diabetes2$Insulin)
sd(diabetes2$BMI)
sd(diabetes2$DiabetesPedigreeFunction)
sd(diabetes2$Age)
sd(diabetes2$Outcome)

#tikrinama koreliacija
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(diabetes2)

#duomenu rinkiniu padalijimas
library(caTools)
set.seed(99)
split = sample.split(diabetes2$Outcome, SplitRatio = 0.80)
split
trainData = subset(diabetes2, split == TRUE)
testData = subset(diabetes2, split == FALSE)
trainData
testData

#sudaromas modelis
LogM_1 = glm(Outcome ~ Pregnancies + Age + Glucose + BloodPressure + BMI + DiabetesPedigreeFunction + SkinThickness + Insulin, data = trainData, family = binomial)
summary(LogM_1)

#sudaromas naujas modelis
LogM_2 = glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, data = trainData, family = binomial)
summary(LogM_2)

#regresoriu galimybiu santykiai ir pasikliautinieji intervalai
exp(LogM_2$coefficients)
exp(confint.default(LogM_2))

#modelio suderinamumo kriterijus
LogM.tuscias <- glm(Outcome ~ 1, data = trainData, family = binomial())
anova(LogM.tuscias, LogM_2, test = "Chisq")

#determinacijos koeficientas
rkv <- 1 - LogM_2$deviance / LogM_2$null.deviance
rkv

#prognozavimas
progTrain = predict(LogM_2, type = "response")
table(trainData$Outcome, progTrain > 0.5)

#sensitivity
114/199
#specifity
334/380
#accuracy
448/579

#modelio prognoziu tikslumas
progTest = predict(LogM_2, type = "response", newdata = testData)
table(testData$Outcome, progTest >= 0.5)

#sensitivity
21/50
#specifity
88/95
#accuracy
109/145
