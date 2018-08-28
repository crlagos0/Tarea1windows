library(tidyverse)
library(rmarkdown)

#Bases separadas por sexo
hom <- base %>% filter(male == 1) %>% select(age, currentSmoker, prevalentStroke, TenYearCHD, cigsPerDay, BPMeds, education, diabetes, totChol, sysBP, diaBP, BMI, BMIcat, heartRate, glucose, PAM)
muj <- base %>% filter(male == 0) %>% select(age, currentSmoker, prevalentStroke, TenYearCHD, cigsPerDay, BPMeds, education, diabetes, totChol, sysBP, diaBP, BMI, BMIcat, heartRate, glucose, PAM)

#Base pacientes hipertensos
hipertensos <- base %>% filter(prevalentHyp == 1) %>% select(BPMeds, TenYearCHD, PAM, sysBP, diaBP)

#Chi cuadrado nivel educacional vs riesgo CHD
Xtested <- chisq.test(base$education, base$TenYearCHD)

#Tabla porcentajes CHD por nivel educacional
edCHD <- base %>% filter(!is.na(education)) %>%  group_by(education, TenYearCHD) %>% select(TenYearCHD) %>% summarise(PropEd = n()) %>% mutate(PorcEd = 100*PropEd/sum(PropEd))

#Chi cuadrado sexo vs riesgo CHD
Xsex <- chisq.test(base$male, base$TenYearCHD)

#Tabla porcentajes CHD por sexo
sexCHD <- base %>% filter(!is.na(male)) %>% group_by(male, TenYearCHD) %>% summarise(nCHD = n()) %>% mutate(PorcentCHD = 100*nCHD/sum(nCHD))

#Chi cuadrado tabaquismo vs CHD risk
Xsmoke <- chisq.test(base$currentSmoker, base$TenYearCHD)

##Porcentaje de individuos sexo##
porhom <- percent(base$male == 1, ci=TRUE, ci.level = .95)
pormuj <- percent(base$male == 0, ci=TRUE, ci.level = .95)

##Gráfico porcentaje con IAM segun sexo
ggplot(sexCHD, aes(x = Sexo, y = PorcentCHD)) + geom_col(aes(fill = TenYearCHD)) + theme_classic()


##Tabla por nivel educacional
NivEdCHD <- NivEdCHD %>% mutate(Porcentaje = Total*100/sum(Total))

##Gráfico por nivel educacional
ggplot(NivEdCHD, aes(x=education, y=Porcentaje)) + geom_col(aes(fill = TenYearCHD)) + theme_classic()

##Tabla nivel educacional 
base %>% group_by(education, TenYearCHD) %>%  summarise(nCHD = n()) %>% mutate(Porcentaje = nCHD*100/sum(nCHD))