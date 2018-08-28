library(tidyverse)
library(rmarkdown)
library(memisc)

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
edCHD <- base %>% group_by(education, TenYearCHD) %>%  summarise(nCHD = n()) %>% mutate(Porcentaje = nCHD*100/sum(nCHD))



> broom::glance(chisq.test(base$education, base$TenYearCHD))
# A tibble: 1 x 4
statistic     p.value parameter method                    
<dbl>       <dbl>     <int> <chr>                     
  1      32.0 0.000000519         3 Pearson's Chi-squared test
> broom::glance(chisq.test(base$education, base$TenYearCHD))$p.value


# Gráfico por nivel educacional 
ggplot(edCHD, aes(x=Nivel_educacional, y=Porcentaje, fill= FALSE)) + geom_col() + theme_classic() + theme(legend.position = "none")+ scale_x_continuous(breaks = c(1,2,3,4), labels = c("Analfabeto", "Básica", "Secundaria", "Universitaria")) + scale_y_continuous(breaks = c(seq(0, 100, by = 5)))


# Base sólo hombres
CHDhom <- base %>% group_by(TenYearCHD) %>% filter(male == 1)

#Base Sólo mujeres
CHDmuj <- base %>% group_by(TenYearCHD) %>% filter(male == 0)

#tabla CHD por sexo
sexCHD <- base %>% group_by(male, TenYearCHD) %>%  summarise(nCHD = n()) %>% mutate(Porcentaje = nCHD*100/sum(nCHD)) %>% filter(TenYearCHD == 1) %>% mutate(Sexo = if_else(male == 0, "Mujer", "Hombre"))
