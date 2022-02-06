library(haven)
library(dplyr)
library(lavaan)
library("tibble")
library(tidySEM)
library(ggplot2)
library('lattice')
library("hrbrthemes")
library("semPlot")


#IMPORTING DATASET
base_final <- read_dta("C:/Cursos_PUCP/2021-1/Tesis 2/Base de datos/BASE_FINAL/Bases de Datos/base_final_recode.dta")


base_acotada <- base_final %>%
        select(P50AR, P50BR, P50CR, P50DR, P50ER, P50FR,
               P57R, P58R, P59R, P56R,
               P64AR, P64BR, P64CR, P64DR,
               P63A, P63B, P63C, P63F, P60A, P60B,
               P72A, P72B, P72C, P72D, P69,
               tel_cel, P44DR, P44ER, P44FR, P26, P78, SEXO, AMBITOR, EDAD, educ, P1A, P1D, seguro, P46R,
               P80E, P80G, P80L, P80H, P80D, AMBITO, lima_callao,int_urb,int_rural,SUBMUESTRA, dormirR, ocioR, P99 )

base_acotada <- as.data.frame(base_acotada)


#Deleting NA
base_acotada <-base_acotada[complete.cases(base_acotada), ] 
nrow(base_acotada) #2798


#****************************
#MODEL SPECIFICATIONS
#****************************

model <- '

##modelo de medida  (measurement equations)
        Sociabilidad =~ P50AR + P50BR + P50CR + P50DR + P50ER + P50FR                                          
        Agencia  =~  P57R + P58R + P59R + P56R  
          #P56R es la unica que está en la escala del 1-4
        Ciudadania =~ P64AR + P64BR + P64CR + P64DR                                                           
        Seguridad =~ P63A + P63B + P63C + P63F + P60A + P60B 
        Salud =~ P72A + P72B + P72C + P72D + P69 

##"causal" relationships                                                                                 
        Sociabilidad ~  tel_cel + P44DR + P44ER + P44FR + P78 + SEXO + AMBITOR + EDAD + educ 
        Agencia  ~   tel_cel + P44DR + P44ER + P44FR + P26 + P78 + SEXO + AMBITOR + EDAD + educ 
        Ciudadania ~  P78 + SEXO + AMBITOR + EDAD + educ 
        Seguridad ~ P26 + P78 + SEXO + AMBITOR + EDAD + educ 
        Salud ~  seguro + P46R + P78 + SEXO + AMBITOR + EDAD + educ + P1A + P1D 
'

#*********************
#ESTIMATING OUR MODEL
#*********************

fit <- sem(model, data=base_acotada , ordered=TRUE)


summary(fit, fit.measures = TRUE, standardized = TRUE,rsquare=FALSE)


#**************************
#ESTIMATING CAPABILITIES
#**************************

capability_score <- lavPredict(fit)

Sociabilidad <- capability_score[,1]
Agencia <- capability_score[,2] 
Ciudadania <- capability_score[,3] 
Seguridad <- capability_score[,4]
Salud <- capability_score[,5]


#****************************
#Capability standardization
#****************************


#Creamos dataframe con los valores sin normalizar
norm_capability2 <- data.frame(Sociabilidad, Agencia, Ciudadania, Seguridad, Salud )


#Normalizamos las capacidades (entre dimensión) 

for(i in 1:5){
        
        norm_capability2[,i] <-( capability_score[,i] - min(norm_capability2) ) / ( max(norm_capability2) - min(norm_capability2) )
        
}

colnames(norm_capability2) <- c("sociabilidad2", "agencia2", "ciudadania2", "seguridad2", "salud2") 


#A LA BASE_ACOTADA LE PEGAMOS LAS CAPACIDADES NORMALIZADAS
df <- cbind(base_acotada, norm_capability, norm_capability2)

setwd("C:/Cursos_PUCP/2021-1/Tesis 2/Base de datos/BASE_FINAL/Bases de Datos")

write.csv(df,'base_capability_score.csv',row.names = FALSE)


#********************
# Saving results
#*******************

coef <- parameterEstimates(fit, se = TRUE, pvalue = TRUE,  ci = FALSE)
coef <- coef[,c(1:5,7)]

scoef <- standardizedSolution(fit, se = TRUE, pvalue = TRUE, ci = FALSE)
scoef <- scoef[,c(4,5,7)]
colnames(scoef) <- c("est.std", "se.est", "pvalue.est")

model_coef <- cbind(coef, scoef)

write.csv(model_coef,'coeficientes_model.csv',row.names = FALSE)



#**************************
#Model robustness
#**************************

#Consideraramos como no seguro al SIS. Es decir, no es considerado al momento de crear la nueva variable seguro_2


base_final2 <- mutate(base_final, seguro_2 =
                           case_when( P68A==0 | P68B==0 | P68C==0 | P68D==0 | P68F==0 | P68G==0 | P68H==0 ~ 1, #sí tiene seguro
                                      P68A== 1 & P68B== 1 & P68C== 1 & P68D== 1 & P68F== 1 & P68G== 1 & P68H== 1 ~ 0  #no tiene seguro
                           ))

table(base_final2$seguro_2) #hay menos personas que tienen seguro ahora


model3 <- '

##modelo de medida  (measurement equations)
        Sociabilidad =~ P50AR + P50BR + P50CR + P50DR + P50ER + P50FR                                          
        Agencia  =~  P57R + P58R + P59R + P56R  
          #P56R es la unica que está en la escala del 1-4
        Ciudadania =~ P64AR + P64BR + P64CR + P64DR                                                           
        Seguridad =~ P63A + P63B + P63C + P63F + P60A + P60B 
        Salud =~ P72A + P72B + P72C + P72D + P69 

##"causal" relationships                                                                                  
        Agencia  ~   tel_cel + P44DR + P44ER + P44FR + P26 + P78 + SEXO + AMBITOR + EDAD + educ 
        Ciudadania ~  P78 + SEXO + AMBITOR + EDAD + educ 
        Seguridad ~ P26 + P78 + SEXO + AMBITOR + EDAD + educ 
        Salud ~  seguro_2 + P46R + P78 + SEXO + AMBITOR + EDAD + educ + P1A + P1D 
'

fit <- sem(model3, data=base_final2 , ordered=TRUE)

summary(fit, fit.measures = TRUE, standardized = TRUE,rsquare=FALSE)





