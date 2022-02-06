
setwd("C:/Cursos_PUCP/2021-1/Tesis 2/Base de datos/BASE_FINAL/Bases de Datos")

df<- read.csv("base_capability_score.csv")
names(df)


#videos:
#https://www.youtube.com/watch?v=9STZ7MxkNVg&list=PLqzoL9-eJTNDp_bWyWBdw2ioA43B3dBrl&index=2
#https://www.youtube.com/watch?v=Zet-qmEEfCU


# let's bootstrap...
set.seed(112358)   # for reproducibility
n <- nrow(df)  # the number of observations to sample
n
B <- 10000  # the number of bootstrap samples

variables<-data.frame(df$sociabilidad2, df$agencia2, df$ciudadania2, df$seguridad2, df$salud2)

# now, get those bootstrap samples 
for (i in 1:5){
BootstrapSamples <- matrix( sample(variables[,i], size= n*B, replace=TRUE), 
                            nrow=n, ncol=B)
assign(paste("capacidad", i, sep = ""), BootstrapSamples)
}

#capacidad1: sociabilidad/ capacidad2:agencia/ capacidad3: ciudadania/ capacidad4: seguridad/ capacidad5: salud

# let's take a moment to discuss what that code is doing...
dim(capacidad4)

######################################################
# SOCIABILIDAD
######################################################

# lets calculate the absolute diff in means
test.stat1 <- abs(mean(df$sociabilidad2[df$SEXO==1]) - mean(df$sociabilidad2[df$SEXO==0]))  #diff in means
test.stat2 <- abs(mean(df$sociabilidad2[df$AMBITOR==1]) - mean(df$sociabilidad2[df$AMBITOR==0]))  #diff in means

# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)  #PARA SEXO
Boot.test.stat2 <- rep(0,B)  #PARA ÁMBITO


for (i in 1:B){
        # calculate the boot-test-stat1 and save it (SEXO)
        Boot.test.stat1[i] <- abs( mean(capacidad1[,i][df$SEXO==1]) - 
                                           mean(capacidad1[,i][df$SEXO==0]) )
        
        # calculate the boot-test-stat2 and save it (ÁMBITO)
        Boot.test.stat2[i] <- abs( mean(capacidad1[,i][df$AMBITOR==1]) - 
                                           mean(capacidad1[,i][df$AMBITOR==0]) )
}

#H0: las medias son iguales para ambos grupos
#p-value menor a 0.05 rechaza la H0 al 5% de significancia

# let's calculate the p-value for test statistic 1 and 2 (abs diff in means)
#sum(Boot.test.stat1 >= test.stat1)/B
mean( Boot.test.stat1 >= test.stat1) #SEXO  

#sum(Boot.test.stat2 >= test.stat2)/B
mean( Boot.test.stat2 >= test.stat2) #AMBITO


######################################################
# AGENCIA
######################################################

# lets calculate the absolute diff in means
test.stat1 <- abs(mean(df$agencia2[df$SEXO==1]) - mean(df$agencia2[df$SEXO==0]))  #diff in means
test.stat2 <- abs(mean(df$agencia2[df$AMBITOR==1]) - mean(df$agencia2[df$AMBITOR==0]))  #diff in means

# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)  #PARA SEXO
Boot.test.stat2 <- rep(0,B)  #PARA ÁMBITO


for (i in 1:B){
        # calculate the boot-test-stat1 and save it (SEXO)
        Boot.test.stat1[i] <- abs( mean(capacidad2[,i][df$SEXO==1]) - 
                                           mean(capacidad2[,i][df$SEXO==0]) )
        
        # calculate the boot-test-stat2 and save it (ÁMBITO)
        Boot.test.stat2[i] <- abs( mean(capacidad2[,i][df$AMBITOR==1]) - 
                                           mean(capacidad2[,i][df$AMBITOR==0]) )
}

#H0: las medias son iguales para ambos grupos
#p-value menor a 0.05 rechaza la H0 al 5% de significancia

# let's calculate the p-value for test statistic 1 and 2 (abs diff in means)
#sum(Boot.test.stat1 >= test.stat1)/B
mean( Boot.test.stat1 >= test.stat1) #SEXO  

#sum(Boot.test.stat2 >= test.stat2)/B
mean( Boot.test.stat2 >= test.stat2) #AMBITO


######################################################
# CIUDADANIA
######################################################

# lets calculate the absolute diff in means
test.stat1 <- abs(mean(df$ciudadania2[df$SEXO==1]) - mean(df$ciudadania2[df$SEXO==0]))  #diff in means
test.stat2 <- abs(mean(df$ciudadania2[df$AMBITOR==1]) - mean(df$ciudadania2[df$AMBITOR==0]))  #diff in means

# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)  #PARA SEXO
Boot.test.stat2 <- rep(0,B)  #PARA ÁMBITO


for (i in 1:B){
        # calculate the boot-test-stat1 and save it (SEXO)
        Boot.test.stat1[i] <- abs( mean(capacidad3[,i][df$SEXO==1]) - 
                                           mean(capacidad3[,i][df$SEXO==0]) )
        
        # calculate the boot-test-stat2 and save it (ÁMBITO)
        Boot.test.stat2[i] <- abs( mean(capacidad3[,i][df$AMBITOR==1]) - 
                                           mean(capacidad3[,i][df$AMBITOR==0]) )
}

#H0: las medias son iguales para ambos grupos
#p-value menor a 0.05 rechaza la H0 al 5% de significancia

# let's calculate the p-value for test statistic 1 and 2 (abs diff in means)
#sum(Boot.test.stat1 >= test.stat1)/B
mean( Boot.test.stat1 >= test.stat1) #SEXO  

#sum(Boot.test.stat2 >= test.stat2)/B
mean( Boot.test.stat2 >= test.stat2) #AMBITO


######################################################
# SEGURIDAD
######################################################

# lets calculate the absolute diff in means
test.stat1 <- abs(mean(df$seguridad2[df$SEXO==1]) - mean(df$seguridad2[df$SEXO==0]))  #diff in means
test.stat2 <- abs(mean(df$seguridad2[df$AMBITOR==1]) - mean(df$seguridad2[df$AMBITOR==0]))  #diff in means

# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)  #PARA SEXO
Boot.test.stat2 <- rep(0,B)  #PARA ÁMBITO


for (i in 1:B){
        # calculate the boot-test-stat1 and save it (SEXO)
        Boot.test.stat1[i] <- abs( mean(capacidad4[,i][df$SEXO==1]) - 
                                           mean(capacidad4[,i][df$SEXO==0]) )
        
        # calculate the boot-test-stat2 and save it (ÁMBITO)
        Boot.test.stat2[i] <- abs( mean(capacidad4[,i][df$AMBITOR==1]) - 
                                           mean(capacidad4[,i][df$AMBITOR==0]) )
}

#H0: las medias son iguales para ambos grupos
#p-value menor a 0.05 rechaza la H0 al 5% de significancia

# let's calculate the p-value for test statistic 1 and 2 (abs diff in means)
#sum(Boot.test.stat1 >= test.stat1)/B
mean( Boot.test.stat1 >= test.stat1) #SEXO  

#sum(Boot.test.stat2 >= test.stat2)/B
mean( Boot.test.stat2 >= test.stat2) #AMBITO


######################################################
# SALUD
######################################################

# lets calculate the absolute diff in means
test.stat1 <- abs(mean(df$salud2[df$SEXO==1]) - mean(df$salud2[df$SEXO==0]))  #diff in means
test.stat2 <- abs(mean(df$salud2[df$AMBITOR==1]) - mean(df$salud2[df$AMBITOR==0]))  #diff in means

# initialize the vector to store the TEST-STATS
Boot.test.stat1 <- rep(0,B)  #PARA SEXO
Boot.test.stat2 <- rep(0,B)  #PARA ÁMBITO


for (i in 1:B){
        # calculate the boot-test-stat1 and save it (SEXO)
        Boot.test.stat1[i] <- abs( mean(capacidad5[,i][df$SEXO==1]) - 
                                           mean(capacidad5[,i][df$SEXO==0]) )
        
        # calculate the boot-test-stat2 and save it (ÁMBITO)
        Boot.test.stat2[i] <- abs( mean(capacidad5[,i][df$AMBITOR==1]) - 
                                           mean(capacidad5[,i][df$AMBITOR==0]) )
}

#H0: las medias son iguales para ambos grupos
#p-value menor a 0.05 rechaza la H0 al 5% de significancia

# let's calculate the p-value for test statistic 1 and 2 (abs diff in means)
#sum(Boot.test.stat1 >= test.stat1)/B
mean( Boot.test.stat1 >= test.stat1) #SEXO  

#sum(Boot.test.stat2 >= test.stat2)/B
mean( Boot.test.stat2 >= test.stat2) #AMBITO