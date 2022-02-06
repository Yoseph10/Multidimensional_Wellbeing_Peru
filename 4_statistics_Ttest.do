clear all
cls

global main "C:\Cursos_PUCP\2021-1\Tesis 2\Base de datos\BASE_FINAL"
global dta    "$main/Bases de Datos"
global works "$main/Outputs"

cd "$dta"

use base_final_pesos


*p80e: social integration
*p80g: agency
*p80l: citizenship
*p80h: security
*p80d: health


*-----------------------------------------------------------------------------
*NORMALIZACION ENTRE CAPACIDADES 
*-----------------------------------------------------------------------------

*Permite comparación entre capacidades
rename sociabilidad2 Sociabilidad
rename agencia2 Agencia
rename ciudadania2 Ciudadanía
rename seguridad2 Seguridad
rename salud2 Salud

global varlist2 "Sociabilidad Agencia Ciudadanía Seguridad Salud"


**********************************
*COMPARANDO CAPACIDADES (AGREGADO)
**********************************

***ESTADÍSTICOS***

tabstat $varlist2, stats(n mean median min max sd skewness kurtosis) format(%6.3fc) save
mat T = r(StatTotal)' // the prime is for transposing the matrix

putexcel set "comparando_capacidades_agregado.xlsx", replace 
putexcel A1 = matrix(T), names


kdensity Sociabilidad, addplot(kdensity Agencia ||kdensity Ciudadanía || kdensity Seguridad || kdensity Salud ) ///
ytitle("Densidad") xtitle("Puntajes normalizados") ///
legend(col(2) label (1 "Sociabilidad") label (2 "Agencia") label (3 "Ciudadanía") label (4 "Seguridad") label (5 "Salud")) ///
graphregion(color(white)) 

***HISTOGRAMAS***

*Sociabilidad
histogram Sociabilidad ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes normalizados") ///
	title("Sociabilidad") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")
	
*Agencia
histogram Agencia ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes normalizados") ///
	title("Agencia / Empoderamiento") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")

*Ciudadania
histogram Ciudadanía ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes normalizados") ///
	title("Ciudadanía") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")

*Seguridad
histogram Seguridad ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes normalizados") ///
	title("Seguridad") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")

*Salud
histogram Salud ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes normalizados") ///
	title("Salud") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")

	
***DISTRIBUCIONES ACUMULADAS***	

*Previamente instalar distplot - buscar en search distplot
distplot $varlist2, ///
	ytitle("Porcentaje acumulado",size(small)) xtitle("Puntajes normalizados", size(small)) ///
	title("Distribuciones acumuladas de las capacidades") ///
	legend( label (1 "Sociabilidad") label (2 "Agencia / Empoderamiento") label (3 "Ciudadanía") ///
	label (4 "Seguridad") label (5 "Salud") ) ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")
	
	
**********************************
*COMPARANDO CAPACIDADES (DESAGREGADO)
**********************************

***********
*NORMALIDAD
***********

*testeando normalidad 
sfrancia $varlist2
sktest $varlist2

  /*En todos los casos, se rechaza H0: normalidad en la variable (sería problemático
  usar el ttest*/

  
*Testando normalidad por grupos en base a kernels

foreach i in $varlist2{

kdensity `i'  if ambitor==1, addplot(kdensity `i'  if ambitor==0 ||kdensity `i'  if sexo==0 || kdensity `i'  if sexo==1) ///
ytitle("Densidad") xtitle("Puntajes normalizados") ///
title("`i'") ///
legend(col(2) label (1 "Urbano") label (2 "Rural") label (3 "Hombre") label (4 "Mujer")) ///
graphregion(color(white)) 

graph export "$works/`i'_grupos.png", as(png) name("Graph") replace
}	
	
***********	
*Desagregado por ambito
************

*urbano = 1/ rural = 0

graph box $varlist2 , over(ambitor, relabel(1 "Rural" 2 "Urbano")) ///
ytitle("Puntajes normalizados", size(small)) ///
legend( label (1 "Sociabilidad") label (2 "Agencia / Empoderamiento") label (3 "Ciudadanía") ///
label (4 "Seguridad") label (5 "Salud") ) ///
graphregion(color(white)) 

	
*Test de varianza (para ver si las varianzas de dos grupos es iguales)
foreach var in Sociabilidad Agencia Ciudadanía Seguridad Salud{

sdtest `var', by(ambitor) 

}
   
   *Ho: ratio = 1/ varianza-desv estandar iguales entre grupos
   
   *Sociabilidad: no se rechaza H0
   *Agencia: se rechaza H0
   *Ciudadania: no se rechaza H0
   *Seguridad: no se rechaza H0
   *Salud: no se rechaza H0


*Test de medias
foreach var in Sociabilidad Agencia Ciudadanía Seguridad Salud{

ttest `var', by(ambitor) 
}

ttest agencia2, by(ambitor) unequal

*Wilcoxon rank-sum test
foreach var in Sociabilidad Agencia Ciudadanía Seguridad Salud{

ranksum  `var', by(ambitor) 
}
*Se rechaza H0 en todos los casos

***********	
*Desagregado por sexo
************

*mujer = 1 /hombre=0

graph box $varlist2 , over(sexo, relabel(1 "Hombre" 2 "Mujer")) ///
ytitle("Puntajes normalizados", size(small)) ///
legend( label (1 "Sociabilidad") label (2 "Agencia / Empoderamiento") label (3 "Ciudadanía") ///
label (4 "Seguridad") label (5 "Salud") ) ///
graphregion(color(white))

*Test de varianza
foreach var in Sociabilidad Agencia Ciudadanía Seguridad Salud{

sdtest `var', by(sexo) 

}

   *Ho: ratio = 1/ varianza-desv estandar iguales entre grupos
   
   *Sociabilidad: no se rechaza H0
   *Agencia: no se rechaza H0
   *Ciudadania: no se rechaza H0
   *Seguridad: no se rechaza H0
   *Salud: no se rechaza H0
   
   
*Test de medias
foreach var in Sociabilidad Agencia Ciudadanía Seguridad Salud{

ttest `var', by(sexo) 

}
*Se rechaza H0 en todos los casos


*Wilcoxon rank-sum test
foreach var in Sociabilidad Agencia Ciudadanía Seguridad Salud{

ranksum `var', by(sexo) 
}

************************************
*AGREGANDO CAPACIDADES (BIENESTAR)
************************************

*Bienestar con pesos individuales
gen bienestar_ind2 = Sociabilidad*wp80e + Agencia*wp80g + Ciudadanía*wp80l + Seguridad*wp80h + Salud*wp80d

*Bienestar con pesos sociales
gen bienestar_soc2 = Sociabilidad*w_soc_p80e + Agencia*w_soc_p80g + Ciudadanía*w_soc_p80l + Seguridad*w_soc_p80h + Salud*w_soc_p80d

*Bienestar con pesos iguales
gen bienestar_igual2 = (Sociabilidad + Agencia + Ciudadanía + Seguridad + Salud)/5


global bienestar2 "bienestar_ind2 bienestar_soc2 bienestar_igual2" 

*SIN SEGURIDAD
gen bienestar_ind2s = Sociabilidad*wp80e_2 + Agencia*wp80g_2 + Ciudadanía*wp80l_2 + Salud*wp80d_2

gen bienestar_soc2s = Sociabilidad*w_soc_p80e_2 + Agencia*w_soc_p80g_2 + Ciudadanía*w_soc_p80l_2 + Salud*w_soc_p80d_2

gen bienestar_igual2s = (Sociabilidad + Agencia + Ciudadanía + Salud)/4

***ESTADÍSTICOS***

*Comparando los distintos tipos de agregación
tabstat $bienestar2, stats(n mean median min max sd skewness kurtosis) format(%6.3fc) save
mat T = r(StatTotal)' 

putexcel set "comparando_bienestar_agregado.xlsx", replace 
putexcel A1 = matrix(T), names


***HISTOGRAMAS***

histogram bienestar_ind2 ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes ponderados con pesos individuales") ///
	title("Bienestar") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")

histogram bienestar_soc2 ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes ponderados con pesos sociales") ///
	title("Bienestar") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")
	
histogram bienestar_igual2 ,frequency kdensity fcolor(olive_teal) lcolor(emerald) ///
	ytitle("Frecuencia") xtitle("Puntajes ponderados con pesos iguales") ///
	title("Bienestar") ///
	graphregion(color(white)) 
	*note("Fuente: Elaboración propia.")

*Densidad kernel estimadas
kdensity bienestar_ind2, addplot(kdensity bienestar_soc2 || kdensity bienestar_igual2) ///
    ytitle("Densidad") xtitle("Puntajes de bienestar multidimensional") ///
	legend(pos(3) col(1) label (1 "Con pesos individuales") label (2 "Con pesos sociales") label (3 "Con pesos iguales") ) ///
	graphregion(color(white)) 

	
************************************
*COMPARANDO BIENESTAR (DESAGREGADO)
************************************

*testeando normalidad
sfrancia $bienestar2
sktest $bienestar2

*Testando normalidad por grupos en base a kernels

kdensity bienestar_ind2 if ambitor==1, addplot(kdensity bienestar_ind2  if ambitor==0 ||kdensity bienestar_ind2  if sexo==0 || kdensity bienestar_ind2  if sexo==1) ///
ytitle("Densidad") xtitle("Puntajes normalizados") ///
title("Bienestar con pesos individuales") ///
legend(col(2) label (1 "Urbano") label (2 "Rural") label (3 "Hombre") label (4 "Mujer")) ///
graphregion(color(white)) 
graph export "$works/bienestar_ind2_grupos.png", as(png) name("Graph") replace


kdensity bienestar_soc2 if ambitor==1, addplot(kdensity bienestar_soc2  if ambitor==0 ||kdensity bienestar_soc2  if sexo==0 || kdensity bienestar_soc2  if sexo==1) ///
ytitle("Densidad") xtitle("Puntajes normalizados") ///
title("Bienestar con pesos sociales") ///
legend(col(2) label (1 "Urbano") label (2 "Rural") label (3 "Hombre") label (4 "Mujer")) ///
graphregion(color(white)) 
graph export "$works/bienestar_soc2_grupos.png", as(png) name("Graph") replace


kdensity bienestar_igual2 if ambitor==1, addplot(kdensity bienestar_igual2 if ambitor==0 ||kdensity bienestar_igual2 if sexo==0 || kdensity bienestar_igual2  if sexo==1) ///
ytitle("Densidad") xtitle("Puntajes normalizados") ///
title("Bienestar con pesos iguales") ///
legend(col(2) label (1 "Urbano") label (2 "Rural") label (3 "Hombre") label (4 "Mujer")) ///
graphregion(color(white)) 
graph export "$works/bienestar_igual2_grupos.png", as(png) name("Graph") replace


***********	
*Desagregado por ambito
************

*urbano = 1/ rural = 0
tabstat $bienestar2, by(ambitor) stats(n mean sd) format(%6.3fc)

*Test de varianza (para ver si las varianzas de dos grupos es iguales)
foreach var in bienestar_ind2 bienestar_soc2 bienestar_igual2 {

sdtest `var', by(ambitor) 

}
   
   *Ho: ratio = 1/ varianza-desv estandar iguales entre grupos
   
   *bienestar_ind2: se rechaza H0
   *bienestar_soc2: se rechaza H0
   *bienestar_igual2: se rechaza H0

*Test de medias
foreach var in bienestar_ind2 bienestar_soc2 bienestar_igual2{

ttest `var', by(ambitor) unequal

}

***********	
*Desagregado por sexo
************

*mujer = 1 /hombre=0
tabstat $bienestar2, by(sexo) stats(n mean) format(%6.3fc)

*Test de varianza (para ver si las varianzas de dos grupos es iguales)
foreach var in bienestar_ind2 bienestar_soc2 bienestar_igual2 {

sdtest `var', by(sexo) 

}
   
   *Ho: ratio = 1/ varianza-desv estandar iguales entre grupos
   
   *bienestar_ind2: no se rechaza H0
   *bienestar_soc2: no se rechaza H0
   *bienestar_igual2: no se rechaza H0
   
   
*test de medias
foreach var in bienestar_ind2 bienestar_soc2 bienestar_igual2 bienestar_idh2{

ttest `var', by(sexo) 

}

*********
*SIN SEGURIDAD
*********

*Test de varianza
foreach var in bienestar_ind2s bienestar_soc2s bienestar_igual2s{

sdtest `var', by(ambitor) 

}
   
   *Ho: ratio = 1/ varianza-desv estandar iguales entre grupos
   
   *bienestar_ind2s: no se rechaza H0
   *bienestar_soc2s: no se rechaza H0
   *bienestar_igual2s: no se rechaza H0


*desagregado por ámbito
foreach var in bienestar_ind2s bienestar_soc2s bienestar_igual2s{

ttest `var', by(ambitor) 

}

