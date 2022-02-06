clear all
cls

global main "C:\Cursos_PUCP\2021-1\Tesis 2\Base de datos\BASE_FINAL"
global dta    "$main/Bases de Datos"

cd "$dta"

import delimited "base_capability_score"


********************************
*VARS DEFINITION:
********************************
*p80e: social integration
*p80g: agency
*p80l: citizenship
*p80h: security
*p80d: health


*********************************
*WEIGHTS
*********************************

egen denominador = rowtotal(p80e p80g p80l p80h p80d)

*without security
egen denominador_2 = rowtotal(p80e p80g p80l p80d)


*** INDIVIDUAL WEIGHTS

foreach var in p80e p80g p80l p80h p80d{

gen w`var' = `var'/denominador

}

       *gen x = wp80e + wp80g + wp80l + wp80h + wp80d / la suma da 1

	   
*without security
foreach var in p80e p80g p80l p80d{

gen w`var'_2 = `var'/denominador_2

}
	   
	      
*** SOCIAL WEIGHTS

foreach var in p80e p80g p80l p80h p80d{

egen w_soc_`var' = mean(w`var')

}

       *gen y = w_soc_p80e + w_soc_p80g + w_soc_p80l + w_soc_p80h + w_soc_p80d / la suma da 1

	   
*without security
foreach var in p80e p80g p80l p80d{

egen w_soc_`var'_2 = mean(w`var'_2)

}


mean w_soc_*

save "base_final_pesos", replace