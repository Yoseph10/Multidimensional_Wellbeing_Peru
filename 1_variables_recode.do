clear all
cls

global main "C:\Cursos_PUCP\2021-1\Tesis 2\Base de datos\BASE_FINAL"
global dta    "$main/Bases de Datos"

cd "$dta"

*unicode analyze "base_final.dta"
*unicode encoding set latin1 //puede ser con ISO-8859-10 también
*unicode translate "base_final.dta"

use base_final


*******************************************************************************
*FUNCTIONINGS
*******************************************************************************

***SOCIAL INTEGRATION


recode P50F (0=.), gen(P50FR)

foreach var in A B C D E{

recode P50`var' (0=1) (1=2) (2=3) (3=4), gen(P50`var'R)

}

foreach var in A B{

recode P55`var' (0=4) (1=3) (2=2) (3=1), gen(P55`var'R)

}


***AGENCY

recode P56 (0=1) (1=2) (2=3) (3=4), gen(P56R)

foreach var in 7 8 9{

recode P5`var' (0=5) (1=4) (2=3) (3=2) (4=1), gen(P5`var'R)

}

***CITIZENSHIP

foreach var in A B C D E F G H I{

recode P64`var' (0 = .) (1=4) (2=3) (3=2) (4=1), gen(P64`var'R)

}

foreach var in A B C D{

recode P65`var' (0 = .) , gen(P65`var'R)

}


***REST-LEISURE

gen dormir = (P66LU_H + P66MA_H + P66MI_H + P66JU_H + P66VI_H + P66SA_H + P66DO_H)/7

*tabstat dormir ,by(AMBITO) 

gen dormirR = 1 if dormir < 6
replace dormirR = 2 if dormir >=6 & dormir <= 9
replace dormirR = 3 if dormir>9 

*tabstat dormirR ,by(AMBITO) 


gen ocio = (P67LU_H + P67MA_H + P67MI_H + P67JU_H + P67VI_H + P67SA_H + P67DO_H)/7
*tabstat ocio, stats(n mean p25 p50 p75 p95)
*tabstat ocio ,by(AMBITO) 


gen ocioR = 1 if ocio < 0.5
replace ocioR = 2 if ocio >=0.5 & ocio <= 1.5
replace ocioR = 3 if ocio >1.5 & ocio <= 3
replace ocioR = 4 if ocio >3


*******************************************************************************
*Transformation of EXOGENOUS
*******************************************************************************

*Exógenas de características del hogar

foreach var in D E F{

recode P44`var' (0 = 1) (1 = 0) , gen(P44`var'R)

}

gen tel_cel = 1 if P44A==0 | P44B==0 | P44C==0
replace tel_cel = 0 if P44A==1 & P44B==1 & P44C==1
*1 = sí
*0 = no

recode P35 (0 = 1) (1 = 0) , gen(agua)


*Exogenous of household characteristics

recode AMBITO (0 1 = 1) (2 = 0), gen(AMBITOR)
*1 = Lima-Callao e Interior Urbano
*0 = Interior Rural

recode AMBITO (0 = 1) (1 2 = 0), gen(lima_callao)
recode AMBITO (1 = 1) (0 2 = 0), gen(int_urb)
recode AMBITO (2 = 1) (0 1 = 0), gen(int_rural)


recode P3 (0 = 1) (1 = 2) (2 = 3) (3 = 4) (4 = 5) (5 = 6) (6 = 7) (7 = 8) (8 = 9) (9 = 10) (10 = 11), gen (educ)


************
*Health
************

*near the medical center
recode P46 (0=6) (1=5) (2=4) (3=3) (4=2) (5=1), gen(P46R)


*health insurance
gen seguro = 1 if P68A==0 | P68B==0 | P68C==0 | P68D==0 | P68E==0 | P68F==0 | P68G==0 | P68H==0

replace seguro = 0 if P68A== 1 & P68B== 1 & P68C== 1 & P68D== 1 & P68E== 1 & P68F== 1 & P68G== 1 & P68H== 1

*toilet service
recode P37 (0=6) (1=5) (2=4) (3=3) (4=2) (5=1), gen(P37R)



save "base_final_recode.dta", replace
