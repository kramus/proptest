********************
*Analisis de Suarez*
*years sin agrupar**
********************

gen p=100*dif/casos
sort year
twoway (line p year if tipo==1) (line p year if tipo==2)

*Comparacion x yr

glm dif year if tipo==1 , fam(poi) lnoff(casos) ef
glm dif year if tipo==2 , fam(poi) lnoff(casos) ef


*Comparacion x tipo

glm dif i.tipo  , fam(poi) lnoff(casos) ef


*Eval interaccion

xi: glm dif i.tipo*i.year  , fam(poi) lnoff(casos) 
estimates store model1
xi: glm dif i.tipo i.year  , fam(poi) lnoff(casos) 
lrtest model1

xi: glm dif i.tipo*i.year  , fam(poi) lnoff(casos) 

lincom  _Itipo_2, rrr
lincom  _Itipo_2 + _ItipXyea_2_2005, rrr
lincom  _Itipo_2 + _ItipXyea_2_2006, rrr
lincom  _Itipo_2 + _ItipXyea_2_2007, rrr
lincom  _Itipo_2 + _ItipXyea_2_2008, rrr
lincom  _Itipo_2 + _ItipXyea_2_2009, rrr
lincom  _Itipo_2 + _ItipXyea_2_2010, rrr
lincom  _Itipo_2 + _ItipXyea_2_2011, rrr
lincom  _Itipo_2 + _ItipXyea_2_2012, rrr
lincom  _Itipo_2 + _ItipXyea_2_2013, rrr
lincom  _Itipo_2 + _ItipXyea_2_2014, rrr


*********************************
***Analisis con years agrupados**
*********************************
**Labels y recodificaciones
label define mod 1"MSM" 2"IDU"
label value tipo mod
*recode year en grupos de 3
recode year (2004/2006 = 1 "2004-2006") (2007/2009 = 2 "2007-2009") (2010/2012 = 3 "2010-2012") /*
			*/(2013/max = 4 "2013-2014"), gen(cat_year) label(yr)

**Recode de todas las variables en base al cat_year
bysort cat_year tipo: egen cat_dif = sum(dif)
bysort cat_year tipo: egen cat_caso = sum(casos)
gen cat_p = 100*cdif/cat_caso

*Exportar DB a csv
*outsheet propdatos.csv, comma


**Graficas de numero de muertes y de los percent changes
twoway (line cat_p cat_year if tipo==1) (line cat_p cat_year if tipo==2), /*
		*/ytitle("Percent Change") xtitle("Period of study") xlabel(1 2 3 4, valuelabels angle(90)) legend(lab(1 "MSM") lab(2 "IDU"))/*
		*/saving(pchange.png)

*Comparacion x yr

glm cat_dif cat_year if tipo==1 , fam(poi) lnoff(cat_caso) ef
glm cat_dif cat_year if tipo==2 , fam(poi) lnoff(cat_caso) ef


*Comparacion x tipo

glm cat_dif i.tipo  , fam(poi) lnoff(cat_caso) ef


*Eval interaccion

*Modelo con interaccion
xi: glm cat_dif i.tipo*i.cat_year  , fam(poi) lnoff(cat_caso) 
estimates store model1
*Modelo crudo
xi: glm cat_dif i.tipo i.cat_year  , fam(poi) lnoff(cat_caso) 
lrtest model1

*Se usa modelo con interaccion
xi: glm cat_dif i.tipo*i.cat_year  , fam(poi) lnoff(cat_caso) 

lincom  _Itipo_2, rrr
lincom  _Itipo_2 + _ItipXcat_2_2, rrr
lincom  _Itipo_2 + _ItipXcat_2_3, rrr
lincom  _Itipo_2 + _ItipXcat_2_4, rrr
