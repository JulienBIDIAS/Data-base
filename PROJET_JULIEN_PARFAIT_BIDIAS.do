// PROGRAM EVALUATION FOR INTERNATIONAL DEVELOPMENT

***************** PROJET ISE-3 2023  ******************

// PROFESSOR Omar SENE

// Identification

*** NOM     : BIDIAS ASSALA 
*** PRENOM  : JULIEN PARFAIT
*** CLASSE  : ISE-3
*** ECOLE   : ENSAE PIERRE NDIAYE


*** PARAMETRAGE 

clear all
version 18.0
set more off

*** DEFINITION DU REPERTOIRE AUTOMATIQUE 




global workD "C:\Livre_ISE3\Evaluation_impact_des_politiques_publiques\PROJET 2023\PROJET_JULIEN_PARFAIT_BIDIAS"

global PrimD "$workD\PrimaryData"
global TempD "$workD\TempData"
global SecD  "$workD\SecondaryData"
global ProgD "$workD\Programs" /* Pour les programmes*/

global ResultD "$workD\Output" /* Pour les résultats*/
**********************************************************************************************

cd "$ResultD"/*Repertoire par de travail par défaut*/ 

** load the data set



webuse set https://julienbidias.github.io/julienbidias.gihtub.io/

webuse set https://julienbidias.github.io/Data-base/

webuse ECON523-Meyersson-data, clear 


****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************

****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************


****** 1. Part of Table I, specifically the contents of the red box (the first three variables in the table) :


** Creation de la variable mayor in 1994

gen mayor1994=cond(margin1994>0,1,0)

tab mayor1994 


** Labellisation de la variable mayor in 1994

** Attribution du label 

label var mayor1994 "Mayor type"

** Recodage de la variable 

label define labmayor1994 0"Secular" 1"Islamic" 

label values mayor1994 labmayor1994

* tabulation avec les labels 

numlabel, add

tab mayor1994

numlabel, remove // On enleve les labels 

* variables Imayor pour les statistiques descriptives 

gen Imayor1994 = mayor1994


label var Imayor1994 "Islamic mayor in 1994"

tab Imayor1994 


********************************************************************************************************


******** REPLICATION DU 1ER TABLEAU

** Au format Excel 

** Pour executer les commendes ci-dessous, il sera nécessaire d'installer le package associé aux tableaux de statistiques groupées pour les regressions sur discontinuité (Word Bank)

** Faire 

*ssc install ietoolkit


// Installer avant d'exécuter les commendes ci-dessous. Merci. 

iebaltab hs_women hs_men Imayor1994, grpvar(mayor1994) total stats(desc(sd) pair(diff)) totallabel(All) grouplabels(1 "Islamic" @ 0 "Secular") rowvarlabels nonote control(0) order(1 0) groupcodes savexlsx("fichier2") replace 


* Le tableau sera mis en forme sous latex.


***** On tiendra compte du fait que le résultat ne retourne pas la diifrérence de moyenne du fait que l'on fait une analyse de la variable sur elle même. 


* Nous effectuons un test sur elle pour ressortir les résultats des statistiques descriptives du tableau à répliquer que nous introduirons par la suite sur latex pour remplacer le manquant au niveau de la différence de moyenne. 


ttest mayor1994, by(Imayor1994)

return list 

di r(sd)

global sd = r(sd)

sum mayor1994

return list 

di r(mean)

global MEAN = r(mean)


iebaltab hs_women hs_men Imayor1994, grpvar(mayor1994) total stats(desc(sd) pair(diff)) totallabel(All) grouplabels(1 "Islamic" @ 0 "Secular") rowvarlabels nonote control(0) order(1 0) groupcodes savetex("fichier2") replace 


************************************************************************************************************************************************************************************************************************************************************************************************************************

**** 2. Part of Figure II, specifically the histogram in Panel (a).


*** REPLICATION DE L'HISTOGRAMME 

sort margin1994

hist margin1994, bin(99) graphregion(color(white) lcolor(white) lwidth(vthick)) ylabel(, nogrid) xline(0, lpattern(shortdash) lcolor(gs6)) percent fcolor(gs10) lcolor(gs2) xtitle("Islamic win margin") xscale(lcolor(none)) yscale(lcolor(none)) plotregion(lpattern(solid) lcolor(black))

*graph export

graph export "Figure2.pdf", replace

graph export "Figure2.png", replace



****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************


***** 3. Part of Table II, specifically the contents of the red box (the first eight columns):


* regression lineaire sans covariables 


* h etant egal a 1

global Band = 1.000

* 1ere regression 
sum hs_women
return list 
global meanh = r(mean)

reg hs_women mayor1994 if abs(margin1994)< 1


outreg2 using "MON.xls", stats(coef se) ct(No) addstat(Outcome mean, $meanh , Bandwith, $Band ,R-ajusted, e(r2), Observations, e(N)) nocon nor2 word excel dec(3) noobs replace 

* regression lineaire avec covariables 

* variables au dessus du seuil  & variables pour à ajouter dans la regression quadratique et cubique 


gen margin1994i1_i = cond(margin1994>0,margin1994,0)

gen margin1994i2_i = margin1994i1_i^2

gen margin1994i3_i = margin1994i1_i^3

gen margin1994i4_i = margin1994i1_i^4

gen margin1994i1   = margin1994
gen margin1994i2   = margin1994^2
gen margin1994i3   = margin1994^3
gen margin1994i4   = margin1994^4



********************************************************************************************************
**** 2e regression *********************************


*  h etant egal a 1

regress hs_women mayor1994 margin1994i1 lnpop1994 voteshare1994 parties1994 under19 over60 sexratio provcenter submetrocenter hhsize distcenter i.province if abs(margin1994) < 1, vce(robust)


outreg2 using "MON.xls", stats(coef se) ct(Yes) addstat(Outcome mean, $meanh ,Bandwith, $Band ,R-ajusted, e(r2), Observations, e(N)) nocon nor2 word excel dec(3) noobs keep(mayor1994) append


********************************************************************************************************

* recuperation de h selon l'algorithme de Kalaynaran


* Il faut d'abord installer la commende RD

*ssc install rd, replace


rd hs_women margin1994, z0(0)

ereturn list 

global h_1 = e(w100) // pour h chapeau
global h_2 = e(w50)  // pour h/2
global h_3 = e(w200) // pour 2h


* regression lineaire sans covariables avec h chapeau =0.240

sum hs_women if abs(margin1994) <$h_1

global mean1 = r(mean)

reg hs_women mayor1994 margin1994i1* if abs(margin1994) <$h_1 , vce(robust)


outreg2 using "MON.xls", stats(coef se) ct(No) addstat(Outcome mean, $mean1 ,Bandwith, $h_1 ,R-ajusted, e(r2), Observations, e(N)) nocon  nor2 word excel dec(3) noobs keep(mayor1994) append


* regression lineaire avec covariables avec h chapeau =0.240


regress hs_women mayor1994 margin1994i1* lnpop1994 voteshare1994 parties1994 under19 over60 sexratio provcenter submetrocenter hhsize distcenter i.province if abs(margin1994) < $h_1 , vce(robust)

outreg2 using "MON.xls", stats(coef se) ct(Yes) addstat(Outcome mean, $mean1 , Bandwith, $h_1 ,R-ajusted, e(r2), Observations, e(N)) nocon nor2 word excel dec(3) noobs keep(mayor1994) append


* regression linaire avec covariables avec h/2 =0.240/2

sum hs_women if abs(margin1994) <$h_2

global mean2 = r(mean)

regress hs_women mayor1994 margin1994i1* lnpop1994 voteshare1994 parties1994 under19 over60 sexratio provcenter submetrocenter hhsize distcenter i.province if abs(margin1994) < $h_2 , vce(robust)


outreg2 using "MON.xls", stats(coef se) ct(Yes) addstat(Outcome mean, $mean2 , Bandwith, $h_2,R-ajusted, e(r2), Observations, e(N)) nocon nor2 word excel dec(3) noobs keep(mayor1994) append


* regression lineaire avec covariables avec 2h =2*0.240


sum hs_women if abs(margin1994) <$h_3

global mean3 = r(mean)


regress hs_women mayor1994 margin1994i1* lnpop1994 voteshare1994 parties1994 under19 over60 sexratio provcenter submetrocenter hhsize distcenter i.province if abs(margin1994) < $h_3 , vce(robust)


outreg2 using "MON.xls", stats(coef se) ct(Yes) addstat(Outcome mean, $mean3 , Bandwith, $h_3 ,R-ajusted, e(r2), Observations, e(N)) nocon nor2 word excel dec(3) noobs keep(mayor1994) append


* regression Quadratique 

sum hs_women if abs(margin1994) <$h_1

global meanh = r(mean)

regress hs_women mayor1994 margin1994i1* margin1994i2* lnpop1994 voteshare1994 parties1994 under19 over60 sexratio provcenter submetrocenter hhsize distcenter i.province if abs(margin1994) < $h_1

outreg2 using "MON.xls", stats(coef se) ct(Yes) addstat(Outcome mean, $meanh, Bandwith, $h_1 ,R-ajusted, e(r2), Observations, e(N)) nocon nor2 word excel dec(3) noobs keep( mayor1994) append


* regression Cubique avec covariables (pour h)


regress hs_women mayor1994 margin1994i1*   margin1994i2*  margin1994i3* lnpop1994 voteshare1994 parties1994 under19 over60 sexratio provcenter submetrocenter hhsize distcenter i.province if abs(margin1994) < $h_1, vce(robust)

outreg2 using "MON.xls", stats(coef se) nor2 ct(Yes) addstat(Outcome mean, $meanh, Bandwith, $h_1 ,R-ajusted, e(r2), Observations, e(N)) nocon word excel dec(3) noobs keep(mayor1994) append 

***************************************************************************************************************************FIN**********************************************************************************************************************************************************************************************************************************************




