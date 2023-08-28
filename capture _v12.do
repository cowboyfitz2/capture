********************************************************************************
*** Do file for v12 of reg capture paper (Pub Choice R&R)***********************
*** started 6/22/23 ************************************************************
use "E:\434236WRD\Root\Documents\Archive\SurfaceArchive10242019\ND\merged_4PERCseminar1.dta", clear
set more off
egen fieldtag=tag(fieldname date)
bys fieldname date: egen fieldwells=count(apino!=.)
recode fieldviolate(.=0) if fieldtag==1
lab var z "Field Illegal Flaring"
lab var switchup "Rule Tightens"
lab var switchdown "Rule Loosens"

**Fix coding of rule switch, which was backwards
/*Mapping of Rules in data to table 1
K->B
I->D
H->G
C->F
D->E
A->A
B->D */
g switch_up=switchdown
g switch_down=switchup
lab var switch_up "Rule Tightens"
lab var switch_down "Rule Loosens"
drop switchup switchdown
rename switch_up switchup
rename switch_down switchdown

 merge m:1 year month using "E:\434236WRD\Root\Documents\Research\Capture\NDprices.dta"
 g fmonthoilrev=fmonth_oil*fpp_oil_nd
 g fmonthgasrev=(fieldgas-fieldflare)*cg_ng_nd
 g fmonthrev=fmonthoilrev+fmonthgasrev
 lab var fmonthrev "Field Monthly Revenue"

*****Ordered Probit
g orderedrule=6 if rule=="A"
replace orderedrule=5 if rule=="K"
replace orderedrule=4 if rule=="I"
replace orderedrule=4 if rule=="B"
replace orderedrule=3 if rule=="D"
replace orderedrule=2 if rule=="C"
replace orderedrule=1 if rule=="H"

oprobit orderedrule fmonthrev if fieldtag==1 
est sto op1
margins, dydx (fmonthrev)
est sto op1marg
oprobit orderedrule fieldflare if fieldtag==1 
est sto op1a
oprobit orderedrule fmonthrev fieldflare if fieldtag==1
est sto op2
oprobit orderedrule fmonthrev fieldflare _Ffieldname* if fieldtag==1
est sto op3
estout op1marg op1a op2 op3, style(tex) label cells(b(fmt(9) star) se(fmt(9) par)) stats(N ll bic) drop(_Ffield*) replace
***This table goes in the response to reviewer document

egen fieldcode=group(fieldname)
keep if fieldtag==1
bys fieldcode: egen frevmean=mean(fmonthrev)
bys fieldcode: egen fn=count(frevmean)
egen frevvar1=total((fmonthrev-frevmean)^2)
g frevvar=frevvar1/fn
g frevsd=sqrt(frevvar)

logit switch frevsd
est sto l1
margins, dydx (frevsd)
est sto l1marg
logit switch fieldflare if fieldtag==1 
est sto l2
margins, dydx (fieldflare)
est sto l2marg
logit switch frevsd fieldflare if fieldtag==1
est sto l3
margins, dydx (frevsd fieldflare)
est sto l3marg
logit switch frevsd fieldflare _Ffieldname* if fieldtag==1
est sto l4
margins, dydx (frevsd fieldflare)
est sto l4marg
estout l1marg l2marg l3marg l4marg, style(tex) label cells(b(fmt(9) star) se(fmt(9) par)) stats(N ll bic) drop(_Ffield*) replace
***This table goes in the response to reviewer document



set more off
cd "D:\434236WRD\Root\Documents\Research\Capture\Results"
logit switch fieldwells if fieldtag==1, cl(fieldname)
est sto est1

logit switch fieldwells fmonth_oil _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est2
margins, dydx(fieldwells fmonth_oil)
est sto est2marg

logit switch fieldwells fmonth_oil fieldviolate _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est3
margins, dydx (fieldwells fmonth_oil fieldviolate)
est sto est3marg

logit switch fieldwells fmonth_oil z _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est4
margins, dydx (fieldwells fmonth_oil z)
est sto est4marg

logit switch fieldwells fmonth_oil fieldflare _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est4b
margins, dydx (fieldwells fmonth_oil fieldflare)
est sto est4bmarg

logit switch fieldwells fmonth_oil fieldflare z _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est4c
margins, dydx (fieldwells fmonth_oil fieldflare)
est sto est4cmarg

logit switchup fieldwells fmonth_oil _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est5
margins, dydx (fieldwells fmonth_oil)
est sto est5marg

logit switchup fieldwells fmonth_oil fieldviolate _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est6
margins, dydx (fieldwells fmonth_oil fieldviolate)
est sto est6marg

logit switchup fieldwells fmonth_oil z _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est7
margins, dydx (fieldwells fmonth_oil z)
est sto est7marg

logit switchup fieldwells fmonth_oil z fieldflare _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est7b
margins, dydx (fieldwells fmonth_oil z)
est sto est7bmarg

logit switchdown fieldwells fmonth_oil _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est8
margins, dydx (fieldwells fmonth_oil)
est sto est8marg

logit switchdown fieldwells fmonth_oil fieldviolate _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est9
margins, dydx (fieldwells fmonth_oil fieldviolate)
est sto est9marg

logit switchdown fieldwells fmonth_oil z _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est10
margins, dydx (fieldwells fmonth_oil z)
est sto est10marg

logit switchdown fieldwells fmonth_oil z fieldflare _Ffield* _Yyear* if fieldtag==1, cl(fieldname)
est sto est10b
margins, dydx (fieldwells fmonth_oil z)
est sto est10bmarg

lab var fieldwells "Field Wells"

****This creates table 6 for the MS
estout est2marg est4marg est4bmarg est4cmarg using "aztab6.tex", style(tex) label cells(b(fmt(4) star) se(fmt(4) par)) stats(N ll bic) drop(_Ffield* _Yyear*) replace
****This creates table 7 for the MS
estout est6marg est7marg est7bmarg est9marg est10marg est10bmarg using "aztab7.tex", style(tex) label cells(b(fmt(4) star) se(fmt(4) par)) stats(N ll bic) drop(_Ffield* _Yyear*) replace

***Include month-of-year controls for reviewer appendix
xi i.month, pre(_M) noomit
logit switch fieldwells fmonth_oil _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est2m
margins, dydx(fieldwells fmonth_oil)
est sto est2mmarg

logit switch fieldwells fmonth_oil z _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est4m
margins, dydx (fieldwells fmonth_oil z)
est sto est4mmarg

logit switch fieldwells fmonth_oil fieldflare _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est4bm
margins, dydx (fieldwells fmonth_oil fieldflare)
est sto est4bmmarg

logit switch fieldwells fmonth_oil fieldflare z _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est4cm
margins, dydx (fieldwells fmonth_oil fieldflare)
est sto est4cmmarg

logit switchup fieldwells fmonth_oil fieldviolate _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est6m
margins, dydx (fieldwells fmonth_oil fieldviolate)
est sto est6mmarg

logit switchup fieldwells fmonth_oil z _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est7m
margins, dydx (fieldwells fmonth_oil z)
est sto est7mmarg

logit switchup fieldwells fmonth_oil z fieldflare _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est7bm
margins, dydx (fieldwells fmonth_oil z)
est sto est7bmmarg

logit switchdown fieldwells fmonth_oil fieldviolate _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est9m
margins, dydx (fieldwells fmonth_oil fieldviolate)
est sto est9mmarg

logit switchdown fieldwells fmonth_oil z _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est10m
margins, dydx (fieldwells fmonth_oil z)
est sto est10mmarg

logit switchdown fieldwells fmonth_oil z fieldflare _Ffield* _Yyear* _Mmonth* if fieldtag==1, cl(fieldname)
est sto est10bm
margins, dydx (fieldwells fmonth_oil z)
est sto est10bmmarg

estout est2mmarg est4mmarg est4bmmarg est4cmmarg using "tab6_wmonths.tex", style(tex) label cells(b(fmt(4) star) se(fmt(4) par)) stats(N ll bic) drop(_Ffield* _Yyear* _Mmonth*) replace
estout est6mmarg est7mmarg est7bmmarg est9mmarg est10mmarg est10bmmarg using "tab7_wmonths.tex", style(tex) label cells(b(fmt(4) star) se(fmt(4) par)) stats(N ll bic) drop(_Ffield* _Yyear* _Mmonth*) replace

********************************************************************************
**Firm Level Analysis***********************************************************
********************************************************************************
egen firmcode=group(operator)

gen marker=1
bys firmcode date: egen firmwellstag=total(marker)
*bys firmcode date: egen firmactivefields=group(fieldname)
bys firmcode date: egen firmwells=total(firmwellstag)
bys firmcode date: egen firmillegalflare=total(illegalflare)
bys firmcode date: egen firmflare=total(flaregas)
bys firmcode date: egen firmoil=total(oil)
bys firmcode date: egen firmswitch=total(switch)
bys firmcode date: egen firmswitchup=total(switchup)
bys firmcode date: egen firmswitchdown=total(switchdown)
bys firmcode date: egen firmindicator=seq()

zip firmswitch firmwells firmillegalflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est11
margins, dydx (firmwells firmillegalflare firmoil)
est sto est11m
zip firmswitch firmwells firmflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est12
margins, dydx (firmwells firmflare firmoil)
est sto est12m
zip firmswitch firmwells firmflare firmillegalflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est13
margins, dydx (firmwells firmillegalflare firmflare firmoil)
est sto est13m

****This creates table 8 for the MS
estout est11m est12m est13m using "v11_zip1.tex", style(tex) label cells(b(star) se(par)) stats(N ll bic) replace

zip firmswitchdown firmwells firmillegalflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est14
margins, dydx (firmwells firmillegalflare firmoil)
est sto est14m
zip firmswitchdown firmwells firmflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est15
margins, dydx (firmwells firmflare firmoil)
est sto est15m
zip firmswitchdown firmwells firmflare firmillegalflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est16
margins, dydx (firmwells firmillegalflare firmflare firmoil)
est sto est16m

****This creates directional ZIP results for relaxing
estout est14m est15m est16m using "v11_zip2.tex", style(tex) label cells(b(star) se(par)) stats(N ll bic) replace

zip firmswitchdown firmwells firmillegalflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est17
margins, dydx (firmwells firmillegalflare firmoil)
est sto est17m
zip firmswitchdown firmwells firmflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est18
margins, dydx (firmwells firmflare firmoil)
est sto est18m
zip firmswitchdown firmwells firmflare firmillegalflare firmoil _Ooperator* if firmindicator==1, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est19
margins, dydx (firmwells firmillegalflare firmflare firmoil)
est sto est19m

****This creates directional ZIP results for tightening
estout est17m est18m est19m using "v11_zip3.tex", style(tex) label cells(b(star) se(par)) stats(N ll bic) replace

g largeoperator=1 if operator=="EOG RESOURCES, INC." | operator=="HESS BAKKEN INVESTMENTS II, LLC" | operator=="MARATHON OIL COMPANY" | operator=="WHITING OIL AND GAS CORPORATION" 
recode largeoperator (.=0)

zip firmswitch firmwells firmillegalflare firmoil _Ooperator* if firmindicator==1 & largeoperator==0, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est20
margins, dydx (firmwells firmillegalflare firmoil)
est sto est20m
zip firmswitch firmwells firmflare firmoil _Ooperator* if firmindicator==1 & largeoperator==0, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est21
margins, dydx (firmwells firmflare firmoil)
est sto est21m
zip firmswitch firmwells firmflare firmillegalflare firmoil _Ooperator* if firmindicator==1 & largeoperator==0, inflate(illegalflare fieldwells fmonth_oil _Ffieldname*) cluster(operator)
est sto est22
margins, dydx (firmwells firmillegalflare firmflare firmoil)
est sto est22m

****This creates table of ZIP exclduing large operators for the MS appendix
estout est20m est21m est22m using "v11_zip4.tex", style(tex) label cells(b(star) se(par)) stats(N ll bic) replace

*Rule switches
/*Mapping of Rules in data to table 1
K->B
I->D
H->G
C->F
D->E
A->A
B->D */
g newrule="B" if rule=="K"
replace newrule="G" if rule=="H"
replace newrule="F" if rule=="C"
replace newrule="E" if rule=="D"
replace newrule="A" if rule=="A"
replace newrule="D" if rule=="B"

g switch_to="B" if switchto=="K"
replace switch_to="G" if switchto=="H"
replace switch_to="F" if switchto=="C"
replace switch_to="E" if switchto=="D"
replace switch_to="A" if switchto=="A"
replace switch_to="D" if switchto=="B"

g switch_from="B" if switchfrom=="K"
replace switch_from="G" if switchfrom=="H"
replace switch_from="F" if switchfrom=="C"
replace switch_from="E" if switchfrom=="D"
replace switch_from="A" if switchfrom=="A"
replace switch_from="D" if switchfrom=="B"

egen tag3=tag(month_c fieldname)
keep if tag3==1
*****summary of field rules and changes
tab newrule switch 
*****field rule changes
tab switch_from switch_to

