**                       MSC THESIS
**	             "PREFERENCES FOR REDISTRIBUTION 
**     AND THE TASTE FOR FAIRNESS AS A COMPONENT OF CULTURE"     
**                     VASILAKI ELENI 
**
**                    DATA CLEANING FILE
**                  ======================
version 11
clear
set more off
use ESS6e02_1.dta //the 6th round of ESS data
 
recode yrbrn 7777=. 8888=. 9999=.
recode brncntr 7=. 8=. 9=. 

//Immigrant sample//
replace fbrncntb="DE" if fbrncntb=="01"
replace fbrncntb="SU" if fbrncntb=="02"
replace fbrncntb="CS" if fbrncntb=="03"
replace fbrncntb="YU" if fbrncntb=="04"
replace fbrncntb="TP" if fbrncntb=="05"
replace fbrncntb="RS" if fbrncntb=="06"

replace mbrncntb="DE" if mbrncntb=="01"
replace mbrncntb="SU" if mbrncntb=="02"
replace mbrncntb="CS" if mbrncntb=="03"
replace mbrncntb="YU" if mbrncntb=="04"
replace mbrncntb="TP" if mbrncntb=="05"
replace mbrncntb="RS" if mbrncntb=="06"


replace cntbrthc="DE" if cntbrthc=="01"
replace cntbrthc="SU" if cntbrthc=="02"
replace cntbrthc="CS" if cntbrthc=="03"
replace cntbrthc="YU" if cntbrthc=="04"
replace cntbrthc="TP" if cntbrthc=="05"
replace cntbrthc="RS" if cntbrthc=="06"

replace cntry  =lower(cntry)
replace cntbrthc=lower(cntbrthc)
replace mbrncntb=lower(mbrncntb)
replace fbrncntb=lower(fbrncntb)


gen sample=.
replace sample=0 if brncntr==1 //sample=0: natives//
replace sample=1 if brncntr==2 //sample=1: immigrants//
//sample=2: immigrants from inside ESS countries//
global countries= "al be bg ch cy cz de dk ee es fi fr gb hu ie il is it lt nl no pl pt ru se si sk ua xk" //creating country list//  

foreach cc of global countries {
    qui replace sample=2 if cntbrthc=="`cc'" 
}
tab sample
assert sample==.|sample==0|sample==1|sample==2 if brncntr<.

replace cntbrthc=cntry if sample==0

//COUNTRY DUMMIES//
foreach cc of global countries{
gen byte cd_`cc'=cntry=="`cc'"
}

gen byte imeur=sample==2 /* Indicator variables to signify type of immigrant/native status */
gen byte imoth=sample==1 /* Indicator variables to signify type of immigrant/native status */
gen byte native=sample==0 /* Indicator variables to signify type of immigrant/native status */

//Sample selection//
gen byte todrop=missing(yrbrn)
tab todrop sample, m col
drop if todrop
drop    todrop


replace agea=inwyye-yrbrn if agea==999
gen todrop=age<18
tab todrop sample, m col
drop if todrop
drop    todrop

gen byte todrop=brncntr==.
tab todrop sample, m col
drop if todrop
drop    todrop

assert sample<.

gen flagerr=brncntr==1&(cntbrthc~=cntry)
count if flagerr
tab cntry  if flagerr
tab cntbrthc if flagerr

tab flagerr sample, m col
drop if flagerr
drop 	flagerr

gen flagerr=cntry==cntbrthc&(sample==2)
tab cntry 	if flagerr
tab cntbrthc if flagerr

tab flagerr sample, m col
drop if flagerr
drop 	flagerr

gen flagerr =  cntbrthc=="66"
tab cntry  if flagerr
tab cntbrthc  if flagerr

tab flagerr sample, m col
drop if flagerr
drop 	flagerr

tab gincdif, m
tab gincdif, sum(gincdif)

gen byte todrop=gincdif>=6
tab todrop sample, m col
drop if todrop
drop    todrop

tab ipeqopt, m
tab ipeqopt, sum(ipeqopt)

gen byte todrop=ipeqopt>=7
tab todrop sample, m col
drop if todrop
drop    todrop


gen byte todrop=gndr==9
tab todrop sample, m col
drop if todrop
drop    todrop

tab sample , m

//Creating variables for Regression//
//dependent variable//
//Q:Preferences for redistribution//
recode gincdif 7/9=. 
replace gincdif=6-gincdif
label values gincdif gincdiflbl
label define gincdiflbl 1 "disagree strongly" 2 "disagree" 3 "neither" 4 "agree" 5 "agree strongly"
//Q:Important that people are treated equally and have equal opportunities//
recode  ipeqopt 7/9=.
replace  ipeqopt=7-ipeqopt
label values  ipeqopt  ipeqoptlbl
label define ipeqoptlbl 1 "not like me at all" 2 "not like me" 3 "a little like me" 4 "somewhat like me" 5 "like me" 6 "very much like me"

//Q: Most people try to take advantage of you or try to be fair//
recode pplfair 77/99=.


//explanatory variables//
//1) weighted mean preference for redistribution of natives in one's country of birth//
gen wgincdif=gincdif*dweight if brncntr==1 //natives only//
gen rtmpwgt=dweight if brncntr==1 & gincdif~=.   //tmpwgt =. for non natives and when there is no preference//

egen rmwgt=mean(rtmpwgt),         by(cntbrthc) //means by birthcountry//
egen s_ginc=mean(wgincdif/rmwgt), by(cntbrthc) //***into all obs by birthcountry***//



//2) weighted mean preference for equality of natives in one's country of birth//
gen wipeqopt=ipeqopt*dweight if brncntr==1 //natives only//
gen ftmpwgt=dweight if brncntr==1 & ipeqopt~=. //tmpwgt=. for non natives and when there is no preference//

egen fmwgt=mean(ftmpwgt), by (cntbrthc) //mean weight by birthcountry//
egen s_ipeqopt=mean(wipeqopt/fmwgt), by(cntbrthc) //into all obs by birthcountry//

//number of obs in each immigrant/birth country group//
egen nobsimm=sum(sample==2), by(cntbrthc)

//3)weighted mean preference for fairness of natives in one's country of birth//
gen wpplfair=pplfair*dweight if brncntr==1 //natives only//
gen fatmpwgt=dweight if brncntr==1 & pplfair~=.

egen famwgt=mean(fatmpwgt), by(cntbrthc)
egen s_pplfair=mean(wpplfair/famwgt), by(cntbrthc)

//1) mean preference for income redistribution in host country //

egen hrmwgt =mean(rtmpwgt),          by(cntry)
egen h_ginc =mean(wgincdif/hrmwgt), by(cntry)

assert h_ginc==s_ginc if brncntr==1  //for natives mean host preferences should equal mean source pref//

gen rselfhostdif=gincdif-h_ginc if sample==2 //immigrants only//
egen rselfhostdifm=mean(rselfhostdif) if sample==2, by(cntbrthc)



//2) mean preference for equality in host country//

egen hfmwgt=mean(ftmpwgt), by(cntry)
egen h_ipeqopt=mean(wipeqopt/hfmwgt), by(cntry)

assert h_ipeqopt==s_ipeqopt if brncntr==1 //for natives mean host pref should equal mean source pref//

gen fselfhostdif=ipeqopt-h_ipeqopt if sample==2 //immigrants only//
egen fselfhostdifm=mean(fselfhostdif) if sample==2, by(cntbrthc)

//3)mean preference for fairness in host country//
egen hfamwgt=mean(fatmpwgt), by(cntry)
egen h_pplfair=mean(wpplfair/hfamwgt), by(cntry)

assert h_pplfair==s_pplfair if brncntr==1 //for natives mean host pref should equal mean source pref//

gen faselfhostdif=pplfair-h_pplfair if sample==2 //immigrants only//
egen faselfhostdifm=mean(faselfhostdif) if sample==2, by(cntbrthc)


//**Mean pref. for redistribution by country of parental origin**// 
** For those whose parent is native born, the country in which the parent was born is the country of residence
replace mbrncntb=cntry if mbrncntb=="66"
replace fbrncntb=cntry if fbrncntb=="66"
** Create a dummy for mother coming from an ESS sample country and is an immigrant
gen m_eur=0
foreach cc of global countries{
 replace m_eur=1 if mbrncntb=="`cc'" & mbrncntb~=cntry
}

** Create a dummy for father coming from an ESS sample country
gen f_eur=0
foreach cc of global countries{
 replace f_eur=1 if fbrncntb=="`cc'" & fbrncntb~=cntry
}


gen sameimm= (mbrncntb==fbrncntb) //creating a dummy when both parents are from the same country//

** Create parsamp to sort R into three categories based on parental origin

** parsamp 1: both parents natives (and a native one self)
**         2: both parents immigrants from ESS countries (and a native oneself)
**         3: exactly one parent is an immigrant from an ESS country (and a native oneself)
**         .: all other cases

gen parsamp=.
replace parsamp=1 if mbrncntb==cntry&fbrncntb==cntry&sample==0
replace parsamp=2 if m_eur+f_eur==2               &sample==0
replace parsamp=3 if m_eur+f_eur==1               &sample==0

gen     pbrncntb=cntry   if parsamp==1
replace pbrncntb=mbrncntb if parsamp==2
replace pbrncntb=mbrncntb if parsamp==3 & m_eur==1
replace pbrncntb=fbrncntb if parsamp==3 & f_eur==1
label var pbrncntb "Country of birth of immigrant parent"


** Note, missings reflect a missing parental country of origin or both parents from non ESS countries

** Create temp for R whose parents are both natives and put s_ginc
** into temp (i.e. mean preference by source country, based on all naives including
** those for whom one or both parents are immigrants)
gen temp=s_ginc if sample==0 & parsamp==1

** Now create the mean by the mother's/father's country of birth
egen ms_ginc=mean(temp), by(mbrncntb)
egen fs_ginc=mean(temp), by(fbrncntb)
drop temp
** Only keep these variables for those for who the mother or father is an immigrant

** COMMENTED OUT ==> now it is the mean parental preference including native parents
 replace ms_ginc=. if m_eur~=1
replace fs_ginc=. if f_eur~=1

** Create variables to test whether the effect is stronger if both parents are ESS immigrants
** Average parents' home preferences for those R whose parents are immigrants
egen p_ginc=rowmean(ms_ginc fs_ginc)
gen bothimm=parsamp==2
gen bp_ginc=bothimm*p_ginc

***EQUALITY by mother's/father's source country
gen temp=s_ipeqopt if sample==0 & parsamp==1
egen ms_ipeqopt=mean(temp), by(mbrncntb)
egen fs_ipeqopt=mean(temp), by(fbrncntb)
drop temp
replace ms_ipeqopt=. if m_eur~=1
replace fs_ipeqopt=. if f_eur~=1


egen p_ipeqopt=rowmean(ms_ipeqopt fs_ipeqopt)

gen bp_ipeqopt=bothimm*p_ipeqopt

****FAIRNESS by mother's/father's source country

gen temp=s_pplfair if sample==0 & parsamp==1
egen ms_pplfair=mean(temp), by(mbrncntb)
egen fs_pplfair=mean(temp), by(fbrncntb)
drop temp
replace ms_pplfair=. if m_eur~=1
replace fs_pplfair=. if f_eur~=1


egen p_pplfair=rowmean(ms_pplfair fs_pplfair)

gen bp_pplfair=bothimm*p_pplfair









**//End of p_ginc//**
*************************




//political parties//
gen byte prtvtdde = prtvdde1 /*na dw poia metavliti einai gia germania*/
gen byte prtvtalt = prtvalt1
gen partyall=.

foreach c of global countries{
capture gen byte prtvt`c'=. //***//
capture gen byte prtvta`c'=.
capture gen byte prtvtb`c'=.
capture gen byte prtvtc`c'=.
capture gen byte prtvtd`c'=.
capture gen byte prtvte`c'=.

 recode prtvt`c'  66/99=.
 recode prtvta`c' 66/99=.
 recode prtvtb`c' 66/99=.
 recode prtvtc`c' 66/99=.
 recode prtvtd`c' 66/99=.
 recode prtvte`c' 66/99=. 
 

  forvalues p=1/99{
	   quietly replace partyall=`p' if prtvt`c'==`p'
	   quietly replace partyall=`p' if prtvta`c'==`p'
	   quietly replace partyall=`p' if prtvtb`c'==`p' 
	   quietly replace partyall=`p' if prtvtc`c'==`p'
	   quietly replace partyall=`p' if prtvtd`c'==`p'
	   quietly replace partyall=`p' if prtvte`c'==`p'
}
}
	//Values greater than 34 are all missing/blank votes://
//Checking that values over 34 can be dropped//
foreach c of global countries{
 tab prtvt`c' if partyall>34&cntry=="`c'"
 tab prtvta`c' if partyall>34&cntry=="`c'"
 tab prtvtb`c' if partyall>34&cntry=="`c'"
 tab prtvtc`c' if partyall>34&cntry=="`c'"
 tab prtvtd`c' if partyall>34&cntry=="`c'"
 tab prtvte`c' if partyall>34&cntry=="`c'"

 }
replace partyall=. if partyall>34

gen     otherparty=0

replace otherparty=1 if prtvtal==8
replace otherparty=1 if prtvtcbe==16
replace otherparty=1 if prtvtcbg==13
replace otherparty=1 if prtvtdch==20
replace otherparty=1 if prtvtacy==7
replace otherparty=1 if prtvtccz==8
replace otherparty=1 if prtvtdde==9 
replace otherparty=1 if prtvtcdk==10
replace otherparty=1 if prtvtdee==11
replace otherparty=1 if prtvtces==14
replace otherparty=1 if prtvtcfi==18

replace otherparty=1 if prtvtcfr==14
replace otherparty=1 if prtvtgb==7
replace otherparty=1 if prtvtdhu==55
replace otherparty=1 if prtvtaie==10
replace otherparty=1 if prtvtbil==17
replace otherparty=1 if prtvtais==99
replace otherparty=1 if prtvtbit==14
replace otherparty=1 if prtvtalt==44

replace otherparty=1 if prtvtenl==13
replace otherparty=1 if prtvtano==10
replace otherparty=1 if prtvtcpl==9
replace otherparty=1 if prtvtbpt==13
replace otherparty=1 if prtvtcru==99
replace otherparty=1 if prtvtbse==11
replace otherparty=1 if prtvtdsi==12
replace otherparty=1 if prtvtcsk==7
replace otherparty=1 if prtvtcua==8
replace otherparty=1 if prtvtxk==7


gen byte blankvote=0

replace blankvote=1  if prtvtcbe==17

replace blankvote=1 if prtvtdch==19
replace blankvote=1 if prtvtacy==7
replace blankvote=1 if prtvtces==15
replace blankvote=1 if prtvtcfr==15
replace blankvote=1 if prtvtbil==18
replace blankvote=1 if prtvtais==8
replace blankvote=1 if prtvtenl==14
replace blankvote=1 if prtvtbpt==12

replace partyall=. if otherparty==1|blankvote==1


**Create weighted values for party gincdif**
//weighted pref for redistribution using natives only//
assert gincdif<.
gen tpref=gincdif*dweight if brncntr==1&partyall<. //natives who voted only//
gen vtmpwgt=dweight       if brncntr==1&partyall<. //natives who voted only//
//means of the variables by country //

egen vmwgt=mean(vtmpwgt)      , by(cntry partyall)
egen v_ginc=mean(tpref/vmwgt), by(cntry partyall)

********************************************************************************************


**Create weighted values 
// Country gdp per capita ppp 2011 //
gen s_gdp_pp=.
replace s_gdp_pp=9640.1 if cntbrth=="al" 	
replace s_gdp_pp=40945.9 if cntbrth=="be" 
replace s_gdp_pp=15278.4	if cntbrth=="bg" 
replace s_gdp_pp=28603.5 if cntbrth=="cz" 
replace s_gdp_pp=32983.1 if cntbrth=="cy" 
replace s_gdp_pp=54550.7 if cntbrth=="ch" 
replace s_gdp_pp=42079.9	 if cntbrth=="de" 
replace s_gdp_pp=43314.1	 if cntbrth=="dk" 
replace s_gdp_pp=23575.7 if cntbrth=="ee" 
replace s_gdp_pp=32674.0 if cntbrth=="es" 
replace s_gdp_pp=40251.4	 if cntbrth=="fi" 
replace s_gdp_pp=37325.3 if cntbrth=="fr" 
replace s_gdp_pp=36549.4 if cntbrth=="gb" 
replace s_gdp_pp=22523.8 if cntbrth=="hu" 
replace s_gdp_pp=44912.7 if cntbrth=="ie" 
replace s_gdp_pp=30182.5 if cntbrth=="il" 
replace s_gdp_pp=39619.4 if cntbrth=="is" 
replace s_gdp_pp=35901.3 if cntbrth=="it" 
replace s_gdp_pp=22530.3 if cntbrth=="lt" 
replace s_gdp_pp=46388.3	 if cntbrth=="nl" 
replace s_gdp_pp=62736.7 if cntbrth=="no" 
replace s_gdp_pp=22333.5 if cntbrth=="pl" 
replace s_gdp_pp=26932.4 if cntbrth=="pt" 
replace s_gdp_pp=22569.8	 if cntbrth=="ru" 
replace s_gdp_pp=43709.2	 if cntbrth=="se" 
replace s_gdp_pp=28491.9	 if cntbrth=="si" 
replace s_gdp_pp=25066.1 if cntbrth=="sk"
replace s_gdp_pp=8281.9 if cntbrth=="ua" 
replace s_gdp_pp=8226.9 if cntbrth=="xk" 

** Also create GDP for parental source country
**
** create temp only for those whose parents are both natives
** put s_ginc into temp (i.e. the same variable as before, based on all natives
** including those for whom one or both parents were immigrants)
**
//Parental source country GDP per capita//
gen  temp1=s_gdp_pp if sample==0 & parsamp==1
egen ms_gdp_pp=mean(temp1), by(mbrncntb)
egen fs_gdp_pp=mean(temp1), by(fbrncntb)
drop temp1

** take the average of parents' home preferences for those parents who are immigrants
egen p_gdp_pp=rowmean(ms_gdp_pp fs_gdp_pp)

//log transform gdp//
gen s_lngdp_pp =ln(s_gdp_pp)
replace s_lngdp_pp=0 if sample~=2
assert  s_lngdp_pp<. if sample==2
** For parental value *_gdp is already missing when it should be
gen ms_lngdp_pp=ln(ms_gdp_pp)
gen fs_lngdp_pp=ln(fs_gdp_pp)
gen p_lngdp_pp =ln(p_gdp_pp)

//gini//
gen s_gini=.

replace s_gini=29 if cntbrth=="al"
replace s_gini=26.3 if cntbrth=="be"
replace s_gini=35.1 if cntbrth=="bg"
replace s_gini=29.1 if cntbrth=="cy"
replace s_gini=25.2 if cntbrth=="cz"
replace s_gini=29.7 if cntbrth=="ch"
replace s_gini=29 if cntbrth=="de"
replace s_gini=27.8 if cntbrth=="dk"
replace s_gini=31.9 if cntbrth=="ee"
replace s_gini=34 if cntbrth=="es"
replace s_gini=25.8 if cntbrth=="fi"
replace s_gini=30.8 if cntbrth=="fr"
replace s_gini=33 if cntbrth=="gb"
replace s_gini=26.8 if cntbrth=="hu"
replace s_gini=33.2 if cntbrth=="ie"
replace s_gini=37.6 if cntbrth=="il"
replace s_gini=23.6 if cntbrth=="is"
replace s_gini=31.9 if cntbrth=="it"
replace s_gini=32.9 if cntbrth=="lt"
replace s_gini=25.8 if cntbrth=="nl"
replace s_gini=22.9 if cntbrth=="no"
replace s_gini=31.1 if cntbrth=="pl"
replace s_gini=34.2 if cntbrth=="pt"
replace s_gini=35.4 if cntbrth=="ru"
replace s_gini=24.4 if cntbrth=="se"
replace s_gini=23.8 if cntbrth=="si"
replace s_gini=25.7 if cntbrth=="sk"
replace s_gini=24.6 if cntbrth=="ua"
replace s_gini=30 if cntbrth=="xk"

gen  temp1=s_gini if sample==0 & parsamp==1
egen ms_gini=mean(temp1), by(mbrncntb)
egen fs_gini=mean(temp1), by(fbrncntb)
drop temp1

** take the average of parents' home preferences for those parents who are immigrants
egen p_gini=rowmean(ms_gini fs_gini)

//log transform gdp//
gen s_lngini =ln(s_gini)
replace s_lngini=0 if sample~=2
assert  s_lngini<. if sample==2
** For parental value *_gdp is already missing when it should be
gen ms_lngini=ln(ms_gini)
gen fs_lngini=ln(fs_gini)
gen p_lngini =ln(p_gini)







*****************************//BaseLine Controls//*********************************************

** Gender
** -----
gen byte female=gndr
recode female 1=0 2=1 9=.

**currently working crpdwk/main activity
** Currently Working
** -----------------
recode crpdwk 2=0 6/9=.

replace pdwrk=crpdwk if pdwrk==0 /* if the card "paid work" was not marked, use the answer from the control card */
label val pdwrk noyes
**(ALTERNATIVE: Main Activity)

**partner currently working 
recode crpdwkp 2=0 6/9=.
replace pdwrkp=crpdwkp if pdwrkp==0 /* if card "paid work" was not marked, use answer from control card; variable missing for those w/o partner  */
label val pdwrkp noyes
rename pdwrkp p_pdwrk


**industry
**occupation

**father's employment status when respondent was 14
recode emprf14 7/9=.
gen byte  fe__empl=emprf14==1            if emprf14<.
gen byte  fe__self=emprf14==2            if emprf14<.
gen byte  fe__nwrk=emprf14==3            if emprf14<.
gen byte  fe__fabs=emprf14==4            if emprf14<.
gen byte  fe_mis  =missing(emprf14)

**mother's employment status when r was 14
recode emprm14 7/9=.
gen byte  me__empl=emprm14==1            if emprm14<.
gen byte  me__self=emprm14==2            if emprm14<.
gen byte  me__nwrk=emprm14==3            if emprm14<.
gen byte  me__mabs=emprm14==4            if emprm14<.
gen byte  me_mis  =missing(emprm14)

**father's occupation when r was 14
recode occf14b 66/99=.
**mother's occupation when r was 14
recode occm14b 66/99=.
**ever had paid job
recode pdjobev 2=0
replace pdjobev=1 if pdwrk==1
label val pdjobev noyes

***TPORGWK What type of organisation work/worked for
recode tporgwk 66/99=.
gen byte wrk_gov=tporgwk==1 if tporgwk<.
gen byte wrk_pub=tporgwk==2 if tporgwk<.
gen byte wrk_se=tporgwk==3 if tporgwk<.
gen byte wrk_priv=tporgwk==4 if tporgwk<.
gen byte wrk_self=tporgwk==5 if tporgwk<.
gen byte wrk_other=tporgwk==6 if tporgwk<.
 

**long term unemployment history
recode uemp3m  2=0
recode uemp12m 2=0
** Note uemp12m was not asked if uemp3m==0 because then uemp12m is logically zero
replace uemp12m=0 if uemp3m==0
label val uemp3m noyes
label val uemp12m noyes

**trade union member 
recode mbtru 7/9=.

**Ever a member
**	1=yes, 0=no
gen byte union_ev=0
replace union_ev=1 if mbtru==1|mbtru==2&mbtru~=.

**Currently a member
**	1=yes, 0=no
gen byte union_cur=0
replace union_cur=1 if mbtru==1&mbtru~=.





**main source of income
recode hincsrca 77/88=.
gen byte wages=hincsrca==1
gen byte selfemployed=hincsrca==2|hincsrca==3
gen byte pension=hincsrca==4
gen byte unempbenefits=hincsrca==5
gen byte socbenefits=hincsrca==6
gen byte investment=hincsrca==7
gen byte otherinc=hincsrca==8






**children in household
recode chldhm 9=.
recode chldhhe 6/9=.
recode chldhm  2=0
recode chldhhe 2=0
replace chldhhe=1 if chldhm==1
label val chldhm noyes
label val chldhhe noyes

**marital state
recode maritalb 77/99=.
gen byte married=maritalb==1|maritalb==2 if maritalb<.
gen byte separated=maritalb==3|maritalb==4 if maritalb<.
gen byte widowed=maritalb==5 if maritalb<.
gen byte nevmar=maritalb==6  if maritalb<.
gen byte mar_mis=missing(maritalb)

recode emplno 6666/9999=.
gen byte empl10=emplno>=10

recode jbspv 6/9=.
recode jbspv 2=0

recode njbspv 66666/99999=.



**living with partner/husband/wife

recode lvgptnea 7/9=. 2/6=0

**household size
recode hhmmb 77=. 88=. 99=.
gen lnhhsz=ln(hhmmb)

**educational attainment
recode edulvlb 5555/9999=. 
** Narrower Educational dummies
gen byte edn__prim0 =edulvlb==000 if edulvlb<.
gen byte edn__prim1 =edulvlb>=113 & edulvlb<=129 if edulvlb<.
gen byte edn__sec1  =edulvlb>=212 & edulvlb<=229 if edulvlb<.
gen byte edn__sec2  =edulvlb>=311 & edulvlb<=323 if edulvlb<.
gen byte edn__secpost  =edulvlb>=412 & edulvlb<=423 if edulvlb<.
gen byte edn__tershort  =edulvlb>=510 & edulvlb<=520 if edulvlb<.
gen byte edn__tert1  =edulvlb>=610 & edulvlb<=620 if edulvlb<.
gen byte edn__tert2  =edulvlb>=710 & edulvlb<=800 if edulvlb<.
gen byte edn_mis    =missing(edulvlb)

** Broader Educational Dummies
gen byte edb__lo  =edulvlb>=0 & edulvlb<=229 if edulvlb<.
gen byte edb__sec =edulvlb>=311 & edulvlb<=323  if edulvlb<.
gen byte edb__hi  =edulvlb>=412 & edulvlb<=800 if edulvlb<.
gen byte edb_mis  =missing(edulvlb)


**ALTERNATIVE EDUCATION MEASURE - Years of full-time education completed
**----------------------------------------------------------------------
recode eduyrs 77/99=.

**partner's education
recode edulvlpb 5555/9999=.

** Broader Educational Dummies
gen byte pedb__lo  =edulvlpb>=0 & edulvlpb<=229 if edulvlpb<.
gen byte pedb__sec =edulvlpb>=311 & edulvlpb<=323  if edulvlpb<.
gen byte pedb__hi  =edulvlpb>=412 & edulvlpb<=800 if edulvlpb<.
gen byte pedb_mis  =missing(edulvlpb)


**Educational Attainment - Father
**--------------------------------

recode edulvlfb 5555/9999=.
recode edulvlmb  5555/9999=.

** Broader Educational Dummies
gen byte fedb__lo  =edulvlfb>=0 & edulvlfb<=212 if edulvlfb<.
gen byte fedb__sec =edulvlfb==311 & edulvlfb<=323  if edulvlfb<.
gen byte fedb__hi  =edulvlfb>=412 & edulvlfb<=800 if edulvlfb<.
gen byte fedb_mis  =missing(edulvlfb)

**Educational Attainment - Mother
**--------------------------------

** Broader Educational Dummies
gen byte medb__lo  =edulvlmb>=0 & edulvlmb<=212 if edulvlmb<.
gen byte medb__sec =edulvlmb==311 & edulvlmb<=323  if edulvlmb<.
gen byte medb__hi  =edulvlmb>=412 & edulvlmb<=800 if edulvlmb<.
gen byte medb_mis  =missing(edulvlmb)

**Household income

recode hinctnta 77=. 88=. 99=.
gen byte hinc_lo =hinctnta<=5 if hinctnta<.
gen byte hinc_hi =hinctnta>5 if hinctnta<.
gen byte mis_hinc=missing(hinctnta) 

**income subjective feeling
recode hincfel 7/9=. 
replace hincfel=5-hincfel
label values hincfel hincfellbl
label define hincfellbl 1 "finding it very difficult on present income" 2 "finding it difficult on present income" 3 "coping on present income" 4 "living comfortable"

//More Controls//
**citizenship
recode ctzcntr 7/9=.
**tenure in country
recode livecnta 7777/9999=. 
replace livecnta=6666 if brncntr==1
gen byte ten20p = livecnta>=20  &livecnta<6666           if livecnta<.

gen byte ten_mis = missing(livecnta)
label define ten20p 0 "Less than 20 yrs" 1 "More than 20 yrs"
label values ten20p ten20p
label var ten20p "Tenure in country"

**whether voted
recode vote    2=0 3=0
recode vote 7/9=.
label values vote noyes

**region
capture gen metro=domicil
recode metro   2=1 3/5=0
label values metro metrolbl
label define metrolbl 0 "Non-metropolitan area" 1 "Big city/suburbs"


**linguistic minority
** Show all languages by by country if that languange is spoken by at least 1% of natives
capture drop temp
gen str6 cnt_lang=cntry + "_" + lnghom1
egen temp      = sum(dweight*(sample==0))     , by(cntry)

** frac_lang = weighted fraction of natives speaking that language in that country
egen frac_lang = sum(dweight*(sample==0)/temp), by(cnt_lang)
label var frac_lang "Weighted fraction of natives speaking this language in the country"




** Now create mechanical (strict) definitions of linguistic minority

 ** Cut-off = 50%
	capture drop temp
	gen temp=(frac_lang<0.50) if sample==0
	egen s50lingmin=mean(temp), by(cnt_lang)
	** temp is missing for languages not spoken by natives --> also linguistic minority
	recode s50lingmin .=1
	replace s50lingmin=. if lnghom1=="777"|lnghom1=="888"|lnghom1=="999"
	label var s50lingmin "Language at home spoken by less then 50% of natives at home"

 ** Cut-off = 30%
	drop temp
	gen temp=(frac_lang<0.30) if sample==0
	egen s30lingmin=mean(temp), by(cnt_lang)
	** temp is missing for languages not spoken by natives --> also linguistic minority
	recode s30lingmin .=1
	replace s30lingmin=. if lnghom1=="777"|lnghom1=="888"|lnghom1=="999"
	label var s30lingmin "Language at home spoken by less then 30% of natives at home"

 ** Cut-off = 10%
	drop temp
	gen temp=(frac_lang<0.10) if sample==0
	egen s10lingmin=mean(temp), by(cnt_lang)
	** temp is missing for languages not spoken by natives --> also linguistic minority
	recode s10lingmin .=1
	replace s10lingmin=. if lnghom1=="777"|lnghom1=="888"|lnghom1=="999"
	label var s10lingmin "Language at home spoken by less then 10% of natives at home"

drop temp



**immigrant density  the size of the birth country population by residence country
capture drop temp
capture drop temp2
egen temp=sum(dweight), by(cntry)
egen immdens=sum(dweight/temp), by(cntry cntbrth)

sum immdens if sample==0, d
sum immdens if sample==2, d
gen g50immdens = immdens>r(p50) if sample==2
label var g50immdens "Immigrant density greater than the median"

sum immdens if sample==1, d
drop temp
gen minus_immdens=-immdens
** take one obs per source country - host country pair
bysort cntry cntbrth: gen temp=_n>1


** Show the 10 highest immigrant countries by host country
sort temp cntry minus_immdens
by temp cntry: gen temp2=_n<=10
list cntry cntbrth immdens if temp2 & temp==0

** Show dominance of the natives (fraction native born)
sort temp minus_immdens
list cntry cntbrth immdens if cntry==cntbrth & temp==0

** Show the highest immigrant groups
list cntry cntbrth immdens if cntry~=cntbrth & _n<=100
drop minus_immdens

** Also for 2nd generation
capture drop temp
capture drop temp2
egen temp=sum(dweight), by(cntry)
egen pimmdens=sum(dweight/temp), by(cntry pbrncntb)

sum pimmdens if parsamp==2|parsamp==3, d
gen g50pimmdens = pimmdens>r(p50) if parsamp==2|parsamp==3
label var g50pimmdens "Parental Immigrant density greater than the median"
capture drop temp

***religiosity
capture drop temp
egen temp=sum(dweight) , by(cntry) 
egen s_religiosity=sum(rlgdgr/temp) , by(cntbrth)
sum s_religiosity if sample==2, d 
gen g50s_rel= s_religiosity>r(p50) if sample==2
capture drop temp

 


**religion how religious are you 
recode rlgdgr 77/99=.
gen byte rlg_hi=rlgdgr>=5 if rlgdgr<.
gen byte rlg_mis=missing(rlgdgr)
**Religious Attendance
recode rlgatnd 77/99=.
recode rlgdnm 66=. 77=. 99=.
replace rlgdnm=. if rlgblg==2
gen relmnth=rlgatnd
recode relmnth  5/7=0 1/4=1
label var relmnth "Attending Relig. service at least once a month"
label val relmnth noyes

gen byte rel_mis   = rlgblg~=2 & rlgdnm==.

gen byte rel__none = rlgblg==2                          if rel_mis==0
gen byte rel__cath =(rlgblg~=2) &  rlgdnm==1            if rel_mis==0
gen byte rel__prot =(rlgblg~=2) &  rlgdnm==2            if rel_mis==0
gen byte rel__orth =(rlgblg~=2) &  rlgdnm==3            if rel_mis==0
gen byte rel__ochr =(rlgblg~=2) &  rlgdnm==4            if rel_mis==0
gen byte rel__jew  =(rlgblg~=2) &  rlgdnm==5            if rel_mis==0
gen byte rel__islm =(rlgblg~=2) &  rlgdnm==6            if rel_mis==0
gen byte rel__othn =(rlgblg~=2) & (rlgdnm==7|rlgdnm==8) if rel_mis==0

** Country of origin was part of eastern block
** Country of origin was part of eastern block
** -------------------------------------------
gen byte m_ebloc=0 


replace m_ebloc=1 if m_eur==1&mbrncntb=="cz"
replace m_ebloc=1 if m_eur==1&mbrncntb=="si"
replace m_ebloc=1 if m_eur==12&mbrncntb=="pl"
replace m_ebloc=1 if m_eur==1&mbrncntb=="hu"
replace m_ebloc=1 if m_eur==1&mbrncntb=="ee"
replace m_ebloc=1 if m_eur==1&mbrncntb=="ua"
replace m_ebloc=1 if m_eur==1&mbrncntb=="sk"
replace m_ebloc=1 if m_eur==1&mbrncntb=="ru"
replace m_ebloc=1 if m_eur==1&mbrncntb=="bg"

replace m_ebloc=1 if m_eur==1&mbrncntb=="al"
replace m_ebloc=1 if m_eur==1&mbrncntb=="lt"
replace m_ebloc=1 if m_eur==1&mbrncntb=="xk"


gen byte s_ebloc=0
replace s_ebloc=1 if sample==2&cntbrthc=="cz"
replace s_ebloc=1 if sample==2&cntbrthc=="si"
replace s_ebloc=1 if sample==12&cntbrthc=="pl"
replace s_ebloc=1 if sample==2&cntbrthc=="hu"
replace s_ebloc=1 if sample==2&cntbrthc=="ee"
replace s_ebloc=1 if sample==2&cntbrthc=="ua"
replace s_ebloc=1 if sample==2&cntbrthc=="sk"
replace s_ebloc=1 if sample==2&cntbrthc=="ru"
replace s_ebloc=1 if sample==2&cntbrthc=="bg"

replace s_ebloc=1 if sample==2&cntbrthc=="al"
replace s_ebloc=1 if sample==2&cntbrthc=="lt"
replace s_ebloc=1 if sample==2&cntbrthc=="xk"

** Indicator for Migrating TO one of the EU15 countries

** Belonging to Ethnic Minority
recode blgetmg 7/9=.

gen byte ethm=blgetmg
recode ethm 2=0 3/max=.
label var ethm "Dummy for belonging to ethnic minority"
tab ethm essr, m

** Diad-level -- Creating mean ethnic minority score by residence country * birth country
egen gethm  = mean(ethm), by(cntry cntbrth)

** Country of residence level (including natives) -- measure of heterogeneity in receiving country
egen rethm = mean(ethm), by(cntry)

** create dummy variables for being above/below the median ethnic minority
sum gethm if sample==2, d
gen byte dgethm=gethm>r(p50) if sample==2
tab dgethm if sample==2, m

sum rethm if sample==2, d                         //above the median heterogeneity in receiving country//
gen byte drethm=rethm>r(p50) if sample==2
tab drethm if sample==2, m

** Note the dummy dFracNat = 1  if fraction of immigrants in resident nation is below median
** across all receiving countries in the ESS
assert native==0|native==1
egen fracnat=mean(native), by(cntry) /* fraction natives */
sum fracnat if sample==2,d
** Create dummy for greater than median fraction natives
gen dfracnat = fracnat>r(p50)



** Now create similar variable for 2nd generation individuals
** Diad-level -- Creating mean ethnic minority score by residence country * parent birth country
egen pgethm  = mean(ethm), by(cntry pbrncntb)

** create dummy variables for being above/below the median
sum pgethm if parsamp==2|parsamp==3, d
gen byte dpgethm=pgethm>r(p50) if parsamp==2|parsamp==3
tab dpgethm if parsamp==2|parsamp==3, m

sum rethm if parsamp==2|parsamp==3, d
gen byte dprethm=rethm>r(p50) if parsamp==2|parsamp==3
tab dprethm if parsamp==2|parsamp==3, m

sum fracnat if parsamp==2|parsamp==3,d
** dummy for greater than median fraction natives
gen dpfracnat = fracnat>r(p50)
tab dpfracnat if parsamp==2|parsamp==3, m

***pol ideology
recode lrscale 77/99=.
gen byte left=lrscale>=0 & lrscale<=4 if lrscale<.
gen byte centre=(lrscale==5) if lrscale<.
gen byte right=lrscale>=6 & lrscale<=10 if lrscale<.


************************************************************************************

//Creating summary statistics//
//table 1. immigration flows within ESS countries//

** Col 1: Source country flow (NOBS)
** Col 2: Going to how many distinct countries (out of 27)
** Col 3: Most prevalent destinatino country
** Col 4: NOBS going to most prevalent destination country
**
** Col 5: Destination country flow (NOBS)
** Col 6: Coming from how many distinct countries (out of 27)
** Col 7: Most prevalent source country
** Col 8: NOBS coming from most source destination country

** tagcc = use one obseravation for each Country of origin - host country Combination
egen tagcc=tag(cntry cntbrthc) if sample==2
** nobscc = no. obs. in each Country Combination
egen nobscc=total(1) if sample==2, by(cntry cntbrthc)
** Col 1 & 2: Nobs by source country  &  going # distinct destination countries
egen nrdest=total(tagcc) if sample==2, by(cntbrthc)
tab cntbrthc if sample==2, sum(nrdest)
** Col 3 & 4: Most prevalent destination country and # obs going there
gsort cntbrthc -nobscc
by cntbrthc: gen temp=_n==1 if sample==2
list cntbrthc cntry nobscc if temp==1
drop temp
** Spot check
tab cntry if cntbrthc=="be" & sample==2
** Col 5 & 6: Nobs by destination country  &  going # distinct source countries
egen nrsource=total(tagcc) if sample==2, by(cntry)
tab cntry if sample==2, sum(nrsource)
** Col 3 & 4: Most prevalent source country and # obs coming from there
gsort cntry -nobscc
by cntry: gen temp=_n==1 if sample==2
list cntry cntbrthc nobscc if temp==1
drop temp
** Double check that no immigrants come from a source country that is the country of residence
assert cntry~=cntbrthc if sample==2
** Total number of non-empty cells
egen nonempty=total(tagcc) if sample==2
sum nonempty
di "Out of the 31*30=" 31*30 " possible cells, " r(mean) " (=" 100*r(mean)/(31*30) "%) are non-empty/"
** Spot check
tab cntbrthc if cntry=="be" & sample==2

** These variables are no longer needed
drop nonempty tagcc nobscc nrdest nrsource
