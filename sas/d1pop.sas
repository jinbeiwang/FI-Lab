/******************************************************************************* 
*              Study: FI-Lab                                                   *                                                                     
*            Program: d1pop.sas                                                *                                               
*           Function: create the main population dataset                       *                                      
*             Author: Jinbei Wang                                              *
*     Date Completed: 2025/01/20                                               *                                             
*        SAS Version: SAS V9.4                                                 *                                                 
*           Platform: NT 4.0                                                   *                                           
*         Input Data: All excels under &rootpath.data\raw\xlsx                 *                                
*     Program Output:                                                          *   
*******************************************************************************/*
*     Modif. History:                                                          *
*     Date          Author         Description of the change                   *
*                                                                              *   
********************************************************************************;
*                      Begining of the Program                                 *;
********************************************************************************;
** clean LOG and OUTPUT **;
DM 'OUTPUT; CLEAR; LOG; CLEAR;';

** automatically finding the study folders **;
%macro setpath;
  %global dir PROGNM drive rootpath patht;
    PROC SQL NOPRINT;
        SELECT XPATH INTO : PATHT
        FROM (SELECT * 
              FROM    DICTIONARY.EXTFILES
              WHERE SUBSTR(FILEREF,1,1)="#" AND  SUBSTR(FILEREF,4) IN
                 (SELECT MAX(T2) AS MAXX
                  FROM (SELECT XPATH ,FILEREF , SUBSTR(FILEREF,1,1) AS T1 , (SUBSTR(FILEREF,4)) AS T2
                        FROM  DICTIONARY.EXTFILES
                         WHERE SUBSTR(FILEREF,1,1)="#")));
    QUIT;
     %let dir = %substr(&patht,1,%length(&patht) - %length(%scan(&patht,-1,\)));
	 %let rootpath= %substr(&dir,1,%length(&dir) - %length(%scan(&dir,-1,\))-1);
     %LET PROGNM= %scan( %scan(&patht,-1,\), 1,.);
	 %let drive=%scan(%scan(&patht,1,\), 1, :);
%mend;
%setpath; 

** ---------------------------------------------------------------------- **
** startup, formats, macros                                               **
** ---------------------------------------------------------------------- *;
%inc "&rootpath.macro\setup.sas";

** delete all temp datasets **;
proc datasets lib=work mt=data kill nolist nowarn;
quit;

**Delirium Derived from CAM-ICU Components;
data cam_icu;
    set raw.cam_icu;
	length testcd $20;
	dedtm=charttime*86400;
	icuindtm=icu_intime*86400;
    icuoutdtm=icu_outtime*86400;
	if itemid in (228300 228337 229326) then testcd="Ms_Change";
	else if itemid in (228301 228336 229325) then testcd="Inattention";
	else if itemid in (228302 228334 ) then testcd="Rass_Alter";
	else if itemid in (228303 228335 229324 ) then testcd="Dis_think";
	format dedtm icuindtm icuoutdtm datetime20.;
run;

proc sort data=cam_icu;
    by subject_id hadm_id stay_id dedtm icuindtm icuoutdtm first_careunit last_careunit;
run;

proc transpose data=cam_icu out=cam_icu1;
    by subject_id hadm_id stay_id dedtm icuindtm icuoutdtm first_careunit last_careunit;
	var value;
	id testcd;
run;

data cam_icu1;
    set cam_icu1;
	**Feature 1+2 and Feature 3 or 4;
	if Ms_Change=:'Yes' and (Inattention=:'Yes'|Inattention in ("Language Barrier" "Preexisting Advanced Dementia"))
	and (Rass_Alter="Yes" or Dis_think="Yes")
	then value="Positive";
	else value="Negative";
run;

data delirium_impute;
    set cam_icu1;
	where value="Positive";
	first_dur=(dedtm-icuindtm)/3600;
	**Delirium within ICU and occured after ICU 24h;
    if icuindtm<=dedtm<=icuoutdtm and first_dur>=24;
run;

**Delirium Patients: Raw is ICU each test;
data delirium0;
   set raw.Delirium_icu_each;
   where value="Positive";
   dedtm=charttime*86400;
   icuindtm=icu_intime*86400;
   icuoutdtm=icu_outtime*86400;
   first_dur=(dedtm-icuindtm)/3600;
   **Delirium within ICU and occured after ICU 24h;
   if icuindtm<=dedtm<=icuoutdtm and first_dur>=24;
   format dedtm icuindtm icuoutdtm datetime20.;
run;

data delirium;
   set delirium0 delirium_impute;
run;

proc sort data=delirium;
   by subject_id hadm_id stay_id dedtm;
run;

**9937 Positive Delirium;
data delirium_subj;
   set delirium;
   by subject_id hadm_id stay_id dedtm;
   if first.stay_id;
   keep subject_id hadm_id stay_id dedtm first_dur icuindtm icuoutdtm;
run;

data _null_;
   set delirium_subj;
   by subject_id;
   if ^(first.subject_id and last.subject_id) then abort;
run;

**AKI data;
proc freq data=raw.aki_data;
   tables aki*aki_stage/list missing;
run;

data aki;
   set raw.aki_data;
   where aki=1;
   akidtm=aki_first_time*86400;
   format akidtm datetime20.;
   keep subject_id hadm_id stay_id akidtm aki aki_stage;
run;


**Sepsis data;
proc freq data=raw.sepsis_data;
   tables sepsis3/list missing;
run;

data sepsis;
   set raw.sepsis_data;
   where sepsis3='t';
   sepsis=1;
   sepsisdtm=suspected_infection_time;
   format sepsisdtm datetime20.;
   keep subject_id hadm_id stay_id sepsisdtm sepsis;
run;

**Keep demo data;
**7180 patients AKI-Delirium;
data pop;
   merge delirium_subj(in=a) raw.demo aki sepsis 
         raw.prog_28d(keep=subject_id hadm_id stay_id death_within_icu_28days)
		 raw.prog_90d(keep=subject_id hadm_id stay_id death_within_icu_90days)
		 raw.prog_180d(keep=subject_id hadm_id stay_id death_within_icu_180days)
		 raw.prog_360d(keep=subject_id hadm_id stay_id death_within_icu_360days)
    ;
   by subject_id hadm_id stay_id;
   if a and aki=1;
   if dedtm>=akidtm;
   admitdtm=admittime*86400;
   dischdtm=dischtime*86400;
   format admitdtm dischdtm  datetime20.;
   drop admittime dischtime icu_intime icu_outtime;
run; 



**Check duplicates;
data _null_;
   set pop;
   by subject_id hadm_id stay_id;
   if ^(first.subject_id and last.subject_id) then abort;
run;

proc freq data=pop;
   tables aki*aki_stage/list missing;
run;

data pop1;
   set pop;
   length racegr1 agegr1 $200;
   **Race Group;
   if race =:"WHITE" then do;racegr1="White";racegr1n=1;end;
   else if race ^=:"" then do;racegr1="Non-white";racegr1n=2;end;
/*   else if race =:"BLACK" then do;racegr1="Black or African American";racegr1n=2;end;*/
/*   else if race =:"HISPANIC" or race="SOUTH AMERICAN" then do;racegr1="Hispanic";racegr1n=3;end;*/
/*   else if race in ("OTHER" "PORTUGUESE"  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" "MULTIPLE RACE/ETHNICITY"*/
/*                   "AMERICAN INDIAN/ALASKA NATIVE") then do;*/
/*    racegr1="Other";racegr1n=5;end;*/
/*   else do;*/
/*    racegr1="Unknown";racegr1n=6;*/
/*   end;*/
   **Age Group;
   if 18<=age<=65  then do;agegr1="18 -< 65";agegr1n=1;end;
   else if 65<=age then do;agegr1=">= 65";agegr1n=2;end;
   **BMI;
   bmi=weight/(height/100)**2;
   if gender="F" then sexn=1;
   else if gender="M" then sexn=2;;

   if sepsis=. then sepsis=0;
run;

**BMI with missing >20%, now impute from hosp.omr data, multiple calculated as mean;
data pop2;
   merge pop1(in=a) raw.bmi_hosp;
   by subject_id;
   if a;
   if missing(bmi) then bmi=bmi_result_value;
   drop bmi_result_value;
run;

data a1;
    set ads.pop;
	if .<bmi<15;
run;

**Still has 10% missing;
proc freq data=pop2;
   tables bmi/missing;
run;

/* Step 1: Check outliers  */
proc univariate data=pop2 noprint;
     var bmi;
     output out=stats q1=q1 q3=q3 median=median;
run;

data _null_;
     set stats;
     call symput('q1', q1);
     call symput('q3', q3);
     call symput('median', median);
run;

%let iqr = %sysevalf(&q3 - &q1);
%let lower = %sysevalf(&q1 - 1.5*&iqr);
%let upper = %sysevalf(&q3 + 1.5*&iqr);

/* Step 2: cutoff at +-1.5iqr */
data pop3;
     set pop2;
     if bmi < &lower then bmi = .;
     if bmi > &upper then bmi = .;
run;

proc freq data=pop3;
   tables bmi/missing;
run;

proc means data=pop3;
   var bmi;
run;

data ads.pop;
    set pop3;
run;


proc freq data=ads.pop;
   tables first_careunit*last_careunit/list;
run;

** ---------------------------------------------------------------------- **
** save lOG and LST/check log and generate report                         **
** ---------------------------------------------------------------------- **;
%let prognm=d1pop;
DM 'log;    file "&rootpath.log\&prognm..log" replace';
DM 'OUTPUT; file "&rootpath.log\&prognm..lst" replace';
%logchk(logfile=&prognm);

********************************************************************************;
*                      End of the Program                                      *;
********************************************************************************;
