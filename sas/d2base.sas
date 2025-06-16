/******************************************************************************* 
*              Study: FI-Lab                                                   *                                                                     
*            Program: base.sas                                                 *                                               
*           Function: to read in baseline variables                            *                                      
*             Author: Jinbei Wang                                              *
*     Date Completed: 2025/03/16                                               *                                             
*        SAS Version: SAS V9.4                                                 *                                                 
*           Platform: NT 4.0                                                   *                                           
*         Input Data: &rootpath.data\raw demo,comorbidities,scores,sepsis,     *
*                      ventalation                                             *                                
*     Program Output: ads.tte                                                  *   
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

*------------------------------------------------*;
*       Comorbidities                            *;
*------------------------------------------------*;
proc sort data=raw.Comorbidities out=cm;
     by subject_id hadm_id stay_id;
run;

data cm;
     merge cm(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id sepsis);
	 by  subject_id hadm_id stay_id;
	 if a and b;
	 **Hypertension**;
     cm1_htn=htn;
	 **COVID-19**;
	 cm2_covid=cov;
	 **Coronary Heart Disease**;
	 cm3_chd=ihd;
	 **Stroke**;
     cm4_Stroke=cva;
     ** type 2 diabetes**;
	 cm5_t2dm=t2dm;
	 **Congestive Heart Failure**;
     cm6_chf=hf;
	 **Malignant Tumor**;
	 cm7_cancer=ca;
	 **Schizophrenia**;
	 cm8_sch=Schizophrenia;
	 **Chronic Obstructive Pulmonary Disease**;
	 cm9_copd=copd;
	 **Chronic Kidney Disease**;
	 cm10_ckd=ckd;
	 **Hyperlipidemia**;
	 cm11_hld=hld;
	 **Sepsis**;
	 cm12_sepsis=sepsis;
	 if sepsis=. then cm12_sepsis=0;
	 keep subject_id hadm_id stay_id cm:;
run;

*------------------------------------------------*;
*       SU History                               *;
*------------------------------------------------*;
proc sort data=raw.smoke_alcohol out=smoke_alcohol;
     by subject_id hadm_id stay_id;
run;

data smoke_alcohol;
     merge smoke_alcohol(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id);
	 by subject_id hadm_id stay_id;
     if a and b;
	 **Alcohol Consumption**;
	 if first_etoh_value='1' then su1_Alcohol=1;
	 else su1_Alcohol=0;
	 **Smoke History**;
     if first_tobacco_use=1 or first_tobacco_use_history_value ="Current use or use within 1 month of admission"
     then  su2_smoke=1;
	 else  su2_smoke=0;
	 keep subject_id hadm_id stay_id su:;
run;

*------------------------------------------------*;
*       Surgery                                  *;
*------------------------------------------------*;
*------------------------------------------------*;
proc sort data=raw.cardiac_surgery out=cardiac_surgery;
     by subject_id hadm_id stay_id;
run;

data cardiac_surgery;
     merge cardiac_surgery(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id );
	 by subject_id hadm_id stay_id;
     if a and b;
	 array surgery (*)cardiac_surgery1-cardiac_surgery25;
	 do i=1 to dim(surgery);
         if surgery(i)=1 then cardiac_surgery=1;
	 end;
	 if cardiac_surgery=. then cardiac_surgery=0;
	 keep subject_id hadm_id stay_id cardiac_surgery;
run;

*------------------------------------------------*;
*       Ventilation                              *;
*------------------------------------------------*;
proc sort data=raw.Ventilation out=Ventilation;
     by subject_id hadm_id stay_id;
run;

data Ventilation;
     merge Ventilation(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id);
	 by subject_id hadm_id stay_id;
     if a and b;
     if ventilation=. then ventilation=0;
	 keep subject_id hadm_id stay_id ventilation;
run;


*------------------------------------------------*;
*       CRTT                                     *;
*------------------------------------------------*;
proc sort data=raw.kidney_crrt out=kidney_crrt;
     by subject_id hadm_id stay_id;
run;

data kidney_crrt;
     merge kidney_crrt(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id);
	 by subject_id hadm_id stay_id;
     if a and b;
     if crrt=. then crrt=0;
	 keep subject_id hadm_id stay_id crrt;
run;

*------------------------------------------------*;
*       Drugs                                    *;
* 2025/06/14: 1. retrict to ICU 24h              *;
*             2. add more drugs                  *;
*------------------------------------------------*;
proc sort data=raw.icu_drug_Vasopressor out=Vasopressor;
     by subject_id hadm_id stay_id;
run;

data Vasopressor;
     merge Vasopressor(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id icuindtm);
	 by subject_id hadm_id stay_id;
     if a and b;
     vpstdtm = vp_prescriptions_starttime * 86400;  
     vpendtm = vp_prescriptions_stoptime * 86400;
     st_h = (vpstdtm - icuindtm) / 3600;          
     en_h = (vpendtm - icuindtm) / 3600;
     
     **Used during ICU first 24h; 
     if not missing(st_h) and not missing(en_h) then do;
         if (.< st_h <= 24 & en_h >= 0) then Vasopressor=1;
         else Vasopressor=0;
     end;
     **Missing end date;
     else if missing(en_h) then do;
         if .< st_h <= 24 then Vasopressor=1; 
         else Vasopressor=0;
	 end;
	 format vpstdtm vpendtm datetime20.;
	 keep subject_id hadm_id stay_id Vasopressor;
run;

*----------------Added psychotropic_drugs-----------------------*;
** Including Benzodiazepines, Antipsychotics,Alpha-2 Agonists,  *;
*  Opioids, Tricyclic Antidepressants                           *;
*---------------------------------------------------------------*;
proc sort data=raw.psychotropic_drugs out=psychotropic_drugs;
     by subject_id hadm_id stay_id;
run;

%macro icu24hxx(st_h, en_h,outvar);
     **Used during ICU first 24h; 
     if not missing(&st_h.) and not missing(&en_h.) then do;
         if (.< &st_h. <= 24 & &en_h. >= 0) then &outvar.=1;
         else &outvar.=0;
     end;
     **Missing end date;
     else if missing(&en_h.) then do;
         if .< &st_h. <= 24 then &outvar.=1; 
         else &outvar.=0;
	 end;
%mend;


data psychotropic_drugs;
     merge psychotropic_drugs(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id icuindtm);
	 by subject_id hadm_id stay_id;
     if a and b;

	 **triantidepressants_use;
     trstdtm = triantidepressants_use_prescript * 86400;  
     trendtm = triantidepressants_use_prescrip1 * 86400;
     st_h1 = (trstdtm - icuindtm) / 3600;          
     en_h1 = (trendtm - icuindtm) / 3600;

	 **opioids_use, except fentany_use;
     opstdtm = opioids_use_prescriptions_startt * 86400;  
     opendtm = opioids_use_prescriptions_stopti * 86400;
     st_h2 = (opstdtm - icuindtm) / 3600;          
     en_h2 = (opendtm - icuindtm) / 3600;

	 **fentany_use;
     ftstdtm = fentany_use_prescriptions_startt * 86400;  
     ftendtm = fentany_use_prescriptions_stopti * 86400;
     st_h3 = (ftstdtm - icuindtm) / 3600;          
     en_h3 = (ftendtm - icuindtm) / 3600;

	 **dexmedetomidine_use;
     dexstdtm = dexmedetomidine_use_prescription * 86400;  
     dexendtm = dexmedetomidine_use_prescriptio1 * 86400;
     st_h4 = (dexstdtm - icuindtm) / 3600;          
     en_h4 = (dexendtm - icuindtm) / 3600;

	 **antipsychotics_use;
     psystdtm = antipsychotics_use_prescriptions * 86400;  
     psyendtm = antipsychotics_use_prescription1 * 86400;
     st_h5 = (psystdtm - icuindtm) / 3600;          
     en_h5 = (psyendtm - icuindtm) / 3600;

	 **benzodiazepines_use;
     benstdtm = benzodiazepines_use_prescription * 86400;  
     benendtm = benzodiazepines_use_prescriptio1 * 86400;
     st_h6 = (benstdtm - icuindtm) / 3600;          
     en_h6 = (benendtm - icuindtm) / 3600;

     %icu24hxx(st_h1, en_h1, triantidepressants_use);
     %icu24hxx(st_h2, en_h2, opioids_use);
     %icu24hxx(st_h3, en_h3, fentany_use);
     %icu24hxx(st_h4, en_h4, dexmedetomidine_use);
     %icu24hxx(st_h5, en_h5, antipsychotics_use);
     %icu24hxx(st_h6, en_h6, benzodiazepines_use);

	 format trstdtm trendtm opstdtm opendtm ftstdtm ftendtm dexstdtm dexendtm psystdtm psyendtm benstdtm benendtm datetime20.;
	 keep subject_id hadm_id stay_id triantidepressants_use opioids_use fentany_use 
          dexmedetomidine_use antipsychotics_use benzodiazepines_use;
run;

*------------------------------------------------*;
*       Scores                                   *;
*------------------------------------------------*;
proc sort data=raw.scores out=scores;
     by subject_id hadm_id stay_id;
run;

data scores;
     merge scores(in=a) ads.pop(in=b keep=subject_id hadm_id stay_id);
	 by subject_id hadm_id stay_id;
     if a and b;
	 keep subject_id hadm_id stay_id sofa apsiii sirs sapsii oasis gcs charlson pesi_score spesi_score shock_index pathos_score;
run;


*------------------------------------------------*;
*       Combine all Base Variables               *;
*------------------------------------------------*;

data final;
     merge cm smoke_alcohol cardiac_surgery Ventilation kidney_crrt scores
     Vasopressor psychotropic_drugs;
	 by subject_id hadm_id stay_id;
	 **Add labels;
run;

data ads.base;
     merge final(in=a) ads.pop(in=b );
	 by subject_id hadm_id stay_id;
	 if a and b;
run;

** ---------------------------------------------------------------------- **
** save lOG and LST/check log and generate report                         **
** ---------------------------------------------------------------------- **;
%let prognm=d2base;
DM 'log;    file "&rootpath.log\&prognm..log" replace';
DM 'OUTPUT; file "&rootpath.log\&prognm..lst" replace';
%logchk(logfile=&prognm);

********************************************************************************;
*                      End of the Program                                      *;
********************************************************************************;
