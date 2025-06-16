/******************************************************************************* 
*              Study: FI-Lab                                                   *                                                                     
*            Program: filab.sas                                                *                                               
*           Function: to read in lab+vs tests and calculate FI-LAB index       *                                      
*             Author: Jinbei Wang                                              *
*     Date Completed: 2025/02/28                                               *                                             
*        SAS Version: SAS V9.4                                                 *                                                 
*           Platform: NT 4.0                                                   *                                           
*         Input Data: &rootpath.data\raw, ICU 24h average values               *                                
*     Program Output: ads.filab                                                *   
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

**------------------------------------------------**;
**          Generate the list of test             **;
**------------------------------------------------**;
/*data list;*/
/*   set sashelp.vcolumn;*/
/*   where libname="RAW" and memname=:'ICU24H' and index(name,'24h');*/
/*run;*/

/*proc export data=list outfile="\\delta\dfs\UserData\Jinbei.Wang\Desktop\filab\data\ads\list of labtest.xlsx"*/
/*			dbms=xlsx replace;*/
/*quit;*/

**Not using CALL EXECUTE this time, use AI;
**------------------------------------------------**;
**            Create an empty dataset             **;
**------------------------------------------------**;
proc sql noprint;
   create table lab (
       subject_id num,
       stay_id num,
       hadm_id num,
       testcd char(20),
       test char(200),
       avalc char(200),
       avalu char(20)
   );
quit;

**------------------------------------------------**;
**         Read in each test one by one           **;
**------------------------------------------------**;

%macro read_test(dsin=, testcd=,test=,aval=,avalu=);
    data temp;
	     set raw.&dsin.;
		 length testcd  $20 test avalc $200 avalu $20;
		 where ^missing(&aval.);
		 testcd=%upcase("&testcd");
		 test="&test";
		 avalc=strip(&aval.);
		 avalu=&avalu.;
	     keep subject_id stay_id hadm_id testcd test avalc avalu;
    run;

	data lab;
	     set lab temp;
	run;
%mend read_test;

**-----------------------**;
**         Blood         **;
**-----------------------**;
**ALT;
%read_test(dsin=icu24h_alanine_transaminase, testcd=ALT,test=%str(Alanine transaminase (Units/L)),aval=lab_24hour_alanine_aminotransfer,avalu=lab_24hour_alanine_aminotransfe1);
**ALB;
%read_test(dsin=icu24h_albumin, testcd=ALB,test=%str(Albumin (g/dL)),aval=lab_24hour_albumin,avalu=lab_24hour_albumin_uom);
**ALP;
%read_test(dsin=ICU24H_ALKALINE_PHOSPHATASE, testcd=ALP,test=%str(Alkaline phosphatase (Units/L)),aval=lab_24hour_alkaline_phosphatase,avalu=lab_24hour_alkaline_phosphatase_);
**CA;
%read_test(dsin=ICU24H_CALCIUM, testcd=CA,test=%str(Calcium (mg/dL)),aval=lab_24hour_calcium_total,avalu=lab_24hour_calcium_total_uom);
**CREAT;
%read_test(dsin=ICU24H_CREATININE, testcd=CREAT,test=%str(Creatinine (mg/dL)),aval=lab_24hour_creatinine,avalu=lab_24hour_creatinine_uom);
**FIBRINO;
%read_test(dsin=ICU24H_FIBRINOGEN, testcd=FIBRINO,test=%str(Fibrinogen (mg/dL)),aval=lab_24hour_fibrinogen_functional,avalu=lab_24hour_fibrinogen_functiona1);
**GLUC;
%read_test(dsin=ICU24H_GLUCOSE, testcd=GLUC,test=%str(Glucose (mg/dL)),aval=lab_24hour_glucose,avalu=lab_24hour_glucose_uom);
**HGB;
%read_test(dsin=ICU24H_HGB, testcd=HGB,test=%str(Hemoglobin (g/dL)),aval=lab_24hour_hemoglobin,avalu=lab_24hour_hemoglobin_uom);
**INR;
%read_test(dsin=ICU24H_INR, testcd=INR,test=%str(International normalized ratio),aval=lab_24hour_inrpt,avalu=lab_24hour_inrpt_uom);
**LDH;
%read_test(dsin=ICU24H_LACTATE_DEHYDROGENASE, testcd=LDH,test=%str(Lactate dehydrogenase (Units/L)),aval=lab_24hour_lactate_dehydrogenase,avalu=lab_24hour_lactate_dehydrogenas1);
**PHOS;
%read_test(dsin=ICU24H_PHOSPHATE, testcd=PHOS,test=%str(Phosphorus (mg/dL)),aval=lab_24hour_phosphate,avalu=lab_24hour_phosphate_uom);
**PLAT;
%read_test(dsin=ICU24H_PLAT, testcd=PLAT,test=%str(Platelet count (×10^9/L)),aval=lab_24hour_platelet_count,avalu=lab_24hour_platelet_count_uom);
**K;
%read_test(dsin=ICU24H_POTASSIUM, testcd=K,test=%str(Potassium (mmol/L)),aval=lab_24hour_potassium,avalu=lab_24hour_potassium_uom);
**PT;
%read_test(dsin=ICU24H_PROTHROMBIN_TIME, testcd=PT,test=%str(Prothrombin time (s)),aval=lab_24hour_pt,avalu=lab_24hour_pt_uom);
**APTT;
%read_test(dsin=ICU24H_PTT, testcd=APTT,test=%str(APTT),aval=lab_24hour_ptt,avalu=lab_24hour_ptt_uom);
**SODIUM;
%read_test(dsin=ICU24H_SODIUM, testcd=SODIUM,test=%str(Sodium (mmol/L)),aval=lab_24hour_sodium,avalu=lab_24hour_sodium_uom);
**BILI;
%read_test(dsin=ICU24H_TOTAL_BILIRUBIN, testcd=BILI,test=%str(Total bilirubin (mg/dL)),aval=lab_24hour_bilirubin_total,avalu=lab_24hour_bilirubin_total_uom);
**TROPONT;
%read_test(dsin=ICU24H_TROPONIN, testcd=TROPONT,test=%str(Troponin T (ng/mL)),aval=lab_24hour_troponin_t,avalu=lab_24hour_troponin_t_uom);
**UREA;
%read_test(dsin=ICU24H_UREA_NITROGEN, testcd=UREA,test=%str(Urea nitrogen (mg/dL)),aval=lab_24hour_urea_nitrogen,avalu=lab_24hour_urea_nitrogen_uom);
**WBC;
%read_test(dsin=ICU24H_WBC, testcd=WBC,test=%str(White cell count (×10^3/µL)),aval=lab_24hour_white_blood_cells,avalu=lab_24hour_white_blood_cells_uom);

**-----------------------**;
**      Blood  Gas       **;
**-----------------------**;
**PCO2;
%read_test(dsin=ICU24H_PCO2, testcd=PCO2,test=%str(PCO2 (mm Hg)),aval=lab_24hour_pco2,avalu=lab_24hour_pco2_uom);
**LACTICAC;
%read_test(dsin=ICU24H_LACTATE, testcd=LACTICAC,test=%str(Lactate (mmol/L)),aval=lab_24hour_lactate,avalu=lab_24hour_lactate_uom);
**PH;
%read_test(dsin=ICU24H_PH, testcd=PH,test=%str(PH),aval=lab_24hour_ph,avalu=lab_24hour_ph_uom);
**PO2;
%read_test(dsin=ICU24H_PO2, testcd=PO2,test=%str(PO2 (mm Hg)),aval=lab_24hour_po2,avalu=lab_24hour_po2_uom);

**-----------------------**;
**         Urine         **;
**-----------------------**;

**UBILI;
%read_test(dsin=ICU24H_BILIRUBIN_URINE, testcd=UBILI,test=%str(Urine Bilirubin),aval=lab_24hour_bilirubin,avalu=lab_24hour_bilirubin_uom);
**URBC;
%read_test(dsin=ICU24H_ERYTHROCYTES_URINE, testcd=URBC,test=%str(Urine Erythrocytes),aval=lab_24hour_rbc,avalu=lab_24hour_rbc_uom);
**UGLUC;
%read_test(dsin=ICU24H_GLUCOSE_URINE, testcd=UGLUC,test=%str(Urine Glucose (mg/dL)),aval=lab_24hour_glucose,avalu=lab_24hour_glucose_uom);
**KETONES;
%read_test(dsin=ICU24H_KETONES_URINE, testcd=UKETONES,test=%str(Urine Ketones),aval=lab_24hour_ketone,avalu=lab_24hour_ketone_uom);
**UWBC;
%read_test(dsin=ICU24H_LEUCOCYTES_URINE, testcd=UWBC,test=%str(Urine Leucocytes),aval=lab_24hour_leukocytes,avalu=lab_24hour_leukocytes_uom);
**UPROT;
%read_test(dsin=ICU24H_PROTEIN_URINE, testcd=UPROT,test=%str(Urine Protein),aval=lab_24hour_protein,avalu=lab_24hour_protein_uom);

**-----------------------**;
**      Vital Signs      **;
**-----------------------**;
**DBP:Arterial Blood Pressure;
%read_test(dsin=ICU24H_DBP, testcd=DBP_A,test=%str(Arterial Diastolic Blood Pressure (mmHg)),aval=lab_24hour_firstabpd,avalu=lab_24hour_firstabpd_uom);
**DBP:Non-Invasive Blood Pressure;
%read_test(dsin=ICU24H_DBP, testcd=DBP_N,test=%str(Non-Invasive Diastolic Blood Pressure (mmHg)),aval=lab_24hour_firstnbpd,avalu=lab_24hour_firstnbpd_uom);
**HR;
%read_test(dsin=ICU24H_HR, testcd=HR,test=%str(Heart rate (bpm)),aval=lab_24hour_firsthr,avalu=lab_24hour_firsthr_uom);
**SBP:Arterial Blood Pressure;
%read_test(dsin=ICU24H_SBP, testcd=SBP_A,test=%str(Arterial Systolic Blood Pressure (mmHg)),aval=lab_24hour_firstabps,avalu=lab_24hour_firstabps_uom);
**SBP:Non-Invasive Blood Pressure;
%read_test(dsin=ICU24H_SBP, testcd=SBP_N,test=%str(Non-Invasive Systolic Blood Pressure (mmHg)),aval=lab_24hour_firstnbps,avalu=lab_24hour_firstnbps_uom);


**-----------------------------**;
**    Check Tests and Units    **;
**  33 in total,3 vs+ 30 lb    **;
**-----------------------------**;
proc freq data=lab noprint;
     tables testcd*test*avalu/list missing out=check_test;
run;

proc freq data=lab noprint;
     tables avalc/list missing out=check_aval;
run;

**-------------------------------------------------------------------------------**;
**      DBP and SBP need to imputed based on Non-Invasive Blood Pressure first,  **; 
**      then Arterial Blood Pressure A                                           **;
**-------------------------------------------------------------------------------**;
proc sort data=lab out=bp0;
     by subject_id stay_id hadm_id;
	 where testcd in ("DBP_A" "SBP_A" "DBP_N" "SBP_N");
run;

proc transpose data=bp0 out=bp0x(drop=_name_);
     by subject_id stay_id hadm_id;
	 id testcd;
	 var avalc;
run;

data bp_impute;
     set bp0x;
     if missing(DBP_N) then do;DBP_N=DBP_A;impute=1;end;
     if missing(SBP_N) then do;SBP_N=SBP_A;impute=1;end;
	 **Output testcd;
	 if ^missing(DBP_N) then do;
         testcd="DBP";
		 test="Diastolic blood pressure (mm Hg)";
		 avalc=DBP_N;
		 avalu="mm Hg";
		 output;
	 end;
	 if ^missing(SBP_N) then do;
         testcd="SBP";
		 test="Systolic blood pressure (mm Hg)";
		 avalc=SBP_N;
		 avalu="mm Hg";
		 output;
	 end;
	 drop DBP_N SBP_N DBP_A SBP_A;
run;

data lab1;
     set lab(where=(testcd ^in ("DBP_A" "SBP_A" "DBP_N" "SBP_N")))
	     bp_impute;
run;

proc sort data=lab1;
     by subject_id stay_id hadm_id;
run;

**------------------------------------------------------**;
**     Compare with Normal Range and assign deficts     **;
**------------------------------------------------------**;
**Read in normal ranges;
proc freq data=lab1 noprint;
     tables testcd*test*avalu*impute/list missing out=check_test1;
run;

proc import datafile="\\delta\dfs\UserData\Jinbei.Wang\Desktop\filab\data\ads\range.xlsx"
     out=range dbms=xlsx replace;
run;

proc sql noprint;
   create table lab2 as
   select a.*, b.low, b.high, b.sex
   from lab1 as a, range as b
   where a.testcd=b.testcd
   order by subject_id, stay_id, hadm_id;
quit;

data lab2;
     merge lab2(in=a) 
     ads.pop (in=b keep=subject_id stay_id hadm_id gender);
	 by subject_id stay_id hadm_id;
	 if a and b;
run;

data lab3;
     set lab2;
	 **Compare with range;
	 aval=input(avalc,best.);
	 lown=input(low,??best.);
	 highn=input(high,??best.);
	 if sex=1 then do;
	    if gender='F' then lown=input(strip(scan(scan(low,1,';'),2,':')),??best.);
		if gender='F' then highn=input(strip(scan(scan(high,1,';'),2,':')),??best.);
	    if gender='M' then lown=input(strip(scan(scan(low,2,';'),2,':')),??best.);
		if gender='M' then highn=input(strip(scan(scan(high,2,';'),2,':')),??best.);
	 end;

	 if ^index(test,'Urine') then do;
        if .<round(aval,1e-8)<lown or round(aval,1e-8)>highn>. then deficit=1;
		else deficit=0;
	 end;
	 else do;
        if round(aval,1e-8)>0 then deficit=1;
		else deficit=0;
	 end;
run;

**Calculate the total non-missing tests, total deficits;
proc sql noprint;
     create table lab4 as 
     select subject_id, stay_id, hadm_id,sum(deficit) as total_deficit, count(*) as total_number
/*,*/
/*            (calculated total_deficit / calculated total_number) as flab,*/
/*            (calculated total_deficit*100 / calculated total_number) as flab100*/
     from lab3
     group by subject_id, stay_id, hadm_id;
/*	 having total_number >= 25;*/
quit;

**-------------------------------**;
**      Missing data to see      **;
**-------------------------------**;
data check_miss;
     set lab4;
	 by subject_id stay_id hadm_id;
	 if last.hadm_id;
run;

proc freq data=check_miss;
     tables total_number/list missing;
run;

proc freq data=lab3;
     tables test/list missing out=count;
run;


**Missing test>20%;
data check_misstest;
     set count;
	 if count<7078*0.8;
run;


**-------------------------------**;
**      Missing data to see      **;
**-------------------------------**;

data lab5;
     set lab4;
	 by subject_id stay_id hadm_id;
	 if total_number>=25 then keep=1;
	 else keep=0;
	 flab=total_deficit/total_number;
	 flab100=total_deficit*100/total_number;
	 if last.hadm_id;
	 if keep=1;
run;


**-------------------------------**;
**     Output Final Dataset      **;
**-------------------------------**;

data lab6;
   merge lab5(in=a) ads.base(in=b) 
         raw.prog_28d(keep=subject_id stay_id hadm_id death_within_icu_28days is_icu_dead is_hosp_dead is_dead)
         raw.prog_90d(keep=subject_id stay_id hadm_id death_within_icu_90days)
         raw.prog_180d(keep=subject_id stay_id hadm_id death_within_icu_180days)
         raw.prog_360d(keep=subject_id stay_id hadm_id death_within_icu_360days )
   ;
   by subject_id stay_id hadm_id;
   if a and b;
   if is_dead=. then is_dead=0;
run;

*------------------------------------------------*;
*       Quartiles                                *;
*------------------------------------------------*;
proc univariate data=lab6 noprint;
     var flab;
     output out=quartiles pctlpts=25 50 75 pctlpre=flab_;
run;

data _null_;
     set quartiles;
     call symput('Q1', flab_25);
     call symput('Q2', flab_50);
     call symput('Q3', flab_75);
run;

data fi_lab;
     set lab6;
     length level $2;
     if not missing(flab) then do;
	     if      flab <= &Q1 then level = 'Q1';
	     else if flab <= &Q2 then level = 'Q2';
	     else if flab <= &Q3 then level = 'Q3';
	     else                     level = 'Q4';
     end;
     else level = '';
     leveln=input(compress(level,,'kd'),best.);
	 Q1=input("&Q1",best.);
	 Q2=input("&Q2",best.);;
	 Q3=input("&Q3",best.);;
run;

proc freq data=fi_lab;
     tables level/list missing;
run;

data ads.fi_lab;
     set fi_lab;
/*	 array num(*) _numeric_;*/
/*	 do i=1 to dim(num(i));*/
/*        if num(i)=0 then call missing(num(i));*/
/*	 end;*/
	 label gender="Sex" racegr1="Race Group";
run;


** ---------------------------------------------------------------------- **
** save lOG and LST/check log and generate report                         **
** ---------------------------------------------------------------------- **;
%let prognm=d3filab;
DM 'log;    file "&rootpath.log\&prognm..log" replace';
DM 'OUTPUT; file "&rootpath.log\&prognm..lst" replace';
%logchk(logfile=&prognm);

********************************************************************************;
*                      End of the Program                                      *;
********************************************************************************;
