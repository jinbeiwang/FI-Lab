/******************************************************************************* 
*              Study: FI-Lab                                                   *                                                                     
*            Program: tte.sas                                                 *                                               
*           Function: to read in prognostic variables                          *                                      
*             Author: Jinbei Wang                                              *
*     Date Completed: 2025/03/16                                               *                                             
*        SAS Version: SAS V9.4                                                 *                                                 
*           Platform: NT 4.0                                                   *                                           
*         Input Data: &rootpath.data\raw, prog_28d, 90d, 180d, 360d            *                                
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

/*proc freq data=raw.prog_28d;*/
/*   tables death_within_icu_28days*icu_survival_time/list missing;*/
/*run;*/
*------------------------------------------------*;
*     Notes: cnsr=1 event, cnsr=0 censors        *;
*------------------------------------------------*;
%macro prog_xxd(inlib=raw,inds=,xxd=);
    data &inds.;
         length testcd $20  test $200;
	     set &inlib..&inds.;
	     if dead_time>. then dthdtm=dead_time*86400;
	     if icu_dod>.   then dthdtm_fu=icu_dod*86400;
	     testcd="ICU&xxd.D";
	     test="ICU &xxd. Day Survival";
	     cnsr=death_within_icu_&xxd.days;
	     **cnsr=1 event, cnsr=0 censors;
	     if icu_survival_time>&xxd. or icu_survival_time=. then 
	 	 aval=&xxd.;
		 else aval=icu_survival_time;
		 output;
	     testcd="HOSP&xxd.D";
		 test="Hospital &xxd. Day Survival";
		 cnsr=death_within_hosp_&xxd.days;
		 **cnsr=1 event, cnsr=0 censors;
		 if hosp_survival_time>&xxd. or hosp_survival_time=. then 
		 aval=&xxd.;
		 else aval=hosp_survival_time;
		 output;
	     format dthdtm  dthdtm_fu datetime20.;
		 keep subject_id stay_id hadm_id cnsr test: aval dthdtm dthdtm_fu;
	run;
	proc sort data=&inds.;
	     by subject_id stay_id hadm_id;
	run;

	data &inds.;
	     merge &inds.(in=a) ads.fi_lab (in=b);
		 by subject_id stay_id hadm_id;
		 if a and b;
	run;
%mend prog_xxd;

*------------------------------------------------*;
*       28-day mortality                         *;
*------------------------------------------------*;
%prog_xxd(inds=prog_28d,xxd=28);

*------------------------------------------------*;
*       90-day mortality                         *;
*------------------------------------------------*;
%prog_xxd(inds=prog_90d,xxd=90);

*------------------------------------------------*;
*       180-day mortality                        *;
*------------------------------------------------*;
%prog_xxd(inds=prog_180d,xxd=180);

*------------------------------------------------*;
*       360-day mortality                        *;
*------------------------------------------------*;
%prog_xxd(inds=prog_360d,xxd=360);

*------------------------------------------------*;
*      2025/06 Add In hospital mortality         *;
*------------------------------------------------*;
data prog_hosp;
     length testcd $20  test $200;
	 set raw.prog_28d;
	 if dead_time>. then dthdtm=dead_time*86400;
	 if icu_dod>.   then dthdtm_fu=icu_dod*86400;
	 testcd="HOSPXXD";
	 test="In hospital mortality";
	 cnsr=is_hosp_dead;

	 **cnsr=1 event: died during hospital, cnsr=0 censors;
	 if cnsr=0 then aval=hosp_day;**censor to hospital days;
     else aval=dead_time-admit_time+1;
	 format dthdtm  dthdtm_fu datetime20.;
     keep subject_id stay_id hadm_id cnsr test: aval dthdtm dthdtm_fu;
run;

proc sort data=prog_hosp;
	 by subject_id stay_id hadm_id;
run;

data prog_hosp;
	 merge prog_hosp(in=a) ads.fi_lab (in=b);
     by subject_id stay_id hadm_id;
	 if a and b;
run;

*------------------------------------------------*;
*      2025/06 Add ICU mortality         *;
*------------------------------------------------*;
data prog_icu;
     length testcd $20  test $200;
	 set raw.prog_28d;
	 if dead_time>. then dthdtm=dead_time*86400;
	 if icu_dod>.   then dthdtm_fu=icu_dod*86400;
	 testcd="ICUXXD";
	 test="ICU mortality";
	 cnsr=is_icu_dead;

	 **cnsr=1 event: died during ICU, cnsr=0 censors;
	 if cnsr=0 then aval=icu_day;**censor to ICU days;
     else aval=dead_time-icu_intime+1;
	 format dthdtm  dthdtm_fu datetime20.;
     keep subject_id stay_id hadm_id cnsr test: aval dthdtm dthdtm_fu;
run;

proc sort data=prog_icu;
	 by subject_id stay_id hadm_id;
run;

data prog_icu;
	 merge prog_icu(in=a) ads.fi_lab (in=b);
     by subject_id stay_id hadm_id;
	 if a and b;
run;

*------------------------------------------------*;
*       Combine all mortality                    *;
*------------------------------------------------*;
data final;
     set prog_:;
run;

proc sort data=final;
     by subject_id stay_id hadm_id;
run;

data ads.tte;
     set final;
run;

** ---------------------------------------------------------------------- **
** save lOG and LST/check log and generate report                         **
** ---------------------------------------------------------------------- **;
%let prognm=d4tte;
DM 'log;    file "&rootpath.log\&prognm..log" replace';
DM 'OUTPUT; file "&rootpath.log\&prognm..lst" replace';
%logchk(logfile=&prognm);

********************************************************************************;
*                      End of the Program                                      *;
********************************************************************************;
