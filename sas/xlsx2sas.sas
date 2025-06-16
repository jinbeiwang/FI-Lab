/******************************************************************************* 
*              Study: FI-Lab                                                   *                                                                     
*            Program: xlsx2sas.sas                                             *                                               
*           Function: to convert MImic-iv raw data in excel to sas datset      *                                      
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

***get file names under folder;
ods noresults;
ods graphics off;
filename xlsx pipe "dir &xlsxdir.\*.xlsx  /b";

data xlsxlist; 
   length file $ 200; 
   infile xlsx pad missover;
   input file $ 1-200;
run;

data _null_;
   set xlsxlist;
   xlsxfile="&xlsxdir.\"||strip(file);
   dsout=strip(scan(file,1,'.'));
   call execute ("proc import datafile='"||strip(xlsxfile)||"' out=raw."||strip(dsout)||" dbms=xlsx replace;");
   call execute ("getnames=yes;");
   call execute ("run;quit;");
run;


/***Add updated file;*/
/*proc import datafile="&xlsxdir.\Comorbidities.xlsx" out=raw.Comorbidities dbms=xlsx*/
/*  replace;*/
/*quit;*/
/**/
/*proc import datafile="&xlsxdir.\cardiac_surgery.xlsx" out=raw.cardiac_surgery dbms=xlsx*/
/*  replace;*/
/*quit;*/
/**/
/*proc import datafile="&xlsxdir.\smoke_alcohol.xlsx" out=raw.smoke_alcohol dbms=xlsx*/
/*  replace;*/
/*quit;*/
/**/
/*proc import datafile="&xlsxdir.\cam_icu.xlsx" out=raw.cam_icu dbms=xlsx*/
/*  replace;*/
/*quit;*/
/**/
/*proc import datafile="&xlsxdir.\bmi_hosp.tsv"*/
/*    out=raw.bmi_hosp*/
/*    dbms=tab*/
/*    replace;*/
/*    getnames=yes; /* use yes if the first row contains variable names */*/
/*run;*/
;
/*proc import datafile="&xlsxdir.\Psychotropic_Drugs.xlsx"*/
/*    out=raw.Psychotropic_Drugs*/
/*    dbms=xlsx*/
/*    replace;*/
/*    getnames=yes;*/
/*run;*/
