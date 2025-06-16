/******************************************************************************* 
*              Study: FI-Lab                                                   *                                                                     
*            Program: runall.sas                                               *                                               
*           Function: to run all programs                                      *                                      
*             Author: Jinbei Wang                                              *
*     Date Completed: 2025/03/25                                               *                                             
*        SAS Version: SAS V9.4                                                 *                                                 
*           Platform: NT 4.0                                                   *                                           
*         Input Data: &rootpath.data                                           *                                
*     Program Output: N/A                                                      *   
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

%inc "&rootpath.pgms\d1pop.sas";
%inc "&rootpath.pgms\d2base.sas";
%inc "&rootpath.pgms\d3filab.sas";
%inc "&rootpath.pgms\d4tte.sas";
