*******************************************************************;
**  C:\COUNTS\PROGRAMS\build4.pgm                                  ;
**  PROC FREQ from SAS File for Tue., Wed., & Thu.                 ;
**  Year 2015                                                      ;
**  -- Dec. 23, 2015 --                                            ;
*******************************************************************;
**OPTIONS OBS=200;
OPTIONS NODATE;
OPTIONS number;
OPTIONS PAGESIZE=52;
LIBNAME tcount 'c:\counts\tcount.sas';  
*******************************************************************;
*Change COUNT92 to COUNT93 for 1993 Analysis;
*******************************************************************;
DATA tcount.OUT3IN; SET tcount.count15;
*******************************************************************;
 IF END7AM=0 THEN END7AM=.;
 IF END8AM=0 THEN END8AM=.;
 IF END9AM=0 THEN END9AM=.;
 IF TOTAL24=0 THEN TOTAL24=.;
*******************************************************************;
IF LEG='A' OR LEG='B' OR LEG='O' OR LEG='X' THEN DO;
  IF TOTAL24C='A' THEN DO;
     IF WEEKDAY='TUE' OR WEEKDAY='WED' OR WEEKDAY='THU' 
     THEN DO; OUTPUT tcount.OUT3IN; END;
  END;
END;
PROC FREQ DATA=tcount.out3in;
TABLES TOTAL24C YEAR MONTH WEEKDAY ROUTE COUNTY ROADTYPE DIR  
 LEG LOCATION STATION/missing list;
 TITLE1 'Frequency Distribution';
 TITLE2 '2015 Observed Traffic Counts-State Highways-Bay Area';
 TITLE3 'Count Days: Tuesdays, Wednesdays and Thursdays';
RUN;
PROC FREQ DATA=tcount.OUT3IN;
 Tables route*county/missing;
 TITLE1 'Table 1';
 TITLE2 'San Francisco Bay Area State Highway Routes';
 TITLE3 'Traffic Counts for Fiscal Year 2015';
run;
*******************************************************************;


