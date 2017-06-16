*******************************************************************;
**  C:\COUNTS\programs\build1.pgm                                  ;
**  (New setup to account for changes to 1999 Data)                ;
**  Build SAS file of Count data from Caltrans census data         ;
**  Year 1998-2017                                                      ;
**  -- April. 19, 2017 --                                            ;
*******************************************************************;
OPTIONS OBS=max MISSING='0';
*******************************************************************;
**Note: Change c:\counts\1990\count90.dat to;
**             c:\counts\1993\count93.dat;
**                for 1993 data analysis;
*******************************************************************;
FILENAME Cnt 'c:\counts\2016\count98_16.dat';
*******************************************************************;
** Note: Change DATA count.count90 to;
**                   count.count93 for 1993 Analysis;
*******************************************************************;
DATA tcount.count16; INFILE Cnt missover lrecl=260;
  INPUT ROUTE $ 1-4 ROUTESEQ 5-10 DISTRICTid 12-13
        COUNTY $ 14-16 POSTMILP $ 17 PM 18-23
        POSTMILS $ 24 LEG $ 25 DIR $ 26 ident $ 1-26
        ROADTYPE $ 27 LOCATION $ 28 STATION $ 33-35
        DESCRIP $ 36-65 YEAR 66-67 MONTH 68-69 DATE1 70-71
        @66 date yymmdd6. WEEKDAY $ 72-74 END1AM 75-80
        END1AMC $ 81 END2AM 82-87 END2AMC $ 88 END3AM 89-94
        END3AMC $ 95 END4AM 96-101 END4AMC $ 102 END5AM 103-108
        END5AMC $ 109 END6AM 110-115 END6AMC $ 116 END7AM 117-122
        END7AMC $ 123 END8AM 124-129 END8AMC $ 130 END9AM 131-136
        END9AMC $ 137 END10AM 138-143 END10AMC $ 144 END11AM 145-150
        END11AMC $ 151 END12NOO 152-157 END12NOC $ 158 END1PM 159-164
        END1PMC $ 165 END2PM 166-171 END2PMC $ 172 END3PM 173-178
        END3PMC $ 179 END4PM 180-185 END4PMC $ 186 END5PM 187-192
        END5PMC $ 193 END6PM 194-199 END6PMC $ 200 END7PM 201-206
        END7PMC $ 207 END8PM 208-213 END8PMC $ 214
        END9PM 215-220 END9PMC $ 221 END10PM 222-227 END10PMC $ 228
        END11PM 229-234 END11PMC $ 235 END12NIG 236-241 END12NIC $ 242
        TOTAL24 243-249 TOTAL24C $ 250 DAYTOT 251-257 DAYTOTC $ 258;
format date date7.;
proc export data=tcount.count16
   outfile='c:\counts\2016\16.txt' replace
   dbms=csv;
run;
