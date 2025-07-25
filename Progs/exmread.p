
/* Program.................exmread.p
   Notes:................. Export Meter Readings
   Author:.................S. Chisoro
   
*/
session:DATA-ENTRY-RETURN = TRUE.

DEF  VAR wsStatus   AS CHAR FORM "X(20)".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-OK  LABEL "PROCESS".
DEF VAR wsFile as CHAR.
DEF VAR wsFile1 AS CHAR FORM "X(20)".
DEF VAR wsFile2 AS CHAR FORM "X(20)".
DEF VAR Acc LIKE dbmmf.dbacc.
DEF VAR Meter LIKE dbmmf.Meter.
 DEF VAR Lat LIKE dbmmf.lat.
 DEF VAR Lon LIKE dbmmf.lon.
 DEF VAR Read AS DECIMAL.
 DEF VAR rDate AS DATE.
 DEF VAR cn AS INTEGER.
 DEF VAR cn1 AS INTEGER.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 8.5.


DEF FRAME frm-main
    SKIP(1)
   wsStatus       COLON 26  LABEL "Processing Account........." VIEW-AS TEXT
    btn-ok AT ROW 10.5 COL 10
    btn-exit AT ROW 10.5 COL 65 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13.0
    TITLE "METER READING EXPORT" VIEW-AS DIALOG-BOX.



 ON CHOOSE OF btn-ok IN FRAME frm-main 
     
 DO: 
    OUTPUT TO VALUE(wsFile).
   EXPORT DELIMITER "," "ACCOUNT" "METER" "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING""READING DATE" "READING"
                                                 "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING" "READING DATE" "READING".

    FOR EACH DBMMF:
            ASSIGN wsStatus:SCREEN-VALUE IN FRAME frm-main = string(dbmmf.dbAcc).
                    EXPORT DELIMITER "," dbmmf.dbAcc dbmmf.METER dbmmf.rDate[1] dbmmf.READ[1] dbmmf.rDate[2] dbmmf.READ[2] dbmmf.rDate[3] dbmmf.READ[3] dbmmf.rDate[4] dbmmf.READ[4] dbmmf.rDate[5] dbmmf.READ[5] dbmmf.rDate[6] dbmmf.READ[6]
                                                 dbmmf.rDate[7] dbmmf.READ[7] dbmmf.rDate[8] dbmmf.READ[8] dbmmf.rDate[9] dbmmf.READ[9] dbmmf.rDate[10] dbmmf.READ[10] dbmmf.rDate[11] dbmmf.READ[11] dbmmf.rDate[12] dbmmf.READ[12]  dbmmf.rDate[13] dbmmf.READ[13].
         
     END.
         
     OS-COMMAND NO-WAIT VALUE(wsFile)).
  END.

/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */
FIND FIRST simctr NO-LOCK.
wsFile = simctr.repDir + "METERREADING.CSV".
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

