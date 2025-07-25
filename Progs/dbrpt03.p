/* Program.................dbrpt03.p
   Notes:................. Posted Journals Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "LANDSCAPE"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsAmt   AS DEC   FORM "ZZZZZZZZZ9.99-".
DEF VAR wsVat   AS DEC  FORM "Z,ZZZ,ZZ9.99-".
DEF VAR st-per  LIKE dbhtf.Accper.
DEF VAR end-per  LIKE dbhtf.Accper.
DEF VAR X AS INT.
DEF VAR wsStatus AS CHAR.
DEF VAR wsDesc   AS CHAR.
DEF VAR wsBut AS CHAR.
DEF VAR wsFile AS CHAR FORM "x(40)".

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btn-Export LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(2.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(1.5)
    st-per      LABEL "START Period" COLON 30 SPACE(1)
    end-per     LABEL "END Period"  SKIP(0.5)
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(25) btn-Export SPACE(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "DETBORS POSTED JOURNAL REPORT" VIEW-AS DIALOG-BOX.

FORM 
     dbcmf.dbAcc      LABEL "ACOUNT"
     dbcmf.NAME       LABEL "ACCOUNT NAME" FORM "x(25)"
     dbhtf.Ref        LABEL "REFERENCE"
     dbhtf.trDate     LABEL "DATE"
     dbhtf.Sgrp       LABEL "SERVICE"
     dbhtf.DESCRIP    LABEL "NARRATION" FORM "x(20)"
     dbhtf.Vat        LABEL "VAT"
     dbhtf.Amt        LABEL "AMOUNT" FORM "ZZZZZZZZZ9.99-"
     /*dbhtf.UID        LABEL "INPUT"
     dbhtf.UID2       LABEL "UPDATE"
     wsDesc            LABEL "COMMENT" */
    HEADER skip(1) "       DETBORS POSTED JOURNAL REPORT FOR THE PERIOD: " AT 10 SPACE(2)
    STRING(ST-PER) " TO " STRING(END-PER)
    "Page: " AT 100 PAGE-NUMBER(a)
    SKIP(2) wsTitle FORM "X(80)"
    SKIP(1)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-LABELS NO-BOX
    WIDTH 132 FRAME frm-rpt.

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   wsBut = "Print".
   ASSIGN wsStart wsEnd  st-per end-per.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
END.

ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   wsBut = "Export".
   ASSIGN wsStart wsEnd  st-per end-per.
   OUTPUT STREAM b TO VALUE(wsFile).
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       st-Per:SCREEN-VALUE = SUBSTR(STRING(SIMCTR.CURPER),1,4) + "01"
       end-Per:SCREEN-VALUE = string(SIMCTR.CURPER).
ASSIGN wsFile = simctr.repDir + "Dbrpt03" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv". 
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE report.ip:
    IF wsBut = "Export" THEN DO:
        EXPORT STREAM b DELIMITER ',' "DETBORS POSTED JOURNAL REPORT FOR THE PERIOD: " + STRING(st-per) + " TO " + STRING(end-per). 
        EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "REFERENCE" "DATE" "SERVICE" "DESCRIPTION" "VAT" "AMOUNT".
    END.
    FOR EACH dbhtf WHERE dbhtf.dbAcc >= wsStart AND dbhtf.dbAcc <= wsEnd
                        AND dbhtf.Accper >= int(st-per) AND dbhtf.Accper <= int(end-per) 
                        AND dbhtf.prog = "dbjnl02.p" BY dbhtf.TransID BY dbhtf.ref:
        wsStatus = STRING(dbhtf.dbAcc).
        IF dbhtf.UID2 = dbhtf.UID THEN wsDesc = "?".
        ELSE wsDesc = "".
        DISPLAY wsStatus WITH FRAME frm-main.
        FIND FIRST dbcmf WHERE dbcmf.dbacc = dbhtf.dbacc  NO-LOCK NO-ERROR.
        IF wsBut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' dbcmf.dbAcc dbcmf.NAME dbhtf.Ref dbhtf.trDate dbhtf.Sgrp
                dbhtf.DESCRIP dbhtf.Vat dbhtf.Amt.
        ELSE DO:
            DISPLAY STREAM a dbcmf.dbAcc dbcmf.NAME dbhtf.Ref dbhtf.trDate dbhtf.Sgrp
                     dbhtf.DESCRIP dbhtf.Vat dbhtf.Amt /*dbhtf.UID2 dbhtf.UID wsDesc */ WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        
   END.
   APPLY 'ENTRY' TO btn-exit.
END.
   
