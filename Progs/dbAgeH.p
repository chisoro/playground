/* Program.................dbageH.p
   Notes:................. Age Analysis  Report from histoy
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsper LIKE dbhtf.accper.
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsSer   AS CHAR FORM "X(40)".
DEF VAR wsAmt AS DEC EXTENT 6 FORM "ZZZZZZZZZ9.99-".
DEF VAR wsTot AS DEC EXTENT 6   FORM "ZZZZZZZZZ9.99-".
DEF VAR wsRec AS DEC.
DEF VAR X AS INT.
DEF VAR wsStatus AS CHAR.
DEF VAR wsZero AS LOGICAL.
DEF  VAR wsFilter LIKE dbhtf.sgrp.
DEF VAR wsBut AS CHAR   FORM "x(8)".
DEF VAR wsFile AS CHAR FORM "x(40)".

DEF BUTTON btn-Export LABEL "Export".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsper       LABEL "Age Period" COLON 30  space(2) "(as CCYYMM, eg 202106)" SKIP(1)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
    wsFilter     COLON 30 LABEL "SERVICE"  HELP "Enter 0 for Consolidated Services"
    SPACE(1) "(Enter 0 for Consolidated Services)"  SKIP(0.5)
    wsZero      LABEL "Print Zero Balance Accounts" COLON 30 SKIP(0.5)
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(25) btn-Export space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "AGE ANALYSIS REPORT FROM HISTORY" VIEW-AS DIALOG-BOX.
FORM 
     dbcmf.dbAcc      LABEL "ACCOUNT"
     DBcmf.NAME       LABEL "NAME"      FORM "x(25)"
     wsAmt[1]        LABEL "CURRENT"
     wsAmt[2]        LABEL "30 DAYS"
     wsAmt[3]        LABEL "60 DAYS"
     wsAmt[4]        LABEL "90 DAYS"
     wsAmt[6]        LABEL "120+ DAYS"
     wsAmt[5]        LABEL "BALANCE"
    HEADER skip(1) "          DEBTORS AGE ANALYSIS REPORT AS AT: " AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 100 PAGE-NUMBER(a)
    SKIP(2) wsTitle
    SKIP(1)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

/* ******** Triggers  ***********/
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   wsBut = "Print".
    wsAmt = 0.
    wsTot = 0.
   ASSIGN  wsStart wsEnd  wszero wsFilter wsper.
   IF wsFilter <> 0 THEN
       FIND FIRST dbsgr WHERE dbsgr.sgrp = wsFilter NO-LOCK NO-ERROR.
   IF AVAILABLE dbsgr THEN
       wsTitle = dbsgr.descrip.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.

ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   wsBut = "Export".
   OUTPUT STREAM b TO VALUE(wsFile).
    wsAmt = 0.
    wsTot = 0.
   ASSIGN  wsStart wsEnd  wszero wsFilter wsper.
    EXPORT STREAM b DELIMITER ',' "DEBTORS AGE ANALYSIS REPORT".
    IF wsFilter <> 0 THEN
       FIND FIRST dbsgr WHERE dbsgr.sgrp = wsFilter NO-LOCK NO-ERROR.
   IF AVAILABLE dbsgr THEN
       EXPORT STREAM b DELIMITER ',' dbsgr.descrip.
     EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "CURRENT" "30-DAYS" "60-DAYS" "90-DAYS" "120+ DAYS" "BALANCE".
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       wsZero:SCREEN-VALUE = "YES"
       wsFilter:SCREEN-VALUE = "0"
       wsper:SCREEN-VALUE = STRING(simctr.curper).
wsFile = simctr.repDir + "dbageH" 
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


PROCEDURE Report.ip:
FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd NO-LOCK:
    DISPLAY dbcmf.dbacc @ wsStatus WITH FRAME frm-main.
    PAUSE 0.
    ASSIGN wsAmt = 0
           wsRec = 0.
    IF wsFilter <> 0 THEN DO:
        FOR EACH dbhtf WHERE dbhtf.dbacc = dbcmf.dbacc
                         AND dbhtf.accper <= wsPer AND dbhtf.sgrp = wsFilter NO-LOCK: 
            IF dbhtf.amt < 0 THEN
                ASSIGN wsrec = wsrec + dbhtf.amt
                       wsAmt[5] = wsAmt[5] + dbhtf.amt.
            ELSE IF dbhtf.amt > 0 THEN DO:
                IF dbhtf.Accper = wsper THEN
                    wsAmt[1] = wsAmt[1] + dbhtf.amt.
                IF dbhtf.Accper = (wsper - 1) THEN
                    wsAmt[2] = wsAmt[2] + dbhtf.amt.
                IF dbhtf.Accper = (wsper - 2) THEN
                    wsAmt[3] = wsAmt[3] + dbhtf.amt.
                IF dbhtf.Accper = (wsper - 3) THEN
                    wsAmt[4] = wsAmt[4] + dbhtf.amt.
                IF dbhtf.Accper <= (wsper - 4) THEN
                    wsAmt[6] = wsAmt[6] + dbhtf.amt.
                wsAmt[5] = wsAmt[5] + dbhtf.amt.
            END.
        END. /* eo each dbhtf */
    END.
    ELSE DO:
        FOR EACH dbhtf WHERE dbhtf.dbacc = dbcmf.dbacc
                         AND dbhtf.Accper <= wsPer NO-LOCK: 
            IF dbhtf.amt < 0 THEN
                ASSIGN wsrec = wsrec + dbhtf.amt
                       wsAmt[5] = wsAmt[5] + dbhtf.amt.
            ELSE IF dbhtf.amt > 0 THEN DO:
                IF dbhtf.Accper = wsper THEN
                    wsAmt[1] = wsAmt[1] + dbhtf.amt.
                IF dbhtf.Accper = (wsper - 1) THEN
                    wsAmt[2] = wsAmt[2] + dbhtf.amt.
                IF dbhtf.Accper = (wsper - 2) THEN
                    wsAmt[3] = wsAmt[3] + dbhtf.amt.
                IF dbhtf.Accper = (wsper - 3) THEN
                    wsAmt[4] = wsAmt[4] + dbhtf.amt.
                IF dbhtf.Accper <= (wsper - 4) THEN
                    wsAmt[6] = wsAmt[6] + dbhtf.amt.
                wsAmt[5] = wsAmt[5] + dbhtf.amt.
            END.
        END. /* eo each dbhtf */
    END.
    wsRec = wsRec * -1.
    IF wsRec <> 0 THEN DO:
        IF wsRec >= wsAmt[6] THEN
           ASSIGN wsRec = wsRec - wsAmt[6]
                  wsAmt[6] = 0.
            ELSE IF wsRec < wsAmt[6] THEN
               ASSIGN wsAmt[6] = wsAmt[6] - wsRec
                      wsRec = 0.
        IF wsRec >= wsAmt[4] THEN
           ASSIGN wsRec = wsRec - wsAmt[4]
                  wsAmt[4] = 0.
                ELSE IF wsRec < wsAmt[4] THEN
                   ASSIGN wsAmt[4] = wsAmt[4] - wsRec
                          wsRec = 0.
        IF wsRec >= wsAmt[3] THEN
           ASSIGN wsRec = wsRec - wsAmt[3]
                  wsAmt[3] = 0.
                ELSE IF wsRec < wsAmt[3] THEN
                   ASSIGN wsAmt[3] = wsAmt[3] - wsRec
                          wsRec = 0.
       IF wsRec >= wsAmt[2] THEN
           ASSIGN wsRec = wsRec - wsAmt[2]
                  wsAmt[2] = 0.
                ELSE IF wsRec < wsAmt[2] THEN
                   ASSIGN wsAmt[2] = wsAmt[2] - wsRec
                          wsRec = 0.
      IF wsRec <> 0 THEN
           ASSIGN wsAmt[1] = wsAmt[1] - wsRec
                  wsRec = 0.
    END.
    ASSIGN wsTot[1] = wsTot[1] + wsAmt[1]
           wsTot[2] = wsTot[2] + wsAmt[2]
           wsTot[3] = wsTot[3] + wsAmt[3]
           wsTot[4] = wsTot[4] + wsAmt[4]
           wsTot[5] = wsTot[5] + wsAmt[5]
           wsTot[6] = wsTot[6] + wsAmt[6].
    IF wsAmt[5] = 0 AND wsZero = NO THEN NEXT.
    IF wsBut = "Print"  THEN DO:
         DISPLAY STREAM a dbcmf.dbacc dbcmf.NAME wsAmt[1] wsAmt[2] wsAmt[3] wsAmt[4] wsAmt[6] wsAmt[5]
                WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
    ELSE
        EXPORT STREAM b DELIMITER "," dbcmf.dbacc dbcmf.NAME wsAmt[1] wsAmt[2] wsAmt[3] wsAmt[4] wsAmt[6] wsAmt[5].
   
END. /* eo-each dbcmf */
IF wsBut = "Print"  THEN DO:
    UNDERLINE STREAM a wsAmt[1] wsAmt[2] wsAmt[3] wsAmt[4] wsAmt[6] wsAmt[5]
            WITH FRAME frm-rpt.
    DISPLAY STREAM a "TOTAL" @ dbcmf.NAME 
        wsTot[1] @ wsAmt[1]
        wsTot[2] @ wsAmt[2]
        wsTot[3] @ wsAmt[3]
        wsTot[4] @ wsAmt[4]
        wsTot[5] @ wsAmt[5]
        wsTot[6] @ wsAmt[6] WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
END.
ELSE  
    EXPORT STREAM b DELIMITER "," "TOTAL" " " wsTot[1] wsTot[2] wsTot[3] wsTot[4] wsTot[6] wsTot[5] .
END.
