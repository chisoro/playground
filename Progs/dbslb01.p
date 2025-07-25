/*
  PROGRAM...............:dbslb01.p
 Notes:................. Service Level Benchmark Report 
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE  dbcmf.dbAcc.
DEF VAR wsend LIKE  dbcmf.dbAcc.
DEF VAR wsfile AS CHAR FORM "X(60)".
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR st-per  LIKE dbmtf.Accper.
DEF VAR end-per  LIKE dbmtf.Accper.
DEF VAR X AS INT.
DEF VAR wsStatus AS CHAR.
DEF VAR wsHeader AS CHAR FORM "X(60)" INITIAL "SERVICE LEVEL BENCHMARK REPORT".
DEF VAR wsrate LIKE tblforexh.decrate.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF TEMP-TABLE tt
    FIELD acc LIKE dbhtf.dbacc
    FIELD svgr AS INT
    FIELD amt AS DEC EXTENT 5 FORM "zzz,zzz,zzz,zz9.99-".

FORM SPACE(10)
    dbsgr.Descrip LABEL "SERVICE"  FORM "x(20)"
    tt.AMT[1]        LABEL "ARREARS"
    tt.AMT[2]        LABEL "YTD-BILL"
    tt.AMT[3]        LABEL "YTD-PAYMENTS"
    tt.AMT[4]        LABEL "TO-ARREARS"
    tt.AMT[5]        LABEL "TO-CURRENT"
   HEADER SKIP(4) wsTitle AT 30
           SKIP(2) wsHeader AT 15
    "Page:" AT 80 TRIM(STRING(PAGE-NUMBER(a))) SKIP(2)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-LABEL NO-BOX WIDTH 132 FRAME frmrpt.

DEF FRAME frm-main
    SKIP(2.5)
    wsStart     LABEL "Start Account" COLON 30   SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30    SKIP(1.5)
    wsYear      LABEL "Reporting Year" COLON 30  SKIP(1.5)
    SKIP(1.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "SERVICE LEVEL BENCHMARK REPORT" VIEW-AS DIALOG-BOX.

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   SESSION:SET-WAIT-STATE("").
   FIND FIRST simctr NO-LOCK NO-ERROR.
   ASSIGN wsStart 
          wsEnd 
          wsYear
          st-per = simctr.curper.
   wsYear = INT(STRING(wsYear) + "01").
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &PAGED} 

OS-COMMAND NO-WAIT VALUE(wsFile). 
APPLY 'entry' TO btn-exit IN FRAME frm-main.
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsStart:SCREEN-VALUE = "0"
       wsEnd:SCREEN-VALUE = "999999999998"
       wsyear:SCREEN-VALUE = SUBSTR(STRING(simctr.curper),1,4)
       wsTitle = SIMCTR.CONAME
       wsfile = simctr.repdir + "dbslb01".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE Report.ip:
    wsfile = wsfile + ".csv". 
    OUTPUT STREAM b TO VALUE(wsfile).
    FOR EACH dbhtf NO-LOCK:
        PAUSE 0 BEFORE-HIDE.
        DISPLAY dbhtf.dbacc @ wsStatus WITH NO-LABEL OVERLAY FRAME frm-main.
        IF dbhtf.txtcur <> "" THEN DO:
            FIND LAST tblforexh WHERE tblforexh.txtcur = "USD" AND tblForexH.DtRate <= dbhtf.trdate NO-ERROR.
            wsrate = tblforexh.decrate.
        END.
        ELSE wsrate = 1.
        FIND FIRST tt WHERE tt.svgr = dbhtf.sgrp AND tt.acc = dbhtf.dbacc NO-ERROR.
        IF NOT AVAILABLE tt THEN DO:
            CREATE tt.
            ASSIGN tt.acc = dbhtf.dbacc
                   tt.svgr = dbhtf.sgrp.
        END.
        IF accper < wsyear THEN
           ASSIGN tt.amt[1] = tt.amt[1] + ROUND((dbhtf.amt * wsrate),2).
        ELSE DO:
            ASSIGN tt.amt[2] = tt.amt[2] + ROUND((dbhtf.amt * wsrate ),2) WHEN  prog <> "recupd.p"
                   tt.amt[3] = tt.amt[3] + ROUND((dbhtf.amt * wsrate),2) WHEN prog = "recupd.p".
            IF tt.amt[1] >= (tt.amt[3] * -1) THEN
                ASSIGN tt.amt[4] = tt.amt[4] + ROUND((dbhtf.amt * wsrate ),2) WHEN prog = "recupd.p".
            ELSE IF tt.amt[1] < (tt.amt[3] * -1) THEN
                ASSIGN tt.amt[4] = tt.amt[1] * -1         WHEN tt.amt[3] <> 0
                       tt.amt[5] = tt.amt[3] +  tt.amt[1] WHEN tt.amt[3] <> 0.
        END.
    END.
    EXPORT STREAM b DELIMITER ',' "ACCOUNT" "SERVICE" "ARREARS" "YTD-BILL" "YTD-PAYMENTS" "TO-ARREARS" "TO-CURRENT".
    FOR EACH tt BREAK BY tt.svgr:
        ACCUMULATE tt.amt[1] (SUB-TOTAL BY tt.svgr).
        ACCUMULATE tt.amt[2] (SUB-TOTAL BY tt.svgr).
        ACCUMULATE tt.amt[3] (SUB-TOTAL BY tt.svgr).
        ACCUMULATE tt.amt[4] (SUB-TOTAL BY tt.svgr).
        ACCUMULATE tt.amt[5] (SUB-TOTAL BY tt.svgr).
        EXPORT STREAM b DELIMITER ',' tt.
        IF LAST-OF(tt.svgr) THEN DO:
            FIND FIRST dbsgr WHERE dbsgr.sgrp = tt.svgr NO-ERROR.
            DISPLAY STREAM a dbsgr.descrip
                    ACCUM SUB-TOTAL BY tt.svgr tt.amt[1] @ tt.amt[1]
                    ACCUM SUB-TOTAL BY tt.svgr tt.amt[2] @ tt.amt[2]
                    ACCUM SUB-TOTAL BY tt.svgr tt.amt[3] @ tt.amt[3]
                    ACCUM SUB-TOTAL BY tt.svgr tt.amt[4] @ tt.amt[4]
                    ACCUM SUB-TOTAL BY tt.svgr tt.amt[5] @ tt.amt[5]
                WITH FRAME frmrpt.
                DOWN STREAM a WITH FRAME frmrpt.
        END.
    END.
    OUTPUT STREAM b CLOSE.
END PROCEDURE. 
