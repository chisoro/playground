/* Program.................dbage.p
   Notes:................. Age Analysis Report
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
DEF VAR st-ward LIKE dbcmf.ward.
DEF VAR end-ward LIKE dbcmf.ward.
DEF VAR st-con LIKE dbcmf.cons.
DEF VAR end-con LIKE dbcmf.cons.
DEF VAR st-sub LIKE dbsmf.suburb.
DEF VAR end-sub LIKE dbsmf.suburb.
DEF VAR wsChoice AS CHAR FORM "x(1)".
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR wsSer   AS CHAR FORM "X(40)".
DEF VAR curAmt AS DEC EXTENT 6 FORM "ZZZZZZZZZ9.99-".
DEF VAR TAmt AS DEC EXTENT 6   FORM "ZZZZZZZZZ9.99-".
DEF VAR wsamt  AS DEC EXTENT 2 FORM "ZZZZZZZZZ9.99-".
DEF VAR st-fund LIKE glfund.fund.
DEF VAR end-fund LIKE glfund.fund.
DEF VAR wsSgrp   AS CHAR FORM "x(50)".
DEF VAR j AS INT.
DEF VAR end-grp   LIKE dbblf.Sgrp.
DEF VAR X AS INT.
DEF VAR wsStatus AS CHAR.
DEF VAR wsThresh AS DEC.
DEF VAR wsZero AS LOGICAL.
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
    wsChoice       LABEL "Report By?" COLON 30 
    view-as combo-box size 30 by 4 list-item-pairs
      "All - All Accounts","A","C - By Consumer","C","W - By Ward","W","S - By Suburb","S"
    SKIP(0.5)
    st-Con      LABEL "Consumer FROM" COLON 30 VIEW-AS FILL-IN SIZE 6 BY 1 SPACE(1)
    end-Con     LABEL "TO" VIEW-AS FILL-IN SIZE 6 BY 1 SKIP(0.5)
    st-sub      LABEL "Suburb FROM" COLON 30 SPACE(1)
    end-sub     LABEL "TO"  SKIP(0.5)
    st-ward     LABEL "Ward FROM" VIEW-AS FILL-IN SIZE 6 BY 1 COLON 30 SPACE(1)
    end-ward    LABEL "TO"  VIEW-AS FILL-IN SIZE 6 BY 1 SKIP(0.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
    "Enter service codes, comma seperated,  blank for ALL" COLON 30 SKIP
    wsSgrp     COLON 30 LABEL "Services"  SKIP(0.5)
    wsZero      LABEL "Print Zero Balance Accounts" COLON 30 SKIP(0.5)
    wsThresh      LABEL "Minimun Balance" COLON 30
    SKIP(1.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(25) btn-Export space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "AGE ANALYSIS REPORT" VIEW-AS DIALOG-BOX.
FORM 
     dbcmf.dbAcc      LABEL "ACCOUNT"
     DBcmf.NAME       LABEL "NAME"      FORM "x(25)"
     /*dbcmf.cell       LABEL "Cell" */
     curAmt[1]        LABEL "CURRENT"
     curAmt[2]        LABEL "30 DAYS"
     curAmt[3]        LABEL "60 DAYS"
     curAmt[4]        LABEL "90 DAYS"
     curAmt[5]        LABEL "120+ DAYS"
     curAmt[6]        LABEL "TOTAL"
    HEADER skip(1) "          DEBTORS AGE ANALYSIS REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 100 PAGE-NUMBER(a)
    SKIP(2) wsTitle
    SKIP(1)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

/* ******** Triggers  ***********/
ON 'tab':U OF wsZero
OR   'enter' OF wsZero
DO:
    IF logical(wsZero:SCREEN-VALUE) = YES THEN
        DISABLE wsThresh WITH FRAME frm-main.
    ELSE
        ENABLE wsThresh WITH FRAME frm-main.
    RETURN.
END.

ON 'tab':U OF wsChoice 
    OR 'enter' OF wsChoice
DO:
    ASSIGN wsChoice = wsChoice:SCREEN-VALUE.
    CASE wsChoice:
        WHEN "A" THEN DO:
            DISABLE st-con end-con st-ward end-ward st-sub end-sub WITH FRAME frm-main.
            APPLY 'tab' TO SELF.
        END.
        WHEN "C" THEN DO:
            DISABLE st-ward end-ward st-sub end-sub WITH FRAME frm-main.
            ENABLE st-con end-con WITH FRAME frm-main.
            APPLY 'entry' TO st-con.
        END.     
       WHEN "W" THEN DO:
            DISABLE st-con end-con st-sub end-sub WITH FRAME frm-main.
            ENABLE st-ward end-ward WITH FRAME frm-main.
            APPLY 'entry' TO st-ward.
       END. 
       WHEN "S" THEN DO:
            DISABLE st-con end-con st-ward end-ward  WITH FRAME frm-main.
            ENABLE st-sub end-sub  WITH FRAME frm-main.
            APPLY 'entry' TO st-sub.
       END. 
    END.
    RETURN.
END.
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   wsBut = "Print".
    curAmt = 0.
    TAmt = 0.
   ASSIGN  wsStart wsEnd st-Con end-Con st-ward end-ward wsChoice wsThresh wsZero
            st-sub end-sub wsSgrp.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.

ON 'choose':U OF btn-Export  
DO:
   session:set-wait-state("").
   wsBut = "Export".
   OUTPUT STREAM b TO VALUE(wsFile).
    curAmt = 0.
    TAmt = 0.
   ASSIGN  wsStart wsEnd st-Con end-Con st-ward end-ward wsChoice wsThresh wsZero
            st-sub end-sub wsSgrp.
    EXPORT STREAM b DELIMITER ',' "DEBTORS AGE ANALYSIS REPORT".
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN st-con:SCREEN-VALUE = "0"
       end-con:SCREEN-VALUE = "99"
       st-sub:SCREEN-VALUE = "0"
       end-sub:SCREEN-VALUE = "999"
       st-ward:SCREEN-VALUE = "0"
       end-ward:SCREEN-VALUE = "99"
       wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       wsZero:SCREEN-VALUE = "YES"
       wsThresh:SCREEN-VALUE = "0.00"
       wsSgrp:SCREEN-VALUE = ""
       wsChoice:SCREEN-VALUE = "ALL".
wsFile = simctr.repDir + "dbage" 
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
IF wsSgrp = "" THEN DO:
    FOR EACH dbsgr:
        wsSgrp = wsSgrp + STRING(dbsgr.sgrp) + ','.
    END.
     wsSer = "CONSOLIDATED SERVICES".
END.         
ELSE wsSer = UPPER(wsSgrp).

IF wsChoice = "C" THEN DO:
    IF wsbut = "Export" THEN
       EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "CURRENT"  "30-DAYS"  "60-DAYS" "90-DAYS" "120+ DAYS" "TOTAL".
    FOR EACH dbctf WHERE dbctf.cons >= st-Con AND dbctf.cons <= end-Con NO-LOCK.
        wsTitle = "           Consumer: - " + dbctf.Descrip + "    - SERVICE: " + wsSer.
        ASSIGN TAmt = 0.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "CONSUMER: " + dbctf.Descrip .
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
            AND dbcmf.cons = dbctf.cons AND dbcmf.AccStat = 0 USE-INDEX acc NO-LOCK:
           ASSIGN curAmt = 0
            wsStatus = string(dbcmf.dbacc).
           RUN idata.ip.
        END.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "TOTALS" "" "" TAmt[1]  TAmt[2]  TAmt[3] TAmt[4] TAmt[5] TAmt[6].
        ELSE DO:
           UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] CurAmt[5] CurAmt[6]
                        WITH FRAME frm-rpt.
           DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                            TAmt[4] @ CurAmt[4] TAmt[5] @ CurAmt[5] TAmt[6] @ CurAmt[6]  WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
           PAGE STREAM a.
        END.
    END.
END.
ELSE IF wsChoice = "W" THEN DO:
    IF wsbut = "Export" THEN
       EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "CELL" "CURRENT"  "30-DAYS"  "60-DAYS" "90-DAYS" "120+ DAYS" "TOTAL".
    FOR EACH dbwrd WHERE dbwrd.ward >= st-ward AND dbwrd.ward <= end-ward NO-LOCK:
        wsTitle = "          WARD: - " + dbwrd.Descrip + "    - SERVICE: " + wsSer.
        ASSIGN TAmt = 0.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "WARD: " + dbwrd.Descrip.
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
            AND dbcmf.ward = dbwrd.ward AND dbcmf.AccStat = 0 USE-INDEX acc NO-LOCK:
           ASSIGN curAmt = 0
            wsStatus = string(dbcmf.dbacc).
           RUN idata.ip.
        END.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "TOTALS" "" "" TAmt[1]  TAmt[2]  TAmt[3] TAmt[4] TAmt[5] TAmt[6].
        ELSE DO:
           UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] CurAmt[5] CurAmt[6]
                    WITH FRAME frm-rpt.
           DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                            TAmt[4] @ CurAmt[4] TAmt[5] @ CurAmt[5] TAmt[6] @ CurAmt[6]  WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
           PAGE STREAM a.
        END.
       
    END.
END.
ELSE IF wsChoice = "S" THEN DO:
    IF wsbut = "Export" THEN
       EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "CELL" "CURRENT"  "30-DAYS"  "60-DAYS" "90-DAYS" "120+ DAYS" "TOTAL".
    FOR EACH dbsmf WHERE dbsmf.suburb >= st-sub AND dbsmf.suburb <= end-sub NO-LOCK:
        wsTitle = "          SUBURB: - " + dbsmf.Descrip + "    - SERVICE: " + wsSer.
        ASSIGN TAmt = 0.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "SUBURB: " + dbsmf.Descrip.
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
            AND dbcmf.suburb = dbsmf.suburb AND dbcmf.AccStat = 0 USE-INDEX acc NO-LOCK:
           ASSIGN curAmt = 0
            wsStatus = string(dbcmf.dbacc).
           RUN idata.ip.
        END.
        IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "TOTALS" "" "" TAmt[1]  TAmt[2]  TAmt[3] TAmt[4] TAmt[5] TAmt[6].
        ELSE DO:
           UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] CurAmt[5] CurAmt[6]
                        WITH FRAME frm-rpt.
           DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                            TAmt[4] @ CurAmt[4] TAmt[5] @ CurAmt[5] TAmt[6] @ CurAmt[6]  WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
           PAGE STREAM a.
        END.
    END.
END.
ELSE DO:
   ASSIGN TAmt = 0
          wsTitle = "    SERVICE: " + wsSer
          curAmt = 0.
   IF wsbut = "Export" THEN
       EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "CELL" "CURRENT"  "30-DAYS"  "60-DAYS" "90-DAYS" "120+ DAYS" "TOTAL".
   FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
       AND dbcmf.AccStat = 0 USE-INDEX acc NO-LOCK:
       ASSIGN curAmt = 0
           wsStatus = string(dbcmf.dbacc).
       RUN idata.ip.
   END.
   IF wsbut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' "TOTALS" "" "" TAmt[1]  TAmt[2]  TAmt[3] TAmt[4] TAmt[5] TAmt[6].
   ELSE DO:
       UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] CurAmt[5] CurAmt[6]
                    WITH FRAME frm-rpt.
       DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                        TAmt[4] @ CurAmt[4] TAmt[5] @ CurAmt[5] TAmt[6] @ CurAmt[6]  WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
   END.
   APPLY 'close' TO SELF.
   RETURN.
END.
APPLY 'close' TO THIS-PROCEDURE.
END.

PROCEDURE idata.ip:
 DO X = 1 TO NUM-ENTRIES(wsSgrp):
       j = INT(ENTRY(x,wsSgrp)).
       FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc AND dbblf.Sgrp = j NO-LOCK:
             DISPLAY wsStatus WITH FRAME frm-main.
             ASSIGN curAmt[1] = curAmt[1] + Amt[1] + Amt[2]
                    curAmt[2] = curAmt[2] + Amt[3]
                    curAmt[3] = curAmt[3] + Amt[4] + dbblf.Int
                    curAmt[4] = curAmt[4] + Amt[5]
                    curAmt[5] = curAmt[5] + Amt[6] + Amt[7] + Amt[8] + Amt[9] +  Amt[10] + Amt[11] 
                              + Amt[12] + Amt[13] + Amt[14] + Amt[15].                
       END.
 END.
ASSIGN curAmt[6] = CurAmt[1] + CurAmt[2] + CurAmt[3] + CurAmt[4] + CurAmt[5].
IF curAmt[6] = 0 AND wsZero = NO THEN NEXT.
IF curAmt[6] < wsThresh AND wsThresh <> 0.00 THEN NEXT.
IF wsBut = "Export" THEN
    EXPORT STREAM b DELIMITER ',' dbcmf.dbacc dbcmf.NAME dbcmf.cell CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] CurAmt[5] CurAmt[6].
ELSE DO:
     DISPLAY STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] CurAmt[5] CurAmt[6]
        WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
 END.

ASSIGN TAmt[1] = TAmt[1] + curAmt[1]
       TAmt[2] = TAmt[2] + curAmt[2]
       TAmt[3] = TAmt[3] + curAmt[3]
       TAmt[4] = TAmt[4] + curAmt[4]
       TAmt[5] = TAmt[5] + curAmt[5]
       TAmt[6] = TAmt[6] + curAmt[6].
END.
