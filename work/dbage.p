/* Program.................dbrpt01.p
   Notes:................. Debtors Activity Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
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
DEF VAR st-sub LIKE dbsmf.suburb.
DEF VAR end-sub LIKE dbsmf.suburb.
DEF VAR end-con LIKE dbcmf.cons.
DEF VAR wsChoice AS CHAR FORM "x(1)".
DEF VAR wsTitle AS CHAR FORM "x(100)".
DEF VAR curAmt AS DEC EXTENT 4 FORM "Z,ZZZ,ZZ9.99-".
DEF VAR TAmt AS DEC EXTENT 4 FORM "Z,ZZZ,ZZ9.99-".
DEF VAR wsamt  AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR st-fund LIKE glfund.fund.
DEF VAR end-fund LIKE glfund.fund.
DEF VAR st-per  LIKE dbhtf.Accper.
DEF VAR end-per  LIKE dbhtf.Accper.
DEF VAR X AS INT.
DEF  VAR wsFilter LIKE dbblf.sgrp.
DEF VAR wsStatus AS CHAR.
DEF VAR wsZero AS LOGICAL.
DEF VAR wsSer LIKE dbsgr.Descrip FORM "X(40)".

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
    st-per      LABEL "Satrt Period" COLON 30 SPACE(1)
    end-per     LABEL "END Period"  SKIP(0.5)
    wsZero      LABEL "Print Zero Balance Accounts" COLON 30 SKIP(0.5)
    SKIP(2.5)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "AGE ANALYSIS REPORT" VIEW-AS DIALOG-BOX.

FORM 
     dbcmf.dbAcc        LABEL "ACCOUNT"
     DBcmf.NAME LABEL "NAME"
     curAmt[1]        LABEL "BILLED"
     curAmt[2]        LABEL "ADJUSTMENTS"
     curAmt[3]        LABEL "RECEIPTS"
     curAmt[4]        LABEL "PERIOD NET"
    HEADER skip(1) "          DEBTORS ACTIVITY REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 100 PAGE-NUMBER(a)
    SKIP(2) wsTitle FORM "X(80)"
    SKIP(1)
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

/* ******** Triggers  ***********/
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
            DISABLE st-con end-con st-ward end-ward WITH FRAME frm-main.
            ENABLE st-sub end-sub WITH FRAME frm-main.
            APPLY 'entry' TO st-sub.
       END. 
    END.
    RETURN.
END.
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
    curAmt = 0.
    TAmt = 0.
   ASSIGN wsStart wsEnd st-Con end-Con st-ward end-ward st-per end-per
          st-sub end-sub wsChoice wsZero.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
DISABLE st-con end-con st-ward end-ward st-sub end-sub WITH FRAME frm-main.
ASSIGN st-con:SCREEN-VALUE = "0"
       end-con:SCREEN-VALUE = "99"
       st-sub:SCREEN-VALUE = "0"
       end-sub:SCREEN-VALUE = "999"
       st-ward:SCREEN-VALUE = "0"
       end-ward:SCREEN-VALUE = "99"
       wsStart:SCREEN-VALUE = "0"
       WsEnd:SCREEN-VALUE = "999999999998"
       wsChoice:SCREEN-VALUE = "ALL"
       wszero:SCREEN-VALUE = "YES"
       st-Per:SCREEN-VALUE = SUBSTR(STRING(SIMCTR.CURPER),1,4) + "01"
       end-Per:SCREEN-VALUE = string(SIMCTR.CURPER).
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE report.ip:
IF wsChoice = "S" THEN DO:
    FOR EACH dbsmf WHERE dbsmf.suburb >= st-sub
                     AND dbsmf.suburb <= end-sub NO-LOCK.
        ASSIGN wsTitle = "           Suburb: - " + dbsmf.Descrip
               TAmt = 0.
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
            AND dbcmf.suburb = dbsmf.suburb USE-INDEX acc NO-LOCK:
           ASSIGN curAmt = 0
            wsStatus = string(dbcmf.dbacc).
           RUN idata.ip.
        END.
       UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4]
                    WITH FRAME frm-rpt.
       DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                        TAmt[4] @ CurAmt[4] WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
       PAGE STREAM a.
    END.
END.
ELSE IF wsChoice = "C" THEN DO:
    FOR EACH dbctf WHERE dbctf.cons >= st-Con AND dbctf.cons <= end-Con NO-LOCK.
        wsTitle = "           Consumer: - " + dbctf.Descrip.
        ASSIGN TAmt = 0.
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
            AND dbcmf.cons = dbctf.cons USE-INDEX acc NO-LOCK:
           ASSIGN curAmt = 0
            wsStatus = string(dbcmf.dbacc).
           RUN idata.ip.
        END.
       UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4]
                    WITH FRAME frm-rpt.
       DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                        TAmt[4] @ CurAmt[4] WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
       PAGE STREAM a.
    END.
END.
ELSE IF wsChoice = "W" THEN DO:
    FOR EACH dbwrd WHERE dbwrd.ward >= st-ward AND dbwrd.ward <= end-ward NO-LOCK:
        wsTitle = "          WARD: - " + dbwrd.Descrip.
        ASSIGN TAmt = 0.
        FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd 
            AND dbcmf.ward = dbwrd.ward USE-INDEX acc  NO-LOCK:
           ASSIGN curAmt = 0
            wsStatus = string(dbcmf.dbacc).
           RUN idata.ip.
        END.
       UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4]
                    WITH FRAME frm-rpt.
       DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                        TAmt[4] @ CurAmt[4] WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
       PAGE STREAM a.
       APPLY 'close' TO SELF.
       RETURN.
    END.
END.
ELSE DO:
   ASSIGN TAmt = 0
          wsTitle =  wsSer.
   FOR EACH dbcmf WHERE dbcmf.dbAcc >= wsStart AND dbcmf.dbAcc <= wsEnd USE-INDEX acc NO-LOCK:
       ASSIGN curAmt = 0
           wsStatus = string(dbcmf.dbacc).
       RUN idata.ip.
   END.
   UNDERLINE STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4]
                    WITH FRAME frm-rpt.
       DISPLAY STREAM a "TOTALS" @ dbcmf.NAME TAmt[1] @ CurAmt[1] TAmt[2] @ CurAmt[2] TAmt[3] @ CurAmt[3] 
                        TAmt[4] @ CurAmt[4] WITH FRAME frm-rpt.
       DOWN STREAM a WITH FRAME frm-rpt.
   APPLY 'close' TO SELF.
   RETURN.
END.
END.

PROCEDURE idata.ip:
FOR EACH dbhtf WHERE dbhtf.dbacc = dbcmf.dbacc AND dbhtf.Accper >= st-per AND dbhtf.Accper <= end-per NO-LOCK:
     DISPLAY wsStatus WITH FRAME frm-main.
     ASSIGN curAmt[1] = curAmt[1] + dbhtf.Amt + dbhtf.Vat WHEN dbhtf.prog = "dbsg67.p"
            curAmt[2] = curAmt[2] + dbhtf.Amt + dbhtf.Vat WHEN dbhtf.prog = "dbjnl02.p"
            curAmt[3] = curAmt[3] + dbhtf.Amt + dbhtf.Vat WHEN dbhtf.prog = "recupd.p".                
END.
/* IF curAmt[6] = 0 AND wsZero = NO THEN NEXT.
IF curAmt[6] < wsThresh THEN NEXT. */
curAmt[4] = curAmt[1] + curAmt[2] + curAmt[3].
DISPLAY STREAM a dbcmf.dbacc dbcmf.NAME CurAmt[1] CurAmt[2] CurAmt[3] CurAmt[4] WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
ASSIGN TAmt[1] = TAmt[1] + curAmt[1]
       TAmt[2] = TAmt[2] + curAmt[2]
       TAmt[3] = TAmt[3] + curAmt[3]
       TAmt[4] = TAmt[4] + curAmt[4].
END.
