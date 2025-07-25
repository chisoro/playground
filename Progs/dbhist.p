/* Program.................dbHist.p
   Notes:................. Account Transaction histor Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF  VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF  VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF  VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF  VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF  VAR wsPer LIKE  dbhtf.Accper.
DEF  VAR wsAcc LIKE  dbcmf.dbAcc.
DEF  VAR wsTitle AS CHAR FORM "x(80)" INITIAL "ACCOUNT TRANSACTION HISTORY REPORT FOR PERIOD: ".
DEF  VAR wsTitle2 AS CHAR FORM "x(80)".
DEF  VAR wsamt  AS DEC FORM "ZZZZZZZZZZ9.99-".
DEF  VAR wsFilter LIKE dbhtf.sgrp.
DEF  VAR wsVar LIKE dbsmf.Descrip.
DEF  VAR X AS INT.

FIND FIRST dbcmf NO-LOCK NO-ERROR.

DEF BUTTON btnAcc LABEL "ACCOUNT".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btnclose   LABEL "CLOSE".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 8.5.

DEF FRAME frm-main
    SKIP(1.5)
    btnAcc       COLON 5 SPACE(1)
    wsAcc        NO-LABEL
    dbcmf.NAME   VIEW-AS TEXT NO-LABEL NO-TAB-STOP  SKIP(0.5)
    wsPer        COLON 30 LABEL "START PERIOD" SKIP(0.5)
    wsFilter     COLON 30 LABEL "SERVICE"  HELP "Enter 0 for all Account Service Charges"
    SPACE(1) "(Enter 0 FOR ALL services)"  SKIP(0.5)
    btn-ok AT ROW 10.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 13.2 KEEP-TAB-ORDER
     TITLE "ACCOUNT TRANSACTION HISTORY REPORT" VIEW-AS DIALOG-BOX.

FORM 
    dbhtf.Accper LABEL "PERIOD"
    dbhtf.trDate  LABEL "DATE"
    dbhtf.Sgrp    LABEL "SS"
    dbhtf.Tarif   LABEL "TAR"
    dbhtf.Ref     LABEL "REF" FORM "X(12)"
    dbhtf.DESCRIP LABEL "DESCRIPTION" FORM "X(40)"
    dbhtf.Vat     LABEL "VAT"
    dbhtf.Amt     LABEL "AMOUNT" FORM "zzzzzzzzzz9.99-"
    wsAmt         LABEL "BALANCE"
    HEADER skip(5) wsTitle AT 10 "Page: "  PAGE-NUMBER(a)
    SKIP(3)
    "ACCCOUNT:"  AT 10 dbcmf.dbacc NO-LABEL
    "NAME    :"  AT 10 dbcmf.NAME
    "        :"  AT 10 dbcmf.stno + " " + dbcmf.street FORM "x(30)" 
    "ADDRESS :"  AT 10 dbcmf.add1
    "        :"  AT 10 dbcmf.add2
    "        :"  AT 10 dbcmf.add3 
    SKIP(1)
    SKIP(2)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX NO-LABEL WIDTH 132 FRAME frm-rpt.
    
DEF    QUERY qry-pickAcc FOR dbcmf SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
    DISPLAY dbcmf.dbAcc dbcmf.Name dbcmf.StandNo wsVar LABEL "Suburb" 
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcc 
    brw-pickAcc AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".
    
/* ******** Triggers  ***********/
ON  'enter':u OF wsAcc IN FRAME  frm-main
     OR 'tab':u OF wsAcc IN FRAME  frm-main
DO:
   FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC(wsAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbcmf THEN DO:
      MESSAGE "Account does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO: 
       DISPLAY dbcmf.Name WITH FRAM frm-main.
   END.
   RETURN.    
END.

ON CHOOSE OF btnAcc IN FRAME frm-main
DO:
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-pickAcc FOR EACH dbcmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcc.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickAcc 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
          OR 'enter':u OF brw-pickAcc
          OR 'mouse-select-dblclick' OF brw-pickAcc.
  CLOSE QUERY qry-pickAcc.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON row-display OF brw-PickAcc DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
    wsVar = dbsmf.Descrip.
END. 

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-pickAcc 
    OR 'mouse-select-dblclick' OF brw-pickAcc 
DO: 
   GET CURRENT qry-pickAcc NO-LOCK NO-WAIT.
    wsAcc = dbcmf.dbacc.
   DISPLAY  dbcmf.dbacc @ wsAcc WITH FRAME frm-main.
   RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-main 
DO:
   session:set-wait-state("").
    wsAmt = 0.
   ASSIGN  wsAcc =INT(wsAcc:SCREEN-VALUE)
           wsPer = INT(wsPer:SCREEN-VALUE)
           wsFilter  = INT(wsFilter:SCREEN-VALUE).
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsFilter:SCREEN-VALUE = "0".
APPLY 'ENTRY' TO wsAcc.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE report.ip:
    wsAmt = 0.
    FIND FIRST dbcmf WHERE dbcmf.dbAcc >= INT(wsAcc:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
    IF AVAILABLE dbcmf THEN DO:
        wsTitle2 = "ACCOUNT: " + string(dbcmf.dbAcc) + " ("  +  DBcmf.NAME + ")".
        wsAmt = AccBal.
        IF wsFilter = 0 THEN DO:
            FOR EACH dbhtf WHERE dbhtf.dbAcc = INT(dbcmf.dbAcc) AND dbhtf.Accper >= wsPer:
                wsAmt = wsAmt - dbhtf.Amt.
            END.  
            DISPLAY STREAM a "Balance b/f" @ dbhtf.DESCRIP wsAmt WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
            FOR EACH dbhtf WHERE dbhtf.dbacc = dbcmf.dbacc AND dbhtf.Accper >= wsPer NO-LOCK 
                              BREAK BY dbhtf.dbAcc:
                wsAmt = wsAmt + dbhtf.Amt.
                DISPLAY STREAM a dbhtf.Accper dbhtf.trDate dbhtf.Sgrp dbhtf.Tarif dbhtf.Ref  
                    dbhtf.DESCRIP dbhtf.Amt dbhtf.Vat wsAmt WITH FRAME frm-rpt.
                DOWN STREAM a WITH FRAME frm-rpt.
            END.
        END.
        ELSE DO:
            FIND FIRST dbsgr WHERE dbsgr.sgrp = wsFilter NO-LOCK NO-ERROR.
            IF NOT AVAILABLE dbsgr THEN DO:
                MESSAGE "NO Service records for this account" VIEW-AS ALERT-BOX.
                RETURN.
            END.
            wsAmt = 0.
            wsTitle2 = "ACCOUNT: " + string(dbcmf.dbAcc) + " ("  +  DBcmf.NAME + ")" 
                 + "- " + dbsgr.Descrip + " Services".
        
            FOR EACH dbblf WHERE dbblf.dbAcc = DEC(dbcmf.dbAcc) AND dbblf.Sgrp = wsFilter NO-LOCK:
                DO X = 1 TO 15:
                   wsAmt = wsAmt + dbblf.Amt[X].
                END.
                 wsAmt = wsAmt + dbblf.INT.
            END.
            FOR EACH dbhtf WHERE dbhtf.dbAcc = dbcmf.dbAcc AND dbhtf.Accper >= wsPer
                             AND dbhtf.Sgrp = wsFilter NO-LOCK:
                wsAmt = wsAmt - dbhtf.Amt.
            END.
            DISPLAY STREAM a "Balance b/f" @ dbhtf.DESCRIP wsAmt WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
            FOR EACH dbhtf WHERE dbhtf.dbacc = dbcmf.dbacc AND dbhtf.Accper >= wsPer 
                 AND dbhtf.Sgrp = wsFilter NO-LOCK BREAK BY dbhtf.dbAcc:
                wsAmt = wsAmt + dbhtf.Amt.
                DISPLAY STREAM a dbhtf.Accper dbhtf.trDate dbhtf.Sgrp dbhtf.Tarif dbhtf.Ref  
                    dbhtf.DESCRIP dbhtf.Amt dbhtf.Vat wsAmt WITH FRAME frm-rpt.
                DOWN STREAM a WITH FRAME frm-rpt.
            END.
        END.
    END.
    RELEASE dbhtf.
END.

