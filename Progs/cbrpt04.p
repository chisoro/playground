/* Program.................cbrpt04.p
   Notes:................. Cashbook Transaction history Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF  VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF  VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF  VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF  VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF  VAR st-Per LIKE  CBTrans.Accper.
DEF  VAR end-Per LIKE  CBTrans.Accper.
DEF  VAR wsAcc LIKE  cbkmf.Bank.
DEF  VAR wsTitle AS CHAR FORM "x(80)" INITIAL "ACCOUNT TRANSACTION HISTORY REPORT FOR PERIOD: ".
DEF  VAR wsTitle2 AS CHAR FORM "x(80)".
DEF  VAR wsTitle3 AS CHAR FORM "x(80)".
DEF  VAR wsamt  AS DEC FORM "ZZZZZZZZZZ9.99-".
DEF  VAR wsVar LIKE dbsmf.Descrip.
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF VAR wsBut AS CHAR.
DEF  VAR X AS INT.
DEF VAR LName LIKE glmf.DESCRIPTION.

DEF BUTTON btnAcc LABEL "CASHBOOK".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-Export LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 8.5.

DEF FRAME frm-main
    SKIP(2)
    btnAcc       COLON 17 SPACE(1)
    wsAcc        NO-LABEL
    cbkmf.descrip   FORM "X(40)" VIEW-AS TEXT NO-LABEL NO-TAB-STOP  SKIP(0.5)
    st-Per        COLON 30 LABEL "START PERIOD" SKIP(0.5)
    end-Per       COLON 30 LABEL "END PERIOD" SKIP(0.5)
    SKIP(2.5)
    btn-ok AT ROW 10.7 COL 20
    SPACE(25) btn-Export SPACE(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 13.2 KEEP-TAB-ORDER
     TITLE "ACCOUNT TRANSACTION HISTORY REPORT" VIEW-AS DIALOG-BOX.

FORM 
    CBTrans.Accper  LABEL "PERIOD"
    CBTrans.trDate  LABEL "DATE"
    CBTrans.Ref     LABEL "REF" FORM "X(12)"
    CBTrans.Ledger  LABEL "ALLOCATION"
    CBTrans.Descrip LABEL "DESCRIPTION" FORM "X(40)"
    CBTrans.amount  LABEL "AMOUNT"
    wsAmt           LABEL "BALANCE"
    HEADER skip(5) wsTitle AT 10 "Page: "  PAGE-NUMBER(a) SKIP(1)
    wsTitle2 SKIP(1)
    wsTitle3 SKIP(1)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX NO-LABEL WIDTH 132 FRAME frm-rpt.
    
DEF    QUERY qry-pickAcc FOR CBKMF SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
    DISPLAY cbkmf.Bank LABEL "Bank" cbkmf.descrip LABEL "Bank NAME" 
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcc 
    brw-pickAcc AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".
    
/* ******** Triggers  ***********/
ON  'enter':u OF wsAcc IN FRAME  frm-main
     OR 'tab':u OF wsAcc IN FRAME  frm-main
DO:
   FIND FIRST cbkmf WHERE cbkmf.Bank = INT(wsAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE cbkmf THEN DO:
      MESSAGE "Account does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO: 
       DISPLAY cbkmf.descrip WITH FRAM frm-main.
   END.
   RETURN.    
END.

ON CHOOSE OF btnAcc IN FRAME frm-main
DO:
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-pickAcc FOR EACH cbkmf WHERE cbkmf.Bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcc.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pickAcc 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
          OR 'enter':u OF brw-pickAcc
          OR 'mouse-select-dblclick' OF brw-pickAcc.
  CLOSE QUERY qry-pickAcc.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-pickAcc 
    OR 'mouse-select-dblclick' OF brw-pickAcc 
DO: 
   GET CURRENT qry-pickAcc NO-LOCK NO-WAIT.
    wsAcc = cbkmf.Bank.
   DISPLAY  cbkmf.Bank @ wsAcc WITH FRAME frm-main.
   RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-main 
DO:
   session:set-wait-state("").
   wsBut = "Print".
   wsAmt = cbkmf.bal.
   ASSIGN  wsAcc =INT(wsAcc:SCREEN-VALUE)
           st-Per = INT(st-Per:SCREEN-VALUE)
           end-Per = INT(end-Per:SCREEN-VALUE).
   wsTitle = "ACCOUNT TRANSACTION HISTORY REPORT FOR PERIOD: "
           + STRING(st-per) + " TO " + STRING(end-per).
   wsTitle2 = "CASHBOOK: " + STRING(cbkmf.Bank) + " - " + cbkmf.descrip.
   wsTitle3 = "CURRENT BALANCE: " + STRING(cbkmf.Bal).
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.

ON 'choose':U OF btn-Export IN FRAME frm-main 
DO:
   session:set-wait-state("").
   OUTPUT STREAM b TO VALUE(wsFile).
   wsAmt = cbkmf.bal.
   wsBut = "Export".
   ASSIGN  wsAcc =INT(wsAcc:SCREEN-VALUE)
           st-Per = INT(st-Per:SCREEN-VALUE)
           end-Per = INT(end-Per:SCREEN-VALUE).
   wsTitle = "ACCOUNT TRANSACTION HISTORY REPORT FOR PERIOD: "
           + STRING(st-per) + " TO " + STRING(end-per).
   wsTitle2 = "CASHBOOK: " + STRING(cbkmf.Bank) + " - " + cbkmf.descrip.
   wsTitle3 = "CURRENT BALANCE: " + STRING(cbkmf.Bal).
   RUN Report.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK.
st-Per:SCREEN-VALUE = SUBSTR(STRING(SIMCTR.CURPER),1,4) + "01".
end-Per:SCREEN-VALUE = string(SIMCTR.CURPER).
wsFile = simctr.repDir + "dbage" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
APPLY 'ENTRY' TO wsAcc.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE report.ip:
    FOR EACH CBTrans WHERE CBTrans.bank = wsAcc AND CBTrans.Accper >= st-Per  
        AND (cbTrans.Ledger <> 0 OR cbTrans.Ledger <> 99):
        wsAmt = wsAmt - CBTrans.amount.
    END.
    IF wsBut = "Export" THEN DO:
        EXPORT STREAM b DELIMITER ',' wsTitle.
        EXPORT STREAM b DELIMITER ',' wsTitle2.
        EXPORT STREAM b DELIMITER ',' wsTitle3.
        EXPORT STREAM b DELIMITER ',' "PERIOD" "DATE" "REFERENCE" "ALLOCATION" "DESCRIPTION" "NARRATION" "AMOUNT" "BALANCE".
        EXPORT STREAM b DELIMITER ',' "" "" "" "" "BALANCE B/F"  "" wsAmt.
    END.
    ELSE DO:
        DISPLAY STREAM a "BALANCE B/F" @ CBTrans.Descrip wsAmt  WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
    FOR EACH CBTrans WHERE CBTrans.Bank = wsAcc AND CBTrans.Accper >= st-Per AND CBTrans.Accper <= end-Per
        BY cbTrans.ref BY cbTrans.seq:
        FIND FIRST glmf WHERE glmf.acct= CBTrans.Ledger NO-LOCK NO-ERROR.
        IF AVAILABLE glmf THEN
            LName = glmf.DESCRIPTION.
        ELSE LName = "** Invalid Ledger ** " + STRING(CBTrans.Ledger).
        IF (cbTrans.Ledger <> 0 OR cbTrans.Ledger <> 99) THEN
           wsAmt = wsAmt + CBTrans.amount.
        IF wsBut = "Export" THEN
            EXPORT STREAM b DELIMITER ',' CBTrans.Accper CBTrans.trDate CBTrans.Ref CBTrans.Ledger lName CBTrans.Descrip 
                                    CBTrans.amount  wsAmt.
        ELSE DO:
            DISPLAY STREAM a CBTrans.Accper CBTrans.trDate CBTrans.Ref CBTrans.Ledger CBTrans.Descrip CBTrans.amount
                  wsAmt WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
    END.
    APPLY 'ENTRY' TO BTN-EXIT IN FRAME FRM-MAIN.
END.

