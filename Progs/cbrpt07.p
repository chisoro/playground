/* Program.................cbrpt07.p
   Notes:................. Bank Transaction history Report-Summary
   Author:.................S. Mawire
   Edited:.................S. Chisoro
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
DEF  VAR wsAcc LIKE  cbkmf.bank.
DEF  VAR wsTitle AS CHAR FORM "x(80)" INITIAL "ACCOUNT TRANSACTION HISTORY REPORT FOR PERIOD: ".
DEF  VAR wsTitle2 AS CHAR FORM "x(80)".
DEF  VAR wsTitle3 AS CHAR FORM "x(80)".
DEF  VAR wsamt  AS DEC FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsInc LIKE cbTrans.amount.
DEF VAR wsExp LIKE cbTrans.amount.
DEF VAR wsAmt1 LIKE cbTrans.amount.
DEF  VAR wsVar LIKE dbsmf.Descrip.
DEF VAR wsfile AS CHAR FORM "x(40)".
DEF VAR wsBut AS CHAR.
DEF  VAR X AS INT.

DEF BUTTON btnAcc LABEL "BANK".
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
    btnAcc       COLON 22 SPACE(1)
    wsAcc        NO-LABEL
    cbkmf.descrip   VIEW-AS TEXT NO-LABEL NO-TAB-STOP  SKIP(0.5)
    st-Per        COLON 30 LABEL "START PERIOD" SKIP(0.5)
    end-Per       COLON 30 LABEL "END PERIOD" SKIP(0.5)
    SKIP(2.5)
    btn-ok AT ROW 10.7 COL 20
    SPACE(25) btn-Export SPACE(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 13.2 KEEP-TAB-ORDER
     TITLE "ACCOUNT TRANSACTION HISTORY REPORT WITH SUMMARY" VIEW-AS DIALOG-BOX.

FORM 
    CBTrans.Accper  LABEL "PERIOD"
    CBTrans.trDate  LABEL "DATE"
    CBTrans.Ref     LABEL "REF" FORM "X(12)"
    CBTrans.Ledger  LABEL "ALLOCATION"
    CBTrans.Descrip LABEL "DESCRIPTION" FORM "X(40)"
    CBTrans.amount  LABEL "AMOUNT" FORM "ZZZZZZZZZZ9.99-"
    wsAmt           LABEL "BALANCE"
    HEADER skip(5) wsTitle AT 10 "Page: "  PAGE-NUMBER(a) SKIP(1)
    wsTitle2 SKIP(1)
    wsTitle3 SKIP(1)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX NO-LABEL WIDTH 132 FRAME frm-rpt.
    
DEF    QUERY qry-pickAcc FOR CBKMF SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
    DISPLAY cbkmf.bank LABEL "BANK" cbkmf.descrip LABEL "BANK NAME" 
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
   FIND FIRST cbkmf WHERE cbkmf.bank = INT(wsAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
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
  OPEN QUERY qry-pickAcc FOR EACH cbkmf WHERE cbkmf.bank <> 0 NO-LOCK.
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
    wsAcc = cbkmf.bank.
   DISPLAY  cbkmf.bank @ wsAcc WITH FRAME frm-main.
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
   wsTitle = wsTitle + STRING(st-per) + " TO " + STRING(end-per).
   wsTitle2 = "BANK: " + STRING(cbkmf.bank) + " - " + cbkmf.descrip.
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
   wsTitle = wsTitle + STRING(st-per) + " TO " + STRING(end-per).
   wsTitle2 = "BANK: " + STRING(cbkmf.bank) + " - " + cbkmf.descrip.
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
wsFile = simctr.repDir + "cbrpt07" 
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
    FOR EACH CBTrans WHERE CBTrans.bank = wsAcc AND CBTrans.Accper >= st-Per:
        wsAmt = wsAmt - CBTrans.amount.
    END.
    IF wsBut = "Export" THEN DO:
        EXPORT STREAM b DELIMITER ',' wsTitle.
        EXPORT STREAM b DELIMITER ',' wsTitle2.
        EXPORT STREAM b DELIMITER ',' wsTitle3.
        EXPORT STREAM b DELIMITER ',' "PERIOD" "DATE" "REFERENCE" "ALLOCATION" "DESCRIPTION" "AMOUNT" "BALANCE".
        EXPORT STREAM b DELIMITER ',' "" "" "" "" "BALANCE B/F"  "" wsAmt.
        FOR EACH CBTrans WHERE CBTrans.bank = wsAcc AND CBTrans.Accper >= st-Per 
                      AND CBTrans.Accper <= end-Per:
            wsAmt = wsAmt + CBTrans.amount.
           EXPORT STREAM b DELIMITER ',' CBTrans.Accper CBTrans.trDate CBTrans.Ref CBTrans.Ledger CBTrans.Descrip 
                                              CBTrans.amount  wsAmt.
              
        END.
    END.
    ELSE DO:
        DISPLAY STREAM a "BALANCE B/F" @ CBTrans.Descrip wsAmt  WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        wsInc = 0.
        wsExp = 0.
        wsAmt1 = 0.
        /* Compute group totals and print (IE) */
        FOR EACH CBTrans WHERE CBTrans.bank = wsAcc AND CBTrans.Accper >= st-Per 
                          AND CBTrans.Accper <= end-Per:
                ASSIGN wsInc = wsInc + cbTrans.amount WHEN  cbTrans.amount > 0
                       wsExp = wsExp + cbTrans.amount WHEN  cbTrans.amount < 0.
        END.
        
        wsAmt1 = wsAmt + wsInc + wsExp.
       
        DISPLAY STREAM a "Income" @ CBTrans.Descrip  wsInc @ wsAmt WITH FRAME frm-rpt.
                DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a "Expenditure" @ CBTrans.Descrip  wsExp @ wsAmt WITH FRAME frm-rpt.
                DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a "---------------" @ wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a "BALANCE C/F" @ CBTrans.Descrip wsAmt1 @ wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a "---------------" @ wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a " " @ CBTrans.Descrip WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a "BALANCE B/F" @ CBTrans.Descrip wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
         DISPLAY STREAM a "INCOME" @ CBTrans.Descrip WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
    /* Print Income Transactions */
        FOR EACH CBTrans WHERE CBTrans.bank = wsAcc AND CBTrans.Accper >= st-Per 
                          AND CBTrans.Accper <= end-Per AND cbtrans.amount > 0:
            wsAmt = wsAmt + CBTrans.amount.
            DISPLAY STREAM a CBTrans.Accper CBTrans.trDate CBTrans.Ref CBTrans.Descrip  CBTrans.amount
                 CBTrans.Ledger wsAmt WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt. 
        END.
        DISPLAY STREAM a "---------------" @ wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
         DISPLAY STREAM a "TOTAL" @ CBTrans.Descrip wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
         DISPLAY STREAM a "LESS EXPENDITURE" @ CBTrans.Descrip  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
       /* Print Expense Transactions */
       FOR EACH CBTrans WHERE CBTrans.bank = wsAcc AND CBTrans.Accper >= st-Per 
                          AND CBTrans.Accper <= end-Per AND cbtrans.amount < 0:
            wsAmt = wsAmt + CBTrans.amount.
            DISPLAY STREAM a CBTrans.Accper CBTrans.trDate CBTrans.Ref CBTrans.Descrip  CBTrans.amount
                 CBTrans.Ledger wsAmt WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt. 
        END.
        DISPLAY STREAM a "---------------" @ wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
         DISPLAY STREAM a "Balance C/F" @ CBTrans.Descrip wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
         DISPLAY STREAM a "---------------" @ wsAmt  WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
    
    APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
END.

