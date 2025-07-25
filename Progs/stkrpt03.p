/* Program.................stkrpt03.p
   Notes:................. Stock Usage Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF VAR wsBal AS DEC EXTENT 2.
DEF VAR wsdesc LIKE stkmf.Descrip FORM "X(60)".
DEF VAR st-per LIKE stkTrans.period.
DEF VAR end-per LIKE stkTrans.period.
DEF VAR st-stock LIKE stkmf.stkcode.
DEF VAR end-stock LIKE stkmf.stkcode.
DEF VAR wsStatus LIKE stkmf.stkcode.
DEF VAR wsDes    LIKE stkmf.descrip EXTENT 4.
DEF VAR wsOpt AS CHAR FORM "x(7)".
DEF VAR wsChoice AS LOGICAL INITIAL NO.
DEF VAR wsTitle AS CHAR FORM "x(80)".

DEF BUTTON Btn-Print   LABEL "Print".
DEF BUTTON btn-Ok    LABEL "Ok".
DEF BUTTON btn-Exit    LABEL "Close".
DEF BUTTON BtnStk1     LABEL "Start from Stock".
DEF BUTTON BtnStk2     LABEL "To Stock".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 10.5.

DEF BUFFER bfrStkTrans FOR StkTrans.

DEF    QUERY qry-PickStock FOR stkmf SCROLLING.
    DEF BROWSE brw-PickStock QUERY qry-PickStock
        DISPLAY stkmf.WareHse stkmf.stkCode stkmf.Descrip   
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickStock 
        brw-PickStock AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-Exit colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Stock Selection".

DEFINE FRAME frm-input
    SKIP(1)
       st-per    COLON 26.1 LABEL "Enter Start Period" AUTO-RETURN SKIP(0.5)
       end-per   COLON 26.1 LABEL "Enter End Period"   AUTO-RETURN SKIP(0.5)
       BtnStk1   COLON 10   NO-TAB-STOP st-stock  NO-LABEL wsdes[3] NO-LABEL VIEW-AS TEXT SKIP(0.5)
       BtnStk2   COLON 15.5 NO-TAB-STOP end-stock NO-LABEL wsdes[4] NO-LABEL VIEW-AS TEXT SKIP(.5)
       wsChoice  COLON 26.1 AUTO-RETURN LABEL "Detailed Report (Y/N)" SKIP(1)
       wsStatus  COLON 30 LABEL "Processing stock ....." VIEW-AS TEXT SKIP(2)
       btn-Print colon 10 
       btn-exit colon 60 
       rect-2 AT ROW 1.4 COL 3
       rect-1 AT ROW 12 COL 3    
    WITH VIEW-AS DIALOG-BOX NO-VALIDATE SIZE 85 BY 15
             SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
       TITLE "STORES STOCK MOVEMENT REPORT".

FORM "     " wsDesc            LABEL "STOCK ITEM" FORM "X(60)"
    wsBal[1]           LABEL "QUANTIIY" FORM "zzzzz9.99-"
    wsBal[2]           LABEL "COST" FORM "z,zzz,zz9.99-" 
    HEADER skip(1) wstitle AT 20 SKIP(1)
    "STOCK USAGE REPORT FOR THE PERIOD " AT 10
    ST-PER  "TO" end-per "Page: " AT 75 PAGE-NUMBER(a) SKIP(1)"PRINTED TODAY: " AT 30 TODAY SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

ON 'tab':U OF st-stock IN FRAME frm-input
    OR 'enter' OF st-stock IN FRAME frm-input
DO:
    FIND FIRST stkmf WHERE stkmf.stkCode = st-stock:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE stkmf THEN
        DISPLAY stkmf.Descrip @ wsdes[3] WITH FRAME frm-input.
    RETURN.
END.

ON CHOOSE OF BtnStk1 IN FRAME  frm-input
DO:
    wsOpt = "BtnStk1".
  VIEW FRAME frm-PickStock.
  OPEN QUERY qry-PickStock FOR EACH stkmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickStock.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-PickStock 
          OR close of THIS-PROCEDURE IN FRAME frm-PickStock 
          OR CHOOSE OF btn-ok IN FRAME frm-PickStock 
          OR 'enter':u OF brw-PickStock
          OR 'mouse-select-dblclick' OF brw-PickStock.
  CLOSE QUERY qry-PickStock.
  HIDE FRAME frm-PickStock.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF BtnStk2 IN FRAME  frm-input
DO:
    wsOpt = "BtnStk2".
  VIEW FRAME frm-PickStock.
  OPEN QUERY qry-PickStock FOR EACH stkmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickStock.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-PickStock 
          OR close of THIS-PROCEDURE IN FRAME frm-PickStock 
          OR CHOOSE OF btn-ok IN FRAME frm-PickStock 
          OR 'enter':u OF brw-PickStock
          OR 'mouse-select-dblclick' OF brw-PickStock.
  CLOSE QUERY qry-PickStock.
  HIDE FRAME frm-PickStock.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickStock 
    OR 'enter':u OF brw-PickStock 
    OR 'mouse-select-dblclick' OF brw-PickStock 
DO: 
   GET CURRENT qry-PickStock  EXCLUSIVE-LOCK NO-WAIT.
   IF wsopt = "BtnStk1" THEN
        DISPLAY  stkmf.Stkcode  @ st-stock stkmf.Descrip @ wsdes[3] WITH FRAME frm-input.
   IF wsopt = "BtnStk2" THEN
        DISPLAY  stkmf.Stkcode  @ end-stock stkmf.Descrip @ wsdes[4] WITH FRAME frm-input.
   RETURN.
END.

ON 'tab':U OF end-stock IN FRAME frm-input
    OR 'enter' OF end-stock IN FRAME frm-input
DO:
    FIND FIRST stkmf WHERE stkmf.stkCode = end-stock:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE stkmf THEN
        DISPLAY stkmf.Descrip @ wsdes[4] WITH FRAME frm-input.
    RETURN.
END.

ON 'choose':U OF btn-Print IN FRAME frm-input 
DO:
    ASSIGN st-per end-per st-stock end-stock wsChoice.
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
END.

                    
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
assign st-per:SCREEN-VALUE = SUBSTR(STRING(SIMCTR.CURPER),1,4) + "01"
       end-per:SCREEN-VALUE = string(SIMCTR.CURPER)
       st-stock:SCREEN-VALUE = "0"
       end-stock:SCREEN-VALUE = "ZZZZZZZZZ"
       wsChoice:SCREEN-VALUE = "NO"
       wstitle = SIMCTR.CONAME.
ENABLE ALL WITH FRAME frm-input.
WAIT-FOR CHOOSE OF btn-Exit OR close of THIS-PROCEDURE IN FRAME frm-input.
HIDE FRAME frm-input.


PROCEDURE Report.ip:
FOR EACH stkmf WHERE stkmf.stkcode >= st-stock AND stkmf.stkcode <= end-stock NO-LOCK:
        DISPLAY stkmf.stkcode @ wsStatus WITH FRAME frm-input.
        PAUSE 0.
        FIND FIRST bfrStkTrans WHERE bfrStkTrans.stkCode = stkmf.stkcode
                               AND bfrStkTrans.period >= st-Per 
                               AND (bfrstktrans.TranType = 2 OR bfrstktrans.TranType = 3) NO-LOCK NO-ERROR.
        IF AVAILABLE bfrStkTrans THEN DO:
            ASSIGN wsbal[1] = 0
                   wsbal[2] = 0
                   wsDesc   = stkmf.stkCode + " " + stkmf.Descrip.
            DISPLAY STREAM a wsdesc  WITH FRAME frm-rpt.
            DOWN 2 STREAM a WITH FRAME frm-rpt.
            FOR EACH stkTrans WHERE stktrans.TranType <> 1 AND StkTrans.stkCode = stkmf.stkcode 
                AND StkTrans.period >= st-Per AND StkTrans.period <= end-Per NO-LOCK 
                  BREAK BY stktrans.stkcode BY stktrans.Ledger:
                ASSIGN wsbal[1] = wsBal[1] + StkTrans.Quantity
                       wsbal[2] = wsBal[2] + StkTrans.cost.
               ACCUMULATE StkTrans.Quantity (TOTAL BY StkTrans.stkcode BY StkTrans.Ledger)
                          StkTrans.cost (TOTAL BY StkTrans.stkcode BY stktrans.Ledger ).
               IF wsChoice = YES THEN DO:
                   IF FIRST-OF(stktrans.Ledger) THEN DO:
                        FIND FIRST glmf WHERE glmf.acct = stktrans.Ledger NO-LOCK NO-ERROR.
                        wsDesc   = "     " + STRING(stktrans.Ledger) + " " + glmf.DESCRIPTION.
                        DISPLAY STREAM a wsDesc WITH FRAME frm-rpt.
                        DOWN STREAM a WITH FRAME frm-rpt.
                   END.
                   wsDesc = "          " + STRING(stktrans.period) + " " 
                          + STRING(stktrans.trDate) + " " + stktrans.Ref.
                   DISPLAY STREAM a  wsDesc StkTrans.Quantity @ wsBal[1] StkTrans.cost @ wsBal[2]
                        WITH FRAME frm-rpt.
                   DOWN STREAM a WITH FRAME frm-rpt.
                   IF LAST-OF(stktrans.Ledger) THEN DO:
                        UNDERLINE STREAM a wsBal[1] wsBal[2] WITH FRAME frm-rpt.
                        DOWN STREAM a WITH FRAME frm-rpt.
                   END.
               END.
                IF LAST-OF(stktrans.Ledger) THEN DO:
                    FIND FIRST glmf WHERE glmf.acct = stktrans.Ledger NO-LOCK NO-ERROR.
                    ASSIGN wsDesc   = STRING(stktrans.Ledger) + " " + glmf.DESCRIPTION WHEN wsChoice = NO
                           wsDesc   = "     SUB-TOTAL" WHEN wsChoice = YES.
                    DISPLAY STREAM a wsDesc  (ACCUM TOTAL BY stktrans.Ledger StkTrans.Quantity) @ wsBal[1]
                              (ACCUM TOTAL BY stktrans.Ledger StkTrans.cost) @ wsBal[2] WITH FRAME frm-rpt.
                    DOWN STREAM a WITH FRAME frm-rpt.
                    IF wsChoice = YES THEN
                        DOWN 2 STREAM a WITH FRAME frm-rpt.
                END. 
                IF LAST-OF(stktrans.stkcode) AND (wsBal[1] <> 0 OR wsBal[2] <> 0) THEN DO:
                    UNDERLINE STREAM a wsDesc wsBal[1] wsBal[2] WITH FRAME frm-rpt.
                     DOWN STREAM a WITH FRAME frm-rpt.
                    DISPLAY STREAM a "TOTAL" @ wsDesc  (ACCUM TOTAL BY stktrans.Stkcode StkTrans.Quantity) @ wsBal[1]
                              (ACCUM TOTAL BY stktrans.Stkcode StkTrans.cost) @ wsBal[2] WITH FRAME frm-rpt.
                    DOWN STREAM a WITH FRAME frm-rpt.
                END.  
            END.
            DOWN 2 STREAM a WITH FRAME frm-rpt.
        END.
END.
APPLY 'close' TO THIS-PROCEDURE.
END.
