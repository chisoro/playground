/* Program.................stkrpt02.p
   Notes:................. Stock Movement Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 6.0          NO-UNDO.
DEF VAR wsBal AS DEC EXTENT 2.
DEF VAR wsdesc LIKE stkmf.Descrip.
DEF VAR st-per LIKE stkTrans.period.
DEF VAR end-per LIKE stkTrans.period.
DEF VAR st-stock LIKE stkmf.stkcode.
DEF VAR end-stock LIKE stkmf.stkcode.
DEF VAR st-whse LIKE stkmf.WareHse.
DEF VAR end-whse LIKE stkmf.WareHse.
DEF VAR wsStatus LIKE stkmf.stkcode.
DEF VAR wsDes    LIKE stkmf.descrip EXTENT 4.
DEF VAR wsOpt AS CHAR FORM "x(7)".
DEF VAR wsTitle AS CHAR FORM "x(80)".

DEF BUTTON Btn-Print   LABEL "Print".
DEF BUTTON btn-Ok    LABEL "Ok".
DEF BUTTON btn-Exit    LABEL "Close".
DEF BUTTON BtnStk1     LABEL "From Stock".
DEF BUTTON BtnStk2     LABEL "To Stock".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 12.5.

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
       st-per    COLON 26.1 LABEL "Start Period" AUTO-RETURN SKIP(0.5)
       end-per   COLON 26.1 LABEL "End Period"   AUTO-RETURN SKIP(0.5)
       st-wHse   COLON 26.1 LABEL "From Warehouse" wsdes[1] NO-LABEL VIEW-AS TEXT SKIP(0.5)
       end-wHse  COLON 26.1 LABEL  "To Warehouse" wsdes[2] NO-LABEL VIEW-AS TEXT SKIP(0.5)
       BtnStk1   COLON 13.5   NO-TAB-STOP st-stock  NO-LABEL wsdes[3] NO-LABEL VIEW-AS TEXT SKIP(0.5)
       BtnStk2   COLON 15.5 NO-TAB-STOP end-stock NO-LABEL wsdes[4] NO-LABEL VIEW-AS TEXT SKIP(1)
       wsStatus  COLON 30 LABEL "Processing stock ....." VIEW-AS TEXT SKIP(2.5)
       btn-Print colon 10 
       btn-exit colon 60 
       rect-2 AT ROW 1.4 COL 3
       rect-1 AT ROW 14 COL 3    
    WITH VIEW-AS DIALOG-BOX NO-VALIDATE SIZE 85 BY 17
             SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
       TITLE "STORES STOCK MOVEMENT REPORT".

FORM "     " wsDesc            COLUMN-LABEL "STOCK    DATE/REFERENCE" FORM "X(35)"
    stkTrans.Descrip   COLUMN-LABEL "NARRATION" FORM "X(25)"
    stkTrans.Quantity  COLUMN-LABEL "TRANSCTION! QUANTITY"
    stkTrans.Cost      COLUMN-LABEL "TRANSACTION! COST"
    wsBal[1]           COLUMN-LABEL "BALANCE! QUANTITY" FORM "zzzzz9.99-"
    wsBal[2]           COLUMN-LABEL "BALANCE!($)" FORM "zzzzzz9.99-" 
    stkTrans.Ledger    COLUMN-LABEL "ALLOCATION"
    HEADER skip(3) wsTitle AT 20 SKIP(1)
    "STORES STOCK MOVEMENT REPORT FROM PERIOD: " AT 10
    ST-PER "TO" end-per "PRINTED TODAY: " TODAY "Page: " AT 120 PAGE-NUMBER(a) SKIP(2)
    WITH DOWN STREAM-IO FONT 10 WIDTH 264 CENTERED NO-BOX FRAME frm-rpt.

ON 'tab':U OF st-wHse IN FRAME frm-input
    OR 'enter' OF st-wHse IN FRAME frm-input
DO:
    FIND FIRST stkwhf WHERE stkwhf.Warehse = INT(st-wHse:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE stkwhf THEN
        DISPLAY stkwhf.Descrip @ wsdes[1] WITH FRAME frm-input.
    RETURN.
END.

ON 'tab':U OF end-wHse IN FRAME frm-input
    OR 'enter' OF end-wHse IN FRAME frm-input
DO:
    FIND FIRST stkwhf WHERE stkwhf.Warehse = INT(end-wHse:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE stkwhf THEN
        DISPLAY stkwhf.Descrip @ wsdes[2] WITH FRAME frm-input.
    RETURN.
END.

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
  OPEN QUERY qry-PickStock FOR EACH stkmf WHERE stkmf.Warehse >= INT(st-whse:SCREEN-VALUE) 
                                    AND stkmf.Warehse <= INT(end-whse:SCREEN-VALUE) NO-LOCK.
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
  OPEN QUERY qry-PickStock FOR EACH stkmf WHERE stkmf.Warehse >= INT(st-whse:SCREEN-VALUE) 
                                    AND stkmf.Warehse <= INT(end-whse:SCREEN-VALUE) NO-LOCK.
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
    ASSIGN st-whse End-whse st-per end-per st-stock end-stock.
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
END.

                    
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
assign st-per:SCREEN-VALUE = string(SIMCTR.CURPER)
        st-whse:SCREEN-VALUE = "0"
        end-whse:SCREEN-VALUE = "999"
        st-stock:SCREEN-VALUE = "0"
        end-stock:SCREEN-VALUE = "ZZZZZZZZZ"
        wstitle = SIMCTR.CONAME.
ENABLE ALL WITH FRAME frm-input.
WAIT-FOR CHOOSE OF btn-Exit OR close of THIS-PROCEDURE IN FRAME frm-input.
HIDE FRAME frm-input.


PROCEDURE Report.ip:
FOR EACH stkwhf WHERE stkwhf.WareHse >= st-whse AND stkwhf.WareHse <= end-whse NO-LOCK:
    FOR EACH stkmf WHERE stkmf.Warehse = stkwhf.WareHse AND
                         stkmf.stkcode >= st-stock AND stkmf.stkcode <= end-stock NO-LOCK:
        DISPLAY stkmf.stkcode @ wsStatus WITH FRAME frm-input.
        PAUSE 0.
        ASSIGN wsbal[1] = 0
               wsbal[2] = 0
               wsDesc   = stkmf.stkCode + " " + stkmf.Descrip.
        
        FOR EACH bfrStkTrans WHERE bfrStkTrans.stkCode = stkmf.stkcode
                               AND bfrStkTrans.period < st-Per:
            ASSIGN wsbal[1] = wsBal[1] + bfrStkTrans.Quantity
                   wsbal[2] = wsBal[2] + bfrStkTrans.cost.
        END.
        FIND FIRST bfrStkTrans WHERE bfrStkTrans.stkCode = stkmf.stkcode
                               AND bfrStkTrans.period >= st-Per NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfrStkTrans AND wsbal[1] = 0 THEN NEXT.
        DISPLAY STREAM a wsdesc  "Balance B/F" @ stkTrans.Descrip wsBal[1] wsBal[2] WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        FOR EACH stkTrans  WHERE StkTrans.stkCode = stkmf.stkcode AND StkTrans.period >= st-Per  
               AND StkTrans.period <= end-Per NO-LOCK BY StkTrans.TransID:
            wsDesc = "     " + STRING(stkTrans.period) + " " + STRING(stkTrans.trDate) + " " + stkTrans.Ref.
            ASSIGN wsbal[1] = wsBal[1] + StkTrans.Quantity
                   wsbal[2] = wsBal[2] + StkTrans.cost.
            DISPLAY STREAM a wsDesc stkTrans.Descrip stkTrans.Quantity stkTrans.Cost 
                stkTrans.Ledger wsBal[1] wsBal[2] WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        UNDERLINE STREAM a wsBal[1] wsBal[2] WITH FRAME frm-rpt.
        DISPLAY STREAM a "BALANCE C/D" @ stkTrans.Descrip wsBal[1] wsBal[2] WITH FRAME frm-rpt.
        DOWN 2 STREAM a WITH FRAME frm-rpt.
    END.
END.
APPLY 'close' TO THIS-PROCEDURE.
END.
