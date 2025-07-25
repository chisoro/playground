/* Program.................Stkgrn.p
   Notes:................. Stock good received note
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE wsMsg           "Stock item does not exist"
&SCOPED-DEFINE wsTitle           " Stock-take Capture"
&SCOPED-DEFINE tmptable             Stkgrn
&SCOPED-DEFINE skey                stkgrn.intGrn
&SCOPED-DEFINE pgorientation     "LANDSCAPE"

{varlibrary.i}
DEF VAR w-orientation AS CHAR.
DEF STREAM a.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId      LIKE dbgl.TransID.
DEF VAR wsTime      AS CHAR FORM "x(8)".
DEF VAR wsper      LIKE simctr.curper.
DEF VAR wsyear     AS INT.
DEF VAR wsmonth     AS INT.
DEF VAR wsDate     LIKE stkbctr.BDate.
DEF VAR wsgrn      LIKE stkgrn.intGrn.
DEF VAR X AS INT.
DEF VAR wsNew      AS LOGICAL INITIAL YES.
DEF VAR wsQty AS DEC FORM "zzzzzz9.99-".
DEF VAR wsWhse LIKE stkwhf.WareHse.
DEF VAR wsDescrip LIKE stktrans.Descrip.
DEF VAR wsTitle AS CHAR FORM "x(80)".

DEF BUTTON btnAccept  LABEL "ACCEPT".
DEF BUTTON btn-whse    LABEL "WAREHOUSE".
DEF BUTTON btnStk     LABEL "STOCK".
DEF BUTTON btnOrder   LABEL "ORDER".

DEF TEMP-TABLE bfrData LIKE stkgrn.
DEF BUFFER tmpStock FOR stkTrans.

FIND FIRST SIMCTR NO-LOCK NO-ERROR.
wstitle = SIMCTR.CONAME.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.

DEFINE RECTANGLE rect1
         EDGE-PIXELS 2 graphic-edge  NO-FILL
         SIZE 98 BY 2.3.

DEFINE RECTANGLE rect2
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
         SIZE 98 BY 20.5.


DEF    QUERY qry-PickWhse FOR stkwhf SCROLLING.
DEF BROWSE brw-PickWhse QUERY qry-PickWhse
    DISPLAY stkwhf.WareHse stkwhf.Descrip   
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickWhse 
    brw-PickWhse AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-Exit colon 60
    with view-as dialog-box KEEP-TAB-ORDER no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ware House Selection".

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-exit colon 60
    with view-as dialog-box KEEP-TAB-ORDER no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".

DEF    QUERY qry-Data FOR bfrData SCROLLING.
DEF BROWSE brw-Data QUERY qry-Data
    DISPLAY bfrData.intSeq bfrData.WareHse 
    bfrData.stkCode  wsDescrip  bfrData.decQty bfrData.decAmt WITH 13 DOWN SEPARATORS.

DEF    QUERY qry-PickStock FOR stkmf SCROLLING.
DEF BROWSE brw-PickStock QUERY qry-PickStock
    DISPLAY stkmf.WareHse stkmf.stkCode stkmf.Descrip   
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickStock 
    brw-PickStock AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-Exit colon 60
    with view-as dialog-box KEEP-TAB-ORDER no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Stock Selection".

DEFINE FRAME frm-input
    SKIP(0.5)
      bfrData.intGrn      COLON 80 LABEL "GRN Number" NO-TAB-STOP /*VIEW-AS TEXT */ SKIP(0.5)
      wsper       COLON 20 LABEL "Accounting Period" SPACE(13)
      wsDATE                LABEL "Transaction Date " SKIP(0.5)
      btnOrder   COLON 10 NO-LABEL NO-TAB-STOP bfrData.intOrder NO-LABEL space(10)
      bfrData.invoice LABEL "Invoice Nmuber" HELP "Enter COD if its a COD Transaction" SKIP(0.5) 
      btn-whse     COLON 4.5 NO-TAB-STOP wsWhse NO-LABEL
      stkwhf.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
      BtnStk      COLON 11  NO-TAB-STOP bfrData.Stkcode NO-LABEL 
      stkmf.descrip  NO-LABEL VIEW-AS TEXT  SKIP(0.5)
      bfrData.decQty  COLON 21  LABEL "Quantity "  SPACE(10)
      bfrData.decAmt            LABEL "Value "     SKIP(0.5)
      brw-Data    AT ROW 10.5 COL 5 NO-TAB-STOP
      btnAccept    AT ROW 22 COL 10 space(25) btn-Del NO-TAB-STOP SPACE(25) btn-Exit   LABEL "CLOSE" NO-TAB-STOP  
      rect2 AT ROW 1 COL 3
      rect1 AT ROW 21.6 COL 3 
    with view-as dialog-box NO-VALIDATE SIZE 105 BY 25
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "GOODS RECEIVED DATA CAPTURE".

FORM SPACE(10)
     bfrData.WareHse  COLUMN-LABEL "WAREHOUSE"
     bfrData.period   COLUMN-LABEL "PERIOD"
     bfrData.trDate   COLUMN-LABEL "DATE"
     bfrData.Stkcode  COLUMN-LABEL "STOCK"
     stkmf.descrip           COLUMN-LABEL "DESCRIPTION"
     bfrData.decQty  COLUMN-LABEL "QUANTITY"
     bfrData.decAmt   COLUMN-LABEL "AMOUNT"
     HEADER skip(3) wstitle AT 20               SKIP(1) 
         "GOODS RECEIVED NOTE   " AT 40 "GRN NUMBER:" AT 90 bfrData.intGrn SKIP(1)
         "ORDER NUMBER:" AT 88 bfrData.intoRDER SKIP(1)
         "INVOICE NUMBER:" AT 86 bfrData.INVOICE SKIP(2)
        "Captured by:" AT 20 simusr.Name SPACE(30) " DATE: " TODAY SKIP(2)
    WITH DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

/******* Triggers ***** */
ON 'tab':U OF wsper IN FRAME frm-input
    OR 'leave':U OF wsper IN FRAME frm-input
DO:
    ASSIGN wsPer = DEC(wsPer:SCREEN-VALUE).
    IF wsPer <= SIMCTR.CLOSEPER THEN DO:
        BELL.
        MESSAGE "Period entered has been closed" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF wsPer > SIMCTR.curper THEN DO:
        BELL.
        MESSAGE "Period entered is outside the open accounting periods" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND LAST stkgrn WHERE SUBSTR(string(stkgrn.period),1,4) = SUBSTR(string(wsper),1,4) NO-LOCK NO-ERROR.
        IF AVAILABLE stkgrn THEN
            wsgrn = stkgrn.intGrn + 1.
        ELSE IF NOT AVAILABLE stkgrn THEN DO:
            wsgrn = INT(STRING("1")).
            wsgrn = INT(SUBSTR(string(wsper),1,4) + STRING(wsgrn,"999999")).
        END.
        DISPLAY wsgrn @  bfrData.intGrn WITH FRAME frm-Input.
    END.
    RETURN.
END.

ON 'tab':U OF wsDate IN FRAME frm-input
   OR 'leave':U OF wsDate IN FRAME frm-input
DO:
    ASSIGN wsDate = DATE(wsDate:SCREEN-VALUE).
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > wsper
         THEN DO:
        BELL.
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF INT(YEAR(wsDate)) <> INT(SUBSTR(STRING(wsper),1,4)) OR INT(MONTH(wsDate)) <> INT(SUBSTR(STRING(wsper),5,2)) 
        THEN DO:
        BELL.
        MESSAGE "Your date is outside the Accounting Period...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-whse IN FRAME  frm-input
DO:
  VIEW FRAME frm-Pickwhse.
  OPEN QUERY qry-Pickwhse FOR EACH stkwhf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Pickwhse.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-Pickwhse 
          OR close of THIS-PROCEDURE IN FRAME frm-Pickwhse 
          OR CHOOSE OF btn-ok IN FRAME frm-PickWhse 
          OR 'enter':u OF brw-Pickwhse
          OR 'mouse-select-dblclick' OF brw-Pickwhse.
  CLOSE QUERY qry-Pickwhse.
  HIDE FRAME frm-Pickwhse.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsWhse IN FRAME frm-input .
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Pickwhse 
    OR 'enter':u OF brw-Pickwhse 
    OR 'mouse-select-dblclick' OF brw-Pickwhse 
DO: 
   GET CURRENT qry-Pickwhse  EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY  stkwhf.WareHse  @ wsWhse stkwhf.Descrip WITH FRAME frm-input.
   RETURN.
END.

ON 'enter' OF wsWhse IN FRAME frm-input 
    OR 'tab' OF wsWhse IN FRAME frm-input
DO:
    ASSIGN wsWhse = INT(wsWhse:SCREEN-VALUE)
           wsPer  = DEC(wsPer:SCREEN-VALUE)
           wsDate = DATE(wsDate:SCREEN-VALUE).
    IF  wsWhse:SCREEN-VALUE = "" THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST stkwhf WHERE stkwhf.WareHse =  INT(wsWhse:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stkwhf THEN DO:
              MESSAGE "Warehouse does not exist" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        DISPLAY stkwhf.Descrip WITH FRAME frm-input.
        OPEN QUERY qry-data FOR EACH  bfrData WHERE  bfrData.WareHse = INT(wsWhse:SCREEN-VALUE)
             AND bfrData.period  = DEC(wsPer:SCREEN-VALUE) AND bfrData.UID = varUser.
        DISABLE wsper wsDate wsWhse WITH FRAME frm-input.
    END.
END.

ON CHOOSE OF BtnStk IN FRAME  frm-input
DO:
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
  APPLY 'tab' TO bfrData.stkcode IN FRAME frm-input .
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickStock 
    OR 'enter':u OF brw-PickStock 
    OR 'mouse-select-dblclick' OF brw-PickStock 
DO: 
   GET CURRENT qry-PickStock  EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY  stkmf.Stkcode  @ bfrData.Stkcode stkmf.Descrip WITH FRAME frm-input.
   RETURN.
END.

ON 'enter' OF  bfrData.Stkcode IN FRAME frm-input 
    OR 'tab' OF  bfrData.Stkcode IN FRAME frm-input
DO:
    IF  bfrData.Stkcode:SCREEN-VALUE = "" THEN
    DO:
        BELL.
        MESSAGE "Invalid Stock code entered, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST stkmf WHERE stkmf.Stkcode =  bfrData.Stkcode:SCREEN-VALUE 
                           AND stkmf.WareHse = INT(wsWhse:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stkmf THEN DO:
              MESSAGE "Stock does not exist" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY stkmf.Descrip WITH FRAME frm-input.
            FIND FIRST bfrData WHERE bfrData.stkCode = bfrData.Stkcode:SCREEN-VALUE
                        AND bfrData.period = DEC(wsPer:SCREEN-VALUE)
                        AND bfrData.WareHse = INT(wsWhse:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF AVAILABLE bfrData THEN DO:
                MESSAGE "Stock take data for this stock has already been captured" VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
        END.
    END.
END.

ON 'enter' OF  bfrData.decAmt  IN FRAME frm-input 
    OR 'tab' OF bfrData.decAmt  IN FRAME frm-input
DO:
    IF  DEC(bfrData.decAmt:SCREEN-VALUE) <= 0 THEN DO:
        BELL.
        MESSAGE "Amount cannot be zero or less" VIEW-AS ALERT-BOX.
        ASSIGN bfrData.Stkcode:SCREEN-VALUE = ""
               bfrData.decQty:SCREEN-VALUE = "0.00"
               bfrData.decAmt:SCREEN-VALUE = "0.00".
         APPLY 'entry' TO bfrData.Stkcode IN FRAME frm-input.
         RETURN NO-APPLY.
    END.
    ELSE IF DEC(bfrData.decAmt:SCREEN-VALUE) > 0 THEN DO:
        IF wsNew = YES THEN DO:
             X = X + 1.
             CREATE bfrData.
             ASSIGN  bfrData.intGrn  = wsgrn
                     bfrData.intOrder = INT(bfrData.intOrder:SCREEN-VALUE)
                     bfrData.intSeq  = X
                     bfrData.Invoice = bfrData.Invoice:SCREEN-VALUE
                     bfrData.WareHse = INT(wsWhse:SCREEN-VALUE)
                     bfrData.stkCode = bfrData.Stkcode:SCREEN-VALUE
                     bfrData.period  = DEC(wsPer:SCREEN-VALUE)
                     bfrData.trDate  = DATE(wsDate:SCREEN-VALUE)
                     bfrData.PoDate  = TODAY
                     bfrData.UID     = varUser
                     bfrData.decQty = DEC(bfrData.decQty:SCREEN-VALUE)
                     bfrData.decAmt = DEC(bfrData.decAmt:SCREEN-VALUE).
             OPEN QUERY qry-data FOR EACH  bfrData.
             ASSIGN bfrData.Stkcode:SCREEN-VALUE = ""
                    bfrData.decQty:SCREEN-VALUE = "0.00"
                    bfrData.decAmt:SCREEN-VALUE = "0.00".
             APPLY 'entry' TO bfrData.Stkcode IN FRAME frm-input.
             RETURN NO-APPLY.
        END.
        ELSE DO:
           DISPLAY bfrData.stkCode bfrData.decQty bfrData.decAmt WITH BROWSE brw-Data.
           ASSIGN bfrData.Stkcode:SCREEN-VALUE = ""
                    bfrData.decQty:SCREEN-VALUE = "0.00"
                    bfrData.decAmt:SCREEN-VALUE = "0.00".
             APPLY 'entry' TO bfrData.Stkcode IN FRAME frm-input.
             wsNew = YES.
             RETURN NO-APPLY.
        END.   
    END.
END.

ON 'mouse-select-dblclick':U OF  brw-Data
DO:  
    wsNew = NO.
    GET CURRENT qry-Data  EXCLUSIVE-LOCK NO-WAIT.
    DISPLAY bfrData.stkCode bfrData.decQty bfrData.decAmt WITH FRAME frm-Input.
    APPLY 'entry' TO bfrData.Stkcode IN FRAME frm-input.
    RETURN NO-APPLY.
END.

ON CHOOSE OF btn-Del IN FRAME frm-Input DO:
    GET CURRENT qry-Data EXCLUSIVE-LOCK NO-WAIT.
    DELETE bfrData. 
    Method-Status = brw-Data:DELETE-SELECTED-ROWS().
    APPLY 'entry':u TO bfrData.Stkcode.
END.

ON 'choose':U OF btnAccept IN FRAME frm-Input 
DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="rpt-ip"
                    &paged}
    CLEAR FRAME frm-Input.
    ASSIGN wsDate:SCREEN-VALUE = STRING(TODAY)
           wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
           bfrData.decQty:SCREEN-VALUE = "0.00"
           bfrData.decAmt:SCREEN-VALUE = "0.00"
           wsWhse:SCREEN-VALUE = "1".
    APPLY 'entry':u TO wsPer.
    RETURN.
END.

/********** MAIN LOGIC **********/
wsTime    = STRING(TIME,"HH:MM:SS").
wsTransID = DEC (string(YEAR(TODAY),"9999")
          + string(MONTH(TODAY),"99" )
          + string(DAY(TODAY),"99")
          + SUBSTR(STRING(wsTIME),1,2) 
          + SUBSTR(STRING(wsTIME),4,2) 
          + SUBSTR(STRING(wsTIME),7,2) ).
CLEAR FRAME frm-Input.
ENABLE ALL WITH FRAME frm-Input.
ASSIGN wsDate:SCREEN-VALUE = STRING(TODAY)
       wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       bfrData.decQty:SCREEN-VALUE = "0.00"
       bfrData.decAmt:SCREEN-VALUE = "0.00"
       wsWhse:SCREEN-VALUE = "1".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Input.
HIDE FRAME frm-main.



PROCEDURE rpt-ip:
    FOR EACH  bfrData:
        CREATE bfr{&tmptable}. /* Create GRN transaction */
        BUFFER-COPY bfrData TO bfr{&tmptable}.
        IF bfr{&tmptable}.invoice = "COD" THEN
            ASSIGN bfr{&tmptable}.invoiced = YES
                   bfr{&tmptable}.invDate  = TODAY.
        CREATE tmpStock. /* Create Stores transaction */
        ASSIGN tmpStock.stkCode = bfrData.stkcode
             tmpStock.period  = bfrData.period
             tmpStock.PoDate  = TODAY
             tmpStock.Quantity = bfrData.decQty
             tmpStock.Cost     = bfrData.decAmt
             tmpStock.Descrip  = "Stock Receipt"
             tmpStock.Ledger   = 0
             tmpStock.prog     = "stkgrn.p"
             tmpStock.Ref      = "GRN/" + STRING(bfrData.intGrn)
             tmpStock.TransID  = wsTransId
             tmpStock.TranType = 1
             tmpStock.trDate   = bfrData.trDate
             tmpStock.UID      = varUser
             tmpStock.UnitPrice = ROUND((bfrData.decAmt / bfrData.decQty),2).
        FIND FIRST stkmf WHERE stkmf.stkcode = bfrData.stkcode 
                           AND stkmf.warehse = bfrData.warehse EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       ASSIGN Stkmf.Quantity[2] = bfrData.decQty /* Update Master bin card */
              Stkmf.cost[2]     = bfrData.decAmt
              stkmf.UnitPrice   =  ROUND(((Stkmf.cost[1] + Stkmf.cost[2]) / (Stkmf.Quantity[1] 
                                           + Stkmf.Quantity[2])),2)
              stkmf.Cost[1]     = stkmf.Cost[1] + bfrData.decAmt
              Stkmf.Quantity[1] = Stkmf.Quantity[1]+ bfrData.decQty.
        DISPLAY STREAM a bfrData.WareHse bfrData.period bfrData.trDate bfrData.Stkcode
                         stkmf.descrip bfrData.decQty bfrData.decAmt WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        DELETE bfrData.
    END.
    RETURN.
END.
    
    
