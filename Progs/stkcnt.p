/* Program.................Stkcnt.p
   Notes:................. Stock take capture
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE wsMsg           "Stock item does not exist"
&SCOPED-DEFINE wsTitle           " Stock-take Capture"
&SCOPED-DEFINE tmptable             Stkcnt
&SCOPED-DEFINE skey                stkmf.stkCode
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
DEF VAR wsStkCode  LIKE stkmf.stkCode.
DEF VAR X AS INT.
DEF VAR wsQuantity AS DEC FORM "zzzzzz9.99-".
DEF VAR wsRef  LIKE stktrans.Ref.
DEF VAR wsWhse LIKE stkwhf.WareHse.
DEF VAR wsDescrip LIKE stktrans.Descrip.
DEF VAR wsTitle AS CHAR FORM "x(80)".

DEF BUTTON btn-Update  LABEL "UPDATE".
DEF BUTTON btn-whse    LABEL "WAREHOUSE".
DEF BUTTON btnStk     LABEL "STOCK".

FIND FIRST SIMCTR NO-LOCK NO-ERROR.
wstitle = SIMCTR.CONAME.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.

DEFINE RECTANGLE rect1
         EDGE-PIXELS 2 graphic-edge  NO-FILL
         SIZE 98 BY 2.3.

DEFINE RECTANGLE rect2
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
         SIZE 98 BY 18.5.


DEF    QUERY qry-PickWhse FOR stkwhf SCROLLING.
DEF BROWSE brw-PickWhse QUERY qry-PickWhse
    DISPLAY stkwhf.WareHse stkwhf.Descrip   
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickWhse 
    brw-PickWhse AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-Exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ware House Selection".

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".

DEF    QUERY qry-Data FOR bfr{&tmpTable} SCROLLING.
DEF BROWSE brw-Data QUERY qry-Data
    DISPLAY bfr{&tmpTable}.period bfr{&tmpTable}.trDate bfr{&tmpTable}.WareHse 
    bfr{&tmpTable}.stkCode  wsDescrip  bfr{&tmpTable}.Quantity[1] WITH 13 DOWN SEPARATORS.

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
      wsper       COLON 20 LABEL "Accounting Period" SPACE(5)
      wsDATE                LABEL "Transaction Date " SKIP(0.5)
      btn-whse     COLON 6 NO-TAB-STOP wsWhse NO-LABEL
      stkwhf.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
      BtnStk      COLON 12  NO-TAB-STOP wsStkcode NO-LABEL 
      stkmf.descrip  NO-LABEL VIEW-AS TEXT  SKIP(0.5)
      wsQuantity  COLON 21  LABEL "Quantity "  SKIP(0.5)
      brw-Data    AT ROW 8 COL 5 NO-TAB-STOP
      btn-del     AT ROW 20 COL 21 NO-TAB-STOP SPACE(45) btn-Exit   NO-TAB-STOP  
      rect2 AT ROW 1 COL 3
      rect1 AT ROW 19.6 COL 3 
    with view-as dialog-box NO-VALIDATE SIZE 105 BY 23
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "STOCK-TAKE DATA CAPTURE".

FORM SPACE(10)
     bfr{&tmptable}.WareHse  COLUMN-LABEL "WAREHOUSE"
     bfr{&tmptable}.period   COLUMN-LABEL "PERIOD"
     bfr{&tmptable}.trDate   COLUMN-LABEL "DATE"
     bfr{&tmptable}.Stkcode  COLUMN-LABEL "STOCK"
     stkmf.descrip           COLUMN-LABEL "DESCRIPTION"
      bfr{&tmptable}.Quantity[1]COLUMN-LABEL "QUANTITY"
     HEADER skip(5) wstitle AT 20 SKIP(1)
       "STORES STOCK-TAKE CAPTURE REPORT   " AT 10 "DATE: " TODAY " PERIOD  :" wsPer  " Captured by:" 
        simusr.Name  SKIP(2)
    WITH DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.


/******* Triggers ***** */
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
        OPEN QUERY qry-data FOR EACH  bfr{&tmptable} WHERE  bfr{&tmptable}.WareHse = INT(wsWhse:SCREEN-VALUE)
             AND bfr{&tmptable}.period  = DEC(wsPer:SCREEN-VALUE) AND bfr{&tmptable}.UID = varUser.
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
  APPLY 'tab' TO wsStkcode IN FRAME frm-input .
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickStock 
    OR 'enter':u OF brw-PickStock 
    OR 'mouse-select-dblclick' OF brw-PickStock 
DO: 
   GET CURRENT qry-PickStock  EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY  stkmf.Stkcode  @ wsStkcode stkmf.Descrip WITH FRAME frm-input.
   RETURN.
END.

ON 'enter' OF  wsStkcode IN FRAME frm-input 
    OR 'tab' OF  wsStkcode IN FRAME frm-input
DO:
    IF  wsStkcode:SCREEN-VALUE = "" THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST stkmf WHERE stkmf.Stkcode =  wsStkcode:SCREEN-VALUE 
                           AND stkmf.WareHse = INT(wsWhse:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stkmf THEN DO:
              MESSAGE "Stock does not exist" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY stkmf.Descrip WITH FRAME frm-input.
            FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.stkCode = wsStkcode:SCREEN-VALUE
                        AND bfr{&tmptable}.period = DEC(wsPer:SCREEN-VALUE)
                        AND bfr{&tmptable}.WareHse = INT(wsWhse:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF AVAILABLE bfr{&tmptable} THEN DO:
                MESSAGE "Stock take data for this stock has already been captured" VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
        END.
    END.
END.

ON 'tab':U OF wsper IN FRAME frm-input
    OR 'leave':U OF wsper IN FRAME frm-input
DO:
    ASSIGN wsPer = DEC(wsPer:SCREEN-VALUE).
    IF wsPer <= SIMCTR.CLOSEPER THEN DO:
        MESSAGE "Period entered has been closed" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF wsPer > SIMCTR.curper THEN DO:
        BELL.
        MESSAGE "Period entered is outside the open accounting periods" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
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

ON 'enter' OF  wsQuantity IN FRAME frm-input 
    OR 'tab' OF  wsQuantity IN FRAME frm-input
DO:
    IF  DEC(wsQuantity:SCREEN-VALUE) <= 0 THEN DO:
        BELL.
        MESSAGE "No Need to capture stock quantity zero" VIEW-AS ALERT-BOX.
        ASSIGN wsStkcode:SCREEN-VALUE = ""
                    wsQuantity:SCREEN-VALUE = "0.00".
         APPLY 'entry' TO wsStkcode IN FRAME frm-input.
         RETURN NO-APPLY.
    END.
    IF DEC(wsQuantity:SCREEN-VALUE) > 0 THEN DO:
         CREATE bfr{&tmptable}.
         ASSIGN  bfr{&tmptable}.WareHse = INT(wsWhse:SCREEN-VALUE)
                 bfr{&tmptable}.stkCode = wsStkcode:SCREEN-VALUE
                 bfr{&tmptable}.period  = DEC(wsPer:SCREEN-VALUE)
                 bfr{&tmptable}.trDate  = DATE(wsDate:SCREEN-VALUE)
                 bfr{&tmptable}.PoDate  = TODAY
                 bfr{&tmptable}.UID     = varUser
                 bfr{&tmptable}.Quantity[1] = DEC(wsQuantity:SCREEN-VALUE)
                 bfr{&tmptable}.TransId = wsTransID.
         OPEN QUERY qry-data FOR EACH  bfr{&tmptable} WHERE  bfr{&tmptable}.WareHse = INT(wsWhse:SCREEN-VALUE)
             AND bfr{&tmptable}.period  = DEC(wsPer:SCREEN-VALUE) AND bfr{&tmptable}.UID = varUser.
             ASSIGN wsStkcode:SCREEN-VALUE = ""
                    wsQuantity:SCREEN-VALUE = "0.00".
         APPLY 'entry' TO wsStkcode IN FRAME frm-input.
         RETURN NO-APPLY.
    END.
END.

ON 'mouse-select-dblclick':U OF  brw-Data
DO:
    
    GET CURRENT qry-Data  EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST stkmf WHERE stkmf.Stkcode =  bfr{&tmptable}.stkCode 
                           AND stkmf.WareHse = bfr{&tmptable}.WareHse NO-LOCK NO-ERROR.
    DISABLE wsWhse wsPer wsDate wsStkcode WITH FRAME frm-Input.
    DISPLAY bfr{&tmptable}.stkCode @ wsStkcode bfr{&tmptable}.Quantity[1] @ wsQuantity WITH FRAME frm-Input.
    WAIT-FOR 'tab' OF wsQuantity IN FRAME frm-Input.
    DISPLAY bfr{&tmptable}.Quantity[1] WITH BROWSE brw-Data.
    ENABLE wsWhse wsPer wsDate wsStkcode WITH FRAME frm-Input.
    RETURN.
END.

ON CHOOSE OF btn-Del IN FRAME frm-Input DO:
    GET CURRENT qry-Data EXCLUSIVE-LOCK NO-WAIT.
    DELETE bfr{&tmptable}. 
    Method-Status = brw-Data:DELETE-SELECTED-ROWS().
    APPLY 'entry':u TO wsStkcode.
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
       wsQuantity:SCREEN-VALUE = "0.00".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Input.
HIDE FRAME frm-main.
{PrintOpt.i &stream-name="stream a"
                    &print-prog="rpt-ip"
                    &paged}


PROCEDURE rpt-ip:
    FOR EACH  bfr{&tmptable} WHERE  bfr{&tmptable}.WareHse = wsWhse AND bfr{&tmptable}.period  = wsPer 
                               AND bfr{&tmptable}.UID = varUser NO-LOCK:
        FIND FIRST stkmf WHERE stkmf.stkcode = bfr{&tmptable}.stkcode 
                           AND stkmf.warehse = bfr{&tmptable}.warehse NO-LOCK NO-ERROR. 
        DISPLAY STREAM a bfr{&tmptable}.WareHse bfr{&tmptable}.period bfr{&tmptable}.trDate bfr{&tmptable}.Stkcode
                         stkmf.descrip bfr{&tmptable}.Quantity[1] WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
    RETURN.
END.
    
    
