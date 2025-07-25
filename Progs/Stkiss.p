/* Program.................Stkiss.p
   Notes:................. Stores Issues
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Stock item does not exist"
&SCOPED-DEFINE wsTitle           " Online Stores Issue"
&SCOPED-DEFINE tmptable             Stktrans
&SCOPED-DEFINE skey                Crdtmf.Acc
SESSION:DATA-ENTRY-RETURN = TRUE.
{varlibrary.i}
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
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
DEF VAR wsTCr LIKE wsQuantity.
DEF VAR wsTDr LIKE wsQuantity.
DEF VAR wsDr   LIKE wsQuantity.
DEF VAR wsCr   LIKE wsQuantity.
DEF VAR wsCost LIKE wsQuantity.
DEF VAR wsLedger LIKE glmf.acct.
DEF VAR wsWhse LIKE stkwhf.WareHse.
DEF VAR wsDescrip LIKE stktrans.Descrip.
DEF VAR wsSource LIKE dbgl.SOURCE INITIAL "ST".

DEF BUTTON btn-Update  LABEL "UPDATE".
DEF BUTTON btn-whse    LABEL "WAREHOUSE".
DEF BUTTON btnStk     LABEL "STOCK".

FIND FIRST SIMCTR NO-LOCK NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.

DEFINE RECTANGLE rect-1
         EDGE-PIXELS 2 graphic-edge  NO-FILL
         SIZE 98 BY 2.3.

DEFINE RECTANGLE rect-2
         EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
         SIZE 98 BY 16.5.

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
      btn-whse     COLON 6 NO-TAB-STOP wsWhse NO-LABEL
      stkwhf.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
      BtnStk      COLON 12  NO-TAB-STOP wsStkcode NO-LABEL 
      stkmf.descrip  NO-LABEL VIEW-AS TEXT  SKIP(0.5)
      wsper       COLON 20 LABEL "Accounting Period" SPACE(5)
      wsDATE                LABEL "Transaction Date " SKIP(0.5)
      wsREF       COLON 20  LABEL  "Reference Number " SKIP(0.5)
      wsDESCRIP   COLON 20 LABEL "Narration " SKIP(0.5)
      wsQuantity  COLON 20  LABEL "Quantity "  SPACE(5) wsCost LABEL "Cost "  VIEW-AS TEXT SKIP(0.5)
      btnLedger   COLON 8  NO-TAB-STOP wsLedger NO-LABEL
      glmf.DESCRIPTION       VIEW-AS TEXT NO-LABEL SKIP(0.5)
      btnDept                 COLON 3  no-tab-stop
      glDept.Dept              NO-LABEL
      glDept.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
      btnFund                 COLON 3  no-tab-stop
      glFund.Fund              NO-LABEL
      glfund.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
      btnProj                 COLON 8  no-tab-stop
      glProj.Proj              NO-LABEL
      glProj.DESCRIPTION  view-as text no-label NO-TAB-STOP skip(1.5)
      btn-Update colon 21 
      btn-Exit colon 70
      rect-2 AT ROW 1 COL 3
      rect-1 AT ROW 17.6 COL 3 
    with view-as dialog-box NO-VALIDATE SIZE 103 BY 22
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "STORES ISSUE DATA CAPTURE".

FORM
     wsStkcode    LABEL "STOCK"
     wsDate       LABEL "DATE"
     wsRef        LABEL "REFERENCE"
     wsDescrip   LABEL "DESCRIPTION"
     wsQuantity   LABEL "QUANTITY"
     wsCost       LABEL "COST"
     wsLedger     LABEL "ALLOCATION"
    HEADER skip(1) "STORES ISSUE UPDATE REPORT   " "DATE: " wsDate "PERIOD  :" wsPer  " Batch Captured by:" 
        simusr.Name SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

FORM dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT" 
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" FORM "zz,zzz,zz9.99"
     wsCr                  LABEL "CREDITS" FORM "zz,zzz,zz9.99-"
     HEADER skip(1) "STORES  JOURNAL UPDATE CONSOLIDATION REPORT   " "DATE: " wsDate  "PERIOD  :" wsPer  "Page: " AT 90 PAGE-NUMBER SKIP(1) 
    "        Transaction Captured by:" simusr.Name SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

/******* Triggers ***** */
ON CHOOSE OF btn-whse IN FRAME  frm-input
DO:
  VIEW FRAME frm-Pickwhse.
  OPEN QUERY qry-Pickwhse FOR EACH stkwhf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Pickwhse.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-Pickwhse 
          OR close of THIS-PROCEDURE IN FRAME frm-Pickwhse 
          OR CHOOSE OF btn-ok IN FRAME frm-PickStock 
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
        FIND FIRST stkmf WHERE stkmf.Stkcode =  wsStkcode:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stkmf THEN DO:
              MESSAGE "Stock does not exist" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        DISPLAY stkmf.Descrip WITH FRAME frm-input.
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
        MESSAGE "Period entered is outside the open accounting periods" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.
ON 'TAB':U OF wsDate IN FRAME frm-input
   OR 'leave':U OF wsDate IN FRAME frm-input
DO:
    ASSIGN wsDate = DATE(wsDate:SCREEN-VALUE).
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > wsper
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF INT(YEAR(wsDate)) <> INT(SUBSTR(STRING(wsper),1,4)) OR INT(MONTH(wsDate)) <> INT(SUBSTR(STRING(wsper),5,2)) 
        THEN DO:
        MESSAGE "Your date is outside the Accounting Period...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnLedger IN FRAME frm-input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
   APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsLedger IN FRAME frm-input .
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glmf.acct @ wsLedger glmf.DESCRIPTION WITH FRAME frm-input.
   APPLY 'tab' TO btnLedger IN FRAME frm-input.
   APPLY 'entry' TO wsLedger IN FRAME frm-input.
   RETURN. 
END.

ON "enter" OF  wsLedger IN FRAME frm-input 
    OR "tab" OF wsLedger IN FRAME frm-input
DO:
   FIND FIRST glmf WHERE glmf.acct = DEC(wsLedger:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE glmf THEN DO:
       MESSAGE "Ledger does not exist" VIEW-AS ALERT-BOX.
       APPLY 'entry':u TO SELF.
       RETURN NO-APPLY.
   END.
   ASSIGN gldept.dept:SCREEN-VALUE = STRING(glmf.dept)
          glfund.fund:SCREEN-VALUE = STRING(glmf.fund)
          glproj.proj:SCREEN-VALUE = STRING(glmf.proj).
   IF glmf.dept <> 0 THEN DO:
       APPLY 'tab' TO gldept.dept IN FRAME frm-input.
       DISABLE btnDept gldept.dept WITH FRAME frm-input.
   END.
   ELSE ENABLE btnDept gldept.dept WITH FRAME frm-input.
   IF glmf.fund <> 0 THEN DO:
       APPLY 'tab' TO glfund.fund IN FRAME frm-input.
       DISABLE btnFund  glfund.fund WITH FRAME frm-input.
   END.
   ELSE ENABLE btnFund  glfund.fund WITH FRAME frm-input.
   IF glmf.proj <> 0 THEN DO:
       APPLY 'tab' TO glproj.proj IN FRAME frm-input.
       DISABLE btnProj  glproj.proj WITH FRAME frm-input.
   END.
   ELSE ENABLE btnProj  glproj.proj WITH FRAME frm-input.
END.


ON CHOOSE OF btnFund IN FRAME frm-input
DO:
  VIEW FRAME frm-PickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-PickFund
          OR CHOOSE OF btn-ok IN FRAME frm-PickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-PickFund.
  APPLY 'tab' TO btnFund.
   APPLY 'tab' TO glfund.fund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ glfund.Fund glFund.DESCRIP  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF glfund.Fund IN FRAME frm-input
    OR 'tab':U OF glfund.Fund IN FRAME frm-input
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(glfund.Fund:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Fund entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.descrip WITH FRAME frm-input.
    END.
    RETURN.
END.

ON 'enter' OF  wsQuantity IN FRAME frm-input 
    OR 'tab' OF  wsQuantity IN FRAME frm-input
DO:
    FIND FIRST stkmf WHERE stkmf.stkCode = wsStkcode:SCREEN-VALUE
                       AND stkmf.WareHse = INT(wswHse:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE stkmf AND stkmf.quantity[1] < DEC(wsQuantity:SCREEN-VALUE) THEN DO:
        MESSAGE "Required quantity if less than available stock of "stkmf.quantity[1]
              VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE stkmf AND stkmf.quantity[1] >= DEC(wsQuantity:SCREEN-VALUE) THEN DO:
        wsCost:SCREEN-VALUE = STRING(DEC(wsQuantity:SCREEN-VALUE) * stkmf.UnitPrice).
    END.
END.

ON CHOOSE OF btn-Update IN FRAME frm-input 
DO:
   ASSIGN wsStkcode wsDescrip wsDate wsRef wsQuantity wsCost wsLedger wsWhse.
   ASSIGN wsMonth = INT(SUBSTR(string(wsPer),5,2))
           wsYear  = INT(SUBSTR(string(wsPer),1,4)).
   /* Select print */
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged}
   CLEAR FRAME frm-Input.
   ASSIGN wsDate:SCREEN-VALUE = STRING(TODAY)
          wsDr = 0
          wsCr = 0
          wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER).
   APPLY 'entry' TO wsWhse IN FRAME frm-input.  
   
END.
ON CHOOSE OF btnDept IN FRAME frm-input
DO:
  VIEW FRAME frm-pickdept.
  OPEN QUERY qry-PickDept FOR EACH glDept NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickdept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickdept 
          OR close of THIS-PROCEDURE IN FRAME frm-pickdept
          OR CHOOSE OF btn-ok IN FRAME frm-pickdept 
          OR 'enter':u OF brw-PickDept
          OR 'mouse-select-dblclick' OF brw-PickDept.
  CLOSE QUERY qry-PickDept.
  HIDE FRAME frm-pickdept.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickdept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY gldept.dept @ glDept.dept gldept.descrip WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF glDept.dept IN FRAME frm-input
   OR 'tab':U OF glDept.dept IN FRAME frm-input
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(glDept.dept:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE gldept THEN DO:
        MESSAGE "Invalid Department entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY gldept.descrip WITH FRAME frm-input.
    END.
    RETURN.
END.

ON CHOOSE OF btnProj IN FRAME frm-input
DO:
  VIEW FRAME frm-pickProj.
  OPEN QUERY qry-PickProj FOR EACH glProj NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickProj.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickProj 
          OR close of THIS-PROCEDURE IN FRAME frm-pickProj
          OR CHOOSE OF btn-ok IN FRAME frm-pickProj 
          OR 'enter':u OF brw-PickProj
          OR 'mouse-select-dblclick' OF brw-PickProj.
  CLOSE QUERY qry-PickProj.
  HIDE FRAME frm-pickProj.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ glproj.Proj glproj.DESCRIPTION  WITH FRAME frm-input.
   RETURN.
END.
ON 'enter':U OF glproj.Proj IN FRAME frm-input
    OR 'tab':U OF glproj.Proj IN FRAME frm-input
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(glproj.Proj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glProj.descrip WITH FRAME frm-input.
    END.
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
wsDate:SCREEN-VALUE = STRING(TODAY).
wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER).
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Input.
HIDE FRAME frm-main.

PROCEDURE UPDATE-ip:
DO TRANSACTION ON ERROR UNDO, LEAVE:
    FIND FIRST Stkmf WHERE Stkmf.Stkcode = wsStkcode EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST stkwhf WHERE stkwhf.WareHse = stkmf.WareHse NO-LOCK NO-ERROR.
    CREATE Stktrans.
    ASSIGN Stktrans.Period   = wsPer
           Stktrans.trDate   = wsDate
           Stktrans.Stkcode  = wsStkcode
           Stktrans.DESCRIP  = wsdescrip
           Stktrans.Quantity =  wsQuantity * -1
           Stktrans.Cost     =  wsCost * -1
           stktrans.UnitPrice = stkmf.UnitPrice
           Stktrans.PoDATE   = TODAY
           Stktrans.prog     = "Stkiss.p"
           Stktrans.Ref      =  wsRef
           Stktrans.TranType = 3
           Stktrans.TransID  = wsTransId
           Stktrans.Ledger   = wsLedger
           stktrans.Dept     = INT(gldept.dept:SCREEN-VALUE IN FRAME frm-Input)
           stktrans.Proj     = INT(glProj.Proj:SCREEN-VALUE  IN FRAME frm-Input)
           Stktrans.UID      = VarUser.
           Stktrans.UID2     = VarUser.
    FIND FIRST glmf WHERE glmf.acct = wsLedger  NO-LOCK NO-ERROR.
    ASSIGN stkmf.Cost[1]     = stkmf.Cost[1] - wsCost
           Stkmf.Quantity[1] = Stkmf.Quantity[1] - wsQuantity. 
    DISPLAY STREAM a wsStkcode  wsDate wsRef
                     wsDescrip wsQuantity wsCost wsLedger WITH FRAME frm-rpt.
    DOWN  3 STREAM a WITH FRAME frm-rpt.
    /* Create GL consolidation data */
    IF wsCost <> 0 THEN DO:
        DO X = 1 TO 2:
            IF X = 2 THEN DO:
               ASSIGN wsLedger = stkwhf.Ledger
                       wsCost    = wsCost * -1.
            END.  
           CREATE dbgl.
           ASSIGN dbgl.acct    = wsledger
                  dbgl.fund     = glmf.fund 
                  dbgl.dept     = INT(gldept.dept:SCREEN-VALUE IN FRAME frm-Input) 
                  dbgl.proj     = INT(glProj.Proj:SCREEN-VALUE  IN FRAME frm-Input)
                  dbgl.period  = wsper
                  dbgl.TransID = wsTransId
                  dbgl.trDATE  = wsDate
                  dbgl.UID2    = varUser
                  dbgl.UID     = varUser
                  dbgl.REF     = wsRef
                  dbgl.DESCRIP = "Online consolidation"
                  dbgl.CREDATE = TODAY
                  dbgl.AMT     =  wsCost
                  dbgl.SOURCE = wsSource.

        END. /* do 1 to 2 */
       {glcon.i}
    END. /* IF wsCost */  
END. /* eof Do Transaction */
RELEASE glbal.
RELEASE dbgl.
RELEASE Stktrans.
RELEASE Stkmf.
APPLY 'close' TO THIS-PROCEDURE.
END.
    
procedure Search.ip.
    hCol = browse  brw-ledger:current-column.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        case trim(hCol:label):
            when "Account" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.acct >= DEC(wsSearch:SCREEN-VALUE)
                               BY glmf.acct.
            END.
            when "Description" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.descrip >= wsSearch:SCREEN-VALUE
                               BY glmf.descrip.
            END. 
            when "Department" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.dept >= INT(wsSearch:SCREEN-VALUE)
                               BY glmf.dept BY glmf.acct.

            END.
        END.
        RETURN.
    END. 

