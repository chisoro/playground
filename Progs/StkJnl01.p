/* Program.................stkjnl01.p
   Notes:................. Stock Journal Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE skey            stkbctr.intBatch
&SCOPED-DEFINE tmptable        Stkbtf
&SCOPED-DEFINE tmpFields       bfr{&tmptable}.trDATE ~
                                        COLUMN-LABEL ' Transaction Date ':C ~
                               bfr{&tmptable}.REF ~
                                        COLUMN-LABEL ' Reference ':C ~
                               bfr{&tmptable}.stkCode ~
                                        COLUMN-LABEL ' Stock ':C ~
                               bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C~
                               bfr{&tmptable}.Quantity ~
                                        COLUMN-LABEL ' Quantity ':C ~
                               bfr{&tmptable}.Cost ~
                                        COLUMN-LABEL ' Amount ':C
                               
{varlibrary.i}
DEF VAR varUser LIKE simusr.usercode.
DEF VAR wsdel          AS LOGICAL INITIAL NO.
DEF VAR wsbatch     LIKE  stkbctr.intBatch.
DEF VAR wsrec       LIKE  stkbtf.Seq.
DEF VAR wscurr      LIKE SIMCTR.CURPER.
DEF VAR wsclosed    LIKE  SIMCTR.CLOSEPER.
DEF VAR wsper       LIKE simctr.closeper.
DEF VAR wsDate      LIKE stkbtf.trDate.
DEF VAR wsAmt       LIKE stkbtf.Cost EXTENT 2.
DEF VAR wsQuant     LIKE stkbtf.Quantity.
DEF BUTTON btn-Add1   LABEL "ADD".
DEF BUTTON BtnStk    LABEL "Stock".

DEFINE QUERY qry-stkbctr FOR  stkbctr scrolling.
DEF BROWSE brw-stkbctr QUERY qry-stkbctr
        DISPLAY stkbctr.UID             COLUMN-LABEL "USER"
                stkbctr.period WIDTH 10 COLUMN-LABEL "PERIOD"
                stkbctr.BDate WIDTH 12  COLUMN-LABEL "BATCH DATE"
                stkbctr.intBatch        COLUMN-LABEL "BATCH NUMBER" 
                stkbctr.Quantity        COLUMN-LABEL "QUANTITY" 
                stkbctr.Cost            COLUMN-LABEL "COST"
        WITH 20 DOWN SEPARATORS.

DEFINE QUERY qry-bfr{&tmptable} FOR  bfr{&tmptable} scrolling.
DEF BROWSE brw-bfr{&tmptable} QUERY qry-bfr{&tmptable}
    DISPLAY bfr{&tmptable}.seq              COLUMN-LABEL "SEQUENCE"
            bfr{&tmptable}.trDATE           COLUMN-LABEL "TR-DATE"  
            bfr{&tmptable}.REF              COLUMN-LABEL "REFERENCE"
            bfr{&tmptable}.stkcode          COLUMN-LABEL "STOCK"
            bfr{&tmptable}.DESCRIP WIDTH 35 COLUMN-LABEL "DESCRIPTION"
            bfr{&tmptable}.Quantity         COLUMN-LABEL "QUANTITY"
            bfr{&tmptable}.Cost             COLUMN-LABEL "COST"
    WITH 20 DOWN SEPARATORS.

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

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 17.5.

DEFINE RECTANGLE rect-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 120 BY 2.3.

DEFINE RECTANGLE rect-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 120 BY 20.5.

DEFINE RECTANGLE rect-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 BY 2.3.

DEFINE RECTANGLE rect-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 BY 17.5.

DEFINE FRAME frm-input
    SKIP(1)
      bfr{&tmptable}.Seq       COLON 27  LABEL "Record " SKIP(0.5)
      bfr{&tmptable}.trDATE    COLON 27  LABEL "Transaction Date " SKIP(0.5)
      bfr{&tmptable}.REF       COLON 27  LABEL  "Reference Number " SKIP(0.5)
      BtnStk                   COLON 20  NO-TAB-STOP 
      bfr{&tmptable}.Stkcode          NO-LABEL 
      stkmf.descrip             view-as text NO-LABEL SKIP(0.5)
      bfr{&tmptable}.DESCRIP   COLON 27 LABEL "Narration " SKIP(0.5)
      bfr{&tmptable}.Quantity  COLON 27  LABEL "Quantity "  SKIP(0.5)
      bfr{&tmptable}.Cost      COLON 27  LABEL "Cost "  SKIP(0.5)
      btnLedger                COLON 15  NO-TAB-STOP
      bfr{&tmptable}.Ledger     NO-LABEL
      glmf.DESCRIPTION       view-as text NO-LABEL SKIP(0.5)
      btnDept                 COLON 4  no-tab-stop
      bfr{&tmptable}.Dept              NO-LABEL
      glDept.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
      btnFund                 COLON 8.5  no-tab-stop
      bfr{&tmptable}.Fund              NO-LABEL
      glFund.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
      btnProj                 COLON 14  no-tab-stop
      bfr{&tmptable}.Proj              NO-LABEL
      glProj.DESCRIPTION  view-as text no-label NO-TAB-STOP skip(1)
      rect-6 AT ROW 1.4 COL 3
      rect-5 AT ROW 19 COL 3
      btn-ok AT ROW 19.5 COL 15 SPACE(40) 
      btn-cancel  
    with view-as dialog-box NO-VALIDATE
         SIZE 100 BY 22 side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    
DEF FRAME frm-main
    brw-stkbctr AT ROW 2 COL 8
    btn-add AT ROW 19.7 COL 5 SPACE(15)
    btn-edit SPACE(15) 
    btn-del SPACE(15)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 90 BY 23
    TITLE "Journal Maintenance" view-as dialog-box.

DEF FRAME frm-stkBatch
     wsBatch   LABEL "Batch Number" AT ROW 2 COL 5 SPACE(2)
     wsDate    LABEL "Batch Date"   SPACE(2) 
     wsper     LABEL "Accounting Period"  SPACE(2)
     stkbctr.TranType LABEL "Batch Type" SKIP(0.5)
     Stkbctr.Quantity  COLON 50 LABEL "Quantity"  NO-TAB-STOP 
     Stkbctr.Cost      LABEL "Cost" NO-TAB-STOP SKIP(0.5) 
     brw-bfr{&tmptable} NO-TAB-STOP AT ROW 5.0 COL 5 
    btn-add1 AT ROW 22.7 COL 10
    space(25) btn-edit
    space(25) btn-del
    space(25) btn-cancel SKIP(1)
    rect-4 AT ROW 1.4 COL 3
    rect-3 AT ROW 22 COL 3
    WITH TITLE "Journal Data Capture"
    1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 125 BY 25.5 view-as DIALOG-BOX.

/* *** Triggers to input frame frm-input*** */

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
  APPLY 'tab' TO bfr{&tmptable}.Stkcode IN FRAME frm-input .
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickStock 
    OR 'enter':u OF brw-PickStock 
    OR 'mouse-select-dblclick' OF brw-PickStock 
DO: 
   GET CURRENT qry-PickStock  EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY  stkmf.Stkcode  @ bfr{&tmptable}.Stkcode stkmf.Descrip WITH FRAME frm-input.
   RETURN.
END.

ON 'enter' OF  bfr{&tmptable}.Stkcode IN FRAME frm-input 
    OR 'tab' OF  bfr{&tmptable}.Stkcode IN FRAME frm-input
DO:
    IF  bfr{&tmptable}.Stkcode:SCREEN-VALUE = "" THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST stkmf WHERE stkmf.Stkcode =  bfr{&tmptable}.Stkcode:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stkmf THEN DO:
              MESSAGE "Stock does not exist" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        DISPLAY stkmf.Descrip WITH FRAME frm-input.
    END.
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
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glmf.acct @ bfr{&tmptable}.Ledger glmf.DESCRIPTION WITH FRAME frm-input.
   APPLY 'tab' TO btnLedger IN FRAME frm-input.
   APPLY 'entry' TO bfr{&tmptable}.Ledger IN FRAME frm-input.
   RETURN. 
END.

ON "enter" OF  bfr{&tmptable}.Ledger IN FRAME frm-input 
    OR "tab" OF  bfr{&tmptable}.Ledger IN FRAME frm-input
DO:
    IF stkbctr.TranType = 2 THEN DO:
        FIND FIRST glmf WHERE glmf.acct = int(bfr{&tmptable}.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "Ledger does not exist" VIEW-AS ALERT-BOX.
            APPLY 'entry':u TO SELF.
            RETURN NO-APPLY.
         END.
    END.
END.

ON CHOOSE OF btnFund IN FRAME frm-Input
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
   APPLY 'tab' TO bfr{&tmpTable}.fund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfr{&tmpTable}.Fund glFund.DESCRIP  WITH FRAME frm-Input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmpTable}.Fund IN FRAME frm-Input
    OR 'tab':U OF bfr{&tmpTable}.Fund IN FRAME frm-Input
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(bfr{&tmpTable}.Fund:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Fundect entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.descrip WITH FRAME frm-Input.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input  
DO:
   IF NOT CAN-FIND(stkmf WHERE stkmf.Stkcode = bfr{&tmptable}.Stkcode:SCREEN-VALUE) THEN DO:
        MESSAGE "Invalid Stock entered .. no record saved" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END. 
    ELSE IF stkbctr.TranType = 2 AND NOT CAN-FIND(glmf WHERE glmf.acct = INT(bfr{&tmptable}.Ledger:SCREEN-VALUE)) 
        THEN DO:
            MESSAGE "Invalid Ledger entered .. no record saved" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE IF  bfr{&tmptable}.Stkcode:SCREEN-VALUE = "" THEN
        DO:
             APPLY "CLOSE":U TO THIS-PROCEDURE.
             RETURN NO-APPLY.
        END. 
        ELSE IF wsbtn = "New" THEN DO:
            CREATE  bfr{&tmptable}.
             ASSIGN  bfr{&tmptable}.Ledger  = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE)
                     bfr{&tmptable}.Dept    = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
                     bfr{&tmptable}.Proj    = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
                     bfr{&tmptable}.fund    = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                     bfr{&tmptable}.intBatch = wsbatch
                     bfr{&tmptable}.seq      = INT( bfr{&tmptable}.seq:SCREEN-VALUE)
                     bfr{&tmptable}.Stkcode  = bfr{&tmptable}.Stkcode:SCREEN-VALUE
                     bfr{&tmptable}.Quantity = DEC( bfr{&tmptable}.Quantity:SCREEN-VALUE)
                     bfr{&tmptable}.Cost     = DEC( bfr{&tmptable}.Cost:SCREEN-VALUE)
                     bfr{&tmptable}.REF      = bfr{&tmptable}.REF:SCREEN-VALUE IN FRAME frm-input
                     bfr{&tmptable}.trDATE   = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                     bfr{&tmptable}.Descrip  =  bfr{&tmptable}.Descrip:SCREEN-VALUE
                     stkbctr.Quantity =  stkbctr.Quantity + DEC( bfr{&tmptable}.Quantity:SCREEN-VALUE) 
                     stkbctr.Cost     =  stkbctr.Cost + DEC( bfr{&tmptable}.Cost:SCREEN-VALUE). 
                   ASSIGN wsrec= wsrec + 1
                          wsAmt[1] = stkbctr.Quantity
                          wsAmt[2] = stkbctr.Cost.
                   DISPLAY wsrec @ bfr{&tmptable}.seq  WITH FRAME frm-input.
                   DISPLAY  wsAmt[1] @ stkbctr.Quantity wsAmt[2] @ stkbctr.Cost WITH FRAME frm-stkBatch.
                   OPEN QUERY qry-bfr{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK.
             APPLY 'entry' TO  bfr{&tmptable}.trDATE.
            RETURN.
        END.
        ELSE DO:
            ASSIGN  stkbctr.Quantity =  stkbctr.Quantity -  bfr{&tmptable}.Quantity 
                    stkbctr.Cost =  stkbctr.Cost -  bfr{&tmptable}.Cost.
            ASSIGN  bfr{&tmptable}.Ledger  = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE)
                    bfr{&tmptable}.Dept    = INT(bfr{&tmptable}.dept:SCREEN-VALUE)
                    bfr{&tmptable}.Proj    = INT(bfr{&tmptable}.proj:SCREEN-VALUE)
                    bfr{&tmptable}.fund    = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                    bfr{&tmptable}.Stkcode  = bfr{&tmptable}.Stkcode:SCREEN-VALUE
                    bfr{&tmptable}.Quantity = DEC( bfr{&tmptable}.Quantity:SCREEN-VALUE)
                    bfr{&tmptable}.Cost     = DEC( bfr{&tmptable}.Cost:SCREEN-VALUE)
                    bfr{&tmptable}.REF      = bfr{&tmptable}.REF:SCREEN-VALUE IN FRAME frm-input
                    bfr{&tmptable}.trDATE   = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                    bfr{&tmptable}.Descrip  =  bfr{&tmptable}.Descrip:SCREEN-VALUE
                    stkbctr.Quantity =  stkbctr.Quantity + DEC( bfr{&tmptable}.Quantity:SCREEN-VALUE) 
                    stkbctr.Cost     =  stkbctr.Cost + DEC( bfr{&tmptable}.Cost:SCREEN-VALUE). 
             ASSIGN  wsAmt[1] = stkbctr.Quantity
                     wsAmt[2] = stkbctr.Cost.
            /* CLEAR FRAME frm-input ALL. */
             HIDE FRAME frm-input.
             /* VIEW FRAME frm-stkBatch. */
             DISPLAY  wsAmt[1] @ stkbctr.Quantity wsAmt[2] @ stkbctr.Cost WITH FRAME frm-stkBatch.
             OPEN QUERY qry-bfr{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK.
             APPLY "CLOSE":U TO THIS-PROCEDURE.
             RETURN.
        END.
END.

/* *** Triggers to frame frm-stkBatch*** */
ON CHOOSE OF btn-Add1 IN FRAME frm-stkBatch
 DO:
   FIND LAST  bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-ERROR.
   IF AVAILABLE  bfr{&tmptable} THEN
       wsrec =  bfr{&tmptable}.seq.
    ELSE wsrec = 0.
         wsrec = wsrec + 1.
    wsbtn = "New".
    OPEN QUERY qry-bfr{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch. 
    RUN proc-input.
    RETURN.
END.

ON 'mouse-select-dblclick' OF brw-bfr{&tmptable} IN FRAME frm-stkBatch
    OR CHOOSE OF btn-edit IN FRAME frm-stkBatch
DO: 
    wsbtn = "Upd".
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsrec = int(bfr{&tmptable}.seq)
           wsid = int(bfr{&tmptable}.intbatch).
    FIND FIRST stkbctr WHERE stkbctr.intbatch  = wsid NO-ERROR.
    FIND FIRST stkmf WHERE stkmf.Stkcode = bfr{&tmptable}.Stkcode NO-LOCK NO-ERROR.
    VIEW FRAME frm-input.
    DISPLAY bfr{&tmptable}.Seq
            bfr{&tmptable}.trDATE 
            bfr{&tmptable}.Stkcode 
            stkmf.Descrip
            bfr{&tmptable}.DESCRIP 
            bfr{&tmptable}.ref
            bfr{&tmptable}.Quantity
            bfr{&tmptable}.Cost
            bfr{&tmptable}.Ledger WITH FRAME frm-input.
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.Ledger NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
        DISPLAY glmf.descrip WITH FRAME frm-input.
    FIND FIRST glDept WHERE glDept.dept = bfr{&tmptable}.dept NO-LOCK NO-ERROR.
    IF AVAILABLE gldept THEN
        DISPLAY gldept.descrip WITH FRAME frm-input.
    FIND FIRST glProj WHERE glProj.Proj = bfr{&tmptable}.Proj NO-LOCK NO-ERROR.
    IF AVAILABLE glProj THEN
        DISPLAY glProj.DESCRIPTION WITH FRAME frm-input.
    ENABLE ALL EXCEPT bfr{&tmptable}.Seq WITH FRAME frm-input. 
    WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
    HIDE FRAME frm-input.
    RETURN.
END.
 
ON CHOOSE OF btn-Del IN FRAME frm-stkBatch DO:
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  bfr{&tmptable}.intbatch.
    MESSAGE "Are you sure you want to delete this transaction?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
        FIND FIRST  stkbctr WHERE  stkbctr.intbatch = wsid EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN  stkbctr.Quantity =  stkbctr.Quantity -  bfr{&tmptable}.Quantity
                stkbctr.Cost     =  stkbctr.Cost -  bfr{&tmptable}.Cost.
         DELETE  bfr{&tmptable}.
         Method-Status = brw-bfr{&tmptable}:DELETE-SELECTED-ROWS().
         ASSIGN wsAmt[1] = stkbctr.Quantity
                wsAmt[2] = stkbctr.Cost.
         DISPLAY  wsAmt[1] @ stkbctr.Quantity wsAmt[2] @ stkbctr.Cost WITH FRAME frm-stkBatch.
     END.
    APPLY 'entry':u TO btn-cancel.
END.


ON 'TAB' OF  wsBatch IN FRAME frm-stkBatch
    OR 'enter' OF  wsBatch IN FRAME frm-stkBatch
DO:
    IF int( wsBatch:SCREEN-VALUE) = 0 THEN DO:
        HIDE FRAME frm-stkBatch.
        APPLY 'Close' TO THIS-PROCEDURE.
    END.   
    ELSE DO:
      FIND FIRST  stkbctr WHERE  stkbctr.intbatch = int( wsBatch:SCREEN-VALUE) NO-ERROR.
      IF AVAILABLE  stkbctr THEN DO:
          MESSAGE "Batch number already exist" VIEW-AS ALERT-BOX.
          APPLY 'Close' TO THIS-PROCEDURE.
          RETURN NO-APPLY.
       END.
        ELSE
           APPLY 'TAB' TO wsDate IN FRAME frm-stkBatch.
    END.
END.

ON 'TAB' OF  wsper IN FRAME frm-stkBatch
    OR 'enter' OF  wsPer IN FRAME frm-stkBatch
DO:
    IF INT(wsper:SCREEN-VALUE) <= wscurr
        AND INT(wsper:SCREEN-VALUE) > wsClosed  THEN
    DO:
       apply 'entry' to stkbctr.TranType IN FRAME frm-stkBatch.
    END.
    ELSE DO:
         MESSAGE "The period " wsper " is outside Accounting Periods. No batch will be created"
              VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    
END.

ON 'TAB' OF  stkbctr.TranType IN FRAME frm-stkBatch
    OR 'enter' OF  stkbctr.TranType IN FRAME frm-stkBatch
DO:
    CREATE  stkbctr.
    ASSIGN  stkbctr.UID      = varUser
            wsbatch          = INT( wsBatch:SCREEN-VALUE)
            stkbctr.intbatch = INT( wsBatch:SCREEN-VALUE)
            stkbctr.bdate    = DATE( wsdate:SCREEN-VALUE)
            stkbctr.period   = INT( wsper:SCREEN-VALUE)
            stkbctr.TranType .
        apply 'entry' to btn-add1 IN FRAME frm-stkBatch.
END.


/* ***** Triggers for the main frame **** */

ON CHOOSE OF btn-Add IN FRAME frm-main
DO:  
   wsbatch = 0.
   wsdate = TODAY.
   CLEAR FRAME frm-StkBatch.
   VIEW FRAME frm-stkBatch.
   ENABLE ALL  EXCEPT  stkbctr.Quantity stkbctr.Cost brw-bfr{&tmptable} WITH FRAME frm-stkBatch.
   WAIT-FOR CHOOSE OF btn-cancel OR CLOSE OF THIS-PROCEDURE IN FRAME frm-stkBatch.
   CLEAR FRAME frm-stkBatch ALL.
   DISABLE ALL WITH FRAME frm-stkBatch.
   HIDE FRAME frm-stkBatch.
   VIEW FRAME frm-main.
   OPEN QUERY qry-stkbctr FOR EACH  stkbctr NO-LOCK.
     
END.
 
ON CHOOSE OF btn-Edit IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-stkbctr
DO:
    GET CURRENT qry-stkbctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsbatch =  stkbctr.intbatch.
    FIND FIRST stkbctr WHERE  stkbctr.intbatch = wsBatch EXCLUSIVE-LOCK NO-ERROR.
    CLEAR FRAME frm-stkBatch ALL.
    VIEW FRAME frm-stkBatch.
    ENABLE ALL EXCEPT  wsbatch wsDate wsPer stkbctr.Quantity stkbctr.Cost WITH FRAME frm-stkBatch.
    DISPLAY  stkbctr.intbatch @ wsBatch stkbctr.bdate @ wsDate   stkbctr.period @ wsPer
         stkbctr.TranType stkbctr.Quantity stkbctr.Cost WITH FRAME frm-stkBatch.
    FIND LAST  bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-ERROR.
    IF AVAILABLE  bfr{&tmptable} THEN
        wsrec =  bfr{&tmptable}.seq.
    ELSE wsrec = 0.
    OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
        WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK BY  bfr{&tmptable}.seq.
    WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-stkBatch.
    CLOSE QUERY qry-bfr{&tmptable}.
    CLEAR FRAME frm-stkBatch ALL.
    DISABLE ALL WITH FRAME frm-stkBatch.
    HIDE FRAME frm-stkBatch.
    VIEW FRAME frm-main.
    RETURN.
END.


ON CHOOSE OF btn-Del IN FRAME frm-main DO:
    GET CURRENT qry-stkbctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  stkbctr.intbatch.
    MESSAGE "Are you sure you want to delete this batch?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
         FOR EACH Stkbtf WHERE stkbtf.intBatch = wsid:
             DELETE Stkbtf.
         END.
         DELETE  stkbctr.
         Method-Status = brw-stkbctr:DELETE-SELECTED-ROWS().
     END. 
    APPLY 'entry' TO btn-exit.
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
   DISPLAY gldept.dept @ bfr{&tmptable}.dept gldept.descrip WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.dept IN FRAME frm-input
   OR 'tab':U OF bfr{&tmptable}.dept IN FRAME frm-input
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(bfr{&tmptable}.dept:SCREEN-VALUE) NO-ERROR.
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
   DISPLAY glProj.Proj @ bfr{&tmptable}.Proj glproj.DESCRIPTION  WITH FRAME frm-input.
   RETURN.
END.
ON 'enter':U OF bfr{&tmptable}.Proj IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Proj IN FRAME frm-input
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE) NO-ERROR.
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
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
ASSIGN wscurr = SIMCTR.CURPER
       wsclosed = SIMCTR.CLOSEPER.
OPEN QUERY qry-stkbctr FOR EACH  stkbctr NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-stkbctr.
HIDE FRAME frm-main.

PROCEDURE proc-input:
  VIEW FRAME frm-input.
  ENABLE ALL EXCEPT  bfr{&tmptable}.seq WITH FRAME frm-input.
  IF stkbctr.TranType = 1 THEN
      DISABLE btnLedger bfr{&tmptable}.Ledger
              btnDept bfr{&tmptable}.Dept
              btnProj bfr{&tmptable}.Proj WITH FRAME frm-input.
  DISPLAY wsRec @ bfr{&tmptable}.seq WITH FRAME frm-input.
  WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
  OPEN QUERY qry-stkbctr FOR EACH  stkbctr NO-LOCK.
  HIDE FRAME frm-input.
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


