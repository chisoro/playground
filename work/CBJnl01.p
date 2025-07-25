/* Program.................Cbjnl01.p
   Notes:................. Cashbook Journal Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable        Cbbtf
&SCOPED-DEFINE tmpFields       bfr{&tmptable}.trDATE ~
                                        COLUMN-LABEL ' Transaction Date ':C ~
                               bfr{&tmptable}.bank ~
                                        COLUMN-LABEL ' Stock ':C ~
                               bfr{&tmptable}.REF ~
                                        COLUMN-LABEL ' Reference ':C ~
                               bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C~
                               bfr{&tmptable}.Amount ~
                                        COLUMN-LABEL ' Amount ':C
                               

DEF VAR varUser LIKE simusr.usercode.
DEF VAR wsid LIKE  cbbctr.intBatch.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wsdel          AS LOGICAL INITIAL NO.
DEF VAR wsbatch     LIKE  cbbctr.intBatch.
DEF VAR wsrec       LIKE  Cbbtf.Seq.
DEF VAR wscurr      LIKE SIMCTR.CURPER.
DEF VAR wsclosed    LIKE  SIMCTR.CLOSEPER.
DEF VAR wsper       LIKE simctr.closeper.
DEF VAR wsDate      LIKE Cbbtf.trDate.
DEF VAR wsAmt       LIKE Cbbtf.Amount EXTENT 2.
DEF VAR wsbtn       AS CHAR.
DEF BUTTON btn-Add   LABEL "ADD".
DEF BUTTON btn-Add1   LABEL "ADD".
DEF BUTTON btn-del   LABEL "DELETE".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-Edit  LABEL "EDIT".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btn-ok    LABEL "OK".
DEF BUTTON BtnBank    LABEL "Bank".
DEF BUTTON BtnCb   LABEL "Cash Book".
DEF BUTTON btnLedger LABEL "Ledger ".

DEF BUFFER bfr{&tmptable} FOR Cbbtf PRESELECT.

DEFINE QUERY qry-cbbctr FOR  cbbctr scrolling.
DEF BROWSE brw-cbbctr QUERY qry-cbbctr
        DISPLAY UID             COLUMN-LABEL "USER"
                period WIDTH 10 COLUMN-LABEL "PERIOD"
                BDate WIDTH 12  COLUMN-LABEL "BATCH DATE"
                intBatch        COLUMN-LABEL "BATCH NUMBER" 
                Amt[1]        COLUMN-LABEL "DR" 
                Amt[2]            COLUMN-LABEL "CR"
        WITH 20 DOWN SEPARATORS.

DEFINE QUERY qry-bfr{&tmptable} FOR  bfr{&tmptable} scrolling.
DEF BROWSE brw-bfr{&tmptable} QUERY qry-bfr{&tmptable}
    DISPLAY bfr{&tmptable}.seq              COLUMN-LABEL "SEQUENCE"
            bfr{&tmptable}.trDATE           COLUMN-LABEL "TR-DATE"  
            bfr{&tmptable}.REF              COLUMN-LABEL "REFERENCE"
            bfr{&tmptable}.acb              COLUMN-LABEL "CSAHBOOK"
            bfr{&tmptable}.DESCRIP WIDTH 35 COLUMN-LABEL "DESCRIPTION"
            bfr{&tmptable}.Amount              COLUMN-LABEL "AMOUNT"
            bfr{&tmptable}.Ledger             COLUMN-LABEL "LEDGER"
    WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-ledger FOR glmf SCROLLING.
DEF BROWSE brw-ledger QUERY qry-ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".

DEF    QUERY qry-PickCb FOR cbkmf SCROLLING.
DEF BROWSE brw-PickCb QUERY qry-PickCb
    DISPLAY cbkmf.Acb cbkmf.descrip WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickCb 
    brw-PickCb AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-Exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Cash Book Selection".

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

DEF    QUERY qry-cbkmf FOR cbbtf SCROLLING.
DEF BROWSE brw-cbkmf QUERY qry-cbkmf
    DISPLAY cbbtf.trDate cbbtf.acb cbbtf.Ref cbbtf.Descrip WIDTH 32 cbbtf.Ledger cbbtf.amount   
    WITH 10 DOWN SEPARATORS.

DEF FRAME frm-Input
    SKIP(0.5)
     wsDate  LABEL "Date"  SPACE(20)
     wsRef    LABEL "Reference" SKIP(0.5)
     wsNar    LABEL "Payee" COLON 10 VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.5)
     wsAmt    LABEL "Amount" COLON 10 SKIP(0.5)
     "Accounting Cashbook and Ledger Allocations" COLON 15
     brw-cbkmf    AT ROW 10 COL 2
     btnPrint    AT ROW 19.92 COL 50
     rect-1      AT ROW 19.38 COL 2
     rect-2     AT ROW 1.27 COL 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 98 BY 22 TITLE "ONLINE PAYMENT".

DEFINE FRAME frm-allocate 
     SKIP(1)
    wsTotal  COLON 80 LABEL "Ammount to be allocated" VIEW-AS TEXT
    SKIP(1)
    btnLedger COLON 5 NO-LABEL
    bfCBTrans.Ledger  NO-LABEL  
    glmf.DESCRIPTION NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.5)
    bfCBTrans.Descrip COLON 15 LABEL "Narration" VIEW-AS FILL-IN SIZE 50 BY 1 SKIP(0.5)
    btnAcb          COLON 1 NO-LABEL
    bfCBTrans.Acb     NO-LABEL
    cbkmf.descrip   NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.5)
    bfCBTrans.amount  COLON 20 LABEL "Amount" SKIP(2)
    btnClose COLON 40
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "ALLOCATIONS".
    
DEF FRAME frm-main
    brw-cbbctr AT ROW 2 COL 8
    btn-add AT ROW 19.7 COL 5 SPACE(15)
    btn-edit SPACE(15) 
    btn-del SPACE(15)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 90 BY 23
    TITLE "Journal Maintenance" view-as dialog-box.

DEF FRAME frm-cbBatch
     wsBatch     LABEL "Batch Number" AT ROW 2 COL 5 SPACE(2)
     wsDate      LABEL "Batch Date"   SPACE(2) 
     wsper       LABEL "Accounting Period"  SPACE(2)
     cbbctr.bank LABEL "Bank" SKIP(0.5)
     cbbctr.Amt[1]  COLON 50 LABEL "DR"  NO-TAB-STOP 
     cbbctr.Amt[2]  LABEL "CR" NO-TAB-STOP SKIP(0.5) 
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

ON CHOOSE OF BtnCb IN FRAME  frm-input
DO:
  VIEW FRAME frm-PickCb.
  OPEN QUERY qry-PickCb FOR EACH cbkmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickCb.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-PickCb 
          OR close of THIS-PROCEDURE IN FRAME frm-PickCb 
          OR CHOOSE OF btn-ok IN FRAME frm-PickCb 
          OR 'enter':u OF brw-PickCb
          OR 'mouse-select-dblclick' OF brw-PickCb.
  CLOSE QUERY qry-PickCb.
  HIDE FRAME frm-PickCb.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO bfr{&tmptable}.Acb IN FRAME frm-input .
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickCb 
    OR 'enter':u OF brw-PickCb 
    OR 'mouse-select-dblclick' OF brw-PickCb 
DO: 
   GET CURRENT qry-PickCb  EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY  cbkmf.Acb  @ bfr{&tmptable}.Acb cbkmf.Descrip WITH FRAME frm-input.
   RETURN.
END.

ON 'enter' OF  bfr{&tmptable}.Acb IN FRAME frm-input 
    OR 'tab' OF  bfr{&tmptable}.Acb IN FRAME frm-input
DO:
    IF  bfr{&tmptable}.Acb:SCREEN-VALUE = "" THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST cbkmf WHERE cbkmf.Acb =  bfr{&tmptable}.Acb:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE cbkmf THEN DO:
              MESSAGE "Cashbook does not exist" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        DISPLAY cbkmf.Descrip WITH FRAME frm-input.
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
    IF cbbctr.TranType = 2 THEN DO:
        FIND FIRST glmf WHERE glmf.acct = int(bfr{&tmptable}.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "Ledger does not exist" VIEW-AS ALERT-BOX.
            APPLY 'entry':u TO SELF.
            RETURN NO-APPLY.
         END.
    END.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input  
DO:
   IF NOT CAN-FIND(cbkmf WHERE cbkmf.Acb = INT(bfr{&tmptable}.Acb:SCREEN-VALUE)) THEN DO:
        MESSAGE "Invalid Cashbook entered .. no record saved" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END. 
    ELSE IF  bfr{&tmptable}.Acb:SCREEN-VALUE = "" THEN
        DO:
          APPLY "CLOSE":U TO THIS-PROCEDURE.
          RETURN NO-APPLY.
        END. 
        ELSE IF wsbtn = "New" THEN DO:
            CREATE  bfr{&tmptable}.
             ASSIGN  bfr{&tmptable}.Bank     = cbbctr.Bank
                     bfr{&tmptable}.intBatch = wsbatch
                     bfr{&tmptable}.seq      = INT( bfr{&tmptable}.seq:SCREEN-VALUE)
                     bfr{&tmptable}.trDATE   = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                     bfr{&tmptable}.Acb      = INT(bfr{&tmptable}.Acb:SCREEN-VALUE)
                     bfr{&tmptable}.REF      = bfr{&tmptable}.REF:SCREEN-VALUE IN FRAME frm-input
                     bfr{&tmptable}.Descrip  =  bfr{&tmptable}.Descrip:SCREEN-VALUE
                     bfr{&tmptable}.Amt      = DEC( bfr{&tmptable}.Amt:SCREEN-VALUE)
                     bfr{&tmptable}.Ledger   = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE)
                     cbBCtr.amt[1] =  cbBCtr.amt[1] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                            WHEN DEC(bfr{&tmptable}.AMT:SCREEN-VALUE) > 0 
                     cbBCtr.amt[2] =  cbBCtr.amt[2] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                           WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) < 0.
                   ASSIGN wsrec= wsrec + 1
                          wsAmt[1] = cbbctr.Amt
                          wsAmt[2] = cbbctr.Amt.
                   DISPLAY wsrec @ bfr{&tmptable}.seq  WITH FRAME frm-input.
                   DISPLAY  wsAmt[1] @ cbbctr.Amt[1] wsAmt[2] @ cbbctr.Amt[2] WITH FRAME frm-cbBatch.
                   OPEN QUERY qry-bfr{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch
                                                     AND bfr{&tmptable}.bank = wsBank NO-LOCK.
             APPLY 'entry' TO  bfr{&tmptable}.trDATE.
            RETURN.
        END.
        ELSE DO:
            ASSIGN  cbbctr.Amt[1] =  cbbctr.Amt[1] - bfr{&tmptable}.Amt[1]  WHEN Amt > 0
                    cbbctr.Amt[2] =  cbbctr.Amt[2] - bfr{&tmptable}.Amt[2]  WHEN Amt < 0
            ASSIGN  bfr{&tmptable}.intBatch = wsbatch
                    bfr{&tmptable}.seq      = INT( bfr{&tmptable}.seq:SCREEN-VALUE)
                    bfr{&tmptable}.trDATE   = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                    bfr{&tmptable}.Acb      = INT(bfr{&tmptable}.Acb:SCREEN-VALUE)
                    bfr{&tmptable}.REF      = bfr{&tmptable}.REF:SCREEN-VALUE IN FRAME frm-input
                    bfr{&tmptable}.Descrip  =  bfr{&tmptable}.Descrip:SCREEN-VALUE
                    bfr{&tmptable}.Amt      = DEC( bfr{&tmptable}.Amt:SCREEN-VALUE)
                    bfr{&tmptable}.Ledger   = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE)
                    cbBCtr.amt[1] =  cbBCtr.amt[1] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                            WHEN DEC(bfr{&tmptable}.AMT:SCREEN-VALUE) > 0 
                    cbBCtr.amt[2] =  cbBCtr.amt[2] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                           WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) < 0. 
             ASSIGN  wsAmt[1] = cbbctr.Quantity
                     wsAmt[2] = cbbctr.Cost.
            /* CLEAR FRAME frm-input ALL. */
             HIDE FRAME frm-input.
             /* VIEW FRAME frm-cbBatch. */
             DISPLAY  wsAmt[1] @ cbbctr.Amt[1] wsAmt[2] @ cbbctr.Amt[2] WITH FRAME frm-cbBatch.
             OPEN QUERY qry-bfr{&tmptable} FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch
                                                AND bfr{&tmptable}.bank = wsBank NO-LOCK.
             APPLY "CLOSE":U TO THIS-PROCEDURE.
             RETURN.
        END.
END.

/* *** Triggers to frame frm-cbBatch*** */
ON CHOOSE OF btn-Add1 IN FRAME frm-cbBatch
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

ON 'mouse-select-dblclick' OF brw-bfr{&tmptable} IN FRAME frm-cbBatch
    OR CHOOSE OF btn-edit IN FRAME frm-cbBatch
DO: 
    wsbtn = "Upd".
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsrec = int(bfr{&tmptable}.seq)
           wsid = int(bfr{&tmptable}.intbatch).
    FIND FIRST cbbctr WHERE cbbctr.intbatch  = wsid NO-ERROR.
    FIND FIRST cbkmf WHERE cbkmf.Acb = bfr{&tmptable}.Acb NO-LOCK NO-ERROR.
    VIEW FRAME frm-input.
    DISPLAY bfr{&tmptable}.Seq
            bfr{&tmptable}.trDATE 
            bfr{&tmptable}.Acb cbkmf.Descrip
            bfr{&tmptable}.DESCRIP 
            bfr{&tmptable}.ref
            bfr{&tmptable}.Amt WITH FRAME frm-input.
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.Ledger NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
        DISPLAY glmf.descrip WITH FRAME frm-input.
    ENABLE ALL EXCEPT bfr{&tmptable}.Seq WITH FRAME frm-input. 
    WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
    HIDE FRAME frm-input.
    RETURN.
END.
 
ON CHOOSE OF btn-Del IN FRAME frm-cbBatch DO:
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  bfr{&tmptable}.intbatch.
    MESSAGE "Are you sure you want to delete this transaction?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
        FIND FIRST  cbbctr WHERE  cbbctr.intbatch = wsid EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN  cbbctr.Amt[1] =  cbbctr.Amt[1] -  bfr{&tmptable}.Amt[1]
                cbbctr.Amt[2] =  cbbctr.Amt[2] -  bfr{&tmptable}.Amt[2].
         DELETE  bfr{&tmptable}.
         Method-Status = brw-bfr{&tmptable}:DELETE-SELECTED-ROWS().
         ASSIGN wsAmt[1] = cbbctr.Amt[1]
                wsAmt[2] = cbbctr.Amt[2].
         DISPLAY  wsAmt[1] @ cbbctr.Amt[1] wsAmt[2] @ cbbctr.Amt[2] WITH FRAME frm-cbBatch.
     END.
    APPLY 'entry':u TO btn-cancel.
END.


ON 'TAB' OF  wsBatch IN FRAME frm-cbBatch
    OR 'enter' OF  wsBatch IN FRAME frm-cbBatch
DO:
    IF int( wsBatch:SCREEN-VALUE) = 0 THEN DO:
        HIDE FRAME frm-cbBatch.
        APPLY 'Close' TO THIS-PROCEDURE.
    END.   
    ELSE DO:
      FIND FIRST  cbbctr WHERE  cbbctr.intbatch = int( wsBatch:SCREEN-VALUE) NO-ERROR.
      IF AVAILABLE  cbbctr THEN DO:
          MESSAGE "Batch number already exist" VIEW-AS ALERT-BOX.
          APPLY 'Close' TO THIS-PROCEDURE.
          RETURN NO-APPLY.
       END.
        ELSE
           APPLY 'TAB' TO wsDate IN FRAME frm-cbBatch.
    END.
END.

ON 'TAB' OF  wsper IN FRAME frm-cbBatch
    OR 'enter' OF  wsPer IN FRAME frm-cbBatch
DO:
    IF INT(wsper:SCREEN-VALUE) <= wscurr
        AND INT(wsper:SCREEN-VALUE) > wsClosed  THEN
    DO:
       apply 'entry' to cbbctr.TranType IN FRAME frm-cbBatch.
    END.
    ELSE DO:
         MESSAGE "The period " wsper " is outside Accounting Periods. No batch will be created"
              VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    
END.


/* ***** Triggers for the main frame **** */

ON CHOOSE OF btn-Add IN FRAME frm-main
DO:  
   wsbatch = 0.
   wsdate = TODAY.
   CLEAR FRAME frm-cbBatch.
   VIEW FRAME frm-cbBatch.
   ENABLE ALL  EXCEPT  cbbctr.Amt[1] cbbctr.Amt[2] brw-bfr{&tmptable} WITH FRAME frm-cbBatch.
   WAIT-FOR CHOOSE OF btn-cancel OR CLOSE OF THIS-PROCEDURE IN FRAME frm-cbBatch.
   CLEAR FRAME frm-cbBatch ALL.
   DISABLE ALL WITH FRAME frm-cbBatch.
   HIDE FRAME frm-cbBatch.
   VIEW FRAME frm-main.
   OPEN QUERY qry-cbbctr FOR EACH  cbbctr NO-LOCK.
     
END.
 
ON CHOOSE OF btn-Edit IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-cbbctr
DO:
    GET CURRENT qry-cbbctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsbatch =  cbbctr.intbatch.
    FIND FIRST cbbctr WHERE  cbbctr.intbatch = wsBatch EXCLUSIVE-LOCK NO-ERROR.
    CLEAR FRAME frm-cbBatch ALL.
    VIEW FRAME frm-cbBatch.
    ENABLE ALL EXCEPT  wsbatch wsDate wsPer cbbctr.Amt[1] cbbctr.Amt[2] WITH FRAME frm-cbBatch.
    DISPLAY  cbbctr.intbatch @ wsBatch cbbctr.bdate @ wsDate   cbbctr.period @ wsPer
         cbbctr.TranType cbbctr.Amt[1] cbbctr.Amt[2] WITH FRAME frm-cbBatch.
    FIND LAST  bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-ERROR.
    IF AVAILABLE  bfr{&tmptable} THEN
        wsrec =  bfr{&tmptable}.seq.
    ELSE wsrec = 0.
    OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
        WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK BY  bfr{&tmptable}.seq.
    WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-cbBatch.
    CLOSE QUERY qry-bfr{&tmptable}.
    CLEAR FRAME frm-cbBatch ALL.
    DISABLE ALL WITH FRAME frm-cbBatch.
    HIDE FRAME frm-cbBatch.
    VIEW FRAME frm-main.
    RETURN.
END.


ON CHOOSE OF btn-Del IN FRAME frm-main DO:
    GET CURRENT qry-cbbctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  cbbctr.intbatch.
    MESSAGE "Are you sure you want to delete this batch?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
         FOR EACH Cbbtf WHERE Cbbtf.intBatch = wsid:
             DELETE Cbbtf.
         END.
         DELETE  cbbctr.
         Method-Status = brw-cbbctr:DELETE-SELECTED-ROWS().
     END. 
    APPLY 'entry' TO btn-exit.
END.


/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
ASSIGN wscurr = SIMCTR.CURPER
       wsclosed = SIMCTR.CLOSEPER.
OPEN QUERY qry-cbbctr FOR EACH  cbbctr NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-cbbctr.
HIDE FRAME frm-main.

PROCEDURE proc-input:
  VIEW FRAME frm-input.
  ENABLE ALL EXCEPT  bfr{&tmptable}.seq WITH FRAME frm-input.
  DISPLAY wsRec @ bfr{&tmptable}.seq WITH FRAME frm-input.
  WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
  OPEN QUERY qry-cbbctr FOR EACH  cbbctr NO-LOCK.
  HIDE FRAME frm-input.
END.
