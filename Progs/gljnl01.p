session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable        glb
&SCOPED-DEFINE tmpFields       bfr{&tmptable}.trDATE ~
                                        COLUMN-LABEL ' Transaction Date ':C ~
                               bfr{&tmptable}.REF ~
                                        COLUMN-LABEL ' Reference ':C ~
                               bfr{&tmptable}.acct ~
                                        COLUMN-LABEL ' Account ':C ~
                               bfr{&tmptable}.Dept ~
                                        COLUMN-LABEL ' Department ':C ~
                               bfr{&tmptable}.Proj ~
                                        COLUMN-LABEL ' Project ':C ~
                               bfr{&tmptable}.AMT ~
                                        COLUMN-LABEL ' Amount ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C

DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsid LIKE  glbCtr.intBatch.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wsdel          AS LOGICAL INITIAL NO.
DEF VAR wsbatch     LIKE  glbCtr.intBatch.
DEF VAR wsrec       LIKE  glb.recno.
DEF VAR wscurr      LIKE SIMCTR.CURPER.
DEF VAR wsclosed    LIKE  SIMCTR.CLOSEPER.
DEF VAR wsper       LIKE simctr.closeper.
DEF VAR wsDate      LIKE glbCtr.BDate.
DEF VAR wsAmt       LIKE glbCtr.Amt.
DEF VAR wsVote      LIKE glvote.descrip.
DEF VAR wsbtn AS CHAR.
DEF BUTTON btn-Add   LABEL "ADD".
DEF BUTTON btn-Add1   LABEL "ADD".
DEF BUTTON btn-del   LABEL "DELETE".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-Edit  LABEL "EDIT".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btn-ok    LABEL "OK".
DEF BUTTON btnAcc    LABEL "Leddger Allocation".
DEF BUTTON btnDept   LABEL "Department".
DEF BUTTON btnProj   LABEL "Project".
DEF BUTTON btnFund   LABEL "Segment/Fund".

DEF BUFFER bfr{&tmptable} FOR glb PRESELECT.
DEF BUFFER bfglbCtr       FOR glbCtr PRESELECT.

DEFINE QUERY qry-bfglbCtr FOR  bfglbCtr scrolling.
DEF BROWSE brw-bfglbCtr QUERY qry-bfglbCtr
        DISPLAY UID COLUMN-LABEL "User" 
                period WIDTH 10 COLUMN-LABEL "Period"
                BDate WIDTH 12 COLUMN-LABEL "Batch Date"
                intBatch COLUMN-LABEL "Batch Number" 
                AMT[1]COLUMN-LABEL "Dr Amount" 
                AMT[2] COLUMN-LABEL "Cr Amount"
        WITH 20 DOWN SEPARATORS.

DEFINE QUERY qry-bfr{&tmptable} FOR  bfr{&tmptable} scrolling.
DEF BROWSE brw-bfr{&tmptable} QUERY qry-bfr{&tmptable}
    DISPLAY bfr{&tmptable}.RecNo  
            bfr{&tmptable}.trDATE  
            bfr{&tmptable}.REF  
            bfr{&tmptable}.acct  
            bfr{&tmptable}.AMT DESCRIPTION WIDTH 50
    WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-Ledger FOR glmf SCROLLING.
DEF BROWSE brw-Ledger QUERY qry-Ledger
     DISPLAY wsvote glmf.acct glmf.DESCRIPTION WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-pickDept FOR glDept SCROLLING.
DEF BROWSE brw-pickDept QUERY qry-pickDept
    DISPLAY GLDept.Dept GLDept.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickDept 
    brw-pickDept AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Department Selection".

DEF    QUERY qry-Fund FOR GlFund SCROLLING.
DEF BROWSE brw-Fund  QUERY qry-Fund 
    DISPLAY GLFUND.FUND COLUMN-LABEL "Segment" GLFUND.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 50 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-pickFund 
    brw-Fund AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Segment/Fund Selection".

DEF    QUERY qry-PickProj FOR glProj SCROLLING.
DEF BROWSE brw-PickProj QUERY qry-PickProj
    DISPLAY GLProj.Proj GLProj.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickProj 
    brw-PickProj AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Project Selection".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 17.5.

DEFINE RECTANGLE rect-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 118.5 BY 2.3.

DEFINE RECTANGLE rect-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 118.5 BY 18.5.

DEFINE FRAME frm-input
      wsrec                 COLON 20  LABEL "Record " SKIP(0.5)
      bfr{&tmptable}.trDATE COLON 20  LABEL "Transaction Date " SKIP(0.5)
      bfr{&tmptable}.REF    COLON 20  LABEL  "Reference Number " SKIP(0.5)
      btnAcc     COLON 2  NO-TAB-STOP 
      bfr{&tmptable}.acct   COLON 21   NO-LABEL 
      glmf.DESCRIPTION view-as text NO-LABEL SKIP(0.5)
      btnDept                 COLON 8  no-tab-stop
      bfr{&tmptable}.Dept              NO-LABEL
      SPACE(1) gldept.descrip  view-as text no-label NO-TAB-STOP skip(0.5)
      btnFund                 COLON 5  no-tab-stop
      bfr{&tmptable}.Fund              NO-LABEL
      SPACE(1) glfund.descrip  view-as text no-label NO-TAB-STOP skip(0.5)
      btnProj                 COLON 12  no-tab-stop
      bfr{&tmptable}.Proj              NO-LABEL
      SPACE(1) glProj.descrip  view-as text no-label NO-TAB-STOP skip(0.5)
      bfr{&tmptable}.AMT    COLON 20  LABEL "Amount " SKIP(0.5)
      bfr{&tmptable}.DESCRIPTION COLON 20 LABEL "Narration " HELP "Enter Transaction Narration"
      SKIP(1)
      btn-ok colon 5 
      btn-cancel colon 60 
    with view-as dialog-box KEEP-TAB-ORDER no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    
DEF FRAME frm-main
    brw-bfglbCtr AT ROW 2 COL 5
    btn-add AT ROW 19.7 COL 5 SPACE(15)
    btn-edit SPACE(15) 
    btn-del SPACE(15)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 90 BY 23
    TITLE "Journal Maintenance" view-as dialog-box.

DEF FRAME frm-glb
    wsBatch   LABEL "Batch Number" AT ROW 2 COL 5 SPACE(2)
    wsDate    LABEL "Date"   SPACE(2) 
    wsper     LABEL "Accounting Period"  SPACE(2)
    wsAmt[1]   LABEL "DR"  NO-TAB-STOP space(2) 
    wsAmt[2]   LABEL "CR" NO-TAB-STOP SPACE(2) 
    brw-bfr{&tmptable} NO-TAB-STOP AT ROW 3.5 COL 3.5 
    btn-add1 AT ROW 20.7 COL 10
    space(25) btn-edit
    space(25) btn-del
    space(25) btn-cancel SKIP(1)
    rect-4 AT ROW 1.4 COL 3
    rect-3 AT ROW 20 COL 3
    WITH TITLE "Journal Data Capture"
    1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 130 BY 24 view-as DIALOG-BOX.

DEFINE FRAME frm-Ledger 
     brw-Ledger AT ROW 1 COL 1.5 skip(0.5)
     btn-Ok COLON 8 SPACE (20) btn-exit 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Ledger Selection".

/******* Triggers for button btnAcc ******/
ON CHOOSE OF btnAcc IN FRAME frm-input
DO:
  VIEW FRAME frm-input.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Ledger.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-Ledger 
          OR close of THIS-PROCEDURE IN FRAME frm-Ledger
          OR CHOOSE OF btn-ok IN FRAME frm-Ledger 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-Ledger.
  APPLY 'tab' TO btnAcc.
  APPLY 'entry' TO bfr{&tmptable}.Acct.
  RETURN. 
END. 

ON 'tab':U OF bfr{&tmptable}.acct 
    OR 'enter':U OF bfr{&tmptable}.acct 
DO:
   IF  DEC(bfr{&tmptable}.acct:SCREEN-VALUE) = 0 THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.acct:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "invalid ledger" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-LOCK NO-ERROR.
            FIND FIRST glfund WHERE glfund.fund = glmf.fund NO-LOCK NO-ERROR.
            FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
            DISPLAY glmf.Dept @ bfr{&tmptable}.Dept glmf.fund @ bfr{&tmptable}.fund
                glDept.descrip glmf.Proj @ bfr{&tmptable}.Proj 
                glFund.descrip glProj.descrip glmf.descrip WITH FRAME frm-input.
            IF glmf.dept <> 0 THEN
                DISABLE btnDept bfr{&tmptable}.Dept WITH FRAME frm-Input.
            ELSE DO:
                ENABLE btnDept bfr{&tmptable}.Dept WITH FRAME frm-Input.
                APPLY 'entry' TO  bfr{&tmptable}.dept IN FRAME frm-Input.
            END.
            IF glmf.fund <> 0 THEN
                DISABLE btnFund bfr{&tmptable}.fund WITH FRAME frm-Input.
            ELSE DO:
                ENABLE btnFund bfr{&tmptable}.Fund WITH FRAME frm-Input.
                APPLY 'entry' TO  bfr{&tmptable}.Fund IN FRAME frm-Input.
            END.
            IF glmf.proj <> 0 THEN
                DISABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
            ELSE DO:
                 ENABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
                APPLY 'entry' TO  bfr{&tmptable}.Proj IN FRAME frm-Input.
            END.      
        END.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-Ledger 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger NO-WAIT.
   DISPLAY glmf.acct @  bfr{&tmptable}.acct glmf.DESCRIPTION WITH FRAME frm-input.
END.

ON CHOOSE OF btnDept IN FRAME frm-input
DO:
  VIEW FRAME frm-pickDept.
  OPEN QUERY qry-PickDept FOR EACH glDept NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickDept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickDept 
          OR close of THIS-PROCEDURE IN FRAME frm-pickDept
          OR CHOOSE OF btn-ok IN FRAME frm-pickDept 
          OR 'enter':u OF brw-PickDept
          OR 'mouse-select-dblclick' OF brw-PickDept.
  CLOSE QUERY qry-PickDept.
  HIDE FRAME frm-pickDept.
  APPLY 'tab' TO btnDept IN FRAME frm-input.
  APPLY 'entry' TO bfr{&tmptable}.dept.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickDept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY gldept.dept @ bfr{&tmptable}.dept gldept.DESCRIP  WITH FRAME frm-input.
   APPLY 'tab' TO btnDept IN FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.dept 
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(bfr{&tmptable}.dept:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE gldept THEN DO:
        MESSAGE "Invalid Department entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY gldept.Descrip WITH FRAME frm-input.
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
  APPLY 'tab' TO btnProj IN FRAME frm-input.
  APPLY 'entry' TO bfr{&tmptable}.proj.
  RETURN. 
END.

ON 'enter':U OF bfr{&tmptable}.proj 
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(bfr{&tmptable}.proj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glproj.Descrip WITH FRAME frm-input.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ bfr{&tmptable}.Proj glProj.DESCRIPTION  WITH FRAME frm-input.
   RETURN.
END.


ON CHOOSE OF btnFund IN FRAME frm-input
DO:
  VIEW FRAME frm-pickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-pickFund
          OR CHOOSE OF btn-ok IN FRAME frm-pickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-pickFund.
  APPLY 'tab' TO btnFund IN FRAME frm-input.
  APPLY 'entry' TO bfr{&tmptable}.Fund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund NO-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfr{&tmptable}.Fund glFund.DESCRIP  WITH FRAME frm-input.
   APPLY 'tab' TO btnFund IN FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Fund 
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(bfr{&tmptable}.Fund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Segment/Fund entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.Descrip WITH FRAME frm-input.
        APPLY 'tab' TO bfr{&tmptable}.Fund IN FRAME frm-input.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input  
DO:
    IF  DEC( bfr{&tmptable}.acct:SCREEN-VALUE) = 0 THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END. 
    ELSE DO:
        IF wsbtn = "New" THEN DO:
            CREATE  bfr{&tmptable}.
             ASSIGN  bfr{&tmptable}.UID      = varUser
                     bfr{&tmptable}.intBatch = wsbatch
                     bfr{&tmptable}.recno    = INT(wsrec:SCREEN-VALUE)
                     bfr{&tmptable}.acct     = DEC( bfr{&tmptable}.acct:SCREEN-VALUE)
                     bfr{&tmptable}.Dept = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
                     bfr{&tmptable}.fund = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                     bfr{&tmptable}.Proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
                     bfr{&tmptable}.AMT      = DEC( bfr{&tmptable}.AMT:SCREEN-VALUE)
                     bfr{&tmptable}.REF      =   bfr{&tmptable}.REF:SCREEN-VALUE
                     bfr{&tmptable}.trDATE   = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                     bfr{&tmptable}.Descrip   =  bfr{&tmptable}.Descrip:SCREEN-VALUE.
             /*FIND FIRST bfglbCtr WHERE bfglbCtr.intBatch = wsId EXCLUSIVE-LOCK NO-ERROR. */
             ASSIGN bfglbCtr.amt[1] =  bfglbCtr.amt[1] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                            WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) > 0 
                     bfglbCtr.amt[2] =  bfglbCtr.amt[2] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                           WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) < 0. 
                   ASSIGN wsrec= wsrec + 1
                          wsAmt[1] = bfglbCtr.amt[1]
                          wsAmt[2] = bfglbCtr.amt[2].
                   FIND CURRENT bfglbCtr.
                   DISPLAY wsrec WITH FRAME frm-input.
                   DISPLAY  wsAmt[1] wsAmt[2] WITH FRAME frm-glb.
                   OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
                        WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK.
             APPLY 'entry' TO  bfr{&tmptable}.trDATE.
            RETURN.
        END.
        ELSE DO:
            FIND FIRST bfglbCtr WHERE bfglbCtr.intBatch = wsId EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN  bfglbCtr.amt[1] =  bfglbCtr.amt[1] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.AMT > 0
                    bfglbCtr.amt[2] =  bfglbCtr.amt[2] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.amt < 0.
            ASSIGN   bfr{&tmptable}.UID      = varUser
                     bfr{&tmptable}.acct     = DEC( bfr{&tmptable}.acct:SCREEN-VALUE)
                     bfr{&tmptable}.Dept = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
                     bfr{&tmptable}.fund = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                     bfr{&tmptable}.Proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
                     bfr{&tmptable}.AMT      = DEC( bfr{&tmptable}.AMT:SCREEN-VALUE)
                     bfr{&tmptable}.REF      =   bfr{&tmptable}.REF:SCREEN-VALUE
                     bfr{&tmptable}.trDATE   = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                     bfr{&tmptable}.Descrip   =  bfr{&tmptable}.Descrip:SCREEN-VALUE
                     bfglbCtr.amt[1] =  bfglbCtr.amt[1] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                            WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) > 0 
                     bfglbCtr.amt[2] =  bfglbCtr.amt[2] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                           WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) < 0.
             ASSIGN wsAmt[1] = bfglbCtr.amt[1]
                    wsAmt[2] = bfglbCtr.amt[2].
             FIND CURRENT bfglbCtr.
            /* CLEAR FRAME frm-input ALL. */
             HIDE FRAME frm-input.
             /* VIEW FRAME frm-glb. */
             DISPLAY  wsAmt[1] wsAmt[2] WITH FRAME frm-glb.
             OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
                        WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK.
             APPLY "CLOSE":U TO THIS-PROCEDURE.
             RETURN.
          END.
    END.   
END.


/* *** Triggers to frame frm-glb*** */
ON CHOOSE OF btn-Add1 IN FRAME frm-glb
 DO:
    wsbtn = "New".
    OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
               WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK. 
    RUN proc-input.
    RETURN.
END.

ON 'mouse-select-dblclick' OF brw-bfr{&tmptable} IN FRAME frm-glb
    OR CHOOSE OF btn-edit IN FRAME frm-glb
DO: 
    wsbtn = "Upd".
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsrec = int(bfr{&tmptable}.Recno)
           wsid = int(bfr{&tmptable}.intbatch).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Recno  = wsrec 
        AND bfr{&tmptable}.intbatch  = wsid  EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST gldept WHERE gldept.dept = bfr{&tmptable}.dept NO-LOCK NO-ERROR.
    FIND FIRST glfund WHERE glfun.fund = bfr{&tmptable}.fund NO-LOCK NO-ERROR.
    FIND FIRST glproj WHERE glproj.proj = bfr{&tmptable}.proj NO-LOCK NO-ERROR.
    VIEW FRAME frm-input.
    DISPLAY  wsrec bfr{&tmptable}.trDATE bfr{&tmptable}.REF  
     btnAcc bfr{&tmptable}.acct  bfr{&tmptable}.AMT bfr{&tmptable}.DESCRIPTION
     btnProj bfr{&tmptable}.Proj btnDept  bfr{&tmptable}.Dept  btnFund bfr{&tmptable}.fund  
     gldept.descrip glfund.descrip glproj.descrip WITH FRAME frm-input.
    ENABLE ALL EXCEPT wsrec  WITH FRAME frm-input. 
    WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
    HIDE FRAME frm-input.
    RETURN.
END.
 
ON CHOOSE OF btn-Del IN FRAME frm-glb DO:
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  bfr{&tmptable}.intbatch.
    MESSAGE "Are you sure you want to delete this transaction?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
        FIND FIRST  bfglbCtr WHERE  bfglbCtr.intbatch = wsid EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN  bfglbCtr.amt[1] =  bfglbCtr.amt[1] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.AMT > 0
                bfglbCtr.amt[2] =  bfglbCtr.amt[2] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.amt < 0.
         DELETE  bfr{&tmptable}.
         Method-Status = brw-bfr{&tmptable}:DELETE-SELECTED-ROWS().
         ASSIGN wsAmt[1] = bfglbCtr.amt[1]
                wsAmt[2] = bfglbCtr.amt[2].
         DISPLAY  wsAmt[1] wsAmt[2] WITH FRAME frm-glb.
         FIND CURRENT  bfglbCtr.
     END.
    APPLY 'entry':u TO btn-cancel.
END.


ON 'TAB' OF  wsBatch IN FRAME frm-glb
    OR 'enter' OF  wsBatch IN FRAME frm-glb
DO:
    IF int( wsBatch:SCREEN-VALUE) = 0 THEN DO:
        HIDE FRAME frm-glb.
        APPLY 'Close' TO THIS-PROCEDURE.
    END.   
    ELSE DO:
      FIND FIRST  bfglbCtr WHERE  bfglbCtr.intbatch = int( wsBatch:SCREEN-VALUE) NO-ERROR.
      IF AVAILABLE  bfglbCtr THEN DO:
          MESSAGE "Batch number already exist" VIEW-AS ALERT-BOX.
          APPLY 'Close' TO THIS-PROCEDURE.
          RETURN NO-APPLY.
       END.
        ELSE
           APPLY 'TAB' TO wsDate IN FRAME frm-glb.
    END.
   
END.

ON 'TAB' OF  wsper IN FRAME frm-glb
    OR 'enter' OF  wsPer IN FRAME frm-glb
DO:
    IF INT(wsper:SCREEN-VALUE) <= wscurr
        AND INT(wsper:SCREEN-VALUE) > wsClosed  THEN
    DO:
        CREATE  bfglbCtr.
        ASSIGN  bfglbCtr.UID      = varUser
                wsbatch         = INT( wsBatch:SCREEN-VALUE)
                bfglbCtr.intbatch = INT( wsBatch:SCREEN-VALUE)
                bfglbCtr.bdate    = DATE( wsdate:SCREEN-VALUE)
                bfglbCtr.period   = INT( wsper:SCREEN-VALUE).
        wsId = wsbatch.
        apply 'entry' to btn-Add1 IN FRAME frm-glb.
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
   VIEW FRAME frm-glb.
   ENABLE ALL  EXCEPT  wsAmt[1] wsAmt[2] brw-bfr{&tmptable} WITH FRAME frm-glb.
   WAIT-FOR CHOOSE OF btn-cancel OR CLOSE OF THIS-PROCEDURE IN FRAME frm-glb.
   CLEAR FRAME frm-glb ALL.
   DISABLE ALL WITH FRAME frm-glb.
   HIDE FRAME frm-glb.
   VIEW FRAME frm-main.
   IF simusr.supervisor = YES OR simusr.usercode = "1" THEN
      OPEN QUERY qry-bfglbCtr FOR EACH  bfglbCtr NO-LOCK.
   ELSE
      OPEN QUERY qry-bfglbCtr FOR EACH  bfglbCtr WHERE bfglbCtr.UID  = varuser NO-LOCK.
     
END.
 
ON CHOOSE OF btn-Edit IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-bfglbCtr
DO:
    GET CURRENT qry-bfglbCtr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsbatch =  bfglbCtr.intbatch.
    FIND FIRST bfglbCtr WHERE bfglbCtr.intbatch = wsBatch EXCLUSIVE-LOCK NO-WAIT NO-ERROR. 
    IF NOT AVAILABLE bfglbCtr THEN DO:
        MESSAGE "Someone is working on this Batch please try later" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        CLEAR FRAME frm-glb ALL.
        VIEW FRAME frm-glb.
        DISPLAY  bfglbCtr.intbatch @ wsBatch bfglbCtr.bdate @ wsDate   bfglbCtr.period @ wsPer
             bfglbCtr.AMT[1]  @ wsAmt[1] bfglbCtr.AMT[2] @ wsAmt[2] WITH FRAME frm-glb.
        ENABLE ALL EXCEPT  wsbatch wsDate wsPer wsAmt[1] wsAmt[2] WITH FRAME frm-glb.
        FIND LAST  bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-ERROR.
        IF AVAILABLE  bfr{&tmptable} THEN
            wsrec =  bfr{&tmptable}.recno.
        ELSE wsrec = 0.
        OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
            WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK BY  bfr{&tmptable}.recno.
        WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-glb.
        CLOSE QUERY qry-bfr{&tmptable}.
        CLEAR FRAME frm-glb ALL.
        DISABLE ALL WITH FRAME frm-glb.
        HIDE FRAME frm-glb.
        VIEW FRAM frm-main.
    END.
    RETURN.
END.


ON CHOOSE OF btn-Del IN FRAME frm-main DO:
    GET CURRENT qry-bfglbCtr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  bfglbCtr.intbatch.
    MESSAGE "Are you sure you want to delete this batch?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
         IF bfglbCtr.UID <> varUser AND varUser <> "1" THEN DO:
            MESSAGE "You are not an Administrator, you can only delete your batches"
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
             FOR EACH glb WHERE glb.intbatch = bfglbCtr.intbatch:
                 DELETE glb.
             END.
             DELETE  bfglbCtr.
             Method-Status = brw-bfglbCtr:DELETE-SELECTED-ROWS().
         END. 
     END.
    APPLY 'entry' TO btn-exit.
END.


/********** MAIN LOGIC **********/
FIND FIRST simctr NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
ASSIGN wscurr = SIMCTR.CURPER
       wsclosed = SIMCTR.CLOSEPER.
IF simusr.supervisor = YES OR simusr.usercode = "1" THEN
   OPEN QUERY qry-bfglbCtr FOR EACH  bfglbCtr NO-LOCK.
ELSE
   OPEN QUERY qry-bfglbCtr FOR EACH  bfglbCtr WHERE bfglbCtr.UID  = varuser NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-bfglbCtr.
HIDE FRAME frm-main.

PROCEDURE proc-input: 
  FIND LAST  bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-ERROR.
  IF AVAILABLE  bfr{&tmptable} THEN
       wsrec =  bfr{&tmptable}.recno.
  ELSE wsrec = 0.
  wsrec = wsrec + 1.
  VIEW FRAME frm-input.
  ENABLE ALL EXCEPT  wsrec WITH FRAME frm-input.
  DISPLAY wsrec WITH FRAME frm-input.
  WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
  IF simusr.supervisor = YES OR simusr.usercode = "1" THEN
     OPEN QUERY qry-bfglbCtr FOR EACH  bfglbCtr NO-LOCK.
  ELSE
     OPEN QUERY qry-bfglbCtr FOR EACH  bfglbCtr WHERE bfglbCtr.UID  = varuser NO-LOCK.
  HIDE FRAME frm-input.
END.
