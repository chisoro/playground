session:DATA-ENTRY-RETURN = TRUE.
DEF NEW SHARED  VAR wsa LIKE db003.dbblf.dbAcc.
DEF NEW SHARED  VAR wsbal LIKE inc.munbmf.bal[1].
DEF NEW SHARED  VAR wsint LIKE inc.munbmf.interest[1].
DEF NEW SHARED  VAR wsbtotal LIKE inc.munbmf.bal[1].
DEF NEW SHARED  VAR wsctotal LIKE inc.munbmf.bal[1].
DEF NEW SHARED  VAR wst LIKE inc.munbmf.TYPE.
DEF NEW SHARED  VAR wsRef AS CHAR FORM "x(15)".
DEF NEW SHARED  VAR wstype AS INT FORM "99".
DEF NEW SHARED  VAR wsdif LIKE inc.munbmf.bal[1].
DEF NEW SHARED  VAR wsbt LIKE inc.mundcf.batch-no.
DEF NEW SHARED  VAR stAcc   LIKE db003.dbcmf.dbacc.
DEF NEW SHARED  VAR enAcc   LIKE db003.dbcmf.dbacc.
DEF NEW SHARED  VAR wsstatus  LIKE db003.dbcmf.dbacc.
DEF NEW SHARED  VAR wsdescrip LIKE  glmf.DESCRIPTION.
DEF NEW SHARED  VAR wsAcct LIKE  glmf.acct.
DEF NEW SHARED  VAR i AS INTEGER.
DEF NEW SHARED  VAR wsseq AS INTEGER.
DEF NEW SHARED VAR wsRate LIKE tblforex.decRate.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btn-gl    LABEL "Exchange Gain/Loss Acc".
DEF BUTTON btn-ok1     LABEL "OK".
DEF BUTTON btn-Exit     LABEL "EXIT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 14.5.

DEF    QUERY qry-PickGl FOR glmf SCROLLING.
DEF BROWSE brw-PickGl QUERY qry-PickGl
    DISPLAY glmf.acct glmf.Descrip   
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickGl 
    brw-PickGl AT ROW 2 COL 5
    skip(0.5)
    btn-ok1 colon 5
    btn-Exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "General Ledger Selection".



DEFINE FRAME frmMain
    SKIP(1)
    wsbt COLON 30 LABEL  "Ënter Batch Number" skip (.5)
    wsType COLON 30 LABEL "Enter Debit Journal Type" SKIP(.5)
    wsRef COLON 30 LABEL "Enter Reference" SKIP (.5)
    btn-gl COLON 5 NO-TAB-STOP wsAcct NO-LABEL
    wsdescrip       VIEW-AS TEXT NO-LABEL FORM "x(15)" SKIP(0.5)
    stAcc   COLON 30 LABEL "Enter Start Account" SKIP(1)
    enAcc   COLON 30 LABEL "Enter End Account" SKIP(1.5)
    wsStatus  COLON 30 LABEL  "Processing...."VIEW-AS TEXT 
    skip(3.3)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 16 COL 3
    with SIZE 100.8 BY 19 view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "Exchnage rate Gain/Loss".


/*******Tiggers*************/
ON 'enter':U OF wsbt IN FRAME frmMain 
    OR 'leave':U OF wsbt IN FRAME frmMain 
DO:
    ASSIGN wsbt.
    IF wsbt = 0 THEN DO:
        MESSAGE "Invalid batch number entered...system will close" VIEW-AS ALERT-BOX.
        APPLY 'close' TO THIS-PROCEDURE.
        HIDE FRAME frmMain.
        RETURN.
    END.
    FIND FIRST inc.muntcf WHERE inc.muntcf.batch-no = wsbt NO-LOCK NO-ERROR.
    IF AVAILABLE inc.muntcf THEN DO:
        MESSAGE "Batch number already exists please try again..." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-oK IN FRAME frmMain DO:
    ASSIGN stAcc 
           enAcc
        wstype wsbt wsref wsacct.
    session:set-wait-state("").
    RUN exgl01.p.
    RUN jnlcreate-ip.
END.

ON CHOOSE OF btn-gl IN FRAME  frmMain
DO:
  VIEW FRAME frm-PickGl.
  OPEN QUERY qry-PickGl FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickGl.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-PickGl 
          OR close of THIS-PROCEDURE IN FRAME frm-PickGl 
          OR CHOOSE OF btn-ok1 IN FRAME frm-PickGl 
          OR 'enter':u OF brw-PickGl
          OR 'mouse-select-dblclick' OF brw-PickGl.
  CLOSE QUERY qry-PickGl.
  HIDE FRAME frm-PickGl.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsAcct IN FRAME frmMain .
  RETURN. 
END.

ON CHOOSE OF btn-ok1 IN FRAME frm-PickGl 
    OR 'enter':u OF brw-PickGl 
    OR 'mouse-select-dblclick' OF brw-PickGl 
DO: 
   GET CURRENT qry-PickGl  EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY  glmf.acct  @ wsAcct glmf.Descrip @ wsdescrip WITH FRAME frmMain.
   RETURN.
END.

ON 'enter':U OF wsAcct IN FRAME frmMain
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(wsAcct:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "Invalid ledger entered.......try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE glmf THEN
        DISPLAY glmf.Descrip @ wsdescrip WITH FRAME frmMain.
    RETURN.
END.



/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frmMain.
FIND FIRST dbacc.simctr NO-LOCK NO-ERROR.
ASSIGN stAcc:SCREEN-VALUE = "0"
       enAcc:SCREEN-VALUE = "999999999998".
FIND FIRST tblforex WHERE tblforex.txtCur = simctr.dbCur NO-LOCK NO-ERROR.
wsRate = tblforex.decRate.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frmMain.
HIDE FRAME frmMain.



PROCEDURE jnlcreate-ip:
    IF wsbTotal <> 0 OR wsCTotal <> 0 THEN DO: /*CREATE BATCH*/
        CREATE inc.muntcf.
        ASSIGN 
            inc.muntcf.batch-no = wsbt
            inc.muntcf.batch-date = TODAY
            inc.muntcf.batch-type = wsType
            inc.muntcf.descrip = "Exchange Rate Gain/Loss Adjustment"
            inc.muntcf.acc-pd = dbacc.simctr.curper
            inc.muntcf.batch-status = "U"
            inc.muntcf.batch-tot  = wsbtotal
            inc.muntcf.comp-tot = wsctotal.
    END.
    MESSAGE "JOURNAL CREATION COMPLETED..." VIEW-AS ALERT-BOX.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END PROCEDURE.
