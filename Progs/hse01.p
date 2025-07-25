session:DATA-ENTRY-RETURN = TRUE.
/* Program.................hse01.p
   Notes:...... Scheme file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Scheme already exist"
&SCOPED-DEFINE wsTitle           "LAND SALES SCHEME FILE MAINTENANCE"
&SCOPED-DEFINE tmptable           hsesch
&SCOPED-DEFINE skey               hsesch.scheme
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.Freq ~
                                        COLUMN-LABEL ' Frequency ':C ~
                              bfr{&tmptable}.CLedger ~
                                        COLUMN-LABEL ' Capital Ledger ':C ~
                              bfr{&tmptable}.ILedger ~
                                        COLUMN-LABEL ' Income Ledger ':C ~
                              bfr{&tmptable}.IntLedger ~
                                        COLUMN-LABEL ' Interest Ledger ':C ~
                              bfr{&tmptable}.ExLedger ~
                                        COLUMN-LABEL ' Exchange Gain/Loss ':C ~
                              bfr{&tmptable}.CtrLedger ~
                                        COLUMN-LABEL ' Debtor Control ':C ~
                              bfr{&tmptable}.Bank ~
                                        COLUMN-LABEL ' Bank Control ':C ~
                              bfr{&tmptable}.IntRate  ~
                                        COLUMN-LABEL ' Interest Rate ':C ~
                              bfr{&tmptable}.txtCur ~
                                        COLUMN-LABEL ' Trading Currency ':C
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.scheme ~
                                        COLUMN-LABEL ' Code ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        FORM "x(40)" COLUMN-LABEL ' Description ':C ~
                               bfr{&tmptable}.CLedger ~
                                        COLUMN-LABEL ' Capital Ledger ':C ~
                              bfr{&tmptable}.ILedger ~
                                        COLUMN-LABEL ' Income Ledger ':C ~
                              bfr{&tmptable}.IntLedger ~
                                        COLUMN-LABEL ' Interest Ledger ':C ~
                              bfr{&tmptable}.ExLedger ~
                                        COLUMN-LABEL ' Exchange Gain/Loss ':C ~
                              bfr{&tmptable}.CtrLedger ~
                                        COLUMN-LABEL ' Debtor Control ':C ~
                               bfr{&tmptable}.Bank ~
                                        COLUMN-LABEL ' Bank Control ':C ~
                              bfr{&tmptable}.IntRate  ~
                                        COLUMN-LABEL ' Interest Rate ':C ~
                              bfr{&tmptable}.txtCur ~
                                        COLUMN-LABEL ' Trading !Currency ':C
{varlibrary.i}
DEF VAR wsDes LIKE glmf.DESCRIPTION EXTENT 6.
DEF VAR X AS INT.
DEF VAR wsLedger LIKE glmf.acct.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 160 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 160 by 19.5.

DEFINE RECTANGLE rect-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 120 by 18.5.

DEFINE RECTANGLE rect-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 120 by 2.3.

DEF BUTTON btnLedger1.  
DEF BUTTON btnLedger2. 
DEF BUTTON btnLedger3. 
DEF BUTTON btnLedger4. 
DEF BUTTON btnLedger5. 
DEF BUTTON btnLedger6. 
DEF BUTTON btnCur      LABEL "Trading Currency".

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 21.7 COL 20
    Space(30) btn-edit
    space(30) btn-del
    space(30) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 21 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     BGCOLOR 8 FGCOLOR 1 SIZE 165 BY 25
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    SKIP(1.5)
    {&skey}                 COLON 36.5 LABEL "Scheme" SKIP(0.5)
    bfr{&tmptable}.descrip  COLON 36.5 LABEL "Description" FORM "x(40)" SKIP(0.5)
    bfr{&tmptable}.Freq     COLON 36.5 LABEL "Billing Frequency" SKIP(0.5)
    btnCur                  COLON 18.5 NO-LABEL 
    bfr{&tmptable}.txtCur              NO-LABEL SKIP(0.5)
    bfr{&tmptable}.IntRate  COLON 36.5 LABEL "Interest Rate" SKIP(0.5)
    btnLedger1              COLON 20 LABEL " Capital Ledger " NO-TAB-STOP
    bfr{&tmptable}.CLedger           NO-LABEl
    wsDes[1]                         NO-LABEL VIEW-AS TEXT SKIP(.5)
    btnLedger2              COLON 20 LABEL " Income Ledger " NO-TAB-STOP
    bfr{&tmptable}.ILedger           NO-LABEl
    wsDes[2]                         NO-LABEL VIEW-AS TEXT SKIP(.5)
    btnLedger3              COLON 20 LABEL " Interest Ledger " NO-TAB-STOP
    bfr{&tmptable}.intLedger         NO-LABEl
    wsDes[3]                         NO-LABEL VIEW-AS TEXT SKIP(.5)
    btnLedger4              COLON 8.5 LABEL " Exchange Gain/Loss Ledger " NO-TAB-STOP
    bfr{&tmptable}.ExLedger          NO-LABEl
    wsDes[4]                         NO-LABEL VIEW-AS TEXT SKIP(.5)
    btnLedger5              COLON 13 LABEL " Debtors Control Ledger " NO-TAB-STOP
    bfr{&tmptable}.CtrLedger           NO-LABEl
    wsDes[5]                         NO-LABEL VIEW-AS TEXT SKIP(.5)
    btnLedger6              COLON 15 LABEL " Bank Control Ledger " NO-TAB-STOP
    bfr{&tmptable}.Bank           NO-LABEl
    wsDes[6]                         NO-LABEL VIEW-AS TEXT
    rect-3 AT ROW 1.4 COL 3
    rect-4 AT ROW 20 COL 3
    btn-ok AT ROW 20.7 COL 20 SPACE(50)
    btn-close 
    SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "LAND SALES SCHEME DATA CAPTURE".
    

/* ***** Triggers **** */
ON 'enter':U OF bfr{&tmptable}.Freq
DO:
    APPLY 'tab' TO SELF.
    RETURN.
END.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.

ON CHOOSE OF btnLedger1  IN FRAME frm-input
DO:
   ASSIGN X = 1.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.CLedger.
END.
ON CHOOSE OF btnLedger2  IN FRAME frm-input 
DO:
   ASSIGN X = 2.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.ILedger.
END.
ON CHOOSE OF btnLedger3  IN FRAME frm-input
DO:
   ASSIGN X = 3.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.IntLedger.
END.
ON CHOOSE OF btnLedger4  IN FRAME frm-input
DO:
   ASSIGN X = 4.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.ExLedger.
END.

ON CHOOSE OF btnLedger5  IN FRAME frm-input
DO:
   ASSIGN X = 5.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.CtrLedger.
END.

ON CHOOSE OF btnLedger6  IN FRAME frm-input
DO:
   ASSIGN X = 6.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.Bank.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger NO-LOCK NO-WAIT.
   wsLedger = glmf.acct.
   APPLY 'enter' TO SELF.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.CLedger IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.CLedger IN FRAME frm-input
DO:
    IF wsLedger <> 0 THEN bfr{&tmptable}.CLedger:SCREEN-VALUE = STRING(wsledger).
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.CLedger:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[1] = glmf.DESCRIPTION.
         DISPLAY glmf.acct @ bfr{&tmptable}.CLedger wsDes[1]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    wsLedger = 0.
    RETURN.
END.
ON  'enter':U OF bfr{&tmptable}.ILedger IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.ILedger IN FRAME frm-input
DO:
    IF wsLedger <> 0 THEN bfr{&tmptable}.ILedger:SCREEN-VALUE = STRING(wsledger).
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.ILedger:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[2] = glmf.DESCRIPTION.
         DISPLAY glmf.acct @ bfr{&tmptable}.ILedger wsdes[2]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    wsLedger = 0.
    RETURN.
END.
ON  'enter':U OF bfr{&tmptable}.IntLedger IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.IntLedger IN FRAME frm-input
DO:
    IF wsLedger <> 0 THEN bfr{&tmptable}.IntLedger:SCREEN-VALUE = STRING(wsledger).
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.IntLedger:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[3] = glmf.DESCRIPTION.
         DISPLAY glmf.acct @ bfr{&tmptable}.IntLedger wsdes[3]  WITH FRAM frm-input.
    END.
    ELSE IF wsLedger <> 0 THEN DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    wsLedger = 0.
    RETURN.
END.
ON  'enter':U OF bfr{&tmptable}.ExLedger IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.ExLedger IN FRAME frm-input
DO:
    IF wsLedger <> 0 THEN bfr{&tmptable}.ExLedger:SCREEN-VALUE = STRING(wsledger).
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.ExLedger:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[4] = glmf.DESCRIPTION.
         DISPLAY glmf.acct @ bfr{&tmptable}.ExLedger wsdes[4]  WITH FRAM frm-input.
    END.
    ELSE IF wsLedger <> 0 THEN DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    wsLedger = 0.
    RETURN.
END.

ON  'enter':U OF bfr{&tmptable}.CtrLedger IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.CtrLedger IN FRAME frm-input
DO:
    IF wsLedger <> 0 THEN bfr{&tmptable}.CtrLedger:SCREEN-VALUE = STRING(wsledger).
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.CtrLedger:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[5] = glmf.DESCRIPTION.
         DISPLAY glmf.acct @ bfr{&tmptable}.CtrLedger wsdes[5]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    wsLedger = 0.
    RETURN.
END.

ON  'enter':U OF bfr{&tmptable}.Bank IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Bank IN FRAME frm-input
DO:
    IF wsLedger <> 0 THEN bfr{&tmptable}.Bank:SCREEN-VALUE = STRING(wsledger).
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.Bank:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[6] = glmf.DESCRIPTION.
         DISPLAY glmf.acct @ bfr{&tmptable}.Bank wsdes[6]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    wsLedger = 0.
    RETURN.
END.

ON CHOOSE OF btn-Add IN FRAME frm-main
DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
     wsbtn= "ADD".
    RUN proc-input.
    RELEASE bfr{&tmptable}.
    RETURN.
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF brw-{&tmptable}
    OR 'mouse-select-dblclick' OF brw-{&tmptable}
DO:
    wsbtn = "EDIT".
    btn-ok:LABEL IN  FRAME frm-input = "Update".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DISABLE {&skey} WITH FRAME frm-input.
    run proc-edit.
    RELEASE bfr{&tmptable}.
    RETURN.
END.

ON LEAVE OF {&skey} IN FRAME frm-input
DO: 
     wsid = int({&skey}:SCREEN-VALUE IN FRAME frm-input).
   IF wsid = 0 THEN DO:
        MESSAGE "No value entered" VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          RETURN NO-APPLY.
       END.
    END.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.CLedger:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
         IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "Invalid Ledger supplied...no record saved" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
          FIND FIRST hsesch WHERE hsesch.Scheme  = wsid NO-ERROR.
          IF AVAILABLE hsesch THEN DO:
              MESSAGE  "Scheme already exist" VIEW-AS ALERT-BOX.
              RETURN NO-APPLY.
          END.
      ELSE DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.Scheme    = wsid
                 bfr{&tmptable}.Descrip   = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                 bfr{&tmptable}.CLedger   = DEC(bfr{&tmptable}.CLedger:SCREEN-VALUE)
                 bfr{&tmptable}.ILedger   = DEC(bfr{&tmptable}.ILedger:SCREEN-VALUE)
                 bfr{&tmptable}.IntLedger = DEC(bfr{&tmptable}.IntLedger:SCREEN-VALUE)
                 bfr{&tmptable}.ExLedger  = DEC(bfr{&tmptable}.ExLedger:SCREEN-VALUE)
                 bfr{&tmptable}.CtrLedger = DEC(bfr{&tmptable}.CtrLedger:SCREEN-VALUE)
                 bfr{&tmptable}.bank     = DEC(bfr{&tmptable}.Bank:SCREEN-VALUE)
                 bfr{&tmptable}.Freq      = bfr{&tmptable}.Freq:SCREEN-VALUE
                 bfr{&tmptable}.IntRate   = DEC(bfr{&tmptable}.IntRate:SCREEN-VALUE)
                 bfr{&tmptable}.txtCur    = bfr{&tmptable}.txtCur:SCREEN-VALUE
                 bfr{&tmptable}.DeviceName = lc-host
                 bfr{&tmptable}.Credate = TODAY 
                 bfr{&tmptable}.UID     = varuser.
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
      END.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.CLedger:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
           MESSAGE "Invalid Ledger supplied...no record saved" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ASSIGN bfr{&tmptable}.Descrip   = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                 bfr{&tmptable}.CLedger   = DEC(bfr{&tmptable}.CLedger:SCREEN-VALUE)
                 bfr{&tmptable}.ILedger   = DEC(bfr{&tmptable}.ILedger:SCREEN-VALUE)
                 bfr{&tmptable}.IntLedger = DEC(bfr{&tmptable}.IntLedger:SCREEN-VALUE)
                 bfr{&tmptable}.ExLedger  = DEC(bfr{&tmptable}.ExLedger:SCREEN-VALUE)
                 bfr{&tmptable}.CtrLedger = DEC(bfr{&tmptable}.CtrLedger:SCREEN-VALUE)
                 bfr{&tmptable}.bank     = DEC(bfr{&tmptable}.Bank:SCREEN-VALUE)
                 bfr{&tmptable}.Freq      = bfr{&tmptable}.Freq:SCREEN-VALUE
                 bfr{&tmptable}.IntRate   = DEC(bfr{&tmptable}.IntRate:SCREEN-VALUE)
                 bfr{&tmptable}.txtCur    = bfr{&tmptable}.txtCur:SCREEN-VALUE.
       RELEASE {&tmptable}.
       brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Scheme.
    FIND FIRST hsecmf WHERE hsecmf.Scheme  = bfr{&tmptable}.Scheme NO-ERROR.
    IF AVAILABLE hsecmf THEN DO:
        MESSAGE "Scheme has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE hsecmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON CHOOSE OF btnCur IN FRAME frm-input
DO:
  VIEW FRAME frm-Cur.
  OPEN QUERY qry-Cur FOR EACH tblforex NO-LOCK.
  ENABLE ALL WITH FRAME frm-Cur.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-Cur 
          OR close of THIS-PROCEDURE IN FRAME frm-Cur
          OR CHOOSE OF btn-ok IN FRAME frm-Cur 
          OR 'enter':u OF brw-Cur
          OR 'mouse-select-dblclick' OF brw-Cur.
  CLOSE QUERY qry-Cur.
  HIDE FRAME frm-Cur.
  APPLY 'tab' TO btnCur.
  APPLY 'tab' TO bfr{&tmptable}.txtCur.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-Cur 
    OR 'enter':u OF brw-Cur
    OR 'mouse-select-dblclick' OF brw-Cur
DO: 
   GET CURRENT qry-Cur EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY tblForex.txtCur @ bfr{&tmptable}.txtCur WITH FRAME frm-input.
   APPLY 'TAB' TO bfr{&tmptable}.txtCur IN FRAME frm-input.
END.

ON 'tab':U OF bfr{&tmptable}.txtCur IN FRAME frm-input
   OR 'enter':U OF bfr{&tmptable}.txtCur IN FRAME frm-input
DO:
    FIND FIRST tblForex WHERE tblForex.txtCur = bfr{&tmptable}.txtCur:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tblForex THEN DO:
        MESSAGE "Invalid Currency.....please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
   RETURN.
END.

{audit.i}
/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Scheme <> 0  NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Scheme <> 0  NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.Scheme).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Scheme  = wsid NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.CLedger NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
       wsDes[1] = glmf.DESCRIPTION.
    ELSE wsDes[1] = "Invalid Ledger".
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.ILedger NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
       wsDes[2] = glmf.DESCRIPTION.
    ELSE wsDes[2] = "Invalid Ledger".
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.IntLedger NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
       wsDes[3] = glmf.DESCRIPTION.
    ELSE wsDes[3] = "Invalid Ledger".
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.ExLedger NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
       wsDes[4] = glmf.DESCRIPTION.
    ELSE wsDes[4] = "Invalid Ledger".
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.CtrLedger NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
       wsDes[5] = glmf.DESCRIPTION.
    ELSE wsDes[5] = "Invalid Ledger".
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.Bank NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
       wsDes[6] = glmf.DESCRIPTION.
    ELSE wsDes[6] = "Invalid Ledger".
        
    DISPLAY wsid @ {&skey} {&updfields} wsDes  WITH FRAME frm-input.
    ENABLE ALL EXCEPT {&skey} WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Scheme <> 0  NO-LOCK.
   HIDE FRAME frm-input.
 END.

PROCEDURE btn-t:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
  RETURN.
END.

