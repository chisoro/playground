session:DATA-ENTRY-RETURN = TRUE.
/* Program.................cbbank.p
   Notes:...... Bank file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Bank file already exist"
&SCOPED-DEFINE wsTitle           "Bank file Maintenance"
&SCOPED-DEFINE tmptable             cbkmf
&SCOPED-DEFINE skey                 cbkmf.bank
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' ACCOUNT NAME ':C 
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.bank ~
                                        COLUMN-LABEL ' BANK ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' ACCOUNT NAME':C WIDTH 50 ~
                              bfr{&tmptable}.Ledger ~
                                        COLUMN-LABEL ' Ledger ':C ~
                              bfr{&tmptable}.txtcur ~
                                        COLUMN-LABEL ' CURRENCY ':C

{varlib.i}
DEF VAR wscur LIKE bfr{&tmptable}.txtCur.
DEF BUTTON btnCur      LABEL "CURRENCY".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.


DEF    QUERY qry-ledger FOR glmf SCROLLING.
DEF BROWSE brw-ledger QUERY qry-ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 5
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 100 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.
/* end of Common Variables */

define frame frm-input
    {&skey}                  COLON 30 label "BANK" SKIP(0.5)
    bfr{&tmptable}.descrip   COLON 30 label "DESCRIPTION" SKIP(0.5)
    btnLedger                COLON 18 
    bfr{&tmptable}.Ledger    NO-LABEl
    glmf.DESCRIPTION         NO-LABEL VIEW-AS TEXT SKIP(0.5)
    btnCur                   COLON 16 NO-LABEL NO-TAB-STOP wscur NO-LABEL    
    skip(1.5)
    btn-ok colon 10
    btn-close colon 60 SKIP(1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

DEF QUERY qry-Cur FOR tblForex SCROLLING.
DEF BROWSE brw-cur QUERY qry-cur
    DISPLAY tblForex.txtCur FORM "x(10)"
     WITH NO-LABEL 6 DOWN SEPARATORS.

DEFINE FRAME frm-Cur 
    brw-Cur AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Currency Selection".
    
/* ***** Triggers for the main frame **** */
{trilib.i}

/* ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RELEASE bfr{&tmptable}.
    RETURN.
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF brw-{&tmptable}
    OR 'mouse-select-dblclick' OF brw-{&tmptable}
DO:
    btn-ok:LABEL IN  FRAME frm-input = "Update".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DISABLE {&skey} WITH FRAME frm-input.
    run proc-edit.
    RELEASE bfr{&tmptable}.
    RETURN.
END. */

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

ON CHOOSE OF btnLedger IN FRAME frm-input
    OR CHOOSE OF btnLedger IN FRAME frm-input
DO:
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
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger NO-LOCK NO-WAIT.
   DISPLAY glmf.acct @ bfr{&tmptable}.Ledger glmf.DESCRIPTION WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.Ledger IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Ledger IN FRAME frm-input
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
        DISPLAY glmf.DESCRIPTION  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
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
  APPLY 'tab' TO wsCur.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-Cur 
    OR 'enter':u OF brw-Cur
    OR 'mouse-select-dblclick' OF brw-Cur
DO: 
   GET CURRENT qry-Cur EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY tblForex.txtCur @ wsCur WITH FRAME frm-input.
   APPLY 'TAB' TO wsCur IN FRAME frm-input.
END.

ON 'tab':U OF wsCur IN FRAME frm-input
   OR 'enter':U OF wsCur IN FRAME frm-input
DO:
    FIND FIRST tblForex WHERE tblForex.txtCur = wsCur:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tblForex THEN DO:
        MESSAGE "Invalid Currency.....please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE ASSIGN wsCur = wsCur:SCREEN-VALUE.
    RETURN.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.bank.
    FIND FIRST cbTrans WHERE cbTrans.bank  = bfr{&tmptable}.bank NO-ERROR.
    IF AVAILABLE cbTrans THEN DO:
        MESSAGE "Bank has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE cbTrans THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    ASSIGN wscur.
    IF  wscur:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Cashbook Currency required" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         FIND FIRST cbkmf WHERE cbkmf.Bank  = wsid NO-ERROR.
         IF AVAILABLE cbkmf THEN DO:
                MESSAGE  "Bank already exist" VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
         END.
         ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.bank = wsid
                   bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.Ledger = DEC( bfr{&tmptable}.Ledger:SCREEN-VALUE)
                   bfr{&tmptable}.txtCur = wsCur
                   bfr{&tmptable}.DeviceName = lc-host
                   bfr{&tmptable}.Credate = TODAY 
                   bfr{&tmptable}.UID     = varuser.
            RELEASE bfr{&tmptable}.
            brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
         END.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-Input
               bfr{&tmptable}.txtCur = wsCur
               bfr{&tmptable}.Ledger = DEC( bfr{&tmptable}.Ledger:SCREEN-VALUE).
        RELEASE bfr{&tmptable}.
        APPLY 'close' TO SELF.
        HIDE FRAME frm-input.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.BANK <> 0  NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.BANK <> 0  NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   ASSIGN {&skey}:SCREEN-VALUE = "0"
          wscur:SCREEN-VALUE = "".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.BANK <> 0  NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.bank).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.bank  = wsid NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.Ledger NO-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip bfr{&tmptable}.Ledger bfr{&tmptable}.txtCur @ wscur 
            glmf.DESCRIPTION WITH FRAME frm-input.
    ENABLE {&updFields} btnCur wscur btn-ok btn-close btnLedger bfr{&tmptable}.Ledger  WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.BANK <> 0  NO-LOCK.
   HIDE FRAME frm-input.
 END.


