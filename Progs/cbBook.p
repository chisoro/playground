session:DATA-ENTRY-RETURN = TRUE.
/* Program.................cbbook.p
   Notes:...... Cashbook file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Cashbook already exist"
&SCOPED-DEFINE wsTitle           "Cashbook file Maintenance"
&SCOPED-DEFINE tmptable             cbkmf
&SCOPED-DEFINE skey                 cbkmf.Acb
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.Ledger ~
                                        COLUMN-LABEL ' Ledger ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Acb ~
                                        COLUMN-LABEL ' Cashbook ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        FORM "x(40)" COLUMN-LABEL ' Description ':C ~
                               bfr{&tmptable}.Ledger ~
                                        COLUMN-LABEL ' Ledger ':C
{varlibrary.i}
DEF VAR wsDes LIKE glmf.DESCRIPTION.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 18.5.

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 10
    btn-add AT ROW 20.7 COL 7
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 5
    rect-1 AT ROW 20 COL 5
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 90 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                  COLON 30 LABEL "Cashbook" SKIP(.5)
    bfr{&tmptable}.descrip   colon 30 LABEL "Description" FORM "x(40)" SKIP(0.5)
    btnLedger                COLON 20 
    bfr{&tmptable}.Ledger    NO-LABEl
    glmf.DESCRIPTION         NO-LABEL VIEW-AS TEXT
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    

/* ***** Triggers for the main frame **** */
/*...... Triggers for button btnLedger ....*/
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

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
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
         FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
     IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "Invalid Ledger supplied...no record saved" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
     FIND FIRST cbkmf WHERE cbkmf.Acb  = wsid NO-ERROR.
      IF AVAILABLE cbkmf THEN DO:
          MESSAGE  "Cashbook already exist" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.Acb = wsid
                 bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                 bfr{&tmptable}.Ledger  = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE).
          IF  bfr{&tmptable}.txtCur = "" THEN
                bfr{&tmptable}.txtCur = "ZWL".
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
           APPLY 'entry' TO {&skey} IN FRAME frm-Input.
      END.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
           MESSAGE "Invalid Ledger supplied...no record saved" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.Ledger  = DEC(bfr{&tmptable}.Ledger:SCREEN-VALUE).
        IF  bfr{&tmptable}.txtCur = "" THEN
                bfr{&tmptable}.txtCur = "ZWL".
        RELEASE {&tmptable}.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Acb.
    FIND FIRST cbTrans WHERE cbTrans.Acb  = bfr{&tmptable}.Acb NO-ERROR.
    IF AVAILABLE cbTrans THEN DO:
        MESSAGE "Cashbook has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE cbTrans THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Acb <> 0  NO-LOCK .
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
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Acb <> 0  NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.Acb).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Acb  = wsid NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.Ledger NO-ERROR.
    IF AVAILABLE glmf THEN
        wsdes = glmf.DESCRIPTION.
    ELSE wsDes = "Invalid Ledger".
    DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip bfr{&tmptable}.Ledger wsDes @ glmf.DESCRIPTION WITH FRAME frm-input.
    ENABLE {&updFields} btnLedger btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Acb <> 0  NO-LOCK.
   HIDE FRAME frm-input.
 END.


