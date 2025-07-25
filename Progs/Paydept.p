session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paydept.p
   Notes:...... Payroll department capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Pay Department already exist"
&SCOPED-DEFINE wsTitle           "Pay Department File Maintenance"
&SCOPED-DEFINE tmptable             paydept
&SCOPED-DEFINE skey                 paydept.Dept
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Descrip ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.prefix~
                              bfr{&tmptable}.Cost~  
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.dept ~
                                        COLUMN-LABEL 'Item ':C ~
                              bfr{&tmptable}.Descrip ~
                                        COLUMN-LABEL ' Description ' WIDTH 40~
                              bfr{&tmptable}.prefix ~
                                            COLUMN-LABEL 'GL Prefix '~
                              bfr{&tmptable}.Cost ~
                                            COLUMN-LABEL 'Cost Center ':C
                              

{varlib.i}
DEF VAR wsDesc LIKE Paydept.Descrip.

DEF BUTTON btnCost LABEL "Cost Center".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 18.5.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}  SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.
DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 5
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                 COLON 20 LABEL "Department" SKIP(0.5)
    bfr{&tmptable}.descrip  COLON 20 LABEL "Description"FORM "x(40)" SKIP(0.5)
    bfr{&tmptable}.prefix      COLON 20 LABEL "Ledger Prefix"
                HELP "Prefixed with Fund code" SKIP(0.5) 
     btnCost COLON 10 NO-TAB-STOP
     bfr{&tmptable}.cost    NO-LABEL 
     glmf.descrip NO-LABEL VIEW-AS TEXT 
     SKIP(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PAYROLL ITEM CODE MAINTENANCE".

DEF    QUERY qrycost FOR glmf SCROLLING.
DEF BROWSE brwcost QUERY qrycost
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
    brwcost AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".
    
/* ***** Triggers for the main frame **** */
ON CHOOSE OF btncost IN FRAME frm-input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qrycost FOR EACH glmf WHERE glmf.vote = 2 NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brwcost
          OR 'mouse-select-dblclick' OF brwcost.
  CLOSE QUERY qrycost.
  HIDE FRAME frm-pick.
  IF bfr{&tmptable}.cost:SCREEN-VALUE = "" THEN
       RETURN NO-APPLY.
   ELSE 
  APPLY 'tab' TO bfr{&tmptable}.cost.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brwcost
    OR 'mouse-select-dblclick' OF brwcost
DO: 
   GET CURRENT qrycost EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glmf.acct @ bfr{&tmptable}.cost glmf.DESCRIP  WITH FRAME frm-input.
   
END.

  
/* ***** Triggers for the main frame **** */
ON CHOOSE OF btn-Add IN FRAME frmMain DO:
         btn-ok:LABEL IN  FRAME frm-input = "Save".
        RUN proc-input.
        RETURN.
    END.

ON CHOOSE OF btn-edit
        OR 'enter':u OF brw{&tmptable}
        OR 'mouse-select-dblclick' OF brw{&tmptable}
    DO:
        btn-ok:LABEL IN  FRAME frm-input = "Update".
        GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        DISABLE {&skey} WITH FRAME frm-input.
        run proc-edit.
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

ON 'leave':U OF bfr{&tmptable}.cost 
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.cost:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
      DISPLAY glmf.DESCRIPTION WITH FRAME frm-input.
    ELSE DO:
         MESSAGE "Invalid Ledger, please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST Paydept WHERE Paydept.dept  = wsid NO-ERROR.
        IF AVAILABLE Paydept THEN DO:
           MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.dept = wsid
                   bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.prefix  = INT(bfr{&tmptable}.prefix:SCREEN-VALUE)
                   bfr{&tmptable}.cost    = INT(bfr{&tmptable}.cost:SCREEN-VALUE).
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.prefix  = INT(bfr{&tmptable}.prefix:SCREEN-VALUE)
                   bfr{&tmptable}.cost    = INT(bfr{&tmptable}.cost:SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw{&tmptable}:REFRESH() IN FRAME frmMain.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.dept.
    FIND FIRST payemf WHERE payemf.dept  = bfr{&tmptable}.dept NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        MESSAGE "Department has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE payemf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry{&tmptable}.
HIDE FRAME frmMain.

PROCEDURE proc-input:
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.dept).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dept  = wsid EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.cost NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
        wsDesc = glmf.Descrip.
    DISPLAY wsid @ {&skey} {&updFields} wsDesc @ glmf.Descrip  WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close btncost WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

     

