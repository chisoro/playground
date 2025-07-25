session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbsgr.p
   Notes:...... BService group fime maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg             "Services group already exist"
&SCOPED-DEFINE wsTitle           "Services File Maintenance"
&SCOPED-DEFINE tmptable             dbsgr
&SCOPED-DEFINE skey                 dbsgr.sgrp
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C~
                              bfr{&tmptable}.ctrLedger ~
                                        COLUMN-LABEL ' Ledger ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.sgrp ~
                                        COLUMN-LABEL ' Sevice ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.ctrLedger ~
                                        COLUMN-LABEL ' Ledger ':C~
                              bfr{&tmptable}.IntLedger ~
                                        COLUMN-LABEL ' Interest Ledger ':C

DEF VAR wsb AS CHAR.
{varlibrary.i}
DEF VAR iDes AS CHAR FORM "x(40)".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 18.5.

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 5
    Space(15) btn-edit
    space(15) btn-del
    space(15) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                   COLON 30 LABEL "Service" skip(0.5)
    bfr{&tmptable}.descrip    COLON 30 LABEL "Description" skip(0.5)
    btnLedger                 COLON 18 NO-TAB-STOP 
    bfr{&tmptable}.ctrLedger   NO-LABEL
    SPACE(1) ADes  view-as text no-label NO-TAB-STOP 
    skip(0.5)
    btnDept                 COLON 13  no-tab-stop
    bfr{&tmptable}.Dept              NO-LABEL
    SPACE(1) DDes  view-as text no-label NO-TAB-STOP skip(0.5)
    btnProj                 COLON 17  no-tab-stop
    bfr{&tmptable}.Proj              NO-LABEL
    SPACE(1) PDes  view-as text no-label NO-TAB-STOP skip(0.5)
    btnInt                 COLON 8 NO-TAB-STOP 
    bfr{&tmptable}.IntLedger   NO-LABEL 
    SPACE(1) iDes  view-as text no-label NO-TAB-STOP
    skip(2.5)
    btn-ok colon 20
    btn-close colon 60 SKIP(.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

/* ***** Triggers for on-frame buttons **** */    
ON CHOOSE OF btnLedger IN FRAME frm-input
DO:
  wsb = "Ctr".
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
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickDept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept EXCLUSIVE-LOCK NO-WAIT.
   DDes = gldept.descrip.
   DISPLAY gldept.dept @ bfr{&tmptable}.dept DDes  WITH FRAME frm-input.
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

ON 'enter':U OF bfr{&tmptable}.dept 
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(bfr{&tmptable}.dept:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE gldept THEN DO:
        MESSAGE "Invalid Department entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DDes = gldept.descrip.
        DISPLAY DDes WITH FRAME frm-input.
    END.
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
         PDes = glproj.descrip.
        DISPLAY PDes WITH FRAME frm-input.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
    PDes = glproj.descrip.
   DISPLAY glProj.Proj @ bfr{&tmptable}.Proj PDes  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger EXCLUSIVE-LOCK NO-WAIT.
    ADes = glmf.DESCRIPTION.
   IF wsb = "ctr" THEN
           DISPLAY glmf.acct @ bfr{&tmptable}.ctrLedger ADes  WITH FRAME frm-input.
   ELSE IF wsb = "Int" THEN
           DISPLAY glmf.acct @ bfr{&tmptable}.IntLedger  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnInt IN FRAME frm-input
DO:
    wsb = "Int".
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
/* ***** Triggers for the main frame **** */
{trilib.i}

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.sgrp.
    FIND FIRST dbtmf WHERE dbtmf.sgrp  = bfr{&tmptable}.sgrp  NO-ERROR.
    IF AVAILABLE dbtmf THEN DO:
        MESSAGE "Service has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbcmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'tab':U OF bfr{&tmptable}.ctrLedger 
    OR 'enter':U OF bfr{&tmptable}.ctrLedger 
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.ctrLedger:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "invalid ledger" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-LOCK NO-ERROR.
        IF AVAILABLE gldept THEN
            DDes = gldept.DESCRIP.
        FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
        IF AVAILABLE glproj THEN
            PDes = glProj.DESCRIP.
        DISPLAY glmf.Dept @ bfr{&tmptable}.Dept DDes glmf.Proj @ bfr{&tmptable}.Proj 
            PDes ADes WITH FRAME frm-input.
        IF glmf.dept <> 0 THEN
            DISABLE btnDept bfr{&tmptable}.Dept WITH FRAME frm-Input.
        ELSE  ENABLE btnDept bfr{&tmptable}.Dept WITH FRAME frm-Input.
        IF glm.proj <> 0 THEN
            DISABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
        ELSE ENABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
    END.
    RETURN.
END.

ON 'tab':U OF bfr{&tmptable}.IntLedger 
    OR 'enter':U OF bfr{&tmptable}.IntLedger
DO:
    FIND FIRST glmf WHERE glmf.acct = INT(bfr{&tmptable}.IntLedger:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "invalid ledger" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF AVAILABLE glmf THEN DO:
        IF glmf.dept <> INT(bfr{&tmptable}.dept:SCREEN-VALUE) AND glmf.proj <> 0 THEN DO:
            MESSAGE "Interest Ledger must be within the same department with Control Ledger" 
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        IF glmf.proj <> INT(bfr{&tmptable}.proj:SCREEN-VALUE) AND glmf.proj <> 0 THEN DO:
            MESSAGE "Interest Ledger must be within the same Project with Control Ledger" 
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DISPLAY glmf.descrip @ iDes WITH FRAME frm-Input.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.sgrp = wsid
               bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.ctrLedger = DEC(bfr{&tmptable}.ctrLedger:SCREEN-VALUE)
               bfr{&tmptable}.IntLedger = DEC(bfr{&tmptable}.IntLedger:SCREEN-VALUE)
               bfr{&tmptable}.Dept = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
               bfr{&tmptable}.Proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE).
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.ctrLedger = DEC(bfr{&tmptable}.ctrLedger:SCREEN-VALUE)
               bfr{&tmptable}.intLedger = DEC(bfr{&tmptable}.intLedger:SCREEN-VALUE)
               bfr{&tmptable}.Dept = INT(bfr{&tmptable}.Dept:SCREEN-VALUE)
               bfr{&tmptable}.Proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE).
        RELEASE {&tmptable}.
        APPLY 'close' TO SELF.
        HIDE FRAME frm-input.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END. 
END.
/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

{Proclib.i}

PROCEDURE proc-Edit:
   ASSIGN wsid = bfr{&tmptable}.sgrp.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.sgrp = wsid EXCLUSIVE-LOCK NO-ERROR.
   FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.ctrLedger NO-ERROR.
   IF AVAILABLE glmf THEN
       ADes = glmf.DESCRIP.
   FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-LOCK NO-ERROR.
   IF AVAILABLE gldept THEN
       DDes = gldept.DESCRIP.
   FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
   IF AVAILABLE glproj THEN
       PDes = glProj.DESCRIP.
   FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.IntLedger NO-ERROR.
   IF AVAILABLE glmf THEN
       iDes = glmf.DESCRIP.
   DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip bfr{&tmptable}.ctrLedger bfr{&tmptable}.IntLedger
        iDes ADes bfr{&tmptable}.Dept bfr{&tmptable}.Proj 
       PDes DDes WITH FRAME frm-input.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
   IF glmf.dept <> 0 THEN
            DISABLE btndept bfr{&tmptable}.dept WITH FRAME frm-input.
       IF glmf.proj <> 0 THEN
            DISABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-input.
   DISPLAY wsid @ {&skey} bfr{&tmptable}.Descrip WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.
