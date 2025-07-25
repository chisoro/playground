/* Program.................Gldept.p
   Notes:................. Department  File Maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE wsMsg           "Department  file already exist"
&SCOPED-DEFINE wsTitle           "Department File Maintenance"
&SCOPED-DEFINE tmptable             Gldept
&SCOPED-DEFINE skey                 Gldept.dept
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.fund ~
                                        COLUMN-LABEL ' Fund ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.fund ~
                                        COLUMN-LABEL ' Fund ':C ~
                              bfr{&tmptable}.dept ~
                                        COLUMN-LABEL ' Department ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                         WIDTH 52 COLUMN-LABEL ' Description ':C

{varlibrary.i}
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 78 by 18.5.

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 7
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
    {&skey}                     colon 30 label "Department"
    bfr{&tmptable}.DESCRIP  colon 30 label "Description"skip(0.5)
    btnFund                COLON 20 NO-TAB-STOP
    bfr{&tmptable}.Fund       NO-LABEl
    Glfund.DESCRIP          NO-LABEL VIEW-AS TEXT skip(1.5)
    btn-ok colon 5
    btn-close colon 50
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

/* ***** Triggers for the main frame **** */
{trilib.i}
ON CHOOSE OF btnFund IN FRAME frm-input
    OR CHOOSE OF btnFund IN FRAME frm-input
DO:
  VIEW FRAME frm-pickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-pickFund
          OR CHOOSE OF btn-ok IN FRAME frm-pickFund 
          OR 'enter':u OF brw-fund
          OR 'mouse-select-dblclick' OF brw-fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-pickFund.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickFund 
    OR 'enter':u OF brw-fund
    OR 'mouse-select-dblclick' OF brw-fund
DO: 
   GET CURRENT qry-Fund NO-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfr{&tmptable}.Fund glFund.DESCRIP WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.Fund IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.Fund IN FRAME frm-input
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(bfr{&tmptable}.Fund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glFund THEN
        DISPLAY glFund.DESCRIP  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dept  = wsid NO-ERROR.
         IF AVAILABLE bfr{&tmptable} THEN DO:
           MESSAGE  "Department already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}.
            ASSIGN bfr{&tmptable}.dept = wsid
                   bfr{&tmptable}.fund = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                   bfr{&tmptable}.DESCRIP = bfr{&tmptable}.DESCRIP:SCREEN-VALUE IN FRAME frm-input.
            OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.fund = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                bfr{&tmptable}.DESCRIP = bfr{&tmptable}.DESCRIP:SCREEN-VALUE IN FRAME frm-input.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Dept.
    FIND FIRST glmf WHERE glmf.dept  = bfr{&tmptable}.dept NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        MESSAGE "Department has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE glmf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = DEC(bfr{&tmptable}.dept).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dept  = wsid NO-ERROR.
    FIND FIRST Glfund WHERE glfund.fund = bfr{&tmptable}.fund NO-ERROR.
    DISPLAY wsid @ {&skey} bfr{&tmptable}.DESCRIP  bfr{&tmptable}.fund glfund.descrip 
        WITH FRAME frm-input.
    
    ENABLE {&updFields} BtnFund btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

 PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.
