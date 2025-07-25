session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbptype.p
   Notes:...... Payment methods maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsTitle           "Payment Methods File Maintenance"
&SCOPED-DEFINE tmptable             dbPay
&SCOPED-DEFINE skey                 dbPay.Paytype
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C~
                              bfr{&tmptable}.bank~
                                        COLUMN-LABEL ' Bank ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Paytype ~
                                        COLUMN-LABEL ' PAY METHOD ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' DESCRIPTION ':C WIDTH 40~
                              bfr{&tmptable}.bank~
                                        COLUMN-LABEL ' BANK ':C
                              

{varlib.i}

DEF VAR wsDescrip AS CHAR FORM "x(20)".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 75 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 75 by 18.5.

DEF    QUERY qry-Bank FOR cbkmf SCROLLING.
DEF BROWSE brw-Bank  QUERY qry-Bank 
    DISPLAY cbkmf.Bank  cbkmf.descrip COLUMN-LABEL "Description" 
    WIDTH 60 cbkmf.txtCur COLUMN-LABEL "CURRENCY" WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickBank  
    brw-Bank  AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Bank Selection".

define frame frm-input
    {&skey}                   COLON 30 label "Pay Method"
    bfr{&tmptable}.descrip    COLON 30 label "Description"
    btnBank                   COLON 22 NO-TAB-STOP 
    bfr{&tmptable}.Bank       COLON 30 NO-LABEL
    SPACE(1)  cbkmf.descrip   view-as text no-label no-tab-stop
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 10
    btn-add AT ROW 20.7 COL 10
    Space(10) btn-edit
    space(10) btn-del
    space(10) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 80 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.
/* end of Common Variables */

browse brw-{&tmptable}:allow-column-searching = true.
    
ON CHOOSE OF btnBank IN FRAME frm-input
DO:
  VIEW FRAME frm-pickBank.
  OPEN QUERY qry-Bank FOR EACH cbkmf WHERE cbkmf.Bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickBank.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickBank 
          OR close of THIS-PROCEDURE IN FRAME frm-pickBank
          OR CHOOSE OF btn-ok IN FRAME frm-pickBank 
          OR 'enter':u OF brw-Bank
          OR 'mouse-select-dblclick' OF brw-Bank.
  CLOSE QUERY qry-Bank.
  HIDE FRAME frm-pickBank.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickBank 
    OR 'enter':u OF brw-Bank
    OR 'mouse-select-dblclick' OF brw-Bank
DO: 
   GET CURRENT qry-bANK EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY cbkmf.Bank @ bfr{&tmptable}.Bank cbkmf.descrip  WITH FRAME frm-input.
   RETURN.
END.
/* ***** Triggers for the main frame **** */
{trilib.i}

/*ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
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
    RETURN.
END.*/

ON LEAVE OF {&skey} IN FRAME frm-input
DO: 
     wsid = int({&skey}:SCREEN-VALUE IN FRAME frm-input).
    IF wsid = 0 THEN DO:
    MESSAGE "No value entered" VIEW-AS ALERT-BOX.
    CLEAR FRAME frm-input ALL.
    APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
END.
ELSE DO:
       FIND FIRST {&tmptable} WHERE {&tmptable}.Paytype  = wsid NO-ERROR.
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  "Payment Method already exists" VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          VIEW FRAME frm-input.
          RELEASE {&tmptable}.
          RETURN NO-APPLY.
       END.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Paytype.
    FIND FIRST dbRec WHERE dbRec.Paytype  = bfr{&tmptable}.Paytype  NO-ERROR.
    IF AVAILABLE dbRec THEN DO:
        MESSAGE "Payment Method has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbRec THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.Paytype = wsid
               bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.Bank = INT(bfr{&tmptable}.Bank:SCREEN-VALUE)
               bfr{&tmptable}.DeviceName = lc-host
               bfr{&tmptable}.Credate = TODAY 
               bfr{&tmptable}.UID     = varuser.
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.Bank = INT(bfr{&tmptable}.Bank:SCREEN-VALUE).
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

PROCEDURE proc-Edit:
   ASSIGN wsid = int(bfr{&tmptable}.Paytype).
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Paytype  = wsid NO-ERROR.
   DISPLAY wsid @ {&skey} {&UpdFields} WITH FRAME frm-input.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.


