session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paybnk.p
   Notes:...... Payroll BANK capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Pay Bank already exist"
&SCOPED-DEFINE wsTitle           "Pay Bank File Maintenance"
&SCOPED-DEFINE tmptable             paybnk
&SCOPED-DEFINE skey                 paybnk.Bank
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.BankName ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.BCode FORM "99999"~ 
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.Bank ~
                                        COLUMN-LABEL 'BANK' FORM "99999":C ~
                              bfr{&tmptable}.BankName~
                                        COLUMN-LABEL ' BANK NAME ' WIDTH 40~
                              bfr{&tmptable}.BCode ~
                                            COLUMN-LABEL 'TREASURY CODE ':C~
                    
                              

{varlib.i}
DEF VAR wsDesc LIKE paybnk.BankName.

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
    {&skey}                 COLON 20 FORM "99999" LABEL "BANK" SKIP(0.5)
    bfr{&tmptable}.BankName  COLON 20 LABEL "BANK NAME"FORM "x(40)" SKIP(0.5)
    bfr{&tmptable}.BCode      COLON 20 FORM "99999" LABEL "TREASURY CODE" SKIP(0.5) 
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PAYROLL BANK CODE MAINTENANCE".

   
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

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST paybnk WHERE paybnk.Bank  = wsid NO-ERROR.
        IF AVAILABLE paybnk THEN DO:
           MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.Bank = wsid
                   bfr{&tmptable}.BankName = bfr{&tmptable}.BankName :SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.BCode  = INT(bfr{&tmptable}.BCode:SCREEN-VALUE).
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.BankName = bfr{&tmptable}.BankName:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.BCode  = INT(bfr{&tmptable}.BCode:SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw{&tmptable}:REFRESH() IN FRAME frmMain.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Bank.
    FIND FIRST payemf WHERE payemf.Bank  = bfr{&tmptable}.Bank NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        MESSAGE "Bank has related records - cannot be delete"
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
    ASSIGN wsid = int(bfr{&tmptable}.Bank).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Bank  = wsid EXCLUSIVE-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} {&updFields} WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

     

