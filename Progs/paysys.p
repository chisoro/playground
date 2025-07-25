session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paysys.p
   Notes:...... Payroll systems
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Payroll System already exist"
&SCOPED-DEFINE wsTitle           "Payroll System File Maintenance"
&SCOPED-DEFINE tmptable             paysys
&SCOPED-DEFINE skey                   Paysys.Paysys
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Descrip ~
                              bfr{&tmptable}.txtCur~
                              bfr{&tmptable}.CurPer~
                              bfr{&tmptable}.CurDate~
                              bfr{&tmptable}.PerClosed ~
                              bfr{&tmptable}.ClosedDate~
                              bfr{&tmptable}.Period~
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.paysys ~
                                        COLUMN-LABEL 'Code ':C ~
                              bfr{&tmptable}.Descrip ~
                                        COLUMN-LABEL ' Description ' WIDTH 40~
                              bfr{&tmptable}.CurPer~
                                  COLUMN-LABEL 'Current Period '~
                              bfr{&tmptable}.CurDate~
                                  COLUMN-LABEL 'Current Date '~
                              bfr{&tmptable}.PerClosed ~
                                  COLUMN-LABEL 'Period Closed '~
                             bfr{&tmptable}.Period ~
                                  COLUMN-LABEL 'Tax Periods ':C
                              

{varlib.i}

DEF BUTTON btnCur      LABEL "CURRENCY".

DEF QUERY qry-Cur FOR tblForex SCROLLING.
DEF BROWSE brw-cur QUERY qry-cur
DISPLAY tblForex.txtCur FORM "x(10)" WITH NO-LABEL 6 DOWN SEPARATORS.

DEFINE FRAME frm-Cur 
    brw-Cur AT ROW 2 COL 5 skip(0.5)
    btn-ok colon 5
    btn-close colon 60 
with view-as dialog-box keep-tab-order no-validate
side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Currency Selection".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 102 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 102 by 18.5.
DEFINE RECTANGLE rect3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 65 by 13.

DEFINE RECTANGLE rect4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 65 by 2.3.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}   SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.
DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 10
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN  OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 106 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    SKIP(1)
    {&skey}                   COLON 20   LABEL "System" SKIP(0.5)
    bfr{&tmptable}.descrip    COLON 20   LABEL "Description"FORM "x(40)" SKIP(0.5)
     btnCur    COLON 6  bfr{&tmptable}.txtCur  NO-LABEL SKIP(0.5)
    bfr{&tmptable}.CurDate    COLON 20   LABEL "Current Date" SKIP(0.5)
    bfr{&tmptable}.CurPer     COLON 20    LABEL "Current Period" SKIP(0.5)
    bfr{&tmptable}.ClosedDate COLON 20 LABEL "Last Closed Date" SKIP(0.5)
    bfr{&tmptable}.PerClosed  COLON 20 LABEL "Last Closed Period" SKIP(0.5)
    bfr{&tmptable}.Period     COLON 20 LABEL "Number Tax Period" SKIP(0.5)
    SKIP(0.5)
    btn-ok colon 5
    btn-close colon 50 SKIP(1)
    rect3 AT ROW 1.0 COL 2
    rect4 AT ROW 14 COL 2
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PAYROLL SYSTEM CODE MAINTENANCE".


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
     FIND FIRST Paysys WHERE Paysys.paysys  = wsid NO-ERROR.
        IF AVAILABLE Paysys THEN DO:
           MESSAGE  "Pay Item already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.paysys = wsid
                   bfr{&tmptable}.Descrip    = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.CurDate    = DATE(bfr{&tmptable}.CurDate:SCREEN-VALUE)
                   bfr{&tmptable}.txtCur     = bfr{&tmptable}.txtCur:SCREEN-VALUE
                   bfr{&tmptable}.Curper     = INT(bfr{&tmptable}.curPer:SCREEN-VALUE)
                   bfr{&tmptable}.ClosedDate = DATE(bfr{&tmptable}.closedDate:SCREEN-VALUE)
                   bfr{&tmptable}.PerClosed  = INT(bfr{&tmptable}.Perclosed:SCREEN-VALUE)
                   bfr{&tmptable}.Period  = INT(bfr{&tmptable}.Period:SCREEN-VALUE).
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE  IN FRAME frm-input
                bfr{&tmptable}.Period  = INT(bfr{&tmptable}.Period:SCREEN-VALUE)
                bfr{&tmptable}.txtCur  = bfr{&tmptable}.txtCur:SCREEN-VALUE.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw{&tmptable}:REFRESH() IN FRAME frmMain.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.paysys.
    FIND FIRST payemf WHERE Payemf.paysys  = bfr{&tmptable}.paysys NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        MESSAGE "Payroll system has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE payemf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw{&tmptable}:DELETE-SELECTED-ROWS().
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

/********** MAIN LOGIC **********/
OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable}   NO-LOCK BY  bfr{&tmptable}.Paysys .
ENABLE ALL WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry{&tmptable}.
HIDE FRAME frmMain.

PROCEDURE proc-input:
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK BY  bfr{&tmptable}.Paysys.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK BY  bfr{&tmptable}.Paysys.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.paysys).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.paysys  = wsid EXCLUSIVE-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} {&updFields} WITH FRAME frm-input.
    DISABLE ALL WITH FRAME frm-input.
    ENABLE bfr{&tmptable}.Descrip bfr{&tmptable}.Period bfr{&tmptable}.txtCur btnCur btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK BY  bfr{&tmptable}.Paysys.
   HIDE FRAME frm-input.
 END.

     

