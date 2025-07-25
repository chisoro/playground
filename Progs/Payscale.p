session:DATA-ENTRY-RETURN = TRUE.
/* Program.................payscale.p
   Notes:...... Payroll scale capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Salary Scale already exist"
&SCOPED-DEFINE wsTitle           "Salary Scale File Maintenance"
&SCOPED-DEFINE tmptable             payscale
&SCOPED-DEFINE skey                 Payscale.Scale
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Notch[1] ~
                              bfr{&tmptable}.Notch[2] ~
                              bfr{&tmptable}.Notch[3] ~
                              bfr{&tmptable}.Notch[4] ~
                              bfr{&tmptable}.Notch[5] ~
                              bfr{&tmptable}.Notch[6] ~
                              bfr{&tmptable}.Notch[7] ~
                              bfr{&tmptable}.Notch[8] ~
                              bfr{&tmptable}.Notch[9] ~
                              bfr{&tmptable}.Notch[10] ~
                              bfr{&tmptable}.Notch[11] ~
                              bfr{&tmptable}.Notch[12] ~
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.scale ~
                                        COLUMN-LABEL 'SCALE':C ~
                              bfr{&tmptable}.Notch[1] ~
                              bfr{&tmptable}.Notch[2] ~
                              bfr{&tmptable}.Notch[3] ~
                              bfr{&tmptable}.Notch[4] ~
                              bfr{&tmptable}.Notch[5] ~
                              bfr{&tmptable}.Notch[6] ~
                              bfr{&tmptable}.Notch[7] ~
                              bfr{&tmptable}.Notch[8] ~
                              bfr{&tmptable}.Notch[9] ~
                              bfr{&tmptable}.Notch[10] ~
                              bfr{&tmptable}.Notch[11] ~
                              bfr{&tmptable}.Notch[12] ~
                              

{varlib.i}

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 133 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 133 by 18.5.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}  SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.
DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 5
    Space(30) btn-edit
    space(30) btn-del
    space(30) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 137 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                 COLON 20 LABEL "Scale" SKIP(0.5)
    bfr{&tmptable}.Notch[1] ~ COLON 20 LABEL "Notch 1 " bfr{&tmptable}.Notch[2]~  LABEL "Notch 2 " SKIP(0.5)
    bfr{&tmptable}.Notch[3] ~ COLON 20 LABEL "Notch 3 " bfr{&tmptable}.Notch[4]~  LABEL "Notch 4 " SKIP(0.5)
    bfr{&tmptable}.Notch[5] ~ COLON 20 LABEL "Notch 5 " bfr{&tmptable}.Notch[6]~  LABEL "Notch 6 " SKIP(0.5)
    bfr{&tmptable}.Notch[7] ~ COLON 20 LABEL "Notch 7 " bfr{&tmptable}.Notch[8]~  LABEL "Notch 8 " SKIP(0.5)
    bfr{&tmptable}.Notch[9] ~ COLON 20 LABEL "Notch 9 " bfr{&tmptable}.Notch[10]~ LABEL "Notch 10" SKIP(0.5)
    bfr{&tmptable}.Notch[11] ~ COLON 20 LABEL "Notch 11" bfr{&tmptable}.Notch[12]~ LABEL "Notch 12" SKIP(0.5)
    SKIP(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PAYROLL SALARY SCALE MAITENANCE".


/* ***** Triggers for the main frame **** */

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
     FIND FIRST payscale WHERE payscale.scale  = wsid NO-ERROR.
        IF AVAILABLE payscale THEN DO:
           MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.scale = wsid
                   bfr{&tmptable}.Notch[1] = DEC(bfr{&tmptable}.Notch[1]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[2] = DEC(bfr{&tmptable}.Notch[2]:SCREEN-VALUE) 
                   bfr{&tmptable}.Notch[3] = DEC(bfr{&tmptable}.Notch[3]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[4] = DEC(bfr{&tmptable}.Notch[4]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[5] = DEC(bfr{&tmptable}.Notch[5]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[6] = DEC(bfr{&tmptable}.Notch[6]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[7] = DEC(bfr{&tmptable}.Notch[7]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[8] = DEC(bfr{&tmptable}.Notch[8]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[9] = DEC(bfr{&tmptable}.Notch[9]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[10] = DEC(bfr{&tmptable}.Notch[10]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[11] = DEC( bfr{&tmptable}.Notch[11]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[12] = DEC(bfr{&tmptable}.Notch[12]:SCREEN-VALUE).
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Notch[1] = DEC(bfr{&tmptable}.Notch[1]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[2] = DEC(bfr{&tmptable}.Notch[2]:SCREEN-VALUE) 
                   bfr{&tmptable}.Notch[3] = DEC(bfr{&tmptable}.Notch[3]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[4] = DEC(bfr{&tmptable}.Notch[4]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[5] = DEC(bfr{&tmptable}.Notch[5]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[6] = DEC(bfr{&tmptable}.Notch[6]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[7] = DEC(bfr{&tmptable}.Notch[7]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[8] = DEC(bfr{&tmptable}.Notch[8]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[9] = DEC(bfr{&tmptable}.Notch[9]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[10] = DEC(bfr{&tmptable}.Notch[10]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[11] = DEC( bfr{&tmptable}.Notch[11]:SCREEN-VALUE)
                   bfr{&tmptable}.Notch[12] = DEC(bfr{&tmptable}.Notch[12]:SCREEN-VALUE).
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw{&tmptable}:REFRESH() IN FRAME frmMain.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.scale.
    FIND FIRST paypost WHERE paypost.grade  = bfr{&tmptable}.scale NO-ERROR.
    IF AVAILABLE paypost THEN DO:
        MESSAGE "Salary scale has related records - cannot be delete"
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
    ASSIGN wsid = int(bfr{&tmptable}.scale).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.scale  = wsid EXCLUSIVE-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} {&updFields} WITH FRAME frm-input.
    ENABLE {&updFields} btn-ok btn-close WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

     

