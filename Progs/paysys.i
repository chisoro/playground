
DEF VAR X AS INT.
DEF VAR wsNotch AS DEC.
DEF VAR wsDate AS DATE.
DEF VAR st-per LIKE gltdf.period.

DEF BUTTON btnJob LABEL "OCCUPATION".
DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF VAR wsSys  LIKE Paysys.Paysys.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
        DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
        WIDTH 60 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-pick 
    brw-Sys AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEFINE FRAME frm-Paysys
    SKIP(1.5)
    btn-Sys   COLON 10 NO-LABEL 
    wsSys     NO-LABEL AUTO-RETURN 
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(2.5)
    btn-ok colon 10
    btn-close colon 50
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 70 BY 8
    TITLE "PAYROLL SELECTION" VIEW-AS DIALOG-BOX.

/* Triggers for the Paysys frame */
ON CHOOSE OF btn-sys IN FRAME frm-Paysys
    OR CHOOSE OF btn-sys IN FRAME frm-Paysys
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-sys FOR EACH paysys NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-sys
          OR 'mouse-select-dblclick' OF brw-sys.
  CLOSE QUERY qry-sys.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsSys.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-sys
    OR 'mouse-select-dblclick' OF brw-sys
DO: 
   GET CURRENT qry-sys NO-LOCK NO-WAIT.
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-Paysys.
   ASSIGN wsSys = INT(wsSys:SCREEN-VALUE).
   RETURN. 
END.

ON  'enter':U OF wsSys IN FRAME frm-Paysys
    OR 'tab':U OF wsSys IN FRAME frm-Paysys
DO:
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN
        DISPLAY Paysys.Descrip  WITH FRAM frm-Paysys.
    ELSE DO:
        MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN DO:
        HIDE FRAME frm-Paysys.
        FIND FIRST PAYCTR NO-LOCK NO-ERROR.
        /* OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.paysys = wsSys NO-LOCK .
       ENABLE ALL WITH FRAME frmMain.
        WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
        CLOSE QUERY qry{&tmptable}.
        HIDE FRAME frmMain. */
        APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-Paysys.
    END.
    ELSE DO:
        MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
        APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-Paysys.
    END.
END.

/********** MAIN LOGIC **********/
VIEW FRAME frm-Paysys.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
