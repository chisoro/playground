session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paytrf.p
   Notes:......Transfer employee from one system to another
   Author:.................S. Mawire
*/

DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wswsTarg  LIKE Paysys.Paysys.
DEF VAR wsEmp  LIKE payemf.empcode.
DEF VAR wsDes  LIKE paysys.descrip.
DEF VAR wsName LIKE payemf.FName.
DEF VAR wsdate AS DATE.
DEF VAR wsopt AS LOGICAL INITIAL NO.

DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-wsTarg   LABEL "Target Payroll System".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".

DEF BUFFER bfrPaysys FOR paysys.

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

DEF    QUERY qry-wsTarg FOR bfrPaysys SCROLLING.
DEF BROWSE brw-wsTarg QUERY qry-wsTarg
        DISPLAY bfrPaysys.Paysys bfrPaysys.Descrip COLUMN-LABEL "Decsription" 
        WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Targ 
    brw-wsTarg AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEFINE FRAME frm-Paysys
    SKIP(1.5)
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
    wsSys              NO-LABEL AUTO-RETURN 
    Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
    wsDate   COLON 25  LABEL "Payroll Date" VIEW-AS TEXT SKIP(0.5)          
    wsemp    COLON 26 LABEL "Employee" wsName NO-LABEL VIEW-AS TEXT SKIP(0.5)
    btn-wsTarg   COLON 10 NO-LABEL NO-TAB-STOP
    wswsTarg              NO-LABEL AUTO-RETURN  wsDes NO-LABEL VIEW-AS TEXT SKIP(0.5)
    wsopt        COLON 25 LABEL "Change Start Date?" SKIP(0.5)
    payemf.StartDate COLON 25 LABEL "Start Date" SKIP(0.5)
    btn-ok colon 20 LABEL "TRANSFER"
    btn-close colon 60
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 13
    TITLE "TRANSFER EMPLOYEES BETWEEN SYSTEMS" VIEW-AS DIALOG-BOX.


/* Triggers for the Paysys frame */

ON  'enter':U OF wsemp IN FRAME frm-Paysys
    OR 'tab':U OF wsemp IN FRAME frm-Paysys
     OR 'leave':U OF wsemp IN FRAME frm-Paysys
DO:
    FIND FIRST payemf WHERE payemf.empcode = INT(wsemp:SCREEN-VALUE)
                        AND payemf.Paysys = INT(wsSys:SCREEN-VALUE)  NO-LOCK NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        wsName = payemf.SName + " " + payemf.FName.
        DISPLAY wsName payemf.StartDate WITH FRAME frm-paysys.
        APPLY 'tab' TO SELF.
    END.
    ELSE DO:
         MESSAGE "Invalid Employee number...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-wsTarg IN FRAME frm-Paysys
    OR CHOOSE OF btn-sys IN FRAME frm-Paysys
DO:
  VIEW FRAME frm-Targ.
  OPEN QUERY qry-wsTarg FOR EACH bfrpaysys NO-LOCK.
  ENABLE ALL WITH FRAME frm-Targ.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Targ 
          OR close of THIS-PROCEDURE IN FRAME frm-Targ
          OR CHOOSE OF btn-ok IN FRAME frm-Targ 
          OR 'enter':u OF brw-wsTarg
          OR 'mouse-select-dblclick' OF brw-wsTarg.
  CLOSE QUERY qry-wsTarg.
  HIDE FRAME frm-Targ.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wswsTarg.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Targ 
    OR 'enter':u OF brw-wsTarg
    OR 'mouse-select-dblclick' OF brw-wsTarg
DO: 
   GET CURRENT qry-wsTarg NO-LOCK NO-WAIT.
   DISPLAY bfrPaysys.Paysys @ wswsTarg bfrPaysys.Descrip @ wsDes WITH FRAME frm-Paysys.
   RETURN. 
END.

ON  'enter':U OF wswsTarg IN FRAME frm-Paysys
    OR 'tab':U OF wswsTarg IN FRAME frm-Paysys
DO:
    ASSIGN wswsTarg.
    FIND FIRST bfrpaysys WHERE bfrPaysys.Paysys = INT(wswsTarg:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE bfrpaysys THEN DO:
        DISPLAY bfrPaysys.Descrip @ wsDes WITH FRAME frm-Paysys.
         APPLY 'tab' TO SELF.
    END.
    ELSE DO:
        MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

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
  DISPLAY Paysys.Paysys @ wsSys WITH FRAM frm-paysys.
  APPLY 'tab' TO wsSys.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-sys
    OR 'mouse-select-dblclick' OF brw-sys
DO: 
   GET CURRENT qry-sys NO-LOCK NO-WAIT.
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.CurDate @ wsDate WITH FRAME frm-Paysys.
    APPLY 'entry' TO wsEmp.
   RETURN. 
END.

ON  'enter':U OF wsSys IN FRAME frm-Paysys
    OR 'tab':U OF wsSys IN FRAME frm-Paysys
DO:
    ASSIGN wsSys.
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN DO:
        FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
        IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
        DO:
            APPLY 'entry' TO wsEmp.
        END.
        ELSE DO:
            MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
         
    END.  
    ELSE DO:
        MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF wsopt IN FRAME frm-Paysys 
DO:
    ASSIGN wsopt.
    IF wsopt = YES THEN DO:
        ENABLE payemf.startdate WITH FRAME frm-paysys.
        APPLY 'entry' TO  payemf.startdate.
        RETURN NO-APPLY.
    END.
        
    ELSE IF wsopt = NO THEN
        DISABLE payemf.startdate WITH FRAME frm-paysys.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
   ASSIGN wsSys = INT(wsSys:SCREEN-VALUE)
          wswsTarg = INT(wswsTarg:SCREEN-VALUE)
          wsemp = INT(wsemp:SCREEN-VALUE IN FRAME frm-Paysys).       
    FIND FIRST payemf WHERE payemf.empcode = INT(wsemp:SCREEN-VALUE)
                        AND payemf.Paysys = INT(wsSys:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        ASSIGN payemf.paysys = wswsTarg
               payemf.startdate = DATE(payemf.startdate:SCREEN-VALUE).
        MESSAGE "Employee Transfer completed......." VIEW-AS ALERT-BOX.
        RELEASE payemf.
        APPLY 'tab' TO btn-close IN FRAME frm-Paysys.
    END.
    ELSE DO:
        MESSAGE "No transfer done ....invalid data" VIEW-AS ALERT-BOX.
    END.
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST Payctr NO-LOCK NO-ERROR.
VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-Paysys.
DISABLE payemf.startdate WITH FRAME frm-paysys.
WAIT-FOR CHOOSE OF btn-close OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
RETURN.
