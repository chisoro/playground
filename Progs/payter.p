
SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payter.p
    Notes:...... ..........Capture terminations
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR varUser       LIKE simusr.usercode INITIAL 1.
DEF VAR wsSys         LIKE Paysys.Paysys.
DEF VAR wsDate        AS DATE .
DEF VAR eStat         LIKE payemf.eStatus.
DEF VAR wsCo          AS CHAR FORM "x(100)".
DEF VAR wsEmp         LIKE payemf.empcode.
DEF VAR wsDes         AS CHAR FORM "x(50)".
DEF VAR wsTransId     AS DEC FORM "99999999999999".
 
 DEF BUTTON btn-Sys   LABEL "Payroll System".
 DEF BUTTON btn-emp   LABEL "Employee".
 DEF BUTTON btn-exit  LABEL "CLOSE".
 DEF BUTTON btn-close LABEL "CLOSE".
 DEF BUTTON btn-ok    LABEL "OK".

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 8.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
         DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-emp FOR Payemf SCROLLING.
DEF BROWSE brw-emp QUERY qry-emp
         DISPLAY Payemf.empcode Payemf.SName Payemf.FName 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Pick 
     brw-Sys AT ROW 2 COL 5
     skip(0.5)
     btn-ok colon 5
     btn-close colon 60
     with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEFINE FRAME frm-Emp
     brw-Emp AT ROW 2 COL 5
     skip(0.5)
     btn-ok colon 5
     btn-close colon 60
     with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Employee System Selection".

 DEFINE FRAME frm-Paysys
     SKIP(1.5)
     btn-Sys   COLON 6 NO-LABEL NO-TAB-STOP
     wsSys              NO-LABEL AUTO-RETURN 
     Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     btn-emp    COLON 10 NO-LABEL NO-TAB-STOP
     wsEmp              NO-LABEL
     wsDes              NO-LABEL VIEW-AS TEXT SKIP(0.5)
     eStat      COLON 21.5 LABEL "Change status to" 
                VIEW-AS COMBO-BOX LIST-ITEM-PAIRS "Active", 1, "Suspend with Pay", 2, "Suspend without Pay", 3,
                "Terminated", 4 SKIP(0.5)
     wsDate     COLON 21.5 LABEL "Status Date" SKIP(2.5)
     btn-ok colon 20
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 10.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 13
     TITLE "EMPLOYEE TERMINATIONS " VIEW-AS DIALOG-BOX.


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
    DISPLAY Paysys.Paysys @ wsSys WITH FRAME frm-Paysys.
    RETURN. 
 END.

ON  'enter':U OF wsSys IN FRAME frm-Paysys
     OR 'tab':U OF wsSys IN FRAME frm-Paysys
 DO:
    ASSIGN wsSys = INT(wsSys:SCREEN-VALUE).
     FIND FIRST paysys WHERE Paysys.Paysys = wsSys NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = wsSys) THEN
         DO:
             DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-Paysys.
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

 ON CHOOSE OF btn-emp IN FRAME frm-Paysys
     OR CHOOSE OF btn-emp IN FRAME frm-Paysys
 DO:
   VIEW FRAME frm-emp.
   OPEN QUERY qry-emp FOR EACH payemf WHERE payemf.paysys = wsSys AND payemf.leaver = NO NO-LOCK.
   ENABLE ALL WITH FRAME frm-emp.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-emp 
           OR close of THIS-PROCEDURE IN FRAME frm-emp
           OR CHOOSE OF btn-ok IN FRAME frm-emp 
           OR 'enter':u OF brw-emp
           OR 'mouse-select-dblclick' OF brw-emp.
   CLOSE QUERY qry-emp.
   HIDE FRAME frm-emp.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO wsEmp.
   RETURN. 
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-emp 
     OR 'enter':u OF brw-emp
     OR 'mouse-select-dblclick' OF brw-emp
 DO: 
    GET CURRENT qry-emp NO-LOCK NO-WAIT.
    DISPLAY payemf.empcode @ wsEmp WITH FRAME frm-Paysys.
    RETURN. 
 END.

ON  'enter':U OF wsEmp IN FRAME frm-Paysys
     OR 'tab':U OF wsEmp IN FRAME frm-Paysys
 DO:
    ASSIGN wsEmp = INT(wsEmp:SCREEN-VALUE).
     FIND FIRST payemf WHERE payemf.empcode = wsEmp AND Payemf.Paysys = wsSys NO-ERROR.
     IF AVAILABLE payemf AND payemf.eStat = 4 THEN DO:
         MESSAGE "Employee already terminated...please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     ELSE IF AVAILABLE payemf AND payemf.eStat <> 4 THEN DO:
         FIND FIRST paymtf WHERE paymtf.empcode = wsEmp AND paymtf.curAmt <> 0 NO-LOCK NO-ERROR.
         IF AVAILABLE paymtf THEN DO:
             MESSAGE "No terminations allowed in an Open payroll" VIEW-AS ALERT-BOX.
             APPLY 'close' TO THIS-PROCEDURE.
         END.
          wsDes = payemf.SName + " " + payemf.FName.
          eStat:SCREEN-VALUE = STRING(payemf.eStatus).
           DISPLAY wsDes   payemf.StatDate @ wsDate WITH FRAME frm-Paysys.
     END.
     ELSE DO:
         MESSAGE "Invalid Employee entered...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
 END.

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
     ASSIGN eStat = INT(eStat:SCREEN-VALUE)
            wsDate = DATE(wsDate:SCREEN-VALUE).
    CASE eStat:
        WHEN  2 THEN
            ASSIGN payemf.eStatus = estat
                   payemf.StatDate = wsDate.
        WHEN 3 THEN DO:
            ASSIGN payemf.eStatus = estat
                   payemf.StatDate = wsDate.
        END.
        WHEN 4 THEN DO:
            FIND FIRST paypost WHERE paypost.post = payemf.Post NO-ERROR.
            IF AVAILABLE paypost THEN
                PayPost.Vacant = YES.
            ASSIGN payemf.eStatus  = estat
                   payemf.leaver   = YES
                   payemf.StatDate = wsDate.
        END.
    END CASE.
    RELEASE payemf.
    RELEASE paypost.
    CLEAR FRAME frm-Paysys ALL.
    APPLY 'tab' TO btn-close.
 END.

 /********** MAIN LOGIC **********/
 FIND FIRST simctr NO-LOCK NO-ERROR.
 wsCo = SIMCTR.CONAME.
 VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
 ENABLE ALL WITH FRAME frm-Paysys.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
 HIDE FRAME frm-Paysys.
 RETURN.
