/* Program.................payrun01.p
   Notes:...... Payroll un-Calculate
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsF     LIKE payitem.LVar.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsStatus LIKE payemf.empcode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsDate AS DATE.
DEF VAR wsper AS INT.
DEF VAR st-emp LIKE payemf.empcode.
DEF VAR end-emp LIKE payemf.empcode.
DEF VAR wsName LIKE payemf.sname.
DEF SHARED VAR varUser LIKE simusr.usercode.

DEF BUTTON btn-Emp   LABEL "START EMPLOYEE".
DEF BUTTON btn-Emp1   LABEL "END EMPLOYEE".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btn-Sys   LABEL "Payroll System".


DEF BUFFER bfrPaymtf FOR paymtf.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
        DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
        WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-emp FOR Payemf SCROLLING.
DEF BROWSE brw-emp QUERY qry-emp
        DISPLAY payemf.empcode  payemf.SName WIDTH 40 payemf.FName 
        WIDTH 30 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickemp 
    brw-emp AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 8.5.

DEFINE FRAME frm-main
    SKIP(0.5)
    btn-Sys   COLON 14 NO-LABEL NO-TAB-STOP
    wsSys     NO-LABEL
    Paysys.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
    st-per    COLON 30 LABEL "Payroll Period" VIEW-AS TEXT SKIP(0.5)
    wsDate    COLON 30 LABEL "Run Date" VIEW-AS TEXT SKIP(0.5)
    btn-Emp    COLON 8 NO-LABEL NO-TAB-STOP st-emp NO-LABEL wsname NO-LABEL VIEW-AS TEXT SKIP(0.5)
    btn-Emp1    COLON 10 NO-LABEL NO-TAB-STOP end-emp NO-LABEL SKIP(1.0)
    wsStatus   COLON 20 LABEL "Processing......" VIEW-AS TEXT NO-TAB-STOP
    skip(1.0)
    btn-ok colon 15
    btn-close colon 50
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13
    TITLE "PAYROLL UN-CALCULATE" VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-pick 
    brw-Sys AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

ON CHOOSE OF btn-sys IN FRAME frm-main
    OR CHOOSE OF btn-sys IN FRAME frm-main
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
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-sys
    OR 'mouse-select-dblclick' OF brw-sys
DO: 
   GET CURRENT qry-sys NO-LOCK NO-WAIT.
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-main.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO wsSys IN FRAME frm-main.
END.

ON  'enter':U OF wsSys IN FRAME frm-main
    OR 'tab':U OF wsSys IN FRAME frm-main
    OR 'leave':U OF wsSys IN FRAME frm-main
DO:
    ASSIGN wsSys.
    IF INT(wsSys:SCREEN-VALUE) = 0 THEN
        APPLY 'close' TO THIS-PROCEDURE.
    ELSE DO:
        ASSIGN wsSys.
        FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE paysys THEN DO:
           FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
           IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
           DO:
              DISPLAY Paysys.Descrip WITH FRAM frm-main.
              ASSIGN st-per:SCREEN-VALUE = STRING(paysys.CURPER)
                     wsDate:SCREEN-VALUE = STRING(Paysys.CurDate)
                     wsper = paysys.period.
              APPLY 'entry' TO btn-ok IN FRAME frm-main.
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
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-main DO:
     session:set-wait-state("").
     IF (wsSys:SCREEN-VALUE IN FRAME frm-main) = "" THEN DO:
         MESSAGE "Invalid Payroll System....." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     FIND FIRST Paysys WHERE Paysys.paysys = INT(wsSys:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
     ASSIGN st-per = INT(st-per:SCREEN-VALUE IN FRAME frm-main)
            wsDate = DATE(wsDate:SCREEN-VALUE IN FRAME frm-main)
            wsSys  = INT(wsSys:SCREEN-VALUE IN FRAME frm-main)
            st-emp end-emp.
     RUN Report.ip.
END.

    ON CHOOSE OF btn-EMP IN FRAME frm-main
DO:
  VIEW FRAME frm-pickemp.
  OPEN QUERY qry-emp FOR EACH payemf WHERE payemf.estatus <> 4 AND payemf.paysys = wsSys NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickemp.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickemp 
          OR close of THIS-PROCEDURE IN FRAME frm-pickemp
          OR CHOOSE OF btn-ok IN FRAME frm-pickemp 
          OR 'enter':u OF brw-emp
          OR 'mouse-select-dblclick' OF brw-emp.
  CLOSE QUERY qry-emp.
  HIDE FRAME frm-pickemp.
  APPLY 'tab' TO SELF.
  DISPLAY payemf.empcode @ st-emp WITH FRAME frm-main .
  APPLY 'tab' TO st-emp IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btn-EMP1 IN FRAME frm-main
DO:
  VIEW FRAME frm-pickemp.
  OPEN QUERY qry-emp FOR EACH payemf WHERE payemf.estatus <> 4 AND payemf.paysys = wsSys NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickemp.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickemp 
          OR close of THIS-PROCEDURE IN FRAME frm-pickemp
          OR CHOOSE OF btn-ok IN FRAME frm-pickemp 
          OR 'enter':u OF brw-emp
          OR 'mouse-select-dblclick' OF brw-emp.
  CLOSE QUERY qry-emp.
  HIDE FRAME frm-pickemp.
  APPLY 'tab' TO SELF.
  DISPLAY payemf.empcode @ end-emp WITH FRAME frm-main .
  APPLY 'tab' TO end-emp IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickemp 
    OR 'enter':u OF brw-emp
    OR 'mouse-select-dblclick' OF brw-emp
DO: 
   GET CURRENT qry-emp NO-LOCK NO-WAIT.
   APPLY 'tab' TO SELF.
   RETURN. 
END.

ON  'enter':U OF st-emp IN FRAME frm-main
    OR 'tab':U OF st-emp  IN FRAME frm-main
DO:
    IF INT(st-emp :SCREEN-VALUE) = 0 THEN DO:
        FIND FIRST payemf WHERE payemf.paysys = wsSys NO-LOCK NO-ERROR. 
        st-emp:SCREEN-VALUE = STRING(payemf.empcode).
    END.
    FIND FIRST payemf WHERE payemf.empcode  = INT(st-emp :SCREEN-VALUE) AND payemf.paysys = wsSys NO-LOCK NO-ERROR.
    IF AVAILABLE payemf AND payemf.estatus = 4 THEN DO:
        MESSAGE "Employee has left...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF AVAILABLE payemf  THEN DO:
        wsName = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
        DISPLAY payemf.empcode @ st-emp wsName WITH FRAME frm-main.
    END.
   ELSE IF NOT AVAILABLE payemf AND INT(end-emp:SCREEN-VALUE) <> 0 THEN DO:
        MESSAGE "Invalid Employee code...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON  'enter':U OF end-emp IN FRAME frm-main
    OR 'tab':U OF end-emp  IN FRAME frm-main
DO:
    FIND FIRST payemf WHERE payemf.empcode  = INT(end-emp:SCREEN-VALUE) AND payemf.paysys = wsSys NO-LOCK NO-ERROR.
    IF AVAILABLE payemf AND payemf.estatus = 4 THEN DO:
        MESSAGE "Employee has left...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE payemf AND INT(end-emp:SCREEN-VALUE) <> 9999999 THEN DO:
        MESSAGE "Invalid Employee code...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.
/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST payctr NO-LOCK NO-ERROR.
ASSIGN st-per:SCREEN-VALUE = ""
       wsDate:SCREEN-VALUE = STRING(TODAY)
       st-emp:SCREEN-VALUE = "0"
       end-emp:SCREEN-VALUE = "9999999".
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Report.ip:
    /* Undo any calculations */
    FOR EACH payemf WHERE payemf.paysys = wsSys AND (payemf.estatus = 1 OR payemf.estatus = 2)
                      AND payemf.empcode >= st-emp AND payemf.empcode <= end-emp:
        DISPLAY payemf.empcode @ wsStatus WITH FRAME frm-main.
        PAUSE 0.
        FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode:
            IF paymtf.curAmt <> 0 THEN
               ASSIGN Paymtf.YtdAmt = Paymtf.YtdAmt - Paymtf.CurAmt
                      Paymtf.CurAmt = 0.
        END.
    END.
    FOR EACH paycost WHERE paycost.paysys = wsSys
                      AND paycost.empcode >= st-emp AND paycost.empcode <= end-emp: /*clean costing data */
        DELETE paycost.
    END.
END procedure.

