/* Program.................payend01.p
   Notes:...... Payroll Period Closure
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF VAR X AS INT.
DEF VAR wsDate AS DATE.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsdays   AS INT FORM 99 EXTENT 12
        INITIAL [31,28,31,30,31,30,31,31,30,31,30,31].
DEF VAR wsDay   AS INT FORM "99".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsYear  AS INT FORM "9999".
DEF BUTTON btnJob LABEL "OCCUPATION".
DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsStatus LIKE payemf.empcode.
DEF SHARED VAR varUser LIKE simusr.usercode.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 8.5.

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
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP 
    wsSys     NO-LABEL AUTO-RETURN 
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(1.5)
    CurPer     COLON 23 VIEW-AS TEXT
    CurDate    VIEW-AS TEXT SKIP(0.5)
    PerClosed  COLON 23  VIEW-AS TEXT
    ClosedDate VIEW-AS TEXT SKIP(1)
    wsStatus  COLON 23 LABEL "Processing....." VIEW-AS TEXT SKIP(2)
    btn-ok colon 10
    btn-close colon 50
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13
    TITLE "PERIOD CLOSURE PROCEDURE" VIEW-AS DIALOG-BOX.

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
   GET CURRENT qry-sys EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.ClosedDate 
       Paysys.CurDate Paysys.CurPer Paysys.PerClosed WITH FRAME frm-Paysys.
   ASSIGN wsSys = INT(wsSys:SCREEN-VALUE).
   RETURN. 
END.

ON  'enter':U OF wsSys IN FRAME frm-paySys
    OR 'tab':U OF wsSys IN FRAME frm-paySys
    OR 'leave':U OF wsSys IN FRAME frm-paySys
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
              DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.ClosedDate 
               Paysys.CurDate Paysys.CurPer Paysys.PerClosed WITH FRAME frm-Paysys.
              APPLY 'entry' TO btn-ok IN FRAME frm-paySys.
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

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
    ASSIGN wsYear  = INT(YEAR(Paysys.CurDate))
           wsMonth = INT(MONTH(Paysys.CurDate))
           wsDay   = wsDays[wsMonth].
           IF wsMonth = 2 AND wsYear MOD 4 = 0 THEN
               wsDay = 29.
           ELSE IF wsMonth = 2 AND wsYear MOD 4 <> 0 THEN
               wsDay = 28.
           wsDate = Paysys.CurDate.
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN DO:
        FOR EACH payemf WHERE payemf.paysys = wsSys NO-LOCK:
              FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode:
                  IF paymtf.itemcode = payctr.itemcode THEN NEXT.
                  ELSE
                  ASSIGN Paymtf.CurAmt   = 0
                         Paymtf.InpAmt   = 0
                         Paymtf.YtdAmt   = 0.
              END.
          END.
          wsYear = INT(YEAR(Paysys.CurDate)) - 1.
          wsMonth = INT(MONTH(Paysys.CurDate)) - 1.
          IF wsMonth = 0 THEN
              wsMonth = 12.
          wsDay   = wsDays[wsMonth].
          IF wsMonth = 2 AND wsYear MOD 4 = 0 THEN
               wsDay = 29.
           ELSE IF wsMonth = 2 AND wsYear MOD 4 <> 0 THEN
               wsDay = 28.
          wsDate = Paysys.CurDate.
          MESSAGE wsDate VIEW-AS ALERT-BOX.
          ASSIGN Paysys.ClosedDate = wsDate
                 Paysys.PerClosed = Paysys.CurPer - 1.
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
