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
DEF  VAR wsOpt AS INT VIEW-AS COMBO-BOX SIZE 15 BY 2 
                LIST-ITEM-PAIRS "Month-End",1,"Year-End",2.

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
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(1.5)
    CurPer     COLON 23 VIEW-AS TEXT
    CurDate    VIEW-AS TEXT SKIP(0.5)
    PerClosed  COLON 23  VIEW-AS TEXT
    ClosedDate VIEW-AS TEXT SKIP(1.5)
    wsOpt COLON 16 LABEL "Option"  AUTO-RETURN  SKIP(2.5)
    btn-ok colon 10
    btn-close colon 50
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 70 BY 15
    TITLE "PERIOD END PROCEDURE" VIEW-AS DIALOG-BOX.

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

ON  'enter':U OF wsSys IN FRAME frm-Paysys
    OR 'tab':U OF wsSys IN FRAME frm-Paysys
DO:
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN DO:
       DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.ClosedDate 
               Paysys.CurDate Paysys.CurPer Paysys.PerClosed WITH FRAME frm-Paysys.
      ASSIGN wsSys = INT(wsSys:SCREEN-VALUE).
    END.
    ELSE DO:
        MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
    ASSIGN wsOpt
           wsYear  = INT(YEAR(Paysys.CurDate))
           wsMonth = INT(MONTH(Paysys.CurDate))
           wsDay   = wsDays[wsMonth].
           IF wsMonth = 2 AND wsYear MOD 4 = 0 THEN
               wsDay = 29.
           ELSE IF wsMonth = 2 AND wsYear MOD 4 <> 0 THEN
               wsDay = 28.
           wsDate = DATE(wsMonth,wsDay,wsYear).
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN DO:
       IF wsOpt = 1 THEN DO:
          FOR EACH payemf WHERE payemf.paysys = wsSys NO-LOCK:
              FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode AND Paymtf.CurAmt <> 0:
                  CREATE payhtf.
                  ASSIGN Payhtf.empcode  = payemf.empcode
                         Payhtf.Itemcode = Paymtf.Itemcode 
                         Payhtf.trDate   = wsDate
                         Payhtf.InpAmt   = Paymtf.InpAmt
                         Payhtf.CurAmt   = Paymtf.CurAmt
                         Payhtf.YtdAmt   = Paymtf.YtdAmt.
                  ASSIGN Paymtf.CurAmt   = 0
                         Paymtf.InpAmt   = 0.
              END.
          END.
          IF  wsMonth < 12 THEN
              wsMonth = wsMonth + 1.
          ELSE wsMonth = 1.
          IF wsMonth = 1 THEN
              wsYear = wsYear + 1.
          wsDay   = wsDays[wsMonth].
          IF wsMonth = 2 AND wsYear MOD 4 = 0 THEN
               wsDay = 29.
          ELSE IF wsMonth = 2 AND wsYear MOD 4 <> 0 THEN
               wsDay = 28.
          wsDate = DATE(wsMonth,wsDay,wsYear).
          ASSIGN Paysys.CurDate = wsDate
                 Paysys.CurPer = paysys.CurPer + 1.
       END.
       ELSE IF wsOpt = 2 THEN DO:
           FOR EACH payemf WHERE payemf.paysys = wsSys NO-LOCK:
              FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode:
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
          wsDate = DATE(wsmonth,wsday,wsYear).
          MESSAGE wsDate VIEW-AS ALERT-BOX.
          ASSIGN Paysys.ClosedDate = wsDate
                 Paysys.PerClosed = Paysys.CurPer - 1.
       END.
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
