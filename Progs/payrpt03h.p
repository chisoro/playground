SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt03h.p
    Notes:...... ..........Payroll Item listing Report from History
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsTotal LIKE wsAmt EXTENT 2.
DEF VAR subTotal LIKE wsAmt EXTENT 2.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR wscnt AS INT.
DEF VAR wsDate  AS DATE.
DEF VAR stDate  AS DATE.
DEF VAR wsCo AS CHAR FORM "x(100)".
DEF VAR wsItem LIKE payitem.itemcode.
 
 DEF BUTTON btn-Sys   LABEL "PAYROLL SYSTEM".
 DEF BUTTON btn-exit  LABEL "CLOSE".
 DEF BUTTON btn-close LABEL "CLOSE".
 DEF BUTTON btn-ok    LABEL "PRINT".
  DEF BUTTON btn-Item    LABEL "PAYROLL ITEM".

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 5.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.

 DEF    QUERY qry-Sys FOR Paysys SCROLLING.
 DEF BROWSE brw-Sys QUERY qry-Sys
         DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

 DEF    QUERY qry-pick FOR Payitem SCROLLING.
 DEF BROWSE brw-pick QUERY qry-pick
         DISPLAY Payitem.Itemcode Payitem.Descrip COLUMN-LABEL "Decsription" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

 FORM
    payemf.empcode
    wsdes          LABEL "EMPLOYEES"
    payhtf.CurAmt LABEL "CUR AMOUNT"
    payhtf.YtdAmt LABEL "YTD AMOUNT"
    HEADER wsCo "PAGE" AT 80 PAGE-NUMBER(a) SKIP(1) 
          "PAYROLL ITEM LISTING REPORT FOR PERIOD ENDED " wsDate SKIP(2)
     Payitem.Descrip
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

 DEFINE FRAME frm-sys 
     brw-Sys AT ROW 2 COL 5
     skip(0.5)
     btn-ok colon 5
     btn-close colon 60
     with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEFINE FRAME frm-pick 
      brw-pick AT ROW 2 COL 5
      skip(0.5)
      btn-ok colon 5
      btn-close colon 60
      with view-as dialog-box keep-tab-order no-validate
           side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll Item Selection".

 DEFINE FRAME frm-Paysys
     SKIP(1.5)
     btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
     wsSys              NO-LABEL AUTO-RETURN 
     Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     wsDate    COLON 30 LABEL "PAYROLL DATE" SKIP(0.5)
     btn-Item COLON 13 NO-LABEL NO-TAB-STOP
     payitem.itemcode  NO-LABEL
     Payitem.Descrip   NO-LABEL VIEW-AS TEXT FORM "x(40)" SKIP(1.5) 
     btn-ok colon 20
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 7.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 10
     TITLE "PAYROLL ITEM CODE REPORT FROM HISTORY" VIEW-AS DIALOG-BOX.


 /* Triggers for the Paysys frame */
 ON CHOOSE OF btn-sys IN FRAME frm-Paysys
     OR CHOOSE OF btn-sys IN FRAME frm-Paysys
 DO:
   VIEW FRAME frm-sys.
   OPEN QUERY qry-sys FOR EACH paysys NO-LOCK.
   ENABLE ALL WITH FRAME frm-sys.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-sys 
           OR close of THIS-PROCEDURE IN FRAME frm-sys
           OR CHOOSE OF btn-ok IN FRAME frm-sys 
           OR 'enter':u OF brw-sys
           OR 'mouse-select-dblclick' OF brw-sys.
   CLOSE QUERY qry-sys.
   HIDE FRAME frm-sys.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO wsSys.
   RETURN. 
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-sys 
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
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
              DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.CurDate @ wsDate WITH FRAME frm-Paysys.
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

ON 'enter':U OF wsDate IN FRAME frm-paysys
DO:
     IF  DATE(wsDate:SCREEN-VALUE) = Paysys.CurDate THEN DO:
        MESSAGE "Report for current payroll has its own option, please try that" VIEW-AS ALERT-BOX.
        APPLY 'close' TO THIS-PROCEDURE.
    END.
    ELSE IF  DATE(wsDate:SCREEN-VALUE) >= Paysys.CurDate THEN DO:
        MESSAGE "No history for entered date if current/after current payroll date" VIEW-AS ALERT-BOX.
        wsDate:SCREEN-VALUE = STRING(Paysys.CurDate).
        RETURN NO-APPLY.
    END.
    ELSE IF  NOT CAN-FIND( FIRST payhtf WHERE Payhtf.trDate = DATE(wsDate:SCREEN-VALUE)) THEN DO:
        MESSAGE "No history for entered date for the selected payroll system" VIEW-AS ALERT-BOX.
        wsDate:SCREEN-VALUE = STRING(Paysys.CurDate).
        RETURN NO-APPLY.
    END.
    RETURN.
END.

 ON CHOOSE OF btn-Item IN FRAME frm-Paysys
     OR CHOOSE OF btn-Item IN FRAME frm-Paysys
 DO:
   VIEW FRAME frm-Pick.
   OPEN QUERY qry-Pick FOR EACH payitem NO-LOCK.
   ENABLE ALL WITH FRAME frm-Pick.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Pick 
           OR close of THIS-PROCEDURE IN FRAME frm-Pick
           OR CHOOSE OF btn-ok IN FRAME frm-Pick 
           OR 'enter':u OF brw-Pick
           OR 'mouse-select-dblclick' OF brw-Pick.
   CLOSE QUERY qry-Pick.
   HIDE FRAME frm-Pick.
   APPLY 'tab' TO SELF.
   RETURN. 
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-Pick 
     OR 'enter':u OF brw-Pick
     OR 'mouse-select-dblclick' OF brw-Pick
 DO: 
    GET CURRENT qry-Pick NO-LOCK NO-WAIT.
    DISPLAY Payitem.Itemcode WITH FRAME frm-Paysys.
    APPLY 'tab' TO Payitem.Itemcode.
    RETURN. 
 END.

ON  'enter':U OF Payitem.Itemcode IN FRAME frm-Paysys
     OR 'tab':U OF Payitem.Itemcode IN FRAME frm-Paysys
 DO:
     FIND FIRST Payitem WHERE Payitem.Itemcode = INT(Payitem.Itemcode:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE Payitem THEN DO:
         DISPLAY Payitem.Descrip WITH FRAME frm-Paysys.
     END.
     ELSE DO:
         MESSAGE "Invalid Payroll Item...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
END.

 ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
 DO:
    ASSIGN wsSys = INT(wsSys:SCREEN-VALUE)
           wsDate = DATE(wsDate:SCREEN-VALUE).
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 
    APPLY 'close' TO THIS-PROCEDURE.
 END.
 /********** MAIN LOGIC **********/
 FIND FIRST simctr NO-LOCK NO-ERROR.
 wsCo = SIMCTR.CONAME.
 VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
 ENABLE ALL WITH FRAME frm-Paysys.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
 HIDE FRAME frm-Paysys.
 RETURN.

PROCEDURE Report.ip:
    subTotal = 0.
    wsTotal = 0.
    stDate = DATE("01/01/" + STRING(YEAR(wsDate))).
FOR EACH payemf WHERE payemf.Paysys = wsSys AND payemf.Leaver = NO NO-LOCK :
    subTotal = 0.
    wsdes    = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
    FOR EACH payhtf WHERE payhtf.empcode = Payemf.empcode 
                      AND payhtf.Itemcode = INT(payitem.itemcode:SCREEN-VALUE IN FRAME frm-Paysys) 
                      AND payhtf.trDate >= stDate AND payhtf.trDate < wsDate  NO-LOCK:
        IF payhtf.CurAmt = 0 THEN NEXT.
        ASSIGN subTotal[2] = subTotal[2] + payhtf.CurAmt
               wsTotal[2] = wsTotal[2] + payhtf.CurAmt.
    END.
    FOR EACH payhtf WHERE payhtf.empcode = Payemf.empcode 
                      AND payhtf.Itemcode = INT(payitem.itemcode:SCREEN-VALUE IN FRAME frm-Paysys) 
                      AND payhtf.trDate = DATE(wsDate:SCREEN-VALUE) NO-LOCK:
        IF payhtf.CurAmt = 0 THEN NEXT.
        ASSIGN subTotal[1] = subTotal[1] + payhtf.CurAmt
               wsTotal[1] = wsTotal[1] + payhtf.CurAmt.
    END.
    DISPLAY STREAM a payemf.empcode wsdes subTotal[1] @ payhtf.CurAmt  subTotal[2] @ payhtf.YtdAmt WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.

END.
UNDERLINE STREAM a payhtf.CurAmt payhtf.YtdAmt WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
DISPLAY STREAM a "TOTAL" @ wsdes  wsTotal[1] @ payhtf.CurAmt  wsTotal[2] @ payhtf.YtdAmt WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
 APPLY 'close' TO THIS-PROCEDURE.
END.
