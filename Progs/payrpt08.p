SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt08.p
    Notes:...... ..........Payroll Details Report
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR st-emp  LIKE payemf.empcode.
DEF VAR end-emp LIKE payemf.empcode.
DEF VAR wsName AS CHAR FORM "x(40)".
DEF VAR wsName1 AS CHAR FORM "x(40)".
DEF VAR wsdate  AS DATE.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-" EXTENT 3.
DEF VAR wsTotal LIKE wsAmt.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR wscnt AS INT.
DEF VAR wsPer  AS INT FORM "999999".
DEF VAR wsCo   AS CHAR FORM "x(60)".
DEF VAR wsStatus LIKE payemf.empcode.
DEF VAR wsPer1   AS CHAR FORM "x(16)".
DEF VAR wsMonth AS CHAR INITIAL "JANUARY, FEBRUARY, MARCH, APRIAL, MAY, JUNE, JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER".
 
 DEF BUTTON btn-Sys   LABEL "Payroll System".
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

 FORM  SKIP(2)
      "EMPLOYEE FINANCIAL DETAILS" AT  8 SKIP(1)
      "EMPLOYEE NAME  :" AT 8 wsName NO-LABEL
      "EMPLOYEE ID    :" AT 8 payemf.IDNo NO-LABEL 
      "EMPLOYEE NUMBER:" AT 8 payemf.empcode   NO-LABEL 
      "DEPARTMENT     :" AT 8 paydept.descrip  NO-LABEL 
      "EMPLOYMENT DATE:" AT 8  payemf.startdat NO-LABEL 
      "JOB TITLE      :" AT 8  payemf.Designation NO-LABEL FORM "x(40)" 
      "GRADE          :" AT 8  payemf.scale NO-LABEL 
      SKIP(2)
          HEADER wsCo AT 8 "PAGE" AT 70 PAGE-NUMBER(a) SKIP(1) 
          "PAYROLL DETAILLED ANALYSIS REPORT FOR THE PERIOD " AT 8 wsPer1 SKIP(2)
           WITH NO-BOX NO-LABELS WIDTH 132 FRAME head STREAM-IO.


 FORM
    Payitem.Itemcode AT 10
    wsdes    LABEL "PAYROLL ITEM"
    wsAmt[1] LABEL "CURRENT AMOUNT"
    wsAmt[2] LABEL "YTD-AMOUNT"
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

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
     wsSys              NO-LABEL AUTO-RETURN 
     Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     wsDate    COLON 26 LABEL "PAYROLL DATE" VIEW-AS TEXT SKIP(1)
     st-emp    COLON 26 LABEL "From Employee" wsName NO-LABEL VIEW-AS TEXT SKIP(0.5)
     end-emp   COLON 26 LABEL "To Employee" wsName1 NO-LABEL VIEW-AS TEXT SKIP(0.5)
     wsStatus COLON 26  LABEL "Processing...." VIEW-AS TEXT
     btn-ok AT ROW 10.5 COL 20 SPACE(40)
     btn-close 
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 10.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 13
     TITLE "PAYROLL DETAILLED ANALYSIS REPORT" VIEW-AS DIALOG-BOX.


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
     OR 'leave':U OF wsSys IN FRAME frm-Paysys
 DO:
    IF  INT(wsSys:SCREEN-VALUE) = 0 THEN
        APPLY 'close' TO THIS-PROCEDURE.
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

 ON  'enter':U OF st-emp IN FRAME frm-Paysys
    OR 'tab':U OF st-emp IN FRAME frm-Paysys
    OR 'leave':U OF st-emp IN FRAME frm-Paysys
DO:
     IF INT(st-emp:SCREEN-VALUE) = 0 THEN DO:
         FIND FIRST payemf WHERE payemf.Paysys = INT(wsSys:SCREEN-VALUE)  NO-LOCK NO-ERROR.
         st-emp:SCREEN-VALUE = STRING(payemf.empcode).
     END.
    FIND FIRST payemf WHERE payemf.empcode = INT(st-emp:SCREEN-VALUE)
                        AND payemf.Paysys = INT(wsSys:SCREEN-VALUE)  NO-LOCK NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        wsName = payemf.SName + " " + payemf.FName.
        DISPLAY wsName WITH FRAME frm-paysys.
        APPLY 'tab' TO SELF.
    END.
    ELSE DO:
         MESSAGE "Invalid Employee number...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    RETURN.
END.

ON  'enter':U OF end-emp IN FRAME frm-Paysys
    OR 'tab':U OF end-emp IN FRAME frm-Paysys
    OR 'leave':U OF end-emp IN FRAME frm-Paysys
DO:
    IF INT(end-emp:SCREEN-VALUE) = 0 THEN DO:
         FIND LAST payemf WHERE payemf.Paysys = INT(wsSys:SCREEN-VALUE)  NO-LOCK NO-ERROR.
         end-emp:SCREEN-VALUE = STRING(payemf.empcode).
     END.
    FIND FIRST payemf WHERE payemf.empcode = INT(end-emp:SCREEN-VALUE)
                        AND payemf.Paysys = INT(wsSys:SCREEN-VALUE)  NO-LOCK NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        wsName1 = payemf.SName + " " + payemf.FName.
        DISPLAY wsName1 WITH FRAME frm-paysys.
        APPLY 'tab' TO SELF.
    END.
    ELSE DO:
        APPLY 'tab' TO SELF.
    END.
    RETURN.
END.


 ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
 DO:
    ASSIGN wsSys = INT(wsSys:SCREEN-VALUE)
           wsDate = paysys.CurDate 
           st-emp = INT(st-emp:SCREEN-VALUE IN FRAME frm-Paysys)
           end-emp = INT(end-emp:SCREEN-VALUE IN FRAME frm-Paysys)
           wsPer1 = STRING(ENTRY(MONTH(wsDate),wsMonth)) + "  " + STRING(YEAR(wsDate)).
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 

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
    FOR EACH payemf WHERE payemf.paysys = wsSys AND payemf.empcode >= st-emp AND payemf.empcode <= end-emp NO-LOCK :
        DISPLAY payemf.empcode @ wsStatus WITH FRAME frm-Paysys.
        PAUSE 0.
         wsName = payemf.SName + " " + payemf.FName.
         FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-LOCK NO-ERROR. 
         DISPLAY STREAM a payemf.IDNo payemf.empcode wsName payemf.Designation payemf.scale
                   payemf.empcode payemf.startdate Paydept.Descrip WITH FRAME head.
        DOWN STREAM a WITH FRAME head.
        FOR EACH paymtf WHERE (CurAmt <> 0 OR YtdAmt <> 0 ) AND paymtf.empcode = payemf.empcode NO-LOCK:
            FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-ERROR.
                IF AVAILABLE payitem  AND payitem.cat <> 0 THEN DO:
                    ASSIGN wsdes    =  Payitem.Descrip.
                     DISPLAY STREAM a Payitem.Itemcode wsdes curAmt @ wsAmt[1] ytdAmt @ wsAmt[2] WITH FRAME frm-rpt.
                    DOWN STREAM a WITH FRAME frm-rpt.
                END.
        END.
        PAGE STREAM a.
    END.
 APPLY 'close' TO THIS-PROCEDURE.
END PROCEDURE.
