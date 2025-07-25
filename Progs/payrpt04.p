SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt04.p
    Notes:...... ..........Pension/Medical Aid listing Report
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
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-" EXTENT 4.
DEF VAR wsTotal LIKE wsAmt EXTENT 4.
DEF VAR subTotal LIKE wsAmt.
DEF VAR wsdes AS CHAR FORM "x(40)" EXTENT 4.
DEF VAR wscnt AS INT.
DEF VAR wsPer  AS INT FORM "999999".
DEF VAR wsCo AS CHAR FORM "x(100)".
DEF VAR wsItem LIKE payitem.itemcode EXTENT 3.
DEF VAR wsTitle AS CHAR FORM "x(70)".
DEF VAR X AS INT.
DEF VAR wsOpt AS CHAR FORM "X(15)" VIEW-AS COMBO-BOX LIST-ITEMS "PENSION", "MEDICAL AID".
 
 DEF BUTTON btn-Sys   LABEL "PAYROLL SYSTEM".
 DEF BUTTON btn-exit  LABEL "CLOSE".
 DEF BUTTON btn-close LABEL "CLOSE".
 DEF BUTTON btn-ok    LABEL "PRINT".
 DEF BUTTON btn-Item1 LABEL "BASIC PAY".
 DEF BUTTON btn-Item2 LABEL "EMPLOYEE CONT".
 DEF BUTTON btn-Item3 LABEL "EMPLOYER CONT".

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 10.3.

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
    wsdes[4]     LABEL "EMPLOYEES"
    wsAmt[1]     LABEL "BASIC PAY"
    wsAmt[2]     LABEL "EMPLOYEE CONT"
    wsAmt[3]     LABEL "EMPLOYEER CONT"
    wsAmt[4]     LABEL "TOTAL CONT"
    HEADER wsCo "PAGE" AT 80 PAGE-NUMBER(a) SKIP(1) 
          wsTitle SKIP(2)
    
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

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
     wsOpt     COLON 30 LABEL "REPORT TYPE" SKIP(0.5)
     btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
     wsSys              NO-LABEL AUTO-RETURN 
     Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     Paysys.CurDate    COLON 30 LABEL "CURRENT PAYROLL DATE" VIEW-AS TEXT SKIP(0.5)
     btn-Item1 COLON 17 NO-LABEL NO-TAB-STOP
     wsItem[1]  NO-LABEL
     wsDes[1]   NO-LABEL VIEW-AS TEXT FORM "x(40)" SKIP(0.5) 
     btn-Item2 COLON 10.5 NO-LABEL NO-TAB-STOP
     wsItem[2]  NO-LABEL
     wsDes[2]   NO-LABEL VIEW-AS TEXT FORM "x(40)" SKIP(0.5)
     btn-Item3 COLON 10.5 NO-LABEL NO-TAB-STOP
     wsItem[3]  NO-LABEL
     wsDes[3]   NO-LABEL VIEW-AS TEXT FORM "x(40)" SKIP(1.8) 
     btn-ok colon 20
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 12.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 15
     TITLE "PENSION/MEDICAL AID PAYROLL REPORT" VIEW-AS DIALOG-BOX.


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
     ASSIGN wssys.
    RETURN. 
 END.

ON  'enter':U OF wsSys IN FRAME frm-Paysys
     OR 'tab':U OF wsSys IN FRAME frm-Paysys
 DO:
    ASSIGN wssys.
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
              DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.Curdate WITH FRAME frm-Paysys.
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

 ON CHOOSE OF btn-Item1 IN FRAME frm-Paysys
     OR CHOOSE OF btn-Item1 IN FRAME frm-Paysys
 DO:
     X = 1.
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

 ON CHOOSE OF btn-Item2 IN FRAME frm-Paysys
     OR CHOOSE OF btn-Item2 IN FRAME frm-Paysys
 DO:
     X = 2.
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

 ON CHOOSE OF btn-Item3 IN FRAME frm-Paysys
     OR CHOOSE OF btn-Item3 IN FRAME frm-Paysys
 DO:
     X = 3.
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
    IF X = 1 THEN DO:
        DISPLAY Payitem.Itemcode @ wsItem[1] WITH FRAME frm-Paysys.
        APPLY 'tab' TO wsItem[1].
    END.
    ELSE IF X = 2 THEN DO:
        DISPLAY Payitem.Itemcode @ wsItem[2] WITH FRAME frm-Paysys.
        APPLY 'tab' TO wsItem[2].
    END.
    ELSE IF X = 3 THEN DO:
        DISPLAY Payitem.Itemcode @ wsItem[3] WITH FRAME frm-Paysys.
        APPLY 'tab' TO wsItem[3].
    END.
    RETURN. 
 END.

ON  'enter':U OF wsItem[1] IN FRAME frm-Paysys
     OR 'tab':U OF wsItem[1] IN FRAME frm-Paysys
 DO:
     FIND FIRST Payitem WHERE Payitem.Itemcode = INT(wsItem[1]:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE Payitem THEN DO:
         DISPLAY Payitem.Descrip @ wsDes[1] WITH FRAME frm-Paysys.
     END.
     ELSE DO:
         MESSAGE "Invalid Payroll Item...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
END.

ON  'enter':U OF wsItem[2] IN FRAME frm-Paysys
     OR 'tab':U OF wsItem[2] IN FRAME frm-Paysys
 DO:
     FIND FIRST Payitem WHERE Payitem.Itemcode = INT(wsItem[2]:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE Payitem THEN DO:
         DISPLAY Payitem.Descrip @ wsDes[2] WITH FRAME frm-Paysys.
     END.
     ELSE DO:
         MESSAGE "Invalid Payroll Item...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
END.

ON  'enter':U OF wsItem[3] IN FRAME frm-Paysys
     OR 'tab':U OF wsItem[3] IN FRAME frm-Paysys
 DO:
     FIND FIRST Payitem WHERE Payitem.Itemcode = INT(wsItem[3]:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE Payitem THEN DO:
         DISPLAY Payitem.Descrip @ wsDes[3] WITH FRAME frm-Paysys.
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
           wsItem[1] = INT(wsItem[1]:SCREEN-VALUE)
           wsItem[2] = INT(wsItem[2]:SCREEN-VALUE)
           wsItem[3] = INT(wsItem[3]:SCREEN-VALUE)
           wsOpt.
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 

 END.
 /********** MAIN LOGIC **********/
 FIND FIRST simctr NO-LOCK NO-ERROR.
 wsCo = SIMCTR.CONAME.
 VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
 wsOpt:SCREEN-VALUE = "PENSION".
 ENABLE ALL WITH FRAME frm-Paysys.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
 HIDE FRAME frm-Paysys.
 RETURN.

PROCEDURE Report.ip:
    subTotal = 0.
    wsTotal = 0.
    wsTitle = "          " + wsOpt + " PAYROLL REPORT " 
        + "                        CURRENT PAYROLL DATE: " + STRING(PAYSYS.CURDATE).
FOR EACH payemf WHERE payemf.Paysys = wsSys AND payemf.Leaver = NO NO-LOCK :
    wsdes[4] = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
    wsAmt = 0.
    FOR EACH paymtf WHERE Paymtf.empcode = Payemf.empcode 
                      AND (Paymtf.Itemcode =  wsItem[1]
                       OR  Paymtf.Itemcode =  wsItem[2]
                       OR  Paymtf.Itemcode =  wsItem[3]) NO-LOCK:
        IF Paymtf.Itemcode =  wsItem[1] THEN
           ASSIGN wsAmt[1] = paymtf.CurAmt
                      wsTotal[1] = wsTotal[1] + paymtf.CurAmt.
        ELSE IF Paymtf.Itemcode =  wsItem[2] THEN
                ASSIGN wsAmt[2] = paymtf.CurAmt
                       wsTotal[2] = wsTotal[2] + paymtf.CurAmt.
        ELSE IF Paymtf.Itemcode =  wsItem[3] THEN
                ASSIGN wsAmt[3] = paymtf.CurAmt
                       wsTotal[3] = wsTotal[3] + paymtf.CurAmt.
    END.
    wsAmt[4] = wsAmt[2] + wsAmt[3].
    wsTotal[4] = wsTotal[4] + wsAmt[4].
    DISPLAY STREAM a payemf.empcode wsdes[4] wsAmt[1] wsAmt[2] wsAmt[3] wsAmt[4] WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
END.
UNDERLINE STREAM a  wsAmt[1] wsAmt[2] wsAmt[3] wsAmt[4] WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
DISPLAY STREAM a "TOTAL" @ wsdes[4]  wsTotal[1] @ wsAmt[1] wsTotal[2] @ wsAmt[2]
                  wsTotal[3] @ wsAmt[3] wsTotal[4] @ wsAmt[4]WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
 APPLY 'close' TO THIS-PROCEDURE.
END.
