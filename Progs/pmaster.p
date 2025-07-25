session:DATA-ENTRY-RETURN = TRUE.
session:set-wait-state("wait").
SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt04.p
    Notes:...... ..........Pension/Medical Aid listing Report
    Author:.................S. Mawire
*/
DEF STREAM b.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode INIT "1".
DEF VAR wsfile AS CHAR FORM "x(40)".
DEF VAR ws AS CHAR FORM "x(40)".
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wsGross AS DEC.
DEF VAR wsNet AS DEC.
DEF VAR wsCredit AS DEC.
DEF VAR wsg AS LOGICAL INITIAL YES.
DEF VAR wsn AS LOGICAL INITIAL YES.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsItem LIKE payitem.itemcode .
DEF VAR wsOpt AS CHAR FORM "X(15)" VIEW-AS COMBO-BOX LIST-ITEMS "PENSION", "MEDICAL AID".
 
DEF BUTTON btn-Sys   LABEL "PAYROLL SYSTEM".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".
DEF BUTTON btn-Item LABEL "ITEM".
 

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 11.3.

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
     Paysys.CurDate    COLON 25 LABEL "Current Payroll Date" SKIP(0.5) 
     "Enter items to include in the report(comma separated)" COLON 20 btn-Item NO-LABEL NO-TAB-STOP  SKIP
     ws  COLON 20 NO-LABEL SKIP(0.5)
     wsG COLON 40 LABEL "Do want Gross Pay included (Y/N)" SKIP(0.5)
     wsN COLON 40 LABEL "Do you want Net Pay included (y/N)" SKIP(1)
     payemf.empcode COLON 40 LABEL "Processing......" VIEW-AS TEXT NO-TAB-STOP SKIP(0.5)
     btn-Ok AT ROW 13.71 COL 15  LABEL "EXPORT" WIDGET-ID 48
     btn-Close AT ROW 13.71 COL 65 WIDGET-ID 48
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 13.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 16
     TITLE "PAYROLL REPORTER - EXCEL REPORTS" VIEW-AS DIALOG-BOX.


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
    APPLY 'tab' TO SELF.
    APPLY 'tab' TO wsSys.
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
              DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip  Paysys.CurDate WITH FRAME frm-Paysys.
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
     
 ON 'leave':U OF ws IN FRAME frm-paysys 
 DO:
     ASSIGN ws.
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
    ws = ws + STRING(Payitem.Itemcode) + ",".
    DISPLAY ws WITH FRAME frm-Paysys.
    APPLY 'entry' TO ws IN FRAME frm-paysys.
    RETURN. 
 END.

ON 'choose':U OF btn-ok IN FRAME frm-paysys 
DO:
    ASSIGN wssys ws wsg wsn.
    RUN rpt-ip.
    RETURN.
END.

/*main logic */
FIND FIRST SIMCTR NO-LOCK.
wsFile = simctr.repDir + "pitem.csv". 
OUTPUT STREAM b TO VALUE(wsFile).
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.

PROCEDURE rpt-ip:
EXPORT STREAM b DELIMITER "," "NAME" "ITEM" "AMOUNT".
FOR EACH payemf WHERE payemf.paysys = wsSys AND payemf.eStatus = 1 NO-LOCK:
    DISPLAY payemf.empcode WITH FRAME frm-paysys.
    PAUSE 0.
    IF wsg = YES OR wsn = YES THEN DO:
        ASSIGN wsgross = 0
               wsnet = 0.
        FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode AND Paymtf.CurAmt <> 0 NO-LOCK:
            FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-LOCK NO-ERROR.
            ASSIGN wsGross = wsGross + Paymtf.CurAmt WHEN cat = 1
                   wsNet = wsNet + Paymtf.CurAmt WHEN cat = 1
                   wsNet = wsNet - Paymtf.CurAmt WHEN cat = 2.
        END.
        IF wsGross <> 0 THEN
            EXPORT STREAM b DELIMITER "," FName + " " + SName "Gross Pay" wsGross.
        IF wsNet <> 0 THEN                                                       
       EXPORT STREAM b DELIMITER "," FName + " " + SName "Net Pay" wsNet. 
    END.
    DO X = 1 TO NUM-ENTRIES(ws):
       j = INT(ENTRY(x,ws)).
        FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode AND Paymtf.CurAmt <> 0 
                          AND paymtf.itemcode = j NO-LOCK:
             FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-LOCK NO-ERROR.
             /*IF Payitem.cat <> 3 THEN NEXT.
             ELSE*/
                 EXPORT STREAM b DELIMITER "," FName + " " + SName payitem.descrip CurAmt. 
        END.
    END.
END.
OUTPUT STREAM b CLOSE.
OS-COMMAND NO-WAIT VALUE("c:\simacc\bin\paymaster.xlsm").
 APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
END PROCEDURE.

