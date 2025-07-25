/* Program.................payrpt10.p
    Notes:...... ..........Item code listing Report
    Author:.................S.Chisoro & S. Mawire
*/

session:set-wait-state("wait").
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM b.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsfile AS CHAR FORM "x(40)".
DEF VAR wsfile1 AS CHAR FORM "x(40)".
DEF VAR ws AS CHAR FORM "x(160)".
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wsDate AS DATE.
DEF VAR wsDate1 AS DATE.
DEF VAR wsOpt AS CHAR FORM "X(15)" VIEW-AS COMBO-BOX LIST-ITEMS "Current", "Historical".
DEF VAR wsGross AS DEC.
DEF VAR wsNet AS DEC.
DEF VAR wsCredit AS DEC.
DEF VAR wsAmt AS DEC.
DEF VAR wsg AS LOGICAL INITIAL YES.
DEF VAR wsn AS LOGICAL INITIAL YES.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsItem LIKE payitem.itemcode .
DEF BUTTON btn-Sys   LABEL "PAYROLL SYSTEM".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".
DEF BUTTON btn-Item LABEL "ITEM".
DEF BUTTON btnDate  LABEL "PICK DATE".
DEF BUTTON btnDate1  LABEL "PICK DATE".
DEFINE TEMP-TABLE tmpDate
    FIELD trDate LIKE payhtf.trDate.

DEF  QUERY qry-PickDate FOR tmpDate SCROLLING.
DEF BROWSE brw-PickDate QUERY qry-PickDate
    DISPLAY tmpDate.trDate 
    WITH 10 DOWN SEPARATORS.

DEFINE FRAME frm-PickDate 
    brw-PickDate AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 3
    btn-close colon 10
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Pay Date Selection".

DEF  QUERY qry-PickDate1 FOR tmpDate SCROLLING.
DEF BROWSE brw-PickDate1 QUERY qry-PickDate1
    DISPLAY tmpDate.trDate 
    WITH 10 DOWN SEPARATORS.

DEFINE FRAME frm-PickDate1 
    brw-PickDate1 AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 3
    btn-close colon 10
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Pay Date Selection".
 
 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 14.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 2.5.

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
     btn-Sys         COLON 10 NO-LABEL NO-TAB-STOP
     wsSys              NO-LABEL AUTO-RETURN 
     Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     Paysys.CurDate  COLON 25 LABEL "Current Payroll Date To:" SPACE(5)
     wsOpt            LABEL "Report option"SKIP(0.5)  
     wsDate1       COLON 25   LABEL "First Payroll Run Date "  btnDate1 SPACE(5)
     wsDate          LABEL "Last Payroll Run Date "  btnDate SKIP(0.5)
     "Enter items to include in the report(comma separated)" COLON 20  SKIP
     ws     COLON 20 NO-LABEL  VIEW-AS EDITOR SIZE 60 BY 3 btn-Item NO-LABEL NO-TAB-STOP SKIP(0.5)
     wsG    COLON 40 LABEL "Do want Gross Pay included (Y/N)" SKIP(0.5)
     wsN    COLON 40 LABEL "Do you want Net Pay included (y/N)" SKIP(1)
     payemf.empcode COLON 40 LABEL "Processing......" VIEW-AS TEXT NO-TAB-STOP SKIP(0.5)
     btn-Ok AT ROW 16.71 COL 15  LABEL "EXPORT" WIDGET-ID 48
     btn-Close AT ROW 16.71 COL 65 WIDGET-ID 48
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 16.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D
      SIZE 110 BY 19 
     TITLE "PAYROLL REPORTER - EXCEL REPORTS" VIEW-AS DIALOG-BOX.


 /* Triggers for the Paysys frame */
 ON CHOOSE OF BtnDate IN FRAME frm-Paysys
   
DO:
  
    FOR EACH payhtf BREAK BY payhtf.trDate.
        IF LAST-OF(payhtf.trDate) THEN DO:
            CREATE tmpDate.
            ASSIGN tmpDate.trDate = payhtf.trDate.
        END.
    END.


  VIEW FRAME frm-PickDate.
  OPEN QUERY qry-PickDate FOR EACH tmpDate.
  ENABLE ALL WITH FRAME frm-PickDate.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickDate 
          OR close of THIS-PROCEDURE IN FRAME frm-PickDate
          OR CHOOSE OF btn-ok IN FRAME frm-PickDate 
          OR 'enter':u OF brw-PickDate
          OR 'mouse-select-dblclick' OF brw-PickDate.
  CLOSE QUERY qry-PickDate.
  FOR EACH tmpDate.
      DELETE tmpDate.
  END.
  HIDE FRAME frm-PickDate.
  APPLY 'tab' TO wsDate IN FRAME frm-Paysys.
  APPLY 'tab'TO btnDate IN FRAME frm-paysys.
  APPLY 'entry' TO ws IN FRAME frm-paysys.
END.

 ON CHOOSE OF BtnDate1 IN FRAME frm-Paysys
   
DO:
  
    FOR EACH payhtf BREAK BY payhtf.trDate.
        IF LAST-OF(payhtf.trDate) THEN DO:
            CREATE tmpDate.
            ASSIGN tmpDate.trDate = payhtf.trDate.
        END.
    END.


  VIEW FRAME frm-PickDate1.
  OPEN QUERY qry-PickDate1 FOR EACH tmpDate.
  ENABLE ALL WITH FRAME frm-PickDate1.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickDate1 
          OR close of THIS-PROCEDURE IN FRAME frm-PickDate1
          OR CHOOSE OF btn-ok IN FRAME frm-PickDate1 
          OR 'enter':u OF brw-PickDate1
          OR 'mouse-select-dblclick' OF brw-PickDate1.
  CLOSE QUERY qry-PickDate1.
  FOR EACH tmpDate.
      DELETE tmpDate.
  END.
  HIDE FRAME frm-PickDate1.
  APPLY 'tab' TO wsDate1 IN FRAME frm-Paysys.
  APPLY 'tab'TO btnDate1 IN FRAME frm-paysys.
  APPLY 'entry' TO wsDate IN FRAME frm-paysys.
END.
    
ON CHOOSE OF btn-ok IN FRAME frm-PickDate1 
    OR 'enter':u OF brw-PickDate1
    OR 'mouse-select-dblclick' OF brw-PickDate1
DO: 
   GET CURRENT qry-PickDate1 NO-LOCK NO-WAIT.
  wsDate1:SCREEN-VALUE IN FRAME frm-Paysys = string(tmpDate.trDate).
   APPLY 'tab' TO wsDate1 IN FRAME frm-Paysys.
    APPLY 'tab' TO btnDate1 IN FRAME frm-paysys.
    APPLY 'entry' TO wsDate IN FRAME frm-paysys.
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickDate 
    OR 'enter':u OF brw-PickDate
    OR 'mouse-select-dblclick' OF brw-PickDate
DO: 
   GET CURRENT qry-PickDate NO-LOCK NO-WAIT.
  wsDate:SCREEN-VALUE IN FRAME frm-Paysys = string(tmpDate.trDate).
   APPLY 'tab' TO wsDate IN FRAME frm-Paysys.
    APPLY 'tab' TO btnDate IN FRAME frm-paysys.
    APPLY 'entry' TO ws IN FRAME frm-paysys.
END.

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

 ON 'leave':U OF wsopt IN FRAME frm-paysys
     OR 'enter':U  OF wsopt IN FRAME frm-paysys
 DO:
     ASSIGN wsopt.
     IF wsopt = "Current" THEN DO:
         DISABLE wsdate1 btnDate1 wsdate btnDate WITH FRAME frm-paysys.
         RETURN.
     END.
     ELSE IF wsopt = "Historical" THEN DO:
        ENABLE wsdate btnDate wsDate1 btnDate1 WITH FRAME frm-paysys.
        APPLY 'entry' TO wsDate IN FRAME frm-paysys.
     END.
         
     
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
    ASSIGN wssys ws wsg wsn wsdate wsopt.
    IF wsopt = "Current" THEN
       RUN rpt-ip.
    ELSE IF wsopt = "Historical" THEN
       RUN rpth-ip.
    RETURN.
END.

ON  'enter':U OF wsDate IN FRAME frm-Paysys
     OR 'tab':U OF wsDate IN FRAME frm-Paysys
 DO:
    APPLY 'tab'TO wsDate IN FRAME frm-paysys.
    APPLY 'entry' TO ws IN FRAME frm-paysys.
 END.


/*main logic */
FIND FIRST SIMCTR NO-LOCK.
wsFile = simctr.repDir + "pitem.csv".
wsFile1 = substring(simctr.repDir,1,(LENGTH(simctr.repDir) - 8)).
OUTPUT STREAM b TO VALUE(wsFile).
wsOpt:SCREEN-VALUE = "Current".
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.

PROCEDURE rpt-ip:
EXPORT STREAM b DELIMITER "," "DEPT" "GRADE" "CODE" "NAME" "ITEM" "AMOUNT".
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
            EXPORT STREAM b DELIMITER ","  Dept Grade payemf.empcode FName + " " + SName "Gross Pay" wsGross.
        IF wsNet <> 0 THEN                                                       
       EXPORT STREAM b DELIMITER ","  Dept Grade payemf.empcode FName + " " + SName "Net Pay" wsNet. 
    END.
    DO X = 1 TO NUM-ENTRIES(ws):
       j = INT(ENTRY(x,ws)).
        FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode AND Paymtf.CurAmt <> 0 
                          AND paymtf.itemcode = j NO-LOCK:
             FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-LOCK NO-ERROR.
             /*IF Payitem.cat <> 3 THEN NEXT.
             ELSE*/
                 EXPORT STREAM b DELIMITER "," Dept Grade payemf.empcode FName + " " + SName payitem.descrip CurAmt. 
        END.
    END.
END.
OUTPUT STREAM b CLOSE.
OS-COMMAND NO-WAIT VALUE(wsFile1 + "bin\pmaster.exe").
 APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
END PROCEDURE.

PROCEDURE rpth-ip:
    /*MESSAGE string(wsDate1:SCREEN-VALUE IN FRAME frm-paysys)  +  "    " + STRING(wsDate:SCREEN-VALUE IN FRAME frm-paysys) VIEW-AS ALERT-BOX.*/
EXPORT STREAM b DELIMITER "," "DEPT" "GRADE" "CODE" "NAME" "ITEM" "AMOUNT".
FOR EACH payemf WHERE payemf.paysys = wsSys AND payemf.eStatus = 1 NO-LOCK:
    DISPLAY payemf.empcode WITH FRAME frm-paysys.
    PAUSE 0.
    
    IF wsg = YES OR wsn = YES THEN DO:
        ASSIGN wsgross = 0
               wsnet = 0.
        FOR EACH payhtf WHERE payhtf.empcode = payemf.empcode AND Payhtf.CurAmt <> 0
            AND payhtf.trDate <= date(wsDate:SCREEN-VALUE IN FRAME frm-paysys)  AND payhtf.trDate >= date(wsDate1:SCREEN-VALUE IN FRAME frm-paysys) NO-LOCK:
            FIND FIRST payitem WHERE payitem.itemcode = payhtf.itemcode NO-LOCK NO-ERROR.
            ASSIGN wsGross = wsGross + Payhtf.CurAmt WHEN cat = 1
                   wsNet = wsNet + Payhtf.CurAmt WHEN cat = 1
                   wsNet = wsNet - Payhtf.CurAmt WHEN cat = 2.
        END.
        IF wsGross <> 0 THEN
            EXPORT STREAM b DELIMITER "," Dept Grade payemf.empcode FName + " " + SName "Gross Pay" wsGross.
        IF wsNet <> 0 THEN                                                       
       EXPORT STREAM b DELIMITER ","  Dept Grade payemf.empcode FName + " " + SName "Net Pay" wsNet. 
    END.
    DO X = 1 TO NUM-ENTRIES(ws):
        wsAmt = 0.
       j = INT(ENTRY(x,ws)).
         FOR EACH payhtf WHERE payhtf.empcode = payemf.empcode AND Payhtf.CurAmt <> 0
            AND payhtf.trDate <= date(wsDate:SCREEN-VALUE IN FRAME frm-paysys) AND payhtf.trDate >= date(wsDate1:SCREEN-VALUE IN FRAME frm-paysys) AND payhtf.itemcode = j NO-LOCK:
             FIND FIRST payitem WHERE payitem.itemcode = payhtf.itemcode NO-LOCK NO-ERROR.
             /*IF Payitem.cat <> 3 THEN NEXT.
             ELSE*/
                  wsAmt = wsAmt + CurAmt.
        END.
        IF wsamt <> 0 THEN
            EXPORT STREAM b DELIMITER ","  Dept Grade payemf.empcode FName + " " + SName payitem.descrip wsAmt.
        
    END.
END.
OUTPUT STREAM b CLOSE.
OS-COMMAND NO-WAIT VALUE(wsFile1 + "bin\pmaster\pmaster.exe")..
 APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
END PROCEDURE.

