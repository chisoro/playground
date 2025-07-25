SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt02.p
    Notes:...... ..........Payment listing Report
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsTotal LIKE wsAmt.
DEF VAR subTotal LIKE wsAmt.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR wscnt AS INT.
DEF VAR wsPer  AS INT FORM "999999".
DEF VAR wsCo AS CHAR FORM "x(100)".
DEF VAR wsFile AS CHAR FORM "x(40)".
 
 DEF BUTTON btn-Sys   LABEL "Payroll System".
 DEF BUTTON btn-exit  LABEL "CLOSE".
 DEF BUTTON btn-close LABEL "CLOSE".
 DEF BUTTON btn-ok    LABEL "PRINT".
DEF BUTTON btn-exp    LABEL "Export".

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 4.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.

 DEF    QUERY qry-Sys FOR Paysys SCROLLING.
 DEF BROWSE brw-Sys QUERY qry-Sys
         DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

 FORM
    payemf.empcode
    wsdes          LABEL "EMPLOYEES"
    payemf.Account LABEL "BANK ACCOUNT"
    wsAmt          LABEL "NET PAY"
    HEADER wsCo "PAGE" AT 80 PAGE-NUMBER(a) SKIP(1) 
          "PAYMENT LISTING REPORT FOR THE PERIOD " wsPer SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

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
     wsPer    COLON 26 LABEL "PAYROLL PERIOD" VIEW-AS TEXT SKIP(2)
     btn-ok colon 10 SPACE(20)  btn-exp
     btn-close colon 70
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 6.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 9
     TITLE "PAYMENT LISTING REPORT" VIEW-AS DIALOG-BOX.


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
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
              DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.CurPer @ wsPer WITH FRAME frm-Paysys.
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

 ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
 DO:
    ASSIGN wsSys = INT(wsSys:SCREEN-VALUE)
           wsPer = INT(wsPer:SCREEN-VALUE).
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 

 END.

ON 'choose':U OF btn-exp IN FRAME frm-Paysys 
 DO:
    ASSIGN wsSys = INT(wsSys:SCREEN-VALUE)
           wsPer = INT(wsPer:SCREEN-VALUE).
   OUTPUT STREAM b TO VALUE(wsFile).
   RUN rep-exp.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
END.
 /********** MAIN LOGIC **********/
 FIND FIRST simctr NO-LOCK NO-ERROR.
 wsCo = SIMCTR.CONAME.
 wsFile = simctr.repDir + "payrpt02" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
 VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
 ENABLE ALL WITH FRAME frm-Paysys.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
 HIDE FRAME frm-Paysys.
 RETURN.

PROCEDURE Report.ip:
    subTotal = 0.
    wsTotal = 0.
FOR EACH payemf WHERE payemf.Paysys = wsSys AND payemf.Leaver = NO NO-LOCK BREAK BY payemf.Bank:
    wsAmt = 0.
    FOR EACH paymtf WHERE Paymtf.empcode = Payemf.empcode AND CurAmt <> 0 NO-LOCK:
        FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-LOCK NO-ERROR.
        IF payitem.cat = 3 THEN NEXT.
        ASSIGN wsAmt = wsAmt + paymtf.CurAmt WHEN payitem.cat = 1
               wsAmt = wsAmt - paymtf.CurAmt WHEN payitem.cat = 2.
    END.
    FIND FIRST paybnk WHERE paybnk.bank = payemf.bank NO-LOCK NO-ERROR.
    IF AVAILABLE paybnk THEN
        wsdes = paybnk.BankName.
    ELSE
        wsdes = "Invalid Bank".
    IF FIRST-OF(payemf.bank) THEN DO:
        FIND FIRST paybnk WHERE paybnk.Bank = payemf.Bank NO-LOCK NO-ERROR.
        DISPLAY STREAM a wsDes WITH FRAME frm-rpt.
        DOWN 2 STREAM a WITH FRAME frm-rpt.
        subTotal = 0.
    END.
    ASSIGN wsdes    = TRIM(payemf.SName) + " " + TRIM(payemf.FName)
           subTotal = subTotal + wsAmt.
    IF wsAmt <> 0 THEN DO:
        DISPLAY STREAM a payemf.empcode wsdes payemf.Account  wsAmt WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
    IF LAST-OF(payemf.Bank) THEN DO:
        UNDERLINE STREAM a wsAmt WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a "SUB-TOTAL" @ wsdes subTotal @ wsAmt WITH FRAME frm-rpt.
        DOWN 2 STREAM a WITH FRAME frm-rpt.
        wsTotal = wsTotal + subTotal.
    END.
END.
UNDERLINE STREAM a wsAmt WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
DISPLAY STREAM a "TOTAL" @ wsdes  wsTotal @ wsAmt WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
 APPLY 'close' TO THIS-PROCEDURE.
END.

PROCEDURE rep-exp:
    subTotal = 0.
    wsTotal = 0.
    EXPORT STREAM b  DELIMITER ','  "DEPT" "GRADE" "EMPCODE" "NAME" "BANK" "BANK ACC" "AMOUNT".
    FOR EACH payemf WHERE payemf.Paysys = wsSys AND payemf.Leaver = NO NO-LOCK BREAK BY payemf.Bank:
        wsAmt = 0.
        FOR EACH paymtf WHERE Paymtf.empcode = Payemf.empcode AND CurAmt <> 0 NO-LOCK:
            FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-LOCK NO-ERROR.
            IF payitem.cat = 3 THEN NEXT.
            ASSIGN wsAmt = wsAmt + paymtf.CurAmt WHEN payitem.cat = 1
                   wsAmt = wsAmt - paymtf.CurAmt WHEN payitem.cat = 2.
        END.
        ASSIGN wsdes = payemf.SName  + " " + payemf.FName 
                      subTotal = subTotal + wsAmt.
        IF wsAmt <> 0 THEN
            EXPORT STREAM b  DELIMITER ','  payemf.Dept payemf.Grade payemf.empcode wsdes payemf.Bank payemf.Account wsAmt.
        
    END.
END PROCEDURE.
