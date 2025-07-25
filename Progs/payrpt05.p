SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt05.p
    Notes:...... ..........Salary Reconciliation Report
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8           NO-UNDO.
DEF VAR wsline        AS INT.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR varUser LIKE simusr.usercode INITIAL 1.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-" EXTENT 3.
DEF VAR wsTotal LIKE wsAmt.
DEF VAR wsGross LIKE paymtf.CurAmt.
DEF VAR wsDed LIKE wsGross.
DEF VAR wsTax LIKE wsGross.
DEF VAR wsNet LIKE wsGross.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR wscnt AS INT EXTENT 2.
DEF VAR wsPer  AS INT FORM "999999".
DEF VAR wsCo AS CHAR FORM "x(100)".
DEF VAR X AS INT.
 
DEF TEMP-TABLE tblRec
    FIELD Itemcode LIKE paymtf.itemcode
    FIELD cat  LIKE payitem.cat
    FIELD Amt  LIKE paymtf.CurAmt.

DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".

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
    SKIP(3)
    HEADER wsCo "PAGE" AT 90 PAGE-NUMBER(a) SKIP(1) 
          "PAYROLL RECONCILIATION REPORT FOR THE PERIOD " wsPer
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-hdr.

FORM 
    "NUMBER OF EMPLOYEES:" wsCnt[2] COLON 20 SKIP(1)
    "GROSS INCOME: "        wsGross COLON 20 SKIP(1)
    "ALLOWABLE DEDUCTIONS:" wsDed  COLON 20 SKIP(1)
    "TAXABLE INCOME:"       wsTax  COLON 20 SKIP(1)
    "NET PAY:"              wsNet  COLON 20 SKIP(1)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt1.

FORM
    Payitem.Itemcode
    wsdes    LABEL "PAYROLL ITEM"
    wsAmt[1] LABEL "INCOME"
    wsAmt[2] LABEL "DEDUCTIONS"
    wsAmt[3] LABEL "ACRUALS"
    HEADER "DETAILED ITEM LISTINGS" SKIP(1)
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
     wsPer    COLON 26 LABEL "PAYROLL PERIOD" VIEW-AS TEXT SKIP(2)
     btn-ok colon 20
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 6.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 9
     TITLE "PAYROLL ANALYSIS REPORT" VIEW-AS DIALOG-BOX.


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
 /********** MAIN LOGIC **********/
 FIND FIRST simctr NO-LOCK NO-ERROR.
 wsCo = SIMCTR.CONAME.
 VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
 ENABLE ALL WITH FRAME frm-Paysys.
 WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
 HIDE FRAME frm-Paysys.
 RETURN.

PROCEDURE Report.ip:
    wsCnt = 0.
FOR EACH payitem WHERE payitem.cat <> 0:
    wscnt[1] = 0.
    FOR EACH paymtf WHERE  paymtf.Itemcode = payitem.itemcode AND CurAmt <> 0:
        FIND FIRST payemf WHERE payemf.empcode = paymtf.empcode NO-LOCK NO-ERROR.
        IF payemf.Paysys <> wsSys THEN NEXT.
        ASSIGN wsGross = wsGross + paymtf.CurAmt WHEN Payitem.cat = 1
               wsTax   = wsTax   + paymtf.CurAmt WHEN payitem.tax = 1
               wsTax   = wsTax   - paymtf.CurAmt WHEN payitem.tax = 2
               wsDed   = wsDed   + paymtf.CurAmt WHEN payitem.tax = 2
               wsNet   = wsNet   + paymtf.CurAmt WHEN Payitem.cat = 1
               wsNet   = wsNet   - paymtf.CurAmt WHEN Payitem.cat = 2.
        FIND FIRST tblRec WHERE tblRec.itemcode = paymtf.Itemcode NO-ERROR.
        IF NOT AVAILABLE tblRec THEN DO:
            CREATE tblRec.
            ASSIGN tblRec.itemcode = paymtf.Itemcode
                   tblrec.cat      = Payitem.cat.
        END.
        ASSIGN tblRec.Amt = tblRec.Amt + paymtf.CurAmt.
               wscnt[1] = wscnt[1] + 1.
    END.
    IF wscnt[1] > wsCnt[2] THEN wsCnt[2] = wsCnt[1].
END.
DISPLAY STREAM a WITH FRAME frm-hdr.
DISPLAY STREAM a wsGross wsTax wsDed wsNet wscnt[2] WITH FRAME frm-rpt1.
DOWN STREAM a WITH FRAME frm-rpt1.
FOR EACH tblrec:
    wsAmt = 0.
    ASSIGN wsAmt[1] = tblRec.Amt WHEN tblRec.cat = 1
           wsAmt[2] = tblRec.Amt WHEN tblRec.cat = 2
           wsAmt[3] = tblRec.Amt WHEN tblRec.cat = 3.
   FIND FIRST payitem WHERE payitem.itemcode = tblRec.itemcode NO-ERROR.
   DISPLAY STREAM a Payitem.Itemcode payitem.descrip @ wsdes wsAmt[1] wsAmt[2]
             wsAmt[3] WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   IF LINE-COUNTER(a) + 3 > PAGE-SIZE(a) THEN DO:
       PAGE STREAM a.
       DISPLAY STREAM a WITH FRAME frm-hdr.   
   END.
END.
 APPLY 'close' TO THIS-PROCEDURE.
END.

