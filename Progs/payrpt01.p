SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt01.p
    Notes:...... ..........Payroll Analysis Report
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
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-" EXTENT 3.
DEF VAR wsTotal LIKE wsAmt.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR wscnt AS INT.
DEF VAR wsPer  AS INT FORM "999999".
DEF VAR wsCo AS CHAR FORM "x(100)".
 
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
    Payitem.Itemcode
    wsdes    LABEL "PAYROLL ITEM"
    wscnt    LABEL "EMPLOYEES"
    wsAmt[1] LABEL "INCOME"
    wsAmt[2] LABEL "DEDUCTIONS"
    wsAmt[3] LABEL "ACRUALS"
    HEADER wsCo "PAGE" AT 90 PAGE-NUMBER(a) SKIP(1) 
          "PAYROLL ANALYSIS REPORT FOR THE PERIOD " wsPer SKIP(2)
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
    ASSIGN wsSys.
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
    {PrintOpt.i &stream-name = "stream a"
                     &print-prog = "report.ip"
                     &paged} 
 RETURN.
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
FOR EACH paymtf WHERE CurAmt <> 0 BREAK BY Itemcode:
    FIND FIRST payemf WHERE payemf.empcode = paymtf.empcode NO-LOCK NO-ERROR.
    IF payemf.Paysys <> wsSys THEN NEXT.
    ACCUMULATE paymtf.CurAmt (TOTAL BY Itemcode)
               paymtf.CurAmt (COUNT BY Itemcode).
    IF FIRST-OF(paymtf.Itemcode) THEN
        wsAmt = 0.
    IF LAST-OF(itemcode) THEN DO:
        FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-ERROR.
        IF AVAILABLE payitem  AND payitem.cat <> 0 THEN DO:
            ASSIGN wsAmt[1] = (ACCUM TOTAL BY paymtf.Itemcode CurAmt) WHEN Payitem.cat = 1 OR Payitem.cat = 4
                   wsAmt[2] = (ACCUM TOTAL BY paymtf.Itemcode CurAmt) WHEN Payitem.cat = 2
                   wsAmt[3] = (ACCUM TOTAL BY paymtf.Itemcode CurAmt) WHEN Payitem.cat = 3 
                   wscnt    = (ACCUM COUNT BY paymtf.Itemcode CurAmt) 
                   wsdes    =  Payitem.Descrip
                   wsTotal[1] = wsTotal[1] + wsAmt[1]
                   wsTotal[2] = wsTotal[2] + wsAmt[2]
                   wsTotal[3] = wsTotal[3] + wsAmt[3] .
            DISPLAY STREAM a Payitem.Itemcode wsdes wscnt wsAmt[1] wsAmt[2] wsAmt[3] WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
    END.
END.
UNDERLINE STREAM a wsdes wsAmt[1] wsAmt[2] wsAmt[3]WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
DISPLAY STREAM a "TOTAL" @ wsdes  wsTotal[1] @ wsAmt[1] wsTotal[2] @ wsAmt[2] 
                  wsTotal[3] @ wsAmt[3] WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
APPLY 'close' TO THIS-PROCEDURE.
END.
