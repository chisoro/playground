SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................paycon.p
    Notes:...... ..........Payroll Costing and consolidation
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 8           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser       LIKE simusr.usercode.
DEF VAR wsSys         LIKE Paysys.Paysys.
DEF VAR wsAmt         AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsDr          LIKE wsAmt.
DEF VAR wsCr          LIKE wsAmt.
DEF VAR wsLedger      LIKE paycost.Ledger.
DEF VAR wsdes         AS CHAR FORM "x(40)".
DEF VAR st-per        AS INT FORM "999999".
DEF VAR wsTime        AS CHAR FORM "x(8)".
DEF VAR wsCo          AS CHAR FORM "x(100)".
DEF VAR wsTransId     AS DEC FORM "99999999999999".
DEF VAR wsRate    LIKE tblforex.decRate.
 
 DEF BUTTON btn-Sys   LABEL "Payroll System".
 DEF BUTTON btn-exit  LABEL "CLOSE".
 DEF BUTTON btn-close LABEL "CLOSE".
 DEF BUTTON btn-ok    LABEL "CONSOLIDATE".

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 4.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.
DEF RECT rec SIZE 116 BY 17.
DEF RECT rec1 SIZE 116 BY 2.5.

 DEF    QUERY qry-Sys FOR Paysys SCROLLING.
 DEF BROWSE brw-Sys QUERY qry-Sys
         DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qryView FOR Paycost SCROLLING.
DEF BROWSE brwView QUERY qryView
        DISPLAY paycost.Proj paycost.Fund paycost.Dept paycost.Ledger paycost.empcode 
                paycost.Itemcode COLUMN-LABEL "Code" wsdes COLUMN-LABEL "Description"
                paycost.Amount COLUMN-LABEL "Amount" WITH 20 DOWN SEPARATORS.
 FORM
     SKIP(2)
     HEADER wsCo "PAGE" AT 90 PAGE-NUMBER(a) SKIP(1) 
          "PAYROLL CONSOLIDATION REPORT FOR PAYROLL: " wsSys " FOR THE PERIOD " st-per SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-hdr.

 FORM paycost.Proj LABEL "PROG"
      paycost.Fund LABEL "SEG"
      paycost.Dept LABEL "DEPT"
      wsLedger     LABEL "LEDGER"
      wsdes        LABEL "DESCRIPTION"
      wsDr         LABEL "DEBIT"
      wsCr         LABEL "CREDIT"
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

 DEFINE FRAME frm-View 
     brwView AT ROW 2 COL 5
     skip(1)
     btn-ok colon 20 LABEL "CONSOLIDATE"
     btn-close colon 80
     rec AT ROW 1.5 COL 3
     rec1 AT ROW 18.7 COL 3
     with view-as dialog-box keep-tab-order no-validate
     SIZE 121 BY 22
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll Costing Data".

 DEFINE FRAME frm-pick 
     brw-Sys AT ROW 2 COL 5
     skip(0.5)
     btn-ok colon 5 LABEL "OK"
     btn-close colon 60
     with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

 DEFINE FRAME frm-Paysys
     SKIP(1.5)
     btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
     wsSys              NO-LABEL AUTO-RETURN 
     Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     st-per    COLON 26 LABEL "PAYROLL PERIOD" VIEW-AS TEXT SKIP(2)
     btn-ok colon 20 LABEL "PROCESS"
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 6.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 9
     TITLE "PAYROLL LEDGER CONSOLIDATION " VIEW-AS DIALOG-BOX.


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
         st-per = INT(STRING(YEAR(Paysys.Curdate)) + STRING(MONTH(Paysys.Curdate),"99")).
         FIND FIRST tblforex WHERE tblforex.txtcur = paysys.txtcur NO-LOCK NO-ERROR.
         IF AVAILABLE tblforex AND simctr.LdCur <> paysys.txtcur THEN
             wsRate = tblForex.decRate.
         ELSE wsRate = 1.
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
           DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip st-per WITH FRAME frm-Paysys.
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
     ASSIGN wsSys.
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         st-per = INT(STRING(YEAR(Paysys.Curdate)) + STRING(MONTH(Paysys.Curdate),"99")).
         FIND FIRST tblforex WHERE tblforex.txtcur = paysys.txtcur NO-LOCK NO-ERROR.
         IF AVAILABLE tblforex AND simctr.LdCur <> paysys.txtcur THEN
             wsRate = tblForex.decRate.
         ELSE wsRate = 1.
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
           DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip st-per WITH FRAME frm-Paysys.
           RUN ProView.ip.
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

 ON row-display OF brwView DO:
     FIND FIRST payitem WHERE payitem.itemcode = paycost.itemcode NO-LOCK NO-ERROR.
     wsDes = Payitem.Descrip.
 END.

 ON 'choose':U OF btn-ok IN FRAME frm-View 
 DO:
    ASSIGN wsTime    = STRING(TIME,"HH:MM:SS").
           wsTransID = DEC (string(YEAR(TODAY),"9999")
                     + string(MONTH(TODAY),"99" )
                     + string(DAY(TODAY),"99")
                     + SUBSTR(STRING(wsTIME),1,2) 
                     + SUBSTR(STRING(wsTIME),4,2) 
                     + SUBSTR(STRING(wsTIME),7,2) ).
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 
    HIDE FRAME frm-View.
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

{paycon.i}
