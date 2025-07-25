SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payrpt03.p
    Notes:...... ..........Payroll Item listing Report
    Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "Search".
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10           NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsTotal LIKE wsAmt EXTENT 2.
DEF VAR subTotal LIKE wsAmt.
DEF VAR wsdes AS CHAR FORM "x(40)".
DEF VAR wscnt AS INT.
DEF VAR wsPer  AS INT FORM "999999".
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

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

 DEF    QUERY qry-Sys FOR Paysys SCROLLING.
 DEF BROWSE brw-Sys QUERY qry-Sys
         DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Description" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

 DEF    QUERY qry-pick FOR Payitem SCROLLING.
 DEF BROWSE brw-pick QUERY qry-pick
         DISPLAY Payitem.Itemcode Payitem.Descrip COLUMN-LABEL "Description" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

 FORM
    payemf.empcode
    wsdes          LABEL "EMPLOYEES"
    Paymtf.CurAmt LABEL "CUR AMOUNT"
    Paymtf.YtdAmt LABEL "YTD AMOUNT"
    HEADER wsCo "PAGE" AT 80 PAGE-NUMBER(a) SKIP(1) 
          "PAYROLL ITEM LISTING REPORT FOR THE PERIOD " wsPer SKIP(2)
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
     wsPer    COLON 30 LABEL "PAYROLL PERIOD" VIEW-AS TEXT SKIP(0.5)
     btn-Item COLON 13 NO-LABEL NO-TAB-STOP
     payitem.itemcode  NO-LABEL
     Payitem.Descrip   NO-LABEL VIEW-AS TEXT FORM "x(40)" SKIP(1.5) 
     btn-ok colon 20
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 7.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 10
     TITLE "PAYROLL ITEM CODE REPORT" VIEW-AS DIALOG-BOX.


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
           wsPer = INT(wsPer:SCREEN-VALUE).
    {PrintOpt.i &stream-name="stream a"
                     &print-prog="report.ip"
                     &paged} 

 END.

 on 'start-search':u of BROWSE  brw-pick
    run Search.ip.

/********** MAIN LOGIC **********/
browse  brw-pick:allow-column-searching = true.
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
FOR EACH payemf WHERE payemf.Paysys = wsSys AND payemf.Leaver = NO NO-LOCK :
    wsdes    = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
    FOR EACH paymtf WHERE Paymtf.empcode = Payemf.empcode 
                      AND Paymtf.Itemcode = INT(payitem.itemcode:SCREEN-VALUE IN FRAME frm-Paysys) NO-LOCK:
        IF Paymtf.CurAmt = 0 AND Paymtf.YtdAmt = 0 THEN NEXT.
        ASSIGN wsTotal[1] = wsTotal[1] + paymtf.CurAmt
               wsTotal[2] = wsTotal[2] + paymtf.YtdAmt.
        DISPLAY STREAM a payemf.empcode wsdes Paymtf.CurAmt Paymtf.YtdAmt WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
END.
UNDERLINE STREAM a Paymtf.CurAmt Paymtf.YtdAmt WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
DISPLAY STREAM a "TOTAL" @ wsdes  wsTotal[1] @ Paymtf.CurAmt wsTotal[2] @ Paymtf.YtdAmt WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
 APPLY 'close' TO THIS-PROCEDURE.
END.

procedure Search.ip.
  hCol = browse  brw-pick:current-column.
    view frame Search-opt.
    enable all with frame Search-opt.
    WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
        OR 'window-close' of frame Search-opt or close of this-procedure or
            'esc','f4' of frame Search-opt.
    hide frame Search-opt.
    case trim(hCol:label):
        WHEN "ITEM" then
             open query qry-pick
                  FOR EACH payitem  WHERE payitem.ITEM >= INT(wsSearch:SCREEN-VALUE) no-lock.
        WHEN "DESCRIPTION" then
             open query qry-pick
                  FOR EACH payitem  WHERE payitem.Descrip >= wsSearch:SCREEN-VALUE NO-LOCK BY payitem.Descrip.
        END.
    RETURN.
END. 
