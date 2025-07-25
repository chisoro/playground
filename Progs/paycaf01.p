session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paycaf01.p
   Notes:...... Change Calculation factor - Single employee.
   Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "Search".
DEF VAR X AS INT.
DEF VAR wsNotch AS DEC.
DEF VAR wsDate AS DATE.
DEF VAR wsAmt LIKE paymtf.curAmt.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsName AS CHAR FORM "x(70)".
DEF VAR wsEmp LIKE payemf.empcode.
DEF VAR wsitem LIKE payitem.itemcode.
DEF VAR wsDesc AS CHAR FORM "x(40)".
DEF VAR wsfrm AS CHAR FORM "x(40)".
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF SHARED VAR varUser LIKE simusr.usercode.

DEF BUTTON btn-item LABEL "ITEM CODE".
DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-Emp   LABEL "EMPLOYEE".

DEF RECT rec  SIZE 72 BY 6.
DEF RECT rec1 SIZE 72 BY 2.
DEF RECT rec2 SIZE 94 BY 15.5.
DEF RECT rec3 SIZE 94 BY 2.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
        DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
        WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
    brw-Sys AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEF    QUERY qry-emp FOR Payemf SCROLLING.
DEF BROWSE brw-emp QUERY qry-emp
        DISPLAY payemf.empcode  payemf.SName WIDTH 40 payemf.FName 
        WIDTH 30 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-emp 
    brw-emp AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Employee Selection".

DEF    QUERY qry-item FOR Payitem SCROLLING.
DEF BROWSE brw-item QUERY qry-item
        DISPLAY payitem.Itemcode payitem.Descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-fac FOR Paymtf SCROLLING.
DEF BROWSE brw-fac QUERY qry-fac
        DISPLAY Paymtf.Itemcode COLUMN-LABEL   "ITEM"
                wsdesc          COLUMN-LABEL  "DESCRIPTION"
                Paymtf.StdAmt   COLUMN-LABEL "STD AMT"
                paymtf.curAmt   COLUMN-LABEL "CURRENT"
                paymtf.ytdAmt   COLUMN-LABEL "YTD AMT"
                Paymtf.Active   COLUMN-LABEL "FAC" ENABLE Paymtf.ACTIVE WITH 16 DOWN SEPARATORS.

DEFINE FRAME frm-fac 
    SKIP(0.5)
    btn-emp   COLON 10 NO-LABEL  NO-TAB-STOP
    Payemf.empcode   NO-LABEL AUTO-RETURN 
    wsName NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
    brw-fac AT ROW 3 COL 5
    skip(1.05)
    btn-ok colon 5
    btn-close colon 60
    rec2 AT ROW 1.2     COL 3
    rec3 AT ROW 16.8   COL 3
    with view-as dialog-box keep-tab-order no-validate
     AT ROW 11 COL 5 SIZE 100 BY 19.5    side-labels no-underline three-d SCROLLABLE 
    CENTERED TITLE "Payroll Item De-/Activation".

DEFINE FRAME frm-item 
    brw-item AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Employee Selection".

DEFINE FRAME frm-Paysys
    SKIP(0.5)
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
    wsSys     NO-LABEL AUTO-RETURN 
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(1)
    Paysys.CurPer  COLON 25 LABEL " Current Period   " VIEW-AS TEXT SKIP(1)
    Paysys.CurDate COLON 25 LABEL "Payroll Date   " VIEW-AS TEXT SKIP(2)
    btn-ok colon 10
    btn-close colon 50
    rec AT ROW 1.2     COL 3
    rec1 AT ROW 7.4   COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D SIZE 77 BY 10 
    TITLE "PAYROLL SELECTION" VIEW-AS DIALOG-BOX.

/* Triggers for the Paysys frame */

ON row-display OF brw-fac DO:
    FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-ERROR.
    wsdesc = payitem.descrip.
END.

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
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-Paysys.
   ASSIGN wsSys = INT(wsSys:SCREEN-VALUE).
   RETURN. 
END.

ON  'enter':U OF wsSys IN FRAME frm-Paysys
    OR 'tab':U OF wsSys IN FRAME frm-Paysys
    OR 'leave':U OF wsSys IN FRAME frm-Paysys
DO:
    ASSIGN wsSys.
    IF INT(wsSys:SCREEN-VALUE) = 0 THEN
        APPLY 'close' TO THIS-PROCEDURE.
    ELSE DO:
        ASSIGN wsSys.
        FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE paysys THEN DO:
           FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
           IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
           DO:
              DISPLAY Paysys.Descrip  Paysys.CurDate Paysys.CurPer WITH FRAM frm-Paysys.
              ENABLE btn-ok WITH FRAME frm-paysys.
              APPLY 'entry' TO btn-ok IN FRAME frm-paysys.
              RETURN NO-APPLY.
           END.
           ELSE DO:
              MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
              DISABLE btn-ok WITH FRAME frm-paysys.
              RETURN NO-APPLY.
           END.
        END.  
        ELSE DO:
            MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    END.
     RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN
       RUN Datacap.ip.
    APPLY 'close' TO THIS-PROCEDURE.
END.

ON CHOOSE OF btn-ok IN FRAME frm-fac
DO:
   CLOSE QUERY qry-fac.
   CLEAR FRAME frm-fac.
   APPLY 'entry' TO Payemf.empcode IN FRAME frm-fac.
END.
/* Triggers for the button empcode */
ON CHOOSE OF btn-emp IN FRAME frm-fac
DO:
  wsfrm = FRAME-NAME.
  VIEW FRAME frm-emp.
  OPEN QUERY qry-emp FOR EACH payemf WHERE payemf.paysys = wsSys AND (payemf.estatus = 1 OR payemf.estatus = 2) NO-LOCK.
  ENABLE ALL WITH FRAME frm-emp.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-emp 
          OR close of THIS-PROCEDURE IN FRAME frm-emp
          OR CHOOSE OF btn-ok IN FRAME frm-emp 
          OR 'enter':u OF brw-emp
          OR 'mouse-select-dblclick' OF brw-emp.
  CLOSE QUERY qry-emp.
  HIDE FRAME frm-emp.
  APPLY 'tab' TO SELF IN FRAME frm-fac.
  APPLY 'tab' TO payemf.empcode IN FRAME frm-fac.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-emp 
    OR 'enter':u OF brw-emp
    OR 'mouse-select-dblclick' OF brw-emp
DO: 
   GET CURRENT qry-emp NO-LOCK NO-WAIT.
   wsEmp = payemf.empcode.
   wsName = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
   FIND FIRST payemf WHERE payemf.empcode = wsEmp AND payemf.paysys = wsSys NO-LOCK NO-ERROR.
   IF wsFrm = "frm-fac" THEN
      DISPLAY wsEmp @ payemf.empcode wsName WITH FRAME frm-fac.
END.

ON 'enter':u OF payemf.empcode IN FRAME frm-fac
    OR 'tab':u OF payemf.empcode IN FRAME frm-fac
DO: 
   wsEmp = DEC(payemf.empcode:SCREEN-VALUE IN FRAME frm-fac).
   FIND FIRST payemf WHERE payemf.empcode = wsEmp AND payemf.paysys = wsSys NO-LOCK NO-ERROR.
   IF NOT AVAILABLE payemf THEN DO:
       MESSAGE "Invalid Employee please try again"
           VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
   ELSE IF AVAILABLE payemf AND (payemf.estatus = 3 OR payemf.estatus = 4) THEN DO:
       MESSAGE "Employee not currently active, either left or suspended"
           VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
   ELSE DO:
       wsName = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
       DISPLAY wsEmp @ payemf.empcode wsName WITH FRAME frm-fac.
       OPEN QUERY qry-fac FOR EACH paymtf WHERE paymtf.empcode = wsemp EXCLUSIVE-LOCK.
   END.
END.



on 'start-search':u of BROWSE brw-emp
    run Search.ip.

/* Triggers for the button item */

/********** MAIN LOGIC **********/
browse brw-emp:allow-column-searching = true.
FIND FIRST payctr NO-LOCK NO-ERROR.
VIEW FRAME frm-Paysys.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.

PROCEDURE Datacap.ip:
    HIDE FRAME frm-Paysys.
    VIEW FRAME frm-fac.
    ENABLE ALL WITH FRAME frm-fac.
       WAIT-FOR CHOOSE OF btn-close OR CLOSE OF THIS-PROCEDURE IN FRAME frm-fac.
    HIDE FRAME frm-data.
    RETURN.
END.

procedure Search.ip.
hCol = browse brw-emp:current-column.
    /*assign  frame Search-opt:title = hCol:label + " Column"
            frame Search-opt:x     = hCol:x
            frame Search-opt:y     = browse b-{&t_zoom_table}:y. */
    view frame Search-opt.
    enable all with frame Search-opt.
    WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
        OR 'window-close' of frame Search-opt or close of this-procedure or
            'esc','f4' of frame Search-opt.
    hide frame Search-opt.
    /*if ll-sort = ? then return.*/
    case trim(hCol:label):
        when "SURNAME" then
        do:
           open query qry-emp
                    FOR EACH payemf WHERE payemf .Paysys = wsSys  
                        AND payemf .sname >= wsSearch:SCREEN-VALUE no-lock
                          BY payemf .sname.

        END.
        when "EMPCODE" then
        do:
           open query qry-emp
                    FOR EACH payemf  WHERE payemf .Paysys = wsSys  
                        AND payemf .empcode >= INT(wsSearch:SCREEN-VALUE) USE-INDEX empcode no-lock.
                           

                    END.
        END.
    RETURN.
END.
