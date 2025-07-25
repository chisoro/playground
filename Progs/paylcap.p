session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paylcap.p
   Notes:...... Direct data capture  leave days
   Author:.................S. Mawire
*/
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "Search".
DEF VAR X AS INT.
DEF VAR wstrDate AS DATE.
DEF VAR wstoDate AS DATE.
DEF VAR wsAmt LIKE paymtf.curAmt.
DEF VAR wsLeave LIKE paymtf.curAmt.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsName AS CHAR FORM "x(70)".
DEF VAR wsEmp LIKE payemf.empcode.
DEF VAR wsitem LIKE payitem.itemcode.
DEF VAR wsfrm AS CHAR FORM "x(40)".
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR wsdesc LIKE payitem.descrip.
DEF SHARED VAR varUser LIKE simusr.usercode.

DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-Emp   LABEL "EMPLOYEE".

DEF RECT rec  SIZE 70 BY 10.
DEF RECT rec1 SIZE 70 BY 2.


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

DEF    QUERY qry-leave FOR Payleave SCROLLING.
DEF BROWSE brw-leave  QUERY qry-leave
        DISPLAY payleave.trDate COLUMN-LABEL "FROM" payleave.toDate COLUMN-LABEL "to"  
    payleave.amt COLUMN-LABEL "DAYS" WITH 5 DOWN SEPARATORS.


DEFINE FRAME frm-Paysys
    SKIP(0.5)
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
    wsSys     NO-LABEL AUTO-RETURN 
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(1)
    Paysys.CurPer  COLON 25 LABEL " Current Period   " VIEW-AS TEXT SKIP(1)
    Paysys.CurDate COLON 25 LABEL "Payroll Date   " VIEW-AS TEXT SKIP(2)
    btn-ok colon 10
    btn-close colon 50
    /*rec AT ROW 1.2     COL 2
    rec1 AT ROW 7.4   COL 2 */
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D SIZE 73 BY 10 
    TITLE "PAYROLL SELECTION" VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-Data
    SKIP(0.5)
    btn-emp   COLON 10 NO-LABEL NO-TAB-STOP
    Payemf.empcode   NO-LABEL AUTO-RETURN 
    wsName NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
    wsLeave          COLON 23 LABEL "Available days " NO-TAB-STOP SKIP(0.5)
    wsAmt        COLON 23 LABEL "Days taken " SKIP(0.5)
    wstrDate     COLON 23 LABEL "From" SKIP(0.5)
    wstoDate     COLON 23 LABEL "To " SKIP(0.5)
    brw-leave AT ROW 3 COL 42
    rec AT ROW 1.2     COL 2
    btn-ok AT ROW 11.8   COL 10 SPACE(40) btn-close
    rec1 AT ROW 11.4   COL 2
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D SIZE 73 BY 14
    TITLE "LEAVE DAYS TAKEN CAPTURE" VIEW-AS DIALOG-BOX.

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
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-Paysys.
   ASSIGN wsSys = INT(wsSys:SCREEN-VALUE).
   APPLY 'tab' TO wsSys.
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

ON CHOOSE OF btn-ok IN FRAME frm-data
DO:
    CREATE payleave.
    ASSIGN payleave.empcode = payemf.empcode
           payleave.amt      = wsAmt
           payleave.toDate   = DATE(wstoDate:SCREEN-VALUE)
           payleave.trDate   = wstrDate
           paymtf.inpAmt     = paymtf.inpAmt + wsAmt.
    CLEAR FRAME frm-data.
   APPLY 'entry' TO Payemf.empcode IN FRAME frm-data.
   RETURN NO-APPLY.
END.

/* Triggers for the button empcode */
ON CHOOSE OF btn-emp IN FRAME frm-data
DO:
  wsfrm = FRAME-NAME.
  VIEW FRAME frm-emp.
  OPEN QUERY qry-emp FOR EACH payemf WHERE (payemf.estatus = 1 OR payemf.estatus = 2) AND payemf.Paysys = wsSys NO-LOCK.
  ENABLE ALL WITH FRAME frm-emp.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-emp 
          OR close of THIS-PROCEDURE IN FRAME frm-emp
          OR CHOOSE OF btn-ok IN FRAME frm-emp 
          OR 'enter':u OF brw-emp
          OR 'mouse-select-dblclick' OF brw-emp.
  CLOSE QUERY qry-emp.
  HIDE FRAME frm-emp.
  APPLY 'tab' TO SELF IN FRAME frm-data.
  APPLY 'tab' TO payemf.empcode IN FRAME frm-data.
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
   ELSE IF wsFrm = "frm-data" THEN
      DISPLAY wsEmp @ payemf.empcode wsName WITH FRAME frm-data.
   APPLY 'tab' TO payemf.empcode IN FRAME frm-data.
END.

ON 'enter':u OF payemf.empcode IN FRAME frm-data
    OR 'tab':u OF payemf.empcode IN FRAME frm-data
DO: 
   wsEmp = DEC(payemf.empcode:SCREEN-VALUE IN FRAME frm-data).
   IF wsEmp = 0 THEN DO:
       MESSAGE "No employee Selected..." VIEW-AS ALERT-BOX.
       APPLY 'close' TO THIS-PROCEDURE.
   END.
   ELSE DO:
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
           DISPLAY wsEmp @ payemf.empcode wsName WITH FRAME frm-data.
           FIND FIRST paymtf WHERE paymtf.empcode = payemf.empcode AND paymtf.itemcode = Payctr.Itemcode NO-ERROR.
           IF AVAILABLE paymtf THEN DO:
               wsleave = Paymtf.YtdAmt + Paymtf.InpAmt.
               DISPLAY  wsLeave WITH FRAME frm-data.
               OPEN QUERY qry-leave FOR EACH payleave WHERE payleave.empcode = payemf.empcode.
           END.
           IF NOT AVAILABLE paymtf THEN DO:
               CREATE paymtf.
               ASSIGN paymtf.empcode = payemf.empcode
                      paymtf.itemcode = payctr.itemcode
                      wsleave = 0.
           END.
       END.
   END.
END.

ON 'enter':U OF wsAmt IN FRAME frm-data
    OR 'leave':U OF wsAmt IN FRAME frm-data
DO:
    ASSIGN wsAmt.
    IF wsAmt > wsLeave THEN DO:
        MESSAGE "You cannot take more than available leave days" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
     wsAmt = (wsAmt * -1).
    RETURN.
END.
ON 'enter':U OF wstrDate IN FRAME frm-data
    OR 'leave':U OF wstrDate IN FRAME frm-data
DO:
    ASSIGN wstrDate.
    FIND FIRST payleave WHERE payleave.empcode = payemf.empcode AND payleave.trDate = wstrDate NO-LOCK NO-ERROR.
    IF AVAILABLE payleave THEN DO:
        MESSAGE "This leave record has been captured before" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

on 'start-search':u of BROWSE brw-emp
    run Search.ip.

/********** MAIN LOGIC **********/
browse brw-emp:allow-column-searching = true.
FIND FIRST payctr NO-LOCK NO-ERROR.
VIEW FRAME frm-Paysys.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
RETURN.

PROCEDURE Datacap.ip:
    HIDE FRAME frm-Paysys.
    VIEW FRAME frm-Data.
    wsleave:SCREEN-VALUE = "0".
    FIND FIRST payitem WHERE payitem.itemcode = Payctr.Itemcode NO-ERROR.
    IF NOT AVAILABLE payitem THEN DO:
        MESSAGE "Leave day item not set...." VIEW-AS ALERT-BOX.
        LEAVE.
    END.
    ENABLE ALL WITH FRAME frm-data.
       WAIT-FOR CHOOSE OF btn-close OR CLOSE OF THIS-PROCEDURE IN FRAME frm-data.
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


