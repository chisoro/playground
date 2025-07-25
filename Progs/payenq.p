/* Program.................payenq.p
   Notes:...... Payroll Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "Search".
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF SHARED VAR varUser LIKE simusr.usercode INITIAL 1.
DEF VAR Gross LIKE paymtf.CurAmt.
DEF VAR YtdGross LIKE paymtf.CurAmt.
DEF VAR Taxable LIKE paymtf.CurAmt.
DEF VAR YtdTaxable LIKE paymtf.CurAmt.
DEF VAR StdTaxable LIKE paymtf.CurAmt.
DEF VAR NetPay LIKE paymtf.CurAmt.
DEF VAR Deduct LIKE paymtf.CurAmt.
DEF VAR YtdDeduct LIKE paymtf.CurAmt.
DEF VAR Acrual LIKE paymtf.CurAmt.
DEF VAR TaxCredit LIKE paymtf.CurAmt.
DEF VAR YTaxable LIKE Paymtf.CurAmt.
DEF VAR YTax     LIKE Paymtf.CurAmt.
DEF VAR wsAmt    LIKE Paymtf.CurAmt.
DEF VAR curTax  LIKE  Paymtf.CurAmt.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsStatus LIKE payemf.empcode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsDate AS DATE.
DEF VAR wsName AS CHAR FORM "x(70)".
DEF VAR wsDesc AS CHAR FORM "x(40)".

DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".
DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-Emp   LABEL "EMPLOYEE".
DEF BUTTON btn-Next  LABEL "NEXT".
DEF BUTTON btn-Prev  LABEL "PREVIOUS".

DEF RECT rec  SIZE 70 BY 6.
DEF RECT rec1 SIZE 70 BY 2.
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 110 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 110 by 23.

/*DEF BUFFER bfrPaymtf FOR paymtf. */

DEF    QUERY qry-emp FOR Payemf SCROLLING.
DEF BROWSE brw-emp QUERY qry-emp
        DISPLAY payemf.empcode  payemf.SName WIDTH 40 payemf.FName 
        WIDTH 30 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-enq FOR paymtf SCROLLING.
DEF BROWSE brw-enq QUERY qry-enq
    DISPLAY Paymtf.Itemcode LABEL "ITEM" wsDesc LABEL "DESCRIPTION" 
    Paymtf.StdAmt LABEL "STD AMOUNT" Paymtf.InpAmt LABEL "INPUT" Paymtf.CurAmt LABEL "AMOUNT" 
            Paymtf.YtdAmt LABEL "YTD AMOUNT" Paymtf.ACTIVE  LABEL "ACTIVE" WITH NO-LABEL 20 DOWN SEPARATORS.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
        DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
        WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-picksys 
    brw-Sys AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".
DEFINE FRAME frm-Paysys
    SKIP(0.5)
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
    wsSys     NO-LABEL AUTO-RETURN 
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(1)
    Paysys.CurPer  COLON 25 LABEL " Current Period   " VIEW-AS TEXT SKIP(1)
    Paysys.CurDate COLON 25 LABEL "Payroll Date   " VIEW-AS TEXT SKIP(2)
    btn-ok colon 10
    btn-close colon 50
    rec AT ROW 1.2     COL 2
    rec1 AT ROW 7.4   COL 2
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D SIZE 73 BY 10 
    TITLE "PAYROLL SELECTION" VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-main
    SKIP(1.5)
    btn-Emp     COLON 5 AUTO-RETURN NO-TAB-STOP
    payemf.empcode NO-LABEL 
    wsName         NO-LABEL VIEW-AS TEXT SKIP(0.5)
    payemf.Designation COLON 30 LABEL "JOB" VIEW-AS TEXT SKIP(0.5)
    Paydept.Descrip    COLON 30 LABEL "DEPARTMENT" VIEW-AS TEXT SKIP(0.5)
    NetPay COLON 15 LABEL "NET PAY" VIEW-AS TEXT
    Taxable         LABEL "TAXABLE" VIEW-AS TEXT
    TaxCredit       LABEL "CREDIT" VIEW-AS TEXT
    Gross           LABEL "GROSS"  VIEW-AS TEXT SKIP(0.5)
    brw-enq COLON 3 skip(1.5)
    btn-Next colon 10
    SPACE(30) btn-Prev
    space(30) btn-close 
    rect-2 AT ROW 1.4 COL 3 
    rect-1 AT ROW 24.5 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 117 BY 27.5
    TITLE "PAYROLL CALCULATION" VIEW-AS DIALOG-BOX.

DEFINE FRAME frm-pick 
    brw-emp AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".



DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.
FORM
    HEADER skip(4) "PAYROLL RUN REPORT FOR THE PERIOD:" AT 5
    TRIM(STRING(st-Per)) "Page:" AT 82 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 CENTERED NO-BOX WIDTH 132 FRAME frm-rpt.

/* Triggers for the Paysys frame */
ON CHOOSE OF btn-sys IN FRAME frm-Paysys
    OR CHOOSE OF btn-sys IN FRAME frm-Paysys
DO:
  VIEW FRAME frm-picksys.
  OPEN QUERY qry-sys FOR EACH paysys NO-LOCK.
  ENABLE ALL WITH FRAME frm-picksys.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-picksys 
          OR close of THIS-PROCEDURE IN FRAME frm-picksys
          OR CHOOSE OF btn-ok IN FRAME frm-picksys 
          OR 'enter':u OF brw-sys
          OR 'mouse-select-dblclick' OF brw-sys.
  CLOSE QUERY qry-sys.
  HIDE FRAME frm-picksys.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsSys.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-picksys 
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
       RUN Enquire.ip.
    APPLY 'close' TO THIS-PROCEDURE.
END.

ON CHOOSE OF btn-EMP IN FRAME frm-main
    OR CHOOSE OF btn-EMP IN FRAME frm-main
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-emp FOR EACH payemf WHERE payemf.estatus <> 4 AND payemf.paysys = wsSys NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-emp
          OR 'mouse-select-dblclick' OF brw-emp.
  CLOSE QUERY qry-emp.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO payemf.empcode IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-emp
    OR 'mouse-select-dblclick' OF brw-emp
DO: 
   GET CURRENT qry-emp NO-LOCK NO-WAIT.
   wsName = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
   FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-LOCK NO-ERROR.
   DISPLAY payemf.empcode wsName payemf.Designation Paydept.Descrip WITH FRAME frm-main.
   APPLY 'tab' TO SELF.
   RETURN. 
END.

ON  'enter':U OF payemf.empcode IN FRAME frm-main
    OR 'tab':U OF payemf.empcode  IN FRAME frm-main
DO:
    FIND FIRST payemf WHERE payemf.empcode  = INT(payemf.empcode :SCREEN-VALUE) AND payemf.paysys = wsSys NO-LOCK NO-ERROR.
    IF AVAILABLE payemf AND payemf.estatus = 4 THEN DO:
        MESSAGE "Employee has left...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
        wsName = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
        FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-LOCK NO-ERROR.
        DISPLAY payemf.empcode wsName payemf.Designation Paydept.Descrip WITH FRAME frm-main.
        PAUSE 0.
        VIEW FRAME frm-enq.
        ASSIGN GROSS = 0
               Taxable = 0
                NetPay = 0
               TaxCredit = 0.
        IF AVAILABLE payemf  THEN DO:
        FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode NO-LOCK:
            FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-LOCK NO-ERROR.
            CASE Payitem.cat:
         WHEN 1 THEN DO:
              ASSIGN Gross      = gross + Paymtf.CurAmt
                     YtdGross   = YtdGross + Paymtf.YtdAmt
                     Taxable    = Taxable + Paymtf.CurAmt    WHEN payitem.tax = 1
                     YtdTaxable = YtdTaxable + Paymtf.YtdAmt WHEN payitem.tax = 1
                     StdTaxable = stdTaxable + Paymtf.CurAmt WHEN (Payitem.Regular = YES AND payitem.tax = 1)
                     NetPay     = NetPay +  Paymtf.CurAmt.
         END.
         WHEN 2 THEN 
              ASSIGN Deduct     = Deduct + Paymtf.CurAmt
                     YtdDeduct  = YtdDeduct + Paymtf.YtdAmt
                     Taxable    = Taxable - Paymtf.CurAmt    WHEN payitem.tax = 2 
                     YtdTaxable = YtdTaxable - Paymtf.YtdAmt WHEN payitem.tax = 2
                     StdTaxable = stdTaxable - Paymtf.CurAmt WHEN (Payitem.Regular = YES AND payitem.tax = 2)
                     NetPay  = NetPay - Paymtf.CurAmt.
              WHEN 3 THEN
                   ASSIGN Acrual = acrual + Paymtf.CurAmt.
              WHEN 4 THEN DO:
                  ASSIGN Gross      = gross + Paymtf.CurAmt
                         YtdGross   = YtdGross + Paymtf.YtdAmt
                         Taxable    = Taxable + Paymtf.CurAmt    WHEN payitem.tax = 1
                         YtdTaxable = YtdTaxable + Paymtf.YtdAmt WHEN payitem.tax = 1
                         StdTaxable = stdTaxable + Paymtf.CurAmt WHEN (Payitem.Regular = YES AND payitem.tax = 1).
             END.
              OTHERWISE
                   ASSIGN TaxCredit = TaxCredit + Paymtf.CurAmt WHEN payitem.tax = 3
                          Taxable    = Taxable - Paymtf.CurAmt  WHEN payitem.tax = 2.
        END CASE.
        END.
        OPEN QUERY qry-enq FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode NO-LOCK.
        DISPLAY NetPay Taxable Gross TaxCredit WITH FRAME frm-main.
        APPLY 'entry' TO btn-next.
    END.
   ELSE DO:
        MESSAGE "Invalid Employee code...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON row-display OF brw-Enq DO:
    FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-LOCK NO-ERROR.
    wsDesc = Payitem.Descrip.
END.

ON CHOOSE OF btn-next IN FRAME frm-main
DO:
    
    FIND NEXT payemf WHERE payemf.estatus <> 4 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE  payemf THEN
        FIND LAST  payemf WHERE payemf.estatus <> 4 NO-LOCK NO-ERROR.
    payemf.empcode:SCREEN-VALUE = STRING( payemf.empcode).
    wsName = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
    FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-LOCK NO-ERROR.
    DISPLAY payemf.empcode wsName payemf.Designation Paydept.Descrip WITH FRAME frm-main.
    ASSIGN GROSS = 0
           Taxable = 0
           NetPay = 0
           TaxCredit = 0.
    OPEN QUERY qry-enq FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode NO-LOCK.
    DISPLAY NetPay Taxable Gross TaxCredit WITH FRAME frm-main.
    RETURN.
END.

ON CHOOSE OF btn-Prev IN FRAME frm-MAIN
DO:
    FIND PREV payemf WHERE payemf.estatus <> 4 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE  payemf THEN
        FIND FIRST payemf WHERE payemf.estatus <> 4 NO-LOCK NO-ERROR.
    payemf.empcode:SCREEN-VALUE = STRING( payemf.empcode).
    wsName = TRIM(payemf.SName) + " " + TRIM(payemf.FName).
    FIND FIRST paydept WHERE paydept.dept = payemf.dept NO-LOCK NO-ERROR.
    DISPLAY payemf.empcode wsName payemf.Designation Paydept.Descrip WITH FRAME frm-main.
    ASSIGN GROSS = 0
          Taxable = 0
          NetPay = 0
         TaxCredit = 0.
    OPEN QUERY qry-enq FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode NO-LOCK.
    DISPLAY NetPay Taxable Gross TaxCredit WITH FRAME frm-main.
    RETURN.
END.

on 'start-search':u of BROWSE brw-emp
    run Search.ip.

/********** MAIN LOGIC **********/
browse brw-emp:allow-column-searching = true.
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST payctr NO-LOCK NO-ERROR.
VIEW FRAME frm-Paysys.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
RETURN.

PROCEDURE Enquire.ip:
    HIDE FRAME frm-Paysys.
    ENABLE ALL WITH FRAME frm-main.
    VIEW FRAME frm-main.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
    HIDE FRAME frm-main.
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
