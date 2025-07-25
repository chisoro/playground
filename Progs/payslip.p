session:DATA-ENTRY-RETURN = TRUE.
/* Program.................payslip1.p
   Notes:...... Employee payslip - Pre-printed stationary
   Author:.................S. Mawire
*/
DEF VAR w-orientation AS CHAR      INITIAL "landscape" NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""          NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR st-emp  LIKE payemf.empcode.
DEF VAR end-emp LIKE payemf.empcode.
DEF VAR wsAmt1  AS DEC EXTENT 12 FORM "zzzzzz9.99-".
DEF VAR wsAmt2  LIKE wsAmt1.
DEF VAR wsAmt3  LIKE wsAmt1.
DEF VAR wsNet   AS DEC FORM "zzzzzz9.99-".
DEF VAR wsGross LIKE wsNet.
DEF VAR wsDed   LIKE wsNet.
DEF VAR wsDes1  AS CHAR FORM "x(15)" EXTENT 12.
DEF VAR wsDes2  LIKE wsDes1.
DEF VAR wsDes3  LIKE wsDes1.
DEF VAR wsName  AS CHAR FORM "x(40)".
DEF VAR wsName1  AS CHAR FORM "x(60)".
DEF VAR wsLeav  AS DEC FORM "zz9.99-".
DEF VAR wsSex   AS CHAR FORM "x(6)".
DEF VAR wsBCode AS CHAR FORM "x(6)".
DEF VAR wsAcc   AS CHAR   FORM "x(20)".
DEF VAR wsMethod AS CHAR FORM "x(20)".
DEF VAR wsBank  LIKE paybnk.BankName.
DEF VAR wsDate  AS DATE INITIAL TODAY.
DEF VAR wsleave AS DEC.
DEF VAR i       AS INT.
DEF VAR j       AS INT.
DEF VAR k       AS INT.
DEF VAR X       AS INT.
DEF VAR wsPer   AS CHAR FORM "x(16)".
DEF VAR wsMonth AS CHAR INITIAL "JANUARY, FEBRUARY, MARCH, APRIL, MAY, JUNE, JULY, AUGUST, SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER".
DEF STREAM a.

DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".

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

DEFINE FRAME frm-Paysys
    SKIP(1.5)
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
    wsSys              NO-LABEL AUTO-RETURN 
    Paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
    wsDate    COLON 26 LABEL "Pay Date" SKIP(0.5)
    st-emp    COLON 26 LABEL "From Employee" wsName NO-LABEL VIEW-AS TEXT SKIP(0.5)
    end-emp   COLON 26 LABEL "To Employee" wsName1 NO-LABEL VIEW-AS TEXT SKIP(2.5)
    btn-ok colon 20
    btn-close colon 60
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 13
    TITLE "PAYROLL SELECTION" VIEW-AS DIALOG-BOX.

FORM  SKIP(2)
      simctr.CONAME    AT 30
      SKIP(1)
      "MONTHLY SALARY PAYSLIP" AT  82 SKIP(1)
      "EMPLOYEE DETAILS"  AT 8 
      wsPer              AT 82 NO-LABEL SKIP(1)
      "EMPLOYEE NAME  :" AT 8 wsName NO-LABEL
       "EMPLOYEE ID:"     AT 72 payemf.IDNo NO-LABEL 
       "EMPLOYEE NUMBER:" AT 8 payemf.empcode   NO-LABEL 
       "DEPARTMENT :"    AT 72 paydept.descrip  NO-LABEL 
      "EMPLOYMENT DATE:" AT 8  payemf.startdat NO-LABEL 
      "LEAVE DAYS :"     AT 72 wsleave  NO-LABEL
      "JOB TITLE      :" AT 8  payemf.Designation NO-LABEL 
      "GRADE          :" AT 8  payemf.scale NO-LABEL 
      SKIP(2)
          WITH WIDTH 132
               NO-BOX
               NO-LABELS
               FRAME head STREAM-IO.

FORM  wsDes1[1]       AT 8
      /*wshrs[1]          at 24
      wsrate[1]           at 33 */
      wsAmt1[1]       AT 42
      wsDes2[1]       AT 55
      wsAmt2[1]       AT 71
      wsDes3[1]       AT 84
      wsAmt3[1]       AT 100
          WITH WIDTH 132
              FONT 6
               NO-BOX
               NO-LABELS
               DOWN
               FRAME detail STREAM-IO.

FORM  "NET SALARY    :"   AT 8 wsNet    NO-LABEL SKIP(1)            
      "PAYMENT METHOD:"   AT 8 wsmethod NO-LABEL 
      "PAY DATE      :"   AT 82 wsDate  NO-LABEL 
      "BANK          :"   AT 8 wsBank   NO-LABEL 
      "ACCOUNT NO.   :"   AT 8 wsAcc    NO-LABEL 
      skip(3)
           with width 132
           no-box
           no-labels
           frame footer stream-io.

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
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip Paysys.CurDate @ wsDate WITH FRAME frm-Paysys.
   RETURN. 
END.

ON  'enter':U OF st-emp IN FRAME frm-Paysys
    OR 'tab':U OF st-emp IN FRAME frm-Paysys
DO:
    FIND FIRST payemf WHERE payemf.empcode = INT(st-emp:SCREEN-VALUE)
                        AND payemf.Paysys = INT(wsSys:SCREEN-VALUE)  NO-LOCK NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        wsName = payemf.SName + " " + payemf.FName.
        DISPLAY wsName WITH FRAME frm-paysys.
        APPLY 'tab' TO SELF.
    END.
    ELSE DO:
         MESSAGE "Invalid Employee number...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    RETURN.
END.

ON  'enter':U OF end-emp IN FRAME frm-Paysys
    OR 'tab':U OF end-emp IN FRAME frm-Paysys
DO:
    FIND FIRST payemf WHERE payemf.empcode = INT(end-emp:SCREEN-VALUE)
                        AND payemf.Paysys = INT(wsSys:SCREEN-VALUE)  NO-LOCK NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        wsName1 = payemf.SName + " " + payemf.FName.
        DISPLAY wsName1 WITH FRAME frm-paysys.
        APPLY 'tab' TO SELF.
    END.
    ELSE DO:
         /*MESSAGE "Invalid Employee number...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.*/
        APPLY 'tab' TO SELF.
    END.
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
            DISPLAY Paysys.Descrip Paysys.CurDate @ wsDate WITH FRAM frm-Paysys.
            FIND FIRST payemf WHERE payemf.paysys = wsSys AND payemf.leaver = NO NO-ERROR.
            DISPLAY payemf.empcode @ st-emp WITH FRAME frm-Paysys.
            APPLY 'tab' TO SELF.
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
          wsDate = DATE(wsDate:SCREEN-VALUE)
          st-emp = INT(st-emp:SCREEN-VALUE IN FRAME frm-Paysys)
          end-emp = INT(end-emp:SCREEN-VALUE IN FRAME frm-Paysys)
          wsPer = STRING(ENTRY(MONTH(wsDate),wsMonth)) + "  " + STRING(YEAR(wsDate)).
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST Payctr NO-LOCK NO-ERROR.
VIEW FRAME frm-Paysys IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.
RETURN.

PROCEDURE Report.ip:
FOR EACH payemf WHERE  payemf.paysys = wsSys AND payemf.empcode >= st-emp AND payemf.empcode <= end-emp 
                  AND payemf.Leaver = NO:
    ASSIGN i      = 1
           j      = 1
           k      = 1
           wsNet  = 0
           wsAmt1 = 0
           wsAmt2 = 0
           wsAmt3 = 0
           wsGross = 0
           wsDed   = 0
           wsDes1 = ""
           wsDes2 = ""
           wsDes3 = ""
           wsName = payemf.FName + " " + payemf.SName
           wsSex  = "MALE" WHEN payemf.sex = "M"
           wsSex  = "FEMALE" WHEN payemf.sex = "F"
           wsBcode = ""
           wsAcc = payemf.Account.
    IF wsAcc = "" THEN
        ASSIGN wsBank = ""
               wsMethod = "CASH".
    ELSE DO:
        FIND FIRST paybnk WHERE paybnk.bank = payemf.Bank NO-LOCK NO-ERROR.
        IF AVAILABLE paybnk THEN
            ASSIGN wsBank = paybnk.BankName
                    wsMethod = "BANK TRANSFER".
        ELSE
            ASSIGN wsBank = "INVALID BANK"
                    wsMethod = "CASH".
    END.
    FIND FIRST paymtf WHERE paymtf.empcode = payemf.empcode AND paymtf.itemcode = Payctr.Itemcode NO-ERROR.
    IF AVAILABLE paymtf THEN
        wsleave = Paymtf.YtdAmt.
    ELSE wsleave = 0.
    FIND FIRST paydept WHERE Paydept.Dept = payemf.Dept NO-LOCK NO-ERROR.
    FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode AND paymtf.CurAmt <> 0:
        FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-ERROR.
        IF Payitem.cat = 1 THEN /*Income */
            ASSIGN wsDes1[i] = Payitem.Descrip
                   wsAmt1[i] = paymtf.curAmt
                   wsNet = wsNet + paymtf.curAmt
                   wsGross = wsGross + paymtf.curAmt
                   i = i + 1.
        ELSE IF Payitem.cat = 2 THEN /*Deduction */
                 ASSIGN wsDes2[j] = Payitem.Descrip
                         wsAmt2[j] = paymtf.curAmt
                         wsNet = wsNet - paymtf.curAmt
                         wsDed = wsDed + paymtf.curAmt
                         j = j + 1.
        ELSE IF Payitem.cat = 3 THEN /*Accurals */
                 ASSIGN wsDes3[k] = Payitem.Descrip
                        wsAmt3[k] = paymtf.curAmt
                        k = k + 1.
        ELSE IF Payitem.cat = 4 THEN /*Benefits */
            ASSIGN wsDes1[i] = "* " + Payitem.Descrip
                   wsAmt1[i] = paymtf.curAmt
                   wsGross = wsGross + paymtf.curAmt
                   i = i + 1.
    END.
    DISPLAY STREAM a simctr.CONAME  payemf.IDNo payemf.empcode wsName wsPer payemf.Designation payemf.scale
                   payemf.empcode payemf.startdate Paydept.Descrip wsleave WITH FRAME head.
    DOWN STREAM a WITH FRAME head.
    DO X = 1 TO 12:
        DISPLAY STREAM a wsDes1[X] @ wsDes1[1] wsDes2[X] @ wsDes2[1] wsDes3[X] @ wsDes3[1]
                wsAmt1[X] WHEN wsAmt1[X] <> 0 @ wsAmt1[1]
                wsAmt2[X] WHEN wsAmt2[X] <> 0 @ wsAmt2[1] 
                wsAmt3[X] WHEN wsAmt3[X] <> 0 @ wsAmt3[1]
            WITH FRAME detail.
        DOWN STREAM a WITH FRAME detail.
    END.
    DISPLAY STREAM a "GROSS PAY" @ wsDes1[1] wsGross @ wsAmt1[1] "DEDUCTIONS" @ wsDes2[1] wsDed @ wsAmt2[1] WITH FRAME detail.
    DOWN 2 STREAM a WITH FRAME detail.
    DISPLAY STREAM a wsNet wsMethod wsDate wsBank wsAcc  WITH FRAME footer.
    DOWN STREAM a WITH FRAME footer.
    PAGE STREAM a.
END.
RETURN.
END.
