/* Program.................glbCAP02.p
   Notes:................. Budget Capture by Fund/Segment
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.

DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF NEW SHARED VAR varUser LIKE simusr.usercode INITIAL 1.
DEF VAR wsYear LIKE glFbal.YEAR.
DEF VAR wsAcct LIKE glFbal.acct.
DEF VAR wsFund LIKE glFbal.Fund.
DEF VAR wsdept LIKE glFbal.dept.
DEF VAR wsAmt  LIKE glFbal.bugtotal FORM "zzz,zzz,zzz,zz9.99-".
DEF VAR oldAmt LIKE wsAmt.
DEF VAR wsDes LIKE glmf.DESCRIPTION.
DEF VAR X AS INT.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "SEARCH".

DEF BUTTON btnFund LABEL "FUND/SEGMENT".
DEF BUTTON btnDept LABEL "DEPARTMENT".
DEF BUTTON btnLedger LABEL "LEDGER".
DEF BUTTON btnClose  LABEL "CLOSE".
DEF BUTTON btnOk     LABEL "OK".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 109 by 10.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 109 BY 2.15.


DEF    QUERY qry-Input FOR glFbal SCROLLING.
DEF BROWSE brw-Input QUERY qry-Input
     DISPLAY glFbal.Fund COLUMN-LABEL "FUND ! SEGMENT"
             glFbal.Dept COLUMN-LABEL "DEPARTMENT"
             glFbal.acct COLUMN-LABEL "LEDGER"
             wsDes       COLUMN-LABEL "DESCRIPTION" WIDTH 51.5
             glFbal.bugtotal COLUMN-LABEL "AMOUNT" FORM "zzz,zzz,zzz,zz9.99-"
    WITH 10 DOWN SEPARATORS.

DEF    QUERY qry-PickFund FOR glFund SCROLLING.
DEF BROWSE brw-PickFund QUERY qry-PickFund
    DISPLAY GLFund.Fund GLFund.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-dept FOR Gldept SCROLLING.
DEF BROWSE brw-dept  QUERY qry-dept 
    DISPLAY GLdept.dept COLUMN-LABEL "DEPT !SEGMENT" GLdept.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 50 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-pickDept FOR glDept SCROLLING.
DEF BROWSE brw-pickDept QUERY qry-pickDept
    DISPLAY GLDept.Dept GLDept.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-ledger FOR glmf SCROLLING.
DEF BROWSE brw-ledger QUERY qry-ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.


FORM
    wsYear  COLON 20 LABEL "YEAR" SKIP(0.5)
    wsFund  COLON 20 LABEL "FUND/SEGMENT" SKIP(0.5)
    wsdept  COLON 20 LABEL "DEPARTMENT" SKIP(0.5)
    wsAcct  COLON 20 LABEL "LEDGER/ACCOUNT" SKIP(0.5)
    wsAmt   COLON 20 LABEL "AMOUNT"
    WITH SIDE-LABEL NO-BOX FRAME frm-Input.

DEFINE FRAME frmBudg 
     SKIP(1)
    wsYear COLON 30 LABEL "ESTIMATE YEAR" SKIP(0.5)
    btnFund                 COLON 11  NO-TAB-STOP
    wsFund              NO-LABEL
    SPACE(1) glFund.descrip  VIEW-AS TEXT NO-LABEL NO-TAB-STOP SKIP(0.5)
    btndept                 COLON 13  NO-TAB-STOP
    wsdept              NO-LABEL
    SPACE(1) gldept.DESCRIP  VIEW-AS TEXT NO-LABEL NO-TAB-STOP SKIP(0.5)
    btnLedger COLON 19 NO-LABEL NO-TAB-STOP
    wsAcct  NO-LABEL  
    glmf.DESCRIPTION NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
    wsAmt  COLON 30 LABEL "AMOUNT" SKIP(2)
    brw-Input  AT ROW 11.3 COL 3
    "Press F5 for New Fund"   AT ROW 21.1 COL 10 FGCOLOR 12
    "Press F6 for New Department"   AT ROW 21.1 COL 70 FGCOLOR 12
    btnClose    AT ROW 21.1 COL 50
    rect-1     AT ROW 1.27 COL 3
    rect-2      AT ROW 20.5 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE SIZE 114 BY 23.5
    CENTERED TITLE "ESTIMATES DATA CAPTURE".

DEFINE FRAME frm-PickFund 
    brw-PickFund AT ROW 2 COL 5
    skip(0.5)
    btnOk colon 5
    btnClose colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "FUND/SEGMENT SELECTION".

DEFINE FRAME frm-pickdept 
    brw-dept AT ROW 2 COL 5
    skip(0.5)
    btnOk colon 5
    btnClose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "DEPARTMENT SELECTION".

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.5)
    btnOk colon 5
    btnClose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "LEDGER SELECTION".

{lsearch.i}

ON 'leave':U OF wsYear IN FRAME frmBudg 
DO:
    IF DEC(wsYear:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Invalid year entered.....please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ASSIGN wsYear = DEC(wsYear:SCREEN-VALUE).
END.

ON CHOOSE OF btnFund IN FRAME frmBudg
DO:
  VIEW FRAME frm-pickFund.
  OPEN QUERY qry-PickFund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickFund.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-pickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-pickFund
          OR CHOOSE OF btnOk IN FRAME frm-pickFund 
          OR 'enter':u OF brw-PickFund
          OR 'mouse-select-dblclick' OF brw-PickFund.
  CLOSE QUERY qry-PickFund.
  HIDE FRAME frm-pickFund.
  APPLY 'tab' TO btnFund.
  APPLY 'tab' TO wsFund.
  RETURN. 
END.

ON CHOOSE OF btnOk IN FRAME frm-pickFund 
    OR 'enter':u OF brw-PickFund
    OR 'mouse-select-dblclick' OF brw-PickFund
DO: 
   GET CURRENT qry-PickFund EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ wsFund glFund.descrip  WITH FRAME frmBudg.
   RETURN.
END.

ON 'enter':U OF wsFund IN FRAME frmBudg
    OR 'tab':U OF wsFund IN FRAME frmBudg
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(wsFund:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glFund OR INT(wsFund:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Invalid Fundect entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.descrip WITH FRAME frmBudg.
        ASSIGN wsFund = INT(wsFund:SCREEN-VALUE).
    END.
    RETURN.
END.

ON CHOOSE OF btndept IN FRAME frmBudg
DO:
  VIEW FRAME frm-Pickdept.
  OPEN QUERY qry-dept FOR EACH gldept NO-LOCK.
  ENABLE ALL WITH FRAME frm-Pickdept.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Pickdept 
          OR close of THIS-PROCEDURE IN FRAME frm-Pickdept
          OR CHOOSE OF btnOk IN FRAME frm-Pickdept 
          OR 'enter':u OF brw-dept
          OR 'mouse-select-dblclick' OF brw-dept.
  CLOSE QUERY qry-dept.
  HIDE FRAME frm-Pickdept.
  APPLY 'tab' TO btndept.
   APPLY 'tab' TO wsdept.
  RETURN. 
END.

ON CHOOSE OF btnOk IN FRAME frm-Pickdept 
    OR 'enter':u OF brw-dept
    OR 'mouse-select-dblclick' OF brw-dept
DO: 
   GET CURRENT qry-dept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY gldept.dept @ wsdept gldept.DESCRIP  WITH FRAME frmBudg.
   RETURN.
END.

ON 'enter':U OF wsdept IN FRAME frmBudg
    OR 'tab':U OF wsdept IN FRAME frmBudg
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(wsdept:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE gldept OR INT(wsdept:SCREEN-VALUE) = 0  THEN DO:
        MESSAGE "Invalid Segment entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY gldept.descrip WITH FRAME frmBudg.
        ASSIGN wsdept = INT(wsdept:SCREEN-VALUE).
        OPEN QUERY qry-Input 
        FOR EACH glFbal WHERE glFbal.YEAR = wsYear AND glFbal.Fund = wsFund 
                         AND glFbal.dept = wsdept NO-LOCK.
    END.
    RETURN.
END.

ON CHOOSE OF btnLedger IN FRAME frmBudg
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btnOk IN FRAME frm-pick 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnLedger.
  APPLY 'tab' TO wsAcct.
  RETURN. 
END. 

ON CHOOSE OF btnOk IN FRAME frm-pick 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger NO-WAIT.
   DISPLAY glmf.acct @  wsAcct glmf.DESCRIPTION WITH FRAME frmBudg.
   APPLY 'tab' TO  wsAcct IN FRAME frmBudg.
END.

ON 'tab' of  wsAcct IN FRAME frmBudg
    OR 'enter' OF  wsAcct IN FRAME frmBudg
DO:
    IF  DEC( wsAcct:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Invalid ledger entered" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.Acct = DEC(wsAcct:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL glmf THEN DO:
            MESSAGE "Invalid Ledger entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY glmf.Acct @  wsAcct glmf.DESCRIPTION WITH FRAME frmBudg.
            ASSIGN wsAcct = DEC(wsAcct:SCREEN-VALUE).
            FIND FIRST glFbal WHERE glFbal.YEAR = INT(wsYear:SCREEN-VALUE)
                               AND glFbal.Fund = INT(wsFund:SCREEN-VALUE) 
                               AND glFbal.dept = INT(wsdept:SCREEN-VALUE) AND glFbal.acct = DEC(wsAcct:SCREEN-VALUE) NO-ERROR.
            IF AVAILABLE glFbal THEN
                DISPLAY glFbal.bugTotal @ wsAmt WITH FRAME frmBudg.
            
        END.
    END.
END.

ON 'tab':U OF wsamt IN FRAME frmBudg
    OR 'enter':U OF wsamt IN FRAME frmBudg
DO:
    ASSIGN wsAmt = DEC(wsAmt:SCREEN-VALUE).
    RUN DataIp.
    ASSIGN wsAcct:SCREEN-VALUE  = ""
           wsAmt:SCREEN-VALUE  = "".
    APPLY 'entry' TO wsAcct.
    RETURN NO-APPLY.
END.
ON row-display OF brw-Input DO:
    FIND FIRST glmf WHERE glmf.acct = glFbal.acct NO-ERROR.
    IF AVAILABLE glmf THEN 
        wsdes = glmf.DESCRIPTION.
    ELSE
        wsdes = "**** Invalid Ledger ****".
END.

ON 'f5':u ANYWHERE 
DO:
    IF FRAME-NAME = "frmBudg" THEN DO:
        RELEASE glFbal.
        ENABLE wsFund WITH FRAME frmBudg.
        ASSIGN wsFund:SCREEN-VALUE = " "
               wsdept:SCREEN-VALUE  = ""
               /*wsDept:SCREEN-VALUE = " " */
               wsAcct:SCREEN-VALUE  = "".
        APPLY 'entry' TO wsFund.
        RETURN NO-APPLY.
    END.
END.

ON 'f6':u ANYWHERE 
DO:
    IF FRAME-NAME = "frmBudg" THEN DO:
        RELEASE glFbal.
        ENABLE wsdept WITH FRAME frmBudg.
        ASSIGN wsdept:SCREEN-VALUE  = ""
               wsAcct:SCREEN-VALUE  = "".
        APPLY 'entry' TO wsdept.
        RETURN NO-APPLY.
    END.
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
CLEAR FRAME frmBudg.
ENABLE ALL WITH FRAME frmBudg.
wsYear:SCREEN-VALUE = STRING(YEAR(TODAY)).
APPLY 'entry' TO wsYear IN FRAME frmBudg.
WAIT-FOR CHOOSE OF btnClose OR CLOSE of THIS-PROCEDURE IN FRAME frmBudg.
HIDE FRAME frmBudg.

PROCEDURE DataIP:
    oldAmt = 0.
   FIND FIRST glFbal WHERE glFbal.YEAR = wsYear AND glFbal.Fund = wsFund 
                        AND glFbal.dept = wsdept AND glFbal.acct = wsAcct NO-ERROR.
    IF NOT AVAILABLE glFbal THEN DO:
        CREATE glFbal.
        ASSIGN glFbal.YEAR = wsYear
               glFbal.Fund = wsFund
               glFbal.dept = wsdept
               glFbal.acct = wsAcct
               glFbal.bugTotal = wsAmt.
    END.
    ELSE IF AVAILABLE glFbal THEN DO:
        oldAmt = glFbal.bugTotal.
        glFbal.bugTotal = glFbal.bugTotal - OldAmt + wsAmt.
    END.
    DO X = 1 TO 11: /* default Budget distribution %age */
        glFbal.BudP[X] = 8.33.
    END.
    glFbal.BudP[12] = 8.37. 
   /* Update Ledger Budget */
    FIND FIRST glbal WHERE glbal.YEAR = wsYear AND glbal.acct = wsAcct NO-ERROR.
    IF NOT AVAILABLE glbal THEN DO:
        CREATE glbal.
        ASSIGN glbal.YEAR = wsYear
               glbal.acct = wsAcct
               glbal.bugTotal = wsAmt.
    END.
    ELSE IF AVAILABLE glbal THEN DO:
        glbal.bugTotal = glbal.bugTotal - OldAmt + wsAmt.
    END.
    DO X = 1 TO 11: /* default Budget distribution %age */
        glbal.BudP[X] = 8.33.
    END.
    glbal.BudP[12] = 8.37. 
    /* DISPLAY glFbal.Fund glFbal.dept glFbal.dept glFbal.acct bugTotal WITH BROWSE brw-Input. */ 
    OPEN QUERY qry-Input 
        FOR EACH glFbal WHERE glFbal.YEAR = wsYear AND glFbal.Fund = wsFund 
                         AND glFbal.dept = wsdept NO-LOCK.
END. /* eof DataIP */
