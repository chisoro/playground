/* Program.................glbCAP01.p
   Notes:................. Budget Capture by Programme
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.

DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF NEW SHARED VAR varUser LIKE simusr.usercode INITIAL 1.
DEF VAR wsYear LIKE glPbal.YEAR.
DEF VAR wsAcct LIKE glPbal.acct.
DEF VAR wsProj LIKE glPbal.proj.
DEF VAR wsFund LIKE glPbal.fund.
DEF VAR wsDept LIKE glPbal.dept.
DEF VAR wsAmt  LIKE glPbal.bugtotal FORM "zzz,zzz,zzz,zz9.99-".
DEF VAR oldAmt LIKE wsAmt.
DEF VAR wsDes LIKE glmf.DESCRIPTION.
DEF VAR X AS INT.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "SEARCH".

DEF BUTTON btnProj LABEL "PROGRAMME".
DEF BUTTON btnFund LABEL "SUB-PROGRAMME".
DEF BUTTON btnDept LABEL "COST CENTER".
DEF BUTTON btnLedger LABEL "LEDGER".
DEF BUTTON btnClose  LABEL "CLOSE".
DEF BUTTON btnOk     LABEL "OK".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 109 by 10.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 109 BY 2.15.


DEF    QUERY qry-Input FOR glPBal SCROLLING.
DEF BROWSE brw-Input QUERY qry-Input
     DISPLAY GLPBal.Proj COLUMN-LABEL "PROGRAMME"
             GLPBal.FUND COLUMN-LABEL "   SUB!PROGRAMME"
             GLPBal.acct COLUMN-LABEL "LEDGER"
             wsDes       COLUMN-LABEL "DESCRIPTION" WIDTH 49.5
             GLPBal.bugtotal COLUMN-LABEL "AMOUNT" FORM "zzz,zzz,zzz,zz9.99-"
    WITH 10 DOWN SEPARATORS.

DEF    QUERY qry-PickProj FOR glProj SCROLLING.
DEF BROWSE brw-PickProj QUERY qry-PickProj
    DISPLAY GLProj.Proj GLProj.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-Fund FOR GlFund SCROLLING.
DEF BROWSE brw-Fund  QUERY qry-Fund 
    DISPLAY GLFUND.FUND COLUMN-LABEL "FUND  !SEGMENT" GLFUND.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 50 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-ledger FOR glmf SCROLLING.
DEF BROWSE brw-ledger QUERY qry-ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS. 


FORM
    wsYear  COLON 20 LABEL "YEAR" SKIP(0.5)
    wsProj  COLON 20 LABEL "PROGRAM" SKIP(0.5)
    wsFund  COLON 20 LABEL "SUB-PROGRAM" SKIP(0.5)
    wsAcct  COLON 20 LABEL "LEDGER/ACCOUNT" SKIP(0.5)
    wsAmt   COLON 20 LABEL "AMOUNT"
    WITH SIDE-LABEL NO-BOX FRAME frm-Input.

DEFINE FRAME frmBudg 
     SKIP(1)
    wsYear COLON 30 LABEL "ESTIMATE YEAR" SKIP(0.5)
    btnProj                 COLON 14.5  NO-TAB-STOP
    wsProj              NO-LABEL
    SPACE(1) glProj.DESCRIPTION  VIEW-AS TEXT NO-LABEL NO-TAB-STOP SKIP(0.5)
    btnFund                 COLON 10  NO-TAB-STOP
    wsFund              NO-LABEL
    SPACE(1) glFund.DESCRIP  VIEW-AS TEXT NO-LABEL NO-TAB-STOP SKIP(0.5)
    btnLedger COLON 19 NO-LABEL NO-TAB-STOP
    wsAcct  NO-LABEL  
    glmf.DESCRIPTION NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
    wsAmt  COLON 30 LABEL "AMOUNT" SKIP(2)
    brw-Input  AT ROW 11.3 COL 3
    "Press F5 for New Programme"   AT ROW 21.1 COL 10 FGCOLOR 12
    "Press F6 for New Sub-Programme"   AT ROW 21.1 COL 70 FGCOLOR 12
    btnClose    AT ROW 21.1 COL 50
    rect-1     AT ROW 1.27 COL 3
    rect-2      AT ROW 20.5 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE SIZE 114 BY 23.5
    CENTERED TITLE "ESTIMATES DATA CAPTURE".

DEFINE FRAME frm-PickProj 
    brw-PickProj AT ROW 2 COL 5
    skip(0.5)
    btnOk colon 5
    btnClose colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "PROGRAMME SELECTION".

DEFINE FRAME frm-pickFund 
    brw-Fund AT ROW 2 COL 5
    skip(0.5)
    btnOk colon 5
    btnClose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "FUND/SEGMENT SELECTION".

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

ON CHOOSE OF btnProj IN FRAME frmBudg
DO:
  VIEW FRAME frm-pickProj.
  OPEN QUERY qry-PickProj FOR EACH glProj NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickProj.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-pickProj 
          OR close of THIS-PROCEDURE IN FRAME frm-pickProj
          OR CHOOSE OF btnOk IN FRAME frm-pickProj 
          OR 'enter':u OF brw-PickProj
          OR 'mouse-select-dblclick' OF brw-PickProj.
  CLOSE QUERY qry-PickProj.
  HIDE FRAME frm-pickProj.
  APPLY 'tab' TO btnProj.
  APPLY 'tab' TO wsProj.
  RETURN. 
END.

ON CHOOSE OF btnOk IN FRAME frm-pickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ wsProj glproj.DESCRIPTION  WITH FRAME frmBudg.
   RETURN.
END.

ON 'enter':U OF wsProj IN FRAME frmBudg
    OR 'tab':U OF wsProj IN FRAME frmBudg
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(wsProj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj OR INT(wsProj:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glProj.descrip WITH FRAME frmBudg.
        ASSIGN wsProj = INT(wsProj:SCREEN-VALUE).
    END.
    RETURN.
END.

ON CHOOSE OF btnFund IN FRAME frmBudg
DO:
  VIEW FRAME frm-PickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickFund.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-PickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-PickFund
          OR CHOOSE OF btnOk IN FRAME frm-PickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-PickFund.
  APPLY 'tab' TO btnFund.
   APPLY 'tab' TO wsFund.
  RETURN. 
END.

ON CHOOSE OF btnOk IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ wsFund glFund.DESCRIP  WITH FRAME frmBudg.
   RETURN.
END.

ON 'enter':U OF wsFund IN FRAME frmBudg
    OR 'tab':U OF wsFund IN FRAME frmBudg
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(wsFund:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glFund OR INT(wsFund:SCREEN-VALUE) = 0  THEN DO:
        MESSAGE "Invalid Segment entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.descrip WITH FRAME frmBudg.
        ASSIGN wsFund = INT(wsFund:SCREEN-VALUE).
        OPEN QUERY qry-Input 
        FOR EACH glPbal WHERE glPbal.YEAR = wsYear AND glPbal.Proj = wsProj 
                         AND glPbal.fund = wsFund NO-LOCK.
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
            FIND FIRST glPbal WHERE glPbal.YEAR = wsYear AND glPbal.Proj = wsProj 
                        AND glPbal.fund = wsFund AND glPbal.acct = wsAcct NO-ERROR.
            IF AVAILABLE glPbal THEN
                DISPLAY glPbal.bugTotal @ wsAmt WITH FRAME frmBudg.
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
    FIND FIRST glmf WHERE glmf.acct = glPbal.acct NO-ERROR.
    IF AVAILABLE glmf THEN 
        wsdes = glmf.DESCRIPTION.
    ELSE
        wsdes = "**** Invalid Ledger ****".
END.

ON 'f5':u ANYWHERE 
DO:
    IF FRAME-NAME = "frmBudg" THEN DO:
        RELEASE glPBal.
        ENABLE wsProj WITH FRAME frmBudg.
        ASSIGN wsProj:SCREEN-VALUE = " "
               wsFund:SCREEN-VALUE  = ""
               wsAcct:SCREEN-VALUE  = "".
        APPLY 'entry' TO wsProj.
        RETURN NO-APPLY.
    END.
END.

ON 'f6':u ANYWHERE 
DO:
    IF FRAME-NAME = "frmBudg" THEN DO:
        RELEASE glPBal.
        ENABLE wsFund WITH FRAME frmBudg.
        ASSIGN wsFund:SCREEN-VALUE  = ""
               wsAcct:SCREEN-VALUE  = "".
        APPLY 'entry' TO wsFund.
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
   FIND FIRST glPbal WHERE glPbal.YEAR = wsYear AND glPbal.Proj = wsProj 
                        AND glPbal.fund = wsFund AND glPbal.acct = wsAcct NO-ERROR.
    IF NOT AVAILABLE glPbal THEN DO:
        CREATE glPbal.
        ASSIGN glPbal.YEAR = wsYear
               glPbal.Proj = wsProj
               glPbal.Fund = wsFund
               glPbal.acct = wsAcct
               glPbal.bugTotal = wsAmt.
    END.
    ELSE IF AVAILABLE glPbal THEN DO:
        oldAmt = glPbal.bugTotal.
        glPbal.bugTotal = glPbal.bugTotal - OldAmt + wsAmt.
    END.
    DO X = 1 TO 11: /* default Budget distribution %age */
        glPbal.BudP[X] = 8.33.
    END.
    glPbal.BudP[12] = 8.37. 
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
    /* DISPLAY glPbal.Proj glPbal.fund glPbal.dept glPbal.acct bugTotal WITH BROWSE brw-Input. */ 
    OPEN QUERY qry-Input 
        FOR EACH glPbal WHERE glPbal.YEAR = wsYear AND glPbal.Proj = wsProj 
                         AND glPbal.fund = wsFund NO-LOCK.
END. /* eof DataIP */

 
