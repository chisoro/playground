/* Program.................cbjnl01.p
   Notes:................. Real-time journal capture
   Author:.................S. Mawire
   Edited:.................S. Chisoro
*/
&SCOPED-DEFINE wsMsg           ""
&SCOPED-DEFINE wsTitle           ""
&SCOPED-DEFINE tmpTable            CBTrans
&SCOPED-DEFINE skey                CBTrans.bank

SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
{varlibrary.i}
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsper    LIKE simctr.curper.
DEF VAR wsyear   AS INT.
DEF VAR wsmonth  AS INT.
DEF VAR wsBank   AS CHAR FORM "x(12)".
DEF VAR st-per   LIKE CBTrans.Accper.
DEF VAR wsAmt    LIKE cbkmf.bal.
DEF VAR wsTotal  LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsDr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsCr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsAmt.
DEF VAR wsTCr    LIKE wsAmt.
DEF VAR wsNar    LIKE cbkmf.descrip.
DEF VAR wsRef    LIKE cbtrans.ref.
DEF VAR wsDate   LIKE cbTrans.trDate.
DEF VAR wsFund   LIKE glmf.fund.
DEF VAR wsLedger LIKE cbkmf.ledger.
DEF VAR wsDesc   LIKE glmf.DESCRIPTION.
DEF VAR wsRate   LIKE tblForex.decRate.
DEF VAR wsCur    LIKE cbkmf.txtcur.
DEF VAR X        AS INT.
DEF VAR wsf      AS INT.
DEF VAR wsSource LIKE gltdf.SOURCE INITIAL "CB".
DEF VAR ws-batch AS INT.

DEF TEMP-TABLE tmpTrans LIKE CBTrans.

DEF BUTTON btnPrint    LABEL "COMMIT".
DEF BUTTON btnok       LABEL "OK".
DEF BUTTON btnClose    LABEL "Cancel".
DEF BUTTON btnTransT    LABEL "TRANSACTION TYPE".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 100 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 100 by 7.8.

DEF    QUERY qry-cbkmf FOR tmpTrans SCROLLING.
DEF BROWSE brw-cbkmf QUERY qry-cbkmf
    DISPLAY tmpTrans.Accper tmpTrans.trDate tmpTrans.Bank tmpTrans.Ref 
    tmpTrans.Descrip WIDTH 32 tmpTrans.Ledger tmpTrans.amount   
    WITH 10 DOWN SEPARATORS.

DEF    QUERY qry-trans FOR cbtype SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY cbtype.TranType cbtype.descrip cbtype.DrCr WITH 10 DOWN SEPARATORS.

DEF    QUERY qry-BankPick FOR cbkmf SCROLLING.
DEF BROWSE brw-BankPick QUERY qry-BankPick
     DISPLAY cbkmf.bank cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-BankPick 
     brw-BankPick AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Bank Selection".

DEFINE FRAME frm-trans 
     brw-trans AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Transaction Type Selection".

DEF FRAME frm-Pay
     btnBank AT ROW 1.5 COL 5  NO-TAB-STOP SPACE(2)
     wsBank NO-LABEL  AUTO-RETURN 
     cbkmf.descrip NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 by 1
     SKIP(0.5)
     btnTransT COLON 3 NO-LABEL NO-TAB-STOP cbtype.TranType NO-LABEL 
     cbtype.descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
     st-per LABEL "Period"   COLON 10 AUTO-RETURN SPACE(10)
     wsDate  LABEL "Date"  SPACE(20)
     wsRef    LABEL "Reference" SKIP(0.5)
     wsNar    LABEL "Narration" COLON 10 VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.5)
     wsAmt    LABEL "Amount" COLON 10 SKIP(0.5)
     "Accounting Cashbook and Ledger Allocations" COLON 15
     brw-cbkmf    AT ROW 10 COL 3
     btnPrint    AT ROW 19.3 COL 20 SPACE(40) btnClose
     rect-1      AT ROW 18.8 COL 3
     rect-2     AT ROW 1.27 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 105 BY 21.5 TITLE "ONLINE REAL-TIME JOURNAL CAPTURE".

 DEFINE FRAME frm-allocate 
     SKIP(1)
    wsTotal  COLON 80 LABEL "Amount to be allocated" VIEW-AS TEXT
    SKIP(1)
    btnLedger COLON 5 NO-LABEL NO-TAB-STOP
    bfr{&tmpTable}.Ledger  NO-LABEL  
    glmf.DESCRIPTION NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
    btnDept                 COLON 5  no-tab-stop
    bfr{&tmpTable}.Dept              NO-LABEL
    SPACE(1) glDept.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
    btnProj                 COLON 5  no-tab-stop
    bfr{&tmpTable}.Proj              NO-LABEL
    SPACE(1) glProj.DESCRIPTION  view-as text no-label NO-TAB-STOP skip(0.5)
    btnFund                 COLON 5  no-tab-stop
    bfr{&tmpTable}.Fund              NO-LABEL
    SPACE(1) glFund.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
    bfr{&tmpTable}.Descrip COLON 15 LABEL "Narration" VIEW-AS FILL-IN SIZE 60 BY 1 SKIP(0.5)
   /* btnBank          COLON 1 NO-LABEL NO-TAB-STOP
    bfr{&tmpTable}.Bank     NO-LABEL
    cbkmf.descrip   NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5) */
    bfr{&tmpTable}.amount  COLON 20 LABEL "Amount" SKIP(2)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "ALLOCATIONS".

/*DEFINE FRAME frm-cbPick 
     brw-cbPick AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Cashbook Selection".
*/
DEFINE FRAME frm-Pick 
     brw-Ledger AT ROW 1 COL 1.5 skip(0.5)
     btn-Ok COLON 8 SPACE (20) btn-close 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Ledger Selection".

FORM
    wsDate        LABEL "DATE " SPACE(4)
    st-Per        LABEL "PERIOD" 
    cbkmf.descrip LABEL "CASHBOOK" 
    wsNar         LABEL "NARRATION"  FORM "x(50)"
    wsAmt         LABEL "AMOUNT " 
    HEADER  "ONLINE JOURNAL CAPTURE AND UPDATE - REFERENCE#" wsRef  "Page: " AT 80 PAGE-NUMBER(a)
     SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

FORM
    cbkmf.descrip LABEL "CASHBOOK" 
    tmpTrans.Descrip FORM "x(50)" LABEL "NARRATION" 
    tmpTrans.Ledger LABEL "ALLOCATION" 
    tmpTrans.amount LABEL "AMOUNT" 
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt1.

FORM dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION" FORM "x(50)"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "ONLINE JOURNAL CAPTURE AND UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

ON CHOOSE OF btnBank IN FRAME frm-Pay
DO:
  VIEW FRAME frm-BankPick.
  OPEN QUERY qry-BankPick FOR EACH cbkmf WHERE cbkmf.bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-BankPick.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-BankPick 
          OR close of THIS-PROCEDURE IN FRAME frm-BankPick
          OR CHOOSE OF btnok IN FRAME frm-BankPick 
          OR 'enter':u OF brw-BankPick
          OR 'mouse-select-dblclick' OF brw-BankPick.
  CLOSE QUERY qry-BankPick.
  HIDE FRAME frm-BankPick.
  APPLY 'tab' TO btnBank.
  APPLY 'tab' TO wsBank.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-BankPick 
    OR 'enter':u OF brw-BankPick
    OR 'mouse-select-dblclick' OF brw-BankPick
DO: 
   GET CURRENT qry-BankPick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY cbkmf.Bank @ wsBank cbkmf.DESCRIP WITH FRAME frm-Pay.
   APPLY 'TAB' TO wsBank IN FRAME frm-Pay.
END.

ON 'tab' of wsBank IN FRAME frm-Pay
    OR 'enter' OF wsBank IN FRAME frm-Pay
DO:
    IF  INT(wsbank:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frm-Pay.
    ELSE DO:
        ASSIGN wsBank = wsBank:SCREEN-VALUE. 
        FIND FIRST cbkmf WHERE cbkmf.bank = int(wsBank:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Pay ALL.
            RETURN NO-APPLY.
        END.
        ELSE DO:
           DISPLAY cbkmf.Bank @ wsBank cbkmf.DESCRIP  WITH FRAME frm-Pay.
           ASSIGN wsCur = cbkmf.txtcur.
        END.
    END.
END.

ON CHOOSE OF btnTransT IN FRAME frm-Pay
DO:
  VIEW FRAME frm-Trans.
  OPEN QUERY qry-Trans FOR EACH cbtype NO-LOCK.
  ENABLE ALL WITH FRAME frm-Trans.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Trans 
          OR close of THIS-PROCEDURE IN FRAME frm-Trans
          OR CHOOSE OF btnok IN FRAME frm-Trans 
          OR 'enter':u OF brw-Trans
          OR 'mouse-select-dblclick' OF brw-Trans.
  CLOSE QUERY qry-Trans.
  HIDE FRAME frm-Trans.
  APPLY 'tab' TO btnTransT.
  APPLY 'tab' TO cbtype.TranType.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Trans 
    OR 'enter':u OF brw-Trans
    OR 'mouse-select-dblclick' OF brw-Trans
DO: 
   GET CURRENT qry-Trans EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY cbtype.TranType cbtype.descrip WITH FRAME frm-Pay.
   APPLY 'TAB' TO cbtype.TranType IN FRAME frm-Pay.
END.

ON 'tab' OF cbtype.TranType IN FRAME frm-Pay
    OR 'enter' OF cbtype.TranType IN FRAME frm-Pay
DO:
    IF  INT(cbtype.TranType:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frm-Pay.
    ELSE DO: 
        FIND FIRST cbtype WHERE cbtype.TranType = int(cbtype.TranType:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL cbtype THEN DO:
            MESSAGE "Invalid Transaction Type entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Pay ALL.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY cbtype.TranType cbtype.descrip WITH FRAME frm-Pay.
            ASSIGN wsF = 1  WHEN cbtype.DrCr = "D"
                   wsF = -1 WHEN cbtype.DrCr = "C".
        END.
    END.
END.

/*ON CHOOSE OF btnBank IN FRAME frm-Allocate
DO:
  VIEW FRAME frm-cbPick.
  OPEN QUERY qry-cbPick FOR EACH cbkmf WHERE cbkmf.Bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-cbPick.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-cbPick 
          OR close of THIS-PROCEDURE IN FRAME frm-cbPick
          OR CHOOSE OF btnok IN FRAME frm-cbPick 
          OR 'enter':u OF brw-cbPick
          OR 'mouse-select-dblclick' OF brw-cbPick.
  CLOSE QUERY qry-cbPick.
  HIDE FRAME frm-cbPick.
  APPLY 'tab' TO btnBank.
  APPLY 'tab' TO bfr{&tmpTable}.Bank.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-cbPick 
    OR 'enter':u OF brw-cbPick
    OR 'mouse-select-dblclick' OF brw-cbPick
DO: 
   GET CURRENT qry-cbPick NO-WAIT.
   DISPLAY cbkmf.Bank @ bfr{&tmpTable}.Bank cbkmf.DESCRIP WITH FRAME frm-allocate.
   APPLY 'TAB' TO bfr{&tmpTable}.Bank IN FRAME frm-allocate.
END.

ON 'tab' of bfr{&tmpTable}.Bank IN FRAME frm-allocate
    OR 'enter' OF bfr{&tmpTable}.Bank IN FRAME frm-Allocate
DO:
    IF  INT(bfr{&tmpTable}.Bank:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST cbkmf WHERE cbkmf.Bank = int(bfr{&tmpTable}.Bank:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Cashbook number entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE
        DISPLAY cbkmf.Bank @ bfr{&tmpTable}.Bank cbkmf.DESCRIP WITH FRAME frm-Allocate.
    END.
END.
*/
ON CHOOSE OF btnLedger IN FRAME frm-Allocate
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnLedger.
  APPLY 'tab' TO bfr{&tmpTable}.Ledger.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger NO-WAIT.
   DISPLAY glmf.acct @  bfr{&tmpTable}.Ledger glmf.DESCRIPTION WITH FRAME frm-allocate.
   APPLY 'TAB' TO  bfr{&tmpTable}.Ledger IN FRAME frm-allocate.
END.

ON 'tab' of  bfr{&tmpTable}.Ledger IN FRAME frm-allocate
    OR 'enter' OF  bfr{&tmpTable}.Ledger IN FRAME frm-Allocate
DO:
    IF  DEC( bfr{&tmpTable}.Ledger:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Invalid ledger entered" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.Acct = DEC(bfr{&tmpTable}.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL glmf THEN DO:
            MESSAGE "Invalid Ledger entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY glmf.Acct @  bfr{&tmpTable}.Ledger glmf.DESCRIPTION WITH FRAME frm-Allocate.
            IF glmf.dept <> 0 THEN DO:
               DISPLAY glmf.Dept @  bfr{&tmpTable}.dept WITH FRAME frm-Allocate.
               APPLY 'tab' TO bfr{&tmpTable}.dept IN FRAME frm-Allocate.
               DISABLE bfr{&tmpTable}.dept WITH FRAME frm-Allocate.
            END.
            ELSE ENABLE bfr{&tmpTable}.dept WITH FRAME frm-Allocate.
            IF glmf.proj <> 0 THEN DO:
               DISPLAY glmf.proj @  bfr{&tmpTable}.proj WITH FRAME frm-Allocate.
               APPLY 'tab' TO bfr{&tmpTable}.proj IN FRAME frm-Allocate.
               DISABLE bfr{&tmpTable}.proj WITH FRAME frm-Allocate.
            END.
            ELSE ENABLE bfr{&tmpTable}.proj WITH FRAME frm-Allocate.
            IF glmf.fund <> 0 THEN DO:
               DISPLAY glmf.fund @  bfr{&tmpTable}.fund WITH FRAME frm-Allocate.
               APPLY 'tab' TO bfr{&tmpTable}.fund IN FRAME frm-Allocate.
               DISABLE bfr{&tmpTable}.fund WITH FRAME frm-Allocate.
            END.
            ELSE ENABLE bfr{&tmpTable}.fund WITH FRAME frm-Allocate.
        END.
    END.
END.

ON CHOOSE OF btnDept IN FRAME frm-Allocate
DO:
  VIEW FRAME frm-pickdept.
  OPEN QUERY qry-PickDept FOR EACH glDept NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickdept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickdept 
          OR close of THIS-PROCEDURE IN FRAME frm-pickdept
          OR CHOOSE OF btn-ok IN FRAME frm-pickdept 
          OR 'enter':u OF brw-PickDept
          OR 'mouse-select-dblclick' OF brw-PickDept.
  CLOSE QUERY qry-PickDept.
  HIDE FRAME frm-pickdept.
  APPLY 'tab' TO btnDept.
  APPLY 'tab' TO bfr{&tmpTable}.dept IN FRAME frm-Allocate.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickdept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY gldept.dept @ bfr{&tmpTable}.dept gldept.descrip WITH FRAME frm-Allocate.
   RETURN.
END.

ON 'enter':U OF bfr{&tmpTable}.dept IN FRAME frm-allocate
   OR 'tab':U OF bfr{&tmpTable}.dept IN FRAME frm-allocate
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(bfr{&tmpTable}.dept:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE gldept THEN DO:
        MESSAGE "Invalid Department entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY gldept.descrip WITH FRAME frm-Allocate.
    END.
    RETURN.
END.

ON CHOOSE OF btnProj IN FRAME frm-Allocate
DO:
  VIEW FRAME frm-pickProj.
  OPEN QUERY qry-PickProj FOR EACH glProj NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickProj.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickProj 
          OR close of THIS-PROCEDURE IN FRAME frm-pickProj
          OR CHOOSE OF btn-ok IN FRAME frm-pickProj 
          OR 'enter':u OF brw-PickProj
          OR 'mouse-select-dblclick' OF brw-PickProj.
  CLOSE QUERY qry-PickProj.
  HIDE FRAME frm-pickProj.
  APPLY 'tab' TO btnProj.
  APPLY 'tab' TO bfr{&tmpTable}.proj.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ bfr{&tmpTable}.Proj glproj.DESCRIPTION  WITH FRAME frm-Allocate.
   RETURN.
END.

ON 'enter':U OF bfr{&tmpTable}.proj IN FRAME frm-allocate
    OR 'tab':U OF bfr{&tmpTable}.proj IN FRAME frm-allocate
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(bfr{&tmpTable}.proj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glProj.descrip WITH FRAME frm-Allocate.
    END.
    RETURN.
END.

ON CHOOSE OF btnFund IN FRAME frm-Allocate
DO:
  VIEW FRAME frm-PickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-PickFund
          OR CHOOSE OF btn-ok IN FRAME frm-PickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-PickFund.
  APPLY 'tab' TO btnFund.
   APPLY 'tab' TO bfr{&tmpTable}.fund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfr{&tmpTable}.Fund glFund.DESCRIP  WITH FRAME frm-Allocate.
   RETURN.
END.

ON 'enter':U OF bfr{&tmpTable}.Fund IN FRAME frm-allocate
    OR 'tab':U OF bfr{&tmpTable}.Fund IN FRAME frm-allocate
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(bfr{&tmpTable}.Fund:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Fundect entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.descrip WITH FRAME frm-Allocate.
    END.
    RETURN.
END.

ON 'tab' OF st-Per IN FRAME frm-Pay
    OR 'enter' OF st-Per IN FRAME frm-Pay
DO:
  FIND FIRST SIMCTR NO-LOCK NO-ERROR .
  IF INT(st-per:SCREEN-VALUE) <= SIMCTR.CLOSEPER
      OR INT(st-per:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
      MESSAGE "Invalid Accounting period entered" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
END.

ON 'TAB':U OF wsDate IN FRAME frm-pay
   OR 'enter':U OF wsDate IN FRAME frm-pay
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'tab' OF wsAmt IN FRAME frm-Pay
    OR 'enter' OF wsAmt IN FRAME frm-Pay
DO:
  IF dec(wsAmt:SCREEN-VALUE) > 0.00 THEN DO:
       FIND first tblForex WHERE tblForex.txtCur = cbkmf.txtCur AND date(wsDate:SCREEN-VALUE) >= tblForex.DtRate  NO-LOCK NO-ERROR.
       IF AVAILABLE tblForex THEN DO:
           wsRate = tblForex.decRate.
       END.
       ELSE DO:
            FIND LAST tblForexH WHERE tblForexH.txtCur = cbkmf.txtCur AND tblForexH.dtRate <= date(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
                 wsRate = tblForexH.decRate.
       END.
       wsTotal = DEC(wsAmt:SCREEN-VALUE).
       RUN allocate-ip.
  END.
  ELSE
      RETURN NO-APPLY.
END.

ON 'tab' OF bfr{&tmpTable}.Amount IN FRAME frm-Allocate
    OR 'enter' OF bfr{&tmpTable}.Amount IN FRAME frm-Allocate
DO:
  IF dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) > 0.00 
         AND dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) <= wsTotal THEN DO:
      wsTotal = wsTotal - dec(bfr{&tmpTable}.Amount:SCREEN-VALUE).
      X = X  + 1.
      CREATE tmpTrans.
      ASSIGN tmpTrans.bank      = INT(wsbank:SCREEN-VALUE IN FRAME frm-Pay)
             tmpTrans.Accper   = INT(st-per:SCREEN-VALUE IN FRAME frm-Pay)
             tmpTrans.amount   = dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) * wsF
             tmpTrans.Descrip  = bfr{&tmpTable}.descrip:SCREEN-VALUE IN FRAME frm-Allocate
             tmpTrans.Ledger   = DEC(bfr{&tmpTable}.ledger:SCREEN-VALUE)
             tmpTrans.dept     = int(bfr{&tmpTable}.dept:SCREEN-VALUE)
             tmpTrans.Proj     = int(bfr{&tmpTable}.Proj:SCREEN-VALUE)
             tmpTrans.Fund     = int(bfr{&tmpTable}.Fund:SCREEN-VALUE)
             tmpTrans.OpCode   = varUser
             tmpTrans.Ref      = wsRef:SCREEN-VALUE IN FRAME frm-Pay
             tmpTrans.seq      = X
             tmpTrans.decRate  = wsRate
             tmpTrans.txtCur   = wsCur
             tmpTrans.TransID  = wsTransId
             tmpTrans.TranType = cbtype.TranType
             tmpTrans.trDate   = DATE(wsDate:SCREEN-VALUE IN FRAME frm-pay).
      IF wsTotal > 0.00  THEN DO:
        CLEAR FRAME frm-Allocate.
        DISPLAY wsTotal WITH FRAME frm-Allocate.
        APPLY 'entry' TO  bfr{&tmpTable}.ledger IN FRAME frm-Allocate.
        RETURN.
      END.
      ELSE do:
          OPEN QUERY qry-cbkmf FOR EACH  tmpTrans EXCLUSIVE-LOCK.
             APPLY "CLOSE":U TO THIS-PROCEDURE.
             RETURN.
          END.
  END.
  ELSE DO:
      MESSAGE "Amount enter is either less than 0.00 or Greater than " wsTotal
              SKIP "Please re-enter Amount up to" wsTotal VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
  END.   
END.

ON CHOOSE OF btnPrint IN FRAME frm-Pay   
DO: 
    ASSIGN st-per = INT(st-per:SCREEN-VALUE)
           wsYear = INT(SUBSTR(STRING(st-per),1,4))
           wsMonth = INT(SUBSTR(STRING(st-per),5,2))
           wsRef wsDate st-per wsAmt wsNar
           wsTotal = 0.
   /*FIND FIRST cbkmf WHERE cbkmf.bank = INT(wsBank:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
   cbkmf.bal = cbkmf.bal + ( dec(wsAmt:SCREEN-VALUE) * wsF ). */
   CREATE bfr{&tmpTable}. /* Update and Create Bank transaction */
   ASSIGN bfr{&tmpTable}.Accper = INT(st-per:SCREEN-VALUE)
          bfr{&tmpTable}.amount = ( wsAmt * wsF )
          bfr{&tmpTable}.bank   = int(wsBank:screen-value)
          bfr{&tmpTable}.Descrip = wsNar:SCREEN-VALUE IN FRAME frm-Pay
          bfr{&tmpTable}.OpCode  = varUser
          bfr{&tmpTable}.Ref     = wsRef:SCREEN-VALUE IN FRAME frm-Pay
          bfr{&tmpTable}.TransID = wsTransId
          bfr{&tmpTable}.TranType = cbtype.TranType
          bfr{&tmpTable}.trDate   = DATE(wsDate:SCREEN-VALUE)
          bfr{&tmpTable}.ledger   = 99
          bfr{&tmpTable}.seq      = 0
          bfr{&tmpTable}.decRate  = wsRate
         bfr{&tmpTable}.txtCur    = wsCur.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged} 
  CLEAR FRAME frm-Pay.
   ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
          wsDate:SCREEN-VALUE = STRING(TODAY).
   APPLY 'entry' TO wsBank IN FRAME frm-pay.
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
wsTime    = STRING(TIME,"HH:MM:SS").
wsTransID = DEC (string(YEAR(TODAY),"9999")
              + string(MONTH(TODAY),"99" )
              + string(DAY(TODAY),"99")
              + SUBSTR(STRING(wsTIME),1,2) 
              + SUBSTR(STRING(wsTIME),4,2) 
              + SUBSTR(STRING(wsTIME),7,2) ).
CLEAR FRAME frm-Pay.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY).
ENABLE ALL WITH FRAME frm-Pay.
APPLY 'entry' TO wsBank IN FRAME frm-Pay.
WAIT-FOR CHOOSE OF btnClose OR CLOSE of THIS-PROCEDURE IN FRAME frm-Pay.
HIDE FRAME frm-Pay.

PROCEDURE allocate-ip:
  VIEW FRAME frm-Allocate.
  ENABLE ALL WITH FRAME frm-Allocate.
  DISPLAY wsTotal WITH FRAME frm-Allocate.
  APPLY 'entry' TO bfr{&tmpTable}.ledger IN FRAME frm-Allocate.
  WAIT-FOR close of THIS-PROCEDURE IN FRAME frm-Allocate.
  HIDE FRAME frm-Allocate.
END.

PROCEDURE Update-ip:
DO TRANSACTION ON ERROR UNDO, LEAVE:
    wsTotal = 0.
    DISPLAY STREAM a wsDate st-Per cbkmf.descrip wsNar (wsAmt * wsF) @ wsAmt WITH FRAME frm-rpt.
   DOWN 4 STREAM a WITH FRAME frm-rpt.
   FOR EACH tmpTrans WHERE tmpTrans.TransID = wsTransId BREAK BY tmpTrans.bank:
       CREATE bfr{&tmpTable}. /* Update and Create Bank transaction */
       BUFFER-COPY tmpTrans TO bfr{&tmpTable}.
       wsTotal = wsTotal + tmpTrans.Amount.
       IF FIRST-OF(tmpTrans.bank) THEN DO:
           FIND FIRST cbkmf WHERE cbkmf.Bank = tmpTrans.bank EXCLUSIVE-LOCK NO-ERROR.
           FIND FIRST glmf WHERE glmf.acct = cbkmf.ledger NO-ERROR.
           wsFund = glmf.fund.
       END.
       FIND FIRST dbgl WHERE dbgl.acct = bfr{&tmpTable}.Ledger 
                         AND dbgl.dept = bfr{&tmpTable}.dept AND dbgl.fund = bfr{&tmpTable}.fund
                         AND dbgl.proj = bfr{&tmpTable}.proj no-error. /* create record for ledger */
       IF NOT AVAILABLE dbgl THEN DO:
          CREATE dbgl.
          ASSIGN  dbgl.CREDATE = TODAY
                  dbgl.DESCRIP  = wsNar:SCREEN-VALUE IN FRAME frm-Pay
                  dbgl.period   = INT(st-per:SCREEN-VALUE)
                  dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Pay
                  dbgl.TransID  = wsTransId
                  dbgl.trDATE   = DATE(wsDate:SCREEN-VALUE)
                  dbgl.UID      = varUser
                  dbgl.acct     = tmpTrans.Ledger
                  dbgl.Dept     = tmpTrans.Dept
                  dbgl.Proj     = tmpTrans.Proj
                  dbgl.fund     = tmpTrans.fund
                  dbgl.SOURCE   = "CB".
       END.
       ASSIGN dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * wsRate * -1).
       IF LAST-OF(tmpTrans.bank) THEN DO: /*bank Contra records */
           ASSIGN cbkmf.bal = cbkmf.Bal + wsTotal.
           FIND FIRST glmf WHERE glmf.acct = cbkmf.Ledger NO-LOCK NO-ERROR.
           FIND FIRST dbgl WHERE dbgl.acct = cbkmf.Ledger AND dbgl.fund = glmf.fund 
                             AND dbgl.dept = glmf.dept AND dbgl.proj = glmf.proj no-error.
           IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN  dbgl.CREDATE = TODAY
                       dbgl.DESCRIP  = wsNar:SCREEN-VALUE IN FRAME frm-Pay
                       dbgl.period   = INT(st-per:SCREEN-VALUE)
                       dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Pay
                       dbgl.TransID  = wsTransId
                       dbgl.trDATE   = DATE(wsDate:SCREEN-VALUE)
                       dbgl.UID      = varUser
                       dbgl.acct     = cbkmf.Ledger
                       dbgl.fund     = glmf.fund 
                       dbgl.dept     = glmf.dept 
                       dbgl.proj     = glmf.proj
                       dbgl.SOURCE   = "CB".
           END.
           ASSIGN dbgl.AMT = dbgl.AMT + (wsTotal * wsRate)
                  wsTotal   = 0.
       END. /*eof if-last-of */
       /* Fund Accounting */
      IF tmpTrans.fund <> glmf.fund AND simctr.FUNDACC = YES THEN DO:
           DO X = 1 TO 2:
               ASSIGN wsLedger = SIMCTR.FUNDDb  WHEN X = 1
                      wsLedger = SIMCTR.FUNDCr  WHEN X = 2.
               FIND FIRST dbgl WHERE dbgl.acct = wsLedger no-error.
               IF NOT AVAILABLE dbgl THEN DO:
                  CREATE dbgl.
                  ASSIGN dbgl.CREDATE = TODAY
                         dbgl.DESCRIP  = "Resource advance: EX#" + STRING(wsFund)
                                       + "/TO" + string(glmf.fund) 
                         dbgl.period   = INT(st-per:SCREEN-VALUE)
                         dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Pay
                         dbgl.TransID  = wsTransId
                         dbgl.trDATE   = DATE(wsDate:SCREEN-VALUE)
                         dbgl.UID      = varUser
                         dbgl.acct     = wsLedger
                         dbgl.fund     = glmf.fund  WHEN X = 1
                         dbgl.dept     = glmf.dept  WHEN X = 1
                         dbgl.proj     = glmf.proj  WHEN X = 1
                         dbgl.Dept     = tmpTrans.Dept WHEN X = 2
                         dbgl.Proj     = tmpTrans.Proj WHEN X = 2
                         dbgl.fund     = tmpTrans.fund WHEN X = 2
                         dbgl.SOURCE   = "CB".
               END.
               ASSIGN dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * wsRate) WHEN X = 2
                      dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * wsRate * -1) WHEN X = 1.
           END.
       END.
       DISPLAY STREAM a cbkmf.descrip tmpTrans.Descrip tmpTrans.Ledger tmpTrans.amount
           WITH FRAME frm-rpt1.
       DOWN STREAM a WITH FRAME frm-rpt1.
       DELETE tmpTrans.
   END. /* eof for each */
   DOWN 4 STREAM a WITH FRAME frm-rpt1.
   {glcon.i}
   RELEASE CBKMF.
   RELEASE CBTRANS.
   RELEASE GLTDF.
   RELEASE GLBAL.
   CLEAR FRAME frm-Pay.
   ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
          wsDate:SCREEN-VALUE = STRING(TODAY).
   ENABLE ALL WITH FRAME frm-Pay.
   APPLY 'entry' TO wsBank. 
   RETURN.
END.
END.

procedure Search.ip.
    hCol = browse  brw-ledger:current-column.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        case trim(hCol:label):
            when "Account" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.acct >= DEC(wsSearch:SCREEN-VALUE)
                               BY glmf.acct.
            END.
            when "Description" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.descrip >= wsSearch:SCREEN-VALUE
                               BY glmf.descrip.
            END. 
            when "Department" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.dept >= INT(wsSearch:SCREEN-VALUE)
                               BY glmf.dept BY glmf.acct.

            END.
        END.
        RETURN.
    END. 
