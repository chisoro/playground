/* Program.................Bkpay.p
   Notes:................. Online Payment
   Author:.................S. Mawire
   Edited:.................S. Chisoro
*/

&SCOPED-DEFINE tmpTable            CBTrans
&SCOPED-DEFINE skey                CBTrans.bank

SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
{varlibrary.i}
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsper    LIKE simctr.curper.
DEF VAR wsyear   AS INT.
DEF VAR wsmonth  AS INT.
DEF VAR wsBank   AS CHAR FORM "x(12)".
DEF VAR st-per   LIKE CBTrans.Accper.
DEF VAR wsAmt    LIKE cbkmf.bal.
DEF VAR wsVat    LIKE wsAmt.
DEF VAR wsCrdvat LIKE wsAmt.
DEF VAR wsCrdAmt LIKE wsAmt.
DEF VAR wsTotal  LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsDr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsCr     LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsAmt.
DEF VAR wsTCr    LIKE wsAmt.
DEF VAR wsNar    LIKE Crdmf.Name.
DEF VAR wsRef    LIKE cbtrans.ref.
DEF VAR wsDate   LIKE cbTrans.trDate.
DEF VAR wsFund   LIKE glmf.fund INITIAL 0.
DEF VAR wsLedger LIKE cbkmf.ledger.
DEF VAR wsDesc   LIKE glmf.DESCRIPTION.
DEF VAR wsPay AS LOGICAL LABEL "Pay Creditor Y/N".
DEF VAR wsAcc    LIKE crdmf.acc.
DEF VAR wsSource LIKE gltdf.SOURCE INITIAL "CB".
DEF VAR X        AS INT.
DEF VAR Y        AS INT.
DEF VAR wsfrm    AS INT.
DEF VAR wsCur    LIKE tblForex.txtCur.
DEF VAR wsRate   LIKE tblForex.decRate.
DEF VAR ws-Batch AS INT.


DEF TEMP-TABLE tmpTrans LIKE CBTrans.
DEF BUFFER bfrCrdtmf FOR crdtmf.

DEF BUTTON btnPrint    LABEL "PRINT".
DEF BUTTON btnok       LABEL "OK".
DEF BUTTON btnClose    LABEL "CANCEL".
DEF BUTTON btnInv      LABEL "INVOICE".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 99.8 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 99.8 by 7.8.

DEF    QUERY qry-cbkmf FOR tmpTrans SCROLLING.
DEF BROWSE brw-cbkmf QUERY qry-cbkmf
    DISPLAY tmpTrans.Accper  COLUMN-LABEL "Period" tmpTrans.trDate  COLUMN-LABEL "Date" tmpTrans.bank tmpTrans.Ref  COLUMN-LABEL "REF"
    tmpTrans.seq COLUMN-LABEL "Seq" tmpTrans.Descrip  COLUMN-LABEL "Description" WIDTH 32 tmpTrans.Ledger tmpTrans.amount   
    WITH 10 DOWN SEPARATORS.
     
DEF    QUERY qry-cbPick FOR cbkmf SCROLLING.
DEF BROWSE brw-cbPick QUERY qry-cbPick
     DISPLAY cbkmf.bank cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-BankPick FOR cbkmf SCROLLING.
DEF BROWSE brw-BankPick QUERY qry-BankPick
     DISPLAY cbkmf.bank cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.


DEF    QUERY qry-Crd FOR crdmf SCROLLING.
DEF BROWSE brw-crd QUERY qry-crd
     DISPLAY crdmf.BrCode crdmf.Acc crdmf.Name WIDTH 40 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-BankPick 
     brw-BankPick AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Bank Selection".

DEFINE FRAME frm-crd 
     brw-crd AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Creditor Selection".

DEF    QUERY qry-inv FOR crdtmf SCROLLING.
DEF BROWSE brw-inv QUERY qry-inv
     DISPLAY crdtmf.acc Crdtmf.TrDate crdtmf.ref Crdtmf.Amt crdtmf.decBal WIDTH 40 WITH 20 DOWN SEPARATORS.


DEFINE FRAME frm-inv 
     brw-inv AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Creditor Invoice Selection".

DEF FRAME frm-Pay
     btnBank AT ROW 1.5 COL 5  NO-TAB-STOP SPACE(.5)
     wsBank NO-LABEL  AUTO-RETURN SPACE(5) 
     wsCur LABEL "Currency" VIEW-AS TEXT
     cbkmf.Bal LABEL "Bank Balance"  VIEW-AS FILL-IN NO-TAB-STOP SKIP(0.2) 
     cbkmf.descrip COLON 10 NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 by 1
     SKIP(0.2)
     st-per LABEL "Period"   COLON 10 AUTO-RETURN SPACE(10)
     wsDate LABEL "Date"  SPACE(20)
     wsRef  LABEL "Reference" SKIP(0.5)
     wsPay  COLON 25 btnAcc NO-TAB-STOP wsAcc NO-LABEL SKIP(0.2)
     wsNar    LABEL "Payee" COLON 10 VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.2) 
     wsAmt    LABEL "Amount" COLON 10 SKIP(0.2)
     "Accounting Cashbook and Ledger Allocations" COLON 15
     brw-cbkmf    AT ROW 10 COL 3 AUTO-RETURN
     btnPrint    AT ROW 19.3 COL 20 SPACE(40) btnClose
     rect-1      AT ROW 18.8 COL 3
     rect-2     AT ROW 1.27 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 105 BY 21.5 TITLE "ONLINE PAYMENT".

 DEFINE FRAME frm-allocate 
     SKIP(1)
    wsTotal  COLON 80 LABEL "Amount to be allocated" VIEW-AS TEXT
    SKIP(1)
    btnLedger COLON 5 NO-LABEL
    bfr{&tmpTable}.Ledger  NO-LABEL  
    glmf.DESCRIPTION NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(0.5)
    btnDept                 COLON 5  no-tab-stop
    bfr{&tmpTable}.Dept              NO-LABEL
    SPACE(1) glDept.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
    btnProj                 COLON 16  no-tab-stop
    bfr{&tmpTable}.Proj              NO-LABEL
    SPACE(1) glProj.DESCRIPTION  view-as text no-label NO-TAB-STOP skip(0.5)
    btnFund                 COLON 10  no-tab-stop
    bfr{&tmpTable}.Fund              NO-LABEL
    SPACE(1) glFund.DESCRIP  view-as text no-label NO-TAB-STOP skip(0.5)
    bfr{&tmpTable}.Descrip COLON 15 LABEL "Narration" VIEW-AS FILL-IN SIZE 50 BY 1 SKIP(0.5)
    bfr{&tmpTable}.amount  COLON 20 LABEL "Amount" SKIP(2)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "ALLOCATIONS".

 DEFINE FRAME frm-InvAllocate 
     SKIP(1)
    wsTotal  COLON 60 LABEL "Amount to be allocated" VIEW-AS TEXT
    SKIP(1)
    btnInv COLON 10 NO-LABEL  NO-TAB-STOP Crdtmf.Ref NO-LABEL SKIP(0.5)
    Crdtmf.decBal  COLON 20 LABEL "Invoice Balance" NO-TAB-STOP SKIP(0.5)
    bfr{&tmpTable}.amount  COLON 20 LABEL "Amount" SKIP(2)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "INVOICE ALLOCATIONS".

DEFINE FRAME frm-cbPick 
     brw-cbPick AT ROW 1 COL 1.5 skip(0.5)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Cashbook Selection".

FORM
    wsDate        LABEL "DATE " SPACE(4)
    st-Per        LABEL "PERIOD" 
    cbkmf.descrip LABEL "BANK " 
    wsNar         LABEL "PAYEE" 
    wsAmt         LABEL "AMOUNT " 
    HEADER skip(3) "PAYMENT DETAILS FOR PAYMENT VOUCHER #" wsRef  "Page: " AT 80 PAGE-NUMBER(a)
     SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

FORM 
    cbkmf.descrip LABEL "CASHBOOK" 
    tmpTrans.Descrip FORM "x(32)" LABEL "NARRATION" 
    tmpTrans.Ledger LABEL "ALLOCATION" 
    tmpTrans.amount LABEL "AMOUNT" 
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt1.

FORM 
     dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(3) "PAYMENTS UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

/* Main frame frm-Pay triggers */
ON CHOOSE OF btnBank IN FRAME frm-Pay
DO:
  VIEW FRAME frm-Bankpick.
  OPEN QUERY qry-BankPick FOR EACH cbkmf WHERE cbkmf.bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-Bankpick.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Bankpick 
          OR close of THIS-PROCEDURE IN FRAME frm-Bankpick
          OR CHOOSE OF btnok IN FRAME frm-Bankpick 
          OR 'enter':u OF brw-BankPick
          OR 'mouse-select-dblclick' OF brw-BankPick.
  CLOSE QUERY qry-BankPick.
  HIDE FRAME frm-Bankpick.
  APPLY 'tab' TO btnBank.
  APPLY 'tab' TO wsBank.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Bankpick 
    OR 'enter':u OF brw-BankPick
    OR 'mouse-select-dblclick' OF brw-BankPick
DO: 
   GET CURRENT qry-BankPick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY cbkmf.Bank @ wsBank cbkmf.DESCRIP cbkmf.Bal cbkmf.txtCur @ wscur WITH FRAME frm-Pay.
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
        ELSE
        DISPLAY cbkmf.Bank @ wsBank cbkmf.DESCRIP cbkmf.bal cbkmf.txtCur @ wscur WITH FRAME frm-Pay.
    END.
    ASSIGN wsCur wsBank.
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

ON 'tab':U OF wsPay IN FRAME frm-pay
    OR 'enter':U OF wsPay IN FRAME frm-pay
DO:
    ASSIGN wsPay.
    IF wsPay = NO THEN DO:
        DISABLE btnAcc wsAcc WITH FRAME frm-pay.
        APPLY 'entry' TO wsNar.
    END.
    ELSE IF wsPay = YES THEN DO:
        ENABLE btnAcc wsAcc WITH FRAME frm-pay.
        APPLY 'entry' TO wsAcc.
    END.
    RETURN.
END.

ON CHOOSE OF btnAcc IN FRAME frm-Pay
DO:
  VIEW FRAME frm-crd.
  OPEN QUERY qry-crd FOR EACH crdmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-crd.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-crd 
          OR close of THIS-PROCEDURE IN FRAME frm-crd
          OR CHOOSE OF btnok IN FRAME frm-crd 
          OR 'enter':u OF brw-crd
          OR 'mouse-select-dblclick' OF brw-crd.
  CLOSE QUERY qry-crd.
  HIDE FRAME frm-crd.
  APPLY 'tab' TO btnAcc.
  APPLY 'tab' TO wsAcc.
  APPLY 'tab' TO wsNar.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-crd 
    OR 'enter':u OF brw-crd
    OR 'mouse-select-dblclick' OF brw-crd
DO: 
   GET CURRENT qry-crd NO-WAIT.
   DISPLAY crdmf.Acc @ wsAcc crdmf.Name @ wsNar WITH FRAME frm-pay.
   APPLY 'TAB' TO wsAcc IN FRAME frm-pay.
END.

ON 'tab':U OF wsAcc IN FRAME frm-Pay
    OR 'enter':U OF wsAcc IN FRAME frm-pay
DO:
    ASSIGN wsAcc.
    FIND FIRST crdmf WHERE crdmf.acc = wsAcc  EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE crdmf THEN  DO:
        DISPLAY crdmf.NAME FORM "x(40)" @ wsNar WITH FRAME frm-pay.
        APPLY 'tab' TO wsNar.
    END.
    ELSE IF NOT AVAILABLE crdmf THEN DO:
        MESSAGE "Invalid creditor entered, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'tab' OF wsAmt IN FRAME frm-Pay
    OR 'enter' OF wsAmt IN FRAME frm-Pay
DO:
  IF dec(wsAmt:SCREEN-VALUE) <> 0.00 THEN DO:
      /*Modified to check the date of the transaction before applying exchange rate  S. Chisoro*/
    FIND FIRST tblForex WHERE tblForex.txtCur = (wsCur:SCREEN-VALUE) AND date(wsDate:SCREEN-VALUE) >= tblForex.DtRate  NO-LOCK NO-ERROR.
         IF AVAILABLE tblForex THEN DO:
            ASSIGN wsRate = tblForex.decRate 
                   wsTotal = dec(wsAmt:SCREEN-VALUE).
         END.
         ELSE IF NOT AVAILABLE tblForex  THEN DO:
             FIND FIRST tblForexH WHERE tblForexH.txtCur = wsCur:SCREEN-VALUE AND tblForexH.dtRate <= date(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
                IF AVAILABLE tblForexH THEN DO:
                    ASSIGN wsRate = tblForexH.decRate
                           wsTotal = dec(wsAmt:SCREEN-VALUE).
                END.
         END.
      ASSIGN wsCur = wsCur:SCREEN-VALUE
            wsTime    = STRING(TIME,"HH:MM:SS").
            wsTransID = DEC (string(YEAR(TODAY),"9999")
                      + string(MONTH(TODAY),"99" )
                      + string(DAY(TODAY),"99")
                      + SUBSTR(STRING(wsTIME),1,2) 
                      + SUBSTR(STRING(wsTIME),4,2) 
                      + SUBSTR(STRING(wsTIME),7,2) ).
      RUN allocate-ip.
      END.
  ELSE
      RETURN NO-APPLY.
END.

ON CHOOSE OF btnPrint IN FRAME frm-Pay   
DO: 
    ASSIGN st-per = INT(st-per:SCREEN-VALUE)
           wsYear = INT(SUBSTR(STRING(st-per),1,4))
           wsMonth = INT(SUBSTR(STRING(st-per),5,2))
           wsRef wsDate st-per wsAmt wsNar
           X = 0.
   FIND FIRST cbkmf WHERE cbkmf.bank = INT(wsBank:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
   cbkmf.bal = cbkmf.bal - dec(wsAmt:SCREEN-VALUE).
   /*FIND FIRST tblForex WHERE tblForex.txtCur = wsCur:SCREEN-VALUE NO-LOCK NO-ERROR. */
   CREATE bfr{&tmpTable}. /* Update and Create Bank transaction */
   ASSIGN bfr{&tmpTable}.Accper = INT(st-per:SCREEN-VALUE)
          bfr{&tmpTable}.amount = dec(wsAmt:SCREEN-VALUE) * -1
          bfr{&tmpTable}.bank   = int(wsBank:screen-value)
          bfr{&tmpTable}.Descrip = wsNar:SCREEN-VALUE IN FRAME frm-Pay
          bfr{&tmpTable}.OpCode  = varUser
          bfr{&tmpTable}.Ref     = wsRef:SCREEN-VALUE IN FRAME frm-Pay
          bfr{&tmpTable}.TransID = wsTransId
          bfr{&tmpTable}.TranType = 1
          bfr{&tmpTable}.trDate   = DATE(wsDate:SCREEN-VALUE)
          bfr{&tmpTable}.txtCur  = wsCur
          bfr{&tmpTable}.decRate = wsRate
          bfr{&tmpTable}.ledger  = 99
          bfr{&tmpTable}.seq     = 0.
   IF wsPay = YES THEN DO:
        CREATE bfrCrdtmf. /* Update and create Creditor  transaction */
            /*modified S. Chisoro */
        FIND first tblForex WHERE tblForex.txtCur = (wsCur:SCREEN-VALUE) AND date(wsDate:SCREEN-VALUE) >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
           ASSIGN wsrate = tblForex.decRate.
        END.
        IF NOT AVAILABLE tblForex  THEN DO:
           FIND LAST tblForexH WHERE tblForexH.txtCur = wsCur:SCREEN-VALUE AND tblForexH.dtRate <= date(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
          IF AVAILABLE tblForexH THEN DO:
             ASSIGN wsrate = tblForexH.decRate.
          END.
        END.
        ASSIGN bfrCrdtmf.period  = INT(st-per:SCREEN-VALUE IN FRAME frm-pay)
              bfrCrdtmf.Acc      = wsAcc
              bfrCrdtmf.Descrip  = wsNar:SCREEN-VALUE IN FRAME frm-pay
              bfrCrdtmf.uid      = varUser
              bfrCrdtmf.Ref      = wsRef:SCREEN-VALUE IN FRAME frm-pay
              bfrCrdtmf.TransID  = wsTransId
              bfrCrdtmf.TrType   = 1
              bfrCrdtmf.trDate   = DATE(wsDate:SCREEN-VALUE)
              bfrCrdtmf.ledger   = 0
              bfrCrdtmf.CreDate  = TODAY 
              bfrCrdtmf.OrdNo    = 0
              bfrCrdtmf.Quantity = 1
              bfrCrdtmf.VAT      = 0
              bfrCrdtmf.amt = ROUND((dec(wsAmt:SCREEN-VALUE)  * -1 * wsrate),2).
   END.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged}
   CLEAR FRAME frm-Pay.
   ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
          wsDate:SCREEN-VALUE = STRING(TODAY)
          wsPay:SCREEN-VALUE = "YES"
          wsCur:SCREEN-VALUE = "ZWL".
   APPLY 'entry' TO wsBank IN FRAME frm-pay.
END.

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
   APPLY 'tab' TO  bfr{&tmpTable}.Ledger IN FRAME frm-allocate.
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

ON 'tab' OF bfr{&tmpTable}.Amount IN FRAME frm-Allocate
    OR 'enter' OF bfr{&tmpTable}.Amount IN FRAME frm-Allocate
DO:
    IF  dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) > wsTotal THEN DO:
        MESSAGE "Amount enter is either less than 0.00 or Greater than " wsTotal
                  SKIP "Please re-enter Amount up to" wsTotal VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END. 
    ELSE IF dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) <= wsTotal THEN DO:
        wsTotal = wsTotal - dec(bfr{&tmpTable}.Amount:SCREEN-VALUE).
        IF wsPay = YES THEN DO: /* Creditors payments */
            FIND FIRST simctr NO-LOCK NO-ERROR.
        END. /* if wsPay... */
           CREATE tmpTrans.
           X = X + 1.
            ASSIGN tmpTrans.bank   = INT(wsBank:SCREEN-VALUE IN FRAME frm-pay)
                 tmpTrans.Accper   = INT(st-per:SCREEN-VALUE IN FRAME frm-Pay)
                 tmpTrans.amount   = dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) * -1
                 tmpTrans.Descrip  = bfr{&tmpTable}.descrip:SCREEN-VALUE IN FRAME frm-Allocate 
                 tmpTrans.Ledger   = DEC(bfr{&tmpTable}.ledger:SCREEN-VALUE) 
                 tmpTrans.OpCode   = varUser
                 tmpTrans.Dept     = INT(bfr{&tmpTable}.Dept:SCREEN-VALUE)
                 tmpTrans.Proj     = INT(bfr{&tmpTable}.Proj:SCREEN-VALUE)
                 tmpTrans.fund     = INT(bfr{&tmpTable}.Fund:SCREEN-VALUE)
                 tmpTrans.Ref      = wsRef:SCREEN-VALUE IN FRAME frm-Pay
                 tmpTrans.Seq      = X
                 tmpTrans.TransID  = wsTransId
                 tmpTrans.TranType = 1
                 tmpTrans.trDate   = DATE(wsDate:SCREEN-VALUE IN FRAME frm-pay)
                 tmpTrans.decRate  = wsRate
                 tmpTrans.txtcur   = wsCur.
            IF wsTotal > 0.00  THEN DO:
                 CLEAR FRAME frm-Allocate.
                 DISPLAY wsTotal WITH FRAME frm-Allocate.
                 APPLY 'entry' TO  bfr{&tmpTable}.ledger IN FRAME frm-Allocate.
            END. 
            ELSE DO:
                OPEN QUERY qry-cbkmf FOR EACH  tmpTrans EXCLUSIVE-LOCK.
                APPLY "CLOSE":U TO THIS-PROCEDURE.
                APPLY 'ENTRY' TO btnPrint IN FRAME frm-pay.
                RETURN.
            END.
    END.
END.

/* Frame frm-invAllocate trigers */
ON CHOOSE OF btnInv IN FRAME frm-invAllocate
DO:
  VIEW FRAME frm-Inv.
  OPEN QUERY qry-Inv FOR EACH crdtmf WHERE crdtmf.acc = wsAcc AND crdtmf.decBal > 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-inv.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Inv 
          OR close of THIS-PROCEDURE IN FRAME frm-inv
          OR CHOOSE OF btnok IN FRAME frm-inv 
          OR 'enter':u OF brw-inv
          OR 'mouse-select-dblclick' OF brw-inv.
  CLOSE QUERY qry-inv.
  HIDE FRAME frm-inv.
  APPLY 'tab' TO crdtmf.ref.
  RETURN. 
END.

ON CHOOSE OF btnok IN FRAME frm-Inv 
    OR 'enter':u OF brw-Inv
    OR 'mouse-select-dblclick' OF brw-Inv
DO: 
   GET CURRENT qry-Inv EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY crdtmf.ref crdtmf.decBal @ bfr{&tmpTable}.Amount WITH FRAME frm-invAllocate.
   APPLY 'tab' TO crdtmf.ref IN FRAME frm-invAllocate.
END.

ON 'tab':U OF crdtmf.ref IN FRAME frm-invAllocate
    OR 'enter':U OF crdtmf.ref IN FRAME frm-invAllocate
DO:
    IF  (crdtmf.ref:SCREEN-VALUE) <> "" THEN DO:
        FIND FIRST tmpTrans WHERE tmpTrans.ref = crdtmf.ref NO-ERROR.
        IF AVAILABLE tmpTrans THEN DO:
           MESSAGE "Invoice has already been allocated, please pick another invoice"
                   VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
         END.
         ELSE IF crdtmf.ref <> "" THEN DO: 
              DISPLAY crdtmf.decBal WITH FRAME frm-invAllocate.
              APPLY 'tab' TO crdtmf.decBal IN FRAME frm-invAllocate.
         END. 
    END.
    RETURN.
END.

ON 'tab' OF bfr{&tmpTable}.Amount IN FRAME frm-InvAllocate
    OR 'enter' OF bfr{&tmpTable}.Amount IN FRAME frm-InvAllocate
DO:
    IF  crdtmf.ref:SCREEN-VALUE <> "" AND dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) > crdtmf.decBal THEN DO:
        MESSAGE "Amount enter is greater than Invoice Balance " crdtmf.decBal
           SKIP "Please re-enter Amount up to" crdtmf.decBal " and choose another invoice" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF  dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) > wsTotal THEN DO:
        MESSAGE "Amount enter is either less than 0.00 or Greater than " wsTotal
                  SKIP "Please re-enter Amount up to" wsTotal VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END. 
    ELSE IF dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) <= wsTotal THEN DO:
        FIND FIRST simctr NO-LOCK NO-ERROR.
        FIND FIRST tmpTrans WHERE tmpTrans.srcCode = "crd#" + crdtmf.ref NO-ERROR.
        IF AVAILABLE tmpTrans THEN DO:
            MESSAGE "Invoice has already been allocated, please pick another invoice"
                VIEW-AS ALERT-BOX.
            APPLY 'entry' TO crdtmf.ref IN FRAME frm-invAllocate.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            wsTotal = wsTotal - dec(bfr{&tmpTable}.Amount:SCREEN-VALUE).
            FIND FIRST cbkmf WHERE cbkmf.bank = INT(wsbank:SCREEN-VALUE IN FRAME frm-pay) NO-LOCK NO-ERROR.
            FIND FIRST glmf WHERE glmf.acct = cbkmf.Ledger NO-LOCK NO-ERROR.
            CREATE tmpTrans.
            X = X + 1.
            ASSIGN tmpTrans.bank      = INT(wsbank:SCREEN-VALUE IN FRAME frm-pay)
                   tmpTrans.Accper   = INT(st-per:SCREEN-VALUE IN FRAME frm-Pay)
                   tmpTrans.amount   = dec(bfr{&tmpTable}.Amount:SCREEN-VALUE) * -1
                   tmpTrans.Descrip  = "Creditor Payment: Acc#" + string(crdmf.acc)
                   tmpTrans.Ledger   = simctr.CrdLedger
                   tmpTrans.OpCode   = varUser
                   tmpTrans.Dept   = glmf.Dept
                   tmpTrans.Proj   = glmf.Proj
                   tmpTrans.fund   = glmf.Fund
                   tmpTrans.Ref    =  wsRef:SCREEN-VALUE IN FRAME frm-Pay
                   tmpTrans.srcCode = /*"crd#" + */ crdtmf.ref:SCREEN-VALUE
                   tmpTrans.seq    = X
                   tmpTrans.TransID  = wsTransId
                   tmpTrans.TranType = 1
                   tmpTrans.txtCur   = wsCur
                   tmpTrans.decRate  = wsRate
                   tmpTrans.trDate   = DATE(wsDate:SCREEN-VALUE IN FRAME frm-pay).
            IF wsTotal > 0.00  THEN DO:
                 CLEAR FRAME frm-invAllocate.
                 DISPLAY wsTotal WITH FRAME frm-invAllocate.
                 APPLY 'entry' TO  crdtmf.ref IN FRAME frm-invAllocate.
            END. 
            ELSE DO:
                OPEN QUERY qry-cbkmf FOR EACH  tmpTrans EXCLUSIVE-LOCK.
                APPLY "CLOSE":U TO THIS-PROCEDURE.
                APPLY 'ENTRY' TO btnPrint IN FRAME frm-pay.
                RETURN.
            END.
        END.
    END.
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
CLEAR FRAME frm-Pay.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsPay:SCREEN-VALUE = "YES"
       wsCur:SCREEN-VALUE = "ZWL".
ENABLE ALL WITH FRAME frm-Pay.
APPLY 'entry' TO wsBank IN FRAME frm-Pay.
WAIT-FOR CHOOSE OF btnClose OR CLOSE of THIS-PROCEDURE IN FRAME frm-Pay.
HIDE FRAME frm-Pay.

PROCEDURE allocate-ip:
    IF wspay = NO THEN DO:
        VIEW FRAME frm-Allocate.
        ENABLE ALL WITH FRAME frm-Allocate.
        DISPLAY wsTotal WITH FRAME frm-Allocate.
        APPLY 'entry' TO bfr{&tmpTable}.ledger IN FRAME frm-Allocate.
        WAIT-FOR close of THIS-PROCEDURE IN FRAME frm-Allocate.
        HIDE FRAME frm-Allocate.
        APPLY 'entry' TO btnPrint IN FRAME frm-pay.
    END.
  ELSE IF wsPay = YES THEN DO:
      VIEW FRAME frm-invAllocate.
      ENABLE ALL WITH FRAME frm-invAllocate.
      DISPLAY wsTotal WITH FRAME frm-invAllocate.
      APPLY 'entry' TO crdtmf.ref IN FRAME frm-invAllocate.
      WAIT-FOR close of THIS-PROCEDURE IN FRAME frm-invAllocate.
      HIDE FRAME frm-invAllocate.
      APPLY 'entry' TO btnPrint IN FRAME frm-pay.
  END.
END.

PROCEDURE Update-ip:
    DO TRANSACTION ON ERROR UNDO, LEAVE:
        wsTotal = 0.
    DISPLAY STREAM a wsDate st-Per cbkmf.descrip wsNar (wsAmt * -1) @ wsAmt WITH FRAME frm-rpt.
       DOWN 3 STREAM a WITH FRAME frm-rpt.
       FOR EACH tmpTrans WHERE tmpTrans.TransID = wsTransId BREAK BY tmpTrans.bank:
           CREATE bfr{&tmpTable}. /* Update and Create Acb transaction */
           BUFFER-COPY tmpTrans TO bfr{&tmpTable}.
           ASSIGN /*bfr{&tmpTable}.Amount = (bfr{&tmpTable}.Amount * tblForex.decRate) */
                  wsTotal = wsTotal + tmpTrans.Amount /*(* tblForex.decRate) */.
           IF FIRST-OF(tmpTrans.bank) THEN DO:
               FIND FIRST cbkmf WHERE cbkmf.bank = tmpTrans.bank EXCLUSIVE-LOCK NO-ERROR.
               FIND FIRST glmf WHERE glmf.acct = cbkmf.ledger NO-LOCK NO-ERROR.
               wsFund = glmf.fund.
           END.
           FIND FIRST dbgl WHERE dbgl.acct = bfr{&tmpTable}.Ledger 
                             AND dbgl.dept = bfr{&tmpTable}.dept 
                             AND dbgl.fund = bfr{&tmpTable}.fund
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
           IF LAST-OF(tmpTrans.bank) THEN DO: /*Acb Contra records */
               /*ASSIGN cbkmf.bal = cbkmf.Bal + wsTotal. */
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
               DO Y = 1 TO 2:
                   ASSIGN wsLedger = SIMCTR.FUNDDb  WHEN Y = 1
                          wsLedger = SIMCTR.FUNDCr  WHEN Y = 2.
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
                             dbgl.fund     = glmf.fund  WHEN Y = 1
                             dbgl.dept     = glmf.dept  WHEN Y = 1
                             dbgl.proj     = glmf.proj  WHEN Y = 1
                             dbgl.Dept     = tmpTrans.Dept WHEN Y = 2
                             dbgl.Proj     = tmpTrans.Proj WHEN Y = 2
                             dbgl.fund     = tmpTrans.fund WHEN Y = 2
                             dbgl.SOURCE   = "CB".
                   END.
                   ASSIGN dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * wsrate) WHEN Y = 2
                          dbgl.AMT = dbgl.AMT + (tmpTrans.Amount * wsrate * -1) WHEN Y = 1.
               END.
           END.
           DISPLAY STREAM a cbkmf.descrip tmpTrans.Descrip tmpTrans.Ledger 
               tmpTrans.amount  @ tmpTrans.Amount WITH FRAME frm-rpt1.
           DOWN STREAM a WITH FRAME frm-rpt1.
           IF wsPay = YES THEN
               RUN crdtaf-ip.
      
       END. /* eof for each */
       IF wsPay = YES THEN
           RUN crd-update.
       DOWN 2 STREAM a WITH FRAME frm-rpt1.
       ASSIGN wsTCr = 0
              wsTDr = 0.
           DELETE tmpTrans.
      {glcon.i}
       RELEASE CBKMF.
       RELEASE CBTRANS.
       RELEASE GLTDF.
       RELEASE GLBAL.
       ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
              wsDate:SCREEN-VALUE = STRING(TODAY)
              wsCur:SCREEN-VALUE = "ZWL".
       ENABLE ALL WITH FRAME frm-Pay.
       APPLY 'entry' TO wsBank. 
       RETURN.
    END.
END PROCEDURE.

PROCEDURE crdtaf-ip:
    CREATE  crdtaf.
    ASSIGN  crdtaf.acc     = wsAcc
            crdtaf.amount  = tmpTrans.Amount * wsRate
            crdtaf.Descrip = "PAYMENT" + tmpTrans.ref
            crdtaf.period  = tmpTrans.Accper
            crdtaf.Dept    = tmpTrans.Dept
            crdtaf.Fund    = tmpTrans.fund
            crdtaf.Ledger  = 0
            crdtaf.Proj    = tmpTrans.Proj
            crdtaf.TransID = tmpTrans.TransID
            crdtaf.trDate  = tmpTrans.trDate
            crdtaf.UID     = tmpTrans.OpCode
            wsCrdAmt             = wsCrdAmt + (tmpTrans.Amount * wsRate).
    IF (crdtmf.ref:SCREEN-VALUE IN FRAME frm-invAllocate) <> "" THEN DO:
       FIND FIRST crdtmf WHERE crdtmf.acc = wsAcc AND crdtmf.ref = tmpTrans.srccode NO-ERROR.
        IF AVAILABLE crdtmf THEN DO:
            ASSIGN crdtaf.VAT     = ROUND(((crdtmf.vatBal / crdtmf.DecBal) * (tmpTrans.Amount * wsRate)),2)
                   crdtmf.vatBal  =  crdtmf.vatBal + crdtaf.VAT
                   crdtmf.decBal  =  Crdtmf.decBal + (tmpTrans.Amount * wsRate)
                   wsCrdVat       = wsCrdVat + crdtaf.VAT.
                  /* bfrCrdtmf.decBal     =  bfrCrdtmf.decBal + (tmpTrans.Amount * wsRate)
                   bfrCrdtmf.VAT        =  bfrCrdtmf.VAT + wsCrdVat. */
        END.
    END.     
END PROCEDURE.
                    

PROCEDURE crd-update:
    FIND FIRST  crdmf WHERE  crdmf.acc = wsAcc NO-ERROR.
    ASSIGN  crdmf.VatAmt =  crdmf.vatAmt + wsCrdVat
            crdmf.Bal    =  crdmf.Bal + wsCrdAmt.
    IF wsCrdVat <> 0 THEN
       DO  Y = 1 TO 2:
           CREATE dbgl.
           ASSIGN dbgl.CREDATE = TODAY
                 dbgl.DESCRIP  = "Creditor Payment: Acc#" + string(crdmf.acc) 
                 dbgl.period   = INT(st-per:SCREEN-VALUE IN FRAME frm-Pay)
                 dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Pay
                 dbgl.TransID  = wsTransId
                 dbgl.trDATE   = DATE(wsDate:SCREEN-VALUE)
                 dbgl.UID      = varUser
                 dbgl.fund     = tmpTrans.fund 
                 dbgl.dept     = tmpTrans.dept 
                 dbgl.proj     = tmpTrans.proj
                 dbgl.SOURCE   = "CB".
           IF Y = 1 THEN
               ASSIGN dbgl.AMT  = dbgl.AMT + wsCrdVat 
                      dbgl.acct = DEC(simctr.vat[3]).
           ELSE IF Y = 2 THEN
               ASSIGN dbgl.AMT  = dbgl.AMT + wsCrdVat * -1 
                      dbgl.acct = DEC(simctr.vat[4]).
       END.
       wsCrdVat = 0.
       wsCrdAmt = 0.
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
