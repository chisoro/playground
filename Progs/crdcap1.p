/* Program.................crdCap.p
   Notes:................. Online creditors Data capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Creditor does not exist"
&SCOPED-DEFINE wsTitle           " Online creditors Data capture"
&SCOPED-DEFINE tmptable             Crdtmf
&SCOPED-DEFINE skey                Crdtmf.Acc
SESSION:DATA-ENTRY-RETURN = TRUE.
{varlibrary.i}
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId LIKE gltdf.TransID.
DEF VAR wsTime   AS CHAR FORM "x(8)".
DEF VAR wsper    LIKE simctr.curper.
DEF VAR wsyear   AS INT.
DEF VAR wsmonth  AS INT.
DEF VAR wsAcc    LIKE crdmf.acc.
DEF VAR st-per   LIKE CBTrans.Accper.
DEF VAR wsCtr    LIKE crdtmf.ledger.
DEF VAR wsAmt    LIKE crdmf.bal FORM "zzz,zzz,zz9.99-".
DEF VAR NoteAmt    LIKE crdmf.bal FORM "zzz,zzz,zz9.99-".
DEF VAR NoteVAT    LIKE crdmf.bal FORM "zzz,zzz,zz9.99-".
DEF VAR wsRef    LIKE crdtmf.ref.
DEF VAR wsOrd    LIKE crdtmf.OrdNo.
DEF VAR wsQuant  LIKE crdtmf.quantity FORM "zzz,zzz,zz9.99-".
DEF VAR wsVat    LIKE crdtmf.Vat FORM "zzz,zzz,zz9.99-".
DEF VAR wsType   LIKE crdtmf.Trtype.
DEF VAR wsNote   LIKE stkgrn.intGrn.
DEF VAR wsTotal  LIKE wsAmt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTVat   LIKE wsAmt FORM "zz,zzz,zz9.99-".
DEF VAR wsDr     LIKE wsAmt FORM "zzz,zzz,zz9.99-".
DEF VAR wsCr     LIKE wsAmt FORM "zzz,zzz,zz9.99-".
DEF VAR wsTDr    LIKE wsAmt.
DEF VAR wsTCr    LIKE wsAmt.
DEF VAR wsNar    LIKE crdtmf.descrip.
DEF VAR wsDate   LIKE crdtmf.trDate.
DEF VAR wsFund   LIKE glmf.fund.
DEF VAR wsLedger LIKE crdtmf.ledger.
DEF VAR wsDesc   LIKE glmf.DESCRIPTION.
DEF VAR wsWarehse LIKE stkwhf.WareHse.
DEF VAR wsStock  LIKE stkmf.stkCode.
DEF VAR wsOpt    AS CHAR FORM "X".
DEF VAR wsFac    AS INT.
DEF VAR wsGrn    AS LOGICAL .
DEF VAR X        AS INT.
DEF VAR wsSource LIKE gltdf.SOURCE INITIAL "CR".

DEF BUFFER bfrCrdtaf FOR Crdtaf.
DEF BUFFER bfrStkTrans FOR stkTrans.
DEF BUFFER bfrgrn      FOR stkgrn.

DEF TEMP-TABLE tmpTrans LIKE Crdtaf.
DEF TEMP-TABLE tmpStock LIKE stktrans.

DEF BUTTON btnPrint    LABEL "COMMIT".
DEF BUTTON btnok       LABEL "OK".
DEF BUTTON btnClose    LABEL "Cancel".
DEF BUTTON btnL        LABEL "GENERAL LEDGER".
DEF BUTTON btns        LABEL "STORES".
DEF BUTTON btnOrd      LABEL " ORDER ".


DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 85.5 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 85.5 by 14.2.

DEF    QUERY qry-Pick FOR crdmf SCROLLING.
DEF BROWSE brw-Pick QUERY qry-Pick
     DISPLAY crdmf.Acc crdmf.NAME WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-Ord FOR ordmf SCROLLING.
DEF BROWSE brw-Ord QUERY qry-Ord
     DISPLAY ordmf.OrdNo ordmf.OrdDate ordmf.Descrip WIDTH 40 ordmf.OrdAmt WITH 5 DOWN SEPARATORS.

DEF    QUERY qry-Store FOR stkmf SCROLLING.
DEF BROWSE brw-Store QUERY qry-Store
     DISPLAY stkmf.WareHse stkmf.stkCode stkmf.Descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-crdTrans FOR tmpTrans SCROLLING.
DEF BROWSE brw-crdTrans QUERY qry-crdTrans
     DISPLAY tmpTrans.Descrip tmpTrans.amount FORM "zzz,zzz,zz9.99-" 
            tmpTrans.VAT FORM "zz,zzz,zz9.99-" tmpTrans.Ledger WITH 5 DOWN SEPARATORS.

DEF FRAME frm-Input
    SKIP(1)
     st-per LABEL "Accounting Period"   COLON 20 AUTO-RETURN SKIP(0.2)
     btnacc    COLON 7.5  NO-TAB-STOP
     wsAcc       NO-LABEL  AUTO-RETURN 
     crdmf.NAME  NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.2)
     wsType    COLON 20  LABEL "Transaction Type" 
               VIEW-AS COMBO-BOX LIST-ITEM-PAIRS  "Invoice", 2, "Journal", 3, 
                "Credit Note", 4, " Discount", 5 SKIP(0.2)  
     wsDate    COLON 20  LABEL "Transaction Date"  SPACE(10)
     wsRef               LABEL "Ref/Invoice"  SKIP(0.2)
     btnOrd    COLON 40  wsOrd NO-LABEL SKIP(0.2)
     wsNar     COLON 20  LABEL "Description"  SKIP(0.2)
     wsAmt     COLON 20  LABEL "Amount (INC-VAT)"       SKIP(0.2)
     wsVat     COLON 20  LABEL "VAT"         SKIP(0.2)
     "Accounting  Ledger Allocations" COLON 15 SKIP(0.2)
     brw-crdTrans    AT ROW 16.5 COL 5
     btnPrint     AT ROW 21.8 COL 10 SPACE(40) btnClose
     rect-1      AT ROW 21.3 COL 2
     rect-2     AT ROW 1.27 COL 2
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 89 BY 24.5 TITLE "ONLINE DATA CAPTURE".

DEFINE FRAME frm-alloc 
    SKIP(1)
    btnL COLON 5 SKIP(1)
    btnS COLON 10 SKIP(1)
    wsGrn   LABEL "Goods Received at Stores(Y/N?" SKIP(1) 
    WITH  VIEW-AS DIALOG-BOX SIDE-LABEL NO-UNDERLINE THREE-D CENTERED TITLE "ALLOCATE TO".


 DEFINE FRAME frm-allocate 
     SKIP(1)
    wsTotal  COLON 60 LABEL "Amount to be allocated" VIEW-AS TEXT "  VAT: " wsTVat NO-LABEL VIEW-AS TEXT
    SKIP(1)
    btnLedger           COLON 16 NO-LABEL
    bfrCrdtaf.Ledger    NO-LABEL  
    glmf.DESCRIPTION    NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.2)
    btnDept             COLON 4  no-tab-stop
    bfrCrdtaf.Dept      NO-LABEL
    glDept.DESCRIP      NO-LABEL VIEW-AS text  NO-TAB-STOP skip(0.2)
    btnFund             COLON 9  no-tab-stop
    bfrCrdtaf.Fund      NO-LABEL
    glfund.descrip      NO-LABEL VIEW-AS text  skip(0.2)
    btnProj             COLON 15  no-tab-stop
    bfrCrdtaf.Proj              NO-LABEL
    SPACE(1) glProj.DESCRIPTION  view-as text no-label NO-TAB-STOP skip(0.2)
    bfrCrdtaf.Descrip COLON 27 LABEL "Description" VIEW-AS FILL-IN SIZE 50 BY 1 SKIP(0.2)
    bfrCrdtaf.amount  COLON 27 LABEL "Amount (INC-VAT)" FORM "zzz,zzz,zz9.99-" SKIP(0.2)
    wsQuant           COLON 27  LABEL "Quantity"     SKIP(0.2)
    bfrCrdtaf.Vat     COLON 27 LABEL "VAT" FORM "zzz,zzz,zz9.99-" SKIP(1)
    btnClose COLON 40
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "ALLOCATIONS".

DEFINE FRAME frm-Supplier 
     brw-Pick AT ROW 1 COL 1.5 skip(0.2)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Supplier Selection".
      
DEFINE FRAME frm-Store 
     brw-Store AT ROW 1 COL 1.5 skip(0.2)
     btnOk COLON 8 SPACE (20) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Stock code Selection".


DEFINE FRAME frm-Ord 
     brw-Ord AT ROW 1 COL 1.5 skip(0.2)
     btnOk COLON 20 SPACE (30) btnclose 
     WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
          SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Outstanding Orders Selection".

FORM 
    wsDate        LABEL "DATE " SPACE(4)
    st-Per        LABEL "PERIOD" 
    wsNar         LABEL "SUPPLIER" 
    wsAmt         LABEL "AMOUNT " 
    HEADER SKIP(3) "TRANSACTION CAPTURE/UPDATE REPORT # "wsRef  "Page: " AT 80 PAGE-NUMBER(a)
     SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-CrdRpt.

FORM 
    crdmf.NAME LABEL "SUPPLIER" 
    tmpTrans.Descrip FORM "x(32)" LABEL "NARRATION" 
    tmpTrans.Ledger LABEL "ALLOCATION" 
    tmpTrans.amount LABEL "AMOUNT" FORM "zzz,zzz,zz9.99-" 
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-CrdRpt1.

FORM 
     dbgl.Fund             LABEL "SEGMENT" 
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "CREDITORS JOURNAL UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.
ON 'tab':U OF wstype IN FRAME frm-Input
    OR 'leave':U OF wstype IN FRAME frm-Input
DO:
    ASSIGN wsType.
    IF wstype <> 2 THEN
        DISABLE btnOrd wsOrd WITH FRAME frm-Input.
    ELSE
        ENABLE btnOrd wsOrd WITH FRAME frm-Input.
    RETURN.
END.
ON CHOOSE OF btnacc IN FRAME frm-Input
DO:
  VIEW FRAME frm-Supplier.
  OPEN QUERY qry-pick FOR EACH crdmf WHERE crdmf.acc <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-Supplier.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Supplier 
          OR close of THIS-PROCEDURE IN FRAME frm-Supplier
          OR CHOOSE OF btnok IN FRAME frm-Supplier 
          OR 'enter':u OF brw-pick
          OR 'mouse-select-dblclick' OF brw-pick.
  CLOSE QUERY qry-pick.
  HIDE FRAME frm-Supplier.
  APPLY 'tab' TO btnacc.
  APPLY 'tab' TO wsAcc.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Supplier 
    OR 'enter':u OF brw-pick
    OR 'mouse-select-dblclick' OF brw-pick
DO: 
   GET CURRENT qry-pick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY crdmf.Acc @ wsAcc crdmf.NAME WITH FRAME frm-Input.
   APPLY 'TAB' TO wsAcc IN FRAME frm-Input.
END.

ON 'tab' of wsAcc IN FRAME frm-Input
    OR 'enter' OF wsAcc IN FRAME frm-Input
DO:
    IF  INT(wsAcc:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frm-Input.
    ELSE DO:
        ASSIGN wsAcc = INT(wsAcc:SCREEN-VALUE). 
        FIND FIRST crdmf WHERE crdmf.acc = int(wsAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL crdmf THEN DO:
            MESSAGE "Invalid Supplier entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frm-Input ALL.
            RETURN NO-APPLY.
        END.
        ELSE
        DISPLAY crdmf.Acc @ wsAcc crdmf.NAME  WITH FRAME frm-Input.
    END.
END.

ON CHOOSE OF btnOrd IN FRAME frm-Input
DO:
  VIEW FRAME frm-Ord.
  OPEN QUERY qry-Ord FOR EACH Ordmf WHERE Ordmf.acc = INT(wsAcc:SCREEN-VALUE) AND
      ordmf.OrdStat = 1 OR (ordmf.OrdStat = 2 AND ordmf.InvAmt < ordmf.OrdAmt)  NO-LOCK.
  ENABLE ALL WITH FRAME frm-Ord.
  WAIT-FOR CHOOSE OF btnClose IN FRAME frm-Ord 
          OR close of THIS-PROCEDURE IN FRAME frm-Ord
          OR CHOOSE OF btnok IN FRAME frm-Ord 
          OR 'enter':u OF brw-Ord
          OR 'mouse-select-dblclick' OF brw-Ord.
  CLOSE QUERY qry-Ord.
  HIDE FRAME frm-Ord.
  APPLY 'tab' TO btnOrd.
  APPLY 'tab' TO wsOrd.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-Ord 
    OR 'enter':u OF brw-Ord
    OR 'mouse-select-dblclick' OF brw-Ord
DO: 
   GET CURRENT qry-Ord EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY Ordmf.OrdNo @ wsOrd WITH FRAME frm-Input.
   APPLY 'TAB' TO wsOrd IN FRAME frm-Input.
END.

ON 'tab' of wsOrd IN FRAME frm-Input
    OR 'enter' OF wsOrd IN FRAME frm-Input
DO:
   IF DEC(wsOrd:SCREEN-VALUE) <> 0 THEN DO:
        FIND FIRST ordmf WHERE Ordmf.acc = INT(wsAcc:SCREEN-VALUE) AND ordmf.ordNo = DEC(wsOrd:SCREEN-VALUE)
            AND ordmf.OrdStat = 1 
            OR (ordmf.OrdStat = 2 AND ordmf.InvAmt < ordmf.OrdAmt) NO-ERROR. 
        IF NOT AVAILABLE ordmf THEN DO:
            MESSAGE "Specified order not available or may have been invoiced" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN wsNar:SCREEN-VALUE = ordmf.Descrip    
                   wsAmt:SCREEN-VALUE = STRING(ordmf.OrdAmt).
            APPLY 'entry' TO wsAmt IN FRAME frm-Input.
            RETURN NO-APPLY.
        END.
    END.
END.

ON 'tab':U OF wsAmt IN FRAME frm-Input
    OR 'enter':U OF wsAmt IN FRAME frm-input
DO:
    DISABLE btnPrint WITH FRAME frm-input.
    RETURN.
END.

ON 'tab' OF wsVat IN FRAME frm-Input
    OR 'enter' OF wsVat IN FRAME frm-Input
DO:
    ASSIGN wsVat = DEC(wsVat:SCREEN-VALUE).
  IF dec(wsAmt:SCREEN-VALUE) <> 0.00 THEN DO:
     ASSIGN wsTotal = dec(wsAmt:SCREEN-VALUE)
            wsTVat  = wsVat
            wsType = INT(wsType:SCREEN-VALUE IN FRAME frm-input)
            wsTime    = STRING(TIME,"HH:MM:SS")
            wsTransID = DEC (string(YEAR(TODAY),"9999")
                      + string(MONTH(TODAY),"99" )
                      + string(DAY(TODAY),"99")
                      + SUBSTR(STRING(wsTIME),1,2) 
                      + SUBSTR(STRING(wsTIME),4,2) 
                      + SUBSTR(STRING(wsTIME),7,2) ).
     IF DEC(wsOrd:SCREEN-VALUE) = 0 THEN
        RUN allocate-ip.
     ELSE RUN Per-Order.
  END.
END.

ON 'choose':U OF btnLedger 
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-Ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-Ledger
          OR 'mouse-select-dblclick' OF brw-Ledger.
  CLOSE QUERY qry-Ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnLedger.
  APPLY 'tab' TO bfrCrdtaf.Ledger.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-Ledger
    OR 'mouse-select-dblclick' OF brw-Ledger
DO: 
   GET CURRENT qry-Ledger NO-WAIT.
   DISPLAY glmf.acct @  bfrCrdtaf.Ledger WITH FRAME frm-allocate.
END.

ON 'tab' of  bfrCrdtaf.Ledger IN FRAME frm-allocate
    OR 'enter' OF  bfrCrdtaf.Ledger IN FRAME frm-Allocate
DO:
    IF  DEC( bfrCrdtaf.Ledger:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Invalid ledger entered" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.Acct = dec(bfrCrdtaf.ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL glmf THEN DO:
            MESSAGE "Invalid Ledger entered" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY glmf.Acct @  bfrCrdtaf.Ledger glmf.DESCRIPTION WITH FRAME frm-Allocate.
            IF glmf.dept <> 0 THEN DO:
               FIND FIRST gldept WHERE gldept.dept = glmf.dept NO-LOCK NO-ERROR.
               DISPLAY glmf.Dept @ bfrCrdtaf.Dept gldept.descrip  WITH FRAME frm-Allocate.
               DISABLE btnDept bfrCrdtaf.Dept WITH FRAME frm-Allocate.
            END.   
            ELSE  
               ENABLE btnDept bfrCrdtaf.Dept WITH FRAME frm-Allocate.
            IF glmf.fund <> 0 THEN DO:
               FIND FIRST glFund WHERE glFund.fund = glmf.fund NO-LOCK NO-ERROR.
               DISPLAY glmf.fund @ bfrCrdtaf.fund glfund.descrip  WITH FRAME frm-Allocate.
               DISABLE btnFund bfrCrdtaf.Fund WITH FRAME frm-Allocate.
            END.
            ELSE  
               ENABLE btnFund bfrCrdtaf.Fund WITH FRAME frm-Allocate.
            IF glm.proj <> 0 THEN DO:
               FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
               DISPLAY glmf.Proj @ bfrCrdtaf.Proj glProj.DESCRIPTION WITH FRAME frm-Allocate.
               DISABLE btnProj bfrCrdtaf.Proj WITH FRAME frm-Allocate.
            END.
            ELSE 
               ENABLE btnProj bfrCrdtaf.Proj WITH FRAME frm-Allocate.
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
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickdept 
    OR 'enter':u OF brw-PickDept
    OR 'mouse-select-dblclick' OF brw-PickDept
DO: 
   GET CURRENT qry-PickDept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY gldept.dept @ bfrCrdtaf.dept gldept.descrip WITH FRAME frm-Allocate.
   RETURN.
END.

ON 'enter':U OF bfrCrdtaf.dept IN FRAME frm-allocate
   OR 'tab':U OF bfrCrdtaf.dept IN FRAME frm-allocate
DO:
    FIND FIRST gldept WHERE gldept.dept = INT(bfrCrdtaf.dept:SCREEN-VALUE) NO-ERROR.
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
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ bfrCrdtaf.Proj glproj.DESCRIPTION  WITH FRAME frm-Allocate.
   RETURN.
END.
ON 'enter':U OF bfrCrdtaf.proj IN FRAME frm-allocate
    OR 'tab':U OF bfrCrdtaf.proj IN FRAME frm-allocate
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(bfrCrdtaf.proj:SCREEN-VALUE) NO-ERROR.
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
  VIEW FRAME frm-pickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-pickFund
          OR CHOOSE OF btn-ok IN FRAME frm-pickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-pickFund.
  APPLY 'tab' TO btnFund IN FRAME frm-Allocate.
  APPLY 'entry' TO bfrCrdtaf.Fund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund NO-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfrCrdtaf.Fund glFund.DESCRIP  WITH FRAME frm-Allocate.
   APPLY 'tab' TO btnFund IN FRAME frm-Allocate.
   RETURN.
END.

ON 'enter':U OF bfrCrdtaf.Fund 
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(bfrCrdtaf.Fund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Segment/Fund entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.Descrip WITH FRAME frm-Allocate.
        APPLY 'tab' TO bfrCrdtaf.Fund IN FRAME frm-Allocate.
    END.
    RETURN.
END.

ON 'tab' OF st-Per IN FRAME frm-Input
    OR 'enter' OF st-Per IN FRAME frm-Input
DO:
  FIND FIRST SIMCTR NO-LOCK NO-ERROR .
  IF INT(st-per:SCREEN-VALUE) <= SIMCTR.CLOSEPER
      OR INT(st-per:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
      MESSAGE "Invalid Accounting period entered" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
END.

ON 'tab' OF bfrCrdtaf.Vat IN FRAME frm-Allocate
    OR 'enter' OF bfrCrdtaf.Vat IN FRAME frm-Allocate
DO:
  IF dec(bfrCrdtaf.Amount:SCREEN-VALUE) <> 0.00 
         AND dec(bfrCrdtaf.Amount:SCREEN-VALUE) <= wsTotal
         AND dec(bfrCrdtaf.VAT:SCREEN-VALUE) <= wsTVat  THEN DO:
      ASSIGN wsTotal = wsTotal - dec(bfrCrdtaf.Amount:SCREEN-VALUE)
             wsTVat = wsTVat - dec(bfrCrdtaf.VAT:SCREEN-VALUE).
      IF wsType = 2 OR wsType = 3 THEN
          wsFac = 1.
      ELSE IF wsType = 4 OR wsType = 5 THEN 
          wsFac = -1.
      X = X + 1.
      CREATE tmpTrans.
      ASSIGN tmpTrans.LineSeq  = X
             tmpTrans.Acc      = wsAcc
             tmpTrans.period   = INT(st-per:SCREEN-VALUE IN FRAME frm-Input)
             tmpTrans.amount   = dec(bfrCrdtaf.Amount:SCREEN-VALUE) * wsFac
             tmpTrans.Descrip  = bfrCrdtaf.descrip:SCREEN-VALUE IN FRAME frm-Allocate
             tmpTrans.Ledger   = dec(bfrCrdtaf.ledger:SCREEN-VALUE)
             tmpTrans.Dept     = int(bfrCrdtaf.Dept:SCREEN-VALUE)
             tmpTrans.Proj     = int(bfrCrdtaf.Proj:SCREEN-VALUE)
             tmpTrans.Fund     = int(bfrCrdtaf.Fund:SCREEN-VALUE)
             tmpTrans.TransID  = wsTransId
             tmpTrans.vat      = dec(bfrCrdtaf.Vat:SCREEN-VALUE) * wsFac
             tmpTrans.trDate   = DATE(wsDate:SCREEN-VALUE IN FRAME frm-Input)
             tmpTrans.UID      = varUser. 
      IF wsTotal <> 0.00 OR wsTVat <> 0.00 THEN DO:
        CLEAR FRAME frm-Allocate.
        DISPLAY wsTotal wsTVat WITH FRAME frm-Allocate.
        APPLY 'entry' TO  bfrCrdtaf.ledger IN FRAME frm-Allocate.
        RETURN.
      END.
      ELSE DO:
         CLOSE QUERY qry-crdTrans.
         OPEN QUERY qry-crdTrans FOR EACH  tmpTrans EXCLUSIVE-LOCK.
         ENABLE btnPrint WITH FRAME frm-Input.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN.
     END.
  END.
  ELSE DO:
      MESSAGE "Amount enter is not within " wsTotal
              SKIP "Please re-enter Amount up to" wsTotal VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
  END.   
END.

ON 'TAB':U OF wsDate IN FRAME frm-Input
   OR 'enter':U OF wsDate IN FRAME frm-Input
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnPrint IN FRAME frm-Input   
DO: 
    ASSIGN st-per = INT(st-per:SCREEN-VALUE)
           wsYear = INT(SUBSTR(STRING(st-per),1,4))
           wsMonth = INT(SUBSTR(STRING(st-per),5,2))
           wsRef wsDate wsAmt wsNar wsVat wsOrd.          
   FIND FIRST crdmf WHERE crdmf.Acc = INT(wsAcc:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN crdmf.bal = crdmf.bal + dec(wsAmt:SCREEN-VALUE)
          crdmf.vatAmt = crdmf.vatAmt + dec(wsVat:SCREEN-VALUE)
          Crdmf.YtdAmt = Crdmf.YtdAmt + dec(wsAmt:SCREEN-VALUE).
   CREATE bfr{&tmptable}. /* Update and Create transaction */
   ASSIGN bfr{&tmptable}.period = INT(st-per:SCREEN-VALUE)
          bfr{&tmptable}.amt = dec(wsAmt:SCREEN-VALUE) * wsFac
          bfr{&tmptable}.Acc    = int(wsAcc:SCREEN-VALUE IN FRAME frm-Input)
          bfr{&tmptable}.Descrip = wsNar:SCREEN-VALUE IN FRAME frm-Input
          bfr{&tmptable}.uid     = varUser
          bfr{&tmptable}.Ref     = wsRef:SCREEN-VALUE IN FRAME frm-Input
          bfr{&tmptable}.TransID = wsTransId
          bfr{&tmptable}.TrType  = wsType
          bfr{&tmptable}.trDate   = DATE(wsDate:SCREEN-VALUE)
          bfr{&tmptable}.ledger   = 99
          bfr{&tmptable}.CreDate  = TODAY 
          bfr{&tmptable}.OrdNo    = wsOrd
          bfr{&tmptable}.Quantity = wsQuant
          bfr{&tmptable}.VAT      = dec(wsVat:SCREEN-VALUE) * wsFac
          bfr{&tmptable}.decBal = dec(wsAmt:SCREEN-VALUE) * wsFac
          bfr{&tmptable}.vatBal = dec(wsVat:SCREEN-VALUE) * wsFac.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged}
   CLEAR FRAME frm-Input.
   CLEAR FRAME frm-allocate.
   ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
          wsDate:SCREEN-VALUE = STRING(TODAY)
          wsDr = 0
          wsCr = 0.
   APPLY 'entry' TO wsAcc IN FRAME frm-input. 
END.

ON 'choose':U OF btnClose IN FRAME frm-Allocate 
DO:
    FOR EACH tmpTrans WHERE tmpTrans.TransID = wsTransId:
        DELETE tmpTrans.
    END.
    FOR EACH tmpStock WHERE tmpStock.TransID = wsTransId:
        DELETE tmpStock.
    END.
    CLEAR FRAME frm-Input.
    ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
           wsDate:SCREEN-VALUE = STRING(TODAY)
           wsType:SCREEN-VALUE = "2".
    APPLY 'entry' TO st-per IN FRAME frm-Input.
END.
/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
CLEAR FRAME frm-Input.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsType:SCREEN-VALUE = "2".
ENABLE ALL WITH FRAME frm-Input.
APPLY 'entry' TO st-per IN FRAME frm-Input.
WAIT-FOR CHOOSE OF btnClose OR CLOSE of THIS-PROCEDURE IN FRAME frm-Input.
HIDE FRAME frm-Input.

PROCEDURE allocate-ip:
  VIEW FRAME frm-Allocate.
  ENABLE ALL EXCEPT btnDept btnProj bfrCrdtaf.Dept bfrCrdtaf.Proj WITH FRAME frm-Allocate.
  DISPLAY wsTotal wsTVat WITH FRAME frm-Allocate.
  APPLY 'entry' TO bfrCrdtaf.ledger IN FRAME frm-Allocate.
  WAIT-FOR CHOOSE OF btnClose
      OR close of THIS-PROCEDURE IN FRAME frm-Allocate.
  HIDE FRAME frm-Allocate.
END.

PROCEDURE per-Order:
    FOR EACH Ordtmf WHERE Ordtmf.ordNo = ordmf.ordNo:
           wsTotal <> 0.00 OR wsTVat <> 0.00. 
              CREATE tmpTrans.
              ASSIGN tmpTrans.LineSeq  = Ordtmf.LineSeq
                     tmpTrans.Acc      = wsAcc
                     tmpTrans.period   = INT(st-per:SCREEN-VALUE IN FRAME frm-Input)
                     tmpTrans.Descrip  = Ordtmf.Descrip
                     tmpTrans.Ledger   = Ordtmf.Ledger 
                     tmpTrans.Dept     = Ordtmf.Dept
                     tmpTrans.Proj     =  Ordtmf.Proj
                     tmpTrans.Fund     = Ordtmf.Fund 
                     tmpTrans.TransID  = wsTransId
                     tmpTrans.trDate   = DATE(wsDate:SCREEN-VALUE IN FRAME frm-Input)
                     tmpTrans.UID      = varUser.
              IF  Ordtmf.Amt <= wsTotal THEN
                   ASSIGN tmpTrans.amount   = Ordtmf.Amt
                          tmpTrans.vat      = Ordtmf.vat
                          wsTotal           = wsTotal - Ordtmf.Amt.
              ELSE IF  Ordtmf.Amt > wsTotal THEN
                   ASSIGN tmpTrans.amount   = wsTotal
                          tmpTrans.vat      = ROUND(((Ordtmf.vat / Ordtmf.Amt) * wsTotal),2)
                          wsTotal           = wsTotal - wsTotal.
    END.
    CLOSE QUERY qry-crdTrans.
    OPEN QUERY qry-crdTrans FOR EACH  tmpTrans EXCLUSIVE-LOCK.
    ENABLE btnPrint WITH FRAME frm-Input.
    /*APPLY "CLOSE":U TO THIS-PROCEDURE. */
    RETURN.
END.

PROCEDURE Update-ip:
   DISPLAY STREAM a wsDate st-Per crdmf.NAME @ wsNar bfr{&tmptable}.amt @ wsAmt WITH FRAME frm-CrdRpt.
   DOWN 3 STREAM a WITH FRAME frm-CrdRpt.
   FOR EACH tmpTrans WHERE tmpTrans.TransID = wsTransId:
       CREATE bfrCrdtaf. /* Update and Create allocation transaction */
       BUFFER-COPY tmpTrans TO bfrCrdtaf.
       wsTotal = wsTotal + tmpTrans.Amount.
       FIND FIRST glmf WHERE glmf.acct = tmpTrans.ledger NO-ERROR.
      /* create records for ledger consoliation */
       FIND FIRST dbgl WHERE dbgl.acct = bfrCrdtaf.Ledger no-error. 
       IF NOT AVAILABLE dbgl THEN DO:
          CREATE dbgl. /* Allocation ledger */
          ASSIGN  dbgl.CREDATE = TODAY
                  dbgl.DESCRIP  = tmpTrans.Descrip
                  dbgl.period   = INT(st-per:SCREEN-VALUE IN FRAME frm-Input)
                  dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Input
                  dbgl.TransID  = wsTransId
                  dbgl.UID      = varUser
                  dbgl.acct     = tmpTrans.Ledger
                  dbgl.dept     = tmpTrans.Dept
                  dbgl.proj     = tmpTrans.Proj
                  dbgl.Fund     = tmpTrans.Fund
                  dbgl.SOURCE   = "CR"
                  dbgl.trDATE   = DATE(wsDate:SCREEN-VALUE)
                  dbgl.AMT = dbgl.AMT + (tmpTrans.Amount - tmpTrans.Vat).
       END.
       ELSE dbgl.AMT = dbgl.AMT + (tmpTrans.Amount).
       /*Creditors control ledger */
       FIND FIRST dbgl WHERE dbgl.acct = simctr.crdLedger no-error.
       IF NOT AVAILABLE dbgl THEN DO:
           CREATE dbgl.
           ASSIGN  dbgl.CREDATE = TODAY
                   dbgl.DESCRIP  = wsNar:SCREEN-VALUE IN FRAME frm-Input
                   dbgl.period   = INT(st-per:SCREEN-VALUE)
                   dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Input
                   dbgl.TransID  = wsTransId
                   dbgl.trDATE   = DATE(wsDate:SCREEN-VALUE)
                   dbgl.UID      = varUser
                   dbgl.acct     = simctr.crdLedger
                   dbgl.dept     = tmpTrans.Dept
                   dbgl.proj     = tmpTrans.Proj
                   dbgl.Fund     = tmpTrans.Fund
                   dbgl.SOURCE   = "CR".
       END.
       dbgl.AMT = dbgl.AMT + ((tmpTrans.Amount) * -1).
       IF tmpTrans.Vat <> 0 THEN DO: 
           /* VAT Claimable Provision */
           FIND FIRST dbgl WHERE dbgl.acct = simctr.vat[3] no-error.
           IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN  dbgl.CREDATE = TODAY
                       dbgl.DESCRIP  = wsNar:SCREEN-VALUE IN FRAME frm-Input
                       dbgl.period   = INT(st-per:SCREEN-VALUE)
                       dbgl.REF      = wsRef:SCREEN-VALUE IN FRAME frm-Input
                       dbgl.TransID  = wsTransId
                       dbgl.trDATE   = DATE(wsDate:SCREEN-VALUE)
                       dbgl.UID      = varUser
                       dbgl.acct     = simctr.vat[3]
                       dbgl.dept     = tmpTrans.Dept
                       dbgl.proj     = tmpTrans.Proj
                       dbgl.Fund     = tmpTrans.Fund
                       dbgl.SOURCE   = "CR"
                       dbgl.AMT = dbgl.AMT + (tmpTrans.Vat).
           END.
           ELSE dbgl.AMT = dbgl.AMT + (tmpTrans.Vat).
       END.
       DISPLAY STREAM a crdmf.NAME tmpTrans.Descrip tmpTrans.Ledger tmpTrans.amount
       WITH FRAME frm-CrdRpt1.
      DOWN STREAM a WITH FRAME frm-CrdRpt1.
      IF wsOrd <> 0 THEN DO:  /* update and mark Order line status */
          FIND FIRST ordtmf WHERE ordtmf.ordno = wsOrd AND ordtmf.lineSeq = tmpTrans.lineSeq NO-ERROR.
           ordtmf.invAmt = tmptrans.Amount.
      END.
      DELETE tmpTrans.
   END. /* eof each tmptrans */
   /* update and mark Order status */
   FIND FIRST ordmf WHERE ordmf.ordNo = wsOrd NO-ERROR.
   IF AVAILABLE ordmf THEN DO:
       ordmf.invAmt = wsAmt.
        ASSIGN ordmf.OrdStat = 2 WHEN ordmf.invAmt = ordmf.OrdAmt.
   END.
      
   DOWN 2 STREAM a WITH FRAME frm-CrdRpt1.
  {glcon.i}
   RELEASE crdmf.
   RELEASE crdtmf.
   RELEASE crdtaf.
   RELEASE GLTDF.
   RELEASE GLBAL.
   CLEAR FRAME frm-Input.
   ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
          wsDate:SCREEN-VALUE = STRING(TODAY).
   ENABLE ALL WITH FRAME frm-Input.
   APPLY 'entry' TO wsAcc. 
   RETURN.
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
