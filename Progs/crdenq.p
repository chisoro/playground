/* Program.................CrdEnq.p
   Notes:................. Supplier Account Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsAcc   AS CHAR FORM "x(12)".
DEF VAR wsDesc   LIKE crdmf.Name.
DEF VAR wsCurA   AS DEC FORM "zzzzzzzz9.99-" EXTENT 13.
DEF VAR st-per   LIKE CRDTMF.Period.
DEF VAR wsRef    LIKE CRDTMF.Ref.
DEF VAR wsId     LIKE CRDTMF.TransId.

DEF BUTTON BtnAcc  LABEL "ACCOUNT".
DEF BUTTON btnTrans LABEL "ALLOCATIONS".
DEF BUTTON btnClose LABEL "CLOSE".
DEF BUTTON btnok    LABEL "OK".
DEF BUTTON btn-Next  LABEL "NEXT".
DEF BUTTON btn-Prev  LABEL "PREVIOUS".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 127.5 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 127.5 by 3.2.

DEF    QUERY qry-crdtmf FOR crdtmf SCROLLING.
DEF BROWSE brw-crdtmf QUERY qry-crdtmf
    DISPLAY Period LABEL "PERIOD" 
            TrDate LABEL "DATE" 
            TrType LABEL "TYPE" 
            Ref    LABEL "REFERENCE"
            Descrip LABEL "DESCRIPTION" 
            Amt     LABEL "INVOICE AMOUNT" 
            VAT     LABEL "VAT"
            decBal  LABEL "INVOICE BALANCE" FORM "zzz,zzz,zz9.99-"
    WITH NO-LABEL 22 DOWN SEPARATORS.

DEF    QUERY qry-alloc FOR crdtaf SCROLLING.
DEF BROWSE brw-alloc QUERY qry-alloc      
    DISPLAY crdtaf.period LABEL "PERIOD" 
            crdtaf.trDate LABEL "DATE" 
            crdtaf.Descrip LABEL "DESCRIPTION" 
            crdtaf.amount  LABEL "AMOUNT" 
            crdtaf.VAT     LABEL "VAT"
            crdtaf.Ledger   LABEL "LEDGER"
            crdtaf.TransID LABEL "TRANSACTION" 
    WITH NO-LABEL 8 DOWN SEPARATORS.

DEF    QUERY qry-Pick FOR crdmf SCROLLING.
DEF BROWSE brw-Pick QUERY qry-Pick
    DISPLAY crdmf.acc crdmf.Name WIDTH 40 town WITH 20 DOWN SEPARATORS.

 DEFINE FRAME frm-Pick 
    brw-Pick AT ROW 1 COL 1.5
    skip(0.5)
    btnOk COLON 8 SPACE (20)
    btnclose 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Supplier Selection".

DEFINE FRAME frm-Alloc 
    brw-Alloc AT ROW 1 COL 5
    skip(0.5)
    btnclose COLON 40 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "INVOICE ALLOCATION DETAILS".

DEF FRAME frmEnquiry
     BtnAcc AT ROW 1.5 COL 5  SPACE(.5)
     wsAcc NO-LABEL  AUTO-RETURN SPACE(20)
     st-per LABEL "Start From Period"   AUTO-RETURN 
     Crdmf.YtdAmt LABEL "YTD AMOUNT" NO-TAB-STOP SKIP(0.5) 
     crdmf.Name COLON 5 NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 by 1
     SPACE(10)
     crdmf.Bal   LABEL "Account Balance"  VIEW-AS FILL-IN NO-TAB-STOP
     crdmf.vatAmt LABEL "VAT"              VIEW-AS FILL-IN NO-TAB-STOP
     brw-crdtmf  AT ROW 5 COL 3
     btn-Next    AT ROW 23.8 COL 10 SPACE(20) btn-Prev space(20) btnTrans SPACE(20) btnClose
     rect-1      AT ROW 23.0 COL 3
     rect-2      AT ROW 1.27 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 135 BY 26 TITLE "SUPPLIER ACCOUNT ENQUIRIES".

ON CHOOSE OF BtnAcc IN FRAME frmEnquiry
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-pick FOR EACH crdmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btnok IN FRAME frm-pick 
          OR 'enter':u OF brw-pick
          OR 'mouse-select-dblclick' OF brw-pick.
  CLOSE QUERY qry-pick.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO BtnAcc.
  APPLY 'tab' TO wsAcc.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-pick 
    OR 'enter':u OF brw-pick
    OR 'mouse-select-dblclick' OF brw-pick
DO: 
   GET CURRENT qry-pick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY crdmf.ACC @ wsAcc crdmf.Name crdmf.Bal crdmf.vatAmt Crdmf.YtdAmt WITH FRAME frmEnquiry.
   APPLY 'TAB' TO wsAcc IN FRAME frmEnquiry.
END.

ON CHOOSE OF btn-next IN FRAME frmEnquiry
DO:   
    FIND NEXT crdmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE  crdmf THEN
        FIND LAST  crdmf NO-LOCK NO-ERROR.
    wsAcc:SCREEN-VALUE = STRING(crdmf.acc).
    DISPLAY crdmf.ACC @ wsAcc crdmf.Name crdmf.Bal crdmf.vatAmt Crdmf.YtdAmt WITH FRAME frmEnquiry.
    OPEN QUERY qry-crdtmf FOR EACH crdtmf  WHERE crdtmf.acc = INT(wsAcc:SCREEN-VALUE)
      AND crdtmf.period >= INT(st-per:SCREEN-VALUE) NO-LOCK
      BY crdtmf.period BY crdtmf.TransID .
    APPLY 'TAB' TO wsAcc IN FRAME frmEnquiry.
    RETURN.
END.

ON CHOOSE OF btn-Prev IN FRAME frmEnquiry
DO:
    FIND PREV crdmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE  crdmf THEN
        FIND FIRST crdmf NO-LOCK NO-ERROR.
    wsAcc:SCREEN-VALUE = STRING( crdmf.acc).
    DISPLAY crdmf.ACC @ wsAcc crdmf.Name crdmf.Bal crdmf.vatAmt Crdmf.YtdAmt WITH FRAME frmEnquiry.
    OPEN QUERY qry-crdtmf FOR EACH crdtmf  WHERE crdtmf.acc = INT(wsAcc:SCREEN-VALUE)
      AND crdtmf.period >= INT(st-per:SCREEN-VALUE) NO-LOCK
      BY crdtmf.period BY crdtmf.TransID .
    APPLY 'TAB' TO wsAcc IN FRAME frmEnquiry.
    RETURN.
END.

ON 'tab' of wsAcc IN FRAME frmEnquiry
    OR 'enter' OF wsAcc IN FRAME frmEnquiry
DO:
   IF  INT(wsAcc:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frmEnquiry.
    ELSE DO:
        ASSIGN wsAcc = wsAcc:SCREEN-VALUE. 
        FIND FIRST crdmf WHERE crdmf.ACC = int(wsAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL crdmf THEN DO:
            MESSAGE "Invalid Account number entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frmEnquiry ALL.
            CLOSE QUERY qry-pick.
            RETURN NO-APPLY.
        END.
        ELSE
         DISPLAY crdmf.Acc @ wsAcc crdmf.Name crdmf.bal crdmf.vatAmt Crdmf.YtdAmt WITH FRAME frmEnquiry.
    END.
END.

ON 'tab' OF st-Per IN FRAME frmEnquiry
    OR 'enter' OF st-Per IN FRAME frmEnquiry
DO:
  OPEN QUERY qry-crdtmf FOR EACH crdtmf  WHERE crdtmf.acc = INT(wsAcc:SCREEN-VALUE)
      AND crdtmf.period >= INT(st-per:SCREEN-VALUE) NO-LOCK
      BY crdtmf.period BY crdtmf.TransID .  
END.

ON 'CHOOSE' OF btnTrans IN FRAME frmEnquiry
DO:
  GET CURRENT qry-crdtmf NO-LOCK NO-WAIT.
  VIEW FRAME frm-alloc.
  ENABLE ALL WITH FRAME frm-alloc.
  OPEN QUERY qry-Alloc FOR EACH crdtaf  WHERE crdtaf.Transid = crdtmf.Transid NO-LOCK.
  WAIT-FOR CHOOSE OF btnclose OR CLOSE OF THIS-PROCEDURE IN FRAME frm-alloc.
  CLOSE QUERY qry-alloc.
  HIDE FRAME frm-alloc.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr.
st-per:SCREEN-VALUE = STRING(simctr.curper).
ENABLE ALL WITH FRAME frmEnquiry.
APPLY 'entry' TO wsAcc IN FRAME frmEnquiry.
WAIT-FOR CHOOSE OF btnclose OR CLOSE of THIS-PROCEDURE IN FRAME frmEnquiry.
RELEASE crdmf.
RELEASE crdtmf.
HIDE FRAME frmEnquiry.
