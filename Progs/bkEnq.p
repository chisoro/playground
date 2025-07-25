/* Program.................BkEnq.p
   Notes:................. Bank Account Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsBank   AS CHAR FORM "x(12)".
DEF VAR wsDesc   LIKE cbkmf.descrip.
DEF VAR wsCurA   AS DEC FORM "zzzzzzzz9.99-" EXTENT 13.
DEF VAR st-per   LIKE CBTrans.Accper.
DEF VAR wsRef    LIKE cbTrans.Ref.
DEF VAR wsId     LIKE cbTrans.TransId.

DEF BUTTON btnBank  LABEL "Bank".
DEF BUTTON btnTrans LABEL "View Transactions".
DEF BUTTON btnClose LABEL "CLOSE".
DEF BUTTON btnok    LABEL "OK".

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 117 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 117 by 3.2.

DEF    QUERY qry-cbkmf FOR CBTrans SCROLLING.
DEF BROWSE brw-cbkmf QUERY qry-cbkmf
    DISPLAY Accper trDate TranType Ref Descrip Ledger amount FORM "zzzzzzzzzzz9.99-" StatNo COLUMN-LABEL "StatNo"  
    WITH 22 DOWN SEPARATORS.

DEF    QUERY qry-trans FOR CBTrans SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY  Accper trDate TranType Ref Seq Descrip Ledger amount   
    WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-trans 
    brw-trans AT ROW 2 COL 5
    skip(0.5)
    btnclose COLON 60
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Transaction Listing".

DEF    QUERY qry-Pick FOR cbkmf SCROLLING.
DEF BROWSE brw-Pick QUERY qry-Pick
    DISPLAY cbkmf.bank cbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

 DEFINE FRAME frm-Pick 
    brw-Pick AT ROW 1 COL 1.5
    skip(0.5)
    btnOk COLON 8 SPACE (20)
    btnclose 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Bank Selection".

DEF FRAME frmEnquiry
     btnBank AT ROW 1.5 COL 5  SPACE(.5)
     wsBank NO-LABEL  AUTO-RETURN SPACE(30)
     st-per LABEL "Start From Period"   AUTO-RETURN SKIP(0.5) 
     cbkmf.descrip COLON 10 NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 by 1
    SPACE(10)
    cbkmf.Bal LABEL "Bank Balance"  FORM "zzz,zzz,zzz,zz9.99-" VIEW-AS FILL-IN NO-TAB-STOP 
    cbkmf.txtCur NO-LABEL VIEW-AS TEXT FORM "!!!"
     brw-cbkmf    AT ROW 5 COL 3
     btnTrans    AT ROW 23.5 COL 20 SPACE(42) 
     btnClose 
     rect-1      AT ROW 23.0 COL 3
     rect-2     AT ROW 1.27 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 122 BY 26 TITLE "BANK ENQUIRIES".

ON CHOOSE OF btnBank IN FRAME frmEnquiry
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-pick FOR EACH cbkmf WHERE cbkmf.bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btnok IN FRAME frm-pick 
          OR 'enter':u OF brw-pick
          OR 'mouse-select-dblclick' OF brw-pick.
  CLOSE QUERY qry-pick.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnBank.
  APPLY 'tab' TO wsBank.
  RETURN. 
END. 

ON CHOOSE OF btnok IN FRAME frm-pick 
    OR 'enter':u OF brw-pick
    OR 'mouse-select-dblclick' OF brw-pick
DO: 
   GET CURRENT qry-pick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY cbkmf.Bank @ wsBank cbkmf.DESCRIP cbkmf.Bal cbkmf.txtCur WITH FRAME frmEnquiry.
   APPLY 'TAB' TO wsBank IN FRAME frmEnquiry.
END.

ON 'tab' of wsBank IN FRAME frmEnquiry
    OR 'enter' OF wsBank IN FRAME frmEnquiry
DO:
   IF  INT(wsbank:SCREEN-VALUE) = 0 THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frmEnquiry.
    ELSE DO:
        ASSIGN wsBank = wsBank:SCREEN-VALUE. 
        FIND FIRST cbkmf WHERE cbkmf.bank = int(wsBank:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAIL cbkmf THEN DO:
            MESSAGE "Invalid Bank number entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frmEnquiry ALL.
            CLOSE QUERY qry-cbkmf.
            RETURN NO-APPLY.
        END.
        ELSE
         DISPLAY cbkmf.Bank @ wsBank cbkmf.DESCRIP cbkmf.bal cbkmf.txtCur WITH FRAME frmEnquiry.
    END.
END.

ON 'tab' OF st-Per IN FRAME frmEnquiry
    OR 'enter' OF st-Per IN FRAME frmEnquiry
DO:
  OPEN QUERY qry-cbkmf FOR EACH CBTrans  WHERE CBTrans.bank = INT(wsbank:SCREEN-VALUE)
      AND CBTrans.Accper >= INT(st-per:SCREEN-VALUE) AND cbTrans.seq = 0 NO-LOCK 
      BY CBTrans.Accper BY CBTrans.trDate BY CBTrans.Transid.  
END.


ON CHOOSE OF btnTrans IN FRAME frmEnquiry
DO:
  GET CURRENT qry-cbkmf NO-WAIT.
  ASSIGN wsId = cbTrans.Transid
         wsRef = cbTrans.Ref.
  OPEN QUERY qry-Trans FOR EACH cbTrans WHERE cbTrans.Transid = wsId 
         AND cbTrans.seq <> 0 AND cbTrans.Ref = wsRef NO-LOCK.
  ENABLE ALL WITH FRAME frm-Trans.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-Trans 
          OR close of THIS-PROCEDURE IN FRAME frm-Trans.
  CLOSE QUERY qry-Trans.
  HIDE FRAME frm-Trans.
  RETURN.
END.

ON ROW-DISPLAY OF brw-cbkmf
DO:
    IF cbtrans.amount < 0  THEN
    ASSIGN cbtrans.Amount:FGCOLOR IN BROWSE brw-cbkmf = 12. /* assign red colour */
END.

ON ROW-DISPLAY OF brw-trans
DO:
    IF cbtrans.amount < 0  THEN
    ASSIGN cbtrans.Amount:FGCOLOR IN BROWSE brw-trans = 12. /* assign red colour */
END.
/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frmEnquiry.
APPLY 'entry' TO wsBank IN FRAME frmEnquiry.
WAIT-FOR CHOOSE OF btnclose OR CLOSE of THIS-PROCEDURE IN FRAME frmEnquiry.
RELEASE cbkmf.
RELEASE cbTrans.
HIDE FRAME frmEnquiry.
