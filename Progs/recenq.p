/* Program.................dbEnq.p
   Notes:................. Account Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsDate LIKE dbrecH.recDate.
DEF VAR wsRef LIKE dbrecH.recNo.
DEF VAR wsBank LIKE dbPay.descrip.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zzz,zz9.99-".
define variable hCol as widget-handle.
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON  btn-Ok LABEL "OK".
DEF BUTTON  btnClose LABEL "CLOSE".
DEF BUTTON btnNext   LABEL "Next>".
DEF BUTTON btnPrev   LABEL "<Prev".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 99 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 99 by 17.


DEF    QUERY qry-trans FOR dbrecH SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY dbRecH.OpCode COLUMN-LABEL "CASHIER" dbRecH.SeqNo COLUMN-LABEL "SEQ" 
            dbRecH.Ref COLUMN-LABEL "REFERANCE" dbRecH.rCode COLUMN-LABEL "CODE" 
            wsbank COLUMN-LABEL "BANK" dbRecH.Account COLUMN-LABEL "ACCOUNT" 
            dbRecH.Descrip COLUMN-LABEL "DESCRIPTION" dbRecH.Amount COLUMN-LABEL "AMOUNT" dbRecH.RecStat COLUMN-LABEL "STATUS"
    WITH 10 DOWN SEPARATORS.

DEFINE FRAME frmMain
    skip(2)
    wsDate       colon 20 LABEL "Receipt Date"  SKIP(0.5)
    wsRef       COLON 20 LABEL "Receipt Number" SPACE(60)
    wsAmt               LABEL "Receipt Total" VIEW-AS TEXT
    skip(1.5)
    brw-trans  AT ROW 6 COL 5
    SKIP(1.5)
    btnPrev COLON 10 SPACE(40)
     btnNext SPACE(40)
    btnclose SKIP(1)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "SEARCH POSTED RECEIPT".


/******** Triggers ***********/
ON  'enter':u OF wsRef IN FRAME  frmMain
     OR 'tab':u OF wsRef IN FRAME  frmMain
DO:
   FIND FIRST dbrecH WHERE dbRecH.recNo = DEC(wsRef:SCREEN-VALUE) AND dbRecH.RecDate = DATE(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbrecH THEN DO:
      MESSAGE "Receipt Could not be found please try again......." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO:
       OPEN QUERY qry-trans 
           FOR EACH dbrecH WHERE dbRecH.recNo = DEC(wsRef:SCREEN-VALUE) 
                        AND dbRecH.RecDate = DATE(wsDate:SCREEN-VALUE) NO-LOCK.
   END.
   RETURN.    
END.

ON row-display OF brw-Trans DO:
    FIND FIRST dbPay WHERE dbPay.Paytype = dbRecH.Paytype NO-LOCK NO-ERROR.
    FIND FIRST cbkmf WHERE   cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
    wsBank = cbkmf.descrip.
    wsAmt = wsAmt + dbRecH.Amount.
    DISPLAY wsAmt WITH FRAME frmMain.
END.

ON CHOOSE OF btnPrev IN FRAME frmMain
DO:
    wsAmt = 0.
    ASSIGN wsRef:SCREEN-VALUE = STRING(DEC(wsRef:SCREEN-VALUE) - 1) 
        WHEN DEC(wsRef:SCREEN-VALUE) <> 0 .
    OPEN QUERY qry-trans 
           FOR EACH dbrecH WHERE dbRecH.recNo = DEC(wsRef:SCREEN-VALUE) 
                        AND dbRecH.RecDate = DATE(wsDate:SCREEN-VALUE) NO-LOCK.
END.

ON CHOOSE OF btnNext IN FRAME frmMain
DO:
    wsAmt = 0.
    ASSIGN wsRef:SCREEN-VALUE = STRING(DEC(wsRef:SCREEN-VALUE) + 1) 
        WHEN DEC(wsRef:SCREEN-VALUE) <> 0 .
    OPEN QUERY qry-trans 
           FOR EACH dbrecH WHERE dbRecH.recNo = DEC(wsRef:SCREEN-VALUE) 
                        AND dbRecH.RecDate = DATE(wsDate:SCREEN-VALUE) NO-LOCK.
    
END.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.
   
/********** MAIN LOGIC **********/
ENABLE ALL  WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btnClose OR close of THIS-PROCEDURE IN FRAME frmMain.
HIDE FRAME frmMain.
