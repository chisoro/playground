/* Program.................stkEnq.p
   Notes:.................Stock Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsstkcode   AS CHAR FORM "x(12)".
DEF VAR wsDesc   LIKE stkmf.descrip.
DEF VAR wsCurA   AS DEC FORM "zzzzzzzz9.99-" EXTENT 13.
DEF VAR st-per   LIKE stktrans.period.
DEF VAR wsRef    LIKE stktrans.Ref.
DEF VAR wsId     LIKE stktrans.TransId.

DEF BUTTON btnStock  LABEL "STOCK".
DEF BUTTON btnTrans LABEL "View Transactions".
DEF BUTTON btnClose LABEL "CLOSE".
DEF BUTTON btnok    LABEL "OK".
DEF BUTTON btnNext   LABEL "Next Account>".
DEF BUTTON btnPrev   LABEL "<Prev Account".


DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 122 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 122 by 4.2.

DEF    QUERY qry-stkmf FOR stkmf SCROLLING.
DEF BROWSE brw-stkmf QUERY qry-stkmf
    DISPLAY stkmf.WareHse stkmf.stkCode stkmf.Descrip   
    WITH 22 DOWN SEPARATORS.

DEF    QUERY qry-trans FOR stktrans SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY stktrans.period stktrans.trDate stktrans.Ref stktrans.TranType
            stktrans.Descrip stktrans.Quantity stktrans.Cost stktrans.Ledger   
    WITH 22 DOWN SEPARATORS.

DEF    QUERY qry-Pick FOR stkmf SCROLLING.
DEF BROWSE brw-Pick QUERY qry-Pick
    DISPLAY stkmf.WareHse stkmf.stkCode stkmf.Descrip WITH 22 DOWN SEPARATORS.

 DEFINE FRAME frm-Pick 
    brw-Pick AT ROW 1 COL 1.5
    skip(0.5)
    btnOk COLON 8 SPACE (20)
    btnclose 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Stock Selection".

DEF FRAME frmEnquiry
     btnStock AT ROW 1.5 COL 5
     wsstkcode NO-LABEL  AUTO-RETURN stkmf.descrip NO-LABEL NO-TAB-STOP VIEW-AS TEXT 
     stkmf.Quantity[1]  LABEL "Balance: Quantity"  VIEW-AS TEXT 
     stkmf.measure  NO-LABEL VIEW-AS TEXT SKIP(0.2)
     stkmf.Cost[1] COLON 87 LABEL "        Value"   VIEW-AS TEXT SKIP(0.5)
     stkmf.UnitPrice COLON 87 LABEL "        Unit Cost"   VIEW-AS TEXT SKIP(0.5)
     brw-trans  AT ROW 5.5 COL 3
     btnNext   AT ROW 23.92 COL 10 SPACE(30) btnPrev SPACE(30) btnClose 
     rect-1     AT ROW 23.38 COL 3
     rect-2     AT ROW 1.27 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 126 BY 26 TITLE "STOCK ENQUIRIES".

ON CHOOSE OF btnStock IN FRAME frmEnquiry
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-pick FOR EACH stkmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btnok IN FRAME frm-pick 
          OR 'enter':u OF brw-pick
          OR 'mouse-select-dblclick' OF brw-pick.
  CLOSE QUERY qry-pick.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnStock.
  APPLY 'tab' TO wsstkcode.
  RETURN. 
END. 

ON CHOOSE OF btnOk IN FRAME frm-pick 
    OR 'enter':u OF brw-pick
    OR 'mouse-select-dblclick' OF brw-pick
DO: 
   GET CURRENT qry-pick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY stkmf.Stkcode @ wsstkcode stkmf.DESCRIP stkmf.Quantity[1] stkmf.Cost[1]
           stkmf.Measure stkmf.UnitPrice WITH FRAME frmEnquiry.
   APPLY 'TAB' TO wsstkcode IN FRAME frmEnquiry.
END.

ON 'tab' of wsstkcode IN FRAME frmEnquiry
    OR 'enter' OF wsstkcode IN FRAME frmEnquiry
DO:
   IF  wsstkcode:SCREEN-VALUE = "" THEN
       APPLY "close" TO THIS-PROCEDURE IN FRAME frmEnquiry.
    ELSE DO:
        ASSIGN wsstkcode = wsstkcode:SCREEN-VALUE. 
        FIND FIRST stkmf WHERE stkmf.stkcode = wsstkcode:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE stkmf THEN DO:
            MESSAGE "Invalid Stock code entered" VIEW-AS ALERT-BOX.
            CLEAR FRAME frmEnquiry ALL.
            CLOSE QUERY qry-stkmf.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY stkmf.Stkcode @ wsstkcode stkmf.DESCRIP stkmf.Quantity[1] stkmf.Cost[1]
                    stkmf.Measure stkmf.UnitPrice WITH FRAME frmEnquiry.
           OPEN QUERY qry-trans FOR EACH stktrans  WHERE stktrans.stkcode = stkmf.Stkcode NO-LOCK 
               BY stktrans.period DESCENDING BY stktrans.trDate DESCENDING . 
        END.   
    END.
END.

ON CHOOSE OF btnNext IN FRAME frmEnquiry
DO:
    FIND NEXT stkmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE stkmf THEN
        FIND LAST stkmf NO-LOCK NO-ERROR.
    wsstkcode:SCREEN-VALUE = STRING(stkmf.Stkcode).
    DISPLAY stkmf.Stkcode @ wsstkcode stkmf.DESCRIP stkmf.Quantity[1] stkmf.Cost[1]
           stkmf.Measure stkmf.UnitPrice WITH FRAME frmEnquiry.
    OPEN QUERY qry-trans FOR EACH stktrans  WHERE stktrans.stkcode = stkmf.Stkcode
         NO-LOCK BY stktrans.period DESCENDING BY stktrans.trDate DESCENDING. 
END.

ON CHOOSE OF btnPrev IN FRAME frmEnquiry
DO:
   FIND PREV stkmf NO-LOCK NO-ERROR.
    IF NOT AVAILABLE stkmf THEN
        FIND FIRST stkmf NO-LOCK NO-ERROR.
    wsstkcode:SCREEN-VALUE = STRING(stkmf.Stkcode).
    DISPLAY stkmf.Stkcode @ wsstkcode stkmf.DESCRIP stkmf.Quantity[1] stkmf.Cost[1]
           stkmf.Measure stkmf.UnitPrice WITH FRAME frmEnquiry.
    OPEN QUERY qry-trans FOR EACH stktrans  WHERE stktrans.stkcode = wsstkcode:SCREEN-VALUE
         NO-LOCK BY stktrans.period DESCENDING BY stktrans.trDate DESCENDING. 
END.
/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frmEnquiry.
APPLY 'entry' TO wsstkcode IN FRAME frmEnquiry.
WAIT-FOR CHOOSE OF btnclose OR CLOSE of THIS-PROCEDURE IN FRAME frmEnquiry.
RELEASE stkmf.
RELEASE stktrans.
HIDE FRAME frmEnquiry.
