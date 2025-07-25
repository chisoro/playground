/* Program.................dbrcol.p
   Notes:................. Collection/VAT report
   Author:.................S. Mawire
*/

&SCOPED-DEFINE pgorientation          "PORTRAIT"

SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.

DEF NEW SHARED VAR w-orientation AS CHAR  NO-UNDO.
DEF VAR wsDate LIKE dbrecH.recDate EXTENT 2.
DEF VAR wsop   LIKE dbrecH.opcode EXTENT 2.
DEF VAR wsRef LIKE dbrecH.recNo.
DEF VAR wsBank LIKE dbPay.descrip.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zzz,zz9.99-" EXTENT 2.
DEF VAR wsFile AS CHAR FORM "X(40)".
DEF VAR wsOpt AS INT.
DEF VAR wsTitle AS CHAR FORM "x(70)".

DEF BUTTON  btn-Ok LABEL "OK".
DEF BUTTON  btnClose LABEL "CLOSE".
DEF BUTTON btnExp    LABEL "EXPORT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 70 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 70 by 6.


FORM
     tblForex.txtCur  LABEL "CURRENCY" AT 20
     wsAmt[1]         LABEL "AMOUNT"
     wsAmt[2]         LABEL "VAT"
    HEADER
     SKIP(3) wsTitle AT 30
     skip(2) "       COLLECTIONS WITH VAT REPORT FOR PERIOD: " wsDate[1] "  TO " wsDate[2] 
    "Page: " AT 76 PAGE-NUMBER(a) SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

DEFINE FRAME frmMain
    skip(2.5)
    wsDate[1]       colon 25 LABEL "RECEIPTS DATE: FROM" 
    wsDate[2]       COLON 50  LABEL "TO:" SKIP(0.5)
    wsop[1]         COLON 25 LABEL "OPERATOR FROM"
    wsOp[2]         COLON 50 LABEL "TO:" SKIP(0.5)
    SKIP(1.8)
    btn-ok  COLON 10 SPACE(40) btnclose
    rect-2 AT ROW 1.5 COL 3
    rect-1 AT ROW 7.8 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "COLLECTIONS WITH VAT REPORT".


/******** Triggers ***********/
ON 'choose':U OF btn-ok IN FRAME frmMain 
DO:
    ASSIGN wsdate wsop.
    {PrintOpt.i &stream-name="stream a"
                        &print-prog="rpt-ip"
                        &paged}
    RETURN.
END.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.
   
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
wstitle = simctr.coName.
ENABLE ALL  WITH FRAME frmMain.
ASSIGN wsop[1]:SCREEN-VALUE = "1"
       wsop[2]:SCREEN-VALUE = "zzz".
WAIT-FOR CHOOSE OF btn-ok  OR CHOOSE OF btnClose OR close of THIS-PROCEDURE IN FRAME frmMain.
HIDE FRAME frmMain.

PROCEDURE rpt-ip:
    FOR EACH tblforex:
        wsAmt = 0.
        FOR EACH dbrech WHERE dbRecH.RecDate >= wsDate[1] AND dbRecH.RecDate <= wsDate[2] AND
            dbRecH.OpCode >= wsOp[1] AND  dbRecH.OpCode <= wsOp[2] AND dbRecH.RecStat <> "C" AND dbrech.txtCur = tblforex.txtCur NO-LOCK:
            FIND FIRST dbrcod WHERE dbrcod.rcode = dbrech.rcode NO-LOCK NO-ERROR.
            ASSIGN wsAmt[1] = wsAmt[1] + dbrech.amount.
            IF dbrcod.TARGET = "C" THEN DO:
                FIND FIRST dbblf WHERE dbblf.dbacc = dbrech.account AND dbblf.sgrp = dbrcod.sgrp NO-LOCK NO-ERROR.
                IF AVAILABLE dbblf AND dbblf.dbamt[1] <>  0 THEN
                   wsAmt[2] = wsAmt[2] + ROUND((dbrech.amount * (dbblf.vat / dbblf.dbamt[1]) ),2).
                ELSE 
                    wsAmt[2] = wsAmt[2] + ROUND((dbrech.amount * vat% / (vat% + 100)),2).
            END.
            ELSE
                wsAmt[2] = wsAmt[2] + ROUND((dbrech.amount * vat% / (vat% + 100)),2).
        END.
        DISPLAY STREAM a tblforex.txtCur wsAmt[1] wsAmt[2] WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
END PROCEDURE.
