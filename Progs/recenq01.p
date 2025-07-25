/* Program.................dbEnq01.p
   Notes:................. Posted Receipt Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM b.
DEF VAR wsDate LIKE dbrecH.recDate EXTENT 2.
DEF VAR wsop   LIKE dbrecH.opcode EXTENT 2.
DEF VAR wsRef LIKE dbrecH.recNo.
DEF VAR wsBank LIKE dbPay.descrip.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR wsledger AS DEC.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zzz,zz9.99-".
DEF VAR wsFile AS CHAR FORM "X(40)".

DEF BUTTON  btn-Ok LABEL "OK".
DEF BUTTON  btnClose LABEL "CLOSE".
DEF BUTTON btnNext   LABEL "Next>".
DEF BUTTON btnPrev   LABEL "<Prev".
DEF BUTTON btnExp    LABEL "EXPORT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 99 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 99 by 17.


DEF    QUERY qry-trans FOR dbrecH SCROLLING. 
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY dbrecH.RecDate COLUMN-LABEL "DATE" dbRecH.OpCode COLUMN-LABEL "CASHIER" dbrecH.RecNo COLUMN-LABEL "REC" dbRecH.SeqNo COLUMN-LABEL "SEQ" 
            dbRecH.Ref COLUMN-LABEL "REFERANCE" dbRecH.rCode COLUMN-LABEL "CODE" 
            wsbank COLUMN-LABEL "BANK" dbRecH.Account COLUMN-LABEL "ACCOUNT" 
            dbRecH.Descrip COLUMN-LABEL "DESCRIPTION" dbRecH.Amount COLUMN-LABEL "AMOUNT" 
            dbrecH.txtCur COLUMN-LABEL "CURRENCY" dbRecH.decRate COLUMN-LABEL "RATE" dbRecH.RecStat COLUMN-LABEL "STATUS"
    WITH 20 DOWN SEPARATORS.

DEFINE FRAME frmMain
    skip(0.5)
    wsDate[1]       colon 30 LABEL "RECEIPTS DATE: FROM" 
    wsDate[2]       COLON 60  LABEL "TO:" SKIP(0.5)
    wsop[1]         COLON 30 LABEL "OPERATOR FROM"
    wsOp[2]         COLON 60 LABEL "TO:" SKIP(0.5)
    /*wsRef       COLON 20 LABEL "Receipt Number" SPACE(60)
    wsAmt               LABEL "Receipt Total" VIEW-AS TEXT */
    skip(1.5)
    brw-trans  AT ROW 6 COL 5
    SKIP(1.5)
    btnclose  COLON 20 SPACE(80) btnexp
    SKIP(1)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "BROWSE POSTED RECEIPT".


/******** Triggers ***********/

ON CHOOSE OF btnexp IN FRAME frmMain 
DO:
    IF CAN-FIND (FIRST dbrecH WHERE dbRecH.RecDate >= DATE(wsDate[1]:SCREEN-VALUE)  AND dbRecH.RecDate <= DATE(wsDate[2]:SCREEN-VALUE)
                              AND dbRecH.opcode >= wsop[1]:SCREEN-VALUE  AND dbRecH.opcode <= wsop[2]:SCREEN-VALUE) THEN DO:
         CLOSE QUERY qry-trans.
         wsFile = TRIM(simctr.repDir) + "Rec" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + ".csv".
         OUTPUT STREAM b TO VALUE(wsFile).
         EXPORT STREAM b DELIMITER ',' "DATE" "CASHIER" "REC" "SEQ" "TYPE" "ACCOUNT" "DESCRIPTION" "INCOME" "LEDGER" "REFERENCE" "AMOUNT"
                         "CURRENCY" "RATE" "VAT%" "PAY CODE" "STATUS".
         EXPORT STREAM b DELIMITER ',' "".
         FOR EACH dbrecH WHERE dbRecH.RecDate >= DATE(wsDate[1]:SCREEN-VALUE)  AND dbRecH.RecDate <= DATE(wsDate[2]:SCREEN-VALUE)
                                 AND dbRecH.opcode >= wsop[1]:SCREEN-VALUE  AND dbRecH.opcode <= wsop[2]:SCREEN-VALUE
                               NO-LOCK BY dbrecH.recDate BY dbrecH.opCode BY dbrecH.RecNo BY dbrecH.seq:
             wsledger = 0.
             FIND FIRST dbrcod WHERE dbrcod.rcode = dbrech.rcode NO-LOCK NO-ERROR.
             IF dbrcod.TARGET = "V" THEN
                 wsledger = dbrcod.Ledger.
             ELSE IF dbrcod.TARGET = "C" THEN DO:
                 FIND FIRST dbsgr WHERE dbsgr.sgrp = dbrcod.sgrp NO-LOCK NO-ERROR.
                 IF AVAILABLE dbsgr THEN
                      wsledger = dbsgr.ctrLedger.
             END.
             ELSE IF dbrcod.TARGET = "L" THEN DO:
                 FIND FIRST hsesch NO-LOCK NO-ERROR.
                 IF AVAILABLE hsesch THEN
                      wsledger = hsesch.CtrLedger.
             END.  
             EXPORT STREAM b DELIMITER ',' dbRecH.RecDate dbRecH.OpCode dbRecH.RecNo dbRecH.SeqNo dbRecH.contype dbRecH.Account dbRecH.Descrip
                dbRecH.rCode wsledger dbRecH.Ref dbRecH.Amount dbRecH.txtCur dbRecH.decRate dbrcod.vat% dbRecH.Paytype dbRecH.RecStat.
         END.
         OUTPUT STREAM b CLOSE.
         OS-COMMAND NO-WAIT VALUE(wsFile).
         APPLY 'TAB' TO wsOp[2] IN FRAME frmMain. 
    END.
    RETURN. 
END.

ON 'enter':U OF wsOp[2] IN  FRAME frmMain
   OR  'TAB':U OF wsOp[2] IN  FRAME frmMain
DO:
   OPEN QUERY qry-trans 
           FOR EACH dbrecH WHERE dbRecH.RecDate >= DATE(wsDate[1]:SCREEN-VALUE)  AND dbRecH.RecDate <= DATE(wsDate[2]:SCREEN-VALUE)
                             AND dbRecH.opcode >= wsop[1]:SCREEN-VALUE  AND dbRecH.opcode <= wsop[2]:SCREEN-VALUE
                           NO-LOCK BY dbrecH.recDate BY dbrecH.opCode BY dbrecH.RecNo BY dbrecH.seq.
    RETURN.
END.

ON row-display OF brw-Trans DO:
    FIND FIRST dbPay WHERE dbPay.Paytype = dbRecH.Paytype NO-LOCK NO-ERROR.
    FIND FIRST cbkmf WHERE   cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
    wsBank = cbkmf.descrip.
END.


ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.
   
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ENABLE ALL  WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btnClose OR close of THIS-PROCEDURE IN FRAME frmMain.
HIDE FRAME frmMain.
