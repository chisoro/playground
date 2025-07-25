/* Program.................recrpt.p
   Notes:................. Receipts Report
   Author:.................S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM b.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsDate AS DATE.
DEF VAR wsFile AS CHAR FORM "x(40)".    
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR wsTitle1 AS CHAR FORM "x(80)" INITIAL "DETAILED RECEIPT ANALYSIS REPORT FOR THE DATE ".
DEF VAR wsTotal AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsCode LIKE dbRec.rCode.
DEF VAR wsCur LIKE dbRec.txtCur.
DEF VAR swCashier AS CHAR FORM "x(10)".
DEF VAR X AS INT.


DEF TEMP-TABLE tblRec
    FIELD varOp   LIKE dbrec.opcode
    FIELD varType LIKE dbrec.paytype
    FIELD varrCod LIKE dbrec.rcode
    FIELD varAmt  LIKE dbrec.amount
    FIELD varCur  LIKE dbrec.txtCur.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "PRINT".
DEF BUTTON btn-exp LABEL "EXPORT".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 5.5.

DEF FRAME frm-main
    SKIP(1)
    wsDate    LABEL "Receipt Date" COLON 15 SKIP(1)
    dbRec.recno LABEL "Processing..." COLON 15 VIEW-AS TEXT
    btn-ok AT ROW 7.7 COL 10  SPACE(10)
    btn-exp SPACE(10)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 7 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 65 BY 10.2
    TITLE "RECEIPT REPORT" VIEW-AS DIALOG-BOX KEEP-TAB-ORDER.

FORM 
    dbPay.descrip COLUMN-LABEL "METHOD"
    dbRCod.rCode COLUMN-LABEL "ITEM"
    dbRCod.descrip COLUMN-LABEL "DESCRIPTION"
    wsTotal COLUMN-LABEL "TOTAL"
    dbRec.txtCur COLUMN-LABEL "CURRENCY"    
    HEADER SKIP(1) wsTitle AT 20 skip(1) wsTitle1 AT 10 SPACE(2)
    "Page: " AT 90 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX WIDTH 132 FRAME frm-rpt.
      

FORM 
    swCashier COLUMN-LABEL ""
    dbPay.descrip COLUMN-LABEL "METHOD"
    dbRCod.rCode COLUMN-LABEL "ITEM"
    dbRCod.descrip COLUMN-LABEL "DESCRIPTION"
    wsTotal COLUMN-LABEL "TOTAL"
    dbRec.txtCur COLUMN-LABEL "CURRENCY"    
    HEADER SKIP(1) wsTitle AT 20 skip(1) wsTitle1 AT 10 SPACE(2) "Page: " AT 90 PAGE-NUMBER(a) SKIP(1) 
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX WIDTH 164 FRAME frm-rpt1.


ON 'choose':U OF btn-OK  
DO:
   session:set-wait-state("").
   wsDate =date(wsDate:SCREEN-VALUE).
   wsTitle1 = wsTitle1 + STRING(wsDate).
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Rep-ip"
                    &paged}
    
END.

ON 'choose':U OF btn-Exp  
DO:
   session:set-wait-state("").
   wsDate =date(wsDate:SCREEN-VALUE).
   RUN rec-exp.
    
END.

/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
FIND FIRST simctr.
ASSIGN 
       wsTitle = simctr.CONAME.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

PROCEDURE rep-ip:
    DISPLAY STREAM a  "ALL CASHIERS" @ dbPay.descrip WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt1.
    FOR EACH dbrec WHERE dbRec.recDate = wsDate AND RecStat = "" BREAK BY dbRec.PayType BY dbrec.rcode:
       IF FIRST-OF(dbrec.paytype)  THEN
        DO:
            FIND FIRST dbpay WHERE dbpay.paytype = dbrec.paytype NO-LOCK NO-ERROR.
            DISPLAY STREAM a dbPay.descrip WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        ACCUMULATE dbrec.Amount (SUB-TOTAL BY dbRec.PayType).
        ACCUMULATE dbrec.Amount (SUB-TOTAL BY dbrec.rcode).
        IF LAST-OF(dbrec.rcode) THEN DO:
            FIND FIRST dbrcod WHERE dbrcod.rcode = dbrec.rcode NO-LOCK NO-ERROR.
            DISPLAY STREAM a dbRCod.rCode dbRCod.descrip 
                ACCUM SUB-TOTAL BY dbrec.rcode (dbrec.amount) @ wsTotal dbRec.txtCur WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        IF LAST-OF(dbRec.PayType) THEN DO:
            UNDERLINE STREAM  a wsTotal WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
            DISPLAY STREAM a "SUB-TOTAL" @ dbPay.descrip ACCUM SUB-TOTAL BY dbRec.PayType (dbrec.amount) @ wsTotal WITH FRAME frm-rpt.
            DOWN 2 STREAM a WITH FRAME frm-rpt.
        END.
    END.
    PAGE STREAM a.
    FOR EACH dbrec WHERE dbRec.recDate = wsDate AND RecStat = "" BREAK BY dbRec.OpCode BY dbRec.PayType BY dbrec.rcode:
        IF FIRST-OF(dbRec.opCode) THEN
        DO:
            FIND FIRST dbRecop WHERE dbRecop.usercode =  dbRec.OpCode NO-LOCK NO-ERROR.
            FIND FIRST simusr WHERE simusr.usercode = dbRecop.usercode NO-LOCK NO-ERROR.
            DISPLAY STREAM a  simusr.NAME @ swCashier WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
        IF FIRST-OF(dbRec.PayType) THEN
        DO:
            FIND FIRST dbpay WHERE dbpay.paytype = dbrec.paytype NO-LOCK NO-ERROR.
            DISPLAY STREAM a dbPay.descrip WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
        ACCUMULATE dbrec.Amount (SUB-TOTAL BY dbRec.PayType).
        ACCUMULATE dbrec.Amount (SUB-TOTAL BY dbrec.rcode).
        ACCUMULATE dbrec.Amount (SUB-TOTAL BY dbRec.OpCode).
        IF LAST-OF(dbrec.rcode) THEN DO:
            FIND FIRST dbrcod WHERE dbrcod.rcode = dbrec.rcode NO-LOCK NO-ERROR.
            DISPLAY STREAM a dbRCod.rCode dbRCod.descrip ACCUM SUB-TOTAL BY dbrec.rcode (dbrec.amount) @ wsTotal dbRec.txtCur WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
        END.
        IF LAST-OF(dbRec.PayType) THEN DO:
            UNDERLINE STREAM  a wsTotal WITH FRAME frm-rpt1.
            DOWN STREAM a WITH FRAME frm-rpt1.
            DISPLAY STREAM a "SUB-TOTAL" @ dbPay.descrip ACCUM SUB-TOTAL BY dbRec.PayType (dbrec.amount) @ wsTotal WITH FRAME frm-rpt1.
            DOWN 2 STREAM a WITH FRAME frm-rpt1.
        END.
    END.
END PROCEDURE.

PROCEDURE rec-Exp:
    wsFile = "c:\simacc\reports\RecBnk" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
              + SUBSTR(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTR(STRING(TIME,"HH:MM:SS"),4,2)
              + ".csv".
    OUTPUT STREAM b TO VALUE(wsFile).
    MESSAGE "This will EXTRACT FOR validated receipts only" VIEW-AS ALERT-BOX.
    FOR EACH dbrecctr WHERE valid = YES AND dbrecctr.recDate = wsDate:
        FOR EACH dbrec WHERE dbRec.recDate = wsDate AND dbrec.opcode = dbrecctr.usercode AND RecStat = "":
        DISPLAY recno WITH FRAME frm-Main. PAUSE 0.
            FIND FIRST tblRec WHERE varop = dbrec.opcode AND varType = dbrec.paytype 
                                AND varrCod = dbrec.rcode AND varCur  = dbrec.txtCur NO-ERROR.
            IF NOT AVAILABLE tblrec THEN DO:
               CREATE tblRec.
               ASSIGN varType = dbrec.paytype
                      varOp   = dbrec.opcode
                      varrCod = dbrec.rcode
                      varCur  = dbrec.txtCur.
            END.
            varAmt  = varAmt + dbrec.amount.
        END.
     END.
    IF CAN-FIND(FIRST tblRec) THEN
    EXPORT STREAM b DELIMITER ','  "CASHIER" "PAYTYPE" "INCOME" "AMOUNT" "CURRENCY".
    FOR EACH tblRec BY varOp BY varCur BY varType BY varrCo:
       FIND FIRST simusr WHERE simusr.usercode = varOp NO-LOCK NO-ERROR.
       FIND FIRST dbpay WHERE dbpay.paytype = vartype NO-LOCK NO-ERROR.
       FIND FIRST dbrcod WHERE dbrcod.rcode = varrCod NO-LOCK NO-ERROR.
        EXPORT STREAM b DELIMITER  ','  simusr.NAME dbpay.descrip dbrcod.descrip tblrec.varAmt tblrec.varCur.
    END.
    OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile). 
END PROCEDURE.
                    
