/* Program................dbpar01.o.p
   Notes:................. Debtors parameter list/print
   Author:.................S. Mawire
   Modifide:..............S. Chisoro
   
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR varRep        AS INT  INITIAL 1.
DEF VAR wsFile as CHAR.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btn-Exp LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEF FRAME frm-main
    SKIP(1.5)
    varRep       LABEL "Which Report?" COLON 30 
    VIEW-AS RADIO-SET RADIO-BUTTONS
    "Ward List", 1, "Suburb List", 2,"Consumer TYPE List", 3, "Services List", 4,
    "Tariff List", 5, "Receipting Codes List", 6, "Payment Methods List", 7
    SKIP(0.5)  
    btn-ok AT ROW 12.7 COL 20 space(20) 
    btn-Exp  space(20)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 15.5 KEEP-TAB-ORDER
    TITLE "GENERAL LEDGER PARAMETER LISTING" VIEW-AS DIALOG-BOX.

FORM 
     dbwrd.Ward  AT 10 LABEL "CODE"
     dbwrd.DESCRIP     LABEL "DESCRIPTION"
    HEADER skip(2) "          WARD LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-ward.

FORM 
     dbsmf.suburb     AT 10 LABEL "CODE"
     dbsmf.DESCRIP         LABEL "DESCRIPTION"
    HEADER skip(2) "      SUBURB/AREA LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-suburb.

FORM 
     dbctf.cons  AT 10 LABEL "CODE"
     dbctf.DESCRIP     LABEL "DESCRIPTION"
     dbctf.Interest     LABEL "INTEREST"
    HEADER skip(2) "          RATEPAYER CONSUMER LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-cons.

FORM 
     dbsgr.Sgrp    AT 10 LABEL "CODE"
     dbsgr.DESCRIP       LABEL "DESCRIPTION"
     dbsgr.ctrLedger     LABEL "CONTROL LEDGER"
    HEADER skip(2) "      SERVICES LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-service.
 
FORM 
    dbtmf.Tarif      AT 10 LABEL "CODE"
    dbtmf.Sgrp             LABEL "SERVICE"
    dbtmf.DESCRIP          LABEL "DESCRIPTION" FORM "X(30)"
    dbtmf.Charge           LABEL "CHARGE"
    dbtmf.freq             LABEL "FREQ"
    dbtmf.Iledger          LABEL "LEDGER"
    dbtmf.type             LABEL "TYPE"
    dbtmf.Vat%             LABEL "VAT%"  FORM "zz9.99"
    HEADER skip(2) "      SERVICE TARIFF LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-tariff.

FORM 
    dbRCod.rCode     AT 10 LABEL "CODE"
    dbRCod.DESCRIP         LABEL "DESCRIPTION"
    dbRCod.Sgrp             LABEL "SERVICE"
    dbRCod.Acb              LABEL "CASHBOOK"
    dbRCod.Ledger           LABEL "CONTROL LEDGER"
    dbRCod.target           LABEL "TYPE"
    dbRCod.vat%             LABEL "VAT%" FORM "zz9.99"
    HEADER skip(2) "      RECEIPTING CODES LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-icode.

FORM 
    dbPay.Paytype     AT 10 LABEL "CODE"
    dbPay.DESCRIP         LABEL "DESCRIPTION"
    dbPay.bank             LABEL "BANK"
    HEADER skip(2) "      PAYMENT METHODS LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-Pcode.
/* */

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN  VarRep.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  
END.

   ON 'choose':U OF btn-Exp  
DO:
   session:set-wait-state("").
   ASSIGN  VarRep.
           RUN  reporti.ip.
  
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK.
wsFile = simctr.repDir.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE report.ip:
CASE varRep:
    WHEN 1 THEN DO:
        FOR EACH dbwrd NO-LOCK:
            DISPLAY STREAM a dbwrd.Ward dbwrd.Descrip WITH FRAME frm-ward.
            DOWN STREAM a WITH FRAME frm-ward.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
        FOR EACH dbsmf NO-LOCK:
            DISPLAY STREAM a dbsmf.suburb dbsmf.Descrip WITH FRAME frm-suburb.
            DOWN STREAM a WITH FRAME frm-suburb.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 3 THEN DO:
        FOR EACH dbctf NO-LOCK:
            DISPLAY STREAM a dbctf.cons dbctf.Descrip dbctf.Interest WITH FRAME frm-cons.
            DOWN STREAM a WITH FRAME frm-cons.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 4 THEN DO:
        FOR EACH dbsgr NO-LOCK:
            DISPLAY STREAM a dbsgr.Sgrp dbsgr.Descrip dbsgr.ctrLedger WITH FRAME frm-service.
            DOWN STREAM a WITH FRAME frm-service.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 5 THEN DO:
        FOR EACH dbtmf NO-LOCK:
            DISPLAY STREAM a dbtmf.Charge dbtmf.Descrip dbtmf.freq dbtmf.Iledger dbtmf.Sgrp 
                dbtmf.Tarif dbtmf.type dbtmf.Vat% WITH FRAME frm-tariff.
            DOWN STREAM a WITH FRAME frm-tariff.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 6 THEN DO:
        FOR EACH dbRCod NO-LOCK:
            DISPLAY STREAM a dbRCod.Acb dbRCod.Descrip dbRCod.Ledger dbRCod.vat% 
                dbRCod.target dbRCod.Sgrp dbRCod.rCode WITH FRAME frm-icode.
            DOWN STREAM a WITH FRAME frm-icode.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
     WHEN 7 THEN DO:
        FOR EACH dbPay NO-LOCK:
            DISPLAY STREAM a dbPay.Paytype dbPay.descrip dbPay.bank WITH FRAME frm-pcode.
            DOWN STREAM a WITH FRAME frm-pcode.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
END CASE.
END.

PROCEDURE reporti.ip:
CASE varRep:
    WHEN 1 THEN DO:
        wsFile =  wsFile + "ward.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "WARD" "DESCRIPTION".
        FOR EACH dbwrd NO-LOCK:
            EXPORT DELIMITER "," dbwrd.Ward dbwrd.Descrip.
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
        wsFile =  wsFile + "surburb.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "SURBURB" "DESCRIPTION".
        FOR EACH dbsmf NO-LOCK:
             EXPORT DELIMITER ","  dbsmf.suburb dbsmf.Descrip.
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 3 THEN DO:
        wsFile =  wsFile + "consumertype.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CONSUMER" "DESCRIPTION" "INTEREST".
        FOR EACH dbctf NO-LOCK:
            EXPORT DELIMITER "," dbctf.cons dbctf.Descrip dbctf.Interest.
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 4 THEN DO:
        wsFile =  wsFile + "service.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "SERVICE" "DESCRIPTION" "LEDGER".
        FOR EACH dbsgr NO-LOCK:
            EXPORT DELIMITER "," dbsgr.Sgrp dbsgr.Descrip dbsgr.ctrLedger.
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 5 THEN DO:
        wsFile =  wsFile + "tarrif.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "TARRIF" "DESCRIPTION" "FREQUENCY" "LEDGER" "SERVICE" "TYPE" "VAT%" "CHARGE".
        FOR EACH dbtmf NO-LOCK:
            EXPORT DELIMITER "," dbtmf.Tarif dbtmf.Descrip dbtmf.freq dbtmf.Iledger dbtmf.Sgrp 
                 dbtmf.type dbtmf.Vat%  dbtmf.Charge.
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 6 THEN DO:
        wsFile =  wsFile + "RECEIPTINGCODES.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CODE" "SERVICE" "CASHBOOK" "DESCRIPTION" "LEDGER"   "VAT%" "TARGET".
        FOR EACH dbRCod NO-LOCK:
            EXPORT DELIMITER "," dbRCod.rCode  dbRCod.Sgrp dbRCod.Acb dbRCod.Descrip dbRCod.Ledger dbRCod.vat% 
                dbRCod.target .
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
     WHEN 7 THEN DO:
         wsFile =  wsFile + "PAYMENTMETHODS.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CODE" "DESCRIPTION" "BANK".
        FOR EACH dbPay NO-LOCK:
            EXPORT DELIMITER "," dbPay.Paytype dbPay.descrip dbPay.bank.
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
END CASE.
END.
