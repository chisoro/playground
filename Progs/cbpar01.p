/* Program................cbpar01.p
   Notes:................. Cashbook parameter list/print
   Author:.................S. Mawire
   Modifide:.............S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR varRep        AS INT  INITIAL 1.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".
DEF VAR wsFile as CHAR.
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
    "TRANSACTION TYPE LISTING", 1, "CASH BOOK LISTING", 2,"BANK LISTING", 3
    SKIP(0.5)  
    btn-ok AT ROW 12.7 COL 20 space(20) 
    btn-exp space(20)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 15.5 KEEP-TAB-ORDER
    TITLE "CASHBOOK PARAMETER LISTING" VIEW-AS DIALOG-BOX.

FORM 
     cbtype.TranType  AT 10 LABEL "CODE"
     cbtype.DESCRIP     LABEL "DESCRIPTION"
     cbtype.DrCr        LABEL "Dr/CR"
    HEADER skip(2) "          TRANSACTION TYPE LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-type.

FORM 
    cbkmf.Acb       AT 10 LABEL "CASHBOOK"
    cbkmf.descrip         LABEL "DESCRIPTION"
    cbkmf.Ledger          LABEL "CONTROL LEDGER"
    cbkmf.Bal LABEL "BALANCE"
    cbkmf.txtCur LABEL "CURRENCY"
    HEADER skip(2) "      CASHBOOK LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-cb.

FORM 
     cbkmf.bank       AT 10 LABEL "BANK"
    cbkmf.descrip         LABEL "DESCRIPTION"
    cbkmf.Ledger          LABEL "LEDGER"
    cbkmf.Bal LABEL "BALANCE"
    cbkmf.txtCur LABEL "CURRENCY"
    HEADER skip(2) "          BANK LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-bank.


/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN  VarRep.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  
END.

ON 'choose':U OF btn-exp  
DO:
   session:set-wait-state("").
   ASSIGN  VarRep.
   RUN Reporti.ip.
  
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
        FOR EACH cbtype NO-LOCK:
            DISPLAY STREAM a cbtype.TranType cbtype.DrCr cbtype.descrip WITH FRAME frm-type.
            DOWN STREAM a WITH FRAME frm-type.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
        FOR EACH cbkmf  WHERE cbkmf.acb <> 0 NO-LOCK:
            DISPLAY STREAM a cbkmf.Acb cbkmf.descrip cbkmf.Ledger  cbkmf.bal    cbkmf.txtCur WITH FRAME frm-cb.
            DOWN STREAM a WITH FRAME frm-cb.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 3 THEN DO:
        FOR EACH cbkmf WHERE cbkmf.bank <> 0  NO-LOCK:
            DISPLAY STREAM a cbkmf.bank cbkmf.descrip cbkmf.Ledger  cbkmf.bal cbkmf.txtCur  WITH FRAME frm-bank.
            DOWN STREAM a WITH FRAME frm-bank.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
END CASE.
END.


PROCEDURE reporti.ip:
CASE varRep:
    WHEN 1 THEN DO:
         wsFile =  wsFile + "transaction.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "TYPE" "DEBIT/CREDIT" "DESCRIPTION".
        FOR EACH cbtype NO-LOCK:
            EXPORT DELIMITER "," cbtype.TranType cbtype.DrCr cbtype.descrip.
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
         wsFile =  wsFile + "cashbook.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CASHBOOK" "DESCRIPTION" "LEDGER" "BALANCE" "CURRENCY".
        FOR EACH cbkmf  WHERE cbkmf.acb <> 0 NO-LOCK:
            EXPORT DELIMITER "," cbkmf.Acb cbkmf.descrip cbkmf.Ledger  cbkmf.bal cbkmf.txtCur .
         
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 3 THEN DO:
         wsFile =  wsFile + "bank.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "BANK" "DESCRIPTION" "LEDGER" "BALANCE" "CURRENCY".
        FOR EACH cbkmf WHERE cbkmf.bank <> 0  NO-LOCK:
           EXPORT DELIMITER "," cbkmf.bank cbkmf.descrip cbkmf.Ledger  cbkmf.bal cbkmf.txtCur .
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
END CASE.
END.


