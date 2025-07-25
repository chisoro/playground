
/* Program.................excabtrans.p
   Notes:................. Cash Boom Transactions
   Author:.................S. Chisoro
   
*/
session:DATA-ENTRY-RETURN = TRUE.

DEF  VAR wsStatus   AS CHAR FORM "X(20)".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-OK  LABEL "PROCESS".
DEF VAR wsFile as CHAR.
DEF VAR wsFile1 AS CHAR FORM "X(20)".
DEF VAR wsFile2 AS CHAR FORM "X(20)".
 DEF VAR Read AS DECIMAL.
 DEF VAR rDate AS DATE.
 DEF VAR cn AS INTEGER.
 DEF VAR cn1 AS INTEGER.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 by 8.5.


DEF FRAME frm-main
    SKIP(1)
   cn      COLON 26  LABEL "Processing........." VIEW-AS TEXT
    btn-ok AT ROW 10.5 COL 10
    btn-exit AT ROW 10.5 COL 65 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13.0
    TITLE "CASH BOOK EXPORT" VIEW-AS DIALOG-BOX.



 ON CHOOSE OF btn-ok IN FRAME frm-main 
     
 DO: 
    OUTPUT TO VALUE(wsFile).
   EXPORT DELIMITER "," "BANK"  "PERIOD"  "TRANSACTION DATE"  "TRANSACTION TYPE" "REFERENCE" "SEQUENCE" "AMOUNT"   "CURRENCY" "RATE" "DESCRIPTION" "LEDGER" "PROJECT" "FUND" "DEPARTMENT"  "POSTAL DATE"   "SOURCE CODE" "RECONCILIATION"  "STATEMENT NUMBER" "CASHBOOK".
    FOR EACH CBTRANS:
           CN = CN + 1.
            ASSIGN CN:SCREEN-VALUE IN FRAME frm-main = string(CN).
                    EXPORT DELIMITER "," bank  Accper trDate  TranType Ref seq amount  txtcur decRate  Descrip Ledger Proj Fund Dept  PoDate  srccode  Recon StatNo Acb.
     END.
         
     OS-COMMAND NO-WAIT VALUE(wsFile)).
  END.

/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */
FIND FIRST simctr NO-LOCK.
wsFile = simctr.repDir + "CASHBOOK.CSV".
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


