
/* Program.................exSERVICE.p
   Notes:................. Export SERVICES
   Author:.................S. Chisoro
   
*/
session:DATA-ENTRY-RETURN = TRUE.

DEF  VAR wsStatus   AS CHAR FORM "X(20)".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-OK  LABEL "PROCESS".
DEF VAR wsFile as CHAR.
DEF VAR wsFile1 AS CHAR FORM "X(20)".
DEF VAR wsFile2 AS CHAR FORM "X(20)".
DEF VAR PROGRAM AS CHAR FORM "X(20)".
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
   PROGRAM VIEW-AS TEXT COLON 15  SPACE (5) CN    LABEL "Processing Account........." VIEW-AS TEXT
    btn-ok AT ROW 10.5 COL 10
    btn-exit AT ROW 10.5 COL 65 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 85 BY 13.0
    TITLE "DATA EXPORT" VIEW-AS DIALOG-BOX.



 ON CHOOSE OF btn-ok IN FRAME frm-main 
     
 DO: 
     wsFile = simctr.repDir + "CASHBOOK.CSV".
     OUTPUT TO VALUE(wsFile).
     cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "CASHBOOK".
   EXPORT DELIMITER "," "BANK"  "PERIOD"  "TRANSACTION DATE"  "TRANSACTION TYPE" "REFERENCE" "SEQUENCE" "AMOUNT"   "CURRENCY" "RATE" "DESCRIPTION" "LEDGER" "PROJECT" "FUND" "DEPARTMENT"  "RECONCILIATION"  "STATEMENT NUMBER" "CASHBOOK".
    FOR EACH CBTRANS:
           CN = CN + 1.
            ASSIGN CN:SCREEN-VALUE IN FRAME frm-main = string(CN).
                    EXPORT DELIMITER "," bank  Accper trDate  TranType Ref seq amount  txtcur decRate  Descrip Ledger Proj CBTRANS.Fund CBTRANS.Dept    CBTRANS.Recon CBTRANS.StatNo CBTRANS.Acb.
     END.
         
     wsFile = simctr.repDir + "BANKS.CSV".
     OUTPUT TO VALUE(wsFile).
     cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "BANKS".
   EXPORT DELIMITER "," "BANK" "NAME" "LEDGER" "CURRENCY".
   FOR EACH CBKMF:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," CBKMF.bank CBKMF.descrip CBKMF.Ledger CBKMF.txtCur.
     END.

     wsFile = simctr.repDir + "TRANSACTIONTYPE.CSV".
     OUTPUT TO VALUE(wsFile).
     cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "TRANSACTION TYPE".
  EXPORT DELIMITER "," "TYPE" "DESCRIPTION" "DEBIT/CREDIT".
  FOR EACH CBTYPE:
       cn = cn + 1.
           ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                   EXPORT DELIMITER "," CBTYPE.TranType CBTYPE.descrip CBTYPE.DrCr.
    END.

        

   wsFile = simctr.repDir + "SERVICES.CSV".
   OUTPUT TO VALUE(wsFile).
   cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "SERVICES".
   EXPORT DELIMITER "," "SERVICE" "DESCRIPTION" "CONTROL LEDGER" "INTEREST LEDGER" "PROJECT" "DEPARTMENT".
   FOR EACH DBSGR:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," Sgrp DBSGR.Descrip DBSGR.ctrLedger DBSGR.IntLedger DBSGR.Proj DBSGR.Dept.
     END.

     wsFile = simctr.repDir + "TARRIF.CSV".
   OUTPUT TO VALUE(wsFile).
   cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "TARRIFS".
   EXPORT DELIMITER "," "Tarif"  "SERVICE"  "DESCRIPTION" "BUILDING VALUE" "LAND VLAUE" "INCOME LEDGER"   "PROJCET" "DEPARTMENT" "FUND" "CHARGE" "factor[1]" "factor[2]" "factor[3]" "factor[4]" "factor[5]"  "TCharge[1]" "TCharge[2]" "TCharge[3]" "TCharge[4]" "TCharge[5]" "Tear[1]" "Tear[2]" "Tear[3]" "Tear[4]" "Tear[5]" "TYPE" "FREQUENCE"    "SOURCE" "Vat%".
   FOR EACH DBTMF:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," Tarif  DBTMF.Sgrp  DBTMF.Descrip BValue LValue DBTMF.Iledger   DBTMF.Proj DBTMF.Dept DBTMF.fund Charge factor[1] factor[2] factor[3] factor[4] factor[5]  TCharge[1] TCharge[2] TCharge[3] TCharge[4] TCharge[5] Tear[1] Tear[2] Tear[3] Tear[4] Tear[5] type freq    SOURCE Vat%.
     END.

    wsFile = simctr.repDir + "CONSUMERTYPE.CSV".
   OUTPUT TO VALUE(wsFile).
   cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "CONSUMER TYPE".
   EXPORT DELIMITER "," "CONSUMER" "DESCRIPTION" "INTEREST".
   FOR EACH dbctf:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," dbctf.cons dbctf.Descrip dbctf.Interest.
     END.

    wsFile = simctr.repDir + "PAYMENTTYPE.CSV".
   OUTPUT TO VALUE(wsFile).
   cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "PAYMENT TYPE".
   EXPORT DELIMITER "," "BANK" "DESCRIPTION" "PAY TYPE".
   FOR EACH dbPay:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," dbPay.bank dbPay.descrip dbPay.Paytype.
     END.

    wsFile = simctr.repDir + "RECEIPTINGCODES.CSV".
   OUTPUT TO VALUE(wsFile).
   cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "RECEIPTING CODES".
   EXPORT DELIMITER "," "CODE" "DESCRIPTION"  "SERVICE" "CASHBOOK"  "LEDGER" "PROJECT" "DEPARTMENT"  "TARGET" "TRACK" "VAT%".
   FOR EACH dbRCod:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," dbRCod.rCode dbRCod.Descrip  dbRCod.Sgrp dbRCod.Acb   dbRCod.Ledger dbRCod.Proj dbRCod.Dept  dbRCod.target dbRCod.track dbRCod.vat%.
     END.

     wsFile = simctr.repDir + "CONSUMERS.CSV".
   OUTPUT TO VALUE(wsFile).
   cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "CONSUMERS".
   EXPORT DELIMITER "," "ACCOUNT" "NAME" "BALANCE" "STATUS" "ADDRESS1" "ADDRESS2" "ADDRESS3" "BALBF" "BUILDING VALUE" "CELLPHONE" "CONSUMER TYPE" "CREATE DATE"  "DEED" "DEPAMOUNT" "SEND EMAIL" "EMAIL" "GROUP" "LAST PAYMENT"  "OWNER" "RATE TARIF" "REGISTRATION ID"  "SITE VALUE" "STAND SIZE" "SEND SMS" "SORT NAME" "STAND NUMBER" "STREET NUMBER" "STREET" "SUBURB" "TARIF1" "TARIF2"  "TARIF3"  "TARIF4"  "TARIF5"  "TARIF6"  "TARIF7"  "TARIF8"  "TARIF9"  "TARIF10"  "TARIF11"  "TARIF12"  "TARIF13"  "TARIF14"  "Units[1]" "Units[2]" "Units[3]" "Units[4]" "Units[5]" "Units[6]" "Units[7]" "Units[8]" "Units[9]" "Units[10]" "Units[11]" "Units[12]" "Units[13]" "Units[14]"  "VatNo"  "Ward.".
   FOR EACH dbCMF:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," dbcmf.dbAcc dbcmf.NAME dbcmf.AccBal dbcmf.AccStat dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.BalBf dbcmf.BldValue dbcmf.Cell dbcmf.Cons dbcmf.Credate  dbcmf.DeedNo dbcmf.DepAmt dbcmf.email dbcmf.emailAdd dbcmf.intgrp dbcmf.lpDate   dbcmf.POwner dbcmf.Rate-tarif dbcmf.RegID  dbcmf.SiteValue dbcmf.Size dbcmf.Sms dbcmf.SortName dbcmf.StandNo dbcmf.stno dbcmf.street dbcmf.Suburb dbcmf.tarif[1] dbcmf.tarif[2] dbcmf.tarif[3] dbcmf.tarif[4] dbcmf.tarif[5] dbcmf.tarif[6] dbcmf.tarif[7] dbcmf.tarif[8] dbcmf.tarif[9] dbcmf.tarif[10] dbcmf.tarif[11] dbcmf.tarif[12] dbcmf.tarif[13] dbcmf.tarif[14] dbcmf.Units[1] dbcmf.Units[2] dbcmf.Units[3] dbcmf.Units[4] dbcmf.Units[5] dbcmf.Units[6] dbcmf.Units[7] dbcmf.Units[8] dbcmf.Units[9] dbcmf.Units[10] dbcmf.Units[11] dbcmf.Units[12] dbcmf.Units[13] dbcmf.Units[14]  dbcmf.VatNo dbcmf.Ward.
     END.

     wsFile = simctr.repDir + "CONSUMERHISTORY.CSV".
   OUTPUT TO VALUE(wsFile).
   cn = 0.
   ASSIGN program:SCREEN-VALUE IN FRAME frm-main = "CONSUMER HISTORY".
   EXPORT DELIMITER "," "ACCOUNT" "PERIOD" "DESCRIPTION" "AMOUNT"  "Rate" "DEPARTMENT"  "FUND" "INCOME LEDGER" "PoDate" "PROGRAM" "PROJECT" "REFERENCE" "SEQUENCE" "SERVICE GROUP" "Tarif "  "TRNSACTION DATE" "CURRENCY" "TYPE"  "Vat".
   FOR EACH dbHTF:
        cn = cn + 1.
            ASSIGN cn:SCREEN-VALUE IN FRAME frm-main = string(cn).
                    EXPORT DELIMITER "," dbhtf.dbAcc dbhtf.Accper dbhtf.DESCRIP dbhtf.Amt  dbhtf.decRate dbhtf.Dept  dbhtf.Fund dbhtf.ILedger dbhtf.PoDate dbhtf.prog dbhtf.proj dbhtf.Ref dbhtf.Seq dbhtf.Sgrp dbhtf.Tarif  dbhtf.trDate dbhtf.txtCur dbhtf.type  dbhtf.Vat.
     END.
         
     /*OS-COMMAND NO-WAIT VALUE(wsFile)).*/
  END.

/********** MAIN LOGIC **********/
/*VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW. */
FIND FIRST simctr NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


