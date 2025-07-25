/* Program.................ordprn.i
   Notes:...... include file formprinting orders
   Author:.................S. Mawire
*/
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR l AS INT.
DEF VAR s AS INT.

FORM 
    "ORDER NUMBER:"    AT ROW 10 COLUMN 65 ordmf.OrdNo NO-LABEL
     "DATE:"           AT ROW 11 COLUMN 65 ordmf.OrdDate NO-LABEL SKIP(1)
    "SEND INVOICE TO:" AT ROW 13 COL 5
    "DELIVERY TO:"     AT ROW 13 COL 65
    simCtr.CONAME    AT ROW 14 COL 5
    simCtr.Add1      AT ROW 14 COL 5 
    simCtr.add2      AT ROW 15 COL 5 
    simCtr.Add3      AT ROW 16 COL 5 SKIP(3) 
    "ORDER"          AT 45 SKIP(1)
    "ACCOUNT: "        AT 65 Crdmf.Acc NO-LABEL  FORM "9999999" SKIP
    Crdmf.Name       AT 65 SKIP
    Crdmf.Add1       AT 65 SKIP
    Crdmf.Add2       AT 65 SKIP
    Crdmf.town       AT 65 SKIP(2)
    "     Please supply the items listed below as:"  SKIP(1)
     SPACE(5) "LN     DESCRIPTION                                 QUANTITY     AMOUNT      ALLOCATION" SKIP
     SPACE(5) "______________________________________________________________________________________" SKIP
WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME frmheader.
    
FORM 
    SPACE(5) Ordtmf.LineSeq NO-LABEL
    Ordtmf.Descrip NO-LABEL FORM "x(40)"
    Ordtmf.Qty     NO-LABEL
    Ordtmf.Amt     NO-LABEL
    Ordtmf.Ledger  NO-LABEL
WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME frmDetail.

FORM
    "CONDITIONS OF CONTRACT" AT ROW 15 COL 5 SKIP(1)
    "1. RISK OF SUPPLIER: The Risk remains with the Supplier until delivery has been affected." AT 5 SKIP
    "2. DELIVERY: If the delivery date cannot be adhered to, the Supplier should immediately " AT 5 SKIP
    "   contact the procurement department." AT 5 SKIP
    "3. INVOICES: Invoices must be forwarded immediately after the dispatch of goods or " AT 5 SKIP
    "   services to the above address." AT 5 SKIP
    "4. PRICE ESCALATIONS: Escalations in control prices should be authenticated by the " AT 5 SKIP
    "   relevant date." AT 5 SKIP(2)
    "Prepared by: PROCUREMENT OFFICER" AT 5 
    "Approved by: CHIEF EXECUTIVE OFFICER" AT 50 SKIP(1)
    "Signature:____________________________"  AT 5
    "Signature:____________________________"  AT 50 SKIP(1)
    "Date:________________"  AT 5
    "Date:________________"  AT 50
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME frmFoot.
     
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST ordmf WHERE ordmf.OrdNo = wsid NO-LOCK NO-ERROR.
FIND FIRST crdmf WHERE crdmf.acc = ordmf.Acc NO-LOCK NO-ERROR.
DISPLAY STREAM a ordmf.OrdNo simctr.coname simCtr.Add1 simCtr.add2 simCtr.Add3 ordmf.OrdDate
         Crdmf.Acc Crdmf.Name Crdmf.Add1 Crdmf.Add2 Crdmf.town WITH FRAME frmHeader.
DOWN STREAM a WITH FRAME frmHeader.
l = 1.
FOR EACH ordtmf WHERE ordtmf.ordNo = ordmf.ordNo NO-LOCK:
    DISPLAY STREAM a Ordtmf.LineSeq Ordtmf.Descrip Ordtmf.Qty Ordtmf.Amt  Ordtmf.Ledger
            WITH FRAME frmDetail.
    DOWN STREAM a WITH FRAME frmDetail.
    l = l + 1.
END.
UNDERLINE  STREAM a Ordtmf.Amt WITH FRAME  frmDetail.
DOWN STREAM  a WITH FRAME  frmDetail.
DISPLAY STREAM a "ORDER TOTAL" @ Ordtmf.Descrip Ordmf.ordAmt @ Ordtmf.Amt WITH FRAME frmDetail.
DOWN STREAM  a WITH FRAME frmDetail.
/*s = 16 - l.
DOWN s STREAM a WITH FRAME frmDetail.
DISPLAY STREAM a "BUYER:______________________________" @ Ordtmf.Descrip WITH FRAME frmDetail.
DOWN 2 STREAM  a WITH FRAME frmDetail.
DISPLAY STREAM a "AUTHORISED BY:______________________________" @ Ordtmf.Descrip WITH FRAME frmDetail.
DOWN STREAM  a WITH FRAME frmDetail. */
DISPLAY STREAM a WITH FRAME frmFoot.
DOWN STREAM  a WITH FRAME frmFoot.
   
