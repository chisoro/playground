USING System.Net.Http.*. 
USING System.Environment.

session:DATA-ENTRY-RETURN = TRUE.
/* Program.................DbRec01.p
   Notes:......            New Receipt capture (FDMS)
   Author:.................S. Mawire
   Modified ..............S. Chisoro
*/
&SCOPED-DEFINE tmptable             Dbcmf

DEF BUFFER bfrDbcmf FOR Dbcmf.
DEF BUFFER bfrDbRec FOR dbRec.
DEF BUFFER bfrdbRecop FOR dbRecop.
DEF BUFFER bfrDbRecCtr FOR dbrecctr.
DEF TEMP-TABLE tmpbfrDbRec LIKE DbRec.
DEF TEMP-TABLE tblDetail LIKE dbblf.
DEF BUFFER bfdbCtr FOR dbBCtr.
DEF BUFFER bfrBatch FOR dbbatch.
DEFINE VARIABLE binary-key LIKE simusr.bKey.
DEFINE VARIABLE crypto-value LIKE simusr.password.
DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO.
DEF VAR method-status  AS LOGICAL.
DEF VAR dT AS DATETIME-TZ.
DEF VAR d AS CHAR FORM "x(19)".
DEF STREAM a.
DEF STREAM c.
DEF SHARED VAR varUser     LIKE simusr.usercode. 
DEF VAR varRecNo    LIKE bfrDbRec.RecNo FORM ">>>>>>9".
DEF VAR wsRecNo     LIKE varRecNo.
DEF VAR varDate     LIKE bfrDbRec.RecDate.
DEF VAR varrCode    LIKE bfrDbRec.rCode.
DEF VAR varMeth     LIKE bfrDbRec.Paytype.
DEF VAR varAmount   LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsAmt       LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsTotal     LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsVoid      LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsEquv      LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99".
DEF VAR wsDisc      LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsTall      LIKE wsAmt.
DEF VAR wsAlloc     LIKE wsAmt.
DEF VAR varAcc      LIKE bfrDbRec.Account.
DEF VAR wsAcc       AS CHAR FORM "x(11)".
DEF VAR varRef      LIKE bfrDbRec.Ref.
DEF VAR X           LIKE bfrDbRec.SeqNo.
DEF VAR wscnt       AS INT.
DEF VAR wsTax       AS CHAR.
DEF VAR Y           AS INT.
DEF VAR bBal        LIKE varAmount.
DEF VAR aBal        LIKE varAmount.
DEF VAR wstime      AS CHAR.
DEF VAR wsFile      AS CHAR.
DEF VAR wsValid     AS LOGICAL.
DEF VAR wsAns       AS LOGICAL.
DEF VAR varLedger   LIKE bfrDbRec.Account.
DEF VAR varType     AS CHAR FORM "x(1)" INITIAL "C".
DEF VAR varloop     AS CHAR FORM "x(1)". /*AS LOGICAL INITIAL YES. */
DEF VAR varTend     LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR varChg      LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR varDue      LIKE bfrDbRec.Amount FORM "zzzzzzzz9.99-".
DEF VAR varbal      AS DEC FORM "zzzzzzzzz9.99-".
DEF VAR varServ     AS CHAR FORM "x(20)".
DEF VAR wsCashier   AS CHAR FORM "x(80)".
DEF VAR varTotal     LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR depTotal     LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsTitle     AS CHAR FORM "x(80)".
DEF VAR wsTitle1    LIKE simctr.CONAME.
DEF VAR wsAdd      AS CHAR FORM "x(60)".
DEF VAR  wsVar      AS CHAR FORM "x(40)"  LABEL "Suburb".
DEF VAR varDescrip   LIKE bfrDbRec.Descrip.
DEF VAR wsSearch   LIKE Dbcmf.NAME.
DEF VAR wsRecDetails AS CHAR FORM "x(60)".
DEF VAR wsFot       LIKE wsTitle EXTENT 3.
DEF VAR wsSup LIKE simusr.usercode.
DEF VAR varCur LIKE tblForex.txtCur.
DEF VAR bCur LIKE tblForex.txtCur.
DEF VAR aCur LIKE tblForex.txtCur.
DEF VAR wsdis AS DEC FORM "zz9.99".
DEF VAR wsdisRate LIKE tblForex.decRate.
DEF VAR wsd1Rate LIKE tblForex.decRate.
DEF VAR wsDecRate LIKE tblForex.decRate.
DEF VAR wsReRate  LIKE tblForex.decRate.
DEF VAR dbRate LIKE tblForex.decRate FORM "zzzzzzzzz9.999999".
DEF VAR rdbRate LIKE tblForex.decRate FORM "zzzzzzzzz9.999999".
DEF VAR wsRate LIKE tblForex.decRate FORM "zzzzzzzzz9.999999".
DEF VAR wsReceipts LIKE varTend.
DEF VAR wsTot  AS DEC EXTENT 3 FORM "z,zzz,zzz,zz9.99-".
DEF VAR tt AS CHAR FORM "x(11)" INITIAL "AMOUNT(ZWG)".
DEF VAR wsTotalW LIKE bfrDbRec.Amount.
DEF VAR wsmark  AS CHAR FORM "x(10)" INITIAL "P;CASH".
DEF VAR wsBatch AS INT.
DEF VAR wsledger LIKE glmf.acct.
DEF VAR wsper AS DEC.
/*DEF VAR varUser AS CHAR INITIAL "1". */
DEF VAR wslin AS DEC.
DEF VAR wsVatNo LIKE dbcmf.VatNo.
DEF VAR wsTinNo LIKE dbcmf.TIN.
DEF VAR wsemail LIKE dbcmf.emailadd.
DEF VAR wsphone AS DEC FORM ">>>>>>>>>>>>>>>9".
DEF VAR varName LIKE simusr.NAME.
DEF VAR wsline AS CHAR FORM "x(80)" EXTENT 2.
DEF VAR Allow-disc AS LOGICAL INITIAL NO.
DEF VAR wsnar LIKE dbrate.
DEF VAR varTerminal LIKE eftTerminal.term_id.
DEF VAR varTransNumber AS INT.

def var Account as char.
def var ActionCode as char.
def var AmountTransactionFee as char.
def var AuthorizationProfile as char. 
def var BusinessDate as char. 
def var CardNumber as char. 
def var CardProductName as char. 
def var CurrencyCode as char.
def var vDateTime as char.
def var LocalDate as char. 
def var LocalTime as char. 
def var MerchantId as char.
def var MessageReasonCode as char.
def var PanEntryMode as char. 
def var PosCondition as char. 
def var PosDataCode as char. 
def var ResponseCode as char.
def var RetrievalRefNr as char.  
def var TerminalId as char. 
def var TransactionAmount as char.
def var TransactionId  as char.
def var vType  as char.
DEF VAR j AS INT.
DEF VAR i AS INT.
DEF VAR SAmount AS CHAR.

/*API intergration*/
DEFINE VARIABLE HttpClient AS CLASS System.Net.WebClient. 
DEFINE VARIABLE webResponse AS LONGCHAR NO-UNDO. 
DEF VAR txtWebResponse AS CHAR.
DEF VAR vTemp AS CHAR.
DEF VAR txtUnWantedR AS CHAR EXTENT 12.
DEF VAR rspCode AS CHAR.
DEF VAR rspMessage AS CHAR.

DEFINE VARIABLE cJson AS CHARACTER NO-UNDO.  
DEFINE VARIABLE receiptCurrency AS CHARACTER NO-UNDO.  
DEFINE VARIABLE receiptNotes AS CHARACTER NO-UNDO.  
DEFINE VARIABLE receiptDate AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.

DEFINE VARIABLE sellerName as character.
DEFINE VARIABLE sellerTradeName as character.
DEFINE VARIABLE sellervatNumber as character.
DEFINE VARIABLE sellerTIN as character.
DEFINE VARIABLE sellerPhone as character.
DEFINE VARIABLE sellerEmail as character.
DEFINE VARIABLE sellerProvince as character.
DEFINE VARIABLE sellerCity as character.
DEFINE VARIABLE sellerStreet as character.
DEFINE VARIABLE sellerHouseNumber as character.
DEFINE VARIABLE sellerDistrict as character.

DEFINE VARIABLE buyerName as character.
DEFINE VARIABLE buyerTradeName as character.
DEFINE VARIABLE vatNumber as character.
DEFINE VARIABLE buyerTIN as character.
DEFINE VARIABLE buyerPhone as character.
DEFINE VARIABLE buyerEmail as character.
DEFINE VARIABLE buyerProvince as character.
DEFINE VARIABLE buyerCity as character.
DEFINE VARIABLE buyerStreet as character.
DEFINE VARIABLE buyerHouse as character.
DEFINE VARIABLE buyerDistrict as character.

DEFINE  VARIABLE receiptLineType AS CHAR.
DEFINE VARIABLE hashstring AS CHAR.
DEFINE VARIABLE sigstring AS CHAR.
DEF VAR recGNumber AS INTEGER.
DEF VAR recCounter AS INTEGER.
DEF VAR varHash AS CHAR.
DEF VAR moneyTypeCode AS CHAR FORM "x(4)".

DEFINE VARIABLE w_password LIKE dbrecop.txtpass /*AS CHARACTER FORMAT "X(8)":U  */
     LABEL "CURRENT Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.
DEFINE VARIABLE sup-password AS CHARACTER FORMAT "X(20)":U 
     LABEL "Over-ride Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.
define variable hCol as widget-handle.
DEF BUTTON btnLedger LABEL "LEDGER  ".
DEF BUTTON btnAcc    LABEL "ACCOUNT  ".
DEF BUTTON btnCode LABEL "RECEIPT CODE ".
DEF BUTTON btnMeth LABEL "PAYMENT METHOD  ".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-Ok  LABEL "OK".
DEF BUTTON btnclose  LABEL "CLOSE".
DEF BUTTON  btnSearch LABEL "SEARCH".
DEF BUTTON  btnCur LABEL "CURRENCY".
DEF BUTTON btnprn  LABEL "RE-PRINT".
DEF BUTTON btnClear LABEL "CLEAR".
DEF VAR txtqUrl AS CHAR.
DEF VAR varInvoice  AS CHAR FORM "x(1)" INITIAL "N".
DEF BUTTON btnInvoice LABEL "INVOICE NUMBER".
DEF VAR varInvoiceNumber AS INTEGER FORM "zzzzzzzzz9".
DEF VAR varInvoiceDescrip AS CHAR FORM "x(15)".

DEF TEMP-TABLE tmpTax no-undo
    FIELD taxCode AS CHAR
    FIELD taxPercent AS DECIMAL
    FIELD taxID AS INTEGER
    FIELD taxAmount AS DECIMAL
    FIELD salesAmountWithTax AS DECIMAL.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEF QUERY qry-Land FOR hsecmf SCROLLING.
DEF BROWSE brw-Land QUERY qry-Land
    DISPLAY Hsecmf.dbAcc Hsecmf.Name Hsecmf.Scheme Hsecmf.StandNo Hsecmf.AmtDue[1] COLUMN-LABEL "USD"
    Hsecmf.AmtDue[2] COLUMN-LABEL "ZWG" FORM "z,zzz,zzz,zz9.99-"
     WITH NO-LABEL 10 DOWN SEPARATORS.

DEF QUERY qry-Cust FOR tblDetail SCROLLING.
DEF BROWSE brw-cust QUERY qry-cust
    DISPLAY wsVar LABEL "CODE" WIDTH 5 varServ LABEL "Description" FORM "X(25)"
    varBal FORM "zzz,zzz,zz9.99-" LABEL "Amount Due" varbal * dbRate  LABEL "ZWG Due" FORM "zz,zzz,zzz,zz9.99-"
     WITH NO-LABEL 10 DOWN SEPARATORS.

DEF QUERY qry-Cur FOR tblForex SCROLLING.
DEF BROWSE brw-cur QUERY qry-cur
    DISPLAY txtCur FORM "x(10)"
     WITH NO-LABEL 6 DOWN SEPARATORS.

DEF QUERY qry-Rec FOR tmpbfrDbRec SCROLLING.
DEF BROWSE brw-Rec QUERY qry-Rec
    DISPLAY tmpbfrDbRec.SeqNo COLUMN-LABEL "SEQ" tmpbfrDbRec.Account tmpbfrDbRec.rCode
       tmpbfrDbRec.Amount FORM "z,zzz,zzz,zz9.99-" WITH 10 DOWN SEPARATORS.

DEF    QUERY qry-pickAcc FOR bfrDbcmf SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
    DISPLAY bfrDbcmf.dbAcc bfrDbcmf.Name bfrDbcmf.StandNo bfrDbcmf.stno LABEL "StreetNo" bfrDbcmf.street LABEL "Street" wsVar LABEL "Suburb" 
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Cur 
    brw-Cur AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 3
    btnclose colon 10
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Currency Selection".

DEFINE FRAME frm-pickAcc 
    brw-pickAcc AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

DEFINE FRAME frm-pickLAcc 
    brw-Land AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

DEF    QUERY qry-pickMet FOR dbPay SCROLLING.
DEF BROWSE brw-pickMet QUERY qry-pickMet
    DISPLAY dbPay.Paytype dbPay.descrip 
     WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-pickMet 
    brw-pickMet AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payment Method Selection".

DEF    QUERY qry-pickCode FOR dbrcod SCROLLING.
DEF BROWSE brw-pickCode QUERY qry-pickCode
    DISPLAY dbRCod.rCode dbRCod.Descrip 
     WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-pickCode 
    brw-pickCode AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Receipting Code Selection".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 115 by 11.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 173 by 18.5.
/* general Format */
FORM "CASHIER                 :" COLON 1 varName  NO-LABEL
    "CUSTOMER NAME           :" COLON 1 wsRecDetails NO-LABEL
    "CUSTOMER TIN            :" COLON 1 wsTinNo    NO-LABEL
    "CUSTOMER VAT            :" COLON 1 wsVatNo    NO-LABEL
    "CUSTOMER ADDRESS        :" COLON 1 wsadd      NO-LABEL
    "CUSTOMER PHONE          :" COLON 1 wsPhone    NO-LABEL
    "CUSTOMER EMAIL ADDRESS  :" COLON 1 wsemail    NO-LABEL
    wsline[2] COLON 1 NO-LABEL
    "R;" COLON 1
     HEADER  wsTitle SKIP 
            wsCashier SKIP 
            wsTitle1  
            /*wsAdd SKIP
            wsRecDetails  SKIP(1) */
    WITH WIDTH 90 DOWN STREAM-IO FONT 12 NO-BOX NO-LABEL FRAME frm-recHdr.

FORM wsCashier
     HEADER  wsTitle SKIP 
         wsTitle1
            /*wsAdd SKIP
            wsRecDetails  SKIP(1) */
    WITH WIDTH 90 DOWN STREAM-IO FONT 12 NO-BOX NO-LABEL FRAME frm-recHdr1.

FORM wsAcc    COLON 1  NO-LABEL
     "," COLON 16 
     dbRCod.Descrip  NO-LABEL 
     ','   COLON 49 
     wstax    NO-LABEL
     ','   COLON 60 
     varAmount NO-LABEL FORM "zzzzzzzz9.99-"
    /*HEADER  wsTitle SKIP 
            wsCashier SKIP 
            wsTitle1 SKIP  
            wsAdd SKIP
            wsRecDetails SKIP(1)
    "R;" */
    WITH WIDTH 90 DOWN STREAM-IO FONT 12 NO-BOX FRAME frm-rec.

/* begin Chipinge format */
/*FORM wsAcc    COLON 1  NO-LABEL
     "," COLON 16 
     dbRCod.Descrip  NO-LABEL 
     ','   COLON 49 
     wstax    NO-LABEL
     ','   COLON 60 
     varAmount       NO-LABEL 
    HEADER  wsTitle SKIP 
            wsCashier SKIP 
            wsTitle1 SKIP  
            wsRecDetails SKIP
            wsAdd SKIP
    "R;" 
    WITH DOWN STREAM-IO FONT 10 WIDTH 100 NO-BOX FRAME frm-rec.
*/
FORM SKIP(1)
    "CURRENCY TENDERED:" varCur COLON 27  NO-LABEL SKIP
    "AMOUNT           :" varTotal NO-LABEL SKIP
    "RATE             :" dbRate   NO-LABEL SKIP
    "USD EQUIVALENT   :" wsEquv   NO-LABEL  SKIP
    "AMOUNT TENDERED  :" varTend  NO-LABEL SKIP
    "CHANGE           :" varChg   NO-LABEL SKIP
    "DISCOUNT RECEIVED:" wsDisc   NO-LABEL  SKIP /* CVF */
    "BALANCE BEFORE RECEIPT"    bBal NO-LABEL COLON 44 bcur NO-LABEL SKIP
    "NEW BALANCE AFTER RECEIPT" aBal NO-LABEL COLON 44 acur NO-LABEL SKIP(1)
     WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-footer.

FORM SKIP(1)
     "CURRENCY TENDERED:" varCur COLON 27  NO-LABEL SKIP
     "AMOUNT           :" varAmount NO-LABEL SKIP
     ActionCode NO-LABEL "   :" MessageReasonCode   NO-LABEL  SKIP
     "MERCHANT ID             :" MerchantId   FORM 'X(20)' NO-LABEL SKIP
    "TERMINAL ID   :" TerminalId   FORM 'X(8)'  NO-LABEL  SKIP
    "CARD NUMBER             :" CardNumber   FORM 'X(20)'  NO-LABEL SKIP
    "PRODUCT   :" CardProductName   FORM 'X(15)'  NO-LABEL  SKIP
    "RETRIEVAL REFRENCE NUMBER  :" RetrievalRefNr  FORM 'X(15)'  NO-LABEL SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-footer1.

/* End Chipinge format */

/* begin of non-fiscal format */
FORM wsAcc    COLON 1  NO-LABEL
     dbRCod.Descrip  NO-LABEL  
     varAmount       NO-LABEL 
    HEADER  wsTitle SKIP 
            wsCashier SKIP 
            wsTitle1 SKIP  
            wsRecDetails SKIP
            wsAdd
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-nonFrec.
  FORM 
    SKIP (1) 
     "TOTAL   :" COLON 30  varTotal NO-LABEL  FORM "zzzzzzzz9.99-" SKIP
     "TENDERED:" COLON 30  varTend NO-LABEL SKIP
     "CHANGE  :" COLON 30  varChg NO-LABEL SKIP
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-NonFrec1.       
/* end of non-fiscal format */ 

FORM 
    SKIP(1)
     wsmark  COLON 1 NO-LABEL
     "   TOTAL:" COLON 42  varTotal NO-LABEL FORM "zzzzzzzz9.99-" SKIP
     "TENDERED:" COLON 42  varTend NO-LABEL  FORM "zzzzzzzz9.99-" SKIP
     "  CHANGE:" COLON 42  varChg NO-LABEL   FORM "zzzzzzzz9.99-" SKIP
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-rec1.
    

FORM
    dbPay.descrip LABEL "DESCRIPTION" wsAmt LABEL "AMOUNT" 
     HEADER "EOD" SKIP
     wsTitle1  SKIP
     wsCashier SKIP
     wsTitle SKIP(1)
    
    "------COLLECTION SUMMARY------"SKIP(1)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 CENTERED NO-BOX FRAME frmEod.

DEFINE FRAME frm-amount
    SKIP (1)
    varAmount  /*BGCOLOR 1 FGCOLOR 12 */ COLON 10 FORM "zzz,zzz,zz9.99" LABEL "Amount" SKIP(1)
     btn-ok LABEL "Enter" COLON 5  SPACE(5) btn-exit NO-TAB-STOP LABEL "Cancel"
    WITH SIZE 40 BY 6 view-as dialog-box keep-tab-order   no-validate
         side-labels no-underline CENTERED THREE-D
   TITLE "SWIPE AMOUNT".

DEF VAR varFDnumber LIKE fdmsRecCounter.fDNumber.
DEF VAR varDeviceID LIKE fdmsDevice.DeviceID.

DEFINE FRAME frm-input 
    SKIP(1)
    varRecNo    AT ROW 2 COL 6 LABEL "RECEIPT NUMBER" VIEW-AS TEXT NO-TAB-STOP
    varDate     AT ROW 2 COL 95 LABEL "RECEIPT DATE" VIEW-AS TEXT NO-TAB-STOP SPACE(10)
    varFDNumber AT ROW 2 COL 130 LABEL "FISCAL DAY NUMBER" NO-TAB-STOP VIEW-AS TEXT 
    varType     AT ROW 3 COL 6         LABEL "CUSTOMER/VOTE" AUTO-RETURN
               view-as combo-box size 20 by 2 LIST-ITEM-PAIRS
      "C - Customer Account","C","V - Direct to Vote","V", "L - Landsales Account","L" 
    btnMeth                    varMeth NO-LABEL
    dbPay.descrip AT ROW 3.3 COL 72 NO-LABEL NO-TAB-STOP VIEW-AS TEXT
    varCur        AT ROW 3.3 COL 95 LABEL "CURRENCY" VIEW-AS TEXT SPACE(5)
     varDeviceID AT ROW 3.3 COL 130 LABEL "FDMS DEVICE ID" NO-TAB-STOP VIEW-AS TEXT 
    wsdisRate    AT ROW 4.6 COL 95  LABEL "TODAY's USD RATE"  VIEW-AS TEXT  FONT 6
    /*wsd1Rate    LABEL "Plus 10%" VIEW-AS TEXT FGCOLOR 12 FONT 6 */ SKIP(0.5)  /* CVF*/
    btnAcc      AT ROW 4.6 COL 6 AUTO-RETURN
    varAcc      NO-LABEL AUTO-RETURN
    SPACE(1)  bfrDbcmf.NAME   NO-LABEL NO-TAB-STOP VIEW-AS TEXT SPACE (35)
    varInvoice     AT ROW 5.9 COL 6         LABEL "PAYING INVOICE?" AUTO-RETURN
               view-as combo-box size 20 by 2 LIST-ITEM-PAIRS
      "N - NO","N","Y - YES","Y" 
    btnInvoice                    varInvoiceNumber NO-LABEL
   varInvoiceDescrip AT ROW 6.2 COL 72 NO-LABEL NO-TAB-STOP VIEW-AS TEXT

    wsReRate  AT ROW 5.9 COL 95   LABEL "RECEIPTING USD RATE"  VIEW-AS TEXT  FONT 6 
    varTend    /*BGCOLOR 1 FGCOLOR 12 */ AT ROW 7.2 COL 6 FORM "zzz,zzz,zz9.99" LABEL "AMOUNT TENDERED" 
    btnCode       AT ROW 8.5 COL 6 NO-TAB-STOP
    varRCode   /* BGCOLOR 1 FGCOLOR 12 */    NO-LABEL AUTO-RETURN
    dbRCod.Descrip NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 20 by 1 
    varDescrip /* BGCOLOR 1 FGCOLOR 12  */ AT ROW 9.8 COL 6 LABEL "RECEIPT DETAILS" VIEW-AS FILL-IN SIZE 35 BY 1
    varAmount  /*BGCOLOR 1 FGCOLOR 12 */ AT ROW 11.1 COL 6 FORM "zzz,zzz,zz9.99" LABEL "AMOUNT" 
    varloop     AT ROW 12.3 COL 6 LABEL "MORE ALLOCATIONS Y/N?" 
    varRef     /* BGCOLOR 1 FGCOLOR 12 */ AT ROW 13.6 COL 6 LABEL "PAYMENT REFERENCE" 
    varChg      AT ROW 14.9 COL 6 LABEL "BALANCE" VIEW-AS TEXT NO-TAB-STOP 
    wsReceipts  AT ROW 17.2 COL 6 FORM "zzz,zzz,zz9.99-" LABEL "UNPOSTED RECEIPTS = "  VIEW-AS TEXT
   
    btn-exit AT ROW 21 COL 6 NO-TAB-STOP  SPACE(70) btnprn SPACE(70) btnClear
    "YOU RECEIPT DETAILS" AT ROW 7.5 COL 65
    "ACCOUNT SERVICES DUE" AT ROW 7.5 COL 115
    wsTot[1]   AT ROW 17.2 COL 120 LABEL "TOTALS" wsTot[2] NO-LABEL 
    brw-cust   AT ROW 8.5 COL 108 NO-TAB-STOP
    brw-rec   AT ROW 8.5 COL 60   NO-TAB-STOP
    varDue    AT ROW 17.2 COL 83 LABEL "AMOUNT DUE" VIEW-AS TEXT NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 7.4 COL 59.4
    WITH SIZE 180 BY 23 view-as dialog-box keep-tab-order   no-validate
         side-labels no-underline CENTERED THREE-D
    TITLE "RECEIPT CAPTURE".

DEFINE FRAME frm-pass
    SKIP(1.0)
     simusr.NAME COLON 10 VIEW-AS TEXT LABEL "USER" NO-TAB-STOP SKIP(0.5)
     w_password   COLON 15 LABEL "PASSWORD"   PASSWORD-FIELD SKIP(1)
     btn-ok       COLON 20
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 50 ROW 5.19  
         SIZE 45 BY 6.91
         BGCOLOR 3 FGCOLOR 0 
         CANCEL-BUTTON Btn-exit TITLE "RECEIPTING PASSWORD VERIFICATION".


DEFINE FRAME frm-pass1
    SKIP(1.0)
     wsSup COLON 15 LABEL "Over-ride User"  SKIP(0.5)
     Sup-password   COLON 15 LABEL "PASSWORD"   PASSWORD-FIELD SKIP(1)
     btn-ok       COLON 20
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 50 ROW 5.19  
         SIZE 45 BY 6.91
         BGCOLOR 3 FGCOLOR 0 
         CANCEL-BUTTON Btn-exit TITLE "RECEIPTING DATE OVER-RIDE".

/*edit */
ON 'mouse-select-dblclick' OF brw-Rec IN FRAME frm-Input
 DO:
    GET CURRENT qry-Rec EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN varChg = varchg + tmpbfrDbRec.Amount
           varDue = varDue - tmpbfrDbRec.Amount.
    DISPLAY varChg varDue WITH FRAME frm-INPUT.
    DELETE tmpbfrDbRec.
    Method-Status = brw-Rec:DELETE-SELECTED-ROWS().
END.

ON 'choose':U OF btnprn 
DO:
    /*RUN reprn.ip. 
    APPLY 'entry' TO varAcc IN FRAME frm-Input.*/
    MESSAGE "BUTTON DISABLED" VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
END.
on 'start-search':u of browse brw-pickacc
    run Search.ip.

ON 'choose':U OF btnSearch IN FRAME Search-opt 
    OR 'enter':U OF btnSearch IN FRAME Search-opt
    OR 'tab':U OF btnSearch IN FRAME Search-opt
DO:
    open query qry-pickacc preselect 
                    each bfrDbcmf no-lock 
                        where bfrDbcmf.SortName >= wsSearch:SCREEN-VALUE
                           BY bfrDbcmf.Sortname.
    /*RETURN.*/
END.

ON 'enter':U OF varType IN FRAME frm-input
OR  'LEAVE':U OF varType IN FRAME frm-input
DO:
    ASSIGN varType.
    IF varType = "V" THEN DO:
        varAcc:SCREEN-VALUE = "0".
        varDescrip:SCREEN-VALUE = "".
        DISABLE varAcc btnAcc WITH FRAME frm-input.
        ENABLE varDescrip WITH FRAME frm-input.
    END.
    ELSE IF (varType:SCREEN-VALUE = "C" OR varType:SCREEN-VALUE = "L") THEN DO:
        ENABLE varAcc btnAcc WITH FRAME frm-input.
        /*APPLY 'entry' TO varAcc. */     
    END.
    APPLY 'entry' TO btnMeth. 
    /*RETURN NO-APPLY.*/
END.

ON 'enter':U OF varAcc IN FRAME frm-input 
    OR 'tab':U OF varAcc IN FRAME frm-input
DO:
    ASSIGN varAcc.
    IF DEC(varAcc:SCREEN-VALUE) = 0 THEN DO:
       APPLY 'ENTRY' TO btnClear IN FRAME frm-input.
       RETURN NO-APPLY.
    END.
    IF varType:SCREEN-VALUE = "C" THEN DO:
        FIND FIRST bfrDbcmf WHERE bfrDbcmf.dbAcc = DEC(varAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE bfrDbcmf THEN DO:
            DISPLAY bfrDbcmf.NAME "ZZ" @ varRcode WITH FRAME frm-input.
            varbal = 0.
            varDescrip = bfrDbcmf.NAME.
            ASSIGN wsRecDetails = bfrDbcmf.NAME 
                   wsAdd = TRIM(bfrDbcmf.stno) + " " + TRIM(bfrDbcmf.street)
                   wsVatNo = bfrDbcmf.vatNo
                   wsTinNo = bfrDbcmf.TIN
                   wsPhone = bfrDbcmf.Cell
                   wsemail = bfrDbcmf.emailadd.
            DISPLAY varDescrip WITH FRAME frm-Input.
            DISABLE varDescrip WITH FRAME frm-Input.
            RUN view-trans.ip.
        END.
        ELSE DO:
            MESSAGE "Invalid Account entered" VIEW-AS ALERT-BOX.
            APPLY 'entry' TO SELF.
             RETURN NO-APPLY.
        END.
    END.
    ELSE IF varType:SCREEN-VALUE = "L" THEN DO:
        FIND FIRST hsecmf WHERE hsecmf.dbAcc = DEC(varAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE hsecmf THEN DO:
            DISPLAY hsecmf.NAME @ bfrDbcmf.NAME WITH FRAME frm-input.
            varbal = 0.
            varDescrip = hsecmf.NAME.
            ASSIGN wsRecDetails = hsecmf.NAME 
                   wsAdd = TRIM(hsecmf.Add1) + " " + TRIM(hsecmf.add2)
                   wsVatNo = hsecmf.vatNo
                   wsTinNo = hsecmf.TIN.
                   wsPhone = hsecmf.cell[1].
                   wsemail = hsecmf.emailadd.
            DISPLAY varDescrip WITH FRAME frm-Input.
            DISABLE varDescrip WITH FRAME frm-Input.
            RUN view-trans.ip.
        END.
        ELSE DO:
            MESSAGE "Invalid Account entered" VIEW-AS ALERT-BOX.
            APPLY 'entry' TO SELF.
             RETURN NO-APPLY.
        END.
    END.
END.

ON enter anywhere 
DO:
    APPLY 'tab' TO SELF.
END.

ON 'enter':U OF varRCode IN FRAME frm-input 
    OR 'tab':U OF varRCode IN FRAME frm-input
DO:
    ASSIGN varRCode.
    /*IF varRCode = "8" THEN DO:
        /*for Chipinge to avoid receipting land sales to consumer */
        MESSAGE "Use landsale. This code disabled." VIEW-AS ALERT-BOX.
        APPLY 'ENTRY' TO SELF.
        RETURN NO-APPLY.
    END.
    ELSE DO:*/
    IF varType:SCREEN-VALUE = "C" THEN 
        FIND FIRST dbrCod WHERE dbRCod.rCode = varrCode:SCREEN-VALUE 
                            AND (dbrcod.TARGET = "C" OR dbrcod.TARGET = "D") NO-LOCK NO-ERROR.
    ELSE IF varType:SCREEN-VALUE = "L" THEN 
            FIND FIRST dbrCod WHERE dbRCod.rCode = varrCode:SCREEN-VALUE 
                                AND dbRCod.TARGET = "L"  NO-LOCK NO-ERROR.
    ELSE FIND FIRST dbrCod WHERE dbRCod.rCode = varrCode:SCREEN-VALUE 
                                     AND dbrcod.TARGET = "V"  NO-LOCK NO-ERROR.
    IF AVAILABLE dbrCod THEN
            DISPLAY dbRCod.Descrip WITH FRAME frm-input.
     ELSE DO:
         MESSAGE "Invalid Receipting Code" VIEW-AS ALERT-BOX.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
    END.
END.

ON 'enter':U OF varDescrip IN FRAME frm-input 
    OR 'tab':U OF varDescrip IN FRAME frm-input 
    OR 'LEAVE':U OF varDescrip IN FRAME frm-input 
DO:
    ASSIGN varDescrip.
    IF varDescrip = "" THEN DO:
       MESSAGE "Please enter DESCRIPTION" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    ELSE
        ASSIGN wsRecDetails = varDescrip.
               /*wsAdd = "".  ? */
    RETURN.
END.

ON 'enter':U OF varAmount IN FRAME frm-input 
    OR 'tab':U OF varAmount IN FRAME frm-input
DO:
    ASSIGN varAmount.
    IF varAmount > varChg THEN DO:
        MESSAGE "Amount cannot be greater than tendered amount." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    IF varAmount > 0 THEN DO:
       RUN rec-create.ip.
       varChg = varTend - varDue.
       DISPLAY varChg varDue WITH FRAME frm-input.
       ENABLE brw-rec WITH FRAME frm-input.
       OPEN QUERY qry-rec FOR EACH tmpbfrDbRec.
    END.
    ELSE IF varTend = varDue THEN DO:
        varLoop:SCREEN-VALUE = "N".
        APPLY 'TAB' TO varLoop IN FRAME frm-input.
        RETURN NO-APPLY.
    END. 
END.

ON 'tab':U OF varLoop IN FRAME frm-input
    OR 'enter':U OF varLoop IN FRAME frm-input
DO:
    /* */
    ASSIGN varLoop.
    IF varLoop = "Y" THEN DO: 
         OPEN QUERY qry-rec FOR EACH tmpbfrDbRec.
        DISABLE brw-rec WITH FRAME frm-input.
        DISABLE brw-cust WITH FRAME frm-input.
        APPLY "entry" TO varRCode IN FRAME frm-input.
        RETURN NO-APPLY.
    END.  
    ELSE IF varLoop = "N" THEN DO:
        IF dec(varTend:SCREEN-VALUE) > dec(varDue:SCREEN-VALUE) AND SUBSTR(dbPay.descrip,1,4) <> "CASH" THEN DO:
            MESSAGE "Amount tendered TO equal Amount Due IF NOT cash" VIEW-AS ALERT-BOX.
            APPLY "entry" TO varLoop IN FRAME frm-input.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN wsTitle = "VAT INVOICE/RECEIPT"
                   wsCashier = "REC:" + "     " + string(varRecNo, ">>>>>>9") 
                   + 
                   "        " + STRING(DATE(VarDate),"99/99/9999")
                   + "  TIME: " + substr(string(time,"HH:MM:SS"),1,5)
                   + "   MC NO:" + STRING(varUser, "999").               
            ENABLE varRef WITH FRAME frm-input.
        END.  
    END.
END.


ON 'leave':U OF varTend IN FRAME frm-input 
    OR 'tab':U OF varTend IN FRAME frm-input
DO:
   ASSIGN varTend = DEC(varTend:SCREEN-VALUE).
   IF varTend = 0 THEN DO:
      MESSAGE "Amount cannot be zero" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   varchg = varTend - varDue.
   DISPLAY varTend @ varChg WITH FRAME frm-input.
END.

ON 'enter':U OF varRef IN FRAME frm-input 
   OR 'LEAVE':U OF varRef IN FRAME frm-input 
DO:
    ASSIGN varChg  = DEC(varChg:SCREEN-VALUE)
           varRef  = varRef:SCREEN-VALUE.
    /*IF allow-disc = YES THEN DO: /*payment Discount */
        FIND FIRST dbdischt WHERE dbdischt.cons = vartype AND dbdischt.txtcur = varcur AND dbdischt.trdate = varDate NO-LOCK NO-ERROR.
        IF AVAILABLE dbdischt THEN
            wsdis = dbdischt.disc.
        ELSE wsdis = 0.
    END.*/
    IF CAN-FIND (FIRST tmpbfrDbRec) AND simctr.cocode = "MTK" THEN
       RUN rec-nonFprn.ip.
    ELSE IF CAN-FIND (FIRST tmpbfrDbRec) THEN
       /*RUN rec-prn.ip.*/
        RUN rec-prnFS.ip.
    CLOSE QUERY qry-Cust.
    FOR EACH tmpbfrDbRec:
       DELETE tmpbfrDbRec.
    END.
    FOR EACH tmpTax.
        DELETE tmpTax.
    END.
    
    CLEAR FRAME frm-input ALL.
    ASSIGN wsRecDetails = ""
           wsTINNo = 0
           wsVatNo = 0.
    varMeth:SCREEN-VALUE = "1".
    ENABLE ALL WITH FRAME frm-input.
    ASSIGN  varRecNo = varRecNo + 1
            varDue  = 0
            X = 0.
    DISPLAY varRecNo varDate wsdisRate wsReRate varFDNumber varDeviceID WITH FRAME frm-input.
    varCur:SCREEN-VALUE = "ZWG".
    varloop:SCREEN-VALUE = "N".
    DISABLE  varRef varTend WITH FRAME frm-input.
    APPLY 'ENTRY' TO vartype IN FRAME frm-input.
    RETURN NO-APPLY.
END.
/* On Screen buttons triggers */

ON CHOOSE OF btnClear IN FRAME frm-Input 
    OR 'enter':u OF btnClear IN FRAME frm-input
DO:
    FOR EACH tmpbfrDbRec:
       DELETE tmpbfrDbRec.
    END.
    CLEAR FRAME frm-input ALL.
    ASSIGN wsRecDetails = ""
           wsVATno = 0
           wsTinNo = 0.
    varMeth:SCREEN-VALUE = "1".
    ENABLE ALL EXCEPT varTend varRef WITH FRAME frm-input.
    ASSIGN  varDue  = 0
            X = 0.
    DISPLAY varRecNo varDate wsdisRate wsReRate varFDNumber varDeviceID WITH FRAME frm-input.
   varCur:SCREEN-VALUE = "ZWG".
    varloop:SCREEN-VALUE = "N".
    DISABLE  varRef  WITH FRAME frm-input.
    APPLY 'ENTRY' TO vartype IN FRAME frm-input.
    RETURN.
END.

/* BtnAcc triggers */
ON CHOOSE OF btnAcc IN FRAME frm-Input
DO:
  IF varType = "C" THEN DO:
      VIEW FRAME frm-pickAcc.
      OPEN QUERY qry-pickAcc FOR EACH bfrDbcmf NO-LOCK.
      ENABLE ALL WITH FRAME frm-pickAcc.
      WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickAcc 
              OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
              OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
              OR 'enter':u OF brw-pickAcc
              OR 'mouse-select-dblclick' OF brw-pickAcc.
      CLOSE QUERY qry-pickAcc.
      HIDE FRAME frm-pickAcc.
  END.
  ELSE IF varType = "L" THEN DO:
      VIEW FRAME frm-pickLAcc.
      OPEN QUERY qry-Land FOR EACH hsecmf NO-LOCK.
      ENABLE ALL WITH FRAME frm-pickLAcc.
      WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickLAcc 
              OR close of THIS-PROCEDURE IN FRAME frm-pickLAcc 
              OR CHOOSE OF btn-ok IN FRAME frm-pickLAcc 
              OR 'enter':u OF brw-Land
              OR 'mouse-select-dblclick' OF brw-land.
      CLOSE QUERY qry-land.
      HIDE FRAME frm-pickLAcc.
  END.
  APPLY 'tab' TO btnAcc.
  APPLY 'tab' TO varAcc.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-pickAcc 
    OR 'mouse-select-dblclick' OF brw-pickAcc 
DO: 
   GET CURRENT qry-pickAcc  EXCLUSIVE-LOCK NO-WAIT.
    varAcc = bfrDbcmf.dbacc.
   DISPLAY  varAcc bfrDbcmf.NAME WITH FRAME frm-Input.
   APPLY 'tab' TO varAcc IN FRAME frm-input.
   RETURN.
END.


ON CHOOSE OF btn-exit IN FRAME frm-Amount 
    OR 'enter':u OF btn-exit IN FRAME frm-Amount 
DO: 
   varMeth:SCREEN-VALUE IN FRAME frm-input = '1'.
    HIDE FRAME frm-amount.
END.

ON CHOOSE OF btn-ok IN FRAME frm-Amount 
    OR 'enter':u OF btn-ok IN FRAME frm-Amount 
DO: 
    IF DEC(varAmount:SCREEN-VALUE IN FRAME frm-Amount) <> 0 THEN DO:
           varAmount = DEC(varAmount:SCREEN-VALUE IN FRAME frm-Amount) * 100.
           RUN send-trans.
           RETURN.
           
    END.
    ELSE DO:
        MESSAGE "Amount has to be greater than 0" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
   
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickLAcc 
    OR 'enter':u OF brw-land 
    OR 'mouse-select-dblclick' OF brw-land 
DO: 
   GET CURRENT qry-land  EXCLUSIVE-LOCK NO-WAIT.
    varAcc = hsecmf.dbacc.
   DISPLAY  varAcc hsecmf.NAME @ bfrDbcmf.NAME WITH FRAME frm-Input.
   APPLY 'tab' TO varAcc IN FRAME frm-input.
   RETURN.
END.

ON row-display OF brw-PickAcc DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = bfrDbcmf.suburb NO-LOCK NO-ERROR.
    wsVar = dbsmf.Descrip.
END. 

ON row-display OF brw-cust DO:
    varbal = tblDetail.dbAmt[1].
    IF tblDetail.vat <> 0 THEN DO:
        FIND FIRST dbRCod WHERE dbRCod.Sgrp = tblDetail.Sgrp AND dbrcod.vat% <> 0 NO-LOCK NO-ERROR.
        IF AVAILABLE dbrcod THEN
           wsVar = dbrcod.rcode.
    END.
    ELSE 
        FIND FIRST dbRCod WHERE dbRCod.Sgrp = tblDetail.Sgrp NO-LOCK NO-ERROR.
        IF AVAILABLE dbrcod THEN
           wsVar = dbrcod.rcode.
        ELSE wsVar = "".
    IF dbRCod.target = "C" THEN DO:
        FIND FIRST dbsgr WHERE dbsgr.Sgrp = tblDetail.Sgrp NO-LOCK NO-ERROR.
        IF AVAILABLE dbsgr THEN
            varServ = dbsgr.descrip.
        ELSE 
            varServ = "".
    END.
    IF vartype = "L" THEN
       ASSIGN  wsVar = "LS"
               varServ = "Land Sales".
END. 
/* BtnMet triggers */
ON CHOOSE OF btnMeth IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickMet.
  OPEN QUERY qry-pickMet FOR EACH dbPay NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickMet.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickMet 
          OR close of THIS-PROCEDURE IN FRAME frm-pickMet 
          OR CHOOSE OF btn-ok IN FRAME frm-pickMet 
          OR 'enter':u OF brw-pickMet
          OR 'mouse-select-dblclick' OF brw-pickMet.
  CLOSE QUERY qry-pickMet.
  HIDE FRAME frm-pickMet.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO varMeth.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickMet 
    OR 'enter':u OF brw-pickMet 
    OR 'mouse-select-dblclick' OF brw-pickMet 
DO: 
   GET CURRENT qry-pickMet NO-LOCK NO-WAIT.
    ASSIGN varMeth = dbPay.Paytype.
    IF substring(dbPay.descrip,1,3) = 'EFT' THEN DO:
        IF varTerminal <> "" THEN DO:
            DISPLAY varMeth  dbPay.descrip WITH FRAME frm-input.
            APPLY 'tab' TO varMeth IN FRAME frm-input.
        END.
        ELSE DO:
            MESSAGE "Cashier is not allocated to an EFT Terminal" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
   END.
    ELSE DO:
        DISPLAY  varMeth dbPay.descrip  WITH FRAME frm-Input.
        APPLY 'tab' TO varMeth IN FRAME frm-input.
        RETURN.
    END.
   
END.

ON 'enter':U OF varMeth IN FRAME frm-input 
    OR 'tab':U OF varMeth IN FRAME frm-input
    OR 'leave':U OF varMeth IN FRAME frm-input
DO:
   ASSIGN varMeth = INT(varMeth:SCREEN-VALUE).
   ENABLE varTend WITH FRAME frm-input.
    FIND FIRST dbPay WHERE dbPay.Paytype = varMeth NO-LOCK NO-ERROR.
       IF AVAILABLE dbPay THEN DO:
        FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
        ASSIGN varCur  = cbkmf.txtCur
               wsMark  = "P;" + SUBSTR(dbpay.descrip,1,4) /*"CASH"  */.
        DISPLAY dbPay.descrip varCur WITH FRAME frm-input.
        IF varMeth = 1 THEN
            varRef:SCREEN-VALUE = "CASH".
        FIND FIRST tblForex WHERE tblForex.txtCur = varCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
              ASSIGN rdbRate = tblForex.decRate.
        END.
        ELSE IF NOT AVAILABLE tblForex  THEN DO:
           FIND LAST tblForexH WHERE tblForexH.txtCur = varCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
           IF AVAILABLE tblForexH THEN DO:
               ASSIGN rdbRate = tblForexH.decRate.
            END.
            ELSE IF NOT AVAILABLE tblForexH  THEN DO:
                FIND FIRST tblForexH WHERE  tblForexH.txtCur = varCur AND tblForexH.dtRate >= varDate NO-LOCK NO-ERROR.
                IF AVAILABLE tblForexH THEN DO:
                ASSIGN rdbRate = tblForexH.decRate.
                
            END.
            END.
        END.
            IF  substring(dbPay.descrip,1,3) <> "EFT" THEN DO:
                DISABLE varMeth BtnMeth WITH FRAME frm-Input.
            END.
            ELSE IF  substring(dbPay.descrip,1,3) = "EFT" THEN DO:
                VIEW FRAME frm-Amount.
                ENABLE ALL WITH FRAME frm-Amount.
                
            END.
    END.
    ELSE DO:
        MESSAGE "Invalid Payment Method" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
END. 

/* BtnCode triggers */
ON CHOOSE OF btnCode IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickCode.
  IF varType:SCREEN-VALUE = "C" THEN
     OPEN QUERY qry-pickCode FOR EACH dbrcod WHERE dbRCod.TARGET = "C" OR dbRCod.TARGET = "D" NO-LOCK.
   ELSE
     OPEN QUERY qry-pickCode FOR EACH dbrcod WHERE dbRCod.TARGET = "V" NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickCode.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickCode 
          OR close of THIS-PROCEDURE IN FRAME frm-pickCode 
          OR 'enter':u OF brw-pickCode
      OR CHOOSE OF btn-ok IN FRAME frm-pickCode 
          OR 'mouse-select-dblclick' OF brw-pickCode.
  CLOSE QUERY qry-pickCode.
  HIDE FRAME frm-pickCode.
  APPLY 'tab' TO varrCode.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickCode 
    OR 'enter':u OF brw-pickCode 
    OR 'mouse-select-dblclick' OF brw-pickCode 
DO: 
   GET CURRENT qry-pickCode NO-LOCK NO-WAIT.
   varrCode = dbRCod.rCode.
   DISPLAY  varrCode dbRCod.Descrip WITH FRAME frm-Input.
   APPLY 'tab' TO varrCode IN FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btn-exit IN FRAME frm-Input 
DO:
    MESSAGE "Do you want to do End-of-Day?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            TITLE "RECEIPT CAPTURE CLOSURE" UPDATE wsAns AS LOGICAL.
    IF wsAns = YES THEN DO:
    HIDE FRAME frm-Input.
         /* Print Summary */
          RUN eod.ip. 
    END. 
    RELEASE bfrDbRecCtr.
    RELEASE bfrDbRecop.
    RELEASE bfrDbRec.
    RETURN.
END.
 

/********** MAIN LOGIC **********/
wsValid = NO.
FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST simctr1 NO-LOCK NO-ERROR.
FIND FIRST fdmsDevice WHERE fdmsDevice.dstat = "A" NO-LOCK NO-ERROR.
IF AVAILABLE fdmsDevice THEN DO:
    ASSIGN
    sellerName = fdmsDevice.TNAME
    sellerTradeName = fdmsDevice.TNAME
    sellervatNumber = fdmsDevice.VATNO 
    sellerTIN = fdmsDevice.TIN
    sellerPhone = fdmsDevice.Phone
    sellerEmail =  fdmsDevice.contactemail
    sellerProvince = fdmsDevice.txtProvince
    sellerCity = fdmsDevice.txtCity
    sellerStreet = fdmsDevice.txtStreet
    sellerHouseNumber = fdmsDevice.txtHouse
    txtqUrl = fdmsDevice.qrUrl.
    FIND LAST fdmsRecCounter WHERE fdmsRecCounter.DeviceID = fdmsDevice.DeviceID NO-LOCK NO-ERROR.
    assign
        varFDNumber = fdmsRecCounter.FDNUmber
      varDeviceID = fdmsDevice.DeviceID.
END.
ELSE IF NOT AVAILABLE fdmsDevice THEN DO:
    ASSIGN
    sellerName = SIMCTR.CONAME
    sellerTradeName = SIMCTR.CONAME
    sellervatNumber = SIMCTR.REGNO
    sellerTIN = SIMCTR.BPNO
    sellerPhone = SIMCTR.Phone
    sellerEmail =   ""
    sellerProvince = ""
    sellerCity = SIMCTR.Add3
    sellerStreet = SIMCTR.add2
    sellerHouseNumber = SIMCTR.Add1
    txtqUrl = ""
    varFDNumber = 0
    varDeviceID = 0.
END.

/*Allow-disc = simctr.ODisc. */
wsTitle1 = simctr.CONAME + " VAT REG: " + STRING(SIMCTR.BPNO).
wsline[1] = ".......................................................................".
wsline[2] = ".......................................................................".
varCur = "ZWG".
wsTime    = STRING(TIME,"HH:MM:SS").
wsFile    = (string(YEAR(TODAY),"9999")
          + string(MONTH(TODAY),"99" )
          + string(DAY(TODAY),"99")
          + SUBSTR(STRING(wsTIME),1,2) 
          + SUBSTR(STRING(wsTIME),4,2) 
          + SUBSTR(STRING(wsTIME),7,2))
          + ".csv".
wsFile = simctr.repDir + wsFile.
OUTPUT STREAM c TO VALUE(wsFile).
RUN VALIDATE.ip.
FIND FIRST tblForex WHERE tblForex.txtCur = "USD" NO-LOCK NO-ERROR.
    ASSIGN wsDisRate = tblForex.decRate
           wsD1Rate = tblForex.decRate * 1.1.

 /*modified to pick correct rate by s.Chisoro */
FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbcur  AND tblForex.DtRate <= varDate  NO-LOCK NO-ERROR.
IF AVAILABLE tblForex THEN DO:
    ASSIGN wsReRate = tblForex.decRate.
END.
ELSE IF NOT AVAILABLE tblForex THEN DO:
    FIND LAST tblForexH WHERE  tblForexH.txtCur = simctr.dbcur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
    IF AVAILABLE tblForexH THEN DO:
        ASSIGN wsReRate = tblForexH.decRate.
        
    END.
    ELSE IF NOT AVAILABLE tblForexH  THEN DO:
        FIND FIRST tblForexH WHERE  tblForexH.txtCur = simctr.dbcur AND tblForexH.dtRate >= varDate NO-LOCK NO-ERROR.
        IF AVAILABLE tblForexH THEN DO:
        ASSIGN wsReRate = tblForexH.decRate.
        
    END.
    END.
END.

FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbcur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
IF AVAILABLE tblForex THEN DO:
      ASSIGN dbRate = tblForex.decRate
           rdbRate = tblForex.decRate.
END.
ELSE IF NOT AVAILABLE tblForex  THEN DO:
   FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbcur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
   IF AVAILABLE tblForexH THEN DO:
       ASSIGN dbRate = tblForexH.decRate
           rdbRate = tblForexH.decRate.
    END.
    ELSE IF NOT AVAILABLE tblForexH  THEN DO:
        FIND FIRST tblForexH WHERE  tblForexH.txtCur = simctr.dbcur AND tblForexH.dtRate >= varDate NO-LOCK NO-ERROR.
        IF AVAILABLE tblForexH THEN DO:
        ASSIGN dbRate = tblForexH.decRate
             rdbRate = tblForexH.decRate.
        
    END.
    END.
END.
/*end of modification*/
IF wsValid = YES THEN DO:
    VIEW FRAME frm-input. 
    ENABLE ALL EXCEPT varRef varTend btnInvoice varInvoiceNumber WITH FRAME frm-input.
    CLEAR FRAME frm-input ALL.
    ASSIGN wsRecDetails = ""
           wsVATNo = 0
           wsTINno = 0.
    varMeth:SCREEN-VALUE = "1".
    varloop:SCREEN-VALUE = "N".
    varInvoice:SCREEN-VALUE = "N".
    DISPLAY varRecNo varDate  varCur wsDisRate wsReRate varDeviceID varFDNumber WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-input.
    HIDE FRAME frm-input.
    OUTPUT STREAM a CLOSE.
END.
ELSE RETURN.

PROCEDURE send-trans:
    /*if successful check currency ISO table*/
    FIND FIRST tblCurrency WHERE tblCurrency.CODE = varCur NO-LOCK NO-ERROR.
    IF AVAILABLE tblCurrency THEN DO:
       /*read transaction number and add 1 for terminal*/
        FIND LAST eftTermTrans WHERE eftTermTrans.term_id  = varTerminal  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE eftTermTrans THEN DO:
                varTransNumber = 100000.
        END.
        IF AVAILABLE eftTermTrans THEN DO:
            varTransNumber = INT(eftTermTrans.tran_nr)  + 1.
        END.

        ActionCode = "".
        Account = "".
        AuthorizationProfile = "".
        BusinessDate = "".
        CardNumber = "".
        CardProductName = "".
        CurrencyCode = "".
        vDateTime = "".
        LocalDate = "".
        LocalTime = "".
        MerchantId = "".
        MessageReasonCode = "".
        PanEntryMode = "".
        PosDataCode = "".
        PosCondition = "".
        ResponseCode = "".
        RetrievalRefNr = "".
        TerminalId = "".
        TransactionAmount = "".
        AmountTransactionFee = "".
        TransactionId = "".
        vType = "".
        txtWebResponse = "".
        
         /*read webresponse
        if transaction is failing return message.
        if available update payment method and amount tendered*/
        HttpClient = NEW System.Net.WebClient(). 
        HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 
    
       webResponse = httpClient:DownloadString("http://127.0.0.1:8086/tranreq/" + varTerminal + "/" + string(varAmount)+ "/" + string(varTransNumber)  + "/" + STRING(tblCurrency.num)).
                                                                                                    
        
        HttpClient:Dispose(). 
        DELETE OBJECT HttpClient.
    
        txtWebResponse = STRING(webResponse).
         /*MESSAGE txtWebResponse VIEW-AS ALERT-BOX.*/
         txtUnWantedR[1] = "~{".
        txtUnWantedR[2] = "~}".
        txtUnWantedR[3] = "~[".
        txtUnWantedR[4] = "~]".
        txtUnWantedR[5] = "~"".
        txtUnWantedR[6] = "Balance:".
        txtUnWantedR[7] = "PosStructuredData:".
        txtUnWantedR[8] = "~n".
        txtUnWantedR[9] = "StructuredData:".
        txtUnWantedR[10] = "Transaction:".
        txtUnWantedR[11] = "Error:".
        txtUnWantedR[12] = "Emv".

         txtWebResponse = REPLACE(txtWebResponse, "EmvAmountOther", "Other").
         txtWebResponse = REPLACE(txtWebResponse, "EmvAmount", "SAmount").
         txtWebResponse = REPLACE(txtWebResponse, "EmvTransactionCurrencyCode", "CU").

        DO j = 1 TO EXTENT(txtUnWantedR):
             IF txtUnWantedR[j] <> "" THEN DO:
                 txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR[j], "").
             END.
         END.
       txtWebResponse = REPLACE(txtWebResponse, " ","").
       MESSAGE txtWebResponse VIEW-AS ALERT-BOX.
       j = 0.
       IF j = 0 THEN DO:
              txtWebResponse = TRIM(txtWebResponse).
               j = LOOKUP("MessageReasonCode:9791",txtWebResponse).
            IF j<>0 THEN DO:
                   rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                   rspCode= ENTRY(2,rspCode).
                   
                   DO i = 1 TO NUM-ENTRIES(txtWebResponse):
                       vTemp = ENTRY(i,txtWebResponse).
                       IF SUBSTR(trim(vTemp),1,10) = "ActionCode"  THEN DO:
                            ActionCode = replace(vTemp,":",",").
                       END.
                       IF SUBSTR(trim(vTemp),1,17) = "MessageReasonCode"  THEN DO:
                            MessageReasonCode = replace(vTemp,":",",").
                      END.
                      IF SUBSTR(trim(vTemp),1,12) = "ResponseCode"  THEN DO:
                            ResponseCode = replace(vTemp,":",",").
                      END.
                      IF SUBSTR(trim(vTemp),1,11) = "Description"  THEN DO:
                            rspMessage = replace(vTemp,":",",").
                       END.
                    END.
                    rspMessage = ENTRY(2,rspMessage).
                    ActionCode  = ENTRY(2,ActionCode).
                    MessageReasonCode = ENTRY(2,MessageReasonCode).
                    ResponseCode = entry(2,ResponseCode).
                    MESSAGE "Device not initialised." VIEW-AS ALERT-BOX.
                    varMeth:SCREEN-VALUE IN FRAME frm-input = '1'.
                    HIDE FRAME frm-amount.
            END.
       END.
       IF j = 0 THEN DO:
            txtWebResponse = TRIM(txtWebResponse).
            j = LOOKUP("MessageReasonCode:9721",txtWebResponse).
            IF j<>0 THEN DO:
                MESSAGE "Duplicate transaction number. Transaction Declined! Contact Your Administrator." VIEW-AS ALERT-BOX.
                varMeth:SCREEN-VALUE IN FRAME frm-input = '1'.
                 HIDE FRAME frm-amount.
            END.
       END.

       IF j = 0 THEN DO:
            txtWebResponse = TRIM(txtWebResponse).
            j = LOOKUP("EventData:05",txtWebResponse).
            IF j<>0 THEN DO:
                MESSAGE "System time out" VIEW-AS ALERT-BOX.
                CREATE eftTermTrans.
                ASSIGN
                   eftTermTrans.term_id = varTerminal
                   eftTermTrans.tran_nr = string(varTransNumber).
                    varMeth:SCREEN-VALUE IN FRAME frm-input = '1'.
                 HIDE FRAME frm-amount.
            END.
       END.

       IF j = 0 THEN DO:
            txtWebResponse = TRIM(txtWebResponse).
            j = LOOKUP("EventData:30",txtWebResponse).
            IF j<>0 THEN DO:
                MESSAGE "System time out" VIEW-AS ALERT-BOX.
                CREATE eftTermTrans.
                ASSIGN
                   eftTermTrans.term_id = varTerminal
                   eftTermTrans.tran_nr = string(varTransNumber).
                    varMeth:SCREEN-VALUE IN FRAME frm-input = '1'.
                 HIDE FRAME frm-amount.
            END.
       END.

       IF j = 0 THEN DO:
            txtWebResponse = TRIM(txtWebResponse).
            j = LOOKUP("EventData:17",txtWebResponse).
            IF j<>0 THEN DO:
                MESSAGE "Transaction Cancelled" VIEW-AS ALERT-BOX.
                CREATE eftTermTrans.
                ASSIGN
                   eftTermTrans.term_id = varTerminal
                   eftTermTrans.tran_nr = string(varTransNumber).
                   varMeth:SCREEN-VALUE IN FRAME frm-input = '1'.
                 HIDE FRAME frm-amount.
            END.
       END.

       IF j = 0 THEN DO:
           
           txtWebResponse = TRIM(txtWebResponse).
            j = LOOKUP("MessageReasonCode:9790",txtWebResponse).
            IF j<>0 THEN DO:
                DO i = 1 TO NUM-ENTRIES(txtWebResponse):
                   vTemp = ENTRY(i,txtWebResponse).
                   /*extract the data values*/
                   IF SUBSTR(trim(vTemp),1,7) = "Account"  THEN DO:
                        Account = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,10) = "ActionCode"  THEN DO:
                        ActionCode = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,20) = "AmountTransactionFee"  THEN DO:
                        AmountTransactionFee = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,20) = "AuthorizationProfile"  THEN DO:
                       AuthorizationProfile = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,12) = "BusinessDate"  THEN DO:
                       BusinessDate = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,10) = "CardNumber"  THEN DO:
                       CardNumber = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,15) = "CardProductName"  THEN DO:
                       CardProductName = replace(vTemp,":",",").
                   END.
                   IF CurrencyCode = "" THEN DO:
                        IF SUBSTR(trim(vTemp),1,12) = "CurrencyCode"  THEN DO:
                            CurrencyCode = replace(vTemp,":",",").
                        END.
                   END.
                   IF SUBSTR(trim(vTemp),1,8) = "DateTime"  THEN DO:
                       vDateTime = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,9) = "LocalDate"  THEN DO:
                        LocalDate = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,9) = "LocalTime"  THEN DO:
                        LocalTime = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,10) = "MerchantId"  THEN DO:
                        MerchantId = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,17) = "MessageReasonCode"  THEN DO:
                       MessageReasonCode = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,12) = "PanEntryMode"  THEN DO:
                       PanEntryMode = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,12) = "PosCondition"  THEN DO:
                       PosCondition = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,11) = "PosDataCode"  THEN DO:
                       PosDataCode = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,12) = "ResponseCode"  THEN DO:
                       ResponseCode = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,14) = "RetrievalRefNr"  THEN DO:
                       RetrievalRefNr = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,10) = "TerminalId"  THEN DO:
                       TerminalId = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,17) = "TransactionAmount"  THEN DO:
                       TransactionAmount = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,13) = "TransactionId"  THEN DO:
                       TransactionId = replace(vTemp,":",",").
                   END.
                   IF SUBSTR(trim(vTemp),1,4) = "Type"  THEN DO:
                       vType = replace(vTemp,":",",").
                   END.
                  IF SUBSTR(trim(vTemp),1,7) = "SAmount"  THEN DO:
                       SAmount = replace(vTemp,":",",").
                   END.
                   
            END.
            
            if ActionCode <> "" then
                ActionCode = ENTRY(2,ActionCode).
            if Account <> "" then
                Account = ENTRY(2,Account).
            if AuthorizationProfile <> "" then
                AuthorizationProfile = ENTRY(2,AuthorizationProfile).
            if BusinessDate <> "" then
                BusinessDate = ENTRY(2,BusinessDate).
            if CardNumber <> "" then
                CardNumber = ENTRY(2,CardNumber).
            if CardProductName <> "" then
                CardProductName = ENTRY(2,CardProductName).
            if CurrencyCode <> "" then
                CurrencyCode = ENTRY(2,CurrencyCode).
            if vDateTime <> "" then
                vDateTime = ENTRY(2,vDateTime).
            if LocalDate <> "" then
                LocalDate = ENTRY(2,LocalDate).
            if LocalTime <> "" then
                LocalTime = ENTRY(2,LocalTime).
            if MerchantId <> "" then
                MerchantId = ENTRY(2,MerchantId).
            if MessageReasonCode <> "" then
                MessageReasonCode = ENTRY(2,MessageReasonCode).
            if PanEntryMode <> "" then
                PanEntryMode = ENTRY(2,PanEntryMode).
            if PosDataCode <> "" then
                PosDataCode = ENTRY(2,PosDataCode).
            if PosCondition <> "" then
                PosCondition = ENTRY(2,PosCondition).
            if ResponseCode <> "" then
                ResponseCode = ENTRY(2,ResponseCode).
            if RetrievalRefNr <> "" then
                RetrievalRefNr = ENTRY(2,RetrievalRefNr).
            if TerminalId <> "" then
                TerminalId = ENTRY(2,TerminalId).
            if TransactionAmount <> "" THEN 
                TransactionAmount = ENTRY(2,TransactionAmount).
            IF SAmount <> "" THEN
                SAmount = ENTRY(2,SAmount).
            if AmountTransactionFee <> "" then
                AmountTransactionFee = ENTRY(2,AmountTransactionFee).
            if TransactionId <> "" then
                TransactionId = ENTRY(2,TransactionId).
            if vType <> "" then
                vType = ENTRY(2,vType).
            
            CREATE eftTermTrans.
            ASSIGN
                eftTermTrans.act_cd = ActionCode
                eftTermTrans.acc = Account
                eftTermTrans.auth_pr = AuthorizationProfile
                eftTermTrans.bd = BusinessDate
                eftTermTrans.card_nr = CardNumber
                eftTermTrans.carPName = CardProductName
                eftTermTrans.cur_cd = CurrencyCode
                eftTermTrans.dt = vDateTime
                eftTermTrans.ld = LocalDate
                eftTermTrans.lt = LocalTime
                eftTermTrans.mer_id = MerchantId
                eftTermTrans.messR_cd = MessageReasonCode
                eftTermTrans.panE_md = PanEntryMode
                eftTermTrans.posD_cd = PosDataCode
                eftTermTrans.pos_con = PosCondition
                eftTermTrans.res_cd = ResponseCode
                eftTermTrans.retR_nr = RetrievalRefNr
                eftTermTrans.term_id = TerminalId
                eftTermTrans.tran_amt = TransactionAmount
                eftTermTrans.tran_fee = AmountTransactionFee
                eftTermTrans.tran_nr = TransactionId
                eftTermTrans.tran_typ = vType.
              END.
              IF ActionCode = 'APPROVE' THEN DO:
                  varAmount = DEC(TransactionAmount) / 100.
                   varTend:SCREEN-VALUE IN FRAME frm-input = string(varAmount).
                   varRef :SCREEN-VALUE IN FRAME frm-input = RetrievalRefNr.
                   IF varType:SCREEN-VALUE IN FRAME frm-input = "C" THEN DO:
                       APPLY 'entry' TO varTend IN FRAME frm-input.
                   END.
              END.
              IF ActionCode = 'DECLINE' THEN DO:
                       varAmount = DEC(SAmount) / 100.
                       MESSAGE "Transaction Declined" VIEW-AS ALERT-BOX.
                        ASSIGN wsTitle = "RECEIPT"
                   wsCashier =  STRING(DATE(VarDate),"99/99/9999")
                   + "  TIME: " + substr(string(time,"HH:MM:SS"),1,5)
                   + "   MC NO:" + STRING(varUser, "999"). 
                    OUTPUT STREAM a TO VALUE(bfrDbRecop.prn).
                   DISPLAY STREAM a wsCashier
                    WITH FRAME frm-recHdr1.
                    DOWN STREAM a WITH FRAME frm-rechdr1.
                    DISPLAY STREAM a ActionCode varCur varAmount RetrievalRefNr TerminalId MerchantId MessageReasonCode CardNumber CardProductName WITH FRAME frm-footer1.
                    DOWN STREAM a WITH FRAME frm-footer1.
                    OUTPUT STREAM a CLOSE. 
                       varMeth:SCREEN-VALUE IN FRAME frm-input = '1'.
                        
              END.
          END.
              
    END.
    IF NOT AVAILABLE tblCurrency THEN DO:
       MESSAGE "No such currency" view-as alert-box.
    END.
        ActionCode = "".
        Account = "".
        AuthorizationProfile = "".
        BusinessDate = "".
        CardNumber = "".
        CardProductName = "".
        CurrencyCode = "".
        vDateTime = "".
        LocalDate = "".
        LocalTime = "".
        MerchantId = "".
        MessageReasonCode = "".
        PanEntryMode = "".
        PosDataCode = "".
        PosCondition = "".
        ResponseCode = "".
        RetrievalRefNr = "".
        TerminalId = "".
        TransactionAmount = "".
        AmountTransactionFee = "".
        TransactionId = "".
        vType = "".
        sAmount = "".
        txtWebResponse = "".
        varAmount:SCREEN-VALUE IN FRAME frm-amount = "".
   HIDE FRAME frm-amount.
END PROCEDURE.

PROCEDURE VALIDATE.ip:
    browse brw-pickAcc:allow-column-searching = true.
    FIND FIRST bfrDbRecop WHERE bfrDbRecop.usercode = varUser SHARE-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE bfrDbRecop THEN DO:
        MESSAGE "User not allowed to receipt." VIEW-AS ALERT-BOX.
        wsValid = NO.
    END.
    ELSE IF AVAILABLE bfrDbRecop THEN DO:
         wsValid = YES.
        FIND FIRST simUsr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
        ASSIGN wsCashier = "CASHIER:-" + simUsr.NAME
               VarName   = simUsr.NAME.
        VIEW FRAME frm-pass.
        DISPLAY simusr.NAME WITH FRAME frm-pass.
        FIND FIRST eftTerminal WHERE usr = varUser NO-LOCK NO-ERROR.
        IF AVAILABLE eftTerminal THEN DO:
            varTerminal = eftTerminal.term_id.
        END.
        ELSE IF NOT available eftTerminal THEN DO:
            varTerminal = "".
        END.
        ENABLE ALL WITH FRAME frm-pass.
        APPLY 'entry' TO w_password.
        WAIT-FOR CHOOSE OF btn-ok OR 'enter':u OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-pass.
        HIDE FRAME frm-pass.
        binary-key = bfrDbRecop.bKey.
        crypto-value = bfrDbRecop.password.
        SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
        SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV=?.
        clear-text =GET-STRING(DECRYPT(crypto-value),1).

        IF w_password:SCREEN-VALUE = clear-text  THEN DO:
            IF bfrDbRecop.EOD = YES THEN DO:
                ASSIGN  varRecNo = bfrDbRecop.RecNo + 1
                        varDate = TODAY.
                MESSAGE "Receipting Date " UPDATE varDate.
                IF simctr.COCODE = "MOK" THEN
                  MESSAGE "USD Payment Discount%" UPDATE wsdis.
               FIND FIRST bfrDbRecCtr WHERE bfrDbRecCtr.usercode = varUser
                                     AND bfrDbRecCtr.RecDate = varDate  SHARE-LOCK NO-ERROR.
               IF NOT AVAILABLE bfrDbRecCtr THEN DO:
                    CREATE bfrDbRecCtr.
                    ASSIGN bfrDbRecCtr.RecDate  = varDate
                           bfrDbRecCtr.usercode = VarUser
                           bfrDbRecCtr.EOD      = NO
                           bfrDbRecop.EOD       = NO
                           bfrDbRecop.RecDate   = varDate.
               END.
               ELSE DO:
                   MESSAGE "There are receipts for this date to be updated." VIEW-AS ALERT-BOX.
                   wsValid = NO.
                   RELEASE bfrDbRecCtr.
                   RELEASE bfrDbRecop.
                   APPLY 'close' TO THIS-PROCEDURE.
               END.   
           END. /* eof EOD */
           ELSE DO: /* if not eod */
               FIND FIRST bfrDbRecCtr WHERE bfrDbRecCtr.usercode = varUser 
                                       AND bfrDbRecCtr.recDate = bfrDbRecop.RecDate NO-ERROR.
               IF AVAILABLE bfrDBrecCtr AND bfrDbRecCtr.RecDate <> TODAY THEN DO:
                   MESSAGE "Supervisor Authorisation Required.....No EOD for the previous date"
                       SKIP
                       "Do you want to Proceed.....?"
                       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                        TITLE "RECEIPTING DATE OVER-RIDE" UPDATE choice AS LOGICAL.
                   IF choice THEN DO:
                       VIEW FRAME frm-pass1.
                       ENABLE ALL WITH FRAME frm-pass1.
                       WAIT-FOR CHOOSE OF btn-ok OR 'enter':u OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-pass1.
                       ASSIGN wsSup
                              sup-password.
                       HIDE FRAME frm-pass1.
                       FIND FIRST simusr WHERE simusr.usercode = wsSup:SCREEN-VALUE 
                           NO-LOCK NO-ERROR.
                       IF AVAILABLE simusr THEN DO:
                           binary-key = simusr.bKey.
                            crypto-value = simusr.password.
                               SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
                               SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
                            clear-text = GET-STRING(DECRYPT(crypto-value),1).
                       END.


                       FIND FIRST simusr WHERE simusr.usercode = wsSup:SCREEN-VALUE 
                           AND clear-text = sup-password:SCREEN-VALUE
                           NO-LOCK NO-ERROR.
                       IF AVAILABLE simusr AND simusr.supervisor = YES  THEN
                           ASSIGN varRecNo = bfrDbRecop.RecNo + 1
                                  varDate  = bfrDbRecop.RecDate
                                  wsValid = YES.
                       ELSE DO:
                           MESSAGE "Invalid Supervisor Credentials" VIEW-AS ALERT-BOX.
                           wsValid = NO.
                           APPLY 'close' TO THIS-PROCEDURE.
                       END.

                   END.
                   ELSE DO:
                       wsValid = NO.
                       APPLY 'close' TO THIS-PROCEDURE.
                   END.
               END.
               ELSE DO:
                   ASSIGN varRecNo = bfrDbRecop.RecNo + 1
                          varDate = bfrDbRecop.RecDate.
                   IF simctr.COCODE = "MOK" THEN
                      MESSAGE "USD Payment Discount%" UPDATE wsdis.
               END.

           END.
       END.
       ELSE DO:
           MESSAGE "Invalid Password for this user." VIEW-AS ALERT-BOX.
           wsValid = NO.
           APPLY 'close' TO THIS-PROCEDURE.
       END.
   END.
END.

PROCEDURE view-trans.ip:
    FOR EACH tblDetail.
        DELETE tblDetail.
    END.
    IF varType = "C" THEN DO:
        ASSIGN wsTot = 0
               wscnt = 0
               wsReceipts = 0
               bbal = 0.
        /*create view table */
        FOR EACH dbblf WHERE dbblf.dbacc = DEC(varAcc:SCREEN-VALUE IN FRAME frm-input) AND dbblf.dbAmt[1] <> 0 NO-LOCK:
            CREATE tblDetail.
            BUFFER-COPY dbblf TO tblDetail.         
        END.
        /* Consider Receipts */
        FOR EACH dbrec WHERE dbRec.Account = varAcc AND dbRec.RecStat <> "C" AND dbRec.contype = "C" NO-LOCK:
            FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbcur AND dbrec.recdate >= tblForex.DtRate  NO-LOCK NO-ERROR.
            IF AVAILABLE tblForex THEN DO:
               ASSIGN wsRate = tblForex.decRate.
             END.
             ELSE IF NOT AVAILABLE tblForex  THEN DO:
                  FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbcur AND  tblForexH.dtRate <= dbrec.recdate  NO-LOCK NO-ERROR.
                   IF AVAILABLE tblForexH THEN DO:
                       ASSIGN wsRate = tblForexH.decRate.
                    END.
             END.
        END.

        /*display unposted receipts  s. Chisoro*/
       
        FOR EACH dbrec WHERE dbRec.Account = varAcc AND dbRec.RecStat <> "C" AND dbRec.contype = "C" NO-LOCK:
            FIND FIRST dbrcod WHERE dbrcod.rcode = dbrec.rcode NO-LOCK NO-ERROR.
            FIND FIRST simctr NO-LOCK NO-ERROR.
            FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbCur AND dbRec.recDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
            IF AVAILABLE tblForex THEN DO:
                  ASSIGN wsReceipts =  wsReceipts + round(dbRec.amount / (tblForex.decRate  / dbRec.decRate),2)
                       WHEN dbRCod.TARGET <> "D".
            END.
            ELSE IF NOT AVAILABLE tblForex  THEN DO:
               FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbCur AND tblForexH.dtRate <= dbRec.recDate NO-LOCK NO-ERROR.
               IF AVAILABLE tblForexH THEN DO:
                 ASSIGN  wsReceipts =  wsReceipts + round(dbRec.amount / (tblForexH.decRate  / dbRec.decRate),2)
                      WHEN dbRCod.TARGET <> "D".
                END.
                ELSE IF NOT AVAILABLE tblForexH  THEN DO:
                    FIND FIRST tblForexH WHERE  tblForexH.txtCur = simctr.dbCur AND tblForexH.dtRate >= dbRec.recDate NO-LOCK NO-ERROR.
                    IF AVAILABLE tblForexH THEN DO:
                       ASSIGN wsReceipts =  wsReceipts + round(dbRec.amount / (tblForexH.decRate  / dbRec.decRate),2)
                           WHEN dbRCod.TARGET <> "D".
                    END.
                END.
            END.
        END.
        ASSIGN wsTotalw = 0.
        FOR EACH tblDetail:
            ASSIGN wsTot[1] = wsTot[1] + tblDetail.dbAmt[1] WHEN tblDetail.dbAmt[1] <> 0
                   wscnt = wscnt + 1
                   wsTot[3] = wsTot[3] + tblDetail.dbAmt[1]
                   bBal = bBal + tblDetail.dbAmt[1]
                   wsTotalW = wsTotalW + tblDetail.dbAmt[1] WHEN tblDetail.dbAmt[1] > 0 . 
        END.
        ASSIGN wsTot[2] = wsTot[3] * dbRate
               bBal = bBal - wsReceipts.
        
        /*MESSAGE "Account balance " wsTot[1] VIEW-AS ALERT-BOX.*/
    END.
    ELSE IF varType = "L" THEN DO:
        FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-ERROR.
        FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
              ASSIGN dbRate = tblForex.decRate.
        END.
        ELSE IF NOT AVAILABLE tblForex  THEN DO:
           FIND FIRST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
           IF AVAILABLE tblForexH THEN DO:
               ASSIGN dbRate = tblForexH.decRate.
            END.
        END.
        CREATE tblDetail.
          ASSIGN tblDetail.dbacc = DEC(varAcc:SCREEN-VALUE)
                 tblDetail.sgrp  = 0
                 tblDetail.dbamt[1] = hsecmf.AmtDue[1]
                 tblDetail.amt[1]   = hsecmf.AmtDue[1].
          ASSIGN wsTot[1] = tblDetail.amt[1]
                 wsTot[3] = tblDetail.amt[1]
                 wsTot[2] = tblDetail.amt[1] * dbRate
                 bBal     = tblDetail.amt[1].
          wsReceipts = 0.
          FOR EACH dbrec WHERE dbRec.Account = varAcc AND dbRec.RecStat <> "C" AND dbRec.contype = "L" NO-LOCK:
            FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur AND dbRec.recDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
            IF AVAILABLE tblForex THEN DO:
                  wsReceipts =  wsReceipts + ROUND(dbRec.amount / (tblForex.decRate  / dbRec.decRate),2).
            END.
            ELSE IF NOT AVAILABLE tblForex  THEN DO:
               FIND LAST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= dbRec.recDate NO-LOCK NO-ERROR.
               IF AVAILABLE tblForexH THEN DO:
                  wsReceipts =  wsReceipts + ROUND(dbRec.amount / (tblForexH.decRate  / dbRec.decRate),2).
                END.
                ELSE IF NOT AVAILABLE tblForexH  THEN DO:
                    FIND FIRST tblForexH WHERE  tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate >= dbRec.recDate NO-LOCK NO-ERROR.
                    IF AVAILABLE tblForexH THEN DO:
                       wsReceipts =  wsReceipts + ROUND(dbRec.amount / (tblForexH.decRate  / dbRec.decRate),2).
                    END.
                END.
            END.
          END.
    END.
    DISPLAY wsTot[3] @ wsTot[1] wsTot[2] wsReceipts WITH FRAME frm-input.
   ENABLE brw-cust WITH FRAME frm-input.
  OPEN QUERY qry-cust FOR EACH tblDetail 
      WHERE tblDetail.dbacc = DEC(varAcc:SCREEN-VALUE) NO-LOCK.
END.

PROCEDURE rec-prnFS.ip:
    cFile  = simctr.repDir + "receipts\" + varUser + "R" + string(varRecNo) + ".json".
    /*create a taxallocation table*/
    FOR EACH tmpTax.
        DELETE tmpTax.
    END.
    ASSIGN varTotal = 0
          wsdisc   = 0
          depTotal = 0
          receiptLineType = "Sale".
      
   
     FOR EACH tmpbfrDbRec:
     
        ASSIGN tmpbfrDbRec.Ref = varRef
               tmpbfrDbRec.Paytype = int(varMeth:SCREEN-VALUE IN FRAME frm-input).
         CREATE bfrDbRec.
        BUFFER-COPY tmpbfrDbRec TO bfrDbRec.
        
    END.
    FIND FIRST tmpbfrdbrec NO-ERROR.
    receiptCurrency =  tmpbfrdbrec.txtcur.
    IF varType = 'V' THEN DO:
            buyerName = "".
            buyerTradeName = "".
            vatNumber = "".
            buyerTIN = "".
            buyerPhone = "".
            buyerEmail = "".
            buyerProvince = "".
            buyerCity = "".
            buyerStreet = "".
            buyerHouse = "".
            buyerDistrict = "".
    END.
    ELSE IF varType = 'C' THEN DO:
            FIND FIRST dbcmf WHERE dbcmf.dbacc = varAcc NO-LOCK NO-ERROR.
            IF AVAILABLE dbcmf  THEN DO:
                FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
           
                ASSIGN
                    buyerName = dbcmf.Name
                    buyerTradeName = dbcmf.NAME
                    vatNumber = string(dbcmf.VatNo)
                    buyerTIN = string(dbcmf.TIN)
                    buyerPhone = string(dbcmf.Cell)
                    buyerEmail = dbcmf.emailAdd
                    buyerProvince = dbcmf.province 
                    buyerCity = dbcmf.city
                    buyerStreet =  dbcmf.street + " " + dbsmf.descrip
                    buyerHouse = string(dbcmf.StandNo)
                    buyerDistrict = dbcmf.district.
           END.
    END.
    ELSE IF varType = 'L' THEN DO:
            FIND FIRST hsecmf WHERE hsecmf.dbacc = varAcc NO-LOCK NO-ERROR.
            IF AVAILABLE hsecmf  THEN DO:
                FIND FIRST dbsmf WHERE dbsmf.suburb = hsecmf.suburb NO-LOCK NO-ERROR.
                assign
                    buyerName = hsecmf.Name
                    buyerTradeName = hsecmf.NAME
                    vatNumber = string(hsecmf.VatNo)
                    buyerTIN = string(hsecmf.TIN)
                    buyerPhone = string(hsecmf.Cell[1]) + "/" + string(hsecmf.Cell[2])
                    buyerEmail = hsecmf.emailAdd
                    buyerProvince = hsecmf.province 
                    buyerCity = hsecmf.city
                    buyerStreet =  dbsmf.descrip
                    buyerHouse = string(hsecmf.StandNo)
                    buyerDistrict = hsecmf.district.
            END.
    END.
    /*Money Type*/
    IF trim(substring(dbPay.descrip,1,4)) = "CASH" THEN DO:
         moneyTypeCode = "CASH".
    END.
    ELSE IF TRIM(substring(dbPay.descrip,1,4)) = "BTRF"  THEN DO:
          moneyTypeCode = "BTRF".
    END.
    ELSE DO:
        moneyTypeCode = "CARD".
    END.

    dt = NOW.
    d = SUBSTRING(string(dt),7,4)  + "-" + SUBSTRING(string(dt),4,2) + "-" + SUBSTRING(string(dt),1,2) + "T" +  SUBSTRING(STRING(dt),12,8).
    cJson = '~{' + CHR(10).  
    cJson = cJson + '  "receipt": ~{' + CHR(10).
    /*add condition when no device*/
    cJson = cJson + '    "receiptType": "FiscalTaxInvoice",' + CHR(10).  
    cJson = cJson + '    "receiptCurrency": "' + receiptCurrency + '",' + CHR(10).
    cJson = cJson + '    "invoiceNo": "'  + varUser + "R" + string(varRecNo) + '",' + CHR(10). 
    cJson = cJson + '    "sellerData": ~{' + CHR(10).  
    cJson = cJson + '      "sellerRegisterName": "' + trim(sellertradeName) + '",' + CHR(10). 
    cJson = cJson + '      "sellerTradeName": "' + trim(sellertradeName) + '",' + CHR(10).  
    cJson = cJson + '      "sellervatNumber": "' + trim(sellervatNumber) + '",' + CHR(10).  
    cJson = cJson + '      "sellerTIN": "' + trim(sellerTIN) + '",' + CHR(10).
    cJson = cJson + '      "sellerContacts": ~{' + CHR(10). 
    cJson = cJson + '           "sellerHouse": "' + trim(sellerhouseNumber) + '",' + CHR(10). 
    cJson = cJson + '           "sellerStreet": "' + trim(sellerStreet) + '",' + CHR(10). 
    cJson = cJson + '           "sellerCity": "' + trim(sellerCity) + '",' + CHR(10).
    cJson = cJson + '           "sellerDistrict": "' + trim(sellerDistrict) + '",' + CHR(10).
    cJson = cJson + '           "sellerProvince": "' + trim(sellerProvince) + '",' + CHR(10).
    cJson = cJson + '           "sellerphoneNo": "' + trim(sellerPhone) + '",' + CHR(10).  
    cJson = cJson + '           "selleremail": "' + trim(sellerEmail) + '"' + CHR(10).  
   cJson = cJson + '       }' + CHR(10).
    cJson = cJson + '    },' + CHR(10).
    cJson = cJson + '    "buyerData": ~{' + CHR(10).  
    cJson = cJson + '      "buyerRegisterName": "' + buyerName + '",' + CHR(10). 
    cJson = cJson + '      "buyerTradeName": "' + buyerTradeName + '",' + CHR(10).  
    cJson = cJson + '      "vatNumber": "' + vatNumber + '",' + CHR(10).  
    cJson = cJson + '      "buyerTIN": "' + buyerTIN + '",' + CHR(10).
    cJson = cJson + '      "buyerContacts": ~{' + CHR(10).  
    cJson = cJson + '        "buyerphoneNo": "' + buyerPhone + '",' + CHR(10).  
    cJson = cJson + '        "consumer": "' + varType + '",' + CHR(10).  
    cJson = cJson + '        "buyeremail": "' + buyerEmail + '"' + CHR(10).  
    cJson = cJson + '      },' + CHR(10).  
    cJson = cJson + '      "buyerAddress": ~{' + CHR(10).  
    cJson = cJson + '        "province": "' + buyerProvince + '",' + CHR(10). 
    cJson = cJson + '        "city": "' + buyerCity + '",' + CHR(10). 
    cJson = cJson + '        "street": "' + buyerStreet + '",' + CHR(10).
    cJson = cJson + '        "houseNo": "' + buyerHouse + '",' + CHR(10).  
    cJson = cJson + '        "district": "' + buyerDistrict + '"' + CHR(10).
    cJson = cJson + '      }' + CHR(10).  
    cJson = cJson + '    },' + CHR(10). 
    cJson = cJson + '    "receiptNotes": "' + receiptNotes + '",' + CHR(10).  /*notes*/
    cJson = cJson + '    "receiptDate": "' + d + '",' + CHR(10). 

    /*check this section using document from Zimra . This is for receipt reversal
    cJson = cJson + '    "creditDebitNote": ~{' + CHR(10).  
    cJson = cJson + '      "receiptID": 0,' + CHR(10).
    cJson = cJson + '      "deviceID":"' + string(fdmsDevice.deviceID) + '",' + CHR(10).   
    cJson = cJson + '      "receiptGlobalNo":"' + string(recGnumber) + '",' + CHR(10).   
    cJson = cJson + '      "fiscalDayNo":"' + string(fdmsRecCounter.fDNumber) + '",' + CHR(10).   
    cJson = cJson + '    },' + CHR(10).  */

    cJson = cJson + '    "receiptLinesTaxInclusive": true,' + CHR(10).

    cJson = cJson + '    "receiptLines": [' + CHR(10).  

        /* Add receipt lines using a FOR EACH loop */  
   DEFINE VARIABLE bFirstLine AS LOGICAL NO-UNDO.  
        
    bFirstLine = TRUE. /* A flag to handle commas before each new line entry */ 


    
    FOR EACH tmpbfrDbRec NO-LOCK: 
        
        FIND FIRST dbrCod WHERE dbRCod.rCode = tmpbfrDbRec.rCode NO-ERROR.
        ASSIGN vartotal = vartotal + tmpbfrDbRec.Amount
               depTotal = depTotal + tmpbfrDbRec.Amount WHEN dbRCod.TARGET = "D".
         

         FIND FIRST tmpTax WHERE tmpTax.taxID  = dbRCod.taxID NO-ERROR.
         IF AVAILABLE tmpTax THEN DO:
             ASSIGN
                 tmpTax.taxCode = dbRCod.taxCode
                 tmpTax.taxPercent = dbRCod.vat%
                 tmpTax.taxAmount = tmpTax.taxAmount + ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  tmpbfrDbRec.Amount),2)
                 tmpTax.salesAmountWithTax = tmpTax.salesAmountWithTax + tmpbfrDbRec.Amount .

         END.
         ELSE IF NOT available tmpTax THEN DO:
             CREATE tmpTax.
             ASSIGN
                 tmpTax.TaxID = dbRCod.taxID
                 tmpTax.taxCode = dbRCod.taxCode
                 tmpTax.taxPercent = dbRCod.vat%
                 tmpTax.taxAmount =   ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  tmpbfrDbRec.Amount),2)
                 tmpTax.salesAmountWithTax = tmpbfrDbRec.Amount .
         END.
               
        IF NOT bFirstLine THEN   
            cJson = cJson + ',' + CHR(10).  /* Add comma for subsequent items */  
        ELSE  
            bFirstLine = FALSE. /* After the first entry, we set the flag to FALSE */  
    
        cJson = cJson + '      ~{' + CHR(10).  
        cJson = cJson + '        "receiptLineType": "' + receiptLineType + '",' + CHR(10). 
        cJson = cJson + '        "receiptLineNo": ' + STRING(tmpbfrDbRec.SeqNo) + ',' + CHR(10).  
        /*cJson = cJson + '        "receiptLineHSCode": "' + string(dbRCod.hsCode) + '",' + CHR(10). */  
        cJson = cJson + '        "receiptLineName": "' + dbRCod.Descrip + '",' + CHR(10).  
        cJson = cJson + '        "receiptLinePrice": ' + STRING(tmpbfrDbRec.Amount) + ',' + CHR(10).  
        cJson = cJson + '        "receiptLineQuantity": ' + STRING(1) + ',' + CHR(10).  
        cJson = cJson + '        "receiptLineTotal": ' + STRING(tmpbfrDbRec.Amount) + ',' + CHR(10).  
        cJson = cJson + '        "taxCode": "' + STRING(dbRCod.taxCode) + '",' + CHR(10). 
        cJson = cJson + '        "taxPercent": ' + STRING(dbRCod.vat%) + ',' + CHR(10).  
        cJson = cJson + '        "taxID": ' + STRING(dbRCod.taxID) + CHR(10).  
        cJson = cJson + '      }' + CHR(10).  
    END.
              
   cJson = cJson + '    ],' + CHR(10).
   cJson = cJson + '    "receiptTaxes": [' + CHR(10). 
   bFirstLine = TRUE.
    FOR EACH tmpTax  NO-LOCK:
        IF NOT bFirstLine THEN   
            cJson = cJson + ',' + CHR(10).   
        ELSE  
            bFirstLine = FALSE.
       cJson = cJson + '      ~{' + CHR(10).
       cJson = cJson + '        "taxCode": "' + tmpTax.taxCode + '",' + CHR(10).
       cJson = cJson + '        "taxPercent": ' + STRING(tmpTax.taxPercent) + ',' + CHR(10).  
       cJson = cJson + '        "taxID": ' + STRING(tmpTax.taxID) + ',' + CHR(10). 
       cJson = cJson + '        "taxAmount": ' + STRING(tmpTax.taxAmount) + ',' + CHR(10).
       cJson = cJson + '        "salesAmountWithTax": ' + STRING(tmpTax.salesAmountWithTax)  + CHR(10). 
       cJson = cJson + '      }' + CHR(10).  

    END.
    cJson = cJson + '    ],' + CHR(10).  
    cJson = cJson + '    "receiptPayments": [' + CHR(10).  
    cJson = cJson + '      ~{' + CHR(10).  
    cJson = cJson + '        "moneyTypeCode":"' + moneyTypeCode + '",' + CHR(10).  /*check this */
    cJson = cJson + '        "paymentAmount": ' + STRING(varTend) + CHR(10).  
    cJson = cJson + '      }' + CHR(10).  
    cJson = cJson + '    ],' + CHR(10).  
    cJson = cJson + '    "receiptTotal": ' + STRING(varTotal) + ',' + CHR(10).  
    cJson = cJson + '    "receiptPrintForm": "Receipt48",' + CHR(10). 
    cJson = cJson + '    "rateUSD": ' + string(wsReRate) + ',' + CHR(10). 
    cJson = cJson + '    "rateRecCur": ' + string(rdbRate) + ',' + CHR(10). 
    IF varType <> "V"  THEN DO:
         cJson = cJson + '    "unPosted": ' + string(wsReceipts) + ',' + CHR(10). 
        cJson = cJson + '    "balBF": ' + string(wsTot[1]) + ',' + CHR(10). 
    END.
   cJson = cJson + '    "varInvoice": "' + STRING(varInvoice) + '",' + CHR(10). 
    IF varInvoice = "Y" THEN DO:
                 cJson = cJson + '    "varInvoiceNo": "' + string(varInvoiceNumber) + '",' + CHR(10). 
    END.

    /*get this from  fdmsRecDevice locking and the unlock after receipt update*/
   
    IF (varDeviceId <> 0 AND varInvoice = "N") THEN DO:
        FIND FIRST fdmsRecCounter WHERE fdmsRecCounter.deviceID = fdmsDevice.deviceID EXCLUSIVE-LOCK.
        recCounter = fdmsRecCounter.recNumber  + 1.
        recGNumber = INTEGER(fdmsRecCounter.glbNumber) + 1.
        varHash = fdmsRecCounter.hash.
        cJson = cJson + '    "receiptCounter":' + string(recCounter) + ',' + CHR(10).  
        cJson = cJson + '    "receiptGlobalNo":' + string(recGnumber) + ',' + CHR(10). 
        cJson = cJson + '    "qUrl":"' + trim(txtQurl) + '",' + CHR(10).
        cJson = cJson + '    "pHash":"' + trim(varHash) + '",' + CHR(10). 
        cJson = cJson + '    "deviceID":"' + string(fdmsDevice.deviceID) + '"' + CHR(10). 
    END.
    
    cJson = cJson + '  }' + CHR(10).  
    cJson = cJson + '}' + CHR(10).  
 
FOR EACH tmpTax.
    DELETE tmpTax.
END.
OUTPUT TO VALUE(cFile).  
PUT UNFORMATTED cJson.
/*call api*/
OUTPUT CLOSE. 
/*remeber to update receipt and receiptCounter hash*/
  ASSIGN bBal = 0
           aBal = 0
           wsDisc = 0.00
           fdmsRecCounter.glbNumber  = recGnumber WHEN varDeviceID <> 0
            fdmsRecCounter.recNumber = recCounter WHEN varDeviceID <> 0
            fdmsRecCounter.hash = hashstring WHEN varDeviceID <> 0
           bfrDbRecop.RecNo = varRecNo. 
RELEASE fdmsRecCounter.
/*FIND FIRST fdmsRecCounter WHERE fdmsRecCounter.DeviceID = fdmsDevice.DeviceID NO-LOCK NO-ERROR.*/
END PROCEDURE.

PROCEDURE rec-prn.ip:
    ASSIGN varTotal = 0
           wsdisc   = 0
           depTotal = 0.
    FOR EACH tmpbfrDbRec:
        IF ROUND(tmpbfrDBrec.amount,2) = 0.00 THEN NEXT.
        ASSIGN tmpbfrDbRec.Ref = varRef
               tmpbfrDbRec.Paytype = int(varMeth:SCREEN-VALUE IN FRAME frm-input).
        BUFFER-COPY tmpbfrDbRec TO bfrDbRec.
        CREATE bfrDbRec.
        EXPORT STREAM c DELIMITER ',' tmpbfrDbRec.
    END.
    OUTPUT STREAM a TO VALUE(bfrDbRecop.prn).
    DISPLAY STREAM a varName wsRecDetails wsTINno wsVatNo wsPhone wsemail wsAdd wsline[2]
         WITH FRAME frm-recHdr.
    DOWN STREAM a WITH FRAME frm-rechdr.
    FOR EACH tmpbfrDbRec:
        FIND FIRST dbrCod WHERE dbRCod.rCode = tmpbfrDbRec.rCode NO-ERROR.
        ASSIGN vartotal = vartotal + tmpbfrDbRec.Amount
               depTotal = depTotal + tmpbfrDbRec.Amount WHEN dbRCod.TARGET = "D".
        IF vat% = 0 THEN
            wsTax = "D".
        ELSE wsTax = "A".
        /* Masvingo */
        IF (simctr.cocode = "COM" OR simctr.cocode = "MOK" /*OR simctr.cocode = "STC" */) AND tmpbfrdbrec.txtcur  = "USD" THEN DO:
              DISPLAY STREAM a STRING(tmpbfrDbRec.Account) @ wsAcc dbRCod.Descrip wsTax 
                       round((tmpbfrDbRec.Amount /** tmpbfrdbrec.decrate */ ),2)  FORM "zzzzzzzz9.99-" @ varAmount
            WITH FRAME frm-rec. 
           DOWN STREAM a WITH FRAME frm-rec.
        END.
        ELSE DO:
            DISPLAY STREAM a STRING(tmpbfrDbRec.Account) @ wsAcc dbRCod.Descrip wsTax 
                             tmpbfrDbRec.Amount FORM "zzzzzzzz9.99-" @ varAmount
            WITH FRAME frm-rec. 
            DOWN STREAM a WITH FRAME frm-rec.
        END.
    END.
    FIND LAST tmpbfrDbRec NO-LOCK NO-ERROR.
    IF simctr.COCODE = "CVF" AND varType  = "C" /*CVF */
        AND (tmpbfrDbRec.Account < 51000000 OR tmpbfrDbRec.Account > 71999999) THEN /* City of Victoria Falls Discount */
          ASSIGN wsDisc = ROUND((varTotal * (10 / 100)),2) WHEN cbkmf.txtCur = "USD" .
    ELSE IF simctr.COCODE = "MOK" AND varType  = "C" THEN
       ASSIGN wsDisc = ROUND((varTotal * (wsdis / 100)),2) WHEN cbkmf.txtCur = "USD" . /* Kariba Discount */
    ELSE 
        ASSIGN wsDisc = ROUND((varTotal * (wsdis / 100)),2) .
    ASSIGN bfrDbRecCtr.RecTotal = bfrDbRecCtr.RecTotal + vartotal.

    DISPLAY STREAM a wsMark  varTotal varTend varChg
        WITH FRAME frm-rec1.
    DOWN STREAM a WITH FRAME frm-rec1.
    
    
    FIND FIRST tblForex WHERE tblForex.txtCur = cbkmf.txtCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
          ASSIGN wsRate = tblForex.decRate.
    END.
    ELSE IF NOT AVAILABLE tblForex  THEN DO:
       FIND LAST tblForexH WHERE tblForexH.txtCur = cbkmf.txtCur AND  tblForexH.dtRate <= varDate  NO-LOCK NO-ERROR.
       IF AVAILABLE tblForexH THEN DO:
           ASSIGN wsRate = tblForexH.decRate.
        END.
    END.
    IF varType = "L" THEN DO:
        FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-ERROR.
        FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
              ASSIGN dbRate = tblForex.decRate.
        END.
        ELSE IF NOT AVAILABLE tblForex  THEN DO:
           FIND LAST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
           IF AVAILABLE tblForexH THEN DO:
               ASSIGN dbRate = tblForexH.decRate.
            END.
        END.
    END.
    ELSE IF varType = "C" THEN DO:
        /*FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-ERROR. */
        FIND FIRST tblForex WHERE tblForex.txtCur = SIMCTR.dbCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
              ASSIGN dbRate = tblForex.decRate.
        END.
        ELSE IF NOT AVAILABLE tblForex  THEN DO:
           FIND LAST tblForexH WHERE tblForexH.txtCur = SIMCTR.dbCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
           IF AVAILABLE tblForexH THEN DO:
               ASSIGN dbRate = tblForexH.decRate.
            END.
        END.
    END.
    ASSIGN aBal = bBal - ROUND((varTotal * (wsRate / dbRate)),2) + ROUND((depTotal * (wsRate / dbRate)),2)
           aBal   = 0.00 WHEN varType = "V"
           wsEquv = ROUND((varTotal * (wsRate / dbRate)),2)
           bcur = SIMCTR.dbCur
           acur = SIMCTR.dbCur.
           ASSIGN wsnar =  1       WHEN varCur = "USD"
                  wsnar =  dbrate  WHEN varCur <> "USD".
    DISPLAY STREAM a varCur varTotal wsNar @ dbRate varTend varChg bBal aBal bcur acur
                     wsDisc wsEquv WITH FRAME frm-footer.
    DOWN STREAM a WITH FRAME frm-footer.
    ASSIGN bBal = 0
           aBal = 0
           wsDisc = 0.00
           bfrDbRecop.RecNo = varRecNo.
   /* put stream a control "~033".*//* for POS printer 
   put stream a control "~151".  */
    OUTPUT STREAM a CLOSE. 
    RETURN.
END PROCEDURE. /* eop -rec-prn.ip */ 

PROCEDURE rec-nonFprn.ip:
    ASSIGN wsCashier = "REC: " + string(varRecNo, ">>>>>>9") 
                   + 
                   " " + STRING(DATE(VarDate),"99/99/9999")
                   + " TIME: " + substr(string(time,"HH:MM:SS"),1,5)
                   + " MC NO:" + STRING(varUser, "999").
    ASSIGN varTotal = 0
           depTotal = 0.
    FOR EACH tmpbfrDbRec:
        tmpbfrDbRec.Ref = varRef.
        BUFFER-COPY tmpbfrDbRec TO bfrDbRec.
        CREATE bfrDbRec.
        /*ASSIGN bfrDbRec.paytype = int(varMeth:SCREEN-VALUE IN FRAME frm-input).???? */
        EXPORT STREAM c DELIMITER ',' tmpbfrDbRec.
    END.
    OUTPUT STREAM a TO VALUE(bfrDbRecop.prn).
    put stream a control "~033[". /* for POS Printer */  
    FOR EACH tmpbfrDbRec:
        FIND FIRST dbrCod WHERE dbRCod.rCode = tmpbfrDbRec.rCode NO-ERROR.
        ASSIGN vartotal = vartotal + tmpbfrDbRec.Amount
               deptotal = deptotal + tmpbfrDbRec.Amount WHEN dbRCod.TARGET = "D".
        IF vat% = 0 THEN
            wsTax = "D".
        ELSE wsTax = "A".
        DISPLAY STREAM a STRING(tmpbfrDbRec.Account) @ wsAcc dbRCod.Descrip /*wstax */ 
            tmpbfrDbRec.Amount FORM "zzzzzzzz9.99-" @ varAmount
            WITH FRAME frm-NonFrec. 
        DOWN STREAM a WITH FRAME frm-nonFrec.
    END.
    bfrDbRecCtr.RecTotal = bfrDbRecCtr.RecTotal + vartotal.
    DISPLAY STREAM a varTotal varTend varChg
        WITH FRAME frm-NonFrec1.
    DOWN STREAM a WITH FRAME frm-nonFrec1.
    
    
    FIND FIRST tblForex WHERE tblForex.txtCur = cbkmf.txtCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
          ASSIGN wsRate = tblForex.decRate.
    END.
    ELSE IF NOT AVAILABLE tblForex  THEN DO:
       FIND FIRST tblForexH WHERE tblForexH.txtCur = cbkmf.txtCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
       IF AVAILABLE tblForexH THEN DO:
           ASSIGN wsRate = tblForexH.decRate.
        END.
    END.
    IF varType = "L" THEN DO:
        FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-ERROR.
        FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
              ASSIGN dbRate = tblForex.decRate.
        END.
        ELSE IF NOT AVAILABLE tblForex  THEN DO:
           FIND FIRST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
           IF AVAILABLE tblForexH THEN DO:
               ASSIGN dbRate = tblForexH.decRate.
            END.
        END.
    END.
    ELSE IF varType = "C" THEN DO:
        FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-ERROR.
        FIND FIRST tblForex WHERE tblForex.txtCur = SIMCTR.dbCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
              ASSIGN dbRate = tblForex.decRate.
        END.
        ELSE IF NOT AVAILABLE tblForex  THEN DO:
           FIND FIRST tblForexH WHERE tblForexH.txtCur = SIMCTR.dbCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
           IF AVAILABLE tblForexH THEN DO:
               ASSIGN dbRate = tblForexH.decRate.
            END.
        END.
    END.
    ASSIGN aBal = bBal - ROUND((varTotal * (wsRate / dbRate)),2) + ROUND((depTotal * (wsRate / dbRate)),2)
           aBal   = 0.00 WHEN varType = "V"
           wsEquv = 0.00
           wsDisc = 0.00.
   ASSIGN wsnar =  1       WHEN varCur = "USD"
          wsnar =  dbrate  WHEN varCur <> "USD".
    DISPLAY STREAM a varCur varTotal  wsNar @ dbRate varTend varChg bBal aBal
                     acur bcur wsDisc  wsEquv WITH FRAME frm-footer.
    DOWN 2 STREAM a WITH FRAME frm-footer.
    ASSIGN bBal = 0
           aBal = 0
           bfrDbRecop.RecNo = varRecNo.
   put stream a control "~033". /* for POS printer */
   put stream a control "~151".  
   OUTPUT STREAM a CLOSE. 
   RETURN.
END PROCEDURE. /* eop -rec-nonFprn.ip */ 


    


PROCEDURE reprn.ip:
   wsRecNo = varRecNo - 1.
   vartotal = 0.
   depTotal = 0.
IF CAN-FIND(FIRST bfrDbRec WHERE bfrDbRec.recno = wsRecNo AND bfrDbRec.OpCode = VarUser
            AND bfrDbRec.RecDate = varDate) THEN DO:
    OUTPUT STREAM a TO VALUE(bfrDbRecop.prn).  /* Output to be directed to defualt printer */
    /*put stream a control "~033[5i". */
   /* put stream a control "~033[". */ /* for POS Printer */
    DISPLAY STREAM a varName trim(wsRecDetails) wsTINno wsVatNo wsPhone wsemail WITH FRAME frm-recHdr.
    DOWN STREAM a WITH FRAME frm-rechdr.
    FOR EACH bfrDbRec WHERE bfrDbRec.recno = wsRecNo AND bfrDbRec.OpCode = VarUser 
        BREAK BY bfrDbRec.recno BY bfrDbRec.SeqNo:
        wstax = "".
        IF FIRST-OF(bfrDbRec.recno) THEN DO:
            IF  bfrDbRec.contype = "C" THEN DO:
                FIND FIRST bfrDbcmf WHERE bfrDbcmf.dbacc = bfrDbRec.Account NO-LOCK NO-ERROR.
                IF AVAILABLE bfrDbcmf THEN
                    ASSIGN wsRecDetails = bfrDbcmf.NAME
                           wsAdd = TRIM(bfrDbcmf.stno) + " " + TRIM(bfrDbcmf.street).
            END.
           wsTitle = "DUPLICATE****" + bfrDbRec.txtCur + ":  RECEIPT: " + string(wsRecNo,"99999999") 
        + "     DATE: " + STRING(VarDate).     
        END.
        
        FIND FIRST dbrCod WHERE dbRCod.rCode = bfrDbRec.rCode NO-ERROR.
        ASSIGN vartotal = vartotal + bfrDbRec.Amount
               deptotal = deptotal + bfrDbRec.Amount WHEN dbRCod.TARGET = "D".
        IF vat% = 0 THEN
            wsTax = "D".
        ELSE wsTax = "A".
        DISPLAY STREAM a bfrDbRec.Account @ wsAcc ","
                         dbRCod.Descrip  "," wstax "," 
            bfrDbRec.Amount FORM "zzzzzzzz9.99-" @ varAmount
            WITH FRAME frm-rec.
        DOWN STREAM a WITH FRAME frm-rec.
   END.
    varTend = vartotal.
     /*UNDERLINE STREAM a wsAcc dbRCod.Descrip varAmount WITH FRAME frm-rec.*/
      DOWN 2 STREAM a WITH FRAME frm-rec.
    DISPLAY STREAM a "P;BANK DRAFT" @  wsAcc "Receipt Total" @ dbRCod.Descrip varTotal @ varAmount 
        WITH FRAME frm-rec.
    DOWN 2 STREAM a WITH FRAME frm-rec.
    DISPLAY STREAM a "Amount Tendered" @ dbRCod.Descrip varTend @ varAmount
         WITH FRAME frm-rec.
    DOWN  STREAM a WITH FRAME frm-rec.
    DISPLAY STREAM a "Amount Due" @ dbRCod.Descrip varTotal @ varAmount 
        WITH FRAME frm-rec.
    DOWN  STREAM a WITH FRAME frm-rec.
    UNDERLINE STREAM a varAmount WITH FRAME frm-rec.
    DOWN  STREAM a WITH FRAME frm-rec.
    DISPLAY STREAM a "Change" @ dbRCod.Descrip varChg @ varAmount WITH FRAME frm-rec.
    DOWN  10 STREAM a WITH FRAME frm-rec.
    /*put stream a control "~033".
    put stream a control "~151". */
    /*put stream prt-stream control "~033[5i". */
    OUTPUT STREAM a CLOSE.
    RETURN.
END.
END.

PROCEDURE rec-create.ip:
    X = X + 1.
    DO TRANSACTION ON ERROR UNDO:
        IF  (varType:SCREEN-VALUE IN FRAME frm-input) = "C" THEN DO:
            FIND FIRST bfrDbcmf WHERE bfrDbcmf.dbacc =  DEC(varAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE bfrDbcmf THEN DO:
                MESSAGE "Invalid Account entered no reccord recorded" VIEW-AS ALERT-BOX.
                /*DISABLE brw-rec WITH FRAME frm-input.
                DISABLE brw-cust WITH FRAME frm-input. */
                APPLY "entry" TO varType IN FRAME frm-input.
                RETURN NO-APPLY.
            END.
        END.
         IF  (varType:SCREEN-VALUE IN FRAME frm-input) = "L" THEN DO:
            FIND FIRST hsecmf WHERE hsecmf.dbacc =  DEC(varAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE hsecmf THEN DO:
                MESSAGE "Invalid Account entered no reccord recorded" VIEW-AS ALERT-BOX.
                /*DISABLE brw-rec WITH FRAME frm-input.
                DISABLE brw-cust WITH FRAME frm-input. */
                APPLY "entry" TO varType IN FRAME frm-input.
                RETURN NO-APPLY.
            END.
        END.
            ASSIGN varAmount = DEC(varAmount:SCREEN-VALUE).
                    varDue = varDue + varAmount.
            IF  varrCode:SCREEN-VALUE = "ZZ" AND wsTot[1] > 0 THEN DO:
                wsTall = 0.
                FOR EACH tblDetail WHERE tblDetail.dbAmt[1] > 0:
                   IF tbldetail.vat <> 0 THEN
                       FIND FIRST dbrCod WHERE dbRCod.Sgrp = tblDetail.sgrp AND dbrcod.vat% <> 0 NO-LOCK NO-ERROR.
                    ELSE IF tbldetail.vat = 0 THEN
                       FIND FIRST dbrCod WHERE dbRCod.Sgrp = tblDetail.sgrp  NO-LOCK NO-ERROR.
                   IF  tblDetail.dbAmt[1] > 0 THEN DO:  
                        ASSIGN wsAlloc = ROUND((varAmount * (tblDetail.dbAmt[1] / wsTotalW)),2).
                               wsTAll = wsTall + wsAlloc.
                        IF ROUND(wsalloc,2) >= 0.01 THEN DO:
                            
                            CREATE tmpbfrDbRec.
                            ASSIGN tmpbfrDbRec.SeqNo = x
                                   tmpbfrDbRec.Ref = varRef:SCREEN-VALUE IN FRAME frm-input
                                   tmpbfrDbRec.RecNo = INT(varRecNo:SCREEN-VALUE)
                                   tmpbfrDbRec.RecDate = DATE(varDate:SCREEN-VALUE)
                                   tmpbfrDbRec.rCode =   dbrCod.rCode 
                                   tmpbfrDbRec.Paytype = varMeth
                                   tmpbfrDbRec.txtCur = varCur
                                   tmpbfrDbRec.decRate = rdbRate /*tblForex.decRate*/
                                   tmpbfrDbRec.OpCode = varUser
                                   tmpbfrDbRec.contype = varType 
                                   tmpbfrDbRec.Amount = wsAlloc
                                   tmpbfrDbRec.Descrip = varDescrip
                                   tmpbfrDbRec.Account = varAcc 
                                   X = X + 1.
                        END.  
                   END.
                END.
                IF wsTall <> varAmount THEN DO:
                   FIND FIRST tmpbfrDbRec NO-ERROR.
                   wsAlloc = varAmount - wsTall.
                   tmpbfrDbRec.Amount = tmpbfrDbRec.Amount + wsAlloc.
                END.
            END. /* eo...ZZ & > 0 */
            ELSE IF  varrCode:SCREEN-VALUE = "ZZ" AND wsTot[1] <= 0 THEN DO:
               wsTall = 0.
                FOR EACH tblDetail:
                    IF tbldetail.vat <> 0 THEN
                       FIND FIRST dbrCod WHERE dbRCod.Sgrp = tblDetail.sgrp AND dbrcod.vat% <> 0 NO-LOCK NO-ERROR.
                    ELSE IF tbldetail.vat = 0 THEN
                       FIND FIRST dbrCod WHERE dbRCod.Sgrp = tblDetail.sgrp  NO-LOCK NO-ERROR.
                    wsAlloc = ROUND((varAmount / wscnt),2).
                    wsTAll = wsTall + wsAlloc.
                    IF ROUND(wsAlloc,2) >= 0.01 THEN DO:
                        
                        CREATE tmpbfrDbRec.
                        ASSIGN tmpbfrDbRec.SeqNo = x
                               tmpbfrDbRec.Ref = varRef:SCREEN-VALUE IN FRAME frm-input
                               tmpbfrDbRec.RecNo = INT(varRecNo:SCREEN-VALUE)
                               tmpbfrDbRec.RecDate = DATE(varDate:SCREEN-VALUE)
                               tmpbfrDbRec.rCode =   dbrCod.rCode 
                               tmpbfrDbRec.Paytype = varMeth
                               tmpbfrDbRec.txtCur = varCur
                               tmpbfrDbRec.decRate = rdbRate /*tblForex.decRate*/
                               tmpbfrDbRec.OpCode = varUser
                               tmpbfrDbRec.contype = varType 
                               tmpbfrDbRec.Amount = wsAlloc
                               tmpbfrDbRec.Descrip = varDescrip
                               tmpbfrDbRec.Account = varAcc 
                               X = X + 1.
                    END.
                END.
                IF wsTall <> varAmount THEN DO:
                   FIND FIRST tmpbfrDbRec NO-ERROR.
                   wsAlloc = varAmount - wsTall.
                   tmpbfrDbRec.Amount = tmpbfrDbRec.Amount + wsAlloc.
                END.
            END. /* eo...ZZ */
            ELSE IF  varrCode:SCREEN-VALUE <> "ZZ" THEN DO:
                CREATE tmpbfrDbRec.
                ASSIGN tmpbfrDbRec.SeqNo = x
                       tmpbfrDbRec.Ref = varRef:SCREEN-VALUE IN FRAME frm-input
                       tmpbfrDbRec.RecNo = INT(varRecNo:SCREEN-VALUE)
                       tmpbfrDbRec.RecDate = DATE(varDate:SCREEN-VALUE)
                       tmpbfrDbRec.rCode = varrCode 
                       tmpbfrDbRec.Paytype = varMeth
                       tmpbfrDbRec.txtCur = varCur
                       tmpbfrDbRec.decRate = rdbRate /*tblForex.decRate*/
                       tmpbfrDbRec.OpCode = varUser
                       tmpbfrDbRec.contype = varType 
                       tmpbfrDbRec.Amount = varAmount
                       tmpbfrDbRec.Descrip = varDescrip
                       tmpbfrDbRec.Account = varAcc 
                                               WHEN (varType = "C" OR varType = "L")
                       tmpbfrDbRec.Account = dbRCod.Ledger WHEN varType = "V".
            END. 
    END.
END.

PROCEDURE eod.ip.
    OUTPUT STREAM a TO VALUE(bfrDbRecop.prn).
    wsTitle = "Operator: " + bfrDbRecCtr.usercode + " " + simusr.Name.
    IF simctr.cocode = "MTK" THEN
        put stream a control "~033[".
    FOR EACH dbPay:
        wsAmt = 0.
        FOR EACH bfrDbRec WHERE bfrDbRec.OpCode = bfrDbRecCtr.usercode AND bfrDbRec.RecDate = bfrDbRecCtr.RecDate
            AND bfrDbRec.Paytype = dbPay .Paytype:
            ASSIGN wsAmt  = wsAmt + bfrDbRec.Amount  WHEN bfrDbRec.RecStat <> "C"
                   wsVoid = wsVoid + bfrDbRec.Amount WHEN bfrDbRec.RecStat = "C" .
        END.
        IF wsAmt <> 0 THEN DO:
            wsTotal = wsTotal + wsAmt.
            FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
            DISPLAY STREAM a dbPay.descrip wsAmt /*cbkmf.descrip */ WITH FRAME frmEod.
            DOWN STREAM a WITH FRAME frmEod.
        END.
    END.
    UNDERLINE STREAM a dbPay.descrip wsAmt WITH FRAME frmEod.
    DOWN STREAM a WITH FRAME frmEod.
    DISPLAY STREAM a "Total Collections" @ dbPay.descrip wsTotal @ wsAmt WITH FRAME frmEod.
    DOWN STREAM a WITH FRAME frmEod.
    DISPLAY STREAM a "Voids" @ dbPay.descrip wsVoid @ wsAmt WITH FRAME frmEod.
    DOWN  2 STREAM a WITH FRAME frmEod.
    DISPLAY STREAM a "Collections by Currency"  @ dbPay.descrip WITH FRAME frmEod.
    DOWN STREAM a WITH FRAME frmEod.
    FOR EACH tblforex:
        IF NOT CAN-FIND(FIRST bfrDbRec WHERE  bfrDbRec.txtCur = tblForex.txtCur) THEN NEXT.
        ASSIGN wsAmt = 0
               wsVoid = 0.
        FOR EACH bfrDbRec WHERE bfrDbRec.OpCode = bfrDbRecCtr.usercode AND bfrDbRec.RecDate = bfrDbRecCtr.RecDate
            AND bfrDbRec.txtCur = tblForex.txtCur:
            ASSIGN wsAmt  = wsAmt + bfrDbRec.Amount  WHEN bfrDbRec.RecStat <> "C"
                   wsVoid = wsVoid + bfrDbRec.Amount WHEN bfrDbRec.RecStat = "C" .
        END.
        IF wsAmt <> 0 THEN DO:
            DISPLAY STREAM a tblForex.txtCur @ dbPay.descrip wsAmt /*"Voids: " + STRING(wsVoid) @ cbkmf.descrip */
             WITH FRAME frmEod.
            DOWN STREAM a WITH FRAME frmEod.
        END.
    END.
    /*put stream a control "~033[5i". */
    IF simctr.cocode = "MTK" THEN DO:
        put stream a control "~033". 
        put stream a control "~151".
    END.
    ASSIGN bfrDbRecop.EOD = YES
           bfrDbRecop.RecDate = varDate 
           bfrDbRecCtr.EOD = YES.
END.

procedure Search.ip.
hCol = browse brw-pickAcc:current-column.
    /* assign  frame Search-opt:title = hCol:label + " Column"
            frame Search-opt:x     = hCol:x
            frame Search-opt:y     = browse b-{&t_zoom_table}:y.*/ 
    view frame Search-opt.
    enable all with frame Search-opt.
    WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
        OR 'window-close' of frame Search-opt or close of this-procedure or
            'esc','f4' of frame Search-opt.
    hide frame Search-opt.
    /*if ll-sort = ? then return.*/
    case trim(hCol:label):
        when "Name" then
        do:
           open query qry-pickAcc 
                    FOR EACH bfrDbcmf no-lock 
                        where bfrDbcmf.Sortname >= wsSearch:SCREEN-VALUE USE-INDEX NAME.

        END.
        when "Account" then
        do:
           open query qry-pickAcc 
                    FOR EACH bfrDbcmf no-lock 
                        where bfrDbcmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbacc.

        END.
        when "StandNo" then
        do:
           open query qry-pickAcc  
                    FOR EACH bfrDbcmf no-lock 
                        where bfrDbcmf.StandNo >= wsSearch:SCREEN-VALUE USE-INDEX stand.

        END.
        when "StreetNo" then
        do:
           open query qry-pickAcc  
                    FOR EACH bfrDbcmf no-lock 
                        where bfrDbcmf.Stno >= wsSearch:SCREEN-VALUE USE-INDEX street.

        END.
    END.
    RETURN.
END.
