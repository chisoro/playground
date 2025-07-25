/* Program.................simctr.p
   Notes:................. Parameter File Maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsid LIKE simctr.cocode.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".

define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 116 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 116 by 24.4.

define rectangle rect-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 45 by 9.5.
define rectangle rect-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 50 by 9.5.

define frame frm-input
    SKIP(0.5)
    simctr.cocode   colon 30 label "Company Code" SPACE(5)
    simctr.dbAuth  LABEL "Authorisation Required?"
    simctr.CONAME   COLON 30 LABEL "Campany Name"SKIP(0.2)
    SIMCTR.REGNO    colon 30 LABEL "Registration Number" SPACE(5)
    SIMCTR.BPNO              LABEL "Business Partner Number" SKIP(0.2)
    SIMCTR.Add1     COLON 30 LABEL "Address"
    SIMCTR.Add2     COLON 30 NO-LABEL
    SIMCTR.Add3     COLON 30 NO-LABEL 
    simctr.phone             LABEL "Phone" SKIP(0.5)
    SIMCTR.CLOSEPER colon 30 LABEL "Period Closed" HELP "Enter as YYYYMM" SPACE(5)
    SIMCTR.CURPER            LABEL "Current Period" HELP "Enter as YYYYMM" SPACE(5)
    /*Simctr.dbPer             LABEL "Debtors Closed Period" SKIP(0.5)*/
    SIMCTR.FUNDACC  colon 30 LABEL "Fund Accounting (Y/N)?" SPACE(15)
    simCtr.LForm             LABEL  "Ledger Format" VIEW-AS COMBO-BOX LIST-ITEM-PAIRS
                     "Old Promun",1, "National Format", 2 AUTO-RETURN SKIP(0.5)
    Simctr.ldCur COLON 30 LABEL "Ledger Currency" SPACE(15)
    simctr.dbCur  LABEL "Dabtors Currency" 
    rect-3 AT ROW 13.0 COL 10
    rect-4 AT ROW 13.0 COL 56
    SIMCTR.SURACC    AT ROW 13.2 COL 19.5 LABEL "Surplus/Deficit Sufix"
    SIMCTR.VAT[1]   colon 35 LABEL "Debtors VAT Provision" HELP "Enter Suffix"
    SIMCTR.VAT[2]   COLON 35 LABEL "Debtors VAT Payable "   HELP "Enter Suffix"
    SIMCTR.VAT[3]   colon 35 LABEL "Creditors VAT Provision" HELP "Enter Suffix"
    SIMCTR.VAT[4]   COLON 35 LABEL "Creditors VAT Claimable" HELP "Enter Suffix"
    SIMCTR.FUNDDB   COLON 35 LABEL "Interfund In suffix"
    SIMCTR.FUNDCR   COLON 35  LABEL "Interfund Out suffix"
    SIMCTR.CrdLedger COLON 35 LABEL "Creditors Control Suffix"
    simctr.exLedger COLON 35 LABEL "Exchange Gain/Loss"
    SIMCTR.Bcp    AT ROW 13.5 COL 60 LABEL "Balance Sheet Category Position"
    SIMCTR.Bcl           LABEL "Length" SKIP(0.5)
    SIMCTR.Bscp    COLON 87 LABEL "Balance Sheet S/Category Position"
    SIMCTR.Bscl           LABEL "Length" SKIP(0.5)
    SIMCTR.Icp    COLON 87 LABEL "Income Ledger Category Position"
    SIMCTR.Icl           LABEL "Length" SKIP(0.5)
    SIMCTR.Iscp    COLON 87 LABEL "Income Ledger S/Category Position"
    SIMCTR.Iscl           LABEL "Length" SKIP(0.5)
    SIMCTR.Ecp    COLON 87 LABEL "Expense Ledger Category Position"
    SIMCTR.Ecl           LABEL "Length" SKIP(0.5)
    SIMCTR.Escp    COLON 87 LABEL "Expense Ledger S/Category Position"
    SIMCTR.Escl           LABEL "Length" SKIP(0.8)
    SIMCTR.AssetDepr COLON 30 LABEL "Depreciate Assets Y/N"
    SIMCTR.NDep      COLON 80 LABEL "Depreciation Period" SKIP(0.2)
    Simctr.repDir    COLON 30 LABEL "Reports Directory"
    
    rect-2 AT ROW 1 COL 2
    rect-1 AT ROW 25.5 COL 2
    btn-ok AT ROW 26.3 COL  20
    btn-close colon 80 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d scrollable
         TITLE "COMPANY MASTER CONTROL FILE".
    

/* *** Triggers to input frame frm-input*** */
ON 'enter':U OF simctr.LForm IN FRAME frm-Input
DO:
    APPLY 'tab' TO SELF.
    RETURN.
END.
ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
   DO TRANSACTION:
     ASSIGN simctr. 
    END. 
    RELEASE simctr.
     APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
     APPLY 'quit' TO FRAME frm-input.
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF AVAILABLE SIMCTR THEN
    DISPLAY SIMCTR WITH FRAME FRM-INPUT.
IF NOT AVAILABLE SIMCTR THEN
DO:
    MESSAGE "This record is currently in use." SKIP
                "Please try again later."
            VIEW-AS ALERT-BOX.
    RETURN.
END.
VIEW FRAME frm-input.
ENABLE ALL WITH FRAME frm-input.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
HIDE FRAME frm-input.
/*RELEASE simctr. */
RETURN.

