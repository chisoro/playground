session:DATA-ENTRY-RETURN = TRUE.
/* Program.................DbRec.p
   Notes:......            Receipt capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE tmptable             dbcmf

DEF BUFFER bfrDbRec FOR dbRec.
DEF BUFFER bfrdbRecop FOR dbRecop.
DEF BUFFER bfrDbRecCtr FOR dbrecctr.
DEF TEMP-TABLE tmpbfrDbRec LIKE DbRec.
DEF TEMP-TABLE tblDetail LIKE dbblf.
DEF VAR method-status  AS LOGICAL.
DEF STREAM a.
DEF STREAM c.
DEF VAR varUser     LIKE simusr.usercode INIT "1".
DEF VAR varRecNo    LIKE bfrDbRec.RecNo FORM ">>>>>>9".
DEF VAR wsRecNo     LIKE varRecNo.
DEF VAR varDate     LIKE bfrDbRec.RecDate.
DEF VAR varrCode    LIKE bfrDbRec.rCode.
DEF VAR varMeth     LIKE bfrDbRec.Paytype.
DEF VAR varAmount   LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsAmt       LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsTotal     LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsVoid      LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsEquv      LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
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
DEF VAR varDue      LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR varbal      AS DEC FORM "zzzzzzzzz9.99-".
DEF VAR varServ     AS CHAR FORM "x(20)".
DEF VAR wsCashier   AS CHAR FORM "x(80)".
DEF VAR varTotal     LIKE bfrDbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsTitle     AS CHAR FORM "x(80)".
DEF VAR wsTitle1    LIKE simctr.CONAME.
DEF VAR wsAdd      AS CHAR FORM "x(80)".
DEF VAR  wsVar      AS CHAR FORM "x(40)"  LABEL "Suburb".
DEF VAR varDescrip   LIKE bfrDbRec.Descrip.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR wsRecDetails LIKE wsTitle.
DEF VAR wsFot       LIKE wsTitle EXTENT 3.
DEF VAR wsSup LIKE simusr.usercode.
DEF VAR varCur LIKE tblForex.txtCur.
DEF VAR bCur LIKE tblForex.txtCur.
DEF VAR aCur LIKE tblForex.txtCur.
DEF VAR wsdisRate LIKE tblForex.decRate.
DEF VAR wsd1Rate LIKE tblForex.decRate.
DEF VAR dbRate LIKE tblForex.decRate.
DEF VAR wsRate LIKE tblForex.decRate.
DEF VAR wsReceipts LIKE varTend.
DEF VAR wsTot  AS DEC EXTENT 3 FORM "z,zzz,zzz,zz9.99-".
DEF VAR tt AS CHAR FORM "x(11)" INITIAL "AMOUNT(ZWL)".

DEFINE VARIABLE w_password LIKE dbrecop.txtpass /*AS CHARACTER FORMAT "X(8)":U  */
     LABEL "CURRENT Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.
DEFINE VARIABLE sup-password AS CHARACTER FORMAT "X(8)":U 
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
DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEF QUERY qry-Land FOR hsecmf SCROLLING.
DEF BROWSE brw-Land QUERY qry-Land
    DISPLAY Hsecmf.dbAcc Hsecmf.Name Hsecmf.Scheme Hsecmf.StandNo Hsecmf.AmtDue[1] COLUMN-LABEL "USD"
    Hsecmf.AmtDue[2] COLUMN-LABEL "ZWL" FORM "z,zzz,zzz,zz9.99-"
     WITH NO-LABEL 10 DOWN SEPARATORS.

DEF QUERY qry-Cust FOR tblDetail SCROLLING.
DEF BROWSE brw-cust QUERY qry-cust
    DISPLAY wsVar LABEL "CODE" WIDTH 5 varServ LABEL "Description" FORM "X(25)"
    varBal FORM "zzz,zzz,zz9.99-" LABEL "Amount Due" varbal * dbRate  LABEL "ZWL Due" FORM "zz,zzz,zzz,zz9.99-"
     WITH NO-LABEL 10 DOWN SEPARATORS.

DEF QUERY qry-Cur FOR tblForex SCROLLING.
DEF BROWSE brw-cur QUERY qry-cur
    DISPLAY txtCur FORM "x(10)"
     WITH NO-LABEL 6 DOWN SEPARATORS.

DEF QUERY qry-Rec FOR tmpbfrDbRec SCROLLING.
DEF BROWSE brw-Rec QUERY qry-Rec
    DISPLAY tmpbfrDbRec.SeqNo COLUMN-LABEL "SEQ" tmpbfrDbRec.Account tmpbfrDbRec.rCode
       tmpbfrDbRec.Amount FORM "z,zzz,zzz,zz9.99-" WITH 10 DOWN SEPARATORS.

DEF    QUERY qry-pickAcc FOR dbcmf SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
    DISPLAY dbcmf.dbAcc dbcmf.Name dbcmf.StandNo dbcmf.stno LABEL "StreetNo" dbcmf.street LABEL "Street" wsVar LABEL "Suburb" 
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

FORM wsAcc    COLON 1  NO-LABEL
     "," COLON 16 
     dbRCod.Descrip  NO-LABEL 
     ','   COLON 40 
     wstax    NO-LABEL
     ','   COLON 51 
     varAmount       NO-LABEL 
    HEADER  wsTitle SKIP 
            wsCashier SKIP 
            wsTitle1 SKIP  
            wsRecDetails SKIP
            wsAdd
    "R;" 
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-rec.

FORM wsAcc    COLON 1  NO-LABEL
     dbRCod.Descrip  NO-LABEL  
     /*wstax    NO-LABEL  */
     varAmount       NO-LABEL 
    HEADER  wsTitle SKIP 
            wsCashier SKIP 
            wsTitle1 SKIP  
            wsRecDetails SKIP
            wsAdd
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-nonFrec.
    
FORM 
    /* SKIP (1) */
     "P;CASH" 
     "TOTAL   :" COLON 44  varTotal NO-LABEL SKIP
     "TENDERED:" COLON 44  varTend NO-LABEL SKIP
     "CHANGE  :" COLON 44  varChg NO-LABEL SKIP
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-rec1.
    
FORM 
    SKIP (1) 
     "TOTAL   :" COLON 30  varTotal NO-LABEL SKIP
     "TENDERED:" COLON 30  varTend NO-LABEL SKIP
     "CHANGE  :" COLON 30  varChg NO-LABEL SKIP
    WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-NonFrec1.
        
FORM SKIP
    "CURRENCY TENDERED:"  varCur COLON 27 NO-LABEL SKIP
    "AMOUNT           :" varTotal NO-LABEL SKIP
    "RATE             :" dbRate   NO-LABEL SKIP
    "USD EQUIVALENT   :" wsEquv   NO-LABEL  SKIP
    "AMOUNT TENDERED  :" varTend  NO-LABEL SKIP
    "CHANGE           :" varChg   NO-LABEL SKIP
    "INCENTIVE RECEIVED:" wsDisc   NO-LABEL  SKIP
    "BALANCE BEFORE RECEIPT"    bBal NO-LABEL COLON 44 bcur NO-LABEL SKIP
    "NEW BALANCE AFTER RECEIPT" aBal NO-LABEL COLON 44 acur NO-LABEL SKIP(1)
     WITH DOWN STREAM-IO FONT 10 NO-BOX FRAME frm-footer.

FORM
    dbPay.descrip LABEL "DESCRIPTION" wsAmt LABEL "AMOUNT" 
     HEADER "EOD" SKIP
     wsTitle1  SKIP
     wsCashier SKIP
     wsTitle SKIP(1)
    
    "------COLLECTION SUMMARY------"SKIP(1)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 CENTERED NO-BOX FRAME frmEod.

DEFINE FRAME frm-input 
    SKIP(1)
    varRecNo    AT ROW 2 COL 20 LABEL "Receipt Number" VIEW-AS TEXT NO-TAB-STOP
    varDate     AT ROW 2 COL 110 LABEL "Receipt Date" VIEW-AS TEXT NO-TAB-STOP SKIP(.5)
    varType     COLON 30         LABEL "Customer/Vote" AUTO-RETURN
               view-as combo-box size 20 by 2 LIST-ITEM-PAIRS
      "C - Customer Account","C","V - Direct to Vote","V", "L - Landsales Account","L" 
    btnMeth     NO-TAB-STOP varMeth NO-LABEL
    dbPay.descrip NO-LABEL NO-TAB-STOP VIEW-AS TEXT SKIP(0.5)
    varCur        COLON 70 LABEL "CURRENCY" VIEW-AS TEXT SPACE(30)
    wsdisRate    LABEL "TODAY's USD RATE"  VIEW-AS TEXT  FONT 6
    /*wsd1Rate    LABEL "Plus 10%" VIEW-AS TEXT FGCOLOR 12 FONT 6  */SKIP(0.5)
    btnAcc        COLON 12 AUTO-RETURN
    varAcc      BGCOLOR 1 FGCOLOR 12            NO-LABEL AUTO-RETURN
    SPACE(1)  dbcmf.NAME   NO-LABEL NO-TAB-STOP VIEW-AS TEXT skip(0.5)
    varTend    BGCOLOR 1 FGCOLOR 12  COLON 25 FORM "zzz,zzz,zz9.99" LABEL "Amount Tendered" SKIP(0.5) 
    btnCode       COLON 10 NO-TAB-STOP
    varRCode   BGCOLOR 1 FGCOLOR 12     NO-LABEL AUTO-RETURN
    dbRCod.Descrip NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 20 by 1 SKIP(.5)
    varDescrip BGCOLOR 1 FGCOLOR 12 COLON 17 LABEL "Receipt Details" VIEW-AS FILL-IN SIZE 40 BY 1 SKIP(.5)
    varAmount  BGCOLOR 1 FGCOLOR 12 COLON 30 FORM "zzz,zzz,zz9.99" LABEL "Amount" SKIP(.5)
    varloop     COLON 30 LABEL "More Allocations Y/N?"  SKIP(1.5)
    varRef     BGCOLOR 1 FGCOLOR 12 COLON 30 LABEL "Payment Reference" SKIP(.5)
    varChg      COLON 30 LABEL "Balance" VIEW-AS TEXT NO-TAB-STOP SKIP(0.5)
    wsReceipts  COLON 100 FORM "zzz,zzz,zz9.99-" LABEL "Unposted Receipts = "  VIEW-AS TEXT
    SKIP(1)
    btn-exit COLON 40 NO-TAB-STOP  SPACE(50) btnprn
    "YOU RECEIPT DETAILS" AT ROW 7.5 COL 65
    "ACCOUNT SERVICES DUE" AT ROW 7.5 COL 115
    wsTot[1]   AT ROW 17.2 COL 120 LABEL "TOTALS" wsTot[2] NO-LABEL 
    brw-cust   AT ROW 8.5 COL 108 NO-TAB-STOP
    brw-rec   AT ROW 8.5 COL 60   NO-TAB-STOP
    varDue    AT ROW 17.2 COL 83 LABEL "Amount Due" VIEW-AS TEXT NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 7.4 COL 59.4
    WITH SIZE 180 BY 23 view-as dialog-box keep-tab-order   no-validate
         side-labels no-underline CENTERED THREE-D /*BGCOLOR 3 FGCOLOR 0 */
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
                    each dbcmf no-lock 
                        where dbcmf.SortName >= wsSearch:SCREEN-VALUE
                           BY dbcmf.Sortname.
    /*RETURN.*/
END.

ON 'enter':U OF varType IN FRAME frm-input
OR  'LEAVE':U OF varType IN FRAME frm-input
DO:
    ASSIGN varType.
    IF varType:SCREEN-VALUE = "V" THEN DO:
        varAcc:SCREEN-VALUE = "0".
        DISABLE varAcc btnAcc WITH FRAME frm-input.
        /*APPLY 'entry' TO varRCode. */
    END.
    ELSE IF (varType:SCREEN-VALUE = "C" OR varType:SCREEN-VALUE = "L") THEN DO:
        ENABLE varAcc btnAcc WITH FRAME frm-input.
        /*APPLY 'entry' TO varAcc. */     
    END.
    APPLY 'entry' TO varMeth. 
    RETURN NO-APPLY.
END.

ON 'enter':U OF varAcc IN FRAME frm-input 
    OR 'LEAVE':U OF varAcc IN FRAME frm-input
DO:
    ASSIGN varAcc.
    IF varAcc = 0 THEN DO:
       APPLY 'ENTRY' TO varType IN FRAME frm-input.
       RETURN NO-APPLY.
    END.
    IF varType:SCREEN-VALUE = "C" THEN DO:
        FIND FIRST dbcmf WHERE dbcmf.dbAcc = INT(varAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE dbcmf THEN DO:
            DISPLAY dbcmf.NAME "ZZ" @ varRcode WITH FRAME frm-input.
            varbal = 0.
            varDescrip = dbcmf.NAME.
            ASSIGN wsRecDetails = dbcmf.NAME 
                   wsAdd = TRIM(dbcmf.stno) + " " + TRIM(dbcmf.street).
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
        FIND FIRST hsecmf WHERE hsecmf.dbAcc = INT(varAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF AVAILABLE hsecmf THEN DO:
            DISPLAY hsecmf.NAME @ dbcmf.NAME WITH FRAME frm-input.
            varbal = 0.
            varDescrip = hsecmf.NAME.
            ASSIGN wsRecDetails = hsecmf.NAME 
                   wsAdd = TRIM(hsecmf.Add1) + " " + TRIM(hsecmf.add2).
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
    IF varType:SCREEN-VALUE = "C" THEN 
        FIND FIRST dbrCod WHERE dbRCod.rCode = varrCode:SCREEN-VALUE 
                            AND dbrcod.TARGET = "C" NO-LOCK NO-ERROR.
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
        ASSIGN wsRecDetails = varDescrip
               wsAdd = "". /* ? */
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
        IF dec(varTend:SCREEN-VALUE) > dec(varDue:SCREEN-VALUE) AND SUBSTR(dbPay.descrip,1,4) <> "CASH" /*INT(varMeth:SCREEN-VALUE) <> 1 */ THEN DO:
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
                   + "   MC NO:" + STRING(simUsr.usercode, "999").               
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
    IF CAN-FIND (FIRST tmpbfrDbRec) AND simctr.cocode = "MTK" THEN
       RUN rec-nonFprn.ip.
    ELSE IF CAN-FIND (FIRST tmpbfrDbRec) THEN
       RUN rec-prn.ip.
    CLOSE QUERY qry-Cust.
    FOR EACH tmpbfrDbRec:
       DELETE tmpbfrDbRec.
    END.
    
    CLEAR FRAME frm-input ALL.
    wsRecDetails = "".
    varMeth:SCREEN-VALUE = "1".
    ENABLE ALL WITH FRAME frm-input.
    ASSIGN  varRecNo = varRecNo + 1
            varDue  = 0
            X = 0.
    DISPLAY varRecNo varDate wsdisRate WITH FRAME frm-input.
    varCur:SCREEN-VALUE = "ZWL".
    varloop:SCREEN-VALUE = "N".
    DISABLE  varRef  WITH FRAME frm-input.
    APPLY 'ENTRY' TO vartype IN FRAME frm-input.
    /*APPLY 'entry' TO varMeth IN FRAME frm-input.*/
    RETURN NO-APPLY.
END.
/* On Screen buttons triggers */

/* BtnAcc triggers */
ON CHOOSE OF btnAcc IN FRAME frm-Input
DO:
  IF varType = "C" THEN DO:
      VIEW FRAME frm-pickAcc.
      OPEN QUERY qry-pickAcc FOR EACH dbcmf NO-LOCK.
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
    varAcc = dbcmf.dbacc.
   DISPLAY  varAcc dbcmf.NAME WITH FRAME frm-Input.
   APPLY 'tab' TO varAcc IN FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickLAcc 
    OR 'enter':u OF brw-land 
    OR 'mouse-select-dblclick' OF brw-land 
DO: 
   GET CURRENT qry-land  EXCLUSIVE-LOCK NO-WAIT.
    varAcc = hsecmf.dbacc.
   DISPLAY  varAcc hsecmf.NAME @ dbcmf.NAME WITH FRAME frm-Input.
   APPLY 'tab' TO varAcc IN FRAME frm-input.
   RETURN.
END.

ON row-display OF brw-PickAcc DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
    wsVar = dbsmf.Descrip.
END. 

ON row-display OF brw-cust DO:
    varbal = tblDetail.dbAmt[1].
    /*DO Y = 1 TO 99:
        varbal = varbal + tblDetail.amt[Y].
    END. */
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
        wsVar = "LS".
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
   DISPLAY  varMeth dbPay.descrip  WITH FRAME frm-Input.
   APPLY 'tab' TO varMeth IN FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF varMeth IN FRAME frm-input 
    OR 'tab':U OF varMeth IN FRAME frm-input
    OR 'leave':U OF varMeth IN FRAME frm-input
DO:
   ASSIGN varMeth.
    FIND FIRST dbPay WHERE dbPay.Paytype = varMeth NO-LOCK NO-ERROR.
    IF AVAILABLE dbPay THEN DO:
        FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
        varCur  = cbkmf.txtCur.
        DISPLAY dbPay.descrip varCur WITH FRAME frm-input.
        IF varMeth = 1 THEN
            varRef:SCREEN-VALUE = "CASH".
        FIND FIRST tblForex WHERE tblForex.txtCur = varCur NO-LOCK NO-ERROR.
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
     OPEN QUERY qry-pickCode FOR EACH dbrcod WHERE dbRCod.TARGET = "C" NO-LOCK.
   ELSE
     OPEN QUERY qry-pickCode FOR EACH dbrcod WHERE dbRCod.TARGET = "V" NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickCode.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickCode 
          OR close of THIS-PROCEDURE IN FRAME frm-pickCode 
          OR CHOOSE OF btn-ok IN FRAME frm-pickCode 
          OR 'enter':u OF brw-pickCode
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
wsTitle1 = simctr.CONAME + " VAT REG: " + STRING(SIMCTR.BPNO).
/*wsTitle1 = simctr.CONAME + "    VAT REG:   " + STRING(SIMCTR.BPNO). */
varCur = "ZWL".
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
FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbcur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
IF AVAILABLE tblForex THEN DO:
      ASSIGN dbRate = tblForex.decRate.
END.
ELSE IF NOT AVAILABLE tblForex  THEN DO:
   FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbcur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
   IF AVAILABLE tblForexH THEN DO:
       ASSIGN dbRate = tblForexH.decRate.
    END.
END.
IF wsValid = YES THEN DO:
    VIEW FRAME frm-input. 
    ENABLE ALL EXCEPT varRef WITH FRAME frm-input.
    CLEAR FRAME frm-input ALL.
    wsRecDetails = "".
    varMeth:SCREEN-VALUE = "1".
    varloop:SCREEN-VALUE = "N".
    DISPLAY varRecNo varDate  varCur wsDisRate  WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-input.
    HIDE FRAME frm-input.
    OUTPUT STREAM a CLOSE.
END.
ELSE RETURN.



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
        wsCashier = "CASHIER:-" + simUsr.NAME.
        VIEW FRAME frm-pass.
        DISPLAY simusr.NAME WITH FRAME frm-pass.
        ENABLE ALL WITH FRAME frm-pass.
        APPLY 'entry' TO w_password.
        WAIT-FOR CHOOSE OF btn-ok OR 'enter':u OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-pass.
        HIDE FRAME frm-pass.
        IF w_password:SCREEN-VALUE = bfrDbRecop.txtPass THEN DO:
            IF bfrDbRecop.EOD = YES THEN DO:
                ASSIGN  varRecNo = bfrDbRecop.RecNo + 1
                        varDate = TODAY.
                MESSAGE "Receipting Date " UPDATE varDate.
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
                        HIDE FRAME frm-pass1.
                        FIND FIRST simusr WHERE simusr.usercode = wsSup:SCREEN-VALUE 
                            AND simusr.txtPass = sup-password:SCREEN-VALUE
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
        FOR EACH dbblf WHERE dbblf.dbacc = INT(varAcc:SCREEN-VALUE IN FRAME frm-input) NO-LOCK:
            CREATE tblDetail.
            BUFFER-COPY dbblf TO tblDetail.         
        END.
        /* Consider Receipts */
        FOR EACH dbrec WHERE dbRec.Account = varAcc AND dbRec.RecStat = "" AND dbRec.contype = "C" NO-LOCK:
            FIND FIRST dbrcod WHERE dbrcod.rcode = dbrec.rcode NO-LOCK NO-ERROR.
            FIND tbldetail WHERE tblDetail.sgrp = dbRCod.Sgrp NO-ERROR.
            IF AVAILABLE tblDetail THEN DO:
              ASSIGN tblDetail.dbAmt[1] = tblDetail.dbAmt[1] - ROUND((dbRec.Amount * (dbrec.decRate / dbRate)),2)
                     wsReceipts = wsReceipts + ROUND((dbRec.Amount * (dbrec.decRate / dbRate)),2) .
            END.
        END.
        FOR EACH tblDetail:
            ASSIGN wsTot[1] = wsTot[1] + tblDetail.dbAmt[1] WHEN tblDetail.dbAmt[1] > 0
                   wscnt = wscnt + 1
                   wsTot[3] = wsTot[3] + tblDetail.dbAmt[1]
                   bBal = bBal + tblDetail.dbAmt[1]. 
        END.
        wsTot[2] = wsTot[3] * dbRate.
        /*MESSAGE "Account balance " wsTot[1] VIEW-AS ALERT-BOX.*/
    END.
    ELSE IF varType = "L" THEN DO:
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
        CREATE tblDetail.
          ASSIGN tblDetail.dbacc = DEC(varAcc:SCREEN-VALUE)
                 tblDetail.sgrp  = 0
                 tblDetail.amt[1] = hsecmf.AmtDue[1].
          FOR EACH dbrec WHERE dbRec.Account = varAcc AND dbRec.RecStat = "" AND dbRec.contype = "L" NO-LOCK:
                FIND FIRST dbrcod WHERE dbrcod.rcode = dbrec.rcode NO-LOCK NO-ERROR.
                FIND FIRST tbldetail NO-ERROR.
                IF AVAILABLE tblDetail THEN DO:
                   ASSIGN tblDetail.dbAmt[1] = tblDetail.dbAmt[1] - ROUND((dbRec.Amount * (dbrec.decRate / dbRate)),2)
                          wsReceipts = wsReceipts + ROUND((dbRec.Amount * (dbrec.decRate / dbRate)),2) .
                END.
        END.
          ASSIGN wsTot[1] = tblDetail.amt[1]
                 wsTot[3] = tblDetail.amt[1]
                 wsTot[2] = tblDetail.amt[1] * dbRate
                 bBal     = tblDetail.amt[1].
    END.
    DISPLAY wsTot[3] @ wsTot[1] wsTot[2] wsReceipts WITH FRAME frm-input.
   ENABLE brw-cust WITH FRAME frm-input.
  OPEN QUERY qry-cust FOR EACH tblDetail 
      WHERE tblDetail.dbacc = INT(varAcc:SCREEN-VALUE) NO-LOCK.
END.

PROCEDURE rec-prn.ip:
    ASSIGN varTotal = 0
            wsdisc  = 0.
    FOR EACH tmpbfrDbRec:
        tmpbfrDbRec.Ref = varRef.
        BUFFER-COPY tmpbfrDbRec TO bfrDbRec.
        CREATE bfrDbRec.
        /*ASSIGN bfrDbRec.paytype = int(varMeth:SCREEN-VALUE IN FRAME frm-input).???? */
        EXPORT STREAM c DELIMITER ',' tmpbfrDbRec.
    END.
    OUTPUT STREAM a TO VALUE(bfrDbRecop.prn).  
    FOR EACH tmpbfrDbRec:
        vartotal = vartotal + tmpbfrDbRec.Amount.
        FIND FIRST dbrCod WHERE dbRCod.rCode = tmpbfrDbRec.rCode NO-ERROR.
        IF vat% = 0 THEN
            wsTax = "D".
        ELSE wsTax = "A".
        DISPLAY STREAM a STRING(tmpbfrDbRec.Account) @ wsAcc 
                         dbRCod.Descrip wsTax tmpbfrDbRec.Amount @ varAmount
            WITH FRAME frm-rec. 
        DOWN STREAM a WITH FRAME frm-rec.
    END.
    FIND LAST tmpbfrDbRec NO-LOCK NO-ERROR.
    /*IF simctr.COCODE = "CVF" AND varType  = "C" 
        AND (tmpbfrDbRec.Account < 51000000 OR tmpbfrDbRec.Account > 71999999) THEN /* City of Victoria Falls Discount */
          ASSIGN wsDisc = ROUND((varTotal * (10 / 100)),2) WHEN cbkmf.txtCur = "USD".*/
    ASSIGN bfrDbRecCtr.RecTotal = bfrDbRecCtr.RecTotal + vartotal.

    DISPLAY STREAM a varTotal varTend varChg
        WITH FRAME frm-rec1.
    DOWN STREAM a WITH FRAME frm-rec1.
    
    
    FIND FIRST tblForex WHERE tblForex.txtCur = cbkmf.txtCur AND varDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
          ASSIGN wsRate = tblForex.decRate.
    END.
    ELSE IF NOT AVAILABLE tblForex  THEN DO:
       FIND LAST tblForexH WHERE tblForexH.txtCur = cbkmf.txtCur AND tblForexH.dtRate <= varDate NO-LOCK NO-ERROR.
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
    ASSIGN aBal = bBal - ROUND((varTotal * (wsRate / dbRate)),2)
           aBal   = 0.00 WHEN varType = "V"
           wsEquv = ROUND((varTotal * (wsRate / dbRate)),2)
           bcur = SIMCTR.dbCur
           acur = SIMCTR.dbCur.
    DISPLAY STREAM a varCur varTotal dbRate varTend varChg bBal aBal bcur acur
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
                   + " MC NO:" + STRING(simUsr.usercode, "999")
    varTotal = 0.
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
        vartotal = vartotal + tmpbfrDbRec.Amount.
        FIND FIRST dbrCod WHERE dbRCod.rCode = tmpbfrDbRec.rCode NO-ERROR.
        IF vat% = 0 THEN
            wsTax = "D".
        ELSE wsTax = "A".
        DISPLAY STREAM a STRING(tmpbfrDbRec.Account) @ wsAcc 
                         dbRCod.Descrip /*wstax */ tmpbfrDbRec.Amount @ varAmount
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
    ASSIGN aBal = bBal - ROUND((varTotal * (wsRate / dbRate)),2)
           aBal   = 0.00 WHEN varType = "V"
           wsEquv = 0.00
           wsDisc = 0.00.
    DISPLAY STREAM a varCur varTotal dbRate varTend varChg bBal aBal
                     acur bcur wsDisc wsEquv WITH FRAME frm-footer.
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
IF CAN-FIND(FIRST bfrDbRec WHERE bfrDbRec.recno = wsRecNo AND bfrDbRec.OpCode = VarUser
            AND bfrDbRec.RecDate = varDate) THEN DO:
    OUTPUT STREAM a TO VALUE(bfrDbRecop.prn).  /* Output to be directed to defualt printer */
    /*put stream a control "~033[5i". */
   /* put stream a control "~033[". */ /* for POS Printer */
    FOR EACH bfrDbRec WHERE bfrDbRec.recno = wsRecNo AND bfrDbRec.OpCode = VarUser 
        BREAK BY bfrDbRec.recno BY bfrDbRec.SeqNo:
        wstax = "".
        IF FIRST-OF(bfrDbRec.recno) THEN DO:
            IF  bfrDbRec.contype = "C" THEN DO:
                FIND FIRST dbcmf WHERE dbcmf.dbacc = bfrDbRec.Account NO-LOCK NO-ERROR.
                IF AVAILABLE dbcmf THEN
                    ASSIGN wsRecDetails = dbcmf.NAME
                           wsAdd = TRIM(dbcmf.stno) + " " + TRIM(dbcmf.street).
            END.
           wsTitle = "DUPLICATE****" + bfrDbRec.txtCur + ":  RECEIPT: " + string(wsRecNo,"99999999") 
        + "     DATE: " + STRING(VarDate).     
        END.
        vartotal = vartotal + bfrDbRec.Amount.
        FIND FIRST dbrCod WHERE dbRCod.rCode = bfrDbRec.rCode NO-ERROR.
        IF vat% = 0 THEN
            wsTax = "D".
        ELSE wsTax = "A".
        DISPLAY STREAM a bfrDbRec.Account @ wsAcc ","
                         dbRCod.Descrip  "," wstax "," bfrDbRec.Amount @ varAmount
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
            FIND FIRST dbcmf WHERE dbcmf.dbacc =  DEC(varAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE dbcmf THEN DO:
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
                   FIND FIRST dbrCod WHERE dbRCod.Sgrp = tblDetail.sgrp NO-LOCK NO-ERROR.
                   IF  tblDetail.dbAmt[1] > 0 THEN DO:  
                        ASSIGN wsAlloc = ROUND(((tblDetail.dbAmt[1] / wsTot[1]) * varAmount),2).
                               wsTAll = wsTall + wsAlloc.
                        CREATE tmpbfrDbRec.
                        ASSIGN tmpbfrDbRec.SeqNo = x
                           tmpbfrDbRec.Ref = varRef:SCREEN-VALUE IN FRAME frm-input
                           tmpbfrDbRec.RecNo = INT(varRecNo:SCREEN-VALUE)
                           tmpbfrDbRec.RecDate = DATE(varDate:SCREEN-VALUE)
                           tmpbfrDbRec.rCode =   dbrCod.rCode 
                           tmpbfrDbRec.Paytype = varMeth
                           tmpbfrDbRec.txtCur = varCur
                           tmpbfrDbRec.decRate = tblForex.decRate
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
            END. /* eo...ZZ & > 0 */
            ELSE IF  varrCode:SCREEN-VALUE = "ZZ" AND wsTot[1] <= 0 THEN DO:
                wsTall = 0.
                FOR EACH tblDetail:
                   FIND FIRST dbrCod WHERE dbRCod.Sgrp = tblDetail.sgrp NO-LOCK NO-ERROR.
                    wsAlloc = ROUND((varAmount / wscnt),2).
                    wsTAll = wsTall + wsAlloc.
                    CREATE tmpbfrDbRec.
                    ASSIGN tmpbfrDbRec.SeqNo = x
                       tmpbfrDbRec.Ref = varRef:SCREEN-VALUE IN FRAME frm-input
                       tmpbfrDbRec.RecNo = INT(varRecNo:SCREEN-VALUE)
                       tmpbfrDbRec.RecDate = DATE(varDate:SCREEN-VALUE)
                       tmpbfrDbRec.rCode =   dbrCod.rCode 
                       tmpbfrDbRec.Paytype = varMeth
                       tmpbfrDbRec.txtCur = varCur
                       tmpbfrDbRec.decRate = tblForex.decRate
                       tmpbfrDbRec.OpCode = varUser
                       tmpbfrDbRec.contype = varType 
                       tmpbfrDbRec.Amount = wsAlloc
                       tmpbfrDbRec.Descrip = varDescrip
                       tmpbfrDbRec.Account = varAcc 
                       X = X + 1.
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
                       tmpbfrDbRec.decRate = tblForex.decRate
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
                    FOR EACH dbcmf no-lock 
                        where dbcmf.Sortname >= wsSearch:SCREEN-VALUE USE-INDEX NAME.

        END.
        when "Account" then
        do:
           open query qry-pickAcc 
                    FOR EACH dbcmf no-lock 
                        where dbcmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbacc.

        END.
        when "StandNo" then
        do:
           open query qry-pickAcc  
                    FOR EACH dbcmf no-lock 
                        where dbcmf.StandNo >= wsSearch:SCREEN-VALUE USE-INDEX stand.

        END.
        when "StreetNo" then
        do:
           open query qry-pickAcc  
                    FOR EACH dbcmf no-lock 
                        where dbcmf.Stno >= wsSearch:SCREEN-VALUE USE-INDEX street.

        END.
    END.
    RETURN.
END.
