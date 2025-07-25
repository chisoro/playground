/* Common Variables */
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEFINE VARIABLE hOldRecord AS HANDLE     NO-UNDO.
DEFINE VARIABLE hNewRecord AS HANDLE     NO-UNDO.
DEFINE VARIABLE hOldField AS HANDLE     NO-UNDO.
DEFINE VARIABLE hNewField AS HANDLE     NO-UNDO.
DEFINE VARIABLE cChangedFields   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iChangedFields AS INTEGER    NO-UNDO.
DEFINE SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE lc-host AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE varUser AS CHARACTER.
DEF NEW SHARED VAR wsid LIKE {&skey}.
DEF VAR wsbtn AS CHAR.
DEF VAR varLedger LIKE glmf.acct.
DEF VAR wsvalid AS LOGICAL INITIAL YES.
DEF VAR DDes   LIKE gldept.descrip.
DEF VAR PDes   LIKE glproj.descrip.
DEF VAR ADes   LIKE glmf.DESCRIPTION.
DEF VAR wsVar  LIKE dbsmf.Descrip.
DEF VAR wsSrv LIKE dbsgr.sgrp.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "SEARCH".
DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-upd    LABEL "UPDATE".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btnSFPO   LABEL " Fin. Position Line Number  ".
DEF BUTTON btnSFPE   LABEL "Fin. Performance Line Number".
DEF BUTTON btnCF     LABEL "   Cash Flow Line Number    ".
DEF BUTTON btnEC     LABEL "   Equity Change line Number".
DEF BUTTON btnCat    LABEL "CATEGORY  ".
DEF BUTTON btnSubCat    LABEL "SUB-CATEGORY".
DEF BUTTON btnLedger LABEL "LEDGER  ".
DEF BUTTON btnFund   LABEL "SEGMENT/FUND".
DEF BUTTON btnInt    LABEL "INTEREST LEDGER  ".
DEF BUTTON btnBank   LABEL "BANK  ".
DEF BUTTON btnDept   LABEL "DEPARTMENT/VOTE  ".
DEF BUTTON btnProj    LABEL "PROJECT  ".
DEF BUTTON btnAcb    LABEL "ACB CASHBOOK ".
DEF BUTTON btnService LABEL "SERVICE".
DEF BUTTON btnReset  LABEL "PASSWORD RESET".
DEF BUTTON btnScheme LABEL "SCHEME".
DEF BUTTON btnAcc  LABEL "ACCOUNT".
DEF BUTTON btnTrans LABEL "View Transactions".


DEF BUFFER bfr{&tmptable} FOR {&tmptable} /*PRESELECT */.

DEF    QUERY qry-pickAcc FOR dbcmf SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
        DISPLAY dbcmf.dbAcc dbcmf.Name dbcmf.StandNo wsVar LABEL "Suburb" 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcc 
        brw-pickAcc AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

/*DEF    QUERY qry-pickScheme FOR LProj SCROLLING.
DEF BROWSE brw-pickScheme QUERY qry-pickScheme
        DISPLAY LProj.scheme LProj.Descrip 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickScheme 
        brw-pickScheme AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Scheme Selection".
*/
DEF    QUERY qry-Service FOR dbsgr SCROLLING.
DEF BROWSE brw-Service QUERY qry-Service
    DISPLAY dbsgr.Sgrp dbsgr.Descrip  COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickService 
    brw-Service AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Service Group Selection".

DEF    QUERY qry-ledger FOR glmf SCROLLING.
DEF BROWSE brw-ledger QUERY qry-ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-Fund FOR GlFund SCROLLING.
DEF BROWSE brw-Fund  QUERY qry-Fund 
    DISPLAY GLFUND.FUND COLUMN-LABEL "FUND  !SEGMENT" GLFUND.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 50 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-pickDept FOR glDept SCROLLING.
DEF BROWSE brw-pickDept QUERY qry-pickDept
    DISPLAY GLDept.sub-prog GLDept.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickDept 
    brw-pickDept AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Department Selection".

DEF    QUERY qry-PickProj FOR glProj SCROLLING.
DEF BROWSE brw-PickProj QUERY qry-PickProj
    DISPLAY GLProj.Proj GLProj.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickProj 
    brw-PickProj AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Project Selection".

DEFINE FRAME frm-pickFund 
    brw-Fund AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "FUND/SEGMENT SELECTION".

DEF QUERY qry-Cur FOR tblForex SCROLLING.
DEF BROWSE brw-cur QUERY qry-cur
    DISPLAY txtCur FORM "x(10)"
     WITH NO-LABEL 6 DOWN SEPARATORS.

DEFINE FRAME frm-Cur 
    brw-Cur AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Currency Selection".
    
DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".

DEF    QUERY qry-Acb FOR cbkmf SCROLLING.
DEF BROWSE brw-Acb QUERY qry-Acb
    DISPLAY cbkmf.Acb cbkmf.descrip COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcb 
    brw-Acb AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Cash Book Selection".

DEF    QUERY qry-Bank FOR cbkmf SCROLLING.
DEF BROWSE brw-Bank  QUERY qry-Bank 
    DISPLAY cbkmf.Bank  cbkmf.descrip COLUMN-LABEL "Description" 
    WIDTH 60 cbkmf.txtCur WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickBank  
    brw-Bank  AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Bank Selection".

DEF QUERY qry-{&tmptable} FOR bfr{&tmptable} SCROLLING.
DEF BROWSE brw-{&tmptable} QUERY qry-{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

browse brw-{&tmptable}:allow-column-searching = true.

ON 'start-search':u OF BROWSE brw-{&tmptable}
    run SEARCH.ip.
    
browse brw-ledger:allow-column-searching = true.

ON 'start-search':u OF BROWSE brw-ledger
    run SEARCH.ip.
