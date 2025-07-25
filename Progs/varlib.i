/* Common Variables */
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR hCol           AS WIDGET-HANDLE.
DEFINE VARIABLE hOldRecord AS HANDLE     NO-UNDO.
DEFINE VARIABLE hNewRecord AS HANDLE     NO-UNDO.
DEFINE VARIABLE hOldField AS HANDLE     NO-UNDO.
DEFINE VARIABLE hNewField AS HANDLE     NO-UNDO.
DEFINE VARIABLE cChangedFields   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iChangedFields AS INTEGER    NO-UNDO.
DEFINE SHARED VARIABLE varUser AS CHARACTER.
DEFINE SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE lc-host AS CHAR NO-UNDO.
DEF NEW SHARED VAR wsid LIKE {&skey}.
DEF VAR wsbtn AS CHAR.
DEF VAR wsvalid AS LOGICAL INITIAL YES.
DEF VAR wsSearch      AS CHAR.
/* DEFINE VARIABLE lc-line AS CHAR NO-UNDO. */
DEFINE VAR j AS INTEGER.
DEFINE VAR txtUnWanted AS CHAR EXTENT 3.
DEFINE NEW SHARED VAR wsFile AS CHAR.
DEFINE NEW SHARED VAR wsFile1 AS CHAR.
DEFINE STREAM s-load.
DEFINE VAR wsCommand AS CHAR.

DEF BUTTON btnFund   LABEL "SEGMENT/FUND".
DEF BUTTON btnLedger LABEL "LEDGER  ".
DEF BUTTON btn-Prn    LABEL "PRINT".
DEF BUTTON btn-exp    LABEL "EXPORT".
DEF BUTTON btn-Addl   LABEL "ADD LINE".
DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-Save   LABEL "SAVE".
DEF BUTTON btn-upd    LABEL "UPDATE".
DEF BUTTON btn-dept   LABEL "DEPARTMENT".
DEF BUTTON btn-proj   LABEL "PROJECT".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btnCat     LABEL "CATEGORY  ".
DEF BUTTON btnSubCat  LABEL "SUB-CATEGORY".
DEF BUTTON btnReset   LABEL "Password Reset".
DEF BUTTON btnSearch  LABEL "Search".
DEF BUTTON btnAcc     LABEL "ACCOUNT".
DEF BUTTON btnTrans   LABEL "View Transactions".
DEF BUTTON btnWHse    LABEL "WAREHOUSE".
DEF BUTTON btnBank   LABEL "BANK  ".

DEF BUFFER bfr{&tmptable} FOR {&tmptable} PRESELECT.
     
DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEF QUERY qry-{&tmptable} FOR bfr{&tmptable} SCROLLING.
DEF BROWSE brw-{&tmptable} QUERY qry-{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

browse brw-{&tmptable}:allow-column-searching = true.
