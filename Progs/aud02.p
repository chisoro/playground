/* Program.................aud02.p
   Notes:................. Posted Journals Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF SHARED VAR w-orientation AS CHAR      INITIAL "LANDSCAPE"    NO-UNDO.
DEF SHARED VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF SHARED VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEF SHARED VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsProg AS CHAR.
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "General Ledger ", 1, "Accounts Receivables ", 2, "Accounts Payables", 3, "Cash Book", 4, "Stores", 5.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "PROCEED".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(2.5)
    wsOpt     LABEL "Select Report" COLON 30 SKIP(0.5)
    SKIP(2.5)
    btn-ok AT ROW 20.7 COL 20
    SPACE(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "POSTED JOURNAL REPORT" VIEW-AS DIALOG-BOX.


/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsOpt.
   IF wsOpt = 1 THEN
       wsProg = "gljrpt01.p".
   ELSE IF wsOpt = 2 THEN
       wsProg = "dbrpt03.p".
   ELSE IF wsOpt = 3 THEN
       wsProg = "crdrpt05.p".
   ELSE IF wsOpt = 4 THEN
       wsProg = "cbrpt05.p".
   ELSE IF wsOpt = 5 THEN
       wsProg = "stkrpt04.p".
   RUN VALUE(wsProg).
END.

/********** MAIN LOGIC **********/
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
ASSIGN wsOpt:SCREEN-VALUE = "1".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
