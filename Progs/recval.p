/* Program.................recval.p
   Notes:................. Receipt Validation
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF NEW SHARED STREAM a.
DEF NEW SHARED VAR wsDate      LIKE dbRec.RecDate.
DEF NEW SHARED VAR wsOp        LIKE dbRec.OpCode.
DEF NEW SHARED VAR wsValid     AS LOGICAL INITIAL YES.
DEF NEW SHARED VAR wsStatus   AS CHAR FORM "X(20)".
DEF NEW SHARED VAR wsVar       AS CHAR FORM "X(20)".
DEF NEW SHARED VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF NEW SHARED VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF NEW SHARED VAR wsTitle AS CHAR FORM "x(80)". 
DEF NEW SHARED BUFFER bfdbRec FOR dbRec.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(3)
    wsOp       LABEL "Operator Code" COLON 30 SKIP(0.5)
    SKIP(1)
    wsDate     LABEL "Receipting"    COLON 30 SKIP(0.5)
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24
    TITLE "RECEIPT VALIDATION" VIEW-AS DIALOG-BOX.


/* ******** Triggers  ***********/
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsOp wsDate.
   RUN recva01.p.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
wsTitle = simctr.CONAME + "   Report Date: " + STRING(TODAY).
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

   
