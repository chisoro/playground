/* Program.................recvalH.p
   Notes:................. Receipt Validation from History
   Author:.................S. Mawire
   Edited:.................S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF NEW SHARED STREAM a.
DEF NEW SHARED STREAM b.
DEF NEW SHARED VAR wsDate      LIKE dbRecH.RecDate.
DEF NEW SHARED VAR wsOp        LIKE dbRecH.OpCode.
DEF NEW SHARED VAR wsDate1      LIKE dbRecH.RecDate.
DEF NEW SHARED VAR wsOp1        LIKE dbRecH.OpCode.
DEF NEW SHARED VAR wsValid     AS LOGICAL INITIAL YES.
DEF NEW SHARED VAR wsStatus   AS CHAR FORM "X(20)".
DEF NEW SHARED VAR wsVar       AS CHAR FORM "X(20)".
DEF NEW SHARED VAR wsamt      AS DEC  FORM "ZZZZZZZZZZZZZ9.99-".
DEF NEW SHARED VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZZZZ9.99-".
DEF NEW SHARED VAR wsTitle AS CHAR FORM "x(80)". 
/*DEF NEW SHARED BUFFER bfdbRecH FOR dbRecH.*/

DEF BUTTON btn-exit   LABEL "Exit".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btn-exp     LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(3)
    wsOp       LABEL "Start Operator Code" COLON 30  SPACE (5)  wsOp1       LABEL "End Operator Code" SKIP(0.5)
    SKIP(1)
    wsDate     LABEL "Start Receipting Date"    COLON 30 SPACE (5) wsDate1     LABEL "End Receipting Date" SKIP(0.5)
    btn-ok AT ROW 20.7 COL 20
    space(20)btn-exp  
    SPACE (20) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24
    TITLE "RECEIPT COLLECTION REPORT" VIEW-AS DIALOG-BOX.


/* ******** Triggers  ***********/
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsOp wsDate wsOp1 wsDate1.
   RUN reccoldetail.p.
END.

ON 'choose':U OF btn-EXP  
DO:
   session:set-wait-state("").
   ASSIGN wsOp wsDate wsOp1 wsDate1.
   RUN reccoldetailE.p.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
wsTitle = simctr.CONAME + "   Report Date: " + STRING(TODAY).
wsOp:SCREEN-VALUE = "1".
wsOp1:SCREEN-VALUE = "999".
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

   
