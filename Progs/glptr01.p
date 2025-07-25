/* Program.................glptr01.p
   Notes:................. Print posted transactions
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsUser     AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsPer      LIKE gltdf.period EXTENT 2.
DEF VAR wsAcc      LIKE gltdf.acct EXTENT 2.
DEF VAR wsTitle0  AS CHAR FORM "X(80)".
DEF VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsDTotal      AS DEC  FORM "ZZZZZZZZZZ9.99".
DEF VAR wsCTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsCredit LIKE gltdf.amt.
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR X         AS INT.
DEF VAR j         AS INT.

DEF BUFFER bf-gltdf FOR gltdf.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(3)
    wsPer[1]     LABEL "Start Accounting Period" COLON 30 SKIP(0.5)
    wsPer[2]     LABEL "End Accounting Period" COLON 30 SKIP(0.5)
    SKIP(1)
    wsAcc[1]     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsAcc[2]     LABEL "End Account" COLON 30 SKIP(0.5)
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24
    TITLE "POSTED TRANSACTION REPORT" VIEW-AS DIALOG-BOX.

FORM gltdf.acct       NO-LABEL
     gltdf.DESCRIP    LABEL "DESCRIPTION" FORMAT "X(20)"
     gltdf.trDate     LABEL "DATE"
     gltdf.ref        LABEL "REFERENCE"
     gltdf.AMT        LABEL "DEBITS"
     wsCredit           LABEL "CREDITS"
     wsTotal          LABEL "BALANCE"
    HEADER skip(3) wsTitle0 SKIP(2) "POSTED TRANSACTION FOR THE PERIOD: " wsPer[1] "TO " wsPer[2] 
    "Page: " AT 90 PAGE-NUMBER(a) SKIP(1) 
    "------------------------------------------------------------------------------------"
     SKIP(2)
    WITH DOWN STREAM-IO FONT 6 WIDTH 132 CENTERED NO-LABEL NO-BOX FRAME frm-rpt.

/* ******** Triggers  ***********/
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsPer[1] wsPer[2] wsAcc[1] wsAcc[2].
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  APPLY "close" TO THIS-PROCEDURE.
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
FIND FIRST simctr NO-ERROR.
ASSIGN wsPer[1]:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsPer[2]:SCREEN-VALUE = STRING(SIMCTR.CURPER)
      wsTitle0 = simctr.CONAME.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE Report.ip:  
   FOR EACH glmf where glmf.acct >= wsAcc[1] AND glmf.acct <= wsAcc[2] USE-INDEX acct:
       wsAmt = 0.
       wsTotal = 0.
       wsDTotal = 0.
       wsCTotal = 0.
       /* calculate bf */
       FIND FIRST glbal WHERE glbal.acct = glmf.acct AND glbal.YEAR = INT(SUBSTR(STRING(wsPer[1]),1,4)) NO-ERROR.
       IF AVAILABLE glbal THEN DO:
           ASSIGN wsAmt = glbal.bfbal
                  j     = INT(SUBSTR(STRING(wsPer[1]),5,2)) - 1.
           IF J > 0 THEN DO:
               DO X = 1 TO j:
                   wsAmt = wsAmt + glbal.Amt[X].
               END. 
           END.
           wsTotal = wsTotal + wsAmt.
           /* print transactions */
           DISPLAY STREAM a glmf.acct @ gltdf.acct glmf.DESCRIPTION @ gltdf.descrip WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
           DISPLAY STREAM a "Balance B/F" @ gltdf.descrip wsAmt @ wsTotal WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
           FOR EACH gltdf WHERE gltdf.acct = glmf.acct AND gltdf.per >= wsPer[1] AND gltdf.per <= wsPer[2]
                USE-INDEX acct1:
               wsTotal = wsTotal + gltdf.amt.
               IF gltdf.AMT < 0 THEN DO:
                   DISPLAY STREAM a gltdf.per @ gltdf.acct gltdf.DESCRIP gltdf.trDATE gltdf.REF gltdf.AMT @ wsCredit  wsTotal
                   WITH FRAME frm-rpt.
                   wsCTotal = wsCTotal + gltdf.AMT.
               END.
               IF gltdf.AMT >= 0 THEN DO:
                    DISPLAY STREAM a gltdf.per @ gltdf.acct gltdf.DESCRIP gltdf.trDATE gltdf.REF gltdf.AMT wsTotal
                   WITH FRAME frm-rpt.
                    wsDTotal = wsDTotal + gltdf.AMT.
               END.
               DOWN STREAM a WITH FRAME frm-rpt.
           END.
           UNDERLINE STREAM a gltdf.AMT wsCredit  wsTotal WITH FRAME frm-rpt.
           DISPLAY STREAM a "Balance C/D" @ gltdf.descrip wsDTotal @ gltdf.AMT wsCTotal @ wsCredit wsTotal WITH FRAME frm-rpt.
           DOWN 5 STREAM a WITH FRAME frm-rpt.
       END.
       ELSE NEXT.
   END.
END.
