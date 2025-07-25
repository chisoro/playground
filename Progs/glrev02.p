/* Program.................glrev01.p
   Notes:................. Move posted transactions to another period
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsUser     AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsPer1      LIKE gltdf.period.
DEF VAR wsPer2      LIKE gltdf.period.
DEF VAR wsCPer      LIKE gltdf.period. /*System Closed period */
DEF VAR wsPer      LIKE gltdf.period. /*System current period */
DEF VAR wsYear     AS INT FORM "9999".
DEF VAR wsMonth    AS INT FORM "99".
DEF VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsId      LIKE gltdf.transId.
DEF VAR wsTransId LIKE gltdf.transId.
DEF VAR wsOr      LIKE gltdf.source.
DEF VAR wsTime      AS CHAR FORM "x(8)".
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR X         AS INT.

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
    wsId      LABEL "Transaction ID Number"  COLON 30
     SKIP(1)
    wsPer1     LABEL "From Accounting Period" COLON 30 SKIP(0.5)
    wsPer2     LABEL "To Accounting Period" COLON 30 SKIP(0.5)
    wsOr       LABEL "Transaction Source" COLON 30 SKIP(1)
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24
    TITLE "TRIAL BALANCE REPORT" VIEW-AS DIALOG-BOX.

FORM gltdf.transId     LABEL "TRANSACTION"
     gltdf.trDate      LABEL "DATE"
     gltdf.acct        LABEL "ACCOUNT"
     gltdf.DESCRIP LABEL "DESCRIPTION"
     wsAmt             LABEL "AMOUNT"
    HEADER skip(1) "POSTED TRANSACTION MOVED FOR TRANSACTION: " 
    wsId "Page: " AT 60 PAGE-NUMBER SKIP(1) 
    " Accounting Period: "wsPer1 SPACE(2) "To Period: " wsPer2 SPACE(3) "DONE BY: " wsUser space(3) TODAY SKIP(1)
    "---------------------------------------------------------------------------"
     SKIP(5)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX FRAME frm-rpt.

/* ******** Triggers  ***********/
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   FIND FIRST simctr no-error.
   ASSIGN  wsId wsOr wsPer1 wsPer2.
   ASSIGN wsTime    = STRING(TIME,"HH:MM:SS").
          wsTransID = DEC (string(YEAR(TODAY),"9999")
                  + string(MONTH(TODAY),"99" )
                  + string(DAY(TODAY),"99")
                  + SUBSTR(STRING(wsTIME),1,2) 
                  + SUBSTR(STRING(wsTIME),4,2) 
                  + SUBSTR(STRING(wsTIME),7,2) ).
    IF wsPer1 <= simctr.curPer AND wsPer1 > simctr.closePer AND
       wsPer2 <= simctr.curPer AND wsPer2 > simctr.closePer THEN DO:
        {PrintOpt.i &stream-name="stream a"
                         &print-prog="Update.ip"
                         &paged} 
    END.        
   ELSE DO:
       MESSAGE "Accounting periods outside open accounting periods" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.

ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE UPDATE.ip:
   DO TRANSACTION ON ERROR UNDO, LEAVE:
       FOR EACH gltdf WHERE gltdf.period = wsPer1 AND gltdf.transId = wsId
                        AND SOURCE = wsOr:
           wsAmt = gltdf.amt * -1.
           FIND FIRST glmf WHERE glmf.acct = gltdf.acct NO-ERROR.
           /* Reverse balance*/
           FIND FIRST glbal WHERE glbal.acct = gltdf.acct AND glbal.YEAR = INT(SUBSTR(STRING(wsPer1),1,4)) NO-ERROR.
               glbal.amt[INT(SUBSTR(STRING(wsPer1),5,2))] = glbal.amt[INT(SUBSTR(STRING(wsPer1),5,2))] + wsAmt.
           /* Update new balance */
               FIND FIRST glbal WHERE glbal.acct = gltdf.acct AND glbal.YEAR = (INT(SUBSTR(STRING(wsPer2),1,4))) NO-ERROR.
           IF NOT AVAILABLE glbal THEN DO:
               CREATE glbal.
               ASSIGN GLBAL.acct = gltdf.acct
                      GLBAL.YEAR = INT(SUBSTR(STRING(wsPer1),1,4)).
           END.
           glbal.amt[INT(SUBSTR(STRING(wsPer2),5,2))] = glbal.amt[INT(SUBSTR(STRING(wsPer2),5,2))] + gltdf.amt.
           /* move transaction */
           ASSIGN gltdf.UID2 = wsUser
                  gltdf.descrip = "MID: "+ gltdf.descrip
                  gltdf.period = wsPer2
                  wsTotal = wsTotal + wsAmt.
           /* correct bf balances */
           IF glmf.ACCTYPE = "BS" THEN DO:
               IF wsPer1 < wsPer2 THEN
                   FOR EACH glbal WHERE glbal.acct = gltdf.acct AND glbal.YEAR > (INT(SUBSTR(STRING(wsPer1),1,4)))
                       AND glbal.YEAR <= (INT(SUBSTR(STRING(wsPer2),1,4))):
                       glbal.bfbal = glbal.bfbal + wsAmt.
                   END.
               IF wsPer1 > wsPer2 THEN
                   FOR EACH glbal WHERE glbal.acct = gltdf.acct AND glbal.YEAR > (INT(SUBSTR(STRING(wsPer2),1,4)))
                       AND glbal.YEAR <= (INT(SUBSTR(STRING(wsPer1),1,4))):
                       glbal.bfbal = glbal.bfbal + wsAmt.
                   END.
           END.
           
           DISPLAY STREAM a gltdf.transId gltdf.trDate gltdf.acct gltdf.descrip gltdf.amt @ wsAmt
               WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
       END.
       IF wsTotal <> 0 THEN DO:
           UNDO.
       END.
   END.
END.
