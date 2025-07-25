/* Program.................glrev01.p
   Notes:................. Reverse posted transactions
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR wsUser     AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsPer      LIKE gltdf.period.
DEF VAR wsYear     AS INT FORM "9999".
DEF VAR wsMonth    AS INT FORM "99".
DEF VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsTotal      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsId      LIKE gltdf.transId.
DEF VAR wsTransId LIKE gltdf.transId.
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
    wsPer     LABEL "Transaction Accounting Period" COLON 30 SKIP(0.5)
    wsId      LABEL "Transaction ID Number"  COLON 30
     SKIP(1)
    btn-ok AT ROW 20.7 COL 20
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24
    TITLE "JOURNAL REVERSAL" VIEW-AS DIALOG-BOX.

FORM gltdf.transId     LABEL "TRANSACTION"
     gltdf.trDate      LABEL "DATE"
     gltdf.acct        LABEL "ACCOUNT"
     gltdf.DESCRIP LABEL "DESCRIPTION"
     wsAmt             LABEL "AMOUNT"
    HEADER skip(1) "POSTED TRANSACTION REVERSAL FOR TRANSACTION: " 
    wsId "Page: " AT 60 PAGE-NUMBER SKIP(1) 
    " Accounting Period: " AT 10 wsPer SPACE(3) "DONE BY: " wsUser space(3) TODAY SKIP(1)
    "---------------------------------------------------------------------------"
     SKIP(5)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX FRAME frm-rpt.

/* ******** Triggers  ***********/
ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN wsPer wsId.
   ASSIGN wsYear = INT(SUBSTR(STRING(wsPer),1,4))
          wsMonth = INT(SUBSTR(STRING(wsPer),5,2))
          wsTime    = STRING(TIME,"HH:MM:SS").
          wsTransID = DEC (string(YEAR(TODAY),"9999")
                  + string(MONTH(TODAY),"99" )
                  + string(DAY(TODAY),"99")
                  + SUBSTR(STRING(wsTIME),1,2) 
                  + SUBSTR(STRING(wsTIME),4,2) 
                  + SUBSTR(STRING(wsTIME),7,2) ).
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update.ip"
                    &paged} 
  APPLY "close" TO THIS-PROCEDURE.
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.

ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE UPDATE.ip:
   DO TRANSACTION ON ERROR UNDO, LEAVE:
       FOR EACH gltdf WHERE gltdf.period = wsPer AND gltdf.transId = wsId
           AND SOURCE = "GL" AND SUBSTR(gltdf.descrip,1,4) <> "RID:":
           wsAmt = gltdf.amt * -1.
           CREATE bf-gltdf.
           BUFFER-COPY gltdf TO bf-gltdf NO-ERROR.
           ASSIGN bf-gltdf.amt = wsAmt
                  bf-gltdf.TransId = wsTransId
                  bf-gltdf.UID2 = wsUser
                  bf-gltdf.descrip = "Reversal-TransID: " + STRING(gltdf.TransId)
                  bf-gltdf.CREDATE = TODAY
                  wsTotal = wsTotal + wsAmt
                  gltdf.descrip =  "RID: " + gltdf.descrip.
           FIND FIRST glbal WHERE glbal.acct = gltdf.acct AND glbal.YEAR = wsyear NO-ERROR.
           glbal.amt[wsMonth] = glbal.amt[wsMonth] + wsAmt.
           DISPLAY STREAM a gltdf.transId gltdf.trDate gltdf.acct gltdf.descrip gltdf.amt @ wsAmt
               WITH FRAME frm-rpt.
           DOWN STREAM a WITH FRAME frm-rpt.
       END.
       IF wsTotal <> 0 THEN DO:
           UNDO.
       END.
   END.
END.
