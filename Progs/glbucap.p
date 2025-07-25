/* Program.................glbucap.p
   Notes:................. Budget Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE glmf.acct.
DEF VAR wsDesc LIKE glmf.DESCRIPTION.
DEF VAR wsamt  AS DEC EXTENT 2 FORM "ZZZZZZZZZZ9.99-".
DEF VAR X AS INT.

DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 105 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 105 by 19.7.

DEF BUFFER bfGlbal FOR GLBAL.

DEFINE QUERY qryBudget FOR  bfGlbal scrolling.
DEF BROWSE brwBudget QUERY qryBudget
   DISPLAY bfGlbal.acct LABEL "ACCOUNT"
           wsDesc WIDTH 60 
           bfGlbal.bugtotal WIDTH 16 FORM ">>,>>>,>>>,>>9.99-" LABEL "AMOUNT"
    ENABLE bfGlbal.bugtotal
    WITH 20 DOWN SEPARATORS.

DEF FRAME frm-main
    SKIP(1.5)
    wsYear    LABEL "Budget Year" COLON 30
    wsstart   LABEL "Start Ledger"   COLON 60 SKIP(0.5)
    SKIP(0.5)
    brwBudget   AT ROW 4 COL 8
    btn-exit AT ROW 22.0 COL 60
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 21.5 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 110 BY 25
    TITLE "TOTAL BUDGET CAPTURE BY LEDGER" VIEW-AS DIALOG-BOX.

/**** TRIGGERS ***/
ON 'enter':U OF wsYear 
DO:
    IF INT(wsYear:SCREEN-VALUE) < INT(SUBSTR(STRING(simctr.CURPER),1,4)) THEN DO:
        MESSAGE "You cannot budget for past periods" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.
ON 'ENTER':U OF wsstart IN FRAME frm-main
DO:
    ASSIGN wsYear = INT(wsYear:SCREEN-VALUE)
           wsStart = DEC(wsStart:SCREEN-VALUE).
   FOR EACH glmf WHERE glmf.acct >= wsstart NO-LOCK:
       FIND FIRST bfglbal WHERE bfglbal.YEAR = wsYear 
                            AND bfglbal.acct = glmf.acct NO-ERROR.
       IF AVAILABLE bfGlbal AND budP[1] = 0 THEN DO:
           ASSIGN bfglbal.budp = ROUND((100 / 12),2).
                  bfglbal.budp[12] = 8.37.
       END.
       IF NOT AVAILABLE bfglbal THEN DO:
           CREATE bfglbal.
           ASSIGN bfGlbal.acct = glmf.acct
                  bfglbal.YEAR = wsYear
                  bfglbal.budp = ROUND((100 / 12),2).
           bfglbal.budp[12] = 8.37.
       END.
   END.
   OPEN QUERY qryBudget FOR EACH bfGlbal WHERE bfGlbal.YEAR = wsYear
           AND bfGlbal.acct >= wsStart EXCLUSIVE-LOCK.
    RETURN.
END.

ON row-display OF brwBudget DO:
  FIND FIRST glmf WHERE glmf.acct = bfGlbal.acct NO-LOCK NO-ERROR.
  wsDesc = glmf.DESCRIPTION.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ASSIGN wsYear:SCREEN-VALUE = STRING(INT(SUBSTR(STRING(CURPER),1,4)) + 1)
       wsStart:SCREEN-VALUE = "0".
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

