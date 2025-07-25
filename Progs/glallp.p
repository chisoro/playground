/* Program.................glallp.p
   Notes:................. Budget distribution Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsstart LIKE glmf.acct.
DEF VAR wsDesc LIKE glmf.DESCRIPTION.
DEF VAR wsamt  AS DEC FORM "ZZ9.99" LABEL "TOTAL".
DEF VAR X AS INT.

DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Print".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 11.7.

DEF BUFFER bfGlbal FOR GLBAL.

DEF FRAME frm-main
    SKIP(1.5)
    wsYear    LABEL "Budget Year" COLON 20
    wsstart   LABEL "Start Ledger"   COLON 60 SKIP(0.5)
    SKIP(0.5)
    bfGlbal.acct LABEL "ACCOUNT" COLON 20 VIEW-AS TEXT
    glmf.DESCRIPTION VIEW-AS TEXT  NO-LABEL SKIP(0.5)
    bfGlbal.BudP[1] LABEL "JANUARY" COLON 20 bfGlbal.BudP[2] LABEL "FEBRUARY" COLON 45 
    bfGlbal.BudP[3] LABEL "MARCH" COLON 70 SKIP(0.5)
    bfGlbal.BudP[4] LABEL "APRIL"   COLON 20 bfGlbal.BudP[5] LABEL "MAY" COLON 45 
    bfGlbal.BudP[6] LABEL "JUNE" COLON 70 SKIP(0.5)
    bfGlbal.BudP[7] LABEL "JULY"    COLON 20 bfGlbal.BudP[8] LABEL "AUGUST" COLON 45 
    bfGlbal.BudP[9] LABEL "SEPTEMBER" COLON 70  SKIP(0.5)
    bfGlbal.BudP[10] LABEL "OCTOBER" COLON 20 bfGlbal.BudP[11] LABEL "NOVEMBER" COLON 45 
    bfGlbal.BudP[12] LABEL "DECEMBER" COLON 70 SKIP(0.5)
    wsAmt COLON 20 NO-TAB-STOP VIEW-AS TEXT
    btn-exit AT ROW 14.0 COL 45 NO-TAB-STOP
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 13.5 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 17
    TITLE "BUDGET PERCENTAGE ALLOCATION CAPTURE" VIEW-AS DIALOG-BOX.

/**** TRIGGERS ***/
ON 'enter':U OF wsYear 
DO:
    IF INT(wsYear:SCREEN-VALUE) < INT(SUBSTR(STRING(simctr.CURPER),1,4)) THEN DO:
        MESSAGE "You cannot budget for past periods" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    DISABLE wsYear WITH FRAME frm-main.
    RETURN.
END.
ON 'ENTER':U OF wsstart IN FRAME frm-main
DO:
    ASSIGN wsYear = INT(wsYear:SCREEN-VALUE)
           wsStart = DEC(wsStart:SCREEN-VALUE).
    DISABLE wsStart WITH FRAME frm-main.
    FIND FIRST bfGlbal WHERE bfGlbal.YEAR = wsYear
           AND bfGlbal.acct >= wsStart EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = bfGlbal.acct NO-LOCK NO-ERROR.
    wsDesc = glmf.DESCRIPTION.
    wsAmt = 0.
    DO X = 1 TO 12:
          wsAmt = wsAmt + bfGlbal.budp[X].
    END.
    DISPLAY bfGlbal.acct glmf.descrip bfGlbal.budp wsAmt WITH FRAME frm-main.
    RETURN.
END.

ON 'enter':U OF bfGlbal.budP[12]
DO:
   wsAmt = wsAmt - bfGlbal.budP[12].
   wsAmt = wsAmt + DEC(bfGlbal.budP[12]:SCREEN-VALUE).
    DISPLAY wsAmt WITH FRAME frm-main.
    IF wsAmt > 100.00 OR wsAmt < 100  THEN DO:
        MESSAGE "Total must always be 100%" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND NEXT bfGlbal WHERE bfGlbal.YEAR = wsYear EXCLUSIVE-LOCK NO-ERROR.
        FIND FIRST glmf WHERE glmf.acct = bfGlbal.acct NO-LOCK NO-ERROR.
        wsDesc = glmf.DESCRIPTION.
        wsAmt = 0.
        DO X = 1 TO 12:
              wsAmt = wsAmt + bfGlbal.budp[X].
        END.
        DISPLAY bfGlbal.acct glmf.descrip bfGlbal.budp wsAmt WITH FRAME frm-main.
        APPLY 'tab' TO SELF.
        APPLY 'tab' TO  bfGlbal.budp[1] IN FRAME frm-main.
    END.
END.

ON 'enter':U OF bfGlbal.budP[1]
DO:
   wsAmt = wsAmt - bfGlbal.budP[1].
   wsAmt = wsAmt + DEC(bfGlbal.budP[1]:SCREEN-VALUE).
   DISPLAY wsAmt WITH FRAME frm-main.
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

