/* Program.................glball.p
   Notes:................. Budget Allocation
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsstatus LIKE glmf.acct.
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Budget By Program ", 1, "Budget by Fund/Segment", 2. 
DEF VAR wsAn AS LOGICAL INITIAL YES.
DEF VAR X AS INT.

DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEF BUFFER bfGlbal FOR GLBAL.

DEF FRAME frm-main
    SKIP(1.5)
    wsAn      LABEL "Reset Allocations first" COLON 30
    SKIP(1)
    wsYear    LABEL "Budget Year" COLON 30
    SKIP(1)
    wsOpt     LABEL "Budgetting Option" COLON 30
    SKIP(1)
    wsStatus  LABEL "Processing......." COLON 30 VIEW-AS TEXT 
    SKIP(5)
    btn-ok AT ROW 12 COL 10 SPACE(40)
    btn-Exit
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 75 BY 15
    TITLE "MONTHLY BUDGET ALLOCATION " VIEW-AS DIALOG-BOX.

/**** TRIGGERS ***/
ON CHOOSE OF btn-ok IN FRAME frm-main 
DO:
    ASSIGN wsYear = INT(wsYear:SCREEN-VALUE)
           wsOpt  = INT(wsOpt:SCREEN-VALUE)
           wsAn  = LOGICAL(wsAn:SCREEN-VALUE).
    IF INT(wsYear:SCREEN-VALUE) < INT(SUBSTR(STRING(simctr.CURPER),1,4)) THEN DO:
        MESSAGE "You cannot budget for past periods" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        IF wsOpt = 1 THEN DO:
            IF wsAn = YES THEN DO:
                FOR EACH glPbal WHERE glPbal.YEAR = wsYear AND GlPbal.bugtotal <> 0:
                    glPbal.budget = 0.
                END.
                FOR EACH bfglbal WHERE bfglbal.YEAR = wsYear AND bfGlbal.bugtotal <> 0:
                    bfglbal.budget = 0.
                END.
            END.
           FOR EACH glPbal WHERE glPbal.YEAR = wsYear AND GlPbal.bugtotal <> 0 :
                wsStatus = glPbal.acct.
                DISPLAY wsStatus WITH FRAME frm-main.
                PAUSE 0.
                FIND FIRST bfglbal WHERE bfglbal.acct = glPbal.acct AND bfglbal.YEAR = wsYear NO-ERROR.
                IF NOT AVAILABLE bfglbal  THEN DO:
                    CREATE bfglbal.
                    ASSIGN bfglbal.YEAR = wsYear
                           bfglbal.acct = glFbal.acct.
                END.
                DO X = 1 TO 12:
                    ASSIGN glPbal.budget[X] = ROUND((GlPbal.bugtotal * GlPbal.budP[X] / 100),2)
                           bfglbal.budget[X] =  bfglbal.budget[X] + ROUND((GlPbal.bugtotal * GlPbal.budP[X] / 100),2)
                           bfglbal.bugtotal =  bfglbal.bugtotal  + ROUND((GlPbal.bugtotal * GlPbal.budP[X] / 100),2).
                END.
           END.
        END.
        ELSE DO:
            IF wsAn = YES THEN DO:
                FOR EACH glFbal WHERE glFbal.YEAR = wsYear AND glFbal.bugtotal <> 0:
                    glFbal.budget = 0.
                END.
                FOR EACH bfglbal WHERE bfglbal.YEAR = wsYear AND bfglbal.bugtotal <> 0:
                    bfglbal.budget = 0.
                END.
            END.
            FOR EACH glFbal WHERE glFbal.YEAR = wsYear AND glFbal.bugtotal <> 0 :
                wsStatus = glFbal.acct.
                DISPLAY wsStatus WITH FRAME frm-main.
                PAUSE 0.
                FIND FIRST bfglbal WHERE bfglbal.acct = glFbal.acct AND bfglbal.YEAR = wsYear NO-ERROR.
                IF NOT AVAILABLE bfglbal  THEN DO:
                    CREATE bfglbal.
                    ASSIGN bfglbal.YEAR = wsYear
                           bfglbal.acct = glFbal.acct.
                END.
                DO X = 1 TO 12:
                    ASSIGN glFbal.budget[X] = ROUND((glFbal.bugtotal * glFbal.budP[X] / 100),2)
                           bfglbal.budget[X] =  bfglbal.budget[X] + ROUND((glFbal.bugtotal * glFbal.budP[X] / 100),2)
                           bfglbal.bugtotal =  bfglbal.bugtotal  + ROUND((glFbal.bugtotal * glFbal.budP[X] / 100),2).
                END.
            END.
        END.
    END.
    APPLY 'close' TO THIS-PROCEDURE.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ASSIGN wsYear:SCREEN-VALUE = STRING(INT(SUBSTR(STRING(CURPER),1,4)) + 1)
       wsStatus:SCREEN-VALUE = "0"
       wsAn:SCREEN-VALUE = "YES".
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

