
/* Program.................simEnd.p
   Notes:................. Accounting Period End/Close
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF BUFFER bfrSimctr FOR simctr.
DEF BUFFER bfrglBal FOR glBal.
DEF BUFFER bfrglFBal FOR glfBal.
DEF BUFFER bfrglPBal FOR glPBal.
DEF BUFFER bfrglDBal FOR glDBal.
DEF VAR X AS INT.
DEF VAR wsDate AS DATE.
DEF VAR wsAmt LIKE GLBAL.BFBAL.
DEF VAR wsAcc LIKE bfrSimctr.SURACC.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsdays   AS INT FORM 99 EXTENT 12
        INITIAL [31,28,31,30,31,30,31,31,30,31,30,31].
DEF VAR wsDay   AS INT FORM "99".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsYear  AS INT FORM "9999".
DEF BUTTON btnJob    LABEL "OCCUPATION".
DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-exit  LABEL "EXIT".
DEF BUTTON btn-Edit  LABEL "EDIT".
DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".

DEF  VAR wsOpt AS INT VIEW-AS COMBO-BOX SIZE 15 BY 2 
                LIST-ITEM-PAIRS "Period-End",1,"Year-Closure",2.

DEFINE FRAME frm-SimEnd
    SKIP(1.5)
    bfrSimctr.CurPer     COLON 23 VIEW-AS TEXT LABEL "Current Period" SKIP(0.5)
    bfrSimctr.CLOSEPER   COLON 23  VIEW-AS TEXT LABEL "Closed Period" SKIP(1.5)
    wsOpt COLON 16 LABEL "Option"  AUTO-RETURN  SKIP(1.5)
    glmf.acct        COLON 23 LABEL "Processing......" VIEW-AS TEXT SKIP(1.5)
    btn-ok colon 10
    btn-close colon 50
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 70 BY 12.8
    TITLE "PERIOD END PROCEDURE" VIEW-AS DIALOG-BOX.

/* Triggers for the Paysys frame */

ON 'choose':U OF btn-ok IN FRAME frm-SimEnd 
DO:
    ASSIGN wsOpt.
    IF wsOpt = 1 THEN DO:
       FIND FIRST bfrSimctr EXCLUSIVE-LOCK NO-ERROR.
       IF int(SUBSTR(STRING(bfrSimctr.CURPER),5,2)) < 12 THEN
             bfrSimctr.CURPER = bfrSimctr.CURPER + 1.
        ELSE IF int(SUBSTR(STRING(bfrSimctr.CURPER),5,2)) = 12 THEN DO:
             bfrSimctr.CURPER =  INT(STRING(INT(SUBSTR(STRING(bfrSimctr.CURPER),1,4)) + 1) + "01").
        END.
         FIND FIRST bfrSimctr NO-LOCK NO-ERROR.
         APPLY 'close' TO THIS-PROCEDURE.
    END.
    ELSE DO:
        IF INT(STRING(INT(SUBSTR(STRING(bfrSimctr.CLOSEPER),1,4)) + 1) + "12") > bfrSimctr.CURPER THEN DO:
            MESSAGE "Required period greater than current period...please try again" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        wsYear = INT(SUBSTR(STRING(bfrSimctr.CLOSEPER),1,4)) + 1.
        RUN closure.ip.
        FIND FIRST bfrSimctr EXCLUSIVE-LOCK NO-ERROR.
        bfrSimctr.CLOSEPER =  INT(STRING(INT(SUBSTR(STRING(bfrSimctr.CLOSEPER),1,4)) + 1) + "12").
    END.
    FIND FIRST bfrSimctr NO-LOCK NO-ERROR.
    DISPLAY  bfrSimctr.CURPER  bfrSimctr.CLOSEPER WITH FRAME frm-SimEnd.
    APPLY 'entry' TO btn-close IN FRAME frm-SimEnd.
    RETURN.
END.
    

/********** MAIN LOGIC **********/
FIND FIRST bfrSimctr NO-ERROR .
INT(wsOpt:SCREEN-VALUE) = 1.
wsAcc = bfrSimctr.SURACC.
VIEW FRAME frm-SimEnd.
DISPLAY  bfrSimctr.CURPER  bfrSimctr.CLOSEPER WITH FRAME frm-SimEnd.
ENABLE ALL WITH FRAME frm-SimEnd.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-SimEnd.
HIDE FRAME frm-SimEnd.
APPLY 'esc' TO THIS-PROCEDURE.

PROCEDURE closure.ip:
    FOR EACH glmf WHERE glmf.ACCTYPE = "BS" NO-LOCK:
        DISPLAY glmf.acct WITH FRAME frm-Simend.
        PAUSE 0.
        /* Main balances */
        wsAmt = 0.
        FIND FIRST glbal WHERE GLBAL.acct = glmf.acct AND GLBAL.YEAR = wsYear NO-LOCK NO-ERROR.
        IF AVAILABLE glbal THEN DO:
            wsAmt = GLBAL.BFBAL.
            DO X = 1 TO 12:
                wsAmt = wsAmt + glbal.amt[X].
            END.
            FIND FIRST glbal WHERE GLBAL.acct = glmf.acct AND GLBAL.YEAR = (wsYear + 1) NO-ERROR.
            IF NOT AVAILABLE glbal THEN DO:
                CREATE glbal.
                ASSIGN GLBAL.acct = glmf.acct
                     GLBAL.YEAR   = (wsYear + 1).
            END.
             GLBAL.BFBAL = wsAmt.
        END.
        /* Fund balances */
        wsAmt = 0.
        FOR EACH glfbal WHERE GLfBAL.acct = glmf.acct AND GLfBAL.YEAR = wsYear NO-LOCK:
            wsAmt = GLFBAL.BFBAL.
            DO X = 1 TO 12:
                wsAmt = wsAmt + glfbal.amt[X].
            END.
            FIND FIRST bfrglfbal WHERE bfrGLfBAL.acct = glmf.acct AND bfrGLfBAL.fund = glfbal.fund 
                                   AND bfrGLfBAL.YEAR = (wsYear + 1) NO-ERROR.
            IF NOT AVAILABLE bfrglfbal THEN DO:
                CREATE bfrglfbal.
                ASSIGN bfrGLfBAL.acct = glfbal.acct
                       bfrGLfBAL.fund = glfbal.fund
                       bfrGLfBAL.YEAR   = (wsYear + 1).
            END.
             bfrGLfBAL.BFBAL = wsAmt.
        END.
        /* Project balances */
        wsAmt = 0.
        FOR EACH glpbal WHERE GLPBAL.acct = glmf.acct AND GLPBAL.YEAR = wsYear NO-LOCK:
            wsAmt = GLPBAL.BFBAL.
            DO X = 1 TO 12:
                wsAmt = wsAmt + glPbal.amt[X].
            END.
            FIND FIRST bfrglPbal WHERE bfrGLPBAL.acct = glmf.acct AND bfrGLPBAL.proj = glPbal.proj 
                                   AND bfrGLPBAL.YEAR = (wsYear + 1) NO-ERROR.
            IF NOT AVAILABLE bfrglPbal THEN DO:
                CREATE bfrglPbal.
                ASSIGN bfrGLPBAL.acct = glmf.acct
                       bfrGLPBAL.proj = GLPBAL.proj
                       bfrGLPBAL.YEAR   = (wsYear + 1).
            END.
             bfrGLPBAL.BFBAL = wsAmt.
        END.
        /* Department balances */
        wsAmt = 0.
        FOR EACH glDbal WHERE GLDBAL.acct = glmf.acct AND GLDBAL.YEAR = wsYear NO-LOCK:
            wsAmt = GLDBAL.BFBAL.
            DO X = 1 TO 12:
                wsAmt = wsAmt + glDbal.amt[X].
            END.
            FIND FIRST bfrglDbal WHERE bfrGLDBAL.acct = glmf.acct AND bfrGLDBAL.DEPT = glDbal.DEPT 
                                   AND bfrGLDBAL.YEAR = (wsYear + 1) NO-ERROR.
            IF NOT AVAILABLE bfrglDbal THEN DO:
                CREATE bfrglDbal.
                ASSIGN bfrGLDBAL.acct = glmf.acct
                       bfrGLDBAL.dept = GLDBAL.dept
                       bfrGLDBAL.YEAR   = (wsYear + 1).
            END.
             bfrGLDBAL.BFBAL = wsAmt.
        END.
    END. /* END OF BALANCES OPTIONS */

    /* Global Surplus/Decifit */
   wsAmt = 0.
    FOR EACH glmf WHERE (glmf.ACCTYPE = "I" OR glmf.ACCTYPE = "E") NO-LOCK:  
         DISPLAY glmf.acct WITH FRAME frm-Simend.
         PAUSE 0.
         FOR EACH glBal WHERE glBal.acct = glmf.acct AND glBal.YEAR = wsYear NO-LOCK:
             DO X = 1 TO 12:
                wsAmt = wsAmt + glBal.amt[X].
             END.
         END.
     END.
     FIND FIRST bfrglBal WHERE bfrglBal.acct = bfrsimctr.SurAcc AND bfrglBal.YEAR = wsYear NO-ERROR.
     IF AVAILABLE bfrglBal THEN DO:
         wsAmt = wsAmt + bfrglBal.BFBAL.
         DO X = 1 TO 12:
              wsAmt = wsAmt + bfrglBal.amt[X].
         END.
     END.
     FIND FIRST bfrglBal WHERE bfrglBal.acct = bfrsimctr.SurAcc AND bfrglBal.YEAR = (wsYear + 1) NO-ERROR.
     IF NOT AVAILABLE bfrglbal THEN DO:
            CREATE bfrglBal.
            ASSIGN bfrglBal.acct = bfrsimctr.SurAcc
                   bfrglBal.YEAR   = (wsYear + 1).
     END.
     bfrglBal.bfbal = wsAmt.

    /* Surplus/Decifit by fund */
  FOR EACH glfund NO-LOCK:
      IF CAN-FIND(FIRST bfrglfBal WHERE bfrglfBal.fund = glfund.fund) THEN DO:
            wsAmt = 0.
            FOR EACH glmf WHERE (glmf.ACCTYPE = "I" OR glmf.ACCTYPE = "E") NO-LOCK: 
                DISPLAY glmf.acct WITH FRAME frm-Simend.
                PAUSE 0.
                FOR EACH glfBal WHERE glfBal.acct = glmf.acct AND glfBal.YEAR = wsYear
                                 AND  glfbal.fund = glfund.fund NO-LOCK:
                    DO X = 1 TO 12:
                       wsAmt = wsAmt + glfBal.amt[X].
                    END.
                END.
            END.
            FIND FIRST bfrglfBal WHERE bfrglfBal.acct = bfrsimctr.SurAcc AND bfrglfBal.YEAR = wsYear 
                                    AND  bfrglfbal.fund = glfund.fund NO-ERROR.
            IF AVAILABLE bfrglfBal THEN DO:
                wsAmt = wsAmt + bfrglfBal.BFBAL.
                DO X = 1 TO 12:
                     wsAmt = wsAmt + bfrglfBal.amt[X].
                END.
            END.
            FIND FIRST bfrglfBal WHERE bfrglfBal.acct = bfrsimctr.SurAcc AND bfrglfBal.YEAR = (wsYear + 1) 
                                 AND  bfrglfbal.fund = glfund.fund NO-ERROR.
            IF NOT AVAILABLE bfrglfbal THEN DO:
                   CREATE bfrglfBal.
                   ASSIGN bfrglfBal.acct = bfrsimctr.SurAcc
                          bfrglfBal.fund = glfund.fund
                          bfrglfBal.YEAR   = (wsYear + 1).
            END.
            bfrglfBal.bfbal = wsAmt.
      END.
 END.
    
   /* Department Surplus/Deficit */
    FOR EACH gldept: 
            wsAmt = 0.
            FOR EACH glmf WHERE (glmf.ACCTYPE = "I" OR glmf.ACCTYPE = "E") NO-LOCK: 
                DISPLAY glmf.dept @ glmf.acct WITH FRAME frm-Simend.
                PAUSE 0.
                FOR EACH glDBal WHERE glDBal.acct = glmf.acct AND glDBal.YEAR = wsYear
                                 AND  glDBal.dept = gldept.dept NO-LOCK:
                    DO X = 1 TO 12:
                       wsAmt = wsAmt + glDBal.amt[X].
                    END.
                END.
            END.
            FIND FIRST bfrglDBal WHERE bfrglDBal.acct = bfrsimctr.SurAcc AND bfrglDBal.YEAR = wsYear 
                                    AND  bfrglDBal.dept = gldept.dept NO-ERROR.
            IF AVAILABLE bfrglDBal THEN DO:
                wsAmt = wsAmt + bfrglDBal.BFBAL.
                DO X = 1 TO 12:
                     wsAmt = wsAmt + bfrglDBal.amt[X].
                END.
            END.
            FIND FIRST bfrglDBal WHERE bfrglDBal.acct = bfrsimctr.SurAcc AND bfrglDBal.YEAR = (wsYear + 1) 
                                 AND  bfrglDBal.dept = gldept.dept NO-ERROR.
            IF NOT AVAILABLE bfrglDBal THEN DO:
                   CREATE bfrglDBal.
                   ASSIGN bfrglDBal.acct = bfrsimctr.SurAcc
                          bfrglDBal.dept = gldept.dept
                          bfrglDBal.YEAR   = (wsYear + 1).
            END.
            
            bfrglDBal.bfbal = wsAmt.
    END.
     APPLY 'close' TO THIS-PROCEDURE.
END. 


