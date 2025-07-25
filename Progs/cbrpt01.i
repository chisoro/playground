PROCEDURE ProjRep.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glpbal WHERE glpbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glpbal.acct = tmpTrans.acct 
                         AND glpbal.proj = tmptrans.wsproj NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glpbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT REPORT FOR THE PERIOD:" 
                                   + STRING(st-per) + " TO " + STRING(end-Per).
    EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
    EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    FOR EACH tmpTrans  NO-LOCK BREAK  BY wsClas BY wsProj:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsProj).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
           ELSE 
               EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
            
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(wsProj) THEN DO:
            FIND FIRST glProj WHERE glProj.proj = tmpTrans.wsProj NO-ERROR.
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            EXPORT STREAM b DELIMITER ',' "     " + glProj.descrip
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg)
                            wsPerc.               
        END. 
       IF LAST-OF(wsclas) THEN DO:
              EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg).
        EXPORT STREAM b DELIMITER ',' "".
       END.
    END.
    EXPORT STREAM b DELIMITER ',' "NET FLOW" wsAmt wsTAmt.
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT DETAILED REPORT FOR THE PERIOD:" 
                                   + STRING(st-per) + " TO " + STRING(end-Per).
    EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
    EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
           ELSE 
               EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            EXPORT STREAM b DELIMITER ',' "     " + varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)
                            wsPerc.               
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
              EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg).
              EXPORT STREAM b DELIMITER ',' "".
        END.
    END.
    EXPORT STREAM b DELIMITER ',' "NET FLOW" wsAmt wsTAmt.
END. /*OFP ProjRep.ip */

PROCEDURE ProjRep1.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glpbal WHERE glpbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glpbal.acct = tmpTrans.acct 
                         AND glpbal.proj = tmptrans.wsproj NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glpbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans  NO-LOCK BREAK  BY wsClas BY wsProj:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsProj).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF LAST-OF(wsProj) THEN DO:
            FIND FIRST glProj WHERE glProj.proj = tmpTrans.wsProj NO-ERROR.
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
             DISPLAY STREAM a "     " + glProj.descrip @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
        END. 
       IF LAST-OF(wsclas) THEN DO:
              UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
              DISPLAY STREAM a  "SUB-TOTAL" @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) @ tmpTrans.Tamount 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg)    @ tmpTrans.Budg WITH FRAME a.
              DOWN  2 STREAM a WITH FRAME a.
       END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET FLOW" @ varDescr wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount
         WITH FRAME a.
    DOWN  2 STREAM a WITH FRAME a.
    PAGE STREAM a.
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            DISPLAY STREAM a "     " + varDescr @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
            DOWN STREAM a WITH FRAME a.
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
             UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
             DOWN STREAM a WITH FRAME a.
               DISPLAY STREAM a "SUB-TOTAL" @  varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg)     @ tmpTrans.Budg
                   WITH FRAME a.
              DOWN 2 STREAM a WITH FRAME a.
        END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET FLOW" @ varDescr wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount
         WITH FRAME a.
    DOWN  2 STREAM a WITH FRAME a.
    RETURN.
END. /*OFP ProjRep.ip */
