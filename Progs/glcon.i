/* Procedure:  ...................... Glcon.i.
   Author:...........................S Mawire
 Consolidate to GL and Report */
FOR EACH dbgl WHERE dbgl.TransID = wsTransID AND  dbgl.SOURCE = wsSource:
    CREATE gltdf.
    BUFFER-COPY dbgl TO gltdf.
    ASSIGN gltdf.SOURCE = wsSource.
/* Ledger Balances */
    FIND FIRST glbal WHERE GLBAL.acct = dbgl.acct 
                       AND GLBAL.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
    IF NOT AVAILABLE glbal THEN DO:
        CREATE glbal.
        ASSIGN glbal.acct = dbgl.acct
               glbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
    END.
    ASSIGN glbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
          glbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
/* Project Balances */
    FIND FIRST glPbal WHERE GLPBal.proj = dbgl.proj 
                        AND GLPBal.acct = dbgl.acct 
                       AND GLPBal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
    IF NOT AVAILABLE glPbal THEN DO:
        CREATE glPbal.
        ASSIGN glPbal.acct = dbgl.acct
               GLPBal.proj = dbgl.proj
               glPbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
    END.
    ASSIGN glPbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
          glPbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
/* Department Balances */
    FIND FIRST glDbal WHERE GLDBal.dept = dbgl.dept AND glDbal.acct = dbgl.acct 
                       AND glDbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
    IF NOT AVAILABLE glDbal THEN DO:
        CREATE glDbal.
        ASSIGN glDbal.acct = dbgl.acct
               glDbal.dept = dbgl.dept
               glDbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
    END.
    ASSIGN glDbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
           glDbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
/* Fund/Segment Balances */
    FIND FIRST glfbal WHERE GLfBal.fund = dbgl.fund 
                        AND glfbal.acct = dbgl.acct 
                        AND glfbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
    IF NOT AVAILABLE glfbal THEN DO:
        CREATE glfbal.
        ASSIGN glfbal.acct = dbgl.acct
               glfbal.fund = dbgl.fund
               glfbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
    END.
    ASSIGN glfbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
           glfbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
END. 
ASSIGN wsDr = 0
       wsCr = 0
       wsTDr = 0
       wsTCr = 0
       wsTitle = "   ZiG DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:".
FOR EACH dbgl  WHERE dbgl.TransID = wsTransID AND  dbgl.SOURCE = wsSource:
    IF  dbgl.amt < 0 THEN
        ASSIGN wsDr = 0
               wsCr = dbgl.amt
               wsTCr = wsTCr + dbgl.amt.
    ELSE
        ASSIGN wsCr = 0
               wsDr = dbgl.amt
               wsTDr = wsTDr + dbgl.amt.
    FIND FIRST glmf WHERE glmf.acct = dbgl.acct NO-LOCK NO-ERROR.
    IF NOT AVAILABLE glmf THEN
        MESSAGE dbgl.acct " Account does not exist" VIEW-AS ALERT-BOX.
    DISPLAY STREAM a  dbgl.fund dbgl.Proj dbgl.dept dbgl.acct glmf.DESCRIPTION wsDr wsCr WITH FRAME frm-lrpt.
    DOWN STREAM a WITH FRAME frm-lrpt.
   DELETE dbgl.
END.
UNDERLINE STREAM a glmf.DESCRIPTION wsDr wsCr WITH FRAME frm-lrpt.
DISPLAY  STREAM a "TOTAL " @ glmf.DESCRIPTION wsTDr @ wsDr wsTCr @ wsCr WITH FRAME frm-lrpt.
DOWN STREAM a WITH FRAME frm-lrpt.
RETURN.
