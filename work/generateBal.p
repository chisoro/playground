DEF VAR wsamt AS DEC.
DEF VAR X AS INT.
DEF VAR Y AS INT.
DO Y = 2014 TO 2019:
    FOR EACH gltdf WHERE INT(SUBSTR(STRING(PER),1,4)) = Y:
        DISPLAY gltdf.acc per. PAUSE 0.
        /*FIND FIRST glbal WHERE glbal.YEAR = Y AND glbal.acct = gltdf.acct NO-ERROR.
        IF NOT AVAILABLE glbal THEN DO:
            CREATE glbal.
            ASSIGN glbal.YEAR = Y
                   glbal.acct = gltdf.acct.
        END. 
        glbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glbal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] + gltdf.amt.*/
        /* Project */
        FIND FIRST glpBal WHERE glpBal.YEAR = Y AND glpBal.acct = gltdf.acct 
            AND glPbal.proj = gltdf.proj NO-ERROR.
        IF NOT AVAILABLE glpBal THEN DO:
            CREATE glpBal.
            ASSIGN glpBal.YEAR = Y
                   glpBal.acct = gltdf.acct
                   glPbal.proj = gltdf.proj.
        END.
        glpBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glpBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] + gltdf.amt.
        /* Department */
        FIND FIRST glDBal WHERE glDBal.YEAR = Y AND glDBal.acct = gltdf.acct 
            AND glDBal.dept = gltdf.dept NO-ERROR.
        IF NOT AVAILABLE glDBal THEN DO:
            CREATE glDBal.
            ASSIGN glDBal.YEAR = Y
                   glDBal.acct = gltdf.acct
                   glDBal.dept = gltdf.dept.
        END.
        glDBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] = glDBal.amt[INT(SUBSTR(STRING(gltdf.per),5,2))] + gltdf.amt.
    END.
END.
